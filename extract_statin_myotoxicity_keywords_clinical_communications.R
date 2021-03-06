#################################################################################
## Functions to Extract Statin Myotoxicity Keywords from Clinical Communications
##
## Created by Laura Wiley
## March 13, 2014
## R v3.0.2
##
## process_notes() - this function takes a directory filled with patient clinical communications, and processes all .txt files using extract_statin_phrases_clinical_communications() function in parallel using snowfall
##  This function:
##    1. Takes a directory filled with patient notes as .txt files.
##    2. Splits the job across the numer of threads specified
##    3. Processes each document extracting phrases of a specified length surrounding a statin mention and identifying statin myotoxicty keywords in those phrases and extracts the specific statins mentioned.
##    4. Takes the combined results and removes duplicate statin events. It also removes instances of the generic "statins" (used when a specific statin name was not mentioned) from individuals where other notes have a specified specific statin event.
##  Input: directory (the directory containing only clinical communication txt files), wordspan (the number of words to either side of the statin, in clinical communications we found a wordspan of 15 - thus total phrase length of 30 to be acceptable), threads (the number of threads for snowfall to distribute the task over, 1 defaults to a single thread process), method (either corrected or original related to which set of keywords to use for tagging.)
##  Output: a two column dataframe: IND_SEQ (the individual identifier) and statin (the specific statin or "statins" that the patient had listed in the allergy text).  If patients have recorded allergies to more than one statin they will have multiple entries (e.g. multiple rows), one for each statin)
##
## extract_statin_phrases_clinical_communications() - keywords contain both those in the original list, and those that were found in another 300 records from the same database. This set of records had their entire record reviewed and contained a total of 124 confirmed stain myotoxicity cases and 176 controls.
##  This function:
##    1. Takes a single note
##    2. Identifies all instances of statin mention
##    3. Extracts a variable number of words up and downstream of each statin mention (length determined using the "wordspan" variable in process_notes())
##    4. For each statin phrase, based on which keyword base selected (original or corrected) identifies statin specific myotoxicity events.
##    5. Processes these identified statin events to extract the specific statin/s mentioned. If a specific name is not mentioned, "statins" is used to capture the reports of class effects.
##  Input: a single patient clinical communication note
##  Output: a two column dataframe: IND_SEQ (the individual identifier) and statin (the specific statin or "statins" that the patient had listed in the event text).  If patients have recorded events to more than one statin they will have multiple entries (e.g. multiple rows), one for each statin)
##
#################################################################################

process_notes<-function(directory,wordspan,threads,method=c("corrected","original")){
  require(snowfall)
  match.arg(method)
  notes<-list.files(path=directory,pattern="*.txt",)
  sfInit(parallel=TRUE, cpus=threads)
  sfExport("directory")
  sfExport("wordspan")
  sfExport("method")
  results<-sfClusterApplyLB(notes,extract_statin_phrases_clinical_communications)
  sfStop()
  output<-NULL
  for(i in 1:length(results)){
    if(is.null(results[i][[1]])){next}else{
      result<-as.data.frame(results[i])
      output<-rbind(output,result)
    }
  }
  output<-unique(output)
  output_generalstatin<-output[output$statin=="statins",]
  output_specificstatin<-output[output$statin!="statins",]
  individuals_no_specific_statin<-output_generalstatin[!(output_generalstatin$IND_SEQ%in%output_specificstatin$IND_SEQ),]
  output<-rbind(output_specificstatin,individuals_no_specific_statin)
  return(output)
}


extract_statin_phrases_clinical_communications<-function(notes){
  statin_phrases<-NULL
  filename<-paste(directory,notes,sep="")
  IND_SEQ<-substr(notes,1,regexpr("_",notes)-1)
  NOTE_ID<-substr(notes,regexpr("_",notes)+1,regexpr(".txt",notes)-1)
  
  txt<-readLines(filename)
  names(txt)<-"line"
  txt<-unlist(strsplit(txt,split=" "))
  txt<-txt[txt!=""]
  
  if(!any(grepl('atorva|lipitor|caduet|fluva|lescol|lova|mevacor|altocor|altoprev|pitava|livalo|prava|rosuva|crestor|simva|zocor|vytorin|simcor|hmg|statin |statins',txt,perl=TRUE))){
   
  }else{
    statin_index<-grep('atorva|lipitor|caduet|fluva|lescol|lova|mevacor|altocor|altoprev|pitava|livalo|prava|rosuva|crestor|simva|zocor|vytorin|simcor|hmg|statin |statins',txt,perl=TRUE)
    for(i in 1:length(statin_index)){
      if((statin_index[i]-wordspan)<0){start=0}else{start=(statin_index[i]-wordspan)}
      if((statin_index[i]+wordspan)>length(txt)){end=length(txt)}else{end=(statin_index[i]+wordspan)}
      statin_phrase<-data.frame(IND_SEQ=IND_SEQ,NOTE_ID=NOTE_ID,PHRASE=paste(txt[start:end],collapse=" "),START=start,END=end,NOTE_TEXT=paste(txt,collapse=" "))
      statin_phrases<-rbind(statin_phrases,statin_phrase)
    }
    
    x<-statin_phrases
    statin_phrases<-NULL                      
    
    statin_list<-c("atorvastin","fluvastatin","lovastatin","pitavastatin","pravastatin","rosuvastatin","simvastatin"," statin ","statins","hmg","lipitor","caduet","lescol","mevacor","altocor","altoprev","livalo","pitava","pravachol","crestor","zocor","vytorin","simcor")
    
    if(method=="original"){
      keep_list<-c("cramp","muscle","pain","myop","mya","rha","ache","weak","fatigue","malaise","adverse reaction","hurt","tired","sore","intoleran","cause.*problems","aching",paste("trouble with ",statin_list,sep=""),"allergy","tolerat"," ck","cpk","myos")
      exclude_list<-c("call if.*muscle","lft","retired","reach","best tolerated","warn","watn",paste(statin_list,"is weak",sep=""),"is a pain","attached")
      return_list<-c("myopathy")
      
      kept<-x[grep(paste(keep_list,collapse="|"),tolower(x$PHRASE),perl=TRUE),]    
      excluded<-kept[grep(paste(exclude_list,collapse="|"),tolower(kept$PHRASE),perl=TRUE),]
      returned<-excluded[grep(paste(return_list,collapse="|"),tolower(excluded$PHRASE),perl=TRUE),]
      results<-rbind(kept[!(kept$PHRASE%in%excluded$PHRASE),],returned)
    } else if(method=="corrected"){
      keep_list<-c("cpk.*elevat","elevat.*cpk"," ck.*elevat","elevat.* ck","intolerant to","caus.*pain","myalgia","myopathy","weakness","cramp","myagia","painful hand joints","trigiminal neuralgia","arthralgia","mylagia","muscle pain","polyarthralgia","malaise","fatigue","rhabdomyolosis","muscle aches","\\Wtired","muscle"," ache","tolerate ","tolerated","allerg","pain","myop","mya"," rha","weak","adverse reaction","hurt","sore","intoleran","cause.*problems","aching",paste("trouble with ",statin_list,sep=""),"tolerat"," ck","cpk","myos",paste("had troubl.*",statin_list,sep=""),"side effects",paste("reactions to.*",statin_list,sep=""),"rechallange","sxs recurr","ill effects","bad reaction")
      exclude_list<-c("call.*if.*","denies","we'll stop","[^not.*]able to tolerate","would be harmful","if.*have","allerg.*?","could.*tolerate","can result","increased risk of statin","increase.*chance","can.*tolerate","no evidence","be on the look out","did not tolerate trazodone","zetia 10 mg","pt advised to stop medication for muscle aches and call office","no muscle sx","no muscle pains","ck.*normal","high risk","screen for muscle aches","mg ","call if.*muscle","lft","best tolerated","warn","watn",paste(statin_list,"is weak",sep=""),"is a pain","attached","will tolerate increase","higher.*rates","have.*ck.*check","pain meds","can cause","sore throat","ck.*ordered")
      return_list<-c("call.*if.*recur","call.*if.*better","held.*pain","myopathy",paste("allergic.*to *",statin_list,sep=""),"myalgias are better","pain.*due to.*",paste("can't take.*",statin_list,sep=""),paste("did not tolerate.*",statin_list,sep=""),"cramps persist","pt getting sore","rechallange","muscles.*hurting more","not been able to tolerate statins","if muscle aching goes away","prior reaction to statins","myalgias","persisting statin-induced myositis",paste("intolerant of.*",statin_list,sep=""),"stopped for myalgias",paste("myalgias on.*",statin_list,sep=""),"caused myalgia","h/o myalgia","muscle tests are up","has had myalgias")
      
      kept<-x[grep(paste(keep_list,collapse="|"),tolower(x$PHRASE),perl=TRUE),]    
      excluded<-kept[grep(paste(exclude_list,collapse="|"),tolower(kept$PHRASE),perl=TRUE),]
      returned<-excluded[grep(paste(return_list,collapse="|"),tolower(excluded$PHRASE),perl=TRUE),]
      results<-rbind(kept[!(kept$PHRASE%in%excluded$PHRASE),],returned)
    }
    
    x<-results
    results<-NULL
    
    ## Identify statin specific keywords
    atorvastatin_keywords<-c("atorvastatin","lipitor","caduet")
    fluvastatin_keywords<-c("fluvastatin","lescol")
    lovastatin_keywords<-c("lovastatin","mevacor","altocor","altoprev")
    pitavastatin_keywords<-c("pitavastatin","livalo","pitava")
    pravastatin_keywords<-c("pravastatin","pravachol")
    rosuvastatin_keywords<-c("rosuvastatin","crestor")
    simvastatin_keywords<-c("simvastatin","zocor","vytorin","simcor")
    
    ## Extract Unique IDs for allegeries with each specific statin
    ## Make new column with the pretty formatted statin label
    atorvastatin_cases<-data.frame(IND_SEQ=unique(x[grep(paste(atorvastatin_keywords,collapse="|"),x$PHRASE),]$IND_SEQ))
    atorvastatin_cases$statin<-rep("atorvastatin",nrow(atorvastatin_cases))
    fluvastatin_cases<-data.frame(IND_SEQ=unique(x[grep(paste(fluvastatin_keywords,collapse="|"),x$PHRASE),]$IND_SEQ))
    fluvastatin_cases$statin<-rep("fluvastatin",nrow(fluvastatin_cases))
    lovastatin_cases<-data.frame(IND_SEQ=unique(x[grep(paste(lovastatin_keywords,collapse="|"),x$PHRASE),]$IND_SEQ))
    lovastatin_cases$statin<-rep("lovastatin",nrow(lovastatin_cases))
    pitavastatin_cases<-data.frame(IND_SEQ=unique(x[grep(paste(pitavastatin_keywords,collapse="|"),x$PHRASE),]$IND_SEQ))
    pitavastatin_cases$statin<-rep("pitavastatin",nrow(pitavastatin_cases))
    pravastatin_cases<-data.frame(IND_SEQ=unique(x[grep(paste(pravastatin_keywords,collapse="|"),x$PHRASE),]$IND_SEQ))
    pravastatin_cases$statin<-rep("pravastatin",nrow(pravastatin_cases))
    rosuvastatin_cases<-data.frame(IND_SEQ=unique(x[grep(paste(rosuvastatin_keywords,collapse="|"),x$PHRASE),]$IND_SEQ))
    rosuvastatin_cases$statin<-rep("rosuvastatin",nrow(rosuvastatin_cases))
    simvastatin_cases<-data.frame(IND_SEQ=unique(x[grep(paste(simvastatin_keywords,collapse="|"),x$PHRASE),]$IND_SEQ))
    simvastatin_cases$statin<-rep("simvastatin",nrow(simvastatin_cases))
    
    ## Create full list of all indidivudals who have a specified statin reaction
    individuals_with_statin_specific_allergy<-rbind(atorvastatin_cases,rbind(fluvastatin_cases,rbind(lovastatin_cases,rbind(pitavastatin_cases,rbind(pravastatin_cases,rbind(rosuvastatin_cases,simvastatin_cases))))))
    
    ## Identify any indviduals who do not have a specific statin listed
    ## Put the generic "statins" as their specific statin allergy
    individuals_with_general_statin_allergy<-data.frame(IND_SEQ=unique(x[!x$IND_SEQ%in%individuals_with_statin_specific_allergy$IND_SEQ,]$IND_SEQ))
    individuals_with_general_statin_allergy$statin<-rep("statins",nrow(individuals_with_general_statin_allergy))
    
    ## Return the complet list of statin specific and generic statin individuals
    return(rbind(individuals_with_statin_specific_allergy,individuals_with_general_statin_allergy))                                                    
  }
}
