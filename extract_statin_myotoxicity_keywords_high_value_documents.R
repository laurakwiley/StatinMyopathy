###############################################################################
## Functions to Extract Statin Myotoxicity Keywords from High Value Documents
##
## Created by Laura Wiley
## March 13, 2014
## R v3.0.2
##
## process_notes() - this function takes a directory filled with patient notes, and processes all .txt files using extract_statin_phrases_high_value_documents() function in parallel using snowfall
##  This function:
##    1. Takes a directory filled with patient notes as .txt files.
##    2. Splits the job across the numer of threads specified
##    3. Processes each document extracting phrases of a specified length surrounding a statin mention and identifying statin myotoxicty keywords in those phrases and extracts the specific statins mentioned.
##    4. Takes the combined results and removes duplicate statin events. It also removes instances of the generic "statins" (used when a specific statin name was not mentioned) from individuals where other notes have a specified specific statin event.
##  Input: directory (the directory containing only sectag processed notes), wordspan (the number of words to either side of the statin, in high value documents we found a wordspan of 8 - thus total phrase length of 16 to be acceptable), threads (the number of threads for snowfall to distribute the task over, 1 defaults to a single thread process), method (either corrected or original related to which set of keywords to use for tagging.)
##  Output: a two column dataframe: IND_SEQ (the individual identifier) and statin (the specific statin or "statins" that the patient had listed in the allergy text).  If patients have recorded allergies to more than one statin they will have multiple entries (e.g. multiple rows), one for each statin)
##
## extract_statin_phrases_high_value_documents() - keywords contain both those in the original list, and those that were found in another 300 records from the same database. This set of records had their entire record reviewed and contained a total of 124 confirmed stain myotoxicity cases and 176 controls.
##  This function:
##    1. Takes a single note
##    2. Identifies all instances of statin mention
##    3. Extracts a variable number of words up and downstream of each statin mention (length determined using the "wordspan" variable in process_notes())
##    4. For each statin phrase, based on which keyword base selected (original or corrected) identifies statin specific myotoxicity events.
##    5. Processes these identified statin events to extract the specific statin/s mentioned. If a specific name is not mentioned, "statins" is used to capture the reports of class effects.
##  Input: a single patient document
##  Output: a two column dataframe: IND_SEQ (the individual identifier) and statin (the specific statin or "statins" that the patient had listed in the event text).  If patients have recorded events to more than one statin they will have multiple entries (e.g. multiple rows), one for each statin)
##
###############################################################################


process_notes<-function(directory,wordspan,threads,method=c("corrected","original")){
  require(snowfall)
  match.arg(method)
  notes<-list.files(path=directory,pattern="*.txt",)
  sfInit(parallel=TRUE, cpus=threads)
  sfExport("directory")
  sfExport("wordspan")
  sfExport("method")
  results<-sfClusterApplyLB(notes,extract_statin_phrases_high_value_documents)
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


extract_statin_phrases_high_value_documents<-function(notes){
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
      keep_list<-c(paste("allerg.*",statin_list,sep=""),paste("reaction to.*",statin_list,sep=""),"cramp","cpk"," ck",paste("not tolerat.*",statin_list,sep=""),"aching","muscle"," ache","myop","myos","mya","rha","weak","fatigue","malaise","adverse reaction","hurt","tired","sore","intoleran","cause.*problems",paste("trouble with ",statin_list,sep=""),"arthralgia",paste("felt lousy on ",statin_list,sep=""))
      exclude_list<-c("chronic muscle weakness","no muscle","check.*ck","check.*cpk","cpk.*ok"," ck.*ok","retired","reach","attached","qd","headache","no.*cpk","no.* ck","chk.*cpk","chk.* ck","normal myopathy risk",paste("allerg.*med.*",statin_list,sep=""),"no symptoms.*intolerance","no.*myalgias","lft","liver","can result in myopathy","can result in rhabdomyolosis","fibromyalgia","swelling")
      return_list<-c("myopathy","malaise","myalgias",paste(statin_list,"caused muscle ache",sep=" "),paste("stop ",statin_list,".*if better",sep=""))
      exclude_return<-c("no.*myalgias")
      
      kept<-x[grep(paste(keep_list,collapse="|"),tolower(x$PHRASE),perl=TRUE),]    
      excluded<-kept[grep(paste(exclude_list,collapse="|"),tolower(kept$PHRASE),perl=TRUE),]
      returned<-excluded[grep(paste(return_list,collapse="|"),tolower(excluded$PHRASE),perl=TRUE),]
      returned_filtered<-returned[grep(paste(exclude_return,collapse="|"),tolower(returned$PHRASE),perl=TRUE,invert=TRUE),]
      results<-rbind(kept[!(kept$PHRASE%in%excluded$PHRASE),],returned_filtered)
    } else if(method=="corrected"){
      keep_list<-c(paste("allerg.*",statin_list,sep=""),paste("reaction to.*",statin_list,sep=""),"cramp","cpk"," ck",paste("not tolerat.*",statin_list,sep=""),"aching","muscle"," ache","myop","myos","mya","rha","weak","fatigue","malaise","adverse reaction","hurt","tired","sore","intoleran","cause.*problems",paste("trouble with ",statin_list,sep=""),"arthralgia",paste("felt lousy on ",statin_list,sep=""),"cpk.*elevat","elevat.*cpk"," ck.*elevat","elevat.* ck","intolerant to","caus.*pain","myalgia","myopathy","weakness","cramp","myagia","painful hand joints","trigiminal neuralgia","arthralgia","mylagia","muscle pain","polyarthralgia","malaise","fatigue","rhabdomyolosis","muscle aches","\\Wtired","muscle"," ache","tolerate ","tolerated","pain","myop","mya"," rha","weak","adverse reaction","hurt","sore","intoleran","cause.*problems","aching",paste("trouble with ",statin_list,sep=""),"tolerat"," ck","cpk","myos",paste("had troubl.*",statin_list,sep=""),"side effects",paste("reactions to.*",statin_list,sep=""),"rechallange","sxs recurr","ill effects","bad reaction","joint problems","toxicity","^ck","leg issues","severe symptoms when taking statins")
      exclude_list<-c("chronic muscle weakness","no muscle","check.*ck","check.*cpk","cpk.*ok"," ck.*ok","retired","reach","attached","qd","headache","no.*cpk","no.* ck","chk.*cpk","chk.* ck","normal myopathy risk",paste("allerg.*med.*",statin_list,sep=""),"no symptoms.*intolerance","no [^refills].*myalgias","lft","liver","can result in myopathy","can result in rhabdomyolosis","fibromyalgia","swelling",paste("tolerat.*",statin_list,".*well",sep="")," mg","[0-9]mg","ck for surveillance","without.*side effects",paste("OK with.*",statin_list,sep=""),"no.*side effects","deconditioning","as needed for pain","dizzy"," ha ","cardiomyopathy","high risk for","increased risk of","not had any problems","tolerating.*well","pain clinic","tolerating.*without","chest pain","has tolerated","not having any leg cramps","cannot tolerate statins because.*upset stomach","tolerating.*better","Unable to tolerate fish oil","Tolerates.*with no unexplained muscle pain or weakness","did not tolerate niaspan","side effects of her immunosuppression","chronic back pain","has not had any problems tolerating","no pain in legs","sore throat","graft pain","statins are not affecting his muscles","baseline cpk")
      return_list<-c("[^cardio]myopathy","malaise","(^without).*myalgias",paste(statin_list,"caused muscle ache",sep=" "),paste("stop ",statin_list,".*if better",sep=""),"significantly elevated CPK","arm aches.*related to zocor","had significant fatigue with atorvastatin","questionable side effects","with intolerance","muscle aches and soreness after going up on lipitor","muscle aches and pains: could be.*lipitor","reports having had muscle aches on statins","lipitor \\(legs ached\\)","cannot tolerate statins.*(^stomach)","rhabdomyolysis attributed to statins","\\(.*myalgias.*\\)","\\(rhabdomyolosis\\)","\\(leg aches\\)","some myalgias","[^no ]myalgias on.*statin",paste("mg.*allerg.*",statin_list,sep=""),"cannot tolerate statins due to myalgias","had problems w/ zocor recently","intoleran.*statin.*(^lft|^liver|myal)","\\(muscle aches\\)","lipitor - myalgia","lipitor - ached all over","muscle aching on.*higher dose","leg myalgias.*2/2 simvastatin","myalgias on lipitor","( \\? muscle cramps )","had myalgias with lovastatin","did not tolerate","unablel to tolerate lovastatin 2/2 myalgias","simvastatin myalgias","has muscle aching and weakness with taking lipitor","\\(\\?myalgias with simvastatin\\)","because of myalgias with simvastatin","allergies: myalgias with statins","allergies.*myalgias and elavated cpk","he had myalgias","myalgias with both","zocor myalgias","had cpk elev","\\(joint aches\\)")
      exclude_return<-c("no .*myalgias","normal myopathy risk","did not tolerate niaspan and is now on pravachol")
      
      kept<-x[grep(paste(keep_list,collapse="|"),tolower(x$PHRASE),perl=TRUE),]    
      excluded<-kept[grep(paste(exclude_list,collapse="|"),tolower(kept$PHRASE),perl=TRUE),]
      returned<-excluded[grep(paste(return_list,collapse="|"),tolower(excluded$PHRASE),perl=TRUE),]
      returned_filtered<-returned[grep(paste(exclude_return,collapse="|"),tolower(returned$PHRASE),perl=TRUE,invert=TRUE),]
      results<-rbind(kept[!(kept$PHRASE%in%excluded$PHRASE),],returned_filtered)
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
