##################################################################
## Functions to Extract Statin Myopathy from High Value Documents
##
## Created by Laura Wiley
## March 13, 2014
## R v3.0.2
##
## process_sectag() - this function takes a directory filled with sectag files, and processes all .txt files using identify_statin_allergy_high_value_document() function in parallel using snowfall
##  This function:
##    1. Takes a directory filled with notes processed by sectag (default output are xml style .txt files).
##    2. Splits the job across the numer of threads specified
##    3. Processes each document using identify_statin_allergy_high_value_document()
##    4. Takes the combined results and removes duplicate statin allergies. It also removes instances of the generic "statins" allergy (used when a specific statin name was not mentioned) from individuals where other notes have a specified specific allergy
##  Input: directory (the directory containing only sectag processed notes), threads (the number of threads for snowfall to distribute the task over, 1 defaults to a single thread process), method (either corrected or original related to which set of keywords to use for tagging.)
##  Output: a two column dataframe: IND_SEQ (the individual identifier) and statin (the specific statin or "statins" that the patient had listed in the allergy text).  If patients have recorded allergies to more than one statin they will have multiple entries (e.g. multiple rows), one for each statin)
##
## identify_statin_allergy_high_value_document() - keywords contain both those in the original list, and those that were found in another 300 records from the same database. This set of records had their entire record reviewed and contained a total of 124 confirmed stain myotoxicity cases and 176 controls.
##  This function:
##    1. Takes a single note
##    2. Identifies all instances of section 448 (allergies and adverse reactions(
##    3. Extracts all text tagged by that section header and separates out individual lines
##    4. Based on which keyword base selected (original or corrected) identifies statin specific allergies that are not obviously not myopathy (e.g. liver function test elevation).
##    5. Processes these identified statin allergies to extract the specific statin causing the reaction. If a specific name is not mentioned, "statins" is used to capture the reports of class effects.
##  Input: a single sectag processed file
##  Output: a two column dataframe: IND_SEQ (the individual identifier) and statin (the specific statin or "statins" that the patient had listed in the allergy text).  If patients have recorded allergies to more than one statin they will have multiple entries (e.g. multiple rows), one for each statin)
##
##################################################################

process_sectag<-function(directory,threads,method=c("corrected","original")){
  require(snowfall)
  match.arg(method)
  notes<-list.files(path=directory,pattern="*.txt",)
  sfInit(parallel=TRUE, cpus=threads)
  sfExport("directory","method")
  sfLibrary(stringr)
  results<-sfClusterApplyLB(notes,identify_statin_allergy_high_value_document)
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

identify_statin_allergy_high_value_document<-function(notes){
  allergy_list<-NULL
  filename<-notes
  IND_SEQ<-substr(filename,1,regexpr("_",filename)-1)
  DOC_ID<-substr(filename,regexpr("_",filename)+1,regexpr("\\.txt",filename)-1)
  
  txt<-data.frame(readLines(paste0(directory,filename)))
  names(txt)<-"line"
  
  if(!any(grepl('code="448"',txt$line))){}else{
    
    allergy_start<-grep('code="448"',txt$line)
    for(i in 1:length(allergy_start)){
      subset_start<-allergy_start[i]
      subset_end<-nrow(txt)
      txt_subset<-data.frame(line=txt[subset_start:subset_end,])
      text_starts<-grep("<text>",txt_subset$line)
      text_ends<-grep("</text>",txt_subset$line)
      start<-text_starts[1]
      end<-text_ends[1]
      
      allergy<-data.frame(IND_SEQ=IND_SEQ,DOC_ID=DOC_ID,allergy=txt_subset[start:end,])
      allergy$allergy<-sub("\t<text>","",allergy$allergy)
      allergy$allergy<-sub("</text>","",allergy$allergy)
      allergy<-allergy[allergy$allergy!="",]
      allergy$allergy<-str_trim(allergy$allergy)
      allergy_list<-rbind(allergy_list,allergy)
    }  
    allergy_list<-unique(allergy_list)
    allergy_list$allergy<-tolower(allergy_list$allergy)
    
    x<-allergy_list
    allergy_list<-NULL
    
    statin_list<-c("atorvastin","fluvastatin","lovastatin","pitavastatin","pravastatin","rosuvastatin","simvastatin"," statin ","statins","hmg","lipitor","caduet","lescol","mevacor","altocor","altoprev","livalo","pitava","pravachol","crestor","zocor","vytorin","simcor")
    
    if(method=="original"){
      not_myopathy_keywords<-c("nausea","vomiting","hives","mg ","lft","headache","liver","rash","night trerrors","night terrors","pruritis","gi ","current medications","constipation","renal","swelling","on.*therapy","mg\\.")
      myopathy_keywords<-c(paste(statin_list, " \\(intolerance\\)",sep=""),paste(statin_list, " \\(unknown\\)",sep=""),"pravachol  ","cramp","myalgia","per med record","ache","pain","weak","muscle","myositis","myopathy","fatigue","malaise","rhabdomyolosis")
      filter_by_statin<-x[grep(paste(statin_list,collapse="|"),tolower(x$allergy)),]
      filter_by_notmyopathy<-filter_by_statin[grep(paste(not_myopathy_keywords,collapse="|"),filter_by_statin$allergy,perl=TRUE),]
      filter_notmyopathy_by_myopathy<-filter_by_notmyopathy[grep(paste(myopathy_keywords,collapse="|"),filter_by_notmyopathy$allergy,perl=TRUE,invert=TRUE),]
      statin_myopathy<-filter_by_statin[!filter_by_statin$allergy%in%filter_notmyopathy_by_myopathy$allergy,]
    }else if(method=="corrected"){
      not_myopathy_keywords<-c("diarrhea","nausea","black spots","lft","breath","liver","gi ","headache","dizz","cough","rash","edema","swollen",'ha(\\W|$)',"h/a","qhs","every night","bedtime","hallucinations","proctitis","flatulence","stool","nosebleeds","swelling","hives","pancreatitis","constipation","skin crawls","sore throat","eczema","sweaty","stomach","flushed","urinary","heat","transaminases","qd","hepatitis","urination","gastrointestinal","light headed","increased bp","night terrors","daily","hepatic","hepatotoxicity","anaphylaxis","n/v","every day","increased ed","renal","blurred vision","hyperkalemia","anxiety","itching","night trerrors","bid","sob","night terrors","vomiting"," mg ","[0-9]+mg","pruritis","gi ","current medications","on.*therapy","mg\\.",paste("is tolerating",statin_list,sep=" "))
      myopathy_keywords<-c("myopathy","myalgia","weakness","cramp","myagia","arthralgia","mylagia","muscle pain","polyarthralgia","malaise","fatigue","rhabdomyolosis","muscle aches",paste(statin_list, " \\(intolerance\\)",sep=""),paste(statin_list, " \\(unknown\\)",sep=""),"pravachol  ","per med record"," ache","[^no ]pain","weak","muscle","myositis","fatigue","malaise","rhabdomyolosis",paste("intolerant of ",statin_list,"(\\.|,|;)",sep="")) 
      not_myopathy_secondpass_keywords<-c(paste("tolerating.*",statin_list,".*without.*",sep=""))
      filter_by_statin<-x[grep(paste(statin_list,collapse="|"),tolower(x$allergy)),]
      filter_by_notmyopathy<-filter_by_statin[grep(paste(not_myopathy_keywords,collapse="|"),filter_by_statin$allergy,perl=TRUE),]
      filter_notmyopathy_by_myopathy<-filter_by_notmyopathy[grep(paste(myopathy_keywords,collapse="|"),filter_by_notmyopathy$allergy,perl=TRUE,invert=TRUE),]
      statin_myopathy<-filter_by_statin[!filter_by_statin$allergy%in%filter_notmyopathy_by_myopathy$allergy,]
      statin_myopathy<-statin_myopathy[grep(paste(not_myopathy_secondpass_keywords,collapse="|"),statin_myopathy$allergy,invert=TRUE),]
    }
    
    x<-statin_myopathy
    statin_myopathy<-NULL
    
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
    atorvastatin_cases<-data.frame(IND_SEQ=unique(x[grep(paste(atorvastatin_keywords,collapse="|"),x$allergy),]$IND_SEQ))
    atorvastatin_cases$statin<-rep("atorvastatin",nrow(atorvastatin_cases))
    fluvastatin_cases<-data.frame(IND_SEQ=unique(x[grep(paste(fluvastatin_keywords,collapse="|"),x$allergy),]$IND_SEQ))
    fluvastatin_cases$statin<-rep("fluvastatin",nrow(fluvastatin_cases))
    lovastatin_cases<-data.frame(IND_SEQ=unique(x[grep(paste(lovastatin_keywords,collapse="|"),x$allergy),]$IND_SEQ))
    lovastatin_cases$statin<-rep("lovastatin",nrow(lovastatin_cases))
    pitavastatin_cases<-data.frame(IND_SEQ=unique(x[grep(paste(pitavastatin_keywords,collapse="|"),x$allergy),]$IND_SEQ))
    pitavastatin_cases$statin<-rep("pitavastatin",nrow(pitavastatin_cases))
    pravastatin_cases<-data.frame(IND_SEQ=unique(x[grep(paste(pravastatin_keywords,collapse="|"),x$allergy),]$IND_SEQ))
    pravastatin_cases$statin<-rep("pravastatin",nrow(pravastatin_cases))
    rosuvastatin_cases<-data.frame(IND_SEQ=unique(x[grep(paste(rosuvastatin_keywords,collapse="|"),x$allergy),]$IND_SEQ))
    rosuvastatin_cases$statin<-rep("rosuvastatin",nrow(rosuvastatin_cases))
    simvastatin_cases<-data.frame(IND_SEQ=unique(x[grep(paste(simvastatin_keywords,collapse="|"),x$allergy),]$IND_SEQ))
    simvastatin_cases$statin<-rep("simvastatin",nrow(simvastatin_cases))
    
    ## Create full list of all indidivudals who have a specified statin reaction
    individuals_with_statin_specific_allergy<-rbind(atorvastatin_cases,rbind(fluvastatin_cases,rbind(lovastatin_cases,rbind(pitavastatin_cases,rbind(pravastatin_cases,rbind(rosuvastatin_cases,simvastatin_cases))))))
    
    ## Identify any indviduals who do not have a specific statin listed
    ## Put the generic "statins" as their specific statin allergy
    individuals_with_general_statin_allergy<-data.frame(IND_SEQ=unique(x[!x$IND_SEQ%in%individuals_with_statin_specific_allergy$IND_SEQ,]$IND_SEQ))
    individuals_with_general_statin_allergy$statin<-rep("statins",nrow(individuals_with_general_statin_allergy))
    
    full_list<-rbind(individuals_with_general_statin_allergy,individuals_with_statin_specific_allergy)
    ## Return the complet list of statin specific and generic statin individuals
    return(full_list) 
  }
}
