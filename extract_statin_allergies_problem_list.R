############################################################
## Functions to Extract Statin Myopathy from Problem Lists
##
## Created by Laura Wiley
## March 13, 2014
## R v3.0.2
##
## extract_statin_myopathy_pl_allergy_original_list() - keywords from training set of 300 records from Vanderbilt's deidentified electronic medical record database. These records were not reviewed manually to make an absolute determination of case/control status.
##
## extract_statin_myopathy_pl_allergy_corected_list() - keywords contain both those in the original list, and those that were found in another 300 records from the same database. This set of records had their entire record reviewed and contained a total of 124 confirmed stain myotoxicity cases and 176 controls.
##
## Each function: 
##		1. first extracts statins from the allergy list
##		2. removes allergies where the listed symptom is clearly not myopathy/muscle toxicity related
##		3. Returns off target reactions that also have myotoxicity reaction to the myotoxicity allegy listing.
##		
##  Input: x is a data frame that has a column called "allergy" that ideally has one drug per line.
##	Returns: dataframe that has filtered out any entries where the "allergy" column does not match the criteria described above.
##
##
## extract_specific_statins() - takes output from either previous function and extracts which statins are mentioned in the allergy.  If a specific statin (e.g. lipitor or atorvastatin) are not mentioned, then a general "statins" (to indicate class allergy) is returned.
## This function:
##    1. Extracts all trade and generic names of statins.
##    2. Individuals lacking a specific trade or generic statin name are assigned "statins" as an indicator of a class effect
## Input: x is a dataframe that has at least two columns: IND_SEQ (the individual identifier) and allergy (the filtered allergy text from either of the previous two functions, additional columns are ignored.
## Output: a two column dataframe: IND_SEQ (the individual identifier) and statin (the specific statin or "statins" that the patient had listed in the allergy text).  If patients have recorded allergies to more than one statin they will have multiple entries (e.g. multiple rows), one for each statin)
############################################################


extract_statin_myopathy_pl_allergy_original_list<-function(x){
  statin_list<-c("atorvastin","fluvastatin","lovastatin","pitavastatin","pravastatin","rosuvastatin","simvastatin"," statin ","statins","hmg","lipitor","caduet","lescol","mevacor","altocor","altoprev","livalo","pitava","pravachol","crestor","zocor","vytorin","simcor")
  
  not_myopathy_keywords<-c("lft","liver","swelling","hives","headache","night trerrors","night terrors","nausea","vomiting","mg ","rash","pruritis","gi ","current medications","constipation","renal","on.*therapy","mg\\.")
  myopathy_keywords<-c(paste(statin_list, " \\(intolerance\\)",sep=""),paste(statin_list, " \\(unknown\\)",sep=""),"pravachol  ","cramp","myalgia","per med record","ache","pain","weak","muscle","myositis","fatigue","malaise","rhabdomyolosis")
 
  filter_by_statin<-x[grep(paste(statin_list,collapse="|"),tolower(x$allergy)),]
  filter_by_notmyopathy<-filter_by_statin[grep(paste(not_myopathy_keywords,collapse="|"),filter_by_statin$allergy),]
  filter_notmyopathy_by_myopathy<-filter_by_notmyopathy[grep(paste(myopathy_keywords,collapse="|"),filter_by_notmyopathy$allergy,invert=TRUE),]
  statin_myopathy<-filter_by_statin[!filter_by_statin$allergy%in%filter_notmyopathy_by_myopathy$allergy,]
  return(statin_myopathy)
}

extract_statin_myopathy_pl_allergy_corected_list<-function(x){
  statin_list<-c("atorvastin","fluvastatin","lovastatin","pitavastatin","pravastatin","rosuvastatin","simvastatin"," statin ","statins","hmg","lipitor","caduet","lescol","mevacor","altocor","altoprev","livalo","pitava","pravachol","crestor","zocor","vytorin","simcor")

  not_myopathy_keywords<-c("diarrhea","nausea","black spots","lft","breath","liver","gi ","headache","dizz","cough","rash","edema","swollen","ha","h/a","qhs","every night","bedtime","hallucinations","proctitis","flatulence","stool","nosebleeds","swelling","hives","pancreatitis","constipation","skin crawls","sore throat","eczema","sweaty","stomach","flushed","urinary","heat","transaminases","qd","hepatitis","urination","gastrointestinal","light headed","increased bp","night terrors","daily","hepatic","hepatotoxicity","anaphylaxis","n/v","every day","increased ed","renal","blurred vision","hyperkalemia","anxiety","itching","night trerrors","bid","sob","night terrors","vomiting"," mg ","[0-9]+mg","pruritis","gi ","current medications","on.*therapy","mg\\.")
  myopathy_keywords<-c("myopathy","myalgia","weakness","cramp","myagia","arthralgia","mylagia","muscle pain","polyarthralgia","malaise","fatigue","rhabdomyolosis","muscle aches",paste(statin_list, " \\(intolerance\\)",sep=""),paste(statin_list, " \\(unknown\\)",sep=""),"pravachol  ","per med record"," ache","pain","weak","muscle","myositis","fatigue","malaise","rhabdomyolosis")

  filter_by_statin<-x[grep(paste(statin_list,collapse="|"),tolower(x$allergy)),]
  filter_by_notmyopathy<-filter_by_statin[grep(paste(not_myopathy_keywords,collapse="|"),filter_by_statin$allergy),]
  filter_notmyopathy_by_myopathy<-filter_by_notmyopathy[grep(paste(myopathy_keywords,collapse="|"),filter_by_notmyopathy$allergy,invert=TRUE),]
  statin_myopathy<-filter_by_statin[!filter_by_statin$allergy%in%filter_notmyopathy_by_myopathy$allergy,]
  return(statin_myopathy)
}

extract_specific_statins<-function(x){
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
  rosuva statin_cases$statin<-rep("rosuvastatin",nrow(rosuvastatin_cases))
  simvastatin_cases<-data.frame(IND_SEQ=unique(x[grep(paste(simvastatin_keywords,collapse="|"),x$allergy),]$IND_SEQ))
  simvastatin_cases$statin<-rep("simvastatin",nrow(simvastatin_cases))
  
  ## Create full list of all indidivudals who have a specified statin reaction
  individuals_with_statin_specific_allergy<-rbind.fill(atorvastatin_cases,fluvastatin_cases,lovastatin_cases,pitavastatin_cases,pravastatin_cases,rosuvastatin_cases,simvastatin_cases)
  
  ## Identify any indviduals who do not have a specific statin listed
  ## Put the generic "statins" as their specific statin allergy
  individuals_with_general_statin_allergy<-data.frame(IND_SEQ=unique(x[!x$IND_SEQ%in%individuals_with_statin_specific_allergy$IND_SEQ,]$IND_SEQ))
  individuals_with_general_statin_allergy$statin<-rep("statins",nrow(individuals_with_general_statin_allergy))
  
  ## Return the complet list of statin specific and generic statin individuals
  return(rbind(individuals_with_statin_specific_allergy,individuals_with_general_statin_allergy))                                                    
}

