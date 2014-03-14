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

  not_myopathy_keywords_testset<-c("diarrhea","nausea","black spots","lft","breath","liver","gi ","headache","dizz","cough","rash","edema","swollen","ha","h/a","qhs","every night","bedtime","hallucinations","proctitis","flatulence","stool","nosebleeds","swelling","hives","pancreatitis","constipation","skin crawls","sore throat","eczema","sweaty","stomach","flushed","urinary","heat","transaminases","qd","hepatitis","urination","gastrointestinal","light headed","increased bp","night terrors","daily","hepatic","hepatotoxicity","anaphylaxis","n/v","every day","increased ed","renal","blurred vision","hyperkalemia","anxiety","itching","night trerrors","bid","sob","night terrors","vomiting"," mg ","[0-9]+mg","pruritis","gi ","current medications","on.*therapy","mg\\.")
  myopathy_keywords<-c("myopathy","myalgia","weakness","cramp","myagia","arthralgia","mylagia","muscle pain","polyarthralgia","malaise","fatigue","rhabdomyolosis","muscle aches",paste(statin_list, " \\(intolerance\\)",sep=""),paste(statin_list, " \\(unknown\\)",sep=""),"pravachol  ","per med record"," ache","pain","weak","muscle","myositis","fatigue","malaise","rhabdomyolosis")

  filter_by_statin<-x[grep(paste(statin_list,collapse="|"),tolower(x$allergy)),]
  filter_by_notmyopathy<-filter_by_statin[grep(paste(not_myopathy_keywords,collapse="|"),filter_by_statin$allergy),]
  filter_notmyopathy_by_myopathy<-filter_by_notmyopathy[grep(paste(myopathy_keywords,collapse="|"),filter_by_notmyopathy$allergy,invert=TRUE),]
  statin_myopathy<-filter_by_statin[!filter_by_statin$allergy%in%filter_notmyopathy_by_myopathy$allergy,]
  return(statin_myopathy)
}
