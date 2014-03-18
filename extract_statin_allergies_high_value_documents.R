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
