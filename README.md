Statin Myopathy
==============

We developed two text based methods to identify statin induced myotoxicity from electronic medical records (EMRs):

1. Extract statin allergies from:
   *   Problem lists
   *   High value documents (history and physical, patient progress notes, etc.) processed with [SecTag](http://knowledgemap.mc.vanderbilt.edu/research/content/sectag-tagging-clinical-note-section-headers) to identify Allergy and Adverse Reactions sections.
2. Identify myotoxicity related keywords found within phrases containing statins extracted from:
   *   Clinical Communications (messages between providers or between providers and patients)
   *   High value documents (history and physical, patient progress notes, etc.)

These methods rely on filtering records by inclusion and exclusion keyword filters.  These can be customized to your institution's documentation practices as needed.  We provide two sets of lists for each algorithm:

1. **Original list** - These keywords were identified using a training set of 300 records from Vanderbilt's deidentified electronic medical record database.  These records were not reviewed manually to make an absolute determination of case/control status.
2. **Corrected list** - These keywords contain both those in the original list, and those that were found in another 300 records from the same database.  This set of records had their entire record reviewed and contained a total of 124 confirmed stain myotoxicity cases and 176 controls.

All methods were performed using R v3.0.2 and require the following libraries:
   *   [stringr](http://cran.r-project.org/web/packages/stringr/index.html)
   *   [plyr](http://cran.r-project.org/web/packages/plyr/index.html)
   *   [snowfall](http://cran.r-project.org/web/packages/snowfall/index.html) - for parallel processing large volumes of notes.
