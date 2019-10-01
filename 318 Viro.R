#318 Virology by Test
# Anna Durbin Lab

# 7 August 2019
# Written by Isabel Frank

#This file is meant to return a data frame with a virology by Test.
# To make use of this script, place the each .csv file with Virology
# Information from each test and specimen. Make sure you have an empty
# environment to avoid misleading variables. Then, press command + return
# from top to bottom; this adds functions and objects to the global space.
# After executing the last line, the Virology Summary table will open
# as a data frame. Note: the .csv files may only have one and only one row of 
# headers. 

cvs_pcr318 <-read.csv("318 CVS Virus PCR JHU Multi Test Unblinded Only CT.csv",
                      header = T, stringsAsFactors = F)

cvs_culture318 <-read.csv("318 CVS Viral Culture JHU Multi Test Unblinded Only CT.csv",
                          header = T, stringsAsFactors = F)

semen_pcr318 <-read.csv("318 Semen Virus PCR JHU Multi Test Unblinded Only CT.csv",
                        header = T, stringsAsFactors = F)

semen_culture318 <-read.csv("318 Semen Viral Culture JHU Multi Test Unblinded Only CT.csv",
                            header = T, stringsAsFactors = F)

serum_pcr318 <-read.csv("318 Serum Virus PCR JHU Multi Test Unblinded Only CT.csv",
                        header = T, stringsAsFactors = F)

serum_culture318 <-read.csv("318 Serum Viral Culture JHU Multi Test Unblinded Only CT.csv",
                            header = T, stringsAsFactors = F)

urine_pcr318 <-read.csv("318 Urine Virus PCR JHU Multi Test Unblinded Only CT.csv",
                        header = T, stringsAsFactors = F)

urine_culture318 <-read.csv("318 Urine Viral Culture JHU Multi Test Unblinded Only CT.csv",
                            header = T, stringsAsFactors = F)


#CVS PCR: These functions below are used to compute the number of virermic
# subjects for CVS PCR.
getStudyDaysCVSPCR <- function(){
#Returns a vector of character strings of all header names.

# This function is used to avoid incorrectly repetitavely rewriting all of the 
# study days. 
  study_days <- c("DAY.0..Vaccine.Day.", "DAY.8......1.", "DAY.16......1.", 
                  "DAY.28......2.", "DAY.56....14." , "DAY.90....10.",
                  "DAY.150....14.", "DAY.180....28...14." )
  study_days
}

extractByChallengeCVSPCR <- function(challenge = NA){
#Returns a data frame with the subset of only the rows of the particular
# challenge.
  
# challenge is either "Zika" or "Placebo"
  if (challenge == "Zika"){
    df <- subset(cvs_pcr318, Dose == "rZIKV/D4D30-713 0.5 mL")
  } else if (challenge == "Placebo"){
    df <- subset(cvs_pcr318, Dose == "Placebo 0.5 mL")
  }
  df
}

getVirermicSubjectsCVSPCR <- function(challenge = NA){
#Returns the NUMBER of subjects who were virermic

# challenge is either "Zika" or "Placebo"
  df <- extractByChallengeCVSPCR(challenge)
  virermic_subjects <- c()
  study_days <- getStudyDaysCVSPCR()
  for (subject in df$Subject..){
    subject_df <- subset(df, Subject.. == subject)
    all_personal_titers <- c()
    for (day in study_days){
      all_personal_titers <- c(all_personal_titers, subject_df[day])
    }
    all_personal_titers <- unlist(all_personal_titers, recursive = T)
    all_personal_titers <- gsub("\\(.*?\\)","", all_personal_titers)
    all_personal_titers <- gsub("log 10 genome copies/ml", "", all_personal_titers)
    all_personal_titers <- all_personal_titers[!grepl("<3.24", all_personal_titers) ]
    all_personal_titers <- all_personal_titers[all_personal_titers != ""
                           & as.numeric(all_personal_titers[!is.na(all_personal_titers)]) > 0]
    if (length(all_personal_titers) != 0){
      virermic_subjects <- c(virermic_subjects, subject)
    }

  }
  length(unique(virermic_subjects))
}

#CVS Viral Culture: These functions below are used to compute the number of
# virermic subjects for CVS Viral Culture.
getStudyDaysCVSVC <- function(){
#Returns a vector of character strings of all header names.
  
# This function is used to avoid incorrectly repetitavely rewriting all of the 
# study days.
  study_days <- c("DAY.0..Vaccine.Day.", "DAY.8......1.", "DAY.16......1.",
                  "DAY.28......2.", "DAY.56....14.", "DAY.90....10.",
                  "DAY.150....14." , "DAY.180....28...14.")
  study_days
}

extractByChallengeCVSVC <- function(challenge = NA){
#Returns a data frame with the subset of only the rows of the particular
# challenge.
  
# challenge is either "Zika" or "Placebo"
  if (challenge == "Zika"){
    df <- subset(cvs_culture318, Dose == "rZIKV/D4D30-713 0.5 mL")
  } else if (challenge == "Placebo"){
    df <- subset(cvs_culture318, Dose == "Placebo 0.5 mL")
  }
  df
}

getVirermicSubjectsCVSVC <- function(challenge = NA){
#Returns the NUMBER of subjects who were virermic
  
# challenge is either "Zika" or "Placebo"
  df <- extractByChallengeCVSVC(challenge)
  virermic_subjects <- c()
  study_days <- getStudyDaysCVSVC()
  for (subject in df$Subject..){
    subject_df <- subset(df, Subject.. == subject)
    all_personal_titers <- c()
    for (day in study_days){
      all_personal_titers <- c(all_personal_titers, subject_df[day])
    }
    all_personal_titers <- unlist(all_personal_titers, recursive = T)
    all_personal_titers <- gsub("\\(.*?\\)","", all_personal_titers)
    all_personal_titers <- all_personal_titers[!grepl("<0.5", all_personal_titers) ]
    all_personal_titers <- all_personal_titers[all_personal_titers != ""
                            & as.numeric(all_personal_titers[!is.na(all_personal_titers)]) > 0]
    if (length(all_personal_titers) != 0){
      virermic_subjects <- c(virermic_subjects, subject)
    }
    
  }
  length(unique(virermic_subjects))
}

#Semen PCR: These functions below are used to compute the number of virermic
# subjects for Semen PCR.
getStudyDaysSemenPCR <- function(){
#Returns a vector of character strings of all header names.
  
# This function is used to avoid incorrectly repetitavely rewriting all of the 
# study days.
  study_days <- c("DAY.0..Vaccine.Day.", "DAY.8......1.", "DAY.16......1.", 
                  "DAY.28......2.", "DAY.56....14.", "DAY.90....10.",
                  "DAY.150....14.", "DAY.180....28...14.")
  study_days
}

extractByChallengeSemenPCR <- function(challenge = NA){
#Returns a data frame with the subset of only the rows of the particular
# challenge.
  
# challenge is either "Zika" or "Placebo"
  if (challenge == "Zika"){
    df <- subset(semen_pcr318, Dose == "rZIKV/D4D30-713 0.5 mL")
  } else if (challenge == "Placebo"){
    df <- subset(semen_pcr318, Dose == "Placebo 0.5 mL")
  }
  df
}  

getVirermicSubjectsSemenPCR <- function(challenge = NA){
#Returns the NUMBER of subjects who were virermic
  
# challenge is either "Zika" or "Placebo"
  df <- extractByChallengeSemenPCR(challenge)
  virermic_subjects <- c()
  study_days <- getStudyDaysSemenPCR()
  for (subject in df$Subject..){
    subject_df <- subset(df, Subject.. == subject)
    all_personal_titers <- c()
  for (day in study_days){
    all_personal_titers <- c(all_personal_titers, subject_df[day])
  }
  all_personal_titers <- unlist(all_personal_titers, recursive = T)
  all_personal_titers <- gsub("\\(.*?\\)","", all_personal_titers)
  all_personal_titers <- gsub("log 10 genome copies/ml", "", all_personal_titers)
  all_personal_titers <- all_personal_titers[!grepl("<3.24", all_personal_titers)]
  all_personal_titers <- all_personal_titers[all_personal_titers != "" 
                          & as.numeric(all_personal_titers[!is.na(all_personal_titers)]) > 0]
  if (length(all_personal_titers) != 0){
    virermic_subjects <- c(virermic_subjects, subject)
  }

}
length(unique(virermic_subjects))
}

#Semen Viral Culture: These functions below are used to compute the number of 
# virermic subjects for Semen PCR.
getStudyDaysSemenVC <- function(){
#Returns a vector of character strings of all header names.
  
# This function is used to avoid incorrectly repetitavely rewriting all of the 
# study days.
  study_days <- c("DAY.0..Vaccine.Day.", "DAY.8......1.", "DAY.16......1.",               
                  "DAY.28......2.", "DAY.56....14.", "DAY.90....10.",
                  "DAY.150....14.", "DAY.180....28...14.")
  study_days
}

extractByChallengeSemenVC <- function(challenge = NA){
#Returns a data frame with the subset of only the rows of the particular
# challenge.
  
# challenge is either "Zika" or "Placebo"
    if (challenge == "Zika"){
    df <- subset(cvs_culture318, Dose == "rZIKV/D4D30-713 0.5 mL")
  } else if (challenge == "Placebo"){
    df <- subset(cvs_culture318, Dose == "Placebo 0.5 mL")
  }
  df  
}

getVirermicSubjectsSemenVC <- function(challenge = NA){
#Returns the NUMBER of subjects who were virermic
  
# challenge is either "Zika" or "Placebo"
  df <- extractByChallengeSemenVC(challenge)
  virermic_subjects <- c()
  study_days <- getStudyDaysSemenVC()
  for (subject in df$Subject..){
    subject_df <- subset(df, Subject.. == subject)
    all_personal_titers <- c()
    for (day in study_days){
      all_personal_titers <- c(all_personal_titers, subject_df[day])
    }
    all_personal_titers <- unlist(all_personal_titers, recursive = T)
    all_personal_titers <- gsub("\\(.*?\\)","", all_personal_titers)
    all_personal_titers <- all_personal_titers[!grepl("<0.5", all_personal_titers)]
    all_personal_titers <- all_personal_titers[!grepl("n/a", all_personal_titers)]
    all_personal_titers <- all_personal_titers[all_personal_titers != "" 
                            & as.numeric(all_personal_titers[!is.na(all_personal_titers)]) > 0]
    if (length(all_personal_titers) != 0){
      virermic_subjects <- c(virermic_subjects, subject)
    }
    
  }
  length(unique(virermic_subjects))
}

#Serum PCR: These functions below are used to compute the number of virermic
# subjects for Serum PCR.
getStudyDaysSerumPCR <- function(){
#Returns a vector of character strings of all header names.
  
# This function is used to avoid incorrectly repetitavely rewriting all of the 
# study days.
  study_days <- c("DAY.0..Vaccine.Day.", "DAY.4......1.", "DAY.6......1.", 
                  "DAY.8......1.", "DAY.10......1.", "DAY.12......1.",
                  "DAY.14......1.", "DAY.16......1.", "DAY.21......1.", 
                  "DAY.28......2.", "DAY.56....14.", "DAY.90....10.",
                  "DAY.150....14.")
  study_days
}

extractByChallengeSerumPCR <- function(challenge = NA){
#Returns a data frame with the subset of only the rows of the particular
# challenge.
  
# challenge is either "Zika" or "Placebo"
  if (challenge == "Zika"){
    df <- subset(serum_pcr318, Dose == "rZIKV/D4D30-713 0.5 mL")
  } else if (challenge == "Placebo"){
    df <- subset(serum_pcr318, Dose == "Placebo 0.5 mL")
  }
  df  
}

getVirermicSubjectsSerumPCR <- function(challenge = NA){
#Returns the NUMBER of subjects who were virermic
  
# challenge is either "Zika" or "Placebo"
  df <- extractByChallengeSerumPCR(challenge)
  virermic_subjects <- c()
  study_days <- getStudyDaysSerumPCR()
  for (subject in df$Subject..){
    subject_df <- subset(df, Subject.. == subject)
    all_personal_titers <- c()
    for (day in study_days){
      all_personal_titers <- c(all_personal_titers, subject_df[day])
    }
    all_personal_titers <- unlist(all_personal_titers, recursive = T)
    all_personal_titers <- gsub("\\(.*?\\)","", all_personal_titers)
    all_personal_titers <- gsub("log 10 genome copies/ml", "", all_personal_titers)
    all_personal_titers <- all_personal_titers[!grepl("<3.24", all_personal_titers)]
    all_personal_titers <- all_personal_titers[!grepl("n/a", all_personal_titers)]
    all_personal_titers <- all_personal_titers[all_personal_titers != "" 
                            & as.numeric(all_personal_titers[!is.na(all_personal_titers)]) > 0]
    if (length(all_personal_titers) != 0){
      virermic_subjects <- c(virermic_subjects, subject)
    }

  }
  length(unique(virermic_subjects))
}

#Serum Culture: These functions below are used to compute the number of
# virermic subjects for Serum Viral culture.
getStudyDaysSerumVC <- function(){
#Returns a vector of character strings of all header names.
  
# This function is used to avoid incorrectly repetitavely rewriting all of the 
# study days.
  study_days <- c("DAY.0..Vaccine.Day.", "DAY.4......1.", "DAY.6......1.", 
                  "DAY.8......1.", "DAY.10......1.", "DAY.12......1.",
                  "DAY.14......1.", "DAY.16......1.", "DAY.21......1.", 
                  "DAY.28......2.", "DAY.56....14.", "DAY.90....10.",
                  "DAY.150....14.")
}

extractByChallengeSerumVC <- function(challenge = NA){
#Returns a data frame with the subset of only the rows of the particular
# challenge.
  
# challenge is either "Zika" or "Placebo"
  if (challenge == "Zika"){
    df <- subset(serum_culture318, Dose == "rZIKV/D4D30-713 0.5 mL")
  } else if (challenge == "Placebo"){
    df <- subset(serum_culture318, Dose == "Placebo 0.5 mL")
  }
  df   
}

getVirermicSubjectsSerumVC <- function(challenge = NA){
#Returns the NUMBER of subjects who were virermic
  
# challenge is either "Zika" or "Placebo"
  df <- extractByChallengeSemenVC(challenge)
  virermic_subjects <- c()
  study_days <- getStudyDaysSemenVC()
  for (subject in df$Subject..){
    subject_df <- subset(df, Subject.. == subject)
    all_personal_titers <- c()
    for (day in study_days){
      all_personal_titers <- c(all_personal_titers, subject_df[day])
    }
    all_personal_titers <- unlist(all_personal_titers, recursive = T)
    all_personal_titers <- gsub("\\(.*?\\)","", all_personal_titers)
    all_personal_titers <- all_personal_titers[!grepl("<0.5", all_personal_titers)]
    all_personal_titers <- all_personal_titers[!grepl("n/a", all_personal_titers)]    
    all_personal_titers <- all_personal_titers[all_personal_titers != "" 
                            & as.numeric(all_personal_titers[!is.na(all_personal_titers)]) > 0]
    if (length(all_personal_titers) != 0){
      virermic_subjects <- c(virermic_subjects, subject)
    }
    
  }
  length(unique(virermic_subjects)) 
}

#Urine PCR: These functions below are used to compute the number of virermic
# subjects for Urine Viral culture.
getStudyDaysUrinePCR <- function(){
#Returns a vector of character strings of all header names.
  
# This function is used to avoid incorrectly repetitavely rewriting all of the 
# study days.
  study_days <- c("DAY.0..Vaccine.Day.", "DAY.4......1.", "DAY.6......1.", 
                  "DAY.8......1.", "DAY.10......1.", "DAY.12......1." ,    
                  "DAY.14......1.", "DAY.16......1.", "DAY.21......1.",
                  "DAY.28......2.", "DAY.56....14.", "DAY.90....10.",      
                  "DAY.150....14.", "DAY.180....28...14.")
}

extractByChallengeUrinePCR <- function(challenge = NA){
#Returns a data frame with the subset of only the rows of the particular
# challenge.
  
# challenge is either "Zika" or "Placebo"
  if (challenge == "Zika"){
    df <- subset(urine_pcr318, Dose == "rZIKV/D4D30-713 0.5 mL")
  } else if (challenge == "Placebo"){
    df <- subset(urine_pcr318, Dose == "Placebo 0.5 mL")
  }
  df  
}

getVirermicSubjectsUrinePCR <- function(challenge = NA){
#Returns the NUMBER of subjects who were virermic
  
# challenge is either "Zika" or "Placebo"
  df <- extractByChallengeUrinePCR(challenge)
  virermic_subjects <- c()
  study_days <- getStudyDaysUrinePCR()
  for (subject in df$Subject..){
    subject_df <- subset(df, Subject.. == subject)
    all_personal_titers <- c()
    for (day in study_days){
      all_personal_titers <- c(all_personal_titers, subject_df[day])
    }
    all_personal_titers <- unlist(all_personal_titers, recursive = T)
    all_personal_titers <- gsub("\\(.*?\\)","", all_personal_titers)
    all_personal_titers <- gsub("log 10 genome copies/ml", "", all_personal_titers)
    all_personal_titers <- all_personal_titers[!grepl("<3.24", all_personal_titers)]
    all_personal_titers <- all_personal_titers[!grepl("n/a", all_personal_titers)]
    all_personal_titers <- all_personal_titers[all_personal_titers != "" 
                            & as.numeric(all_personal_titers[!is.na(all_personal_titers)]) > 0]
    if (length(all_personal_titers) != 0){
      virermic_subjects <- c(virermic_subjects, subject)
    }
    
  }
  length(unique(virermic_subjects))
}

#Urine Culture: These functions below are used to compute the number of 
# virermic subjects for Urine viral culture.
getStudyDaysUrineVC <- function(){
#Returns a vector of character strings of all header names.
  
# This function is used to avoid incorrectly repetitavely rewriting all of the 
# study days.
  study_days <- c("DAY.0..Vaccine.Day.", "DAY.4......1.", "DAY.6......1.",               
                  "DAY.8......1.", "DAY.10......1.", "DAY.12......1.",
                  "DAY.14......1.", "DAY.16......1.", "DAY.21......1.",
                  "DAY.28......2.", "DAY.56....14.", "DAY.90....10.",
                  "DAY.150....14.", "DAY.180....28...14.")
}

extractByChallengeUrineVC <- function(challenge = NA){
#Returns a data frame with the subset of only the rows of the particular
# challenge.
  
# challenge is either "Zika" or "Placebo"
  if (challenge == "Zika"){
    df <- subset(urine_culture318, Dose == "rZIKV/D4D30-713 0.5 mL")
  } else if (challenge == "Placebo"){
    df <- subset(urine_culture318, Dose == "Placebo 0.5 mL")
  }
  df  
}

getVirermicSubjectsUrineVC <- function(challenge = NA){
#Returns the NUMBER of subjects who were virermic
  
# challenge is either "Zika" or "Placebo"
  df <- extractByChallengeUrineVC(challenge)
  virermic_subjects <- c()
  study_days <- getStudyDaysUrineVC()
  for (subject in df$Subject..){
    subject_df <- subset(df, Subject.. == subject)
    all_personal_titers <- c()
    for (day in study_days){
      all_personal_titers <- c(all_personal_titers, subject_df[day])
    }
    all_personal_titers <- unlist(all_personal_titers, recursive = T)
    all_personal_titers <- gsub("\\(.*?\\)","", all_personal_titers)
    all_personal_titers <- all_personal_titers[!grepl("<0.5", all_personal_titers) ]
    all_personal_titers <- all_personal_titers[!grepl("n/a", all_personal_titers)]
    all_personal_titers <- all_personal_titers[all_personal_titers != "" 
                            & as.numeric(all_personal_titers[!is.na(all_personal_titers)]) > 0]
    if (length(all_personal_titers) != 0){
      virermic_subjects <- c(virermic_subjects, subject)
    }
    
  }
  length(unique(virermic_subjects)) 
}

#The functions below are used to make the final data frame. If you wish to 
# view the data frame, execute View(bindViroTables()). Conversely, if you wish
# to export the data frame, execute the line
# write.csv(bindViroTables(), "/Users/cirguest/Desktop/323VirologyByTest.csv",
# row.names =F)

makeSmallViroTable <- function(challenge = NA){
#Returns a small table of the specimen, test and number virermic for a
# particular test.
  
# challenge is either "Zika" or "Placebo"
  dose_vector <- rep(challenge, 8)
  specimen_vector <- c("CVS", "CVS", "Semen", "Semen", "Serum","Serum","Urine", "Urine")
  test_vector <- rep(c("Viral Culture", "Virus PCR"), 4)
  result_vector <- c(getVirermicSubjectsCVSVC(challenge),
                     getVirermicSubjectsCVSPCR(challenge), 
                     getVirermicSubjectsSemenVC(challenge),
                     getVirermicSubjectsSemenPCR(challenge),
                     getVirermicSubjectsSerumVC(challenge),
                     getVirermicSubjectsSerumPCR(challenge),
                     getVirermicSubjectsUrineVC(challenge),
                     getVirermicSubjectsUrinePCR(challenge))
  df <- data.frame("Dose" = dose_vector, "Specimen Type" = specimen_vector,
                   "Test" = test_vector, "Number Virermic" = result_vector)
  df
}

bindViroTables <- function(){
#Returns a small table of the specimen, test, and number virermic for both 
# Zika and Placebo.
  df <- NULL
  df <- rbind(df, makeSmallViroTable("Placebo"), makeSmallViroTable("Zika"))
  df
}

write.csv(bindViroTables(), 
          "/Users/cirguest/Desktop/318 Scripts and Spreadsheets /318VirologyByTest.csv", row.names = F)

View(bindViroTables())

