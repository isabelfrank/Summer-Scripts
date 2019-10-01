#Virolgy Tables
# Anna Durbin Lab

# 25 July 2019
# Written by Isabel Frank

#This file is meant to return a data frame with a virology summary.
# To make use of this script, place the .csv file with Virology
# Information. Make sure you have an empty environment to avoid misleading
# variables. Then, press command + return from top to bottom; this adds
# functions and objects to the global space. After executing the last line,
# the Virology Summary table will open as a data frame.

#This is the file that R will read and extract data from.  Column headers
# must be 1 row long and the exact number of columns as the data table.
# Make sure you are in the proper working directory before continuing.

data323_viro <- read.csv("323 Virology JHU Multi Test Unblinded Only CT.csv",
                         header = T, stringsAsFactors = F)

getStudyDays <- function(){
  #Returns a character vector of all of the study day headers with titer values
  # in the cells.
  
  #This function is meant to not have to constantly rewrite this list when 
  # needed in other parts of the script.
  
  study_days <- c("X.Vaccine.Day..DAY.0", "DAY.4......1.day.", 
                  "DAY.6......1.day.", "DAY.8......1.day.", 
                  "DAY.10......1.day.", "DAY.12......1.day.",
                  "DAY.14......1.day.", "DAY.16......1.day.",
                  "DAY.21......1.day.", "X.Challenge.Day..DAY.28...7..3.Days.",
                  "DAY.32......1.day.", "DAY.34......1.day.",
                  "DAY.36......1.day.", "DAY.38......1.day.",
                  "DAY.40......1.day.", "DAY.42......1.day.",
                  "DAY.44......1.day.", "DAY.49......1.day.",
                  "DAY.56......2.day.")
  study_days
}

extractPlaceboGroup <- function(df = data323_viro){
  #Returns a dataframe of all placebo patients.
  
  # This function returns a dataframe of all placebo subjects.
  
  # df is a .csv file that contains virology information.
  placebo_table <- subset(df, Dose == "Placebo 0.5 mL")
  placebo_table
}

getTotalChallenged <- function(challenge = NA){
  #Returns the total number of patients challenge (includes positive and
  # negative virermia)
  
  # This function searches for either "rDEN2" or "rDEN3" in Dose.2 and counts
  # the number of subjects. 
  
  # challenge is the challenge group. Either insert "Dengue 2" or "Dengue 3"
  # with quotes
  unique_string <- NA
  df <- extractPlaceboGroup()
  if(challenge == "Dengue 2"){
    unique_string <- "rDEN2"
  } else if(challenge == "Dengue 3"){
    unique_string <- "rDEN3"
  }
  challenged_subset <- subset(df, grepl(unique_string, Dose.2), !is.na(Dose.2))
  subjects <- challenged_subset$Subject..
  length(unique(subjects))
}

getPercentChallenged <- function(challenge = NA){
  #Returns the percent of patients who tested positive for virermia in a 
  # challenge group.(in the form of a decimal)
  
  # challenge is the challenge group. Either insert "Dengue 2" or "Dengue 3"
  # with quotes
  
  percent <- getNumberVirermic(challenge) / getTotalChallenged(challenge)
  percent
}

formatPercentChallenged <- function(challenge = NA){
  #Returns a formatted form of the percent of virermic patients. The format is
  # "(<percent>%)"
  
  # challenge is the challenge group. Either insert "Dengue 2" or "Dengue 3"
  # with quotes
  percent <- format(getPercentChallenged(challenge)*100,
                    digits = 3, nsmall = 0)
  percent <- paste("(", percent,"%",")", sep = "")
  percent
}

extractPlaceboDengue2 <- function(){
  #Returns a data frame with virology for subjects challenged with Dengue 2
  # and their resulting titers. 
  
  # This function uses getStudyDays() for the headers, and then captures
  # those challenged with Dengue 2 using grepl("rDEN2", Dose.2)
  
  study_days <- getStudyDays()
  placebo_group <- extractPlaceboGroup()
  dengue2_group <- subset(placebo_group,
                          grepl("rDEN2", Dose.2), !is.na(Dose.2))
  dengue2_group[,c("Subject..", study_days)]
}

extractPlaceboDengue3 <- function(){
  #Returns a data frame with virology for subjects challenged with Dengue 3
  # and their resulting titers.
  
  # This function uses getStudyDays() for the headers, and then captures those
  # challenged with Dengue 2 using grepl("rDEN3, Dose.2)
  
  study_days <- getStudyDays()
  placebo_group <- extractPlaceboGroup()
  dengue3_group <- subset(placebo_group,
                          grepl("rDEN3", Dose.2), !is.na(Dose.2))
  dengue3_group[, c("Subject..", study_days)]
}

getVirermicPatients <- function(challenge = NA){
  #Returns a character vector of all the subject numbers that tested positive
  # in a challenge group.
  
  # This function is makes a data frame that has all of the data for one group.
  # Then, for each subject for each study day, they will be added to the
  # virermic list if there is a titer that does not have "<0.5" nor is NA.
  # The unique entries are then returned as a character vector.
  
  # challenge is a string that is either "Dengue 2" or "Dengue 3"
  data_group <- NA
  virermic_subjects <- c()
  if (challenge == "Dengue 2"){
    data_group <- extractPlaceboDengue2()
  } else if (challenge == "Dengue 3"){
    data_group <- extractPlaceboDengue3()
  }
  
  study_days <- getStudyDays()
  for (subject in data_group$Subject..){
    for (day in study_days){
      if(!grepl("<0.5", data_group[day]) && !is.na(data_group[day])){
        virermic_subjects <- c(virermic_subjects, subject)
      }
    }
  }
  virermic_subjects <- unique(virermic_subjects)
  virermic_subjects
}

getPatientTiters <- function(challenge = NA){
  #Returns a numeric vector of the positive titers for each patient who
  # tested positive. 
  
  # This fuction creates a data frame based on whether the challenge is 
  # "Dengue 2" or "Dengue 3". A virermic patient vector is created by calling
  # getVirermicPatients(challenge). For each positive subject, all their titers
  # are put into a vector for each day. These titers are then casted as numeric
  # to then take the max. The maxes for each subject are added to a numeric
  # vector that is returned.
  
  # challenge is either "Dengue 2" or "Dengue 3"
  
  data_group <- NA
  patient_vector <- NA
  virermic_subjects <- c()
  
  study_days <- getStudyDays()
  
  if (challenge == "Dengue 2"){
    data_group <- extractPlaceboDengue2()
  } else if (challenge == "Dengue 3"){
    data_group <- extractPlaceboDengue3()
  }
  patient_vector <- getVirermicPatients(challenge)
  
  max_titer_per_patient <- c()
  for (subject in patient_vector){
    patient_df <- subset(data_group, Subject.. == subject)
    all_patient_titers <- c()
    for (day in study_days){
        all_patient_titers <- c(all_patient_titers, patient_df[day])
    }
    
    all_patient_titers <- unlist(all_patient_titers, recursive = T)
    all_patient_titers <- gsub("\\(.*?\\)","", all_patient_titers)
    all_patient_titers <- all_patient_titers[!grepl("<0.5", 
                              all_patient_titers) & !(all_patient_titers == "") 
                              & !(all_patient_titers == "n/a")]
    all_patient_titers <- as.numeric(all_patient_titers)
    max_titer_per_patient <- c(max_titer_per_patient, max(all_patient_titers))
  }

  max_titer_per_patient
}

getMeanTiter <- function(challenge = NA){
  #Returns the mean titer among all the patients. (Their maximums) in a
  # challenge group.
  
  # This function gets all the positive max patient titers and calculates the
  # mean.
  
  # challenge is either "Dengue 2" or "Dengue 3"
  
  titers <- NA
  titers <- getPatientTiters(challenge)
  mean(titers)
}

getSETiter <- function(challenge = NA){
  # Returns the standard error of the mean of max titers in a challenge
  # group.
  
  # This function first calculates the standard deviation of the all subjects'
  # max titers. Then, the standard deviation is divided by the square root of
  # the number of patients. This value is returned.
  
  # challenge is either "Dengue 2" or "Dengue 3"
  
  titers <- NA
  titers <- getPatientTiters(challenge)
  sd_titer <- sd(titers)
  num_patients <- getNumberVirermic(challenge)
  se_titer <- sd_titer / sqrt(num_patients)
  se_titer
}

formatSETiter <- function(challenge = NA){
  # Returns a formatted version of the standard error of the mean of the titer.
  
  # This function returns the decimal with 2 significant figures with a minimum
  # of 2 decimal places.
  
  # challenge is either "Dengue 2" or "Dengue 3"
  
  format(getSETiter(challenge), digits = 2, nsmall = 2)
}

getMaxTiter <- function(challenge = NA){
  #Returns the max of all titers.
  
  # This function gets all the maximums from each patient and returns the 
  # maximum value.
  
  # challenge is either "Dengue 2" or "Dengue 3"
  
  titers <- NA
  titers <- getPatientTiters(challenge)

  max(titers)
}

formatMaxTiter <- function(challenge = NA){
  #Returns a formatted form of the max titer.
  
  # This function formats the max titer to 3 significant digits and 2 decimal 
  # place minimum.
  
  # challenge is either "Dengue 2" or "Dengue 3"
  
  format(getMaxTiter(challenge), digits = 3, nsmall = 2)
}

getOnSetDays <- function(challenge = NA){
  #Returns a numeric vector of the the onset day for each patient who
  # tested positive. 
  
  # This function initially creates a data frame by challenge. Next,
  # a for loop is created going through each patient and making a vector that 
  # has all titers. Then, the vector is shrunken down to only having the 
  # positive titers for the patient. The day with the minimum is added to
  # a vector that keeps track of the first positive days for each patient.
  # This vector gets returned as numeric.
  
  # challenge is either "Dengue 2" or "Dengue 3"
  
  data_group <- NA
  patient_vector <- NA
  
  study_days <- getStudyDays()
  
  if (challenge == "Dengue 2"){
    data_group <- extractPlaceboDengue2()
  } else if (challenge == "Dengue 3"){
    data_group <- extractPlaceboDengue3()
  }
  
  patient_vector <- getVirermicPatients(challenge)
  first_positive_day_per_patient <- c()
  for (subject in patient_vector){
    patient_df <- subset(data_group, Subject.. == subject)
    all_patient_positive_days <- c()
    for (day in study_days){
      all_patient_positive_days <- c(all_patient_positive_days, patient_df[day])
    }
    
    all_patient_positive_days <- unlist(all_patient_positive_days,
                                       recursive = T)
    all_patient_positive_days <- all_patient_positive_days[!grepl("<0.5", 
                                       all_patient_positive_days) 
                                       & !(all_patient_positive_days == "") & 
                                       !(grepl("n/a", all_patient_positive_days))
                                       & substr(all_patient_positive_days, 1, 1) != "("]
    all_patient_positive_days <- all_patient_positive_days
    all_patient_positive_days <- gsub(".*\\((.*)\\).*", "\\1", all_patient_positive_days)
    first_positive_day_per_patient <- c(first_positive_day_per_patient,
                                        min(as.numeric(all_patient_positive_days)))
  }
  first_positive_day_per_patient
} #Look at this again

getOffSetDays <- function(challenge = NA){
  #Returns a numeric vector of the the offset day for each patient who
  # tested positive. 
  
  # A data frame for the challenge group and a vector of positive subjects is
  # created. For every subject, the max is taken for all study days and added
  # to a vector of last positive days. The last positives are returned. 
  
  # challenge is either "Dengue 2" or "Dengue 3"
  
  data_group <- NA
  patient_vector <- NA
  
  study_days <- getStudyDays()
  
  if (challenge == "Dengue 2"){
    data_group <- extractPlaceboDengue2()
    patient_vector <- getVirermicPatients("Dengue 2")
  } else if (challenge == "Dengue 3"){
    data_group <- extractPlaceboDengue3()
    patient_vector <- getVirermicPatients("Dengue 3")
  }
  last_positive_day_per_patient <- c()
  for (subject in patient_vector){
    patient_df <- subset(data_group, Subject.. == subject)
    all_patient_positive_days <- c()
    for (day in study_days){
      all_patient_positive_days <- c(all_patient_positive_days, patient_df[day])
    }
    
    all_patient_positive_days <- unlist(all_patient_positive_days, recursive = T)
    all_patient_positive_days <- all_patient_positive_days[!grepl("<0.5",
                                    all_patient_positive_days) 
                                    & !(all_patient_positive_days == "")
                                    & !(grepl("n/a", all_patient_positive_days))
                                    & substr(all_patient_positive_days, 1, 1) != "("]
    all_patient_positive_days <- all_patient_positive_days
    all_patient_positive_days <- gsub(".*\\((.*)\\).*", "\\1", all_patient_positive_days)
    last_positive_day_per_patient <- c(last_positive_day_per_patient,
                                    max(as.numeric(all_patient_positive_days)))
  }
  last_positive_day_per_patient 
} #Look at this again

getOnSetMean <- function(challenge = NA){
  #Returns the mean of all onset days for virermic subjects.
  
  #challenge is either "Dengue 2" or "Dengue 3"
  
  on_set_nums <- getOnSetDays(challenge)
  mean(on_set_nums)
}

formatOnSetMean <- function(challenge = NA){
  #Returns the formatted mean of the onset day of virermia.
  
  # This function takes the mean and formats it to 2 significant figures with
  # a minimum of 0 decimal places
  
  # challenge is either "Dengue 2" or "Dengue 3"
  
  mean <- getOnSetMean(challenge)
  format(mean, digits = 2, nsmall = 0)
}

getDuration <- function(challenge = NA){
  #Returns the duration of each virermic subject for how long they were positve
  
  # This function subtracts the onset and off set days for each patient and
  # returns a numeric vector for duration
  
  # challenge is either "Dengue 2" or "Dengue 3"
  
  on_set_nums <- getOnSetDays(challenge)
  off_set_nums <- getOffSetDays(challenge)
  
  duration <- off_set_nums - on_set_nums + 1
  duration
}

getMeanDuration <- function(challenge = NA){
  #Returns mean of all durations from a challenge group.
  
  # challenge is either "Dengue 2" or "Dengue 3"
  
  duration <- getDuration(challenge)
  mean(duration)
}

formatMeanDuration <- function(challenge = NA){
  #Returns a formatted mean duration
  
  # This function formats the mean to have 2 significant figures
  # a minimum of 2 decimal places
  
  # challenge is either "Dengue 2" or "Dengue 3"
  
  duration <- getMeanDuration(challenge)
  format(duration, digits= 2, nsmall = 0)
}

getOnSetRange <- function(challenge = NA){
  #Returns a range vector for onset days for virermic subjects
  
  # challenge is either "Dengue 2" or "Dengue 3"
  
  on_set_days <-getOnSetDays(challenge)
  range(on_set_days)
}

formatOnSetRange <- function(challenge = NA){
  #Returns the formatted range of onset days.
  
  # This function returns the range in the form "( <minimum>-<maximum>)"
  
  # challenge is either "Dengue 2" or "Dengue 3"
  
  my_range <- getOnSetRange(challenge)
  range_string <- NA
  range_string <- paste("(", my_range[1], "-", my_range[2], ")", sep = "")
  range_string
}

getDurationRange <- function(challenge = NA){
  #Returns a numeric vector for the duration range.
  
  # challenge is either "Dengue 2" or "Dengue 3"
  
  durations <- getDuration(challenge)
  range(durations)
}

formatDurationRange <- function(challenge = NA){
  #Returns the formatted range for virermia.
  
  # This function returns the range in the form of "(<minimum>-<maximum>)
  
  # challenge is either "Dengue 2" or "Dengue 3"
  
  dur_range <- getDurationRange(challenge)
  paste("(", dur_range[1], "- ", dur_range[2], ")", sep = "")
}

getNumberVirermic <- function(challenge = NA){
  #Returns the number of virermic patients.
  
  #This function uses a character vector of virermic patients
  # and returns the length as the number virermic.
  
  # challenge is either "Dengue 2" or "Dengue 3"
  
  patient_vector <- getVirermicPatients(challenge)
  length(patient_vector)
}

#Below are the functions to make a data fram for virology. The functions below
# call the other functions to obtain all the value to then be put ine the 
# data frame. To view the data frame, execute the line View(makeViroTable())
# To create a .csv file with the data frame, execute the write.csv(...)
# line and the file will appear in the last folder that is in the path.

makeViroTable <- function(){
  #Returns a data frame for virology.
  
  # This function creates a data frame for virology with the columns 
  # "Challenge", "n Virermic (%)", "Mean +- SE", "Max Titer", "Mean Day Onset",
  # and "Mean Duration".
  
  challenges <- c("Dengue 2", "Dengue 3")
  num_viro <- c(getNumberVirermic("Dengue 2"), getNumberVirermic("Dengue 3"))
  percent_viro <- c(formatPercentChallenged("Dengue 2"),
                    formatPercentChallenged("Dengue 3"))
  mean_titer <- c(getMeanTiter("Dengue 2"), getMeanTiter("Dengue 3"))
  se_titer <- c(formatSETiter("Dengue 2"), formatSETiter("Dengue 3"))
  max_titer <- c(formatMaxTiter("Dengue 2"), formatMaxTiter("Dengue 3"))
  mean_on_set <- c(formatOnSetMean("Dengue 2"), formatOnSetMean("Dengue 3"))
  range_on_set <- c(formatOnSetRange("Dengue 2"), formatOnSetRange("Dengue 3"))
  mean_duration <- c(formatMeanDuration("Dengue 2"),
                     formatMeanDuration("Dengue 3"))
  range_duration <- c(formatDurationRange("Dengue 2"),
                      formatDurationRange("Dengue 3"))
  #The \u00b1 is the plus minus symbol. That is just how you type it in for
  # R to display.
  data.frame("Challenge" = challenges,
             "n Virermic (%)" = paste(num_viro, percent_viro),
             "Mean \u00b1 SE" = paste(mean_titer, "\u00b1", se_titer),
             "Max Titer" = max_titer, 
             "Mean Day Onset (Range)" = paste(mean_on_set, range_on_set),
             "Mean Duration" = paste(mean_duration, range_duration))
}

write.csv(makeViroTable(), 
          "/Users/cirguest/Desktop/323ViroSummary.csv", row.names = F)
View(makeViroTable())

