#AE Tables Post Dose by Treatment
# Anna Durbin Lab

# 22 July 2019
# Written by Isabel Frank

#This file is meant to return a data frame with AEs Post Dose By Treatement.
# To make use of this script, place the .csv file with Demographics with
# Vaccine Information and the .csv file with all the AEs. Make sure you have an
# environment to avoid misleading variables. Then, press command + return from
# top to bottom; this adds functions and objects to the global space. After
# executing the last line, the AEs Post Dose by Treatment table will open as a
# data frame.

#These are the files that R will read and extract data from.  Column headers
# must be 1 row long and the exact number of columns as the data table.
# Make sure you are in the proper working directory before continuing.

data318_ae <- read.csv("318 AEs and Other Problems (JHU).csv", header = T, stringsAsFactors = FALSE)
data318_demo <- read.csv("318 Demographics with Vaccine Information (JHU).csv", 
                 header = T, stringsAsFactors = F)

#Below are the getter and setter functions for obtaining the number of 
# vaccinated and placebo patients. This is a simple number, not a list, nor
# a subset.

getNumPlac <- function(df= data318_demo) {
  # Returns the total number of placebo patients in the study.
  
  # By default, this function will read the data file above, but can be
  # overidden by putting in a different a different data frame as an
  # argument when calling the function. A subset is created by
  # extracting each patient that has "Placebo 0.5 ml" as their 
  # VaccDose. Then, the length of the vector of subject 
  # numbers is calculated (repeats are extracted).
  
  plac_patients <- subset(df, VaccDose == "Placebo 0.5 mL")
  num_plac <- length(unique(plac_patients$Subject.))
  num_plac
}

getNumVacc <- function(df = data318_demo) {
  # Returns the total number of vaccinated patients in the study.
  
  # By default, this function will read the data file above, but can be
  # overidden by putting in a different a different data frame as an
  # argument when calling the function. A subset is created by
  # extracting each patient that has "rZIKV/D4D30-713 0.5 ml" as their 
  # VaccDose. Then, the length of the vector of subject 
  # numbers is calculated (repeats are extracted).
  
  vacc_patients <- subset(df, VaccDose == "rZIKV/D4D30-713 0.5 mL")
  num_vacc <- length(unique(vacc_patients$Subject.))
  num_vacc
}

#Below is the function that returns the row of a data frame for a
# a given AE term. This function will work for any AE. Note, type in the AE
# carefully because it will not catch an error. It will return 0 placebos
# and 0 vaccinated and 1.00 for Fisher.

getNumbersForAETerm <- function(df = data318_ae, term = "Fatigue", type = "Local"){
  # Returns: a data frame with the headers "Type", "Term", "Vaccine", "Placebo",
  # and "Fisher". The "Type" and "Term" columns are meant to hold characters.
  # "Vaccine" and "Placebo" hold a factor with the form
  # <number patients vaccinated who experienced the AE>, " (",
  # <proportion of those vaccinated patients>, " )". "Fisher" contains the
  # Fisher value for the proportions as a numeric.
  
  # This function calculates the number of placebo and vaccine patients as well
  # as proportion of those who experienced a particular AE. These values are
  # put into a into a data frame that this function returns.
  
  # Parameters: df is a .csv file stored as an object with the method
  # read.csv(). The header is one column and only one column. The 
  # number of columns matches the data. term is a character for the term of AE.
  # type is a character for the type of AE.
  
  all_term <- subset(df, grepl(term, AE.Term, ignore.case = T))
  
  plac_term <- subset(all_term, grepl("Placebo", Relationship, ignore.case = T))
  num_placebo_term <- length(unique(plac_term$Volunteer))
  plac_prop <- num_placebo_term / getNumPlac()
  no_term_plac <- getNumPlac() - num_placebo_term
  
  vacc_term <- subset(all_term, grepl("rZIKV", Relationship, ignore.case = T))
  num_vacc_term <- length(unique(vacc_term$Volunteer))
  vacc_prop <- num_vacc_term / getNumVacc()
  no_term_vacc <- getNumVacc() - num_vacc_term
  
  term_matrix <- matrix(c(num_vacc_term, num_placebo_term, no_term_vacc, no_term_plac), nrow =2, ncol = 2, byrow = F)
  fisher_val <- fisher.test(term_matrix, alternative = "greater")$p.value
  
  vacc_prop <- vacc_prop * 100
  plac_prop <- plac_prop * 100
  vacc_prop <- paste(vacc_prop, "%", sep = "")
  plac_prop <- paste(plac_prop, "%", sep = "")
  fisher_val <- format(fisher_val, nsmall = 3, digits = 3)
  
  data_row <- data.frame("Type" = type, "AE" = term, "rZIKV/D4D30-713 n = 20" = paste(num_vacc_term, " (", vacc_prop, ")", sep = ""), 
                         "Placebo n = 8" = paste(num_placebo_term, " (",plac_prop, ")", sep = ""), 
                         "Fisher" = fisher_val)
  data_row
}

#Below are all the functions used to put all of the rows together. The 
# functions sew together smaller data frames into each AE Type. Then , they
# are all combined into one table. To view the data frame, execute the line
# View(allAETypeRows()) To create a .csv file with the data frame, 
# execute the write.csv(...) line and the file will appear in the last folder 
# that is in the path.

combineSystemicAERows <- function() {
  # Returns: Data frame with all number/proportions of vaccinated and placebo
  # patients who experienced each systemic AE. 
  
  # This function returns a data frame with each systemic AE. If you wish to
  # add a systemic AE, add the line of code: (no #)
  # ae_table <- rbind(ae_table, getNumbersForAETerm(type, term))
  # If you wish to not have an AE appear, then simply put a # in front of
  # that line of code to comment it out. 
  
  ae_table <- NULL 
  ae_table <- rbind(ae_table, getNumbersForAETerm(type = "Systemic", term = "Fever"))
  ae_table <- rbind(ae_table, getNumbersForAETerm(type = "Systemic", term = "Headache"))
  ae_table <- rbind(ae_table, getNumbersForAETerm(type = "Systemic", term = "Retro-orbital Pain"))
  ae_table <- rbind(ae_table, getNumbersForAETerm(type = "Systemic", term = "Photophobia"))
  ae_table <- rbind(ae_table, getNumbersForAETerm(type = "Systemic", term = "Nausea"))
  ae_table <- rbind(ae_table, getNumbersForAETerm(type = "Systemic", term = "Fatigue"))
  ae_table <- rbind(ae_table, getNumbersForAETerm(type = "Systemic", term = "Myalgia"))
  ae_table <- rbind(ae_table, getNumbersForAETerm(type = "Systemic", term = "Arthralgia"))
  ae_table <- rbind(ae_table, getNumbersForAETerm(type = "Systemic", term = "Non-purlent Conjuctivitis"))
  ae_table <- rbind(ae_table, getNumbersForAETerm(type = "Systemic", term = "Muscle Weakness"))
  ae_table <- rbind(ae_table, getNumbersForAETerm(type = "Systemic", term = "Zika Virus-like Rash"))

}



combineLaboratoryAEs <- function() {
  # Returns: Data frame with all number/proportions of vaccinated and placebo
  # patients who experienced each laboratory AE. 
  
  # This function returns a data frame with each laboratory AE. If you wish to
  # add a systemic AE, add the line of code: (no #)
  # ae_table <- rbind(ae_table, getNumbersForAETerm(type, term))
  # If you wish to not have an AE appear, then simply put a # in front of
  # that line of code to comment it out.
  ae_table <- NULL 
  ae_table <- rbind(ae_table, getNumbersForAETerm(type = "Laboratory", term = "Dectreased Hemoglobin"))
  ae_table <- rbind(ae_table, getNumbersForAETerm(type = "Laboratory", term = "Neutropenia"))
  ae_table <- rbind(ae_table, getNumbersForAETerm(type = "Laboratory", term = "Dectreased Hemoglobin"))
  ae_table <- rbind(ae_table, getNumbersForAETerm(type = "Laboratory", term = "Elevated ALT"))
  ae_table <- rbind(ae_table, getNumbersForAETerm(type = "Laboratory", term = "Thrombocytopenia"))
}

combineLocalAEs <- function() {
  # Returns: Data frame with all number/proportions of vaccinated and placebo
  # patients who experienced each local AE. 
  
  # This function returns a data frame with each local AE. If you wish to
  # add a systemic AE, add the line of code: (no #)
  # ae_table <- rbind(ae_table, getNumbersForAETerm(type, term))
  # If you wish to not have an AE appear, then simply put a # in front of
  # that line of code to comment it out.
  ae_table <- NULL 
  ae_table <- rbind(ae_table, getNumbersForAETerm(type = "Local", term = "Injection Site Pain"))
  ae_table <- rbind(ae_table, getNumbersForAETerm(type = "Local", term = "Injection Site Erythema"))
  ae_table <- rbind(ae_table, getNumbersForAETerm(type = "Local", term = "Injection Site Tenderness"))
  ae_table <- rbind(ae_table, getNumbersForAETerm(type = "Local", term = "Injection Site Induration"))
}

allAETypeRows <- function(){
  # Returns a data frame with all AEs of each type with their
  # number/proportions of placebo/vaccinated patients as well as the Fisher
  # values.
  
  # This function returns the full AEs Post Dose by Treatment table.
  ae_table <- NULL
  ae_table <- rbind(ae_table, combineSystemicAERows(), combineLaboratoryAEs(), combineLocalAEs())
}

sortAETypesByFisher <- function(df = NA){
  df_by_type <- df[order(df$Type, df$Fisher), ]
  df_by_type
}

sortedAllAEByTypeFisher <- function(){
  all_ae <- allAETypeRows()
  sortAETypesByFisher(all_ae)
}

write.csv(sortedAllAEByTypeFisher(), 
          "/Users/cirguest/Desktop/318 Scripts and Spreadsheets /318AETable.csv", row.names = F)
View(sortedAllAEByTypeFisher())

