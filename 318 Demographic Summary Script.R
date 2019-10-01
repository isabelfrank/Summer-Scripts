# Demgographics Summary for Reports
# Anna Durbin Lab

# 19 July 2019
# Written by Isabel Frank

#This file is meant to return a data frame with the number of vaccinated and
# placebo patients as well as how many were on and off study. To make use of 
# this script, place the .csv file with Demographics with Vaccine Information.
# Make sure you have an empty environment to avoid misleading variables. Then,
# press command + return from top to bottom; this adds  functions and objects
# to the global space. After executing the last line, the Demographics 
# Summary will open in a new tab as a data frame.

#This is the file that R will read and extract data from.  Column headers
# must be 1 row long and the exact number of columns as the data table.
# Make sure you are in the proper working directory before continuing.
data <- read.csv("318 Demographics with Vaccine Information (JHU).csv", 
                 header = T, stringsAsFactors = F)


#The getter functions below will return the number of the number of placebo 
# volunteers and the number of vaccinated volunteers respectively. This is 
# a simple number, not a list, nor a subset.

getNumPlac <- function(df= data) {
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

getNumVacc <- function(df = data) {
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

makeDemographicSummary <- function(df = data){
  # Returns: a data frame with the headers "On Study Vaccine",
  # "Off Study Vaccine", "On Study Placebo", and "Off Study Placebo".
  # Each cell holds a numeric value.
  
  # This function calculates the number of placebo/vaccine patients as well as 
  # the number of on and off study patients. These values are put into a into 
  # a data frame that this function returns. 
  
  # Parameters: df is a .csv file stored as an object with the method
  # read.csv(). The header is one column and only one column. The 
  # number of columns matches the data.
  
  plac_patients <- subset(df, VaccDose == "Placebo 0.5 mL")
  num_plac <- getNumPlac()
  
  vacc_patients <- subset(df, VaccDose == "rZIKV/D4D30-713 0.5 mL")
  num_vacc <- getNumVacc()
  
  offstudy_vacc_patients <- subset(vacc_patients, Study.Status == "Off Study")
  num_offstudy_vacc_patients <- dim(offstudy_vacc_patients)[1]
  
  offstudy_plac_patients <- subset(plac_patients, Study.Status == "Off Study")
  num_offstudy_plac_patients <- dim(offstudy_plac_patients)[1]
  
  num_onstudy_vacc_patients <- num_vacc - num_offstudy_vacc_patients
  
  num_onstudy_plac_patients <- num_plac - num_offstudy_plac_patients
  
  chart <- data.frame("On Study Vaccine" = num_onstudy_vacc_patients, 
                      "Off Study Vaccine" = num_offstudy_vacc_patients, 
                      "On Study Placebo" = num_onstudy_plac_patients, 
                      "Off Study Placebo" = num_offstudy_plac_patients)
  chart
}

# After all the functions are defined, makeDemographicSummary() is
# executed to make the data frame. To view the dataframe in 
# another tab outside the console, execute
# View(makeDemographicSummary()) To create a .csv file with the data frame, 
# execute the write.csv(...) line and the file will appear in the last folder 
# that is in the path.

makeDemographicSummary()
write.csv(makeDemographicSummary(), 
          "/Users/cirguest/Desktop/318 Scripts and Spreadsheets /318DemographicSummary.csv", row.names = F)
View(makeDemographicSummary())
