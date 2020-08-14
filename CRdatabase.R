#--------------------LOAD PACKAGES--------------
#install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(ggplot2, cowplot, lubridate, rvest,dplyr, viridis, tidyverse, countrycode, clipr)

#--------------------EXISTING COMPOUND RISK-----------------

#Function to normalise with upper and lower bounds (when high score = low vulnerability)
normfuncneg <- function(df,upperrisk, lowerrisk, col1){
  #Create new column col_name as sum of col1 and col2
  df[[paste(col1, "_norm")]] <- ifelse(df[[col1]] >= upperrisk, 10,
                                       ifelse(df[[col1]] <= lowerrisk, 0,
                                              ifelse(df[[col1]]  < upperrisk & df[[col1]]  > lowerrisk,  10 - (upperrisk - df[[col1]] )/(upperrisk - lowerrisk)*10, NA)))
  df
}

#Function to normalise with upper and lower bounds (when high score = high vulnerability)
normfuncpos <- function(df,upperrisk, lowerrisk, col1){
  #Create new column col_name as sum of col1 and col2
  df[[paste(col1, "_norm")]] <- ifelse(df[[col1]] <= upperrisk, 10,
                                       ifelse(df[[col1]] >= lowerrisk, 0,
                                              ifelse(df[[col1]]  > upperrisk & df[[col1]]  < lowerrisk,  10 - (upperrisk - df[[col1]] )/(upperrisk - lowerrisk)*10, NA)))
  df
}

#--------------------Create health tab-----------------
HIS <- read.csv("~/Google Drive/PhD/R code/Compound Risk/Compound Risk/covid/compoundriskdata/health.csv", row.names=1)

HIS <- normfuncneg(HIS, 50, 20, "H_HIS_Score")

#-----------------------Oxford rollback score-----------------
OXrollback <- read.csv("~/Google Drive/PhD/R code/Compound Risk/Compound Risk/covid/compoundriskdata/Oxrollback.csv", row.names=1)

OXrollback <- normfuncneg(OXrollback, 0.3, 0.8, "H_OXrollback_score")

#-------------------------COVID deaths and cases--------------------






