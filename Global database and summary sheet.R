#--------------------LOAD PACKAGES--------------
#install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(ggplot2, cowplot, lubridate, rvest,dplyr, viridis, tidyverse, countrycode, clipr)

#--------------------CREATE GLOBAL DATABASE WITH ALL RISK SHEETS-----------------
#Load risk sheets
healthsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/healthsheet.csv")
foodsecurity <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/foodsecuritysheet.csv")
debtsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/debtsheet.csv")
fragilitysheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/fragilitysheet.csv")
macrosheet <-  read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/macrosheet.csv")
Naturalhazardsheet <-  read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/Naturalhazards.csv")
Socioeconomic_sheet <-  read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/Socioeconomic_sheet.csv")

#Join datasets
globalrisk <- full_join(healthsheet, foodsecurity, by="Country") %>%
  left_join(., conflictsheet,  by="Country") %>% 
  left_join(., debtsheet, by="Country") %>% 
  left_join(., fragilitysheet, by="Country") %>% 
  left_join(., macrosheet, by="Country") %>% 
  left_join(., Naturalhazardsheet, by="Country") %>% 
  left_join(., Socioeconomic_sheet, by="Country") %>%
  select(-X.x, -X.y, -X.x.x, -X.y.y, -X.x.x.x, -X.y.y.y)

#Write as csv
write.csv(globalrisk, "Risk_sheets/Global_compound_risk_database.csv")




