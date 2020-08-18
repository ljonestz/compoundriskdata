#--------------------LOAD PACKAGES--------------
#install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(ggplot2, cowplot, lubridate, rvest,dplyr, viridis, tidyverse, countrycode, clipr, sjmisc, xlsx)

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

#-----------------------------------CREATE A FLAG SUMMARY SHEET---------------------------------------
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")

#Add existing and emerging risk scores
riskflags <- globalrisk %>%
  mutate(EXISTING_RISK_COVID_RESPONSE_CAPACITY = H_HIS_Score_norm,
         EXISTING_RISK_FOOD_SECURITY = F_Proteus_Score_norm,
         EXISTING_RISK_CONFLICT = C_GPI_Score_norm,
         EXISTING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID = M_Economic_and_Financial_score_norm, 
         EXISTING_RISK_FISCAL = D_WB_Overall_debt_distress_norm,
         EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY = S_OCHA_Covid.vulnerability.index_norm,
         EXISTING_RISK_NATURAL_HAZARDS = NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS_norm,
         EXISTING_RISK_FRAGILITY_INSTITUTIONS = pmax(Fr_INFORM_Fragility_Score_norm, 
                                                     Fr_FSI_Score_norm, 
                                                     na.rm=T),
         EMERGING_RISK_COVID_RESPONSE_CAPACITY = pmax(H_Oxrollback_score_norm, 
                                                      H_Covidgrowth_casesnorm,
                                                      H_Covidgrowth_deathsnorm,
                                                      H_Covidproj_Projected_Deaths_._1M_norm, 
                                                      na.rm=T),
         EMERGING_RISK_FOOD_SECURITY = case_when(!is.na(F_Fewsnet_Score_norm) ~ pmax(F_Fewsnet_Score_norm,
                                                                                     F_Artemis_Score_norm, 
                                                                                     na.rm=T),
                                                 TRUE ~ F_FAO_6mFPV_norm),
         EMERGING_RISK_CONFLICT = pmax(C_ACLED_event_same_month_difference_perc_norm,
                                       C_ACLED_fatal_same_month_difference_perc_norm,
                                       na.rm=T),
         EMERGING_RISK_FISCAL = D_IMF_debt2020.2019_norm,
         EMERGING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID = pmax(M_GDP_IMF_2019minus2020_norm,
                                                              M_GDP_WB_2019minus2020_norm, 
                                                              na.rm=T),
         EMERGING_RISK_NATURAL_HAZARDS = pmax(NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS_norm,
                                              NH_GDAC_Hazard_Score_Norm, 
                                              NH_INFORM_Crisis_Norm, 
                                              na.rm = T),
         EMERGING_RISK_FRAGILITY_INSTITUTIONS = pmax(Fr_FSI_2019minus2020_norm, 
                                                    Fr_REIGN_couprisk3m_norm, 
                                                    ifelse(NH_INFORM_CRISIS_Type==3, 10, 0),
                                                    na.rm=T)) %>%
  select(Country,EXISTING_RISK_COVID_RESPONSE_CAPACITY,EXISTING_RISK_FOOD_SECURITY,
         EXISTING_RISK_CONFLICT, EXISTING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID,
         EXISTING_RISK_FISCAL, EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY,
         EXISTING_RISK_NATURAL_HAZARDS,EXISTING_RISK_FRAGILITY_INSTITUTIONS,
         EMERGING_RISK_COVID_RESPONSE_CAPACITY, EMERGING_RISK_CONFLICT,
         EMERGING_RISK_CONFLICT,EMERGING_RISK_FISCAL,
         EMERGING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID,EMERGING_RISK_NATURAL_HAZARDS,
         EMERGING_RISK_FRAGILITY_INSTITUTIONS)

#Create tertiary risk flags
vars <- c("EXISTING_RISK_COVID_RESPONSE_CAPACITY", "EXISTING_RISK_FOOD_SECURITY", 
          "EXISTING_RISK_CONFLICT", "EXISTING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID", 
          "EXISTING_RISK_FISCAL", "EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY", 
          "EXISTING_RISK_NATURAL_HAZARDS", "EXISTING_RISK_FRAGILITY_INSTITUTIONS", 
          "EMERGING_RISK_COVID_RESPONSE_CAPACITY", "EMERGING_RISK_CONFLICT", 
          "EMERGING_RISK_FISCAL", "EMERGING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID", 
          "EMERGING_RISK_NATURAL_HAZARDS", "EMERGING_RISK_FRAGILITY_INSTITUTIONS")

riskflags[paste0(vars, "_RISKLEVEL")] <- lapply(riskflags[vars], function(tt){
  ifelse(tt >= 0 & tt < 7, "Low risk",
         ifelse(tt >= 7 & tt < 10, "Medium risk",
                ifelse(tt == 10, "High risk",
                       NA)
                )
         )
})

#Calculate total compound risk scores
riskflags$TOTAL_EXISTING_COMPOUND_RISK_SCORE <- as.numeric(unlist(row_count(riskflags, 
                                                          EXISTING_RISK_COVID_RESPONSE_CAPACITY:EXISTING_RISK_FRAGILITY_INSTITUTIONS,
                                                          count=10,
                                                          append = F)))
riskflags$TOTAL_EMERGING_COMPOUND_RISK_SCORE <- as.numeric(unlist(row_count(riskflags, 
                                                          EMERGING_RISK_CONFLICT:EMERGING_RISK_FRAGILITY_INSTITUTIONS,
                                                          count=10,
                                                          append = F)))
riskflags$medium_risk_existing <- as.numeric(unlist(row_count(riskflags, 
                                   EMERGING_RISK_CONFLICT_RISKLEVEL:EMERGING_RISK_FRAGILITY_INSTITUTIONS_RISKLEVEL,
                                   count="Medium risk",
                                   append = F)))
riskflags$medium_risk_emerging <- as.numeric(unlist(row_count(riskflags, 
                                   EMERGING_RISK_CONFLICT_RISKLEVEL:EMERGING_RISK_FRAGILITY_INSTITUTIONS_RISKLEVEL,
                                   count="Medium risk",
                                   append = F)))
riskflags$TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM <- as.numeric(unlist(riskflags$TOTAL_EXISTING_COMPOUND_RISK_SCORE + (riskflags$medium_risk_existing/2)))
riskflags$TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM <- as.numeric(unlist(riskflags$TOTAL_EMERGING_COMPOUND_RISK_SCORE + (riskflags$medium_risk_emerging/2)))
riskflags <- riskflags %>% select(-medium_risk_emerging, -medium_risk_existing)
         
#----------------------------CRATE SUMMARY SHEET----------------------------------------------------
write.csv(riskflags, "Risk_Sheets/Compound_Risk_Flags_Sheet.csv")

#Create Excel
crxls <- createWorkbook()
addWorksheet(crxls, "riskflags", tabColour = "lightgrey")
writeData(crxls, "riskflags", riskflags, colNames = TRUE)
addWorksheet(crxls, "conflictsheet", tabColour = "red")
writeData(crxls, "conflictsheet", conflictsheet, colNames = TRUE)
addWorksheet(crxls, "debtsheet", tabColour = "lightblue")
writeData(crxls, "debtsheet", debtsheet, colNames = TRUE)
addWorksheet(crxls, "foodsecurity", tabColour = "lightgreen")
writeData(crxls, "foodsecurity", foodsecurity, colNames = TRUE)
addWorksheet(crxls, "fragilitysheet", tabColour = "orange")
writeData(crxls, "fragilitysheet", fragilitysheet, colNames = TRUE)
addWorksheet(crxls, "healthsheet", tabColour = "yellow")
writeData(crxls, "healthsheet", healthsheet, colNames = TRUE)
addWorksheet(crxls, "macrosheet", tabColour = "lightpink")
writeData(crxls, "macrosheet", macrosheet, colNames = TRUE)
addWorksheet(crxls, "Naturalhazardsheet", tabColour = "brown")
writeData(crxls, "Naturalhazardsheet", Naturalhazardsheet, colNames = TRUE)
addWorksheet(crxls, "Socioeconomic_sheet", tabColour = "lightblue")
writeData(crxls, "Socioeconomic_sheet", Socioeconomic_sheet, colNames = TRUE)

#Colour and stlye sheets
Map(function(number, tab){
headerStyle <- createStyle(
  fontSize = 14, fontColour = "#FFFFFF",textDecoration = "bold", halign = "center",
  fgFill = "darkblue", border = "TopBottom", borderColour = "white"
)
addStyle(crxls, sheet = number, headerStyle, rows = 1, cols = 1:50, gridExpand = TRUE)
bodyStyle <- createStyle(border = "TopBottom", borderColour = "white", halign = "center")
addStyle(crxls, sheet = number, bodyStyle, rows = 2:50, cols = 1:250, gridExpand = TRUE)
setColWidths(crxls, 1, cols = 1, widths = 21) ## set column width for row names column
modifyBaseFont(crxls, fontSize = 12, fontColour = "black", fontName = "Arial")
}, c(1:9))

saveWorkbook(crxls, file = "Risk_sheets/Compound_Risk_Flags_Sheet.xlsx", overwrite = TRUE)





  
