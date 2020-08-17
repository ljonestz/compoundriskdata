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

#-----------------------------------CREATE A FLAG SUMMARY SHEET---------------------------------------
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")

riskflags <- globalrisk %>%
  mutate(EXISTING_RISK_COVID_RESPONSE_CAPACITY = H_HIS_Score_norm,
         EXISTING_RISK_FOOD_SECURITY = F_Proteus_Score_norm,
         EXISTING_RISK_CONFLICT = C_GPI_Score_norm,
         EXISTING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID = M_Economic_and_Financial_score_norm, 
         EXISTING_RISK_FISCAL = D_WB_Overall_debt_distress_norm,
         EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY = S_OCHA_Covid.vulnerability.index_norm,
         EXISTING_RISK_NATURAL_HAZARDS = NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS_norm,
         EXISINTG_RISK_FRAGILITY_INSTITUTIONS = pmax(Fr_INFORM_Fragility_Score_norm, 
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
         EMERGING_RISK_FRAGILITY_INSITUTIONS = pmax(Fr_FSI_2019minus2020_norm, 
                                                    Fr_REIGN_couprisk3m_norm, 
                                                    ifelse(NH_INFORM_CRISIS_Type==3, 10, 0),
                                                    na.rm=T)) %>%
  select(Country,EXISTING_RISK_COVID_RESPONSE_CAPACITY,EXISTING_RISK_FOOD_SECURITY,
         EXISTING_RISK_CONFLICT, EXISTING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID,
         EXISTING_RISK_FISCAL, EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY,
         EXISTING_RISK_NATURAL_HAZARDS,EXISINTG_RISK_FRAGILITY_INSTITUTIONS,
         EMERGING_RISK_COVID_RESPONSE_CAPACITY, EMERGING_RISK_CONFLICT,
         EMERGING_RISK_CONFLICT,EMERGING_RISK_FISCAL,
         EMERGING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID,EMERGING_RISK_NATURAL_HAZARDS,
         EMERGING_RISK_FRAGILITY_INSITUTIONS)





