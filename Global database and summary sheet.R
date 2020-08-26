#--------------------LOAD PACKAGES--------------
#install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(ggplot2, cowplot, lubridate, rvest,dplyr, compositions, viridis, tidyverse, countrycode, clipr, sjmisc, openxlsx, EnvStats)

#--------------------CREATE GLOBAL DATABASE WITH ALL RISK SHEETS-----------------
#Load risk sheets
healthsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/healthsheet.csv")
foodsecurity <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/foodsecuritysheet.csv")
debtsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/debtsheet.csv")
conflictsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/conflictsheet.csv")
fragilitysheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/fragilitysheet.csv")
macrosheet <-  read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/macrosheet.csv")
Naturalhazardsheet <-  read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/Naturalhazards.csv")
Socioeconomic_sheet <-  read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/Socioeconomic_sheet.csv")
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")

#Join datasets
globalrisk <- left_join(countrylist, healthsheet, by=c("Countryname", "Country")) %>%
  left_join(., foodsecurity, by=c("Countryname", "Country")) %>%
  left_join(., conflictsheet,  by=c("Countryname", "Country")) %>% 
  left_join(., debtsheet, by=c("Countryname", "Country")) %>% 
  left_join(., fragilitysheet, by=c("Countryname", "Country")) %>% 
  left_join(., macrosheet, by=c("Countryname", "Country")) %>% 
  left_join(., Naturalhazardsheet, by=c("Countryname", "Country")) %>% 
  left_join(., Socioeconomic_sheet,by=c( "Country")) %>%
  select(-X.x, -X.y, -X.x.x, -X.y.y, -X.x.x.x, -X.y.y.y, -X.x.x.x.x, -X.y.y.y.y) %>%
  distinct(Country, .keep_all = TRUE) %>%
  drop_na(Country)

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
                                                      H_new_cases_smoothed_per_million_norm,
                                                      H_new_deaths_smoothed_per_million_norm,
                                                      H_Covidproj_Projected_Deaths_._1M_norm, 
                                                      na.rm=T),
         EMERGING_RISK_FOOD_SECURITY = case_when(!is.na(F_Fewsnet_Score_norm) ~ pmax(F_FAO_6mFPV_norm,
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
         EMERGING_RISK_FRAGILITY_INSTITUTIONS = case_when(NH_INFORM_CRISIS_Type == "Complex crisis" ~ 10,
                                                         TRUE ~ pmax(Fr_FSI_2019minus2020_norm, 
                                                               Fr_REIGN_couprisk3m_norm,
                                                               na.rm=T)),
         EMERGING_RISK_FRAGILITY_INSTITUTIONS = case_when(EXISTING_RISK_FRAGILITY_INSTITUTIONS >=5 & NH_INFORM_CRISIS_Type==3 ~ 10,
                                                          TRUE  ~ EMERGING_RISK_FRAGILITY_INSTITUTIONS)) %>%
  select(Countryname, Country,EXISTING_RISK_COVID_RESPONSE_CAPACITY,EXISTING_RISK_FOOD_SECURITY,
         EXISTING_RISK_CONFLICT, EXISTING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID,
         EXISTING_RISK_FISCAL, EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY,
         EXISTING_RISK_NATURAL_HAZARDS,EXISTING_RISK_FRAGILITY_INSTITUTIONS,
         EMERGING_RISK_COVID_RESPONSE_CAPACITY,EMERGING_RISK_FOOD_SECURITY, EMERGING_RISK_CONFLICT,
         EMERGING_RISK_CONFLICT,EMERGING_RISK_FISCAL, EMERGING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID,
         EMERGING_RISK_NATURAL_HAZARDS, EMERGING_RISK_FRAGILITY_INSTITUTIONS)

#Create tertiary risk flags
vars <- c("EXISTING_RISK_COVID_RESPONSE_CAPACITY", "EXISTING_RISK_FOOD_SECURITY", 
          "EXISTING_RISK_CONFLICT", "EXISTING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID", 
          "EXISTING_RISK_FISCAL", "EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY", 
          "EXISTING_RISK_NATURAL_HAZARDS", "EXISTING_RISK_FRAGILITY_INSTITUTIONS", 
          "EMERGING_RISK_COVID_RESPONSE_CAPACITY", "EMERGING_RISK_FOOD_SECURITY", 
          "EMERGING_RISK_CONFLICT", "EMERGING_RISK_FISCAL", "EMERGING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID", 
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
                                                          EMERGING_RISK_COVID_RESPONSE_CAPACITY:EMERGING_RISK_FRAGILITY_INSTITUTIONS,
                                                          count=10,
                                                          append = F)))
riskflags$medium_risk_existing <- as.numeric(unlist(row_count(riskflags, 
                                   EMERGING_RISK_COVID_RESPONSE_CAPACITY_RISKLEVEL:EMERGING_RISK_FRAGILITY_INSTITUTIONS_RISKLEVEL,
                                   count="Medium risk",
                                   append = F)))
riskflags$medium_risk_emerging <- as.numeric(unlist(row_count(riskflags, 
                                   EMERGING_RISK_COVID_RESPONSE_CAPACITY_RISKLEVEL:EMERGING_RISK_FRAGILITY_INSTITUTIONS_RISKLEVEL,
                                   count="Medium risk",
                                   append = F)))
riskflags$TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM <- as.numeric(unlist(riskflags$TOTAL_EXISTING_COMPOUND_RISK_SCORE + (riskflags$medium_risk_existing/2)))
riskflags$TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM <- as.numeric(unlist(riskflags$TOTAL_EMERGING_COMPOUND_RISK_SCORE + (riskflags$medium_risk_emerging/2)))

#Drop teritiary rates (may want to reinstate in the future)
riskflags <- riskflags %>% 
  select(-medium_risk_emerging, -medium_risk_existing, -all_of(paste0(vars, "_RISKLEVEL"))) %>%
  distinct(Country, .keep_all = TRUE) %>%
  drop_na(Country)

#--------------------------------CREATE DATABASE OF ALTERNATIVE RISK SCORES------------------------------------------------------
#Alternative combined risk scores
names <- c("EMERGING_RISK_CONFLICT", "EMERGING_RISK_FRAGILITY_INSTITUTIONS", "EMERGING_RISK_FISCAL", "EMERGING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID")
riskflags[paste0(names,"_plus1")] <- lapply(riskflags[names], function(xx){ifelse(xx==0, xx+1, xx)})

riskflags$EMERGING_RISK_CONFLICT_MULTIDIMENSIONAL <- geometricmeanRow(riskflags[c("EMERGING_RISK_CONFLICT_plus1", "EMERGING_RISK_FRAGILITY_INSTITUTIONS_plus1", "EMERGING_RISK_FISCAL_plus1")], na.rm=T)
riskflags$EMERGING_RISK_FRAGILITY_INSTITUTIONS_MULTIDIMENSIONAL <- geometricmeanRow(riskflags[c("EMERGING_RISK_FRAGILITY_INSTITUTIONS_plus1", "EMERGING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID_plus1")], na.rm=T)

#remove unnecessary variables                                                     
riskflags <- riskflags %>% 
  select(-EMERGING_RISK_CONFLICT_plus1 ,-EMERGING_RISK_FRAGILITY_INSTITUTIONS_plus1 ,
           -EMERGING_RISK_FISCAL_plus1, -EMERGING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID_plus1)

#Alternativ combined total scores
altflag <- globalrisk
names <- c("S_OCHA_Covid.vulnerability.index_norm", "H_Oxrollback_score_norm", 
           "H_Covidgrowth_casesnorm", "H_Covidgrowth_deathsnorm", "H_HIS_Score_norm","H_new_cases_smoothed_per_million_norm", "H_new_deaths_smoothed_per_million_norm", 
           "F_Proteus_Score_norm", "F_Fewsnet_Score_norm", "F_Artemis_Score_norm", 
           "F_FAO_6mFPV_norm", "C_GPI_Score_norm", "C_ACLED_event_same_month_difference_perc_norm", 
           "C_ACLED_fatal_same_month_difference_perc_norm", "D_WB_Overall_debt_distress_norm", 
           "D_IMF_debt2020.2019_norm", "M_Economic_and_Financial_score_norm", 
           "M_GDP_IMF_2019minus2020_norm", "M_GDP_WB_2019minus2020_norm", 
           "NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS_norm", "NH_GDAC_Hazard_Score_Norm", 
           "Fr_INFORM_Fragility_Score_norm", "Fr_FSI_Score_norm", "Fr_FSI_2019minus2020_norm", 
           "Fr_REIGN_couprisk3m_norm", "H_Covidproj_Projected_Deaths_._1M_norm")

altflag[paste0(names,"_plus1")] <- lapply(altflag[names], function(xx){ifelse(xx==0, xx+1, xx)})

#Calculate alternative variables
altflag$EMERGING_RISK_COVID_RESPONSE_CAPACITY_AV <- geometricmeanRow(altflag[c("H_Oxrollback_score_norm_plus1", 
                                                                                "H_Covidgrowth_casesnorm_plus1",
                                                                                "H_Covidgrowth_deathsnorm_plus1",
                                                                               "H_new_cases_smoothed_per_million_norm_plus1",
                                                                               "H_new_deaths_smoothed_per_million_norm_plus1",
                                                                                "H_Covidproj_Projected_Deaths_._1M_norm_plus1")], na.rm=T)
altflag$EMERGING_RISK_CONFLICT_AV = geometricmeanRow(altflag[c("C_ACLED_event_same_month_difference_perc_norm_plus1",
                                                            "C_ACLED_fatal_same_month_difference_perc_norm_plus1")], na.rm=T)
altflag$EMERGING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID_AV = geometricmeanRow(altflag[c("M_GDP_IMF_2019minus2020_norm_plus1",
                                                                                      "M_GDP_WB_2019minus2020_norm_plus1")], na.rm=T)
altflag$EMERGING_RISK_FRAGILITY_INSTITUTIONS_AV <- ifelse(is.na(altflag$NH_INFORM_CRISIS_Type) | altflag$NH_INFORM_CRISIS_Type != "Complex crisis", geometricmeanRow(altflag[c("Fr_FSI_2019minus2020_norm_plus1", 
                                                                                                                                                                               "Fr_REIGN_couprisk3m_norm_plus1")], na.rm=T), 10)
#Merge datasets to include alt variables
riskflags <- inner_join(riskflags, altflag, by=c("Country", "Countryname"), keep=F)

#Calculate emerging risk score using existing risk 
riskflags <- riskflags %>%
  mutate(EMERGING_RISK_COVID_RESPONSE_CAPACITY_SQ = case_when(!is.na(EXISTING_RISK_COVID_RESPONSE_CAPACITY) ~ sqrt(EXISTING_RISK_COVID_RESPONSE_CAPACITY * EMERGING_RISK_COVID_RESPONSE_CAPACITY),
                                                              TRUE ~ EMERGING_RISK_COVID_RESPONSE_CAPACITY),
         EMERGING_RISK_FOOD_SECURITY_SQ = case_when(is.na(F_Fewsnet_Score) ~ sqrt(EXISTING_RISK_FOOD_SECURITY * EMERGING_RISK_FOOD_SECURITY),
                                                    TRUE ~ EMERGING_RISK_FOOD_SECURITY),
         EMERGING_RISK_CONFLICT_SQ = case_when(!is.na(EXISTING_RISK_CONFLICT) ~ sqrt(EXISTING_RISK_CONFLICT * EMERGING_RISK_CONFLICT),
                                               TRUE ~ EMERGING_RISK_CONFLICT),
         EMERGING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID_SQ  = case_when(!is.na(EXISTING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID) ~ sqrt(EXISTING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID * EMERGING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID),
                                                                       TRUE ~ EXISTING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID),
         EMERGING_RISK_FISCAL_SQ = case_when(!is.na(EXISTING_RISK_FISCAL) ~ sqrt(EXISTING_RISK_FISCAL * EMERGING_RISK_FISCAL),
                                                    TRUE ~ EMERGING_RISK_FISCAL),
         EMERGING_RISK_NATURAL_HAZARDS_SQ = EMERGING_RISK_NATURAL_HAZARDS,
         EMERGING_RISK_FRAGILITY_INSTITUTIONS_SQ = case_when(!is.na(EXISTING_RISK_FRAGILITY_INSTITUTIONS) ~ sqrt(EXISTING_RISK_FRAGILITY_INSTITUTIONS * EMERGING_RISK_FRAGILITY_INSTITUTIONS),
                                                                                                            TRUE ~ EMERGING_RISK_FRAGILITY_INSTITUTIONS)
)

#Calculate total emerging risk scores for SQ
sqnam <- c("EMERGING_RISK_COVID_RESPONSE_CAPACITY_SQ", "EMERGING_RISK_FOOD_SECURITY_SQ", 
  "EMERGING_RISK_CONFLICT_SQ", "EMERGING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID_SQ", 
  "EMERGING_RISK_FISCAL_SQ", "EMERGING_RISK_NATURAL_HAZARDS_SQ", 
  "EMERGING_RISK_FRAGILITY_INSTITUTIONS_SQ")

riskflags$TOTAL_EMERGING_COMPOUND_RISK_SCORE_SQ <- rowSums(riskflags[sqnam] >=7)

#-----------------------------CREATE RELIABILITY SCORES------------------------------------------
#Calculate the number of missing values in each of the source indicators for the various risk components (as a proportion)
reliabilitysheet <- globalrisk %>%
  mutate(RELIABILITY_EXISTING_COVID_RESPONSE_CAPACITY = case_when(is.na(H_HIS_Score_norm) ~ 1,
                                                                             TRUE ~ 0),
         RELIABILITY_EXISTING_FOOD_SECURITY = case_when(is.na(F_Proteus_Score_norm) ~ 1,
                                                              TRUE ~ 0),
         RELIABILITY_EXISTING_CONFLICT = case_when(is.na(C_GPI_Score_norm) ~ 1,
                                                         TRUE ~ 0),
         RELIABILITY_EXISTING_MACROECONOMIC_EXPOSURE_TO_COVID = case_when(is.na(M_Economic_and_Financial_score_norm) ~ 1,
                                                                                TRUE ~ 0),
         RELIABILITY_EXISTING_FISCAL = case_when(is.na(D_WB_Overall_debt_distress_norm) ~ 1,
                                                       TRUE ~ 0),
         RELIABILITY_EXISTING_SOCIOECONOMIC_VULNERABILITY = case_when(is.na(S_OCHA_Covid.vulnerability.index_norm) ~ 1,
                                                                            TRUE ~ 0),                                                                                     
         RELIABILITY_EXISTING_NATURAL_HAZARDS = case_when(is.na(NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS_norm)~1,
                                                          TRUE ~ 0),
         RELIABILITY_EXISTING_FRAGILITY_INSTITUTIONS = rowSums(is.na(globalrisk %>%
                                                                       select(Fr_INFORM_Fragility_Score_norm, 
                                                                              Fr_FSI_Score_norm)))/2,
         RELIABILITY_EMERGING_COVID_RESPONSE_CAPACITY = rowSums(is.na(globalrisk %>%
                                                                        select(H_Oxrollback_score_norm,
                                                                               H_Covidgrowth_casesnorm, 
                                                                               H_Covidgrowth_deathsnorm, 
                                                                               H_new_cases_smoothed_per_million_norm,
                                                                               H_new_deaths_smoothed_per_million_norm,
                                                                               H_Covidproj_Projected_Deaths_._1M_norm)))/6,
         RELIABILITY_EMERGING_FOOD_SECURITY = rowSums(is.na(globalrisk %>%
                                                              select(F_Fewsnet_Score_norm,
                                                                     F_Artemis_Score_norm, 
                                                                     F_FAO_6mFPV_norm)))/3,
         RELIABILITY_EMERGING_CONFLICT = rowSums(is.na(globalrisk %>%
                                                         select(C_ACLED_event_same_month_difference_perc_norm, 
                                                                C_ACLED_fatal_same_month_difference_perc_norm)))/2,
         RELIABILITY_EMERGING_FISCAL = case_when(is.na(D_IMF_debt2020.2019_norm) ~ 1,
                                                 TRUE ~ 0),
         RELIABILITY_EMERGING_MACROECONOMIC_EXPOSURE_TO_COVID = rowSums(is.na(globalrisk %>%
                                                                                select(M_GDP_IMF_2019minus2020_norm,
                                                                                       M_GDP_WB_2019minus2020_norm)))/2,
         RELIABILITY_EMERGING_NATURAL_HAZARDS = rowSums(is.na(globalrisk %>%
                                                                select(NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS_norm, 
                                                                       NH_GDAC_Hazard_Score_Norm, 
                                                                       NH_INFORM_Crisis_Norm)))/3,
         RELIABILITY_EMERGING_FRAGILITY_INSTITUTIONS = rowSums(is.na(globalrisk %>%
                                                                       select(Fr_FSI_2019minus2020_norm, 
                                                                              Fr_REIGN_couprisk3m_norm, 
                                                                              NH_INFORM_CRISIS_Type)))/3) 
#Create total reliability variabiles
reliabilitysheet <- reliabilitysheet %>%
  mutate(RELIABILITY_SCORE_EXISTING_RISK = rowMeans(select(., starts_with("RELIABILITY_EXISTING"))),
         RELIABILITY_SCORE_EMERGING_RISK = rowMeans(select(., starts_with("RELIABILITY_EMERGING")))) %>%
  select(Countryname, Country, RELIABILITY_SCORE_EXISTING_RISK, RELIABILITY_SCORE_EMERGING_RISK, RELIABILITY_EXISTING_COVID_RESPONSE_CAPACITY,RELIABILITY_EXISTING_FOOD_SECURITY,
         RELIABILITY_EXISTING_CONFLICT, RELIABILITY_EXISTING_MACROECONOMIC_EXPOSURE_TO_COVID,
         RELIABILITY_EXISTING_FISCAL, RELIABILITY_EXISTING_SOCIOECONOMIC_VULNERABILITY,
         RELIABILITY_EXISTING_NATURAL_HAZARDS,RELIABILITY_EXISTING_FRAGILITY_INSTITUTIONS,
         RELIABILITY_EMERGING_COVID_RESPONSE_CAPACITY,RELIABILITY_EMERGING_FOOD_SECURITY, RELIABILITY_EMERGING_CONFLICT,
         RELIABILITY_EMERGING_CONFLICT,RELIABILITY_EMERGING_FISCAL, RELIABILITY_EMERGING_MACROECONOMIC_EXPOSURE_TO_COVID,
         RELIABILITY_EMERGING_NATURAL_HAZARDS, RELIABILITY_EMERGING_FRAGILITY_INSTITUTIONS) %>%
  arrange(Country)
  
#Write as a csv file for the reliability sheet
write.csv(reliabilitysheet, "Risk_sheets/reliabilitysheet.csv")

#Combine the reliability sheet with the global database and write as csv
reliable <- reliabilitysheet %>%
  select(Countryname, Country, RELIABILITY_SCORE_EXISTING_RISK, RELIABILITY_SCORE_EMERGING_RISK)

globalrisk <- left_join(globalrisk, reliable, by = c("Countryname", "Country"))

#Save database of all risk indicators (+ reliability scores)
write.csv(globalrisk, "Risk_Sheets/Global_compound_risk_database.csv")

#Combine the reliability sheet with the summary risk flag sheet
reliable <- reliabilitysheet %>%
  select(Countryname, Country, RELIABILITY_SCORE_EXISTING_RISK, RELIABILITY_SCORE_EMERGING_RISK)

riskflags <- left_join(riskflags, reliable, by = c("Countryname", "Country"))

#Write csv file of all risk flags (+reliability scores)
write.csv(riskflags, "Risk_Sheets/Compound_Risk_Flag_Sheets.csv")

#----------------------------CRATE SUMMARY EXCEL FILE----------------------------------------------------
#Select relevant variables
riskset <- riskflags %>%
  select(Countryname, Country, EXISTING_RISK_COVID_RESPONSE_CAPACITY, 
         EXISTING_RISK_FOOD_SECURITY, EXISTING_RISK_CONFLICT, EXISTING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID, 
         EXISTING_RISK_FISCAL, EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY, 
         EXISTING_RISK_NATURAL_HAZARDS, EXISTING_RISK_FRAGILITY_INSTITUTIONS, 
         EMERGING_RISK_COVID_RESPONSE_CAPACITY, EMERGING_RISK_FOOD_SECURITY, 
         EMERGING_RISK_CONFLICT, EMERGING_RISK_FISCAL, EMERGING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID, 
         EMERGING_RISK_NATURAL_HAZARDS, EMERGING_RISK_FRAGILITY_INSTITUTIONS,TOTAL_EXISTING_COMPOUND_RISK_SCORE,   TOTAL_EMERGING_COMPOUND_RISK_SCORE,TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM, 
         TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM,RELIABILITY_SCORE_EXISTING_RISK, RELIABILITY_SCORE_EMERGING_RISK)

#Add blank columns to riskflags dataset
riskflagsblank <- riskset %>%
  add_column(" " = NA, .after = "Country") %>%
  add_column("  " = NA, .after = "EMERGING_RISK_FRAGILITY_INSTITUTIONS") %>%
  add_column("   " = NA, .after = "TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM") 
  
#Create Excel
crxls <- createWorkbook()
addWorksheet(crxls, "riskflags", tabColour = "lightgrey")
writeData(crxls, "riskflags", riskflagsblank, colNames = TRUE)
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
addWorksheet(crxls, "Reliability_sheet", tabColour = "grey")
writeData(crxls, "Reliability_sheet", reliabilitysheet, colNames = TRUE)

#Insert alternative flag sheet
addWorksheet(crxls, "Alternativeflag_sheet", tabColour = "purple")
#Select relevant variables
alt <- riskflags %>%
  select(Countryname, Country, contains("_AV"), contains("SQ")) %>%
  arrange(Country)
#Add blank columns
alt <- alt %>%
  add_column(" " = NA, .after = "Country") %>%
  add_column("  " = NA, .after = "EMERGING_RISK_FRAGILITY_INSTITUTIONS_AV") %>%
  add_column("   " = NA, .after = "EMERGING_RISK_FRAGILITY_INSTITUTIONS_SQ") 
#Writesheet
writeData(crxls, "Alternativeflag_sheet", alt, colNames = TRUE)

#Colour and stlye sheets
Map(function(number, tab){
headerStyle <- createStyle(
  fontSize = 10, 
  fontColour = "#FFFFFF",
  textDecoration = "bold", 
  halign = "center", 
  valign = "center",
  fgFill = "#001933",
  border = "TopBottom", 
  borderColour = "white", 
  wrapText = TRUE,
  textRotation = 90
)

addStyle(crxls, 
         sheet = number, 
         headerStyle, 
         rows = 1, 
         cols = 1:50, 
         gridExpand = TRUE
)

bodyStyle <- createStyle(fgFill = "white", 
                         border = "TopBottomLeftRight",
                         borderColour = "white",
                         halign = "center"
)

addStyle(crxls, 
         sheet = number, 
         bodyStyle, 
         rows = 2:191, 
         cols = 1:50, 
         gridExpand = TRUE
)

setColWidths(crxls, number, cols = 1, widths = 10) ## set column width for row names column
setRowHeights(crxls, number, rows = 1, heights =  150) ## set column width for row names column

modifyBaseFont(crxls, 
               fontSize = 12,
               fontColour = "black", 
               fontName = "Arial"
)
}, c(1:11))

#Set specific style for the risk tab sheet
headerStyle <- createStyle(
  fontSize = 10, 
  fontColour = "white",
  textDecoration = "bold", 
  halign = "center", 
  valign = "center",
  fgFill = "lightslategray",
  border = "TopBottom", 
  borderColour = "white", 
  wrapText = TRUE,
  textRotation = 90
)

addStyle(crxls, 
         sheet = 1, 
         headerStyle, 
         rows = 1, 
         cols = 4:11, 
         gridExpand = TRUE
)

headerStyle <- createStyle(
  fontSize = 10, 
  fontColour = "black",
  textDecoration = "bold", 
  halign = "center", 
  valign = "center",
  fgFill = "sandybrown",
  border = "TopBottom", 
  borderColour = "white", 
  wrapText = TRUE,
  textRotation = 90
)

addStyle(crxls, 
         sheet = 1, 
         headerStyle, 
         rows = 1, 
         cols = 12:18, 
         gridExpand = TRUE
)

headerStyle2 <- createStyle(
  fontSize = 10, 
  fontColour = "black",
  textDecoration = "bold", 
  halign = "center", 
  valign = "center",
  fgFill = "white",
  border = "TopBottom", 
  borderColour = "white", 
  wrapText = TRUE,
  textRotation = 90
)

addStyle(crxls, 
         sheet = 1, 
         headerStyle2, 
         rows = 1, 
         cols = c(3, 19, 24, 27:50),
         gridExpand = TRUE
)

setColWidths(crxls, 1, cols = 1, widths = 10) ## set column width for row names column
setRowHeights(crxls, 1, rows = 1, heights =  150) ## set column width for row names column

#Conditional formatting colours for main sheet
posStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
medStyle <- createStyle(fontColour = "#CC6600", bgFill = "#FFE5CC")
negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
naStyle <- createStyle(fontColour = "white", bgFill = "white")

#Conditional Cell Formatting for main sheet
conditionalFormatting(crxls, "riskflags", cols=4:18, rows=1:191, rule="==10", style = negStyle)
conditionalFormatting(crxls, "riskflags", cols=4:18, rows=1:191, type = "between", rule=c(7.00, 9.99), style = medStyle)
conditionalFormatting(crxls, "riskflags", cols=4:18, rows=1:191, type = "between", rule=c(0, 6.999), style = posStyle)
conditionalFormatting(crxls, "riskflags", cols=4:18, rows=1:191, rule = '=""', style = naStyle)
conditionalFormatting(crxls, "riskflags", cols=25:26, rows=1:191, type = "between", rule=c(2/3, 1), style = negStyle)
conditionalFormatting(crxls, "riskflags",cols=25:26, rows=1:191, type = "between", rule=c(1/3, 0.665), style = medStyle)
conditionalFormatting(crxls, "riskflags", cols=25:26, rows=1:191, type = "between", rule=c(0, 0.332), style = posStyle)
conditionalFormatting(crxls, "riskflags", cols=25:26, rows=1:191, rule = '=""', style = naStyle)

#Function for the remaining tabs
cond <- function(sheet, numhigh, numlow){

  posStyle <- createStyle(fontColour = "#006100", 
                          bgFill = "#C6EFCE")
  medStyle <- createStyle(fontColour = "#CC6600",
                          bgFill = "#FFE5CC")
  negStyle <- createStyle(fontColour = "#9C0006",
                          bgFill = "#FFC7CE")
  naStyle <- createStyle(fontColour = "white", 
                         bgFill = "white")
  
  conditionalFormatting(crxls, sheet, cols=numhigh:numlow, rows=1:191, rule="==10", style = negStyle)
  conditionalFormatting(crxls, sheet, cols=numhigh:numlow, rows=1:191, type = "between", rule=c(7.00, 9.99), style = medStyle)
  conditionalFormatting(crxls, sheet, cols=numhigh:numlow, rows=1:191, type = "between", rule=c(0, 6.9999), style = posStyle)
  conditionalFormatting(crxls, sheet, cols=numhigh:numlow, rows=1:191, rule = '=""', style = naStyle)
}

#Conditional formatting of specific cells
cond("conflictsheet", which(colnames(conflictsheet) == "C_GPI_Score_norm"), which(colnames(conflictsheet) == "C_GPI_Score_norm"))
cond("conflictsheet", which(colnames(conflictsheet) == "C_ACLED_fatal_same_month_difference_perc_norm"), which(colnames(conflictsheet) == "C_ACLED_event_month_threeyear_difference_perc_norm"))
cond("debtsheet", which(colnames(debtsheet) == "D_WB_Overall_debt_distress_norm"), which(colnames(debtsheet) == "D_WB_Overall_debt_distress_norm"))
cond("debtsheet", which(colnames(debtsheet) == "D_IMF_debt2020.2019_norm"), which(colnames(debtsheet) == "D_IMF_debt2020.2019_norm"))
cond("foodsecurity", which(colnames(foodsecurity) == "F_Proteus_Score_norm"), which(colnames(foodsecurity) == "F_Proteus_Score_norm"))
cond("foodsecurity", which(colnames(foodsecurity) == "F_Fewsnet_Score_norm"), which(colnames(foodsecurity) == "F_Fewsnet_Score_norm"))
cond("foodsecurity", which(colnames(foodsecurity) == "F_FAO_6mFPV_norm"), which(colnames(foodsecurity) == "F_FAO_6mFPV_norm"))
cond("foodsecurity", which(colnames(foodsecurity) == "F_Artemis_Score_norm"), which(colnames(foodsecurity) == "F_Artemis_Score_norm"))
cond("fragilitysheet", which(colnames(fragilitysheet) == "Fr_FSI_2019minus2020_norm"), which(colnames(fragilitysheet) == "Fr_FSI_Score_norm"))
cond("fragilitysheet", which(colnames(fragilitysheet) == "Fr_INFORM_Fragility_Score_norm"), which(colnames(fragilitysheet) == "Fr_INFORM_Fragility_Score_norm"))
cond("fragilitysheet", which(colnames(fragilitysheet) == "Fr_REIGN_couprisk3m_norm"), which(colnames(fragilitysheet) == "Fr_REIGN_couprisk3m_norm"))
cond("healthsheet", which(colnames(healthsheet) == "H_HIS_Score_norm"), which(colnames(healthsheet) == "H_HIS_Score_norm"))
cond("healthsheet", which(colnames(healthsheet) == "H_Oxrollback_score_norm"), which(colnames(healthsheet) == "H_Oxrollback_score_norm"))
cond("healthsheet", which(colnames(healthsheet) == "H_Covidgrowth_deathsnorm"), which(colnames(healthsheet) == "H_Covidgrowth_casesnorm"))
cond("healthsheet", which(colnames(healthsheet) == "H_new_cases_smoothed_per_million_norm"), which(colnames(healthsheet) == "H_new_cases_smoothed_per_million_norm"))
cond("healthsheet", which(colnames(healthsheet) == "H_new_deaths_smoothed_per_million_norm"), which(colnames(healthsheet) == "H_new_deaths_smoothed_per_million_norm"))
cond("healthsheet", which(colnames(healthsheet) == "H_Covidproj_Projected_Deaths_._1M_norm"), which(colnames(healthsheet) == "H_Covidproj_Projected_Deaths_._1M_norm"))
cond("macrosheet", which(colnames(macrosheet) == "M_GDP_WB_2019minus2020_norm"), which(colnames(macrosheet) == "M_GDP_IMF_2019minus2020_norm"))
cond("macrosheet", which(colnames(macrosheet) == "M_Economic_and_Financial_score_norm"), which(colnames(macrosheet) == "M_Economic_and_Financial_score_norm"))
cond("Naturalhazardsheet", which(colnames(Naturalhazardsheet) == "NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS_norm"), which(colnames(Naturalhazardsheet) == "NH_UKMO_TOTAL.RISK.NEXT.12.MONTHS_norm"))
cond("Naturalhazardsheet", which(colnames(Naturalhazardsheet) == "NH_GDAC_Hazard_Score_Norm"), which(colnames(Naturalhazardsheet) == "NH_GDAC_Hazard_Score_Norm"))
cond("Naturalhazardsheet", which(colnames(Naturalhazardsheet) == "NH_INFORM_Crisis_Norm"), which(colnames(Naturalhazardsheet) == "NH_INFORM_Crisis_Norm"))
cond("Socioeconomic_sheet", which(colnames(Socioeconomic_sheet) == "S_OCHA_Covid.vulnerability.index_norm"), which(colnames(Socioeconomic_sheet) == "S_OCHA_Covid.vulnerability.index_norm"))

#Conditional formatting colours
posStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
medStyle <- createStyle(fontColour = "#CC6600", bgFill = "#FFE5CC")
negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
naStyle <- createStyle(fontColour = "white", bgFill = "white")

#Conditional Cell Formatting
conditionalFormatting(crxls, "Reliability_sheet", cols=5:19, rows=1:191, rule="==1", style = negStyle)
conditionalFormatting(crxls, "Reliability_sheet", cols=5:19, rows=1:191, type = "between", rule=c(0.700, 0.999), style = medStyle)
conditionalFormatting(crxls, "Reliability_sheet", cols=5:19, rows=1:191, type = "between", rule=c(0, 0.6999), style = posStyle)
conditionalFormatting(crxls, "Reliability_sheet", cols=5:19, rows=1:191, rule = '=""', style = naStyle)
conditionalFormatting(crxls, "Alternativeflag_sheet", cols=c(4:7, 9:15), rows=1:191,type = "between", rule=c(7, 10), style = negStyle)
conditionalFormatting(crxls, "Alternativeflag_sheet", cols=c(4:7, 9:15), rows=1:191, type = "between", rule=c(5, 6.9999), style = medStyle)
conditionalFormatting(crxls, "Alternativeflag_sheet", cols=c(4:7, 9:15), rows=1:191, type = "between", rule=c(0, 4.9999), style = posStyle)
conditionalFormatting(crxls, "Alternativeflag_sheet", cols=c(4:7, 9:15), rows=1:191, rule = '=""', style = naStyle)

#DatabarsconditionalFormatting
conditionalFormatting(crxls, "riskflags", cols = 20:23, rows = 1:191, type = "databar", style=c("#C6EFCE", "#CD5C5C")) 
conditionalFormatting(crxls, "Reliability_sheet", cols = 2:4, rows = 1:191, type = "databar", style=c("#C6EFCE", "#CD5C5C")) 
conditionalFormatting(crxls, "Alternativeflag_sheet", cols=17, rows=1:191, type = "databar", style=c("#C6EFCE", "#CD5C5C")) 

#Insert Global Maps
#install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(ggplot2, cowplot, lubridate, rvest,dplyr, viridis, tidyverse, countrycode)

#Loading world database
world <- map_data("world")
world <- world %>%
  dplyr::rename(Country = region) %>%
  dplyr::mutate(Country = countrycode(Country, origin = 'country.name', destination = 'iso3c'))

#Join datasets with risk flags
worldmap <- inner_join(world, riskflags, by="Country")

#Map theme
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
  plot.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F") , 
  legend.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
  text = element_text(colour = "lightgrey")
)

#Draw map one
map <- ggplot(data = worldmap, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM)) +
  scale_fill_distiller(palette ="Blues", direction = 1) + # or direction=1
  ggtitle("Total Existing Compound Risk Score") +
  plain +
  labs(fill = "Total # of risks")

#Draw map two
map2 <- ggplot(data = worldmap, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("Total Emerging Compound Risk Score") +
  plain +
  labs(fill = "Total # of risks")

#Join the maps and print to the system
jointmap <- cowplot::plot_grid(map, map2, ncol = 1, align = c("hv"))
print(jointmap)

#Insert plot into the worksheet
insertPlot(crxls, 1, xy = c("AA", 5), width = 11.5, height =9.5, fileType = "png", units = "in")

#Save the final worksheet
saveWorkbook(crxls, file = "Risk_sheets/Compound_Risk_Monitor.xlsx", overwrite = TRUE)



  
