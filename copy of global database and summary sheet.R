######################################################################################################
#
#  CODE USED TO CREATE THE GLOBAL DATABASET ON COMPOUND DISASTER RISK
#  (to be run after risk component sheets have been generated)
#
######################################################################################################

# LOAD PACKAGES ---------
# install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(
  ggplot2, cowplot, lubridate, rvest, dplyr, compositions, viridis,
  tidyverse, countrycode, clipr, sjmisc, awalker89 / openxlsx, EnvStats, 
  gsheet
)

#
##
### ********************************************************************************************
####    CREATE GLOBAL DATABASE WITH ALL RISK SHEETS ---------
### ********************************************************************************************
##
#

# Load risk sheets
healthsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/healthsheet.csv")
foodsecurity <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/foodsecuritysheet.csv")
debtsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/debtsheet.csv")
fragilitysheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/fragilitysheet.csv")
macrosheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/macrosheet.csv")
Naturalhazardsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/Naturalhazards.csv")
Socioeconomic_sheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/Socioeconomic_sheet.csv")
acapssheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/acapssheet.csv")
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")

# Join datasets
globalrisk <- left_join(countrylist, healthsheet, by = c("Countryname", "Country")) %>%
  left_join(., foodsecurity, by = c("Countryname", "Country")) %>%
  left_join(., debtsheet, by = c("Countryname", "Country")) %>%
  left_join(., fragilitysheet, by = c("Countryname", "Country")) %>%
  left_join(., macrosheet, by = c("Countryname", "Country")) %>%
  left_join(., Naturalhazardsheet, by = c("Countryname", "Country")) %>%
  left_join(., Socioeconomic_sheet, by = c("Country")) %>%
  left_join(., acapssheet, by = c("Country", "Countryname")) %>%
  select(-X.x, -X.y, -X.x.x, -X.y.y, -X.x.x.x, -X.y.y.y, -X.x.x.x.x, -X.y.y.y.y, -X) %>%
  distinct(Country, .keep_all = TRUE) %>%
  drop_na(Country)

# Write as csv
write.csv(globalrisk, "Risk_sheets/Global_compound_risk_database.csv")

#
##
### ********************************************************************************************
####    CREATE A FLAG SUMMARY SHEET ---------
### ********************************************************************************************
##
#

# Identify indicators as Underlying Vulnerability or Upcoming Threat, and give pretty name ---------
# Currently incomplete: need to add in all indicators. Also need to figure out how to take the pmax of all indicators in a dimension and timeframe
indicators <- matrix(data = c("Health", "underlying", "H_HIS_Score_norm", "HIS Score",
                              "Health", "underlying", "H_INFORM_rating.Value_norm", "INFORM Rating"
),
ncol = 4,
byrow = T)
colnames(indicators) <- c("dimension", "timeframe", "indicator", "prettyName")
indicators <- as.data.frame(indicators)
# 
# head(pmax(globalrisk$H_HIS_Score_norm, globalrisk$H_INFORM_rating.Value_norm))
# head(pmax(globalrisk[,test]))
# 
# sapply(globalrisk[,test], length)
# 
# mutate(globalrisk, pmax(
#   
# ))
# test <- indicators[which(indicators$dimension == "Health" & indicators$timeframe == "underlying"),"indicator"]

# Add existing and emerging risk scores
riskflags <- globalrisk %>%
  mutate(
    EXISTING_RISK_HEALTH = pmax(
      H_HIS_Score_norm,
      H_INFORM_rating.Value_norm
    ),
    EXISTING_RISK_FOOD_SECURITY = F_Proteus_Score_norm,
    EXISTING_RISK_MACRO_FISCAL = pmax(
      M_Economic_and_Financial_score_norm,
      D_WB_external_debt_distress_norm,
      na.rm = T
    ),
    EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY = S_INFORM_vul_norm,
    EXISTING_RISK_NATURAL_HAZARDS = pmax(
      NH_Hazard_Score_norm,
      # NH_multihazard_risk_norm,
      na.rm=T
    ),
    EXISTING_RISK_FRAGILITY_INSTITUTIONS = Fr_FCS_FSI_Normalised,
    EMERGING_RISK_HEALTH = pmax(
      H_Oxrollback_score_norm,
      H_Covidgrowth_casesnorm,
      H_Covidgrowth_deathsnorm,
      H_new_cases_smoothed_per_million_norm,
      H_new_deaths_smoothed_per_million_norm,
      H_add_death_prec_current_norm,
      H_health_acaps,
      H_GovernmentResponseIndexForDisplay_norm,
      H_wmo_don_alert,
      na.rm = T
    ),
    EMERGING_RISK_FOOD_SECURITY = case_when(
      (!is.na(F_fews_crm_norm) | !is.na(F_fpv_rating)) ~ as.numeric(pmax(
        F_fews_crm_norm,
        F_Artemis_Score_norm,
        F_fpv_rating,
        na.rm = T
      )),
      (is.na(F_fews_crm_norm) | is.na(F_fpv_rating)) ~  as.numeric(F_fpv_rating),
      TRUE ~ NA_real_
    ),
    EMERGING_RISK_MACRO_FISCAL = pmax(
      M_GDP_IMF_2019minus2020_norm,
      M_GDP_WB_2019minus2020_norm,
      M_macrofin_risk_norm,
      D_IMF_debt2020.2019_norm,
      na.rm = T
    ),
    EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY = pmax(
      S_pov_prop_19_20_norm,
      S_pov_abs_19_20_norm,
      S_change_unemp_norm,
      S_income_support.Rating_crm_norm,
      S_Household.risks,
      S_phone_average_index_norm,
      na.rm = T
    ),
    EMERGING_RISK_NATURAL_HAZARDS = pmax(
      NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS_norm,
      NH_GDAC_Hazard_Score_Norm,
      NH_natural_acaps,
      NH_la_nina_risk,
      na.rm = T
    ),
    EMERGING_RISK_FRAGILITY_INSTITUTIONS =pmax(
      Fr_REIGN_Normalised,
      Fr_Displaced_UNHCR_Normalised,
      Fr_BRD_Normalised,
      na.rm = T
    )
  ) %>%
  select(
    Countryname, Country, EXISTING_RISK_HEALTH, EXISTING_RISK_FOOD_SECURITY,
    EXISTING_RISK_MACRO_FISCAL, EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY,
    EXISTING_RISK_NATURAL_HAZARDS, EXISTING_RISK_FRAGILITY_INSTITUTIONS,
    EMERGING_RISK_HEALTH, EMERGING_RISK_FOOD_SECURITY, EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY, EMERGING_RISK_MACRO_FISCAL,
    EMERGING_RISK_NATURAL_HAZARDS, EMERGING_RISK_FRAGILITY_INSTITUTIONS, F_fews_crm_norm
  )

# Create tertiary risk flags
vars <- c(
  "EXISTING_RISK_HEALTH", "EXISTING_RISK_FOOD_SECURITY",
  "EXISTING_RISK_MACRO_FISCAL", "EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY",
  "EXISTING_RISK_NATURAL_HAZARDS", "EXISTING_RISK_FRAGILITY_INSTITUTIONS",
  "EMERGING_RISK_HEALTH", "EMERGING_RISK_FOOD_SECURITY",
  "EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY",
  "EMERGING_RISK_MACRO_FISCAL",
  "EMERGING_RISK_NATURAL_HAZARDS", "EMERGING_RISK_FRAGILITY_INSTITUTIONS"
)

riskflags[paste0(vars, "_RISKLEVEL")] <- lapply(riskflags[vars], function(tt) {
  ifelse(tt >= 0 & tt < 7, "Low risk",
         ifelse(tt >= 7 & tt < 10, "Medium risk",
                ifelse(tt == 10, "High risk", NA
                )
         )
  )
})

# Calculate total compound risk scores
riskflags$TOTAL_EXISTING_COMPOUND_RISK_SCORE <- as.numeric(unlist(row_count(
  riskflags,
  EXISTING_RISK_HEALTH:EXISTING_RISK_FRAGILITY_INSTITUTIONS,
  count = 10,
  append = F
)))

riskflags$TOTAL_EMERGING_COMPOUND_RISK_SCORE <- as.numeric(unlist(row_count(
  riskflags, 
  EMERGING_RISK_HEALTH:EMERGING_RISK_FRAGILITY_INSTITUTIONS,
  count = 10,
  append = F
)))

riskflags$medium_risk_existing <- as.numeric(unlist(row_count(riskflags,
                                                              EXISTING_RISK_HEALTH_RISKLEVEL:EXISTING_RISK_FRAGILITY_INSTITUTIONS_RISKLEVEL,
                                                              count = "Medium risk",
                                                              append = F
)))

riskflags$medium_risk_emerging <- as.numeric(unlist(row_count(riskflags,
                                                              EMERGING_RISK_HEALTH_RISKLEVEL:EMERGING_RISK_FRAGILITY_INSTITUTIONS_RISKLEVEL,
                                                              count = "Medium risk",
                                                              append = F
)))

riskflags$TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM <- as.numeric(unlist(riskflags$TOTAL_EXISTING_COMPOUND_RISK_SCORE + (riskflags$medium_risk_existing / 2)))
riskflags$TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM <- as.numeric(unlist(riskflags$TOTAL_EMERGING_COMPOUND_RISK_SCORE + (riskflags$medium_risk_emerging / 2)))

# Drop teritiary rates (may want to reinstate in the future)
riskflags <- riskflags %>%
  select(
    -medium_risk_emerging, -medium_risk_existing, -all_of(paste0(vars, "_RISKLEVEL"))
  ) %>%
  distinct(Country, .keep_all = TRUE) %>%
  drop_na(Country)

#
##
### ********************************************************************************************
####    CREATE DATABASE OF ALTERNATIVE RISK SCORES ---------
### ********************************************************************************************
##
#

# Alternative combined risk scores
names <- c(
  "EMERGING_RISK_FRAGILITY_INSTITUTIONS", 
  "EMERGING_RISK_MACRO_FISCAL", "EXISTING_RISK_FRAGILITY_INSTITUTIONS"
)

riskflags[paste0(names, "_plus1")] <- lapply(riskflags[names], function(xx) {
  ifelse(xx == 0, xx + 1, xx)
})

# Geometric means
riskflags <- riskflags %>%
  rowwise() %>%
  mutate(
    EMERGING_RISK_FRAGILITY_INSTITUTIONS_MULTIDIMENSIONAL = geometricmean(c(
      EMERGING_RISK_FRAGILITY_INSTITUTIONS_plus1,
      EMERGING_RISK_MACRO_FISCAL_plus1),
      na.rm = T
    ),
    EMERGING_RISK_FRAGILITY_INSTITUTIONS_MULTIDIMENSIONAL_SQ = geometricmean(c(
      EXISTING_RISK_FRAGILITY_INSTITUTIONS_plus1,
      EMERGING_RISK_FRAGILITY_INSTITUTIONS_MULTIDIMENSIONAL),
      na.rm = T
    )
  )

# remove unnecessary variables
riskflags <- riskflags %>%
  select(-contains("_plus1"))

# Alternativ combined total scores
altflag <- globalrisk
names <- c(
  "S_INFORM_vul_norm", "H_Oxrollback_score_norm", "H_wmo_don_alert",
  "H_Covidgrowth_casesnorm", "H_Covidgrowth_deathsnorm", "H_HIS_Score_norm", "H_INFORM_rating.Value_norm",
  "H_new_cases_smoothed_per_million_norm", "H_new_deaths_smoothed_per_million_norm",
  "F_Proteus_Score_norm", "F_fews_crm_norm", "F_Artemis_Score_norm","F_fpv_rating", "D_WB_external_debt_distress_norm",
  "D_IMF_debt2020.2019_norm", "M_Economic_and_Financial_score_norm", "M_GDP_IMF_2019minus2020_norm", "M_GDP_WB_2019minus2020_norm","M_macrofin_risk_norm",
  "NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS_norm", "NH_GDAC_Hazard_Score_Norm",  "H_add_death_prec_current_norm",  "H_add_death_prec_current_norm", 
  "S_Household.risks", "S_phone_average_index_norm",  "NH_la_nina_risk", "NH_natural_acaps","Fr_FCS_FSI_Normalised", 
  "Fr_REIGN_Normalised", "Fr_Displaced_UNHCR_Normalised", "Fr_BRD_Normalised"
)

altflag[paste0(names, "_plus1")] <- lapply(altflag[names], function(xx) {
  ifelse(xx == 0, xx + 1, xx)
})

# Calculate alternative variables
altflag <- altflag %>%
  rowwise() %>%
  mutate(
    EMERGING_RISK_HEALTH_AV = geometricmean(c(
      H_Oxrollback_score_norm_plus1,
      H_Covidgrowth_casesnorm_plus1,
      H_Covidgrowth_deathsnorm_plus1,
      H_new_cases_smoothed_per_million_norm_plus1,
      H_new_deaths_smoothed_per_million_norm_plus1,
      H_add_death_prec_current_norm_plus1),
      H_wmo_don_alert_plus1,
      na.rm = T
    ),
    EMERGING_RISK_MACRO_FISCAL_AV = geometricmean(c(
      M_GDP_IMF_2019minus2020_norm,
      M_GDP_WB_2019minus2020_norm,
      M_macrofin_risk_norm,
      D_IMF_debt2020.2019_norm,
      na.rm = T
    )),
    EMERGING_RISK_FRAGILITY_INSTITUTIONS_AV = geometricmean(c(
      Fr_REIGN_Normalised,
      Fr_Displaced_UNHCR_Normalised,
      Fr_BRD_Normalised,
      na.rm = T
    ))
    ,
    EMERGING_RISK_HEALTH_SQ_ALT = geometricmean(c(
      H_Oxrollback_score_norm_plus1,
      max(altflag$H_Covidgrowth_casesnorm,
          altflag$H_Covidgrowth_deathsnorm,
          altflag$H_new_cases_smoothed_per_million_norm,
          altflag$H_new_deaths_smoothed_per_million_norm,
          altflag$H_add_death_prec_current,
          altflag$H_health_acaps,
          altflag$H_wmo_don_alert,
          na.rm = T
      )
    ),
    na.rm = T
    )
  )

#--------------------------------Calculate Coefficient of Variation----------------------------------------------------------------
altflag <- altflag %>%
  rowwise() %>%
  mutate(
    H_coefvar = cv(c(
      H_Oxrollback_score_norm,
      H_Covidgrowth_casesnorm,
      H_Covidgrowth_deathsnorm,
      H_new_cases_smoothed_per_million_norm,
      H_new_deaths_smoothed_per_million_norm,
      H_add_death_prec_current,
      H_wmo_don_alert),
      na.rm = T
    ),
    M_coefvar = cv(c(
      M_GDP_IMF_2019minus2020_norm,
      M_GDP_WB_2019minus2020_norm,
      M_macrofin_risk_norm,
      D_IMF_debt2020.2019_norm),
      na.rm = T
    ),
    Fr_coefvar = cv(c(
      Fr_REIGN_Normalised,
      Fr_Displaced_UNHCR_Normalised,
      Fr_BRD_Normalised),
      na.rm = T
    ),
    NH_coefvar = cv(c(
      NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS_norm,
      NH_GDAC_Hazard_Score_Norm,
      NH_natural_acaps,
      NH_la_nina_risk),
      na.rm = T
    ),
    F_coefvar = cv(c(
      F_fpv_rating,
      F_Artemis_Score_norm,
      F_fpv_rating),
      na.rm = T
    )
  )

# Merge datasets to include alt variables
riskflags <- inner_join(riskflags,
                        altflag,
                        by = c("Country", "Countryname"),
                        keep = F
)

#-----------------------------Calculate emerging risk score using existing risk ----------------------------------------------
riskflags <- riskflags %>%
  mutate(
    EMERGING_RISK_HEALTH_SQ = case_when(
      !is.na(EXISTING_RISK_HEALTH) ~ sqrt(EXISTING_RISK_HEALTH * EMERGING_RISK_HEALTH),
      TRUE ~ EMERGING_RISK_HEALTH
    ),
    EMERGING_RISK_HEALTH_SQ_SQ = case_when(
      !is.na(EXISTING_RISK_HEALTH) ~ sqrt(EXISTING_RISK_HEALTH * EMERGING_RISK_HEALTH_SQ_ALT),
      TRUE ~ EMERGING_RISK_HEALTH
    ),
    EMERGING_RISK_FOOD_SECURITY_SQ = case_when(
      !is.na(EXISTING_RISK_FOOD_SECURITY) ~ sqrt(EXISTING_RISK_FOOD_SECURITY * EMERGING_RISK_FOOD_SECURITY),
      TRUE ~ EMERGING_RISK_FOOD_SECURITY
    ),
    EMERGING_RISK_MACRO_FISCAL_SQ = case_when(
      !is.na(EXISTING_RISK_MACRO_FISCAL) ~ sqrt(EXISTING_RISK_MACRO_FISCAL * EMERGING_RISK_MACRO_FISCAL),
      TRUE ~ EMERGING_RISK_MACRO_FISCAL
    ),
    EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY_SQ = case_when(
      !is.na(EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY) ~ sqrt(EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY * EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY),
      TRUE ~ as.numeric(EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY)
    ),
    EMERGING_RISK_NATURAL_HAZARDS_SQ = EMERGING_RISK_NATURAL_HAZARDS,
    EMERGING_RISK_FRAGILITY_INSTITUTIONS_SQ = case_when(
      !is.na(EXISTING_RISK_FRAGILITY_INSTITUTIONS) ~ sqrt(EXISTING_RISK_FRAGILITY_INSTITUTIONS * EMERGING_RISK_FRAGILITY_INSTITUTIONS),
      TRUE ~ EMERGING_RISK_FRAGILITY_INSTITUTIONS
    )
  )

# Calculate total emerging risk scores for SQ
sqnam <- c(
  "EMERGING_RISK_HEALTH_SQ", "EMERGING_RISK_FOOD_SECURITY_SQ",
  "EMERGING_RISK_MACRO_FISCAL_SQ","EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY_SQ",
  "EMERGING_RISK_NATURAL_HAZARDS_SQ",
  "EMERGING_RISK_FRAGILITY_INSTITUTIONS_SQ"
)

riskflags$TOTAL_EMERGING_COMPOUND_RISK_SCORE_SQ <- rowSums(riskflags[sqnam] >= 7, na.rm = T)

#
##
### ********************************************************************************************
####    CREATE DATABASE OF RELIABILITY SCORES
### ********************************************************************************************
##
#

# Calculate the number of missing values in each of the source indicators for the various risk components (as a proportion)
reliabilitysheet <- globalrisk %>%
  mutate(
    RELIABILITY_EXISTING_HEALTH = rowSums(is.na(globalrisk %>%
                                                  select(H_HIS_Score_norm, H_INFORM_rating.Value_norm)),
                                          na.rm = T
    ) / 2,
    RELIABILITY_EXISTING_FOOD_SECURITY = case_when(
      is.na(F_Proteus_Score_norm) ~ 1,
      TRUE ~ 0
    ),
    RELIABILITY_EXISTING_MACRO_FISCAL = rowSums(is.na(globalrisk %>%
                                                        select(
                                                          M_Economic_and_Financial_score_norm,
                                                          D_WB_external_debt_distress_norm,
                                                        )),
                                                na.rm = T
    ) / 4,
    RELIABILITY_EXISTING_SOCIOECONOMIC_VULNERABILITY = case_when(
      is.na(S_INFORM_vul_norm) ~ 1,
      TRUE ~ 0
    ),
    RELIABILITY_EXISTING_NATURAL_HAZARDS = case_when(
      is.na(NH_Hazard_Score_norm) ~ 1,
      TRUE ~ 0
    ),
    RELIABILITY_EXISTING_FRAGILITY_INSTITUTIONS = case_when(
      is.na(Fr_FCS_FSI_Normalised) ~ 1,
      TRUE ~ 0
    ),
    RELIABILITY_EMERGING_HEALTH = rowSums(is.na(globalrisk %>%
                                                  select(
                                                    H_Oxrollback_score_norm,
                                                    H_Covidgrowth_casesnorm,
                                                    H_Covidgrowth_deathsnorm,
                                                    H_new_cases_smoothed_per_million_norm,
                                                    H_new_deaths_smoothed_per_million_norm,
                                                    H_add_death_prec_current_norm
                                                  )),
                                          na.rm = T
    ) / 6,
    RELIABILITY_EMERGING_FOOD_SECURITY = rowSums(is.na(globalrisk %>%
                                                         select(
                                                           F_fews_crm_norm,
                                                           F_Artemis_Score_norm,
                                                           F_fpv_rating,
                                                         )),
                                                 na.rm = T
    ) / 3,
    EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY = rowSums(is.na(globalrisk %>%
                                                                select(
                                                                  S_pov_prop_19_20_norm,
                                                                  S_pov_abs_19_20_norm,
                                                                  S_change_unemp_norm,
                                                                  S_income_support.Rating_crm_norm,
                                                                  S_Household.risks,
                                                                  S_phone_average_index_norm
                                                                )),
                                                        na.rm = T
    ) / 3,
    RELIABILITY_EMERGING_MACRO_FISCAL = rowSums(is.na(globalrisk %>%
                                                        select(
                                                          M_GDP_IMF_2019minus2020_norm,
                                                          M_GDP_WB_2019minus2020_norm,
                                                          M_macrofin_risk_norm,
                                                          D_IMF_debt2020.2019_norm,
                                                        )),
                                                na.rm = T
    ) / 4,
    RELIABILITY_EMERGING_NATURAL_HAZARDS = rowSums(is.na(globalrisk %>%
                                                           select(
                                                             NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS_norm,
                                                             NH_GDAC_Hazard_Score_Norm,
                                                             NH_Hazard_Score_norm
                                                           )),
                                                   na.rm = T
    ) / 3,
    RELIABILITY_EMERGING_FRAGILITY_INSTITUTIONS = rowSums(is.na(globalrisk %>%
                                                                  select(
                                                                    Fr_REIGN_Normalised,
                                                                    Fr_Displaced_UNHCR_Normalised,
                                                                    Fr_BRD_Normalised,
                                                                  )),
                                                          na.rm = T
    ) / 3
  )

# Create total reliability variabiles
reliabilitysheet <- reliabilitysheet %>%
  mutate(
    RELIABILITY_SCORE_EXISTING_RISK = round(rowMeans(select(., starts_with("RELIABILITY_EXISTING"))), 1),
    RELIABILITY_SCORE_EMERGING_RISK = round(rowMeans(select(., starts_with("RELIABILITY_EMERGING"))), 1)
  ) %>%
  select(
    Countryname, Country, RELIABILITY_SCORE_EXISTING_RISK, RELIABILITY_SCORE_EMERGING_RISK, RELIABILITY_EXISTING_HEALTH, RELIABILITY_EXISTING_FOOD_SECURITY,
    RELIABILITY_EXISTING_MACRO_FISCAL, RELIABILITY_EXISTING_SOCIOECONOMIC_VULNERABILITY,
    RELIABILITY_EXISTING_NATURAL_HAZARDS, RELIABILITY_EXISTING_FRAGILITY_INSTITUTIONS,
    RELIABILITY_EMERGING_HEALTH, RELIABILITY_EMERGING_FOOD_SECURITY,
    EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY, RELIABILITY_EMERGING_MACRO_FISCAL,
    RELIABILITY_EMERGING_NATURAL_HAZARDS, RELIABILITY_EMERGING_FRAGILITY_INSTITUTIONS
  ) %>%
  arrange(Country)

# Write as a csv file for the reliability sheet
write.csv(reliabilitysheet, "Risk_sheets/reliabilitysheet.csv")

#------------------------------Combine the reliability sheet with the global database------------------------------------
reliable <- reliabilitysheet %>%
  select(Countryname, Country, RELIABILITY_SCORE_EXISTING_RISK, RELIABILITY_SCORE_EMERGING_RISK)

globalrisk <- left_join(globalrisk, reliable, by = c("Countryname", "Country"))

# Save database of all risk indicators (+ reliability scores)
write.csv(globalrisk, "Risk_Sheets/Global_compound_risk_database.csv")

#------------------------------Combine the reliability sheet with the summary risk flag sheet-----------------------------
reliable <- reliabilitysheet %>%
  select(Countryname, Country, RELIABILITY_SCORE_EXISTING_RISK, RELIABILITY_SCORE_EMERGING_RISK)

riskflags <- left_join(riskflags %>%
                         select(
                           "Countryname", "Country",
                           contains(c("_AV", "_SQ", "_ALT", "EXISTING_", "EMERGING_", "coefvar"))
                         ),
                       reliable, 
                       by = c("Countryname", "Country")
)

# Write csv file of all risk flags (+reliability scores)
write.csv(riskflags, "Risk_Sheets/Compound_Risk_Flag_Sheets.csv")

#
##
### ********************************************************************************************
####    CREATE SUMMARY EXCEL FILE ----------
### ********************************************************************************************
##
#

# Select relevant variables
riskset <- riskflags %>%
  select(
    Countryname, Country, EXISTING_RISK_HEALTH,
    EXISTING_RISK_FOOD_SECURITY, EXISTING_RISK_MACRO_FISCAL, EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY,
    EXISTING_RISK_NATURAL_HAZARDS, EXISTING_RISK_FRAGILITY_INSTITUTIONS,
    EMERGING_RISK_HEALTH, EMERGING_RISK_FOOD_SECURITY,
    EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY,
    EMERGING_RISK_MACRO_FISCAL,
    EMERGING_RISK_NATURAL_HAZARDS, EMERGING_RISK_FRAGILITY_INSTITUTIONS,
    TOTAL_EXISTING_COMPOUND_RISK_SCORE, TOTAL_EMERGING_COMPOUND_RISK_SCORE, TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM,
    TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM, RELIABILITY_SCORE_EXISTING_RISK, RELIABILITY_SCORE_EMERGING_RISK
  )

# Add blank columns to riskflags dataset
riskflagsblank <- riskset %>%
  arrange(Country) %>%
  add_column(" " = NA, .after = "Country") %>%
  add_column("  " = NA, .after = "EMERGING_RISK_FRAGILITY_INSTITUTIONS") %>%
  add_column("   " = NA, .after = "TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM")

# Create Excel
crxls <- createWorkbook()
addWorksheet(crxls, "riskflags", tabColour = "lightgrey")
writeData(crxls, "riskflags", riskflagsblank, colNames = TRUE)
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

# Insert alternative flag sheet
addWorksheet(crxls, "Alternativeflag_sheet", tabColour = "#9999CC")
# Select relevant variables
alt <- riskflags %>%
  select(Countryname, Country, contains("_AV"), contains("_MULTIDIMENSIONAL"), contains("SQ"), contains("coefvar"), -contains("OCHA")) %>%
  arrange(Country)
# Add blank columns
alt <- alt %>%
  add_column(" " = NA, .after = "Country") %>%
  add_column("  " = NA, .after = "EMERGING_RISK_FRAGILITY_INSTITUTIONS_AV") %>%
  add_column("   " = NA, .after = "EMERGING_RISK_FRAGILITY_INSTITUTIONS_MULTIDIMENSIONAL") %>%
  add_column("    " = NA, .after = "EMERGING_RISK_FRAGILITY_INSTITUTIONS_SQ") %>%
  add_column("     " = NA, .after = "TOTAL_EMERGING_COMPOUND_RISK_SCORE_SQ")
# Writesheet
writeData(crxls, "Alternativeflag_sheet", alt, colNames = TRUE)

#-----------------------------------------Conditional formatting-------------------------------------------------

# Colour and stlye sheets
Map(function(number, tab) {
  headerStyle <- createStyle(
    fontSize = 10,
    # fontColour = "#FFFFFF",
    # textDecoration = "bold",
    halign = "center",
    # valign = "center",
    # fgFill = "#000000",
    border = c("bottom", "left", "right"),
    borderColour = c("black", "white", "white"),
    borderStyle = c("thin", "thick", "thick"),
    wrapText = TRUE,
    # textRotation = 90
  )
  
  addStyle(crxls,
           sheet = number,
           headerStyle,
           rows = 1,
           cols = 1:57, # why 57?
           gridExpand = TRUE # what is this?
  )
  
  bodyStyle <- createStyle(
    fgFill = "white",
    border = "TopBottomLeftRight",
    borderColour = "white",
    halign = "center"
  )
  
  addStyle(crxls,
           sheet = number,
           bodyStyle,
           rows = 2:191,
           cols = 1:57,
           gridExpand = TRUE
  )
  
  setColWidths(crxls, number, cols = 1, widths = 10) ## set column width for row names column
  setRowHeights(crxls, number, rows = 1, heights = 150) ## set column width for row names column
  
  modifyBaseFont(crxls,
                 fontSize = 12,
                 fontColour = "black",
                 fontName = "Arial"
  )
}, c(1:2))

# Set specific style for the risk tab sheet
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

addStyle(
  crxls,
  sheet = 1,
  headerStyle,
  rows = 1,
  cols = 4:9,
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
         cols = 10:15,
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
         cols = c(3, 16, 21, 24:51),
         gridExpand = TRUE
)

setColWidths(crxls, 1, cols = 1, widths = 10) ## set column width for row names column
setRowHeights(crxls, 1, rows = 1, heights = 150) ## set column width for row names column

# Conditional formatting colours for main sheet
posStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
medStyle <- createStyle(fontColour = "#CC6600", bgFill = "#FFE5CC")
negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
naStyle <- createStyle(fontColour = "white", bgFill = "white")

# Conditional Cell Formatting for main sheet
conditionalFormatting(crxls, "riskflags", cols = 4:15, rows = 1:191, rule = "==10", style = negStyle)
conditionalFormatting(crxls, "riskflags", cols = 4:15, rows = 1:191, type = "between", rule = c(7.00, 9.99), style = medStyle)
conditionalFormatting(crxls, "riskflags", cols = 4:15, rows = 1:191, type = "between", rule = c(0, 6.999), style = posStyle)
conditionalFormatting(crxls, "riskflags", cols = 4:15, rows = 1:191, rule = '=""', style = naStyle)
conditionalFormatting(crxls, "riskflags", cols = 22:23, rows = 1:191, type = "between", rule = c(2 / 3, 1), style = negStyle)
conditionalFormatting(crxls, "riskflags", cols = 22:23, rows = 1:191, type = "between", rule = c(1 / 3, 0.665), style = medStyle)
conditionalFormatting(crxls, "riskflags", cols = 22:23, rows = 1:191, type = "between", rule = c(0, 0.332), style = posStyle)
conditionalFormatting(crxls, "riskflags", cols = 22:23, rows = 1:191, rule = '=""', style = naStyle)

# Function for the remaining tabs
cond <- function(sheet, numhigh, numlow) {
  posStyle <- createStyle(
    fontColour = "#006100",
    bgFill = "#C6EFCE"
  )
  medStyle <- createStyle(
    fontColour = "#CC6600",
    bgFill = "#FFE5CC"
  )
  negStyle <- createStyle(
    fontColour = "#9C0006",
    bgFill = "#FFC7CE"
  )
  naStyle <- createStyle(
    fontColour = "white",
    bgFill = "white"
  )
  
  conditionalFormatting(crxls, sheet, cols = numhigh:numlow, rows = 1:191, rule = "==10", style = negStyle)
  conditionalFormatting(crxls, sheet, cols = numhigh:numlow, rows = 1:191, type = "between", rule = c(7.00, 9.99), style = medStyle)
  conditionalFormatting(crxls, sheet, cols = numhigh:numlow, rows = 1:191, type = "between", rule = c(0, 6.9999), style = posStyle)
  conditionalFormatting(crxls, sheet, cols = numhigh:numlow, rows = 1:191, rule = '=""', style = naStyle)
}

# Conditional formatting of specific cells
cond("debtsheet", which(colnames(debtsheet) == "D_WB_external_debt_distress_norm"), which(colnames(debtsheet) == "D_WB_external_debt_distress_norm"))
cond("debtsheet", which(colnames(debtsheet) == "D_IMF_debt2020.2019_norm"), which(colnames(debtsheet) == "D_IMF_debt2020.2019_norm"))
cond("debtsheet", which(colnames(debtsheet) == "D_CESI_Index_norm"), which(colnames(debtsheet) == "D_CESI_Index_norm"))
cond("debtsheet", which(colnames(debtsheet) == "D_EconomicSupportIndexForDisplay_norm"), which(colnames(debtsheet) == "D_EconomicSupportIndexForDisplay_norm"))
cond("debtsheet", which(colnames(debtsheet) == "D_CPIA.scores_norm"), which(colnames(debtsheet) == "D_CPIA.scores_norm"))
cond("foodsecurity", which(colnames(foodsecurity) == "F_Proteus_Score_norm"), which(colnames(foodsecurity) == "F_Proteus_Score_norm"))
cond("foodsecurity", which(colnames(foodsecurity) == "F_fews_crm_norm"), which(colnames(foodsecurity) == "F_fews_crm_norm"))
cond("foodsecurity", which(colnames(foodsecurity) == "F_fpv_rating"), which(colnames(foodsecurity) == "F_fpv_rating"))
cond("foodsecurity", which(colnames(foodsecurity) == "F_Artemis_Score_norm"), which(colnames(foodsecurity) == "F_Artemis_Score_norm"))
cond("foodsecurity", which(colnames(foodsecurity) == "F_fpv_rating"), which(colnames(foodsecurity) == "F_fpv_rating"))
cond("fragilitysheet", which(colnames(fragilitysheet) == "Fr_FCS_FSI_Normalised"), which(colnames(fragilitysheet) == "Fr_Overall_Conflict_Risk_Score"))
cond("healthsheet", which(colnames(healthsheet) == "H_HIS_Score_norm"), which(colnames(healthsheet) == "H_HIS_Score_norm"))
cond("healthsheet", which(colnames(healthsheet) == "H_INFORM_rating.Value_norm"), which(colnames(healthsheet) == "H_INFORM_rating.Value_norm"))
cond("healthsheet", which(colnames(healthsheet) == "H_Oxrollback_score_norm"), which(colnames(healthsheet) == "H_Oxrollback_score_norm"))
cond("healthsheet", which(colnames(healthsheet) == "H_Covidgrowth_deathsnorm"), which(colnames(healthsheet) == "H_Covidgrowth_casesnorm"))
cond("healthsheet", which(colnames(healthsheet) == "H_new_cases_smoothed_per_million_norm"), which(colnames(healthsheet) == "H_new_cases_smoothed_per_million_norm"))
cond("healthsheet", which(colnames(healthsheet) == "H_new_deaths_smoothed_per_million_norm"), which(colnames(healthsheet) == "H_new_deaths_smoothed_per_million_norm"))
cond("healthsheet", which(colnames(healthsheet) == "H_add_death_prec_current_norm"), which(colnames(healthsheet) == "H_add_death_prec_current_norm"))
cond("macrosheet", which(colnames(macrosheet) == "M_GDP_WB_2019minus2020_norm"), which(colnames(macrosheet) == "M_GDP_IMF_2019minus2020_norm"))
cond("macrosheet", which(colnames(macrosheet) == "M_macrofin_risk_norm"), which(colnames(macrosheet) == "M_macrofin_risk_norm"))
cond("macrosheet", which(colnames(macrosheet) == "M_Economic_and_Financial_score_norm"), which(colnames(macrosheet) == "M_Economic_and_Financial_score_norm"))
cond("Naturalhazardsheet", which(colnames(Naturalhazardsheet) == "NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS_norm"), which(colnames(Naturalhazardsheet) == "NH_UKMO_TOTAL.RISK.NEXT.12.MONTHS_norm"))
cond("Naturalhazardsheet", which(colnames(Naturalhazardsheet) == "NH_GDAC_Hazard_Score_Norm"), which(colnames(Naturalhazardsheet) == "NH_GDAC_Hazard_Score_Norm"))
cond("Naturalhazardsheet", which(colnames(Naturalhazardsheet) == "NH_Hazard_Score_norm"), which(colnames(Naturalhazardsheet) == "NH_Hazard_Score_norm"))
cond("Naturalhazardsheet", which(colnames(Naturalhazardsheet) == "NH_multihazard_risk_norm"), which(colnames(Naturalhazardsheet) == "NH_multihazard_risk_norm"))
cond("Naturalhazardsheet", which(colnames(Naturalhazardsheet) == "NH_la_nina_risk"), which(colnames(Naturalhazardsheet) == "NH_la_nina_risk"))
cond("Socioeconomic_sheet", which(colnames(Socioeconomic_sheet) == "S_INFORM_vul_norm"), which(colnames(Socioeconomic_sheet) == "S_INFORM_vul_norm"))
cond("Socioeconomic_sheet", which(colnames(Socioeconomic_sheet) == "S_pov_prop_19_20_norm"), which(colnames(Socioeconomic_sheet) == "S_income_support.Rating_crm_norm"))
cond("Socioeconomic_sheet", which(colnames(Socioeconomic_sheet) == "S_change_unemp_norm"), which(colnames(Socioeconomic_sheet) == "S_change_unemp_norm"))
cond("Socioeconomic_sheet", which(colnames(Socioeconomic_sheet) == "S_income_support.Rating_crm_norm"), which(colnames(Socioeconomic_sheet) == "S_income_support.Rating_crm_norm"))
cond("Socioeconomic_sheet", which(colnames(Socioeconomic_sheet) == "S_Household.risks"), which(colnames(Socioeconomic_sheet) == "S_Household.risks"))
cond("Socioeconomic_sheet", which(colnames(Socioeconomic_sheet) == "S_phone_average_index_norm"), which(colnames(Socioeconomic_sheet) == "S_phone_average_index_norm"))

# Conditional formatting colours
posStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
medStyle <- createStyle(fontColour = "#CC6600", bgFill = "#FFE5CC")
negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
naStyle <- createStyle(fontColour = "white", bgFill = "white")

# Conditional Cell Formatting
conditionalFormatting(crxls, "Reliability_sheet", cols = 5:18, rows = 1:191, rule = "==1", style = negStyle)
conditionalFormatting(crxls, "Reliability_sheet", cols = 5:18, rows = 1:191, type = "between", rule = c(0.700, 0.999), style = medStyle)
conditionalFormatting(crxls, "Reliability_sheet", cols = 5:18, rows = 1:191, type = "between", rule = c(0, 0.6999), style = posStyle)
conditionalFormatting(crxls, "Reliability_sheet", cols = 5:18, rows = 1:191, rule = '=""', style = naStyle)
conditionalFormatting(crxls, "Alternativeflag_sheet", cols = c(4:6, 8:9, 11:19), rows = 1:191, type = "between", rule = c(7, 10), style = negStyle)
conditionalFormatting(crxls, "Alternativeflag_sheet", cols = c(4:6, 8:9, 11:19), rows = 1:191, type = "between", rule = c(5, 6.9999), style = medStyle)
conditionalFormatting(crxls, "Alternativeflag_sheet", cols = c(4:6, 8:9, 11:19), rows = 1:191, type = "between", rule = c(0, 4.9999), style = posStyle)
conditionalFormatting(crxls, "Alternativeflag_sheet", cols = c(4:6, 8:9, 11:19), rows = 1:191, rule = '=""', style = naStyle)

# DatabarsconditionalFormatting
conditionalFormatting(crxls, "riskflags", cols = 17:20, rows = 1:191, type = "databar", style = c("#C6EFCE", "#CD5C5C"))
conditionalFormatting(crxls, "Reliability_sheet", cols = 2:4, rows = 1:191, type = "databar", style = c("#C6EFCE", "#CD5C5C"))
conditionalFormatting(crxls, "Alternativeflag_sheet", cols = 21, rows = 1:191, type = "databar", style = c("#C6EFCE", "#CD5C5C"))

#----------------------------------Insert Global Maps---------------------------------------------------------------------
# install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(ggplot2, cowplot, lubridate, rvest, dplyr, viridis, tidyverse, countrycode)

# Loading world database
world <- map_data("world")
world <- world %>%
  dplyr::rename(Country = region) %>%
  dplyr::mutate(Country = suppressWarnings(countrycode(Country, origin = "country.name", destination = "iso3c")))

# Join datasets with risk flags
worldmap <- inner_join(world, riskflags, by = "Country")

# Map theme
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
  plot.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
  legend.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
  text = element_text(colour = "lightgrey")
)

# Draw map one
map <- ggplot(data = worldmap, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM)) +
  scale_fill_distiller(palette = "Blues", direction = 1) + # or direction=1
  ggtitle("Total Existing Compound Risk Score") +
  plain +
  labs(fill = "Total # of risks")

# Draw map two
map2 <- ggplot(data = worldmap, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM)) +
  scale_fill_distiller(palette = "Reds", direction = 1) + # or direction=1
  ggtitle("Total Emerging Compound Risk Score") +
  plain +
  labs(fill = "Total # of risks")

# Join the maps and print to the system
jointmap <- cowplot::plot_grid(map, map2, ncol = 1, align = c("hv"))
print(jointmap)

# Insert plot into the worksheet
insertPlot(crxls, 1, xy = c("AA", 5), width = 11.5, height = 9.5, fileType = "png", units = "in")

# Save plots
ggsave("Plots/Snapshots/global_emerging_map.pdf", map2, width = 11.5, height = 9.5)
ggsave("Plots/Snapshots/global_existing_map.pdf", map, width = 11.5, height = 9.5)

#----------------------------------------Save the final worksheet------------------------------------------------------
saveWorkbook(crxls, file = "Risk_sheets/Compound_Risk_Monitor.xlsx", overwrite = TRUE)


#--------------------------------------------------Sandbox---------------------------------------------------------------
# Writing excel syntax in R

syntaxTest <- c(1, 2, "=A1+B1")
RtoXL <- createWorkbook()
addWorksheet(RtoXL, "syntaxTest")
writeData(RtoXL, "syntaxTest", syntaxTest)
saveWorkbook(RtoXL, file="~/Desktop/R2XL.xlsx")

sT <- as.matrix(syntaxTest, nrow = 1, ncol = 3)

# This works
wb <- loadWorkbook("~/Desktop/R2XL.xlsx", isUnzipped = F)
writeData(wb, 1, c(5), startRow = 2, startCol = 1, colNames = F)
saveWorkbook(wb, file="~/Desktop/R2XL2.xlsx", overwrite = T)



wb
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(sT)

library(clipr)
write_clip(sT)
