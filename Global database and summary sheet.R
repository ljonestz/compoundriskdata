######################################################################################################
#
#  CODE USED TO CREATE THE GLOBAL DATABASET ON COMPOUND DISASTER RISK
#  (to be run after risk component sheets have been generated)
#
######################################################################################################

# LOAD PACKAGES ----
# install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(
  ggplot2, cowplot, lubridate, rvest, dplyr, compositions, viridis,
   countrycode, clipr, sjmisc, awalker89 / openxlsx, EnvStats, 
  gsheet, tidyverse
)
{
#
##
### ********************************************************************************************
####    CREATE GLOBAL DATABASE WITH ALL RISK SHEETS ----
### ********************************************************************************************
##
#
 
# Load risk sheets
healthsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/healthsheet.csv") %>% dplyr::select(-X)
foodsecurity <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/foodsecuritysheet.csv") %>% dplyr::select(-X)
#debtsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/debtsheet.csv") %>% dplyr::select(-X)
fragilitysheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/fragilitysheet.csv") %>% dplyr::select(-X)
macrosheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/macrosheet.csv") %>% dplyr::select(-X)
Naturalhazardsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/Naturalhazards.csv") %>% dplyr::select(-X)
Socioeconomic_sheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/Socioeconomic_sheet.csv") %>% dplyr::select(-X)
#acapssheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/acapssheet.csv") %>% dplyr::select(-X)
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv") %>% dplyr::select(-X)

# # Load risk sheets
# healthsheet <- read.csv("Risk_sheets/healthsheet.csv")[,-1] # drops first column, X, which is row number
# foodsecurity <- read.csv("Risk_sheets/foodsecuritysheet.csv")[,-1]
# fragilitysheet <- read.csv("Risk_sheets/fragilitysheet.csv")[,-1]
# macrosheet <- read.csv("Risk_sheets/macrosheet.csv")[,-1]
# Naturalhazardsheet <- read.csv("Risk_sheets/Naturalhazards.csv")[,-1]
# Socioeconomic_sheet <- read.csv("Risk_sheets/Socioeconomic_sheet.csv")[,-1]
# countrylist <- read.csv("Indicator_dataset/countrylist.csv")[,-1]

# Join datasets
# — `globalrisk` ----
globalrisk <- left_join(countrylist, healthsheet, by = c("Countryname", "Country")) %>%
  left_join(., foodsecurity, by = c("Countryname", "Country")) %>%
  # left_join(., debtsheet, by = c("Countryname", "Country")) %>%
  left_join(., fragilitysheet, by = c("Countryname", "Country")) %>%
  left_join(., macrosheet, by = c("Countryname", "Country")) %>%
  left_join(., Naturalhazardsheet, by = c("Countryname", "Country")) %>%
  left_join(., Socioeconomic_sheet, by = c("Countryname", "Country")) %>%
  # left_join(., acapssheet, by = c("Country", "Countryname")) %>%
  #dplyr::select(-X.x, -X.y, -X.x.x, -X.y.y, -X.x.x.x, -X) %>%
  distinct(Country, .keep_all = TRUE) %>%
  drop_na(Country)

# Write as csv
write.csv(globalrisk, "Risk_sheets/Global_compound_risk_database.csv")

#
##
### ********************************************************************************************
####    CREATE A FLAG SUMMARY SHEET ----
### ********************************************************************************************
##
#

# Add existing and emerging risk scores
#—`riskflags` ----
riskflags <- globalrisk %>%
  mutate(
    EXISTING_RISK_HEALTH = pmax(
      H_HIS_Score_norm,
      H_INFORM_rating.Value_norm
    ),
    EXISTING_RISK_FOOD_SECURITY = F_Proteus_Score_norm,
    EXISTING_RISK_MACRO_FISCAL = M_EIU_Score_12m_norm,
    EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY = S_INFORM_vul_norm,
    EXISTING_RISK_NATURAL_HAZARDS = pmax(
      NH_Hazard_Score_norm,
      na.rm=T
    ),
    EXISTING_RISK_FRAGILITY_INSTITUTIONS = Fr_FCS_Normalised,
    EMERGING_RISK_HEALTH = pmax(
      H_Oxrollback_score_norm,
      H_Covidgrowth_casesnorm,
      H_Covidgrowth_deathsnorm,
      H_new_cases_smoothed_per_million_norm,
      H_new_deaths_smoothed_per_million_norm,
      H_GovernmentResponseIndexForDisplay_norm,
      H_health_acaps,
      H_wmo_don_alert,
      na.rm = T
    ),
    EMERGING_RISK_FOOD_SECURITY = case_when(
      (!is.na(F_fews_crm_norm) | !is.na(F_fao_wfp_warning)) ~ as.numeric(pmax(
        F_fews_crm_norm,
        F_fao_wfp_warning,
        na.rm = T
      )),
      ((is.na(F_fews_crm_norm) | is.na(F_fao_wfp_warning)) & !is.na(F_fpv_rating)) ~  as.numeric(F_fpv_rating),
      TRUE ~ NA_real_
    ),
    EMERGING_RISK_MACRO_FISCAL = M_EIU_12m_change_norm,
    EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY = pmax(
      S_pov_comb_norm,
      S_change_unemp_norm,
      S_income_support.Rating_crm_norm,
      S_Household.risks,
      S_phone_average_index_norm,
      na.rm = T
    ),
    EMERGING_RISK_NATURAL_HAZARDS = as.numeric(pmax(
      NH_GDAC_Hazard_Score_Norm,
      NH_natural_acaps,
      NH_seasonal_risk_norm,
      NH_locust_norm,
      na.rm = T
    )),
    EMERGING_RISK_FRAGILITY_INSTITUTIONS =pmax(
      Fr_REIGN_Normalised,
      Fr_Displaced_UNHCR_Normalised,
      Fr_BRD_Normalised,
      na.rm = T
    )
  ) %>%
  dplyr::select(
    Countryname, Country, EXISTING_RISK_HEALTH, EXISTING_RISK_FOOD_SECURITY,
    EXISTING_RISK_MACRO_FISCAL, EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY,
    EXISTING_RISK_NATURAL_HAZARDS, EXISTING_RISK_FRAGILITY_INSTITUTIONS,
    EMERGING_RISK_HEALTH, EMERGING_RISK_FOOD_SECURITY, EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY, EMERGING_RISK_MACRO_FISCAL,
    EMERGING_RISK_NATURAL_HAZARDS, EMERGING_RISK_FRAGILITY_INSTITUTIONS #, F_fews_crm_norm
  )

# —Create ternary risk flags----
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

# —Calculate total compound risk scores ----
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

riskflags$TOTAL_EXISTING_COMPOUND_RISK_SCORE_MED <- as.numeric(unlist(riskflags$TOTAL_EXISTING_COMPOUND_RISK_SCORE + (riskflags$medium_risk_existing / 2)))
riskflags$TOTAL_EMERGING_COMPOUND_RISK_SCORE_MED <- as.numeric(unlist(riskflags$TOTAL_EMERGING_COMPOUND_RISK_SCORE + (riskflags$medium_risk_emerging / 2)))

# Drop ternary rates (may want to reinstate in the future)
riskflags <- riskflags %>%
  dplyr::select(
    -medium_risk_emerging, -medium_risk_existing, -all_of(paste0(vars, "_RISKLEVEL"))
  ) %>%
  distinct(Country, .keep_all = TRUE) %>%
  drop_na(Country)

#
##
### ********************************************************************************************
####    CREATE DATABASE OF ALTERNATIVE RISK SCORES ----
### ********************************************************************************************
##
#

# —Alternative combined risk scores ----
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
  dplyr::select(-contains("_plus1"))

# Alternative combined total scores
altflag <- globalrisk
names <- c(
  "S_INFORM_vul_norm", "H_Oxrollback_score_norm", "H_wmo_don_alert",
  "H_Covidgrowth_casesnorm", "H_Covidgrowth_deathsnorm", "H_HIS_Score_norm", "H_INFORM_rating.Value_norm",
  "H_new_cases_smoothed_per_million_norm", "H_new_deaths_smoothed_per_million_norm",
  "F_Proteus_Score_norm", "F_fews_crm_norm", "F_fao_wfp_warning", "F_fpv_rating", #"D_WB_external_debt_distress_norm",
  "M_EIU_12m_change_norm", "M_EIU_Score_12m_norm",
   "NH_GDAC_Hazard_Score_Norm",  "H_GovernmentResponseIndexForDisplay_norm",  "H_GovernmentResponseIndexForDisplay_norm", 
  "S_Household.risks", "S_phone_average_index_norm",  "NH_seasonal_risk_norm","NH_locust_norm", "NH_natural_acaps","Fr_FCS_Normalised", 
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
      H_GovernmentResponseIndexForDisplay_norm_plus1,
      H_wmo_don_alert_plus1,
      na.rm = T
      )),
    EMERGING_RISK_MACRO_FISCAL_AV = M_EIU_12m_change_norm,
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

#--------------------------------—Calculate Coefficient of Variation----------------------------------------------------------------
altflag <- altflag %>%
  rowwise() %>%
  mutate(
    H_coefvar = cv(c( # Should these match the selected indicators?
      H_Oxrollback_score_norm,
      H_Covidgrowth_casesnorm,
      H_Covidgrowth_deathsnorm,
      H_new_cases_smoothed_per_million_norm,
      H_new_deaths_smoothed_per_million_norm,
      # H_add_death_prec_current,
      H_wmo_don_alert),
      na.rm = T
      ),
    M_coefvar = cv(c(M_EIU_12m_change_norm),
      na.rm = T
    ),
    Fr_coefvar = cv(c(
      Fr_REIGN_Normalised,
      Fr_Displaced_UNHCR_Normalised,
      Fr_BRD_Normalised),
      na.rm = T
    ),
    NH_coefvar = cv(c(
      NH_GDAC_Hazard_Score_Norm,
      NH_natural_acaps,
      NH_seasonal_risk_norm),
      na.rm = T
    ),
    F_coefvar = cv(c(
      F_fpv_rating,
      F_fpv_rating),
      na.rm = T
    ),
    S_coefvar = cv(c(
      S_pov_comb_norm,
      S_change_unemp_20_norm,
      S_income_support.Rating_crm_norm,
      na.rm = T
    ))
  )

# Merge datasets to include alt variables
riskflags <- inner_join(riskflags,
                        altflag,
                        by = c("Country", "Countryname"),
                        keep = F
)

#-----------------------------—Calculate Overall Alert w/ Geometric Mean ----------------------------------------------
# Geometric mean of existing and emerging (and emerging when existing is NA)
# This is how we are now calculating overall alert, though here it's called emerging risk sq

riskflags <- riskflags %>%
  mutate(
    OVERALL_HEALTH_GEO = case_when(
      !is.na(EXISTING_RISK_HEALTH) ~ sqrt(EXISTING_RISK_HEALTH * EMERGING_RISK_HEALTH),
      TRUE ~ EMERGING_RISK_HEALTH
    ),
    # OVERALL_HEALTH_GEO_ALT = case_when(
    #   !is.na(EXISTING_RISK_HEALTH) ~ sqrt(EXISTING_RISK_HEALTH * EMERGING_RISK_HEALTH_SQ_ALT),
    #   TRUE ~ EMERGING_RISK_HEALTH
    # ),
    OVERALL_FOOD_SECURITY_GEO = case_when(
      !is.na(EXISTING_RISK_FOOD_SECURITY) ~ sqrt(EXISTING_RISK_FOOD_SECURITY * EMERGING_RISK_FOOD_SECURITY),
      TRUE ~ EMERGING_RISK_FOOD_SECURITY
    ),
    OVERALL_MACRO_FISCAL_GEO = case_when(
      !is.na(EXISTING_RISK_MACRO_FISCAL) ~ sqrt(EXISTING_RISK_MACRO_FISCAL * EMERGING_RISK_MACRO_FISCAL),
      TRUE ~ EMERGING_RISK_MACRO_FISCAL
    ),
    OVERALL_SOCIOECONOMIC_VULNERABILITY_GEO = case_when(
      !is.na(EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY) ~ sqrt(EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY * EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY),
      TRUE ~ as.numeric(EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY)
    ),
    OVERALL_NATURAL_HAZARDS_GEO = case_when(
      !is.na(EXISTING_RISK_NATURAL_HAZARDS) ~ sqrt(EXISTING_RISK_NATURAL_HAZARDS * EMERGING_RISK_NATURAL_HAZARDS),
      TRUE ~ EMERGING_RISK_NATURAL_HAZARDS
    ),
    OVERALL_FRAGILITY_INSTITUTIONS_GEO = case_when(
      !is.na(EXISTING_RISK_FRAGILITY_INSTITUTIONS) ~ sqrt(EXISTING_RISK_FRAGILITY_INSTITUTIONS * EMERGING_RISK_FRAGILITY_INSTITUTIONS),
      TRUE ~ EMERGING_RISK_FRAGILITY_INSTITUTIONS
    )
  )

# Calculate total emerging risk scores for SQ
geonam <- c(
  "OVERALL_HEALTH_GEO", "OVERALL_FOOD_SECURITY_GEO",
  "OVERALL_MACRO_FISCAL_GEO","OVERALL_SOCIOECONOMIC_VULNERABILITY_GEO",
  "OVERALL_NATURAL_HAZARDS_GEO",
  "OVERALL_FRAGILITY_INSTITUTIONS_GEO"
)

# Emerging risk score as all high risk scores
riskflags$OVERALL_FLAGS_GEO <- rowSums(riskflags[geonam] >= 7, na.rm = T) 

# Emerging risk score as high + med
riskflags$OVERALL_FLAGS_GEO_MED <-  rowSums(riskflags[geonam] >= 7, na.rm = T) + (rowSums(riskflags[geonam] < 7 & riskflags[geonam] >= 5, na.rm = T) / 2)


#-----------------------------—Calculate Overall Alert w/ Arithmetic Mean ----------------------------------------------
riskflags <- riskflags %>%
  mutate(
    OVERALL_HEALTH_AV = case_when(
      !is.na(EXISTING_RISK_HEALTH) ~ (EXISTING_RISK_HEALTH + EMERGING_RISK_HEALTH)/2,
      TRUE ~ EMERGING_RISK_HEALTH
    ),
    OVERALL_FOOD_SECURITY_AV = case_when(
      !is.na(EXISTING_RISK_FOOD_SECURITY) ~ (EXISTING_RISK_FOOD_SECURITY + EMERGING_RISK_FOOD_SECURITY)/2,
      TRUE ~ EMERGING_RISK_FOOD_SECURITY
    ),
    OVERALL_MACRO_FISCAL_AV = case_when(
      !is.na(EXISTING_RISK_MACRO_FISCAL) ~ (EXISTING_RISK_MACRO_FISCAL + EMERGING_RISK_MACRO_FISCAL)/2,
      TRUE ~ EMERGING_RISK_MACRO_FISCAL
    ),
    OVERALL_SOCIOECONOMIC_VULNERABILITY_AV = case_when(
      !is.na(EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY) ~ (EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY + EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY)/2,
      TRUE ~ as.numeric(EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY)
    ),
    OVERALL_NATURAL_HAZARDS_AV = case_when(
      !is.na(EXISTING_RISK_NATURAL_HAZARDS) ~ (EXISTING_RISK_NATURAL_HAZARDS + EMERGING_RISK_NATURAL_HAZARDS)/2,
      TRUE ~ EMERGING_RISK_NATURAL_HAZARDS
    ),
    OVERALL_FRAGILITY_INSTITUTIONS_AV = case_when(
      !is.na(EXISTING_RISK_FRAGILITY_INSTITUTIONS) ~ (EXISTING_RISK_FRAGILITY_INSTITUTIONS + EMERGING_RISK_FRAGILITY_INSTITUTIONS)/2,
      TRUE ~ EMERGING_RISK_FRAGILITY_INSTITUTIONS
    )
  )

# Calculate total emerging risk scores for SQ
arithnam <- c(
  "OVERALL_HEALTH_AV", "OVERALL_FOOD_SECURITY_AV",
  "OVERALL_MACRO_FISCAL_AV","OVERALL_SOCIOECONOMIC_VULNERABILITY_AV",
  "OVERALL_NATURAL_HAZARDS_AV",
  "OVERALL_FRAGILITY_INSTITUTIONS_AV"
)

# Emerging risk score as all high risk scores
riskflags$OVERALL_SCORE_AV <- rowSums(riskflags[arithnam] >= 7, na.rm = T) 

# Emerging risk score as high + med
riskflags$OVERALL_SCORE_AV_MED <-  rowSums(riskflags[arithnam] >= 7, na.rm = T) + (rowSums(riskflags[arithnam] < 7 & riskflags[arithnam] >= 5, na.rm = T) / 2)


#-----------------------------—Calculate Overall Alert w/ Filter: Emerging & Existing == 10 ----------------------------------------------
riskflags <- riskflags %>%
  mutate(
    OVERALL_HEALTH_FILTER_10 = case_when(
      EXISTING_RISK_HEALTH == 10 & EMERGING_RISK_HEALTH == 10 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_FOOD_SECURITY_FILTER_10 = case_when(
      EXISTING_RISK_FOOD_SECURITY == 10 & EMERGING_RISK_FOOD_SECURITY == 10 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_MACRO_FISCAL_FILTER_10 = case_when(
      EXISTING_RISK_MACRO_FISCAL == 10 & EMERGING_RISK_MACRO_FISCAL == 10 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_SOCIOECONOMIC_VULNERABILITY_FILTER_10 = case_when(
      EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY == 10 & EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY == 10 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_NATURAL_HAZARDS_FILTER_10 = case_when(
      EXISTING_RISK_NATURAL_HAZARDS == 10 & EMERGING_RISK_NATURAL_HAZARDS == 10 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_FRAGILITY_INSTITUTIONS_FILTER_10 = case_when(
      EXISTING_RISK_FRAGILITY_INSTITUTIONS == 10 & EMERGING_RISK_FRAGILITY_INSTITUTIONS == 10 ~ 10,
      TRUE ~ 0
    )
  )

# Calculate total emerging risk scores for SQ
filter10nam <- c(
  "OVERALL_HEALTH_FILTER_10", "OVERALL_FOOD_SECURITY_FILTER_10",
  "OVERALL_MACRO_FISCAL_FILTER_10","OVERALL_SOCIOECONOMIC_VULNERABILITY_FILTER_10",
  "OVERALL_NATURAL_HAZARDS_FILTER_10",
  "OVERALL_FRAGILITY_INSTITUTIONS_FILTER_10"
)

# Emerging risk score as all high risk scores
riskflags$OVERALL_SCORE_FILTER_10 <- rowSums(riskflags[filter10nam] >= 7, na.rm = T) 

# Emerging risk score as high + med
riskflags$OVERALL_SCORE_FILTER_10_MED <-  rowSums(riskflags[filter10nam] >= 7, na.rm = T) + (rowSums(riskflags[filter10nam] < 7 & riskflags[filter10nam] >= 5, na.rm = T) / 2)

#-----------------------------—Calculate Overall Alert w/ Filter: Emerging & Existing >= 7 ----------------------------------------------
riskflags <- riskflags %>%
  mutate(
    OVERALL_HEALTH_FILTER_7 = case_when(
      EXISTING_RISK_HEALTH >= 7 & EMERGING_RISK_HEALTH >= 7 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_FOOD_SECURITY_FILTER_7 = case_when(
      EXISTING_RISK_FOOD_SECURITY >= 7 & EMERGING_RISK_FOOD_SECURITY >= 7 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_MACRO_FISCAL_FILTER_7 = case_when(
      EXISTING_RISK_MACRO_FISCAL >= 7 & EMERGING_RISK_MACRO_FISCAL >= 7 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_SOCIOECONOMIC_VULNERABILITY_FILTER_7 = case_when(
      EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY >= 7 & EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY >= 7 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_NATURAL_HAZARDS_FILTER_7 = case_when(
      EXISTING_RISK_NATURAL_HAZARDS >= 7 & EMERGING_RISK_NATURAL_HAZARDS >= 7 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_FRAGILITY_INSTITUTIONS_FILTER_7 = case_when(
      EXISTING_RISK_FRAGILITY_INSTITUTIONS >= 7 & EMERGING_RISK_FRAGILITY_INSTITUTIONS >= 7 ~ 10,
      TRUE ~ 0
    )
  )

# Calculate total emerging risk scores for SQ
filter7nam <- c(
  "OVERALL_HEALTH_FILTER_7", "OVERALL_FOOD_SECURITY_FILTER_7",
  "OVERALL_MACRO_FISCAL_FILTER_7","OVERALL_SOCIOECONOMIC_VULNERABILITY_FILTER_7",
  "OVERALL_NATURAL_HAZARDS_FILTER_7",
  "OVERALL_FRAGILITY_INSTITUTIONS_FILTER_7"
)

# Emerging risk score as all high risk scores
riskflags$OVERALL_SCORE_FILTER_7 <- rowSums(riskflags[filter7nam] >= 7, na.rm = T) 

# Emerging risk score as high + med
riskflags$OVERALL_SCORE_FILTER_7_MED <-  rowSums(riskflags[filter7nam] >= 7, na.rm = T) + (rowSums(riskflags[filter7nam] < 7 & riskflags[filter7nam] >= 5, na.rm = T) / 2)

#-----------------------------—Calculate Overall Alert w/ Filter: Emerging & Existing >= 7, Medium if one is below 7 but >= 5 ----------------------------------------------
riskflags <- riskflags %>%
  mutate(
    OVERALL_HEALTH_FILTER_7_5 = case_when(
      EXISTING_RISK_HEALTH >= 7 & EMERGING_RISK_HEALTH >= 7 ~ 10,
      EXISTING_RISK_HEALTH >= 7 & EMERGING_RISK_HEALTH >= 5 ~ 5,
      EXISTING_RISK_HEALTH >= 5 & EMERGING_RISK_HEALTH >= 7 ~ 5,
      TRUE ~ 0
    ),
    OVERALL_FOOD_SECURITY_FILTER_7_5 = case_when(
      EXISTING_RISK_FOOD_SECURITY >= 7 & EMERGING_RISK_FOOD_SECURITY >= 7 ~ 10,
      EXISTING_RISK_FOOD_SECURITY >= 7 & EMERGING_RISK_FOOD_SECURITY >= 5 ~ 5,
      EXISTING_RISK_FOOD_SECURITY >= 5 & EMERGING_RISK_FOOD_SECURITY >= 7 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_MACRO_FISCAL_FILTER_7_5 = case_when(
      EXISTING_RISK_MACRO_FISCAL >= 7 & EMERGING_RISK_MACRO_FISCAL >= 7 ~ 10,
      EXISTING_RISK_MACRO_FISCAL >= 7 & EMERGING_RISK_MACRO_FISCAL >= 5 ~ 5,
      EXISTING_RISK_MACRO_FISCAL >= 5 & EMERGING_RISK_MACRO_FISCAL >= 7 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_SOCIOECONOMIC_VULNERABILITY_FILTER_7_5 = case_when(
      EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY >= 7 & EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY >= 7 ~ 10,
      EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY >= 7 & EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY >= 5 ~ 5,
      EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY >= 5 & EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY >= 7 ~ 5,
      TRUE ~ 0
    ),
    OVERALL_NATURAL_HAZARDS_FILTER_7_5 = case_when(
      EXISTING_RISK_NATURAL_HAZARDS >= 7 & EMERGING_RISK_NATURAL_HAZARDS >= 7 ~ 10,
      EXISTING_RISK_NATURAL_HAZARDS >= 7 & EMERGING_RISK_NATURAL_HAZARDS >= 5 ~ 5,
      EXISTING_RISK_NATURAL_HAZARDS >= 5 & EMERGING_RISK_NATURAL_HAZARDS >= 7 ~ 5,
      TRUE ~ 0
    ),
    OVERALL_FRAGILITY_INSTITUTIONS_FILTER_7_5 = case_when(
      EXISTING_RISK_FRAGILITY_INSTITUTIONS >= 7 & EMERGING_RISK_FRAGILITY_INSTITUTIONS >= 7 ~ 10,
      EXISTING_RISK_FRAGILITY_INSTITUTIONS >= 7 & EMERGING_RISK_FRAGILITY_INSTITUTIONS >= 5 ~ 5,
      EXISTING_RISK_FRAGILITY_INSTITUTIONS >= 5 & EMERGING_RISK_FRAGILITY_INSTITUTIONS >= 7 ~ 5,
      TRUE ~ 0
    )
  )

# Calculate total emerging risk scores for SQ
filter75nam <- c(
  "OVERALL_HEALTH_FILTER_7_5", "OVERALL_FOOD_SECURITY_FILTER_7_5",
  "OVERALL_MACRO_FISCAL_FILTER_7_5","OVERALL_SOCIOECONOMIC_VULNERABILITY_FILTER_7_5",
  "OVERALL_NATURAL_HAZARDS_FILTER_7_5",
  "OVERALL_FRAGILITY_INSTITUTIONS_FILTER_7_5"
)

# Emerging risk score as all high risk scores
riskflags$OVERALL_SCORE_FILTER_7_5 <- rowSums(riskflags[filter75nam] >= 7, na.rm = T) 

# Emerging risk score as high + med
riskflags$OVERALL_SCORE_FILTER_7_5_MED <-  rowSums(riskflags[filter75nam] >= 7, na.rm = T) + (rowSums(riskflags[filter75nam] < 7 & riskflags[filter75nam] >= 5, na.rm = T) / 2)

#
##
### ********************************************************************************************
####    CREATE DATABASE OF RELIABILITY SCORES ----
### ********************************************************************************************
##
#

# Calculate the number of missing values in each of the source indicators for the various risk components (as a proportion)
reliabilitysheet <- globalrisk %>%
  mutate(
    RELIABILITY_EXISTING_HEALTH = rowSums(is.na(globalrisk %>%
                                                                   dplyr::select(H_HIS_Score_norm, H_INFORM_rating.Value_norm)),
                                                           na.rm = T
    ) / 2,
    RELIABILITY_EXISTING_FOOD_SECURITY = case_when(
      is.na(F_Proteus_Score_norm) ~ 1,
      TRUE ~ 0
    ),
    RELIABILITY_EXISTING_MACRO_FISCAL = rowSums(is.na(globalrisk %>%
                                                        dplyr::select(
                                                          M_EIU_Score_12m_norm 
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
      is.na(Fr_FCS_Normalised) ~ 1,
      TRUE ~ 0
    ),
    RELIABILITY_EMERGING_HEALTH = rowSums(is.na(globalrisk %>% 
                                                  dplyr::select(
                                                    H_Oxrollback_score_norm,
                                                    H_Covidgrowth_casesnorm,
                                                    H_Covidgrowth_deathsnorm,
                                                    H_new_cases_smoothed_per_million_norm,
                                                    H_new_deaths_smoothed_per_million_norm,
                                                    H_GovernmentResponseIndexForDisplay_norm
                                                  )),
    na.rm = T
    ) / 6,
    RELIABILITY_EMERGING_FOOD_SECURITY = rowSums(is.na(globalrisk %>%
                                                         dplyr::select(
                                                           F_fews_crm_norm,
                                                           F_fao_wfp_warning,
                                                           F_fpv_rating,
                                                         )),
                                                 na.rm = T
    ) / 2,
    EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY = rowSums(is.na(globalrisk %>%
                                                                dplyr::select(
                                                                  S_pov_comb_norm,
                                                                  S_change_unemp_norm,
                                                                  S_income_support.Rating_crm_norm,
                                                                  S_Household.risks,
                                                                  S_phone_average_index_norm
                                                                )),
                                                        na.rm = T
    ) / 3,
    RELIABILITY_EMERGING_MACRO_FISCAL = rowSums(is.na(globalrisk %>%
                                                        dplyr::select(
                                                          M_EIU_12m_change_norm
                                                        )),
                                                na.rm = T
    ) / 4,
    RELIABILITY_EMERGING_NATURAL_HAZARDS = rowSums(is.na(globalrisk %>%
                                                           dplyr::select(
                                                             NH_GDAC_Hazard_Score_Norm,
                                                             NH_Hazard_Score_norm
                                                           )),
                                                   na.rm = T
    ) / 3,
    RELIABILITY_EMERGING_FRAGILITY_INSTITUTIONS = rowSums(is.na(globalrisk %>%
                                                                  dplyr::select(
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
    RELIABILITY_SCORE_EXISTING_RISK = round(rowMeans(dplyr::select(., starts_with("RELIABILITY_EXISTING"))), 1),
    RELIABILITY_SCORE_EMERGING_RISK = round(rowMeans(dplyr::select(., starts_with("RELIABILITY_EMERGING"))), 1)
  ) %>%
  dplyr::select(
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

#------------------------------—Combine the reliability sheet with the global database------------------------------------
reliable <- reliabilitysheet %>%
  dplyr::select(Countryname, Country, RELIABILITY_SCORE_EXISTING_RISK, RELIABILITY_SCORE_EMERGING_RISK)

globalrisk <- left_join(globalrisk, reliable, by = c("Countryname", "Country"))

# Save database of all risk indicators (+ reliability scores)
write.csv(globalrisk, "Risk_Sheets/Global_compound_risk_database.csv")

#------------------------------—Combine the reliability sheet with the summary risk flag sheet-----------------------------
reliable <- reliabilitysheet %>%
  dplyr::select(Countryname, Country, RELIABILITY_SCORE_EXISTING_RISK, RELIABILITY_SCORE_EMERGING_RISK)

riskflags <- left_join(riskflags %>%
                         dplyr::select(
                           "Countryname", "Country",
                           contains(c("_AV", "_SQ", "_ALT", "_FILTER", "_GEO", "EXISTING_", "EMERGING_", "coefvar"))
                         ),
                       reliable, 
                       by = c("Countryname", "Country")) %>% 
  dplyr::select(-contains("S_phone"))

# Write csv file of all risk flags (+reliability scores)

write.csv(riskflags, "Risk_Sheets/Compound_Risk_Flag_Sheets.csv")
}
# 
# #
# ##
# ### ********************************************************************************************
# ####    CREATE SUMMARY EXCEL FILE ----
# ### ********************************************************************************************
# ##
# #
# 
# # dplyr::select relevant variables
# riskset <- riskflags %>%
#   dplyr::select(
#     Countryname, Country, EXISTING_RISK_HEALTH,
#     EXISTING_RISK_FOOD_SECURITY, EXISTING_RISK_MACRO_FISCAL, EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY,
#     EXISTING_RISK_NATURAL_HAZARDS, EXISTING_RISK_FRAGILITY_INSTITUTIONS,
#     EMERGING_RISK_HEALTH, EMERGING_RISK_FOOD_SECURITY,
#     EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY,
#     EMERGING_RISK_MACRO_FISCAL,
#     EMERGING_RISK_NATURAL_HAZARDS, EMERGING_RISK_FRAGILITY_INSTITUTIONS,
#     TOTAL_EXISTING_COMPOUND_RISK_SCORE, TOTAL_EMERGING_COMPOUND_RISK_SCORE,
#     TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM,TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM, 
#     TOTAL_EMERGING_COMPOUND_RISK_SCORE_SQ, TOTAL_EMERGING_COMPOUND_RISK_SCORE_SQ_MED,
#     RELIABILITY_SCORE_EXISTING_RISK, RELIABILITY_SCORE_EMERGING_RISK
#   )
# 
# # Add blank columns to riskflags dataset
# riskflagsblank <- riskset %>%
#   arrange(Country) %>%
#   add_column(" " = NA, .after = "Country") %>%
#   add_column("  " = NA, .after = "EMERGING_RISK_FRAGILITY_INSTITUTIONS") %>%
#   add_column("   " = NA, .after = "TOTAL_EMERGING_COMPOUND_RISK_SCORE_SQ_MED")
# 
# # Create Excel
# crxls <- createWorkbook()
# addWorksheet(crxls, "riskflags", tabColour = "lightgrey")
# writeData(crxls, "riskflags", riskflagsblank, colNames = TRUE)
# addWorksheet(crxls, "debtsheet", tabColour = "lightblue")
# writeData(crxls, "debtsheet", debtsheet, colNames = TRUE)
# addWorksheet(crxls, "foodsecurity", tabColour = "lightgreen")
# writeData(crxls, "foodsecurity", foodsecurity, colNames = TRUE)
# addWorksheet(crxls, "fragilitysheet", tabColour = "orange")
# writeData(crxls, "fragilitysheet", fragilitysheet, colNames = TRUE)
# addWorksheet(crxls, "healthsheet", tabColour = "yellow")
# writeData(crxls, "healthsheet", healthsheet, colNames = TRUE)
# addWorksheet(crxls, "macrosheet", tabColour = "lightpink")
# writeData(crxls, "macrosheet", macrosheet, colNames = TRUE)
# addWorksheet(crxls, "Naturalhazardsheet", tabColour = "brown")
# writeData(crxls, "Naturalhazardsheet", Naturalhazardsheet, colNames = TRUE)
# addWorksheet(crxls, "Socioeconomic_sheet", tabColour = "lightblue")
# writeData(crxls, "Socioeconomic_sheet", Socioeconomic_sheet, colNames = TRUE)
# addWorksheet(crxls, "Reliability_sheet", tabColour = "grey")
# writeData(crxls, "Reliability_sheet", reliabilitysheet, colNames = TRUE)
# 
# # Insert alternative flag sheet
# addWorksheet(crxls, "Alternativeflag_sheet", tabColour = "#9999CC")
# # dplyr::select relevant variables
# alt <- riskflags %>%
#   dplyr::select(Countryname, Country, contains("_AV"), contains("_MULTIDIMENSIONAL"), contains("SQ"), contains("coefvar"), -contains("OCHA")) %>%
#   arrange(Country)
# # Add blank columns
# alt <- alt %>%
#   add_column(" " = NA, .after = "Country") %>%
#   add_column("  " = NA, .after = "EMERGING_RISK_FRAGILITY_INSTITUTIONS_AV") %>%
#   add_column("   " = NA, .after = "EMERGING_RISK_FRAGILITY_INSTITUTIONS_MULTIDIMENSIONAL") %>%
#   add_column("    " = NA, .after = "EMERGING_RISK_FRAGILITY_INSTITUTIONS_SQ") %>%
#   add_column("     " = NA, .after = "TOTAL_EMERGING_COMPOUND_RISK_SCORE_SQ")
# # Writesheet
# writeData(crxls, "Alternativeflag_sheet", alt, colNames = TRUE)
# 
# #-----------------------------------------Conditional formatting-------------------------------------------------
# 
# # Colour and stlye sheets
# Map(function(number, tab) {
#   headerStyle <- createStyle(
#     fontSize = 10,
#     # fontColour = "#FFFFFF",
#     # textDecoration = "bold",
#     halign = "center",
#     # valign = "center",
#     # fgFill = "#000000",
#     border = c("bottom", "left", "right"),
#     borderColour = c("black", "white", "white"),
#     borderStyle = c("thin", "thick", "thick"),
#     wrapText = TRUE,
#     # textRotation = 90
#   )
#   
#   addStyle(crxls,
#            sheet = number,
#            headerStyle,
#            rows = 1,
#            cols = 1:57,
#            gridExpand = TRUE
#   )
#   
#   bodyStyle <- createStyle(
#     fgFill = "white",
#     border = "TopBottomLeftRight", # might drop Top to not over ride header black border
#     borderColour = "white",
#     halign = "center"
#   )
#   
#   addStyle(crxls,
#            sheet = number,
#            bodyStyle,
#            rows = 2:191,
#            cols = 1:57,
#            gridExpand = TRUE
#   )
#   
#   setColWidths(crxls, number, cols = 1, widths = 10) ## set column width for row names column
#   setRowHeights(crxls, number, rows = 1, heights = 150) ## set column width for row names column
#   
#   modifyBaseFont(crxls,
#                  fontSize = 12,
#                  fontColour = "black",
#                  fontName = "Arial"
#   )
# }, c(1:10))
# 
# # Set specific style for the risk tab sheet
# headerStyle <- createStyle(
#   fontSize = 10,
#   # fontColour = "white",
#   # textDecoration = "bold",
#   halign = "center",
#   # valign = "center",
#   # fgFill = "lightslategray",
#   border = c("bottom", "left", "right"),
#   borderColour = c("black", "white", "white"),
#   borderStyle = c("thin", "thick", "thick"),
#   wrapText = TRUE,
#   # textRotation = 90
# )
# 
# addStyle(
#   crxls,
#   sheet = 1,
#   headerStyle,
#   rows = 1,
#   cols = 3:16,
#   gridExpand = TRUE
# )
# 
# addStyle(crxls,
#          sheet = 1,
#          headerStyle2,
#          rows = 1,
#          cols = c(3, 16, 23, 26:51),
#          gridExpand = TRUE
# )
# 
# setColWidths(crxls, 1, cols = 1, widths = 10) ## set column width for row names column
# setRowHeights(crxls, 1, rows = 1, heights = 150) ## set column width for row names column
# 
# # Conditional formatting colours for main sheet
# posStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
# medStyle <- createStyle(fontColour = "#CC6600", bgFill = "#FFE5CC")
# negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
# naStyle <- createStyle(fontColour = "white", bgFill = "white")
# 
# # Conditional Cell Formatting for main sheet
# conditionalFormatting(crxls, "riskflags", cols = 4:15, rows = 1:191, rule = "==10", style = negStyle)
# conditionalFormatting(crxls, "riskflags", cols = 4:15, rows = 1:191, type = "between", rule = c(7.00, 9.99), style = medStyle)
# conditionalFormatting(crxls, "riskflags", cols = 4:15, rows = 1:191, type = "between", rule = c(0, 6.999), style = posStyle)
# conditionalFormatting(crxls, "riskflags", cols = 4:15, rows = 1:191, rule = '=""', style = naStyle)
# conditionalFormatting(crxls, "riskflags", cols = 24:25, rows = 1:191, type = "between", rule = c(2 / 3, 1), style = negStyle)
# conditionalFormatting(crxls, "riskflags", cols = 24:25, rows = 1:191, type = "between", rule = c(1 / 3, 0.665), style = medStyle)
# conditionalFormatting(crxls, "riskflags", cols = 24:25, rows = 1:191, type = "between", rule = c(0, 0.332), style = posStyle)
# conditionalFormatting(crxls, "riskflags", cols = 24:25, rows = 1:191, rule = '=""', style = naStyle)
# 
# # Function for the remaining tabs
# cond <- function(sheet, numhigh, numlow) {
#   posStyle <- createStyle(
#     fontColour = "#006100",
#     bgFill = "#C6EFCE"
#   )
#   medStyle <- createStyle(
#     fontColour = "#CC6600",
#     bgFill = "#FFE5CC"
#   )
#   negStyle <- createStyle(
#     fontColour = "#9C0006",
#     bgFill = "#FFC7CE"
#   )
#   naStyle <- createStyle(
#     fontColour = "white",
#     bgFill = "white"
#   )
#   
#   conditionalFormatting(crxls, sheet, cols = numhigh:numlow, rows = 1:191, rule = "==10", style = negStyle)
#   conditionalFormatting(crxls, sheet, cols = numhigh:numlow, rows = 1:191, type = "between", rule = c(7.00, 9.99), style = medStyle)
#   conditionalFormatting(crxls, sheet, cols = numhigh:numlow, rows = 1:191, type = "between", rule = c(0, 6.9999), style = posStyle)
#   conditionalFormatting(crxls, sheet, cols = numhigh:numlow, rows = 1:191, rule = '=""', style = naStyle)
# }
# 
# # Conditional formatting of specific cells
# cond("debtsheet", which(colnames(debtsheet) == "D_WB_external_debt_distress_norm"), which(colnames(debtsheet) == "D_WB_external_debt_distress_norm"))
# cond("debtsheet", which(colnames(debtsheet) == "D_IMF_debt2020.2019_norm"), which(colnames(debtsheet) == "D_IMF_debt2020.2019_norm"))
# cond("debtsheet", which(colnames(debtsheet) == "D_CESI_Index_norm"), which(colnames(debtsheet) == "D_CESI_Index_norm"))
# cond("debtsheet", which(colnames(debtsheet) == "D_EconomicSupportIndexForDisplay_norm"), which(colnames(debtsheet) == "D_EconomicSupportIndexForDisplay_norm"))
# cond("debtsheet", which(colnames(debtsheet) == "D_CPIA.scores_norm"), which(colnames(debtsheet) == "D_CPIA.scores_norm"))
# cond("foodsecurity", which(colnames(foodsecurity) == "F_Proteus_Score_norm"), which(colnames(foodsecurity) == "F_Proteus_Score_norm"))
# cond("foodsecurity", which(colnames(foodsecurity) == "F_fews_crm_norm"), which(colnames(foodsecurity) == "F_fews_crm_norm"))
# cond("foodsecurity", which(colnames(foodsecurity) == "F_fao_wfp_warning"), which(colnames(foodsecurity) == "F_fao_wfp_warning"))
# cond("foodsecurity", which(colnames(foodsecurity) == "F_fpv_rating"), which(colnames(foodsecurity) == "F_fpv_rating"))
# cond("fragilitysheet", which(colnames(fragilitysheet) == "Fr_FCS_Normalised"), which(colnames(fragilitysheet) == "Fr_Overall_Conflict_Risk_Score"))
# cond("healthsheet", which(colnames(healthsheet) == "H_HIS_Score_norm"), which(colnames(healthsheet) == "H_HIS_Score_norm"))
# cond("healthsheet", which(colnames(healthsheet) == "H_INFORM_rating.Value_norm"), which(colnames(healthsheet) == "H_INFORM_rating.Value_norm"))
# cond("healthsheet", which(colnames(healthsheet) == "H_Oxrollback_score_norm"), which(colnames(healthsheet) == "H_Oxrollback_score_norm"))
# cond("healthsheet", which(colnames(healthsheet) == "H_Covidgrowth_deathsnorm"), which(colnames(healthsheet) == "H_Covidgrowth_casesnorm"))
# cond("healthsheet", which(colnames(healthsheet) == "H_new_cases_smoothed_per_million_norm"), which(colnames(healthsheet) == "H_new_cases_smoothed_per_million_norm"))
# cond("healthsheet", which(colnames(healthsheet) == "H_new_deaths_smoothed_per_million_norm"), which(colnames(healthsheet) == "H_new_deaths_smoothed_per_million_norm"))
# cond("healthsheet", which(colnames(healthsheet) == "H_GovernmentResponseIndexForDisplay_norm"), which(colnames(healthsheet) == "H_GovernmentResponseIndexForDisplay_norm"))
# cond("macrosheet", which(colnames(macrosheet) == "M_EIU_12m_change_norm"), which(colnames(macrosheet) == "M_EIU_12m_change_norm"))
# cond("macrosheet", which(colnames(macrosheet) == "M_EIU_Score_12m_norm"), which(colnames(macrosheet) == "M_EIU_Score_12m_norm"))
# cond("Naturalhazardsheet", which(colnames(Naturalhazardsheet) == "NH_GDAC_Hazard_Score_Norm"), which(colnames(Naturalhazardsheet) == "NH_GDAC_Hazard_Score_Norm"))
# cond("Naturalhazardsheet", which(colnames(Naturalhazardsheet) == "NH_Hazard_Score_norm"), which(colnames(Naturalhazardsheet) == "NH_Hazard_Score_norm"))
# cond("Naturalhazardsheet", which(colnames(Naturalhazardsheet) == "NH_multihazard_risk_norm"), which(colnames(Naturalhazardsheet) == "NH_multihazard_risk_norm"))
# cond("Naturalhazardsheet", which(colnames(Naturalhazardsheet) == "NH_seasonal_risk_norm"), which(colnames(Naturalhazardsheet) == "NH_seasonal_risk_norm"))
# cond("Naturalhazardsheet", which(colnames(Naturalhazardsheet) == "NH_locust_norm"), which(colnames(Naturalhazardsheet) == "NH_locust_norm"))
# cond("Socioeconomic_sheet", which(colnames(Socioeconomic_sheet) == "S_INFORM_vul_norm"), which(colnames(Socioeconomic_sheet) == "S_INFORM_vul_norm"))
# cond("Socioeconomic_sheet", which(colnames(Socioeconomic_sheet) == "S_pov_comb_norm"), which(colnames(Socioeconomic_sheet) == "S_income_support.Rating_crm_norm"))
# cond("Socioeconomic_sheet", which(colnames(Socioeconomic_sheet) == "S_change_unemp_20_norm"), which(colnames(Socioeconomic_sheet) == "S_change_unemp_20_norm"))
# cond("Socioeconomic_sheet", which(colnames(Socioeconomic_sheet) == "S_income_support.Rating_crm_norm"), which(colnames(Socioeconomic_sheet) == "S_income_support.Rating_crm_norm"))
# cond("Socioeconomic_sheet", which(colnames(Socioeconomic_sheet) == "S_Household.risks"), which(colnames(Socioeconomic_sheet) == "S_Household.risks"))
# cond("Socioeconomic_sheet", which(colnames(Socioeconomic_sheet) == "S_phone_average_index_norm"), which(colnames(Socioeconomic_sheet) == "S_phone_average_index_norm"))
# 
# # Conditional formatting colours
# posStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
# medStyle <- createStyle(fontColour = "#CC6600", bgFill = "#FFE5CC")
# negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
# naStyle <- createStyle(fontColour = "white", bgFill = "white")
# 
# # Conditional Cell Formatting
# conditionalFormatting(crxls, "Reliability_sheet", cols = 5:18, rows = 1:191, rule = "==1", style = negStyle)
# conditionalFormatting(crxls, "Reliability_sheet", cols = 5:18, rows = 1:191, type = "between", rule = c(0.700, 0.999), style = medStyle)
# conditionalFormatting(crxls, "Reliability_sheet", cols = 5:18, rows = 1:191, type = "between", rule = c(0, 0.6999), style = posStyle)
# conditionalFormatting(crxls, "Reliability_sheet", cols = 5:18, rows = 1:191, rule = '=""', style = naStyle)
# conditionalFormatting(crxls, "Alternativeflag_sheet", cols = c(4:6, 8:9, 11:19), rows = 1:191, type = "between", rule = c(7, 10), style = negStyle)
# conditionalFormatting(crxls, "Alternativeflag_sheet", cols = c(4:6, 8:9, 11:19), rows = 1:191, type = "between", rule = c(5, 6.9999), style = medStyle)
# conditionalFormatting(crxls, "Alternativeflag_sheet", cols = c(4:6, 8:9, 11:19), rows = 1:191, type = "between", rule = c(0, 4.9999), style = posStyle)
# conditionalFormatting(crxls, "Alternativeflag_sheet", cols = c(4:6, 8:9, 11:19), rows = 1:191, rule = '=""', style = naStyle)
# 
# # DatabarsconditionalFormatting
# conditionalFormatting(crxls, "riskflags", cols = 17:22, rows = 1:191, type = "databar", style = c("#C6EFCE", "#CD5C5C"))
# conditionalFormatting(crxls, "Reliability_sheet", cols = 2:4, rows = 1:191, type = "databar", style = c("#C6EFCE", "#CD5C5C"))
# conditionalFormatting(crxls, "Alternativeflag_sheet", cols = 21, rows = 1:191, type = "databar", style = c("#C6EFCE", "#CD5C5C"))
# 
# #----------------------------------Insert Global Maps---------------------------------------------------------------------
# # install.packages("librarian")     #Run if librarian is not already installed
# librarian::shelf(ggplot2, cowplot, lubridate, rvest, dplyr, viridis, tidyverse, countrycode)
# 
# # Loading world database
# world <- map_data("world")
# world <- world %>%
#   dplyr::rename(Country = region) %>%
#   dplyr::mutate(Country = suppressWarnings(countrycode(Country, origin = "country.name", destination = "iso3c")))
# 
# # Join datasets with risk flags
# worldmap <- inner_join(world, riskflags, by = "Country")
# 
# # Map theme
# plain <- theme(
#   axis.text = element_blank(),
#   axis.line = element_blank(),
#   axis.ticks = element_blank(),
#   panel.border = element_blank(),
#   panel.grid = element_blank(),
#   axis.title = element_blank(),
#   plot.title = element_text(hjust = 0.5),
#   panel.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
#   plot.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
#   legend.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
#   text = element_text(colour = "lightgrey")
# )
# 
# # Draw map one
# map <- ggplot(data = worldmap, mapping = aes(x = long, y = lat, group = group)) +
#   coord_fixed(1.3) +
#   geom_polygon(aes(fill = TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM)) +
#   scale_fill_distiller(palette = "Blues", direction = 1) + # or direction=1
#   ggtitle("Total Existing Compound Risk Score") +
#   plain +
#   labs(fill = "Total # of risks")
# 
# # Draw map two
# map2 <- ggplot(data = worldmap, mapping = aes(x = long, y = lat, group = group)) +
#   coord_fixed(1.3) +
#   geom_polygon(aes(fill = TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM)) +
#   scale_fill_distiller(palette = "Reds", direction = 1) + # or direction=1
#   ggtitle("Total Emerging Compound Risk Score") +
#   plain +
#   labs(fill = "Total # of risks")
# 
# # Join the maps and print to the system
# jointmap <- cowplot::plot_grid(map, map2, ncol = 1, align = c("hv"))
# print(jointmap)
# 
# # Insert plot into the worksheet
# insertPlot(crxls, 1, xy = c("AA", 5), width = 11.5, height = 9.5, fileType = "png", units = "in")
# 
# # Save plots
# ggsave("Plots/Snapshots/global_emerging_map.pdf", map2, width = 11.5, height = 9.5)
# ggsave("Plots/Snapshots/global_existing_map.pdf", map, width = 11.5, height = 9.5)
# 
# #----------------------------------------Save the final worksheet------------------------------------------------------
# saveWorkbook(crxls, file = "Risk_sheets/Compound_Risk_Monitor.xlsx", overwrite = TRUE)
