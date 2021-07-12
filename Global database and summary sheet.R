######################################################################################################
#
#  CODE USED TO CREATE THE GLOBAL DATABASET ON COMPOUND DISASTER RISK
#  (to be run after risk component sheets have been generated)
#
######################################################################################################

# LOAD PACKAGES ----
packages <- c("curl", "dplyr", "EnvStats", "stats", "countrycode", "ggplot2", 
              "jsonlite","lubridate", "matrixStats", "readr", "readxl",   
              "rmarkdown", "rvest", "sjmisc", "stringr", "tidyr", "xml2", "zoo")
invisible(lapply(packages, require, character.only = TRUE))
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
# countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")[,-1]

# Join datasets
# — `globalrisk` ----
globalrisk <- left_join(countrylist, healthsheet, by = c("Countryname", "Country")) %>%
  left_join(., foodsecurity, by = c("Countryname", "Country")) %>%
  # left_join(., debtsheet, by = c("Countryname", "Country")) %>%
  left_join(., fragilitysheet, by = c("Country")) %>%
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

# Add underlying and emerging risk scores
#—`riskflags` ----
riskflags <- globalrisk %>%
  mutate(
    UNDERLYING_RISK_HEALTH = pmax(
      H_HIS_Score_norm,
      H_INFORM_rating.Value_norm
    ),
    UNDERLYING_RISK_FOOD_SECURITY = F_Proteus_Score_norm,
    UNDERLYING_RISK_MACRO_FISCAL = M_EIU_Score_12m_norm,
    UNDERLYING_RISK_SOCIOECONOMIC_VULNERABILITY = S_INFORM_vul_norm,
    UNDERLYING_RISK_NATURAL_HAZARDS = pmax(
      NH_Hazard_Score_norm,
      na.rm=T
    ),
    UNDERLYING_RISK_FRAGILITY_INSTITUTIONS = Fr_FCS_Normalised,
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
    Countryname, Country, UNDERLYING_RISK_HEALTH, UNDERLYING_RISK_FOOD_SECURITY,
    UNDERLYING_RISK_MACRO_FISCAL, UNDERLYING_RISK_SOCIOECONOMIC_VULNERABILITY,
    UNDERLYING_RISK_NATURAL_HAZARDS, UNDERLYING_RISK_FRAGILITY_INSTITUTIONS,
    EMERGING_RISK_HEALTH, EMERGING_RISK_FOOD_SECURITY, EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY, EMERGING_RISK_MACRO_FISCAL,
    EMERGING_RISK_NATURAL_HAZARDS, EMERGING_RISK_FRAGILITY_INSTITUTIONS #, F_fews_crm_norm
  )

# —Create ternary risk flags----
vars <- c(
  "UNDERLYING_RISK_HEALTH", "UNDERLYING_RISK_FOOD_SECURITY",
  "UNDERLYING_RISK_MACRO_FISCAL", "UNDERLYING_RISK_SOCIOECONOMIC_VULNERABILITY",
  "UNDERLYING_RISK_NATURAL_HAZARDS", "UNDERLYING_RISK_FRAGILITY_INSTITUTIONS",
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
riskflags$UNDERLYING_FLAGS <- as.numeric(unlist(row_count(
  riskflags,
  UNDERLYING_RISK_HEALTH:UNDERLYING_RISK_FRAGILITY_INSTITUTIONS,
  count = 10,
  append = F
)))

riskflags$EMERGING_FLAGS <- as.numeric(unlist(row_count(
  riskflags, 
  EMERGING_RISK_HEALTH:EMERGING_RISK_FRAGILITY_INSTITUTIONS,
  count = 10,
  append = F
)))

riskflags$medium_risk_underlying <- as.numeric(unlist(row_count(riskflags,
                                                                UNDERLYING_RISK_HEALTH_RISKLEVEL:UNDERLYING_RISK_FRAGILITY_INSTITUTIONS_RISKLEVEL,
                                                                count = "Medium risk",
                                                                append = F
)))

riskflags$medium_risk_emerging <- as.numeric(unlist(row_count(riskflags,
                                                              EMERGING_RISK_HEALTH_RISKLEVEL:EMERGING_RISK_FRAGILITY_INSTITUTIONS_RISKLEVEL,
                                                              count = "Medium risk",
                                                              append = F
)))

riskflags$UNDERLYING_FLAGS_MED <- as.numeric(unlist(riskflags$UNDERLYING_FLAGS + (riskflags$medium_risk_underlying / 2)))
riskflags$EMERGING_FLAGS_MED <- as.numeric(unlist(riskflags$EMERGING_FLAGS + (riskflags$medium_risk_emerging / 2)))

# Drop ternary rates (may want to reinstate in the future)
riskflags <- riskflags %>%
  dplyr::select(
    -medium_risk_emerging, -medium_risk_underlying, -all_of(paste0(vars, "_RISKLEVEL"))
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
  "EMERGING_RISK_MACRO_FISCAL", "UNDERLYING_RISK_FRAGILITY_INSTITUTIONS"
)

riskflags[paste0(names, "_plus1")] <- lapply(riskflags[names], function(xx) {
  ifelse(xx == 0, xx + 1, xx)
})

# Geometric means
riskflags <- riskflags %>%
  rowwise() %>%
  mutate(
    EMERGING_RISK_FRAGILITY_INSTITUTIONS_MULTIDIMENSIONAL = geoMean(c(
      EMERGING_RISK_FRAGILITY_INSTITUTIONS_plus1,
      EMERGING_RISK_MACRO_FISCAL_plus1),
      na.rm = T
    ),
    EMERGING_RISK_FRAGILITY_INSTITUTIONS_MULTIDIMENSIONAL_SQ = geoMean(c(
      UNDERLYING_RISK_FRAGILITY_INSTITUTIONS_plus1,
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
    EMERGING_RISK_HEALTH_AV = geoMean(c(
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
    EMERGING_RISK_FRAGILITY_INSTITUTIONS_AV = geoMean(c(
      Fr_REIGN_Normalised,
      Fr_Displaced_UNHCR_Normalised,
      Fr_BRD_Normalised,
      na.rm = T
    ))
    ,
    EMERGING_RISK_HEALTH_SQ_ALT = geoMean(c(
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
      F_fews_crm_norm,
      F_fao_wfp_warning,
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
# Geometric mean of underlying and emerging (and emerging when underlying is NA)
# This is how we are now calculating overall alert, though here it's called emerging risk sq

riskflags <- riskflags %>%
  mutate(
    OVERALL_HEALTH_GEO = case_when(
      !is.na(UNDERLYING_RISK_HEALTH) ~ sqrt(UNDERLYING_RISK_HEALTH * EMERGING_RISK_HEALTH),
      TRUE ~ EMERGING_RISK_HEALTH
    ),
    # OVERALL_HEALTH_GEO_ALT = case_when(
    #   !is.na(UNDERLYING_RISK_HEALTH) ~ sqrt(UNDERLYING_RISK_HEALTH * EMERGING_RISK_HEALTH_SQ_ALT),
    #   TRUE ~ EMERGING_RISK_HEALTH
    # ),
    OVERALL_FOOD_SECURITY_GEO = case_when(
      !is.na(UNDERLYING_RISK_FOOD_SECURITY) ~ sqrt(UNDERLYING_RISK_FOOD_SECURITY * EMERGING_RISK_FOOD_SECURITY),
      TRUE ~ EMERGING_RISK_FOOD_SECURITY
    ),
    OVERALL_MACRO_FISCAL_GEO = case_when(
      !is.na(UNDERLYING_RISK_MACRO_FISCAL) ~ sqrt(UNDERLYING_RISK_MACRO_FISCAL * EMERGING_RISK_MACRO_FISCAL),
      TRUE ~ EMERGING_RISK_MACRO_FISCAL
    ),
    OVERALL_SOCIOECONOMIC_VULNERABILITY_GEO = case_when(
      !is.na(UNDERLYING_RISK_SOCIOECONOMIC_VULNERABILITY) ~ sqrt(UNDERLYING_RISK_SOCIOECONOMIC_VULNERABILITY * EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY),
      TRUE ~ as.numeric(EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY)
    ),
    OVERALL_NATURAL_HAZARDS_GEO = case_when(
      !is.na(UNDERLYING_RISK_NATURAL_HAZARDS) ~ sqrt(UNDERLYING_RISK_NATURAL_HAZARDS * EMERGING_RISK_NATURAL_HAZARDS),
      TRUE ~ EMERGING_RISK_NATURAL_HAZARDS
    ),
    OVERALL_FRAGILITY_INSTITUTIONS_GEO = case_when(
      !is.na(UNDERLYING_RISK_FRAGILITY_INSTITUTIONS) ~ sqrt(UNDERLYING_RISK_FRAGILITY_INSTITUTIONS * EMERGING_RISK_FRAGILITY_INSTITUTIONS),
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
      !is.na(UNDERLYING_RISK_HEALTH) ~ (UNDERLYING_RISK_HEALTH + EMERGING_RISK_HEALTH)/2,
      TRUE ~ EMERGING_RISK_HEALTH
    ),
    OVERALL_FOOD_SECURITY_AV = case_when(
      !is.na(UNDERLYING_RISK_FOOD_SECURITY) ~ (UNDERLYING_RISK_FOOD_SECURITY + EMERGING_RISK_FOOD_SECURITY)/2,
      TRUE ~ EMERGING_RISK_FOOD_SECURITY
    ),
    OVERALL_MACRO_FISCAL_AV = case_when(
      !is.na(UNDERLYING_RISK_MACRO_FISCAL) ~ (UNDERLYING_RISK_MACRO_FISCAL + EMERGING_RISK_MACRO_FISCAL)/2,
      TRUE ~ EMERGING_RISK_MACRO_FISCAL
    ),
    OVERALL_SOCIOECONOMIC_VULNERABILITY_AV = case_when(
      !is.na(UNDERLYING_RISK_SOCIOECONOMIC_VULNERABILITY) ~ (UNDERLYING_RISK_SOCIOECONOMIC_VULNERABILITY + EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY)/2,
      TRUE ~ as.numeric(EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY)
    ),
    OVERALL_NATURAL_HAZARDS_AV = case_when(
      !is.na(UNDERLYING_RISK_NATURAL_HAZARDS) ~ (UNDERLYING_RISK_NATURAL_HAZARDS + EMERGING_RISK_NATURAL_HAZARDS)/2,
      TRUE ~ EMERGING_RISK_NATURAL_HAZARDS
    ),
    OVERALL_FRAGILITY_INSTITUTIONS_AV = case_when(
      !is.na(UNDERLYING_RISK_FRAGILITY_INSTITUTIONS) ~ (UNDERLYING_RISK_FRAGILITY_INSTITUTIONS + EMERGING_RISK_FRAGILITY_INSTITUTIONS)/2,
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
riskflags$OVERALL_FLAGS_AV <- rowSums(riskflags[arithnam] >= 7, na.rm = T) 

# Emerging risk score as high + med
riskflags$OVERALL_FLAGS_AV_MED <-  rowSums(riskflags[arithnam] >= 7, na.rm = T) + (rowSums(riskflags[arithnam] < 7 & riskflags[arithnam] >= 5, na.rm = T) / 2)


#-----------------------------—Calculate Overall Alert w/ Filter: Emerging & Underlying == 10 ----------------------------------------------
riskflags <- riskflags %>%
  mutate(
    OVERALL_HEALTH_FILTER_10 = case_when(
      UNDERLYING_RISK_HEALTH == 10 & EMERGING_RISK_HEALTH == 10 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_FOOD_SECURITY_FILTER_10 = case_when(
      UNDERLYING_RISK_FOOD_SECURITY == 10 & EMERGING_RISK_FOOD_SECURITY == 10 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_MACRO_FISCAL_FILTER_10 = case_when(
      UNDERLYING_RISK_MACRO_FISCAL == 10 & EMERGING_RISK_MACRO_FISCAL == 10 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_SOCIOECONOMIC_VULNERABILITY_FILTER_10 = case_when(
      UNDERLYING_RISK_SOCIOECONOMIC_VULNERABILITY == 10 & EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY == 10 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_NATURAL_HAZARDS_FILTER_10 = case_when(
      UNDERLYING_RISK_NATURAL_HAZARDS == 10 & EMERGING_RISK_NATURAL_HAZARDS == 10 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_FRAGILITY_INSTITUTIONS_FILTER_10 = case_when(
      UNDERLYING_RISK_FRAGILITY_INSTITUTIONS == 10 & EMERGING_RISK_FRAGILITY_INSTITUTIONS == 10 ~ 10,
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
riskflags$OVERALL_FLAGS_FILTER_10 <- rowSums(riskflags[filter10nam] >= 7, na.rm = T) 

# Emerging risk score as high + med
riskflags$OVERALL_FLAGS_FILTER_10_MED <-  rowSums(riskflags[filter10nam] >= 7, na.rm = T) + (rowSums(riskflags[filter10nam] < 7 & riskflags[filter10nam] >= 5, na.rm = T) / 2)

#-----------------------------—Calculate Overall Alert w/ Filter: Emerging & Underlying >= 7 ----------------------------------------------
riskflags <- riskflags %>%
  mutate(
    OVERALL_HEALTH_FILTER_7 = case_when(
      UNDERLYING_RISK_HEALTH >= 7 & EMERGING_RISK_HEALTH >= 7 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_FOOD_SECURITY_FILTER_7 = case_when(
      UNDERLYING_RISK_FOOD_SECURITY >= 7 & EMERGING_RISK_FOOD_SECURITY >= 7 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_MACRO_FISCAL_FILTER_7 = case_when(
      UNDERLYING_RISK_MACRO_FISCAL >= 7 & EMERGING_RISK_MACRO_FISCAL >= 7 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_SOCIOECONOMIC_VULNERABILITY_FILTER_7 = case_when(
      UNDERLYING_RISK_SOCIOECONOMIC_VULNERABILITY >= 7 & EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY >= 7 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_NATURAL_HAZARDS_FILTER_7 = case_when(
      UNDERLYING_RISK_NATURAL_HAZARDS >= 7 & EMERGING_RISK_NATURAL_HAZARDS >= 7 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_FRAGILITY_INSTITUTIONS_FILTER_7 = case_when(
      UNDERLYING_RISK_FRAGILITY_INSTITUTIONS >= 7 & EMERGING_RISK_FRAGILITY_INSTITUTIONS >= 7 ~ 10,
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
riskflags$OVERALL_FLAGS_FILTER_7 <- rowSums(riskflags[filter7nam] >= 7, na.rm = T) 

# Emerging risk score as high + med
riskflags$OVERALL_FLAGS_FILTER_7_MED <-  rowSums(riskflags[filter7nam] >= 7, na.rm = T) + (rowSums(riskflags[filter7nam] < 7 & riskflags[filter7nam] >= 5, na.rm = T) / 2)

#-----------------------------—Calculate Overall Alert w/ Filter: Emerging & Underlying >= 7, Medium if one is below 7 but >= 5 ----------------------------------------------
riskflags <- riskflags %>%
  mutate(
    OVERALL_HEALTH_FILTER_7_5 = case_when(
      UNDERLYING_RISK_HEALTH >= 7 & EMERGING_RISK_HEALTH >= 7 ~ 10,
      UNDERLYING_RISK_HEALTH >= 7 & EMERGING_RISK_HEALTH >= 5 ~ 5,
      UNDERLYING_RISK_HEALTH >= 5 & EMERGING_RISK_HEALTH >= 7 ~ 5,
      TRUE ~ 0
    ),
    OVERALL_FOOD_SECURITY_FILTER_7_5 = case_when(
      UNDERLYING_RISK_FOOD_SECURITY >= 7 & EMERGING_RISK_FOOD_SECURITY >= 7 ~ 10,
      UNDERLYING_RISK_FOOD_SECURITY >= 7 & EMERGING_RISK_FOOD_SECURITY >= 7 ~ 10,
      UNDERLYING_RISK_FOOD_SECURITY >= 7 & EMERGING_RISK_FOOD_SECURITY >= 7 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_MACRO_FISCAL_FILTER_7_5 = case_when(
      UNDERLYING_RISK_MACRO_FISCAL >= 7 & EMERGING_RISK_MACRO_FISCAL >= 7 ~ 10,
      UNDERLYING_RISK_MACRO_FISCAL >= 7 & EMERGING_RISK_MACRO_FISCAL >= 7 ~ 10,
      UNDERLYING_RISK_MACRO_FISCAL >= 7 & EMERGING_RISK_MACRO_FISCAL >= 7 ~ 10,
      TRUE ~ 0
    ),
    OVERALL_SOCIOECONOMIC_VULNERABILITY_FILTER_7_5 = case_when(
      UNDERLYING_RISK_SOCIOECONOMIC_VULNERABILITY >= 7 & EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY >= 7 ~ 10,
      UNDERLYING_RISK_SOCIOECONOMIC_VULNERABILITY >= 7 & EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY >= 5 ~ 5,
      UNDERLYING_RISK_SOCIOECONOMIC_VULNERABILITY >= 5 & EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY >= 7 ~ 5,
      TRUE ~ 0
    ),
    OVERALL_NATURAL_HAZARDS_FILTER_7_5 = case_when(
      UNDERLYING_RISK_NATURAL_HAZARDS >= 7 & EMERGING_RISK_NATURAL_HAZARDS >= 7 ~ 10,
      UNDERLYING_RISK_NATURAL_HAZARDS >= 7 & EMERGING_RISK_NATURAL_HAZARDS >= 5 ~ 5,
      UNDERLYING_RISK_NATURAL_HAZARDS >= 5 & EMERGING_RISK_NATURAL_HAZARDS >= 7 ~ 5,
      TRUE ~ 0
    ),
    OVERALL_FRAGILITY_INSTITUTIONS_FILTER_7_5 = case_when(
      UNDERLYING_RISK_FRAGILITY_INSTITUTIONS >= 7 & EMERGING_RISK_FRAGILITY_INSTITUTIONS >= 7 ~ 10,
      UNDERLYING_RISK_FRAGILITY_INSTITUTIONS >= 7 & EMERGING_RISK_FRAGILITY_INSTITUTIONS >= 5 ~ 5,
      UNDERLYING_RISK_FRAGILITY_INSTITUTIONS >= 5 & EMERGING_RISK_FRAGILITY_INSTITUTIONS >= 7 ~ 5,
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
riskflags$OVERALL_FLAGS_FILTER_7_5 <- rowSums(riskflags[filter75nam] >= 7, na.rm = T) 

# Emerging risk score as high + med
riskflags$OVERALL_FLAGS_FILTER_7_5_MED <-  rowSums(riskflags[filter75nam] >= 7, na.rm = T) + (rowSums(riskflags[filter75nam] < 7 & riskflags[filter75nam] >= 5, na.rm = T) / 2)

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
    RELIABILITY_UNDERLYING_HEALTH = rowSums(is.na(globalrisk %>%
                                                    dplyr::select(H_HIS_Score_norm, H_INFORM_rating.Value_norm)),
                                            na.rm = T
    ) / 2,
    RELIABILITY_UNDERLYING_FOOD_SECURITY = case_when(
      is.na(F_Proteus_Score_norm) ~ 1,
      TRUE ~ 0
    ),
    RELIABILITY_UNDERLYING_MACRO_FISCAL = rowSums(is.na(globalrisk %>%
                                                          dplyr::select(
                                                            M_EIU_Score_12m_norm 
                                                          )),
                                                  na.rm = T
    ) / 4,
    RELIABILITY_UNDERLYING_SOCIOECONOMIC_VULNERABILITY = case_when(
      is.na(S_INFORM_vul_norm) ~ 1,
      TRUE ~ 0
    ),
    RELIABILITY_UNDERLYING_NATURAL_HAZARDS = case_when(
      is.na(NH_Hazard_Score_norm) ~ 1,
      TRUE ~ 0
    ),
    RELIABILITY_UNDERLYING_FRAGILITY_INSTITUTIONS = case_when(
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
    RELIABILITY_SCORE_UNDERLYING_RISK = round(rowMeans(dplyr::select(., starts_with("RELIABILITY_UNDERLYING"))), 1),
    RELIABILITY_SCORE_EMERGING_RISK = round(rowMeans(dplyr::select(., starts_with("RELIABILITY_EMERGING"))), 1)
  ) %>%
  dplyr::select(
    Countryname, Country, RELIABILITY_SCORE_UNDERLYING_RISK, RELIABILITY_SCORE_EMERGING_RISK, RELIABILITY_UNDERLYING_HEALTH, RELIABILITY_UNDERLYING_FOOD_SECURITY,
    RELIABILITY_UNDERLYING_MACRO_FISCAL, RELIABILITY_UNDERLYING_SOCIOECONOMIC_VULNERABILITY,
    RELIABILITY_UNDERLYING_NATURAL_HAZARDS, RELIABILITY_UNDERLYING_FRAGILITY_INSTITUTIONS,
    RELIABILITY_EMERGING_HEALTH, RELIABILITY_EMERGING_FOOD_SECURITY,
    EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY, RELIABILITY_EMERGING_MACRO_FISCAL,
    RELIABILITY_EMERGING_NATURAL_HAZARDS, RELIABILITY_EMERGING_FRAGILITY_INSTITUTIONS
  ) %>%
  arrange(Country)

# Write as a csv file for the reliability sheet
write.csv(reliabilitysheet, "Risk_sheets/reliabilitysheet.csv")
write.csv(reliabilitysheet, "external/risk-sheets/reliabilitysheet.csv")
write.csv(reliabilitysheet, paste0("external/risk-sheets/archive/", Sys.Date(), "-reliabilitysheet.csv"))

#------------------------------—Combine the reliability sheet with the global database------------------------------------
reliable <- reliabilitysheet %>%
  dplyr::select(Countryname, Country, RELIABILITY_SCORE_UNDERLYING_RISK, RELIABILITY_SCORE_EMERGING_RISK)

globalrisk <- left_join(globalrisk, reliable, by = c("Countryname", "Country"))

# Save database of all risk indicators (+ reliability scores)
write.csv(globalrisk, "Risk_sheets/Global_compound_risk_database.csv")
write.csv(globalrisk, "external/risk-sheets/Global_compound_risk_database.csv")
write.csv(globalrisk, paste0("external/risk-sheets/archive/", Sys.Date(),"-Global_compound_risk_database.csv"))

#------------------------------—Combine the reliability sheet with the summary risk flag sheet-----------------------------
reliable <- reliabilitysheet %>%
  dplyr::select(Countryname, Country, RELIABILITY_SCORE_UNDERLYING_RISK, RELIABILITY_SCORE_EMERGING_RISK)

riskflags <- left_join(riskflags %>%
                         dplyr::select(
                           "Countryname", "Country",
                           contains(c("_AV", "_SQ", "_ALT", "_FILTER", "_GEO", "UNDERLYING_", "EMERGING_", "coefvar"))
                         ),
                       reliable, 
                       by = c("Countryname", "Country")) %>% 
  dplyr::select(-contains("S_phone"))

# Write csv file of all risk flags (+reliability scores)

write.csv(riskflags, "Risk_sheets/Compound_Risk_Flag_Sheets.csv")
write.csv(riskflags, "external/risk-sheets/Compound_Risk_Flag_Sheets.csv")
write.csv(riskflags, paste0("external/risk-sheets/archive/", Sys.Date(), "-Compound_Risk_Flag_Sheets.csv"))

#
##
### ********************************************************************************************
####    LONG AND MINIMAL OUTPUT FILE ----
### ********************************************************************************************
##
#

# —Only use the relevant aggregated metrics; renamed for dashboard ----
names <- read_csv("riskflags-dashboard-names.csv")
riskflags_select <- riskflags[,c("Country", "Countryname", names$old_name)]
names(riskflags_select) <- c("Country", "Countryname", names$new_name)
write_csv(riskflags_select, "Risk_sheets/crm-aggregated.csv")
}

# —Lengthen data ----
flags_long <- pivot_longer(riskflags_select, !c("Country", "Countryname"), values_to = "Value") %>%
  separate(name, into = c("Outlook", "Key"), sep = "_" ) %>%
  mutate(
    Dimension = case_when(
      grepl("Flag", Key) ~ "Flag",
      TRUE ~ Key),
    Key = case_when(
      Dimension == "Flag" ~ "Number of Flags",
      TRUE ~ Key),
    `Data Level` = case_when(
      Dimension == "Flag" ~ "Flag Count",
      TRUE ~ "Dimension Value"
    ),
    `Display Status` = "Main"
    )

indicators_select <- read_csv("crm-indicators.csv") # written in `indicators.R` 

indicators_long <- pivot_longer(indicators_select, !c("Country", "Countryname"), values_to = "Value") %>% 
  separate(name, into = c("Outlook", "Dimension", "Key"), sep = "_" ) %>%
  mutate(`Data Level` = "Indicator",
         `Display Status` = "Secondary")

long <- rbind(indicators_long, flags_long) %>%
  mutate(
    `Overall Contribution` = case_when(
      Outlook == "Overall" ~ T,
      `Data Level` == "Indicator" ~ T,
      TRUE ~ F),
    `Risk Label` = case_when(
      `Data Level` == 'Dimension Value' & (Outlook == 'Emerging' | Outlook == 'Underlying') & Value >= 10 ~ "High",
      `Data Level` == 'Dimension Value' & (Outlook == 'Emerging' | Outlook == 'Underlying') & Value >= 7 ~ "Medium",
      `Data Level` == 'Dimension Value' & (Outlook == 'Emerging' | Outlook == 'Underlying') & Value >= 0 ~ "Low",
      `Data Level` == 'Dimension Value' & (Outlook == 'Overall') & Value >= 7 ~ "High",
      `Data Level` == 'Dimension Value' & (Outlook == 'Overall') & Value >= 5 ~ "Medium",
      `Data Level` == 'Dimension Value' & (Outlook == 'Overall') & Value >= 0 ~ "Low",
      `Data Level` == 'Flag Count' ~ as.character(floor(Value)),
      `Data Level` == 'Indicator' ~ as.character(floor(Value))
    ),
    Value = case_when(
      `Data Level` == "Flag Count" ~ (Value),
      TRUE ~ floor(Value)
    ),
    Value_Char = case_when(
      `Data Level` == "Flag Count" ~ as.character(Value),
      TRUE ~ as.character(floor(Value))
    )
    ) %>% mutate(
    Outlook = factor(Outlook, levels = c("Overall", "Underlying", "Emerging")),
    Dimension = factor(Dimension, c("Flag", "Food Security", "Conflict and Fragility", "Health", "Macro Fiscal",  "Natural Hazard", "Socioeconomic Vulnerability")),
    Country = factor(Country, levels = sort(unique(Country))),
    Countryname = factor(Countryname, levels = sort(unique(Countryname))),
    `Data Level` = factor(`Data Level`, c("Flag Count", "Dimension Value", "Indicator")),  
    `Display Status` = factor(`Display Status`)
  ) %>%
  relocate(Value_Char, `Risk Label`, .after = Value) %>%
  relocate(`Data Level`, .before = Outlook) %>%
  arrange(Country, Outlook, Dimension, `Data Level`) %>%
  subset(Key != "Indicator Reliability") %>%
  mutate(Index = row_number(), .before = 1)

# Compare most recent update to previous
# Not ideal, because running multiple times in a day obscures changes from first run through in a day
prev <- read_csv("external/crm-output-latest.csv")

indicatorChanges <- function(indicator) {
  long_ind <- subset(long, Key == indicator)
  prev_ind <- subset(prev, Key == indicator)
  
  # Make sure Countries line up in the two columns
  wrong_rows <- which(subset(long_ind)[, 'Country'] != subset(prev_ind)[, 'Country'])
  if (sum(wrong_rows) > 0) {
    warning("Rows are not aligned. See following rows in the newer dataframe: ", long$Index[wrong_rows,])
  }
  
  changes <- which(subset(long_ind)[, 'Value'] != subset(prev_ind)[, 'Value'])
  
  return(length(changes))
}

indicators <- as.data.frame(read.csv("indicators-list.csv"))

updateLog <- data.frame(Indicator = indicators$Indicator, Changed_Countries = sapply(indicators$Indicator, indicatorChanges)) %>%
  mutate(
    Update_Date = case_when(
      Changed_Countries > 0 ~ Sys.Date()
      ))
updateLogPrevious <- read_csv("external/updates-log.csv")
updateLogCombined <- rbind(updateLog, updateLogPrevious) %>%
  .[!is.na(.$Update_Date),]
write_csv(updateLogCombined, "external/updates-log.csv")

# Write long output file (crm-output-latest.csv) ----
# outputPrevious <- read_csv("external/crm-output-latest.csv")
write_csv(long, "external/crm-output-latest.csv")

outputAll <- read_csv("external/crm-output-all.csv")

long <- long %>%
  mutate(
    Date = Sys.Date(),
    Record_ID = if(!is.null(outputAll$Record_ID)) {
      max(outputAll$Record_ID) + long$Index
    } else {Index}
    ) %>%
  relocate(Record_ID, .before = 1)

outputAll <- rbind(outputAll, long)

write_csv(outputAll, "external/crm-output-all.csv")

# Country changes ----
# List countries that changed 
flagChanges  <- merge(subset(long, Dimension == "Flag")[,c("Index", "Countryname", "Country", "Outlook", "Value")],
      subset(prev, Dimension == "Flag")[,c("Index", "Value")],
      by = "Index") %>%
       rename(Count = Value.x,
              `Previous Count` = Value.y) %>%
              # Risk = `Risk Label.x`,
              # `Previous Risk` = `Risk Label.y`) %>%
  mutate(`Flag Change` = Count - `Previous Count`
         # `Risk Change` = as.numeric(Risk) - as.numeric(`Previous Risk`)
         )

rmarkdown::render('output-report.Rmd', output_format = c("html_document", "pdf_document"), output_file = paste0(Sys.Date(), "-report") %>% rep(2))