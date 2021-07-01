librarian::shelf(
  cowplot, lubridate, rvest, viridis, countrycode,
  clipr, awalker89, openxlsx, dplyr, readxl,
  gsheet, zoo, wppExplorer, haven, EnvStats, jsonlite, matrixStats,
  ggalt, raster, ggthemes, tidyverse,
  sjmisc, googledrive, rgdal
)

#--------------------FUNCTION TO CALCULATE NORMALISED SCORES-----------------
# Function to normalise with upper and lower bounds (when low score = high vulnerability)
normfuncneg <- function(df, upperrisk, lowerrisk, col1) {
  # Create new column col_name as sum of col1 and col2
  df[[paste0(col1, "_norm")]] <- ifelse(df[[col1]] <= upperrisk, 10,
                                        ifelse(df[[col1]] >= lowerrisk, 0,
                                               ifelse(df[[col1]] > upperrisk & df[[col1]] < lowerrisk, 10 - (upperrisk - df[[col1]]) / 
                                                        (upperrisk - lowerrisk) * 10, NA)
                                               )
                                        )
  df
}

# Function to normalise with upper and lower bounds (when high score = high vulnerability)
normfuncpos <- function(df, upperrisk, lowerrisk, col1) {
  # Create new column col_name as sum of col1 and col2
  df[[paste0(col1, "_norm")]] <- ifelse(df[[col1]] >= upperrisk, 10,
                                        ifelse(df[[col1]] <= lowerrisk, 0,
                                               ifelse(df[[col1]] < upperrisk & df[[col1]] > lowerrisk, 10 - (upperrisk - df[[col1]]) / 
                                                        (upperrisk - lowerrisk) * 10, NA)
                                               )
                                        )
  df
}

#--------------------------—MPO: Poverty projections----------------------------------------------------
# If file exists locally, read locally; otherwise, use drive_download
mpo <- tryCatch(
  {
    read_dta("~/Google Drive/PhD/R code/Compound Risk/global.dta")
  }, error = function(e) {
    drive_download("Restricted_Data/global.dta", path = "tmp-global.dta", overwrite = T, verbose = F)
    data <- read_dta("tmp-global.dta")
    unlink("tmp-global.dta")
    return(data)
  }
)

# Add population
pop <- wpp.by.year(wpp.indicator("tpop"), 2020)

pop$charcode <- suppressWarnings(countrycode(pop$charcode,
                                             origin = "iso2c",
                                             destination = "iso3c"
                                             )
                                 )

colnames(pop) <- c("Country", "Population")

mpo_data <- mpo %>%
  rename(Country = Code) %>%
  left_join(., pop, by= "Country")  %>%
  mutate_at(
    vars(contains("y20")),
    ~ as.numeric(as.character(.))
  ) %>%
  mutate(
    pov_prop_22_21 = y2022 - y2021,
    pov_prop_21_20 = y2021 - y2020,
    pov_prop_20_19 = y2020 - y2019,
    ) %>%
  filter(Label == "International poverty rate ($1.9 in 2011 PPP)") %>%
  rename_with(
    .fn = ~ paste0("S_", .),
    .cols = colnames(.)[!colnames(.) %in% c("Country")]
  ) 

# Normalise based on percentiles
mpo_data <- normfuncpos(mpo_data, 0.5, -1.5, "S_pov_prop_22_21")
mpo_data <- normfuncpos(mpo_data, 1, -1, "S_pov_prop_21_20")
mpo_data <- normfuncpos(mpo_data, 3, 0, "S_pov_prop_20_19")

mpo_data <- mpo_data %>%
  mutate(
    S_pov_comb_norm = rowMaxs(as.matrix(dplyr::select(.,
                                                      S_pov_prop_22_21_norm,
                                                      S_pov_prop_21_20_norm, 
                                                      S_pov_prop_20_19_norm))
                              , na.rm = T)
  ) %>% dplyr::select(Country,
                      S_pov_comb_norm, 
                      S_pov_prop_22_21_norm,
                      S_pov_prop_21_20_norm, 
                      S_pov_prop_20_19_norm,
                      S_pov_prop_22_21,
                      S_pov_prop_21_20, 
                      S_pov_prop_20_19)

write_csv(mpo_data, "Indicator_dataset/mpo.csv")

#----------------------------—WB PHONE SURVEYS-----------------------------------------------------
# If file exists locally, read locally; otherwise, use drive_download
phone_data <- tryCatch(
  {
    read_excel("~/Google Drive/PhD/R code/Compound Risk/Restricted_Data/Phone_surveys_Mar.xlsx",
               sheet = "2. Harmonized Indicators")
  }, error = function(e) {
    drive_download("Restricted_Data/Phone_surveys_Mar.xlsx", path = "tmp-Phone_surveys.xlsx", overwrite = T, verbose = F)
    data <- read_excel("tmp-Phone_surveys.xlsx",
                             sheet = "2. Harmonized Indicators")
    unlink("tmp-Phone_surveys.xlsx")
    return(data)
  }
)

phone_compile <- phone_data %>%
  filter(level_data == "Gender=All, Urb_rur=National. sector=All") %>%
  mutate(survey_no = as.numeric(as.character(str_replace(wave, "WAVE", "")))) %>%
  group_by(code) %>%
  mutate(last_survey = max(survey_no, na.rm=T)) %>%
  ungroup() %>%
  filter(last_survey == survey_no) 
  
phone_data <- phone_compile %>%
  dplyr::select(code, indicator_description, indicator_val) %>%
  pivot_wider(names_from = indicator_description, values_from = indicator_val  )

phone_index <-phone_data %>%
  dplyr::select(
    "code", "% of respondents currently employed/working",  "% of respondents who have stopped working since COVID-19 outbreak", 
    "% able to access [staple food item] in the past 7 days when needed? - any staple food" ,
    "% of HHs that saw reduced their remittances" , "% of HHs not able to perform normal farming activities (crop, livestock, fishing)" ,
    "% of HHs able to pay rent for the next month",
    "% of respondents who were not able to work as usual last week","Experienced decrease in wage income (% HHs with wage income as a source of livelihood in the past 12 months)",
    "% of HHs that experienced change in total income - decrease"  ,"% of HHs used money saved for emergencies to cover basic living expenses" ,
    "% of respondents received government assistance when experiencing labor income/job loss" ,   
    "% of HHs sold assets such as property during the pandemic in order to pay for basic living expenses" ,
    "In the last 30 days, you skipped a meal because there was not enough money or other resources for food?(%)"   ,
    "In the last 30 days, your household worried about running out of food because of a lack of money or other resources?(%)" ,
  )

# Normalised values
#phone_index <- normfuncpos(phone_index, 70, 0, "% of respondents currently employed/working")
  phone_index <- normfuncpos(phone_index, 50, 0, "% of respondents who have stopped working since COVID-19 outbreak" )
phone_index <- normfuncneg(phone_index, 80, 100, "% able to access [staple food item] in the past 7 days when needed? - any staple food" )
#phone_index <- normfuncpos(phone_index, 70, 0, "% of HHs that saw reduced their remittances" )
#phone_index <- normfuncpos(phone_index, 25, 0, "% of HHs not able to perform normal farming activities (crop, livestock, fishing)")
#phone_index <- normfuncneg(phone_index, 50, 100, "% of HHs able to pay rent for the next month")
#phone_index <- normfuncpos(phone_index, 25, 0,  "% of respondents who were not able to work as usual last week")
#phone_index <- normfuncpos(phone_index, 50, 0,  "Experienced decrease in wage income (% HHs with wage income as a source of livelihood in the past 12 months)")
#phone_index <- normfuncpos(phone_index, 50, 0,  "% of HHs that experienced change in total income - decrease")
phone_index <- normfuncpos(phone_index, 25, 0,  "% of HHs used money saved for emergencies to cover basic living expenses" )
phone_index <- normfuncneg(phone_index, 5, 80,  "% of respondents received government assistance when experiencing labor income/job loss")
#phone_index <- normfuncpos(phone_index, 20, 0,  "% of HHs sold assets such as property during the pandemic in order to pay for basic living expenses"  )
phone_index <- normfuncpos(phone_index, 50, 0,  "In the last 30 days, you skipped a meal because there was not enough money or other resources for food?(%)"  )
#phone_index <- normfuncpos(phone_index, 50, 0,  "In the last 30 days, your household worried about running out of food because of a lack of money or other resources?(%)")
           
# Calculate index
phone_index_data <- phone_index %>%
  mutate(
    phone_average_index = dplyr::select(., contains("_norm")) %>% rowMeans(na.rm=T)  ) %>%
  rename(Country = code) %>%
  rename_with(
    .fn = ~ paste0("S_", .), 
    .cols = -contains("Country")
  )

phone_index_data <- normfuncpos(phone_index_data, 7, 0, "S_phone_average_index") %>%
  dplyr::select(Country, S_phone_average_index, S_phone_average_index_norm)
write_csv(phone_index_data, "Indicator_dataset/phone.csv")
