#----------------------------—WB PHONE SURVEYS-----------------------------------------------------
# If file exists locally, read locally; otherwise, use drive_download
# phone_data <- tryCatch(
#   {
#     read_excel("~/Google Drive/PhD/R code/Compound Risk/Restricted_Data/Phone_surveys_Mar.xlsx",
#                sheet = "2. Harmonized Indicators")
#   }, error = function(e) {
#     drive_download("Restricted_Data/Phone_surveys_Mar.xlsx", path = "tmp-Phone_surveys.xlsx", overwrite = T, verbose = F)
#     data <- read_excel("tmp-Phone_surveys.xlsx",
#                        sheet = "2. Harmonized Indicators")
#     unlink("tmp-Phone_surveys.xlsx")
#     return(data)
#   }
# )

phone_data <- read_excel("/Users/bennotkin/Downloads/Phone_surveys_June.xlsx",
                         sheet = "2. Harmonized Indicators")

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

phone_index_data <- normfuncpos(phone_index_data, 7, 0, "S_phone_average_index")