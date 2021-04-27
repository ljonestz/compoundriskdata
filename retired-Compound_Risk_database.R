######################################################################################################
#
#  CODE USED TO TO PRODUCE INDIVIDUAL INDICATOR DATASETS AND RISK COMPONENT SHEETS
#
######################################################################################################

#--------------------LOAD PACKAGES-----------------------------------------
# install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(
  cowplot, lubridate, rvest, viridis, countrycode,
  clipr, awalker89 / openxlsx, dplyr, readxl,
  gsheet, zoo, wppExplorer, haven, EnvStats, jsonlite, matrixStats,
  ggalt, raster, sf, mapview, maptools, ggthemes, tidyverse,
  sjmisc, googledrive, rgdal
)
{
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

#
##
### ********************************************************************************************
####    CREATE ACAPS SHEET USING A RANGE OF SOURCE INDICATORS ----
### ********************************************************************************************
##
#

#--------------------—LOAD ACAPS realtime database-------------------------------------------
# Load website
acaps <- read_html("https://www.acaps.org/countries")

# Select relevant columns from the site all merged into a single string
country <- acaps %>%
  html_nodes(".severity__country__label, .severity__country__crisis__label, .severity__country__crisis__value") %>%
  html_text()

# Find country labels in the string (select 2nd lag behind a numberic variable if the given item is a character)
countryisolate <- suppressWarnings(ifelse(!is.na(as.numeric(as.character(country))) & lag(is.na(as.numeric(as.character(country))), 2),
                                          lag(country, 2),
                                          NA
))

# For all variables that have a country label, change to iso categories
country[which(!is.na(countryisolate)) - 2] <- countrycode(country[which(!is.na(countryisolate)) - 2],
                                                          origin = "country.name",
                                                          destination = "iso3c",
                                                          nomatch = NULL
)

# Collect list of all world countries
world <- map_data("world")
world <- world %>%
  dplyr::rename(Country = region) %>%
  dplyr::mutate(Country = suppressWarnings(countrycode(Country,
                                                       origin = "country.name",
                                                       destination = "iso3c",
                                                       nomatch = NULL
  )))

countrynam <- levels(as.factor(world$Country))

# Find all countries in the list and replace with correct country name (then fill in remaining NAs)
gap <- ifelse(country %in% countrynam, country, NA)
gaplist <- na.locf(gap)

# Create new dataframe with correct countrynames
acapslist <- cbind.data.frame(country, gaplist)
acapslist <- acapslist[!acapslist$country %in% countrynam, ]
acapslist <- acapslist %>%
  filter(country != "Countrylevel")

# Create new column with the risk scores (and duplicate for missing rows up until the correct value)
acapslist$risk <- suppressWarnings(ifelse(!is.na(as.numeric(as.character(acapslist$country))), as.numeric(as.character(acapslist$country)), NA))
acapslist$risk <- c(na.locf(acapslist$risk), NA)

# Remove duplicate rows and numeric rows
acapslist <- acapslist %>%
  filter(is.na(as.numeric(as.character(country)))) %>%
  filter(country != "Country level") %>%
  filter(country != "Country Level") %>%
  filter(country != "")

# Save csv with full acapslist
write.csv(acapslist, "Indicator_dataset/acaps.csv")

# List of countries with specific hazards
conflictnams <- acapslist %>%
  filter(str_detect(acapslist$country, c("conflict|Crisis|crisis|Conflict|Refugees|refugees|
                                         Migration|migration|violence|violence|Boko Haram"))) %>%
  filter(risk >= 4) %>%
  dplyr::select(gaplist)

conflictnams <- unique(conflictnams)

# Food security countries
foodnams <- acapslist[str_detect(acapslist$country, c("Food|food|famine|famine")), ] %>%
  filter(risk >= 4) %>%
  dplyr::select(gaplist)

foodnams <- unique(foodnams)

# Natural hazard countries
naturalnams <- acapslist[str_detect(acapslist$country, c("Floods|floods|Drought|drought|Cyclone|cyclone|
                                                          Flooding|flooding|Landslides|landslides|
                                                          Earthquake|earthquake")), ] %>%
  filter(risk >= 3) %>%
  dplyr::select(gaplist)

naturalnams <- unique(naturalnams)

# Epidemic countries
healthnams <- acapslist[str_detect(acapslist$country, c("Epidemic|epidemic")), ] %>%
  filter(risk >= 3) %>%
  dplyr::select(gaplist)

healthnams <- unique(healthnams)

# Load countries in the CRM
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")

acapssheet <- countrylist %>%
  dplyr::select(-X) %>%
  mutate(
    Fr_conflict_acaps = case_when(
      Country %in% unlist(as.list(conflictnams)) ~ 10,
      TRUE ~ 0
    ),
    H_health_acaps = case_when(
      Country %in% unlist(as.list(healthnams)) ~ 10,
      TRUE ~ 0
    ),
    NH_natural_acaps = case_when(
      Country %in% unlist(as.list(naturalnams)) ~ 10,
      TRUE ~ 0
    ),
    F_food_acaps = case_when(
      Country %in% unlist(as.list(foodnams)) ~ 10,
      TRUE ~ 0
    )
  )

# Write ACAPS sheet
write.csv(acapssheet, "Risk_sheets/acapssheet.csv")

print("ACAPS sheet written")

#
##
### ********************************************************************************************
####    HEALTH: CREATE HEALTH SHEET USING A RANGE OF SOURCE INDICATORS ----
### ********************************************************************************************
##
#

#--------------------—HIS Score-----------------
HIS <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/HIS.csv")

HIS <- HIS %>%
  rename(Country = H_Country) %>%
  dplyr::select(-X)

#Normalise scores
HIS <- normfuncneg(HIS, 20, 70, "H_HIS_Score")

#-----------------------—Oxford rollback Score-----------------
#OXrollback <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-scratchpad/master/rollback_checklist/rollback_checklist.csv")

# Risk of Openness is the reviewed, and updated, version of Oxford Rollback.
OXrollback <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-scratchpad/master/risk_of_openness_index/data/riskindex_timeseries_latest.csv")

# Remove NAs and select columns
# Risk of Openness is a time series; select most recent
OXrollback <- OXrollback[!is.na(OXrollback$openness_risk),c("CountryCode", "Date", "openness_risk")] %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(desc(Date)) %>%
  { .[!duplicated(.$CountryCode),] } %>%
  dplyr::select(-Date)

colnames(OXrollback) <- paste0("H_", colnames(OXrollback))

OXrollback <- OXrollback %>%
  rename(
    H_Oxrollback_score = H_openness_risk,
    Country = H_CountryCode
  ) #%>%
  # mutate(
  #   Country = countrycode(Countryname,
  #   origin = "country.name",
  #   destination = "iso3c",
  #   nomatch = NULL
  # ))

upperrisk <- quantile(OXrollback$H_Oxrollback_score, probs = c(0.9), na.rm = T)
lowerrisk <- quantile(OXrollback$H_Oxrollback_score, probs = c(0.1), na.rm = T)

OXrollback <- normfuncpos(OXrollback, upperrisk, lowerrisk, "H_Oxrollback_score")

#------------------------—COVID deaths and cases--------------------------
covidweb <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")

covid <- covidweb %>%
  mutate(date = as.Date(date)) %>%
  filter(date > Sys.Date() - 28)

# bi-weekly growth rate for covid deaths and cases
covidgrowth <- covid %>%
  mutate(
    previous2week = case_when(
    date >= Sys.Date() - 13 ~ "twoweek",
    TRUE ~ "lasttwoweek"
  )) %>%
  group_by(iso_code, previous2week) %>%
  summarise(
    meandeaths = mean(new_deaths_per_million, na.rm = T),
    meancase = mean(new_cases_per_million, na.rm = T),
  )

covidgrowth <- covidgrowth %>%
  group_by(iso_code) %>%
  filter(!is.na(meandeaths) & !is.na(meancase)) 

# remove countries without two weeks
covidgrowth <- covidgrowth %>%
  mutate(remove = iso_code %in% 
           as.data.frame(covidgrowth %>% 
                           dplyr::count(iso_code) %>% 
                           filter(n == 2) %>% 
                           dplyr::select(iso_code))$iso_code)

# Calculate variables of interest
covidgrowth <- covidgrowth %>%
  filter(remove == TRUE) %>%
  mutate(
    growthdeath = meandeaths[previous2week == "twoweek"] - meandeaths,
    growthratedeaths = case_when(
      meandeaths[previous2week == "lasttwoweek"] == 0 ~ 0.01,
      meandeaths > 0 ~ growthdeath / meandeaths[previous2week == "lasttwoweek"] * 100,
      TRUE ~ NA_real_
    ),
    growthcase = meancase[previous2week == "twoweek"] - meancase,
    growthratecases = case_when(
      meandeaths[previous2week == "lasttwoweek"] == 0 ~ 0.01,
      meancase > 0 ~ growthcase / meancase[previous2week == "lasttwoweek"] * 100,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::filter(previous2week != "twoweek") %>%
  dplyr::select(-previous2week, -growthcase, -growthdeath, -meandeaths, -meancase, -remove)

# Normalised scores for deaths
covidgrowth <- normfuncpos(covidgrowth, 150, 0, "growthratedeaths")
covidgrowth <- normfuncpos(covidgrowth, 150, 0, "growthratecases")

# Rename columns
colnames(covidgrowth) <- c(
  "Country", "H_Covidgrowth_biweeklydeaths", "H_Covidgrowth_biweeklycases",
  "H_Covidgrowth_deathsnorm", "H_Covidgrowth_casesnorm"
)

# Varibles on number of cases
covidcurrent <- covid %>%
  filter(date == Sys.Date() - 1) %>%
  dplyr::select(iso_code, new_cases_smoothed_per_million, new_deaths_smoothed_per_million) %>%
  rename(Country = iso_code)

covidcurrent <- normfuncpos(covidcurrent, 250, 0, "new_cases_smoothed_per_million")
covidcurrent <- normfuncpos(covidcurrent, 5, 0, "new_deaths_smoothed_per_million")

# Rename columns
colnames(covidcurrent) <- c(
  "Country", "H_new_cases_smoothed_per_million", "H_new_deaths_smoothed_per_million",
  "H_new_cases_smoothed_per_million_norm", "H_new_deaths_smoothed_per_million_norm"
)

# #—(Alternative COVID deaths)
# # Load COVID data
# cov <- read.csv("https://raw.githubusercontent.com/scc-usc/ReCOVER-COVID-19/master/results/forecasts/global_deaths_current_0.csv")
# cov_current <- read.csv("https://raw.githubusercontent.com/scc-usc/ReCOVER-COVID-19/master/results/forecasts/global_deaths.csv")
# 
# # Summarise country totals (forecast)
# cov_dat <- cov %>%
#   dplyr::select(Country, colnames(cov)[10], colnames(cov)[9]) %>%
#   rename(
#     w8forecast = colnames(cov)[10], 
#     w7forecast = colnames(cov)[9]
#     ) %>%
#   mutate(Country = suppressWarnings(countrycode(Country, 
#                                                 origin = "country.name",
#                                                 destination = "iso3c"
#                                                 )
#          )) %>%
#   drop_na(Country)
# 
# # Summarise country totals (current)
# cov_cur <- cov_current %>%
#   dplyr::select(Country, last(colnames(cov_current))) %>%
#   rename(
#     current = last(colnames(cov_current)),
#     ) %>%
#   mutate(
#     Country = suppressWarnings(countrycode(Country, 
#                                                 origin = "country.name",
#                                                 destination = "iso3c"
#                                            )
#       )) %>%
#   drop_na(Country)
# 
# # Add population
# pop <- wpp.by.year(wpp.indicator("tpop"), 2020)
# 
# pop$charcode <- suppressWarnings(countrycode(pop$charcode, 
#                                              origin = "iso2c", 
#                                              destination = "iso3c"
#                                              )
#                                  )
# 
# colnames(pop) <- c("Country", "Population")
# 
# # Join datasets
# cov_forcast_alt <- left_join(cov_dat, pop, by = "Country", keep = F) %>%
#   left_join(., cov_cur) %>%
#   drop_na(Country) %>%
#   mutate(
#     week_increase = w8forecast - w7forecast,
#     new_death_per_m = week_increase / (Population / 1000),
#     add_death_prec_current = ((w8forecast / current) * 100) - 100
#     ) %>%
#   rename_with(.fn = ~ paste0("H_", .), 
#               .cols = colnames(.)[-1]
#               )
# 
# # Normalise
# cov_forcast_alt <- normfuncpos(cov_forcast_alt, 100, 0, "H_add_death_prec_current")

#--------------------------—Oxford Response Tracker----------------------------
Oxres <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv")

#Select latest data
Ox_cov_resp <- Oxres %>%
  group_by(CountryCode) %>%
  filter(Date == max(Date)) %>%
  dplyr::select(
    CountryCode, Date, GovernmentResponseIndex, GovernmentResponseIndexForDisplay,
    EconomicSupportIndex, EconomicSupportIndexForDisplay, ContainmentHealthIndex,
    ContainmentHealthIndexForDisplay, E1_Income.support, E1_Flag
  )

colnames(Ox_cov_resp) <- c("Country", paste0("H_", colnames(Ox_cov_resp[,-1])))

#Create normalised scores
Ox_cov_resp <- normfuncneg(Ox_cov_resp, 15, 80, "H_GovernmentResponseIndexForDisplay")
Ox_cov_resp <- normfuncneg(Ox_cov_resp, 0, 100, "H_EconomicSupportIndexForDisplay")

#------------------------------—INFORM COVID------------------------------------------------------
inform_cov <- read_html("https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Covid-19/INFORM-Covid-19-Warning-beta-version")

all_dat <- lapply(2:24, function(tt) {
  see <- lapply(c("data-country", "data-value", "style"), function(xx) {
    inform_cov %>% 
      html_nodes(paste0("td:nth_child(", paste(tt), ")")) %>%
      html_attr(xx)
  })
  do.call(rbind, Map(data.frame, cname = see[1], Value = see[2], Rating = see[3]))
})

inform_covid_warning_raw <- do.call(rbind, Map(data.frame, INFORM_rating=all_dat[1], covid_case_rate=all_dat[2], legal_stringency=all_dat[3],
                                               international_travel=all_dat[4], internal_movement=all_dat[5], stay_home=all_dat[6],
                                               income_support=all_dat[7], debt_relief=all_dat[8], gdp_change=all_dat[9],
                                               unemployment=all_dat[10], inflation=all_dat[11], school_close=all_dat[12],
                                               ipc_3_plus=all_dat[13], growth_events=all_dat[14], public_info=all_dat[15],
                                               testing_policy=all_dat[16], contact_trace=all_dat[17], growth_conflict=all_dat[18],
                                               seasonal_flood=all_dat[19], seasonal_cyclone=all_dat[20], seasonal_exposure=all_dat[21],
                                               ASAP_hotspot=all_dat[22]))

severity <- as.data.frame(all_dat[23]) %>%
  rename(INFORM_rating.cname = cname,
         INFORM_severity.Value = Value,
         INFORM_severity.Rating = Rating)

inform_covid_warning_raw <- left_join(inform_covid_warning_raw, severity, by = "INFORM_rating.cname")
  
inform_covid_warning <-  inform_covid_warning_raw %>%
  rename(
    Countryname = INFORM_rating.cname,
    hold_one = INFORM_severity.Rating
  ) %>%
  dplyr::select(-contains(".cname")) %>%
  mutate_at(
    vars(contains(".Rating")),
    ~ case_when(
      . == "background:#FF0000;" ~ "High",
      . == "background:#FFD800;" ~ "Medium",
      . == "background:#00FF00;" ~ "Low",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    hold_one = case_when(
      hold_one == "background:#FF0000;" ~ "High",
      hold_one == "background:#FFD800;" ~ "Medium",
      is.na(hold_one) ~ "Low",
      TRUE ~ NA_character_
    )) %>%
  rename(INFORM_severity.Rating = hold_one) %>%
  mutate(
    INFORM_rating.Value = as.numeric(as.character(INFORM_rating.Value)),
    Country = countrycode(Countryname, origin = "country.name", destination = "iso3c", nomatch = NULL
    )) %>%
  dplyr::select(-Countryname) %>%
  rename_with(
    .fn = ~ paste0("H_", .), 
    .cols = colnames(.)[!colnames(.) %in% c("Country", "Countryname")]
  )

inform_covid_warning <- normfuncpos(inform_covid_warning, 6, 2, "H_INFORM_rating.Value")

write.csv(inform_covid_warning, "Indicator_dataset/inform_covid_warning.csv")

#----------------------------------—WMO DONS--------------------------------------------------------------
dons_raw <- read_html("https://www.who.int/emergencies/disease-outbreak-news")

dons_select <- dons_raw %>%
  html_nodes(".sf-list-vertical") %>%
  html_nodes("h4") #%>%
#html_text()

dons_date <- dons_select %>%
  html_nodes("span:nth-child(2)") %>%
  html_text()

dons_text <- dons_select %>%
  html_nodes(".trimmed") %>%
  html_text()

wmo_don_full <- bind_cols(dons_text, dons_date) %>%
  rename(text = "...1" ,
         date = "...2") %>%
  mutate(disease = trimws(sub("\\s[-——].*", "", text)),
         country = trimws(sub(".*–", "", text)),
         country = trimws(sub(".*-", "", country)),
         date = dmy(date)) %>%
  separate_rows(country, sep = ",") %>%
  mutate(wmo_country_alert = countrycode(country,
                                         origin = "country.name",
                                         destination = "iso3c",
                                         nomatch = NULL
  ))

countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")
wmo_don <- countrylist %>%
  dplyr::select(-X) %>%
  mutate(wmo_don_alert = case_when(Country %in% wmo_don_full$wmo_country_alert ~ 10,
                                   TRUE ~ 0))  %>%
  rename(H_wmo_don_alert = wmo_don_alert) %>%
  dplyr::select(-Countryname)

#---------------------------------—Health ACAPS---------------------------------
acaps_health <- acapssheet[,c("Country", "H_health_acaps")]

#----------------------------------—Create combined Health Sheet-------------------------------------------
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")
countrylist <- countrylist %>%
  dplyr::select(-X)

health_sheet <- left_join(countrylist, HIS, by = "Country") %>%
  left_join(., OXrollback, by = "Country") %>%
  left_join(., covidgrowth, by = "Country") %>%
  left_join(., covidcurrent, by = "Country") %>%
  left_join(., Ox_cov_resp, by = "Country") %>%
  # left_join(., cov_forcast_alt, by = "Country") %>% # not current
  left_join(., inform_covid_warning, by = "Country", "Countryname") %>%
  left_join(., wmo_don, by = "Country") %>%
  left_join(., acaps_health, by = "Country") %>%
  arrange(Country)

write.csv(health_sheet, "Risk_sheets/healthsheet.csv")
print("Health sheet written")
#
##
### ********************************************************************************************
####    FOOD SECURITY: CREATE FOOD SECURITY SHEET USING A RANGE OF SOURCE INDICATORS ----
### ********************************************************************************************
##
#

#---------------------------------—LOAD FOOD SECURITY DATA---------------------------
# -------------------------------— Proteus Index -------------------------------
proteus <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/proteus.csv")

proteus <- proteus %>%
  rename(F_Proteus_Score = Proteus.index) %>%
  dplyr::select(-X) %>%
  mutate(
    Country = countrycode(Country,
    origin = "country.name",
    destination = "iso3c",
    nomatch = NULL
  ))

upperrisk <- quantile(proteus$F_Proteus_Score, probs = c(0.90), na.rm = T)
lowerrisk <- quantile(proteus$F_Proteus_Score, probs = c(0.10), na.rm = T)
proteus <- normfuncpos(proteus, upperrisk, lowerrisk, "F_Proteus_Score")

# # —(Artemis)
# #artemis <- read.csv("~/Google Drive/PhD/R code/Compound Risk/Restricted_Data/artemis.csv")
# # artemis <- read.csv("https://drive.google.com/file/d/1EmGwfnF2iS4-oHqn3HoXxuv12YGQyGDv/view?usp=sharing")
# drive_download("Restricted_Data/artemis.csv", path = "tmp-artemis.csv")
# artemis <- read.csv("tmp-artemis.csv")
# unlink("tmp-artemis.csv")
# 
# upperrisk <- 0.2
# lowerrisk <- 0
# artemis <- normfuncpos(artemis, upperrisk, lowerrisk, "F_Artemis_Score")
# 
# artemis <- artemis %>%
#   mutate(
#     Country = countrycode(Country,
#                           origin = "country.name",
#                           destination = "iso3c",
#                           nomatch = NULL
#       )
#     ) %>%
#   dplyr::select(-X)

#------------------—FEWSNET (with CRW threshold)---

#Load database
fewswb <- suppressMessages(read_csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/FEWS February 2021 Update_04-05-21.csv", col_types = cols()))

#Calculate country totals
fewsg <- fewswb %>%
#  dplyr::select(-X) %>%
  group_by(country, year_month) %>%
  mutate(countrypop = sum(pop)) %>%
  ungroup()

#Calculate proportion and number of people in IPC class 
fewspop <- fewsg %>%
  group_by(country, year_month) %>%
  mutate(
    countryproportion = (pop / countrypop) * 100,
    ipc3plusabsfor = case_when(fews_proj_med_adjusted >=3 ~ pop,
                               TRUE ~ NA_real_),
    ipc3pluspercfor = case_when(fews_proj_med_adjusted >=3 ~ countryproportion,
                                TRUE ~ NA_real_),
    ipc4plusabsfor = case_when(fews_proj_med_adjusted >= 4 ~ pop,
                               TRUE ~ NA_real_),
    ipc4pluspercfor = case_when(fews_proj_med_adjusted >= 4 ~ countryproportion,
                                TRUE ~ NA_real_),
    ipc3plusabsnow = case_when(fews_ipc_adjusted >=3 ~ pop,
                               TRUE ~ NA_real_),
    ipc3pluspercnow = case_when(fews_ipc_adjusted >=3 ~ countryproportion,
                                TRUE ~ NA_real_),
    ipc4plusabsnow = case_when(fews_ipc_adjusted >= 4 ~ pop,
                               TRUE ~ NA_real_),
    ipc4pluspercnow = case_when(fews_ipc_adjusted >= 4 ~ countryproportion,
                                TRUE ~ NA_real_)
  )

#Functions to calculate absolute and geometric growth rates
pctabs <- function(x) x- lag(x)
pctperc <- function(x) x - lag(x) / lag(x)

#Summarise country totals per in last round of FEWS
fewssum <- fewspop %>%
  filter(year_month == "2021_02" | year_month == "2020_10") %>%
  group_by(country, year_month) %>%
  mutate(totalipc3plusabsfor = sum(ipc3plusabsfor, na.rm=T),
         totalipc3pluspercfor = sum(ipc3pluspercfor, na.rm=T),
         totalipc4plusabsfor = sum(ipc4plusabsfor, na.rm=T),
         totalipc4pluspercfor = sum(ipc4pluspercfor, na.rm=T),
         totalipc3plusabsnow = sum(ipc3plusabsnow, na.rm=T),
         totalipc3pluspercnow = sum(ipc3pluspercnow, na.rm=T),
         totalipc4plusabsnow = sum(ipc4plusabsnow, na.rm=T),
         totalipc4pluspercnow = sum(ipc4pluspercnow, na.rm=T)) %>%
  distinct(country, year_month, .keep_all = TRUE) %>%
  dplyr::select(-ipc3plusabsfor, -ipc3pluspercfor, -ipc4plusabsfor, -ipc4pluspercfor, 
         -ipc3plusabsnow, -ipc3pluspercnow, -ipc4plusabsnow, -ipc4pluspercnow,
         -admin_name, -pop) %>%
  group_by(country) %>%
  mutate(pctchangeipc3for = pctabs(totalipc3pluspercfor),
         pctchangeipc4for = pctperc(totalipc4pluspercfor),
         pctchangeipc3now = pctabs(totalipc3pluspercnow),
         pctchangeipc4now = pctperc(totalipc4pluspercnow),
         diffactfor = totalipc3pluspercfor - totalipc3pluspercnow,
         fshighrisk = case_when((totalipc3plusabsfor >= 5000000 | totalipc3pluspercfor >= 20) & pctchangeipc3for >= 5  ~ "High risk",
                                (totalipc3plusabsnow >= 5000000 | totalipc3pluspercnow >= 20) & pctchangeipc3now >= 5  ~ "High risk",
                                totalipc4pluspercfor >= 2.5  & pctchangeipc4for >= 10  ~ "High risk",
                                totalipc4pluspercnow >= 2.5  & pctchangeipc4now >= 10  ~ "High risk",
                                TRUE ~ "Not high risk")) %>%
  dplyr::select(-fews_ipc, -fews_ha, -fews_proj_near, -fews_proj_near_ha, -fews_proj_med, 
         -fews_proj_med_ha, -fews_ipc_adjusted, -fews_proj_med_adjusted, -countryproportion) %>%
  filter(year_month == "2021_02")

# Find max ipc for any region in the country
fews_summary <- fewsg %>%
  group_by(country, year_month) %>%
  summarise(max_ipc = max(fews_proj_med_adjusted, na.rm = T)) %>%
  mutate(
    year_month = str_replace(year_month, "_", "-"),
    year_month = as.Date(as.yearmon(year_month)),
    year_month = as.Date(year_month)) %>%
  filter(!is.infinite(max_ipc)) %>%
  filter(year_month == max(year_month, na.rm = T))

# Join the two datasets
fews_dataset <- left_join(fewssum, fews_summary, by = "country") %>%
  mutate(
    fews_crm_norm = case_when(
      fshighrisk == "High risk" ~ 10,
      fshighrisk != "High risk" & max_ipc == 5 ~ 9,
      fshighrisk != "High risk" & max_ipc == 4 ~ 8,
      fshighrisk != "High risk" & max_ipc == 3 ~ 7,
      fshighrisk != "High risk" & max_ipc == 2 ~ 5,
      fshighrisk != "High risk" & max_ipc == 1 ~ 3,
      TRUE ~ NA_real_
    ),
    Country = countrycode(
      country,
      origin = "country.name",
      destination = "iso3c",
      nomatch = NULL
    )) %>%
  dplyr::select(-country) %>%
  rename_with(
    .fn = ~ paste0("F_", .),
    .cols = colnames(.)[!colnames(.) %in% c("Country", "country")]
  )

colnames(fews_dataset[-1]) <- paste0("F_", colnames(fews_dataset[-1])) 

# #—(Alternative Food price volatility scopes) -----------------------
# fao_fpma <- read_html("http://www.fao.org/giews/food-prices/home/en/")
# 
# #Highlight countries that are undergoing high FPV  
# fpv_alt_raw <- fao_fpma %>%
#   html_nodes(".maplist-country") %>%
#   html_text() %>% 
#   as.tibble() %>%
#   mutate(Country = countrycode(value,
#                                origin = "country.name",
#                                destination = "iso3c",
#                                nomatch = NULL)
#   )
# 
# #Compile dataframe with 10 for high risk countries
# countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")
# 
# fpv_alt <- countrylist %>%
#   dplyr::select(-X, -Countryname) %>%
#   mutate(F_fpv_alt = case_when(Country %in% fpv_alt_raw$Country ~ 10,
#                                TRUE ~ 0)
#   )
# 
# #Write csv and save to github
# write.csv(fpv_alt, "Indicator_Dataset/FPV_alternative.csv")

#------------------------—WBG FOOD PRICE MONITOR------------------------------------
ag_ob_data <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/food-inflation.csv")

ag_ob_data <- ag_ob_data %>%
  mutate_at(
    vars(contains("19"), contains("20"), contains("21")),
    ~ as.numeric(as.character(gsub(",", ".", .)))
  )

ag_ob <- ag_ob_data %>%
  filter(X == "Food Change Yoy") %>%
  dplyr::select(-Income.Level, -Color.Bin, -X) %>%
  group_by(Country) %>%
  summarise(
    Apr = Apr.20[which(!is.na(Apr.20))[1]],
    May = May.20[which(!is.na(May.20))[1]],
    June = Jun.20[which(!is.na(Jun.20))[1]],
    Jul = Jul.20[which(!is.na(Jul.20))[1]],
    Aug = Aug.20[which(!is.na(Aug.20))[1]],
    Sep = Sep.20[which(!is.na(Sep.20))[1]],
    Oct = Oct.20[which(!is.na(Oct.20))[1]],
    Nov = Nov.20[which(!is.na(Nov.20))[1]],
  ) %>%
  mutate(fpv = case_when(
    !is.na(June) ~ June,
    is.na(June) & !is.na(May) ~ May,
    is.na(June) & is.na(May) & !is.na(Apr) ~ Apr,
    TRUE ~ NA_real_
  ),
  fpv_rating = case_when(
    fpv <= 0.02 ~ 1,
    fpv > 0.02 & fpv <= 0.05 ~ 3,
    fpv > 0.05 & fpv <= 0.30 ~ 5,
    fpv >= 0.30 ~ 7,
    TRUE ~ NA_real_
  ),
  Country = countrycode(Country,
                        origin = "country.name",
                        destination = "iso3c",
                        nomatch = NULL
  )) %>%
  rename_with(   
    .fn = ~ paste0("F_", .),
    .cols = colnames(.)[!colnames(.) %in% c("Country")]
  )

#-------------------------—FAO/WFP HOTSPOTS----------------------------
fao_wfp <- suppressWarnings(read_csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/WFP%3AFAO_food.csv", col_types = cols()) %>%
  dplyr::select(-X2))

fao_wfp <- fao_wfp %>%
  mutate(Country = countrycode(Country,
                               origin = "country.name",
                               destination = "iso3c",
                               nomatch = NULL
  ))

fao_wfp$F_fao_wfp_warning <- 10

#------------------------—Create combined Food Security sheet--------------------
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")
countrylist <- countrylist %>%
  dplyr::select(-X)

food_sheet <- left_join(countrylist, proteus, by = "Country") %>%
  left_join(., fews_dataset, by = "Country") %>%
  # left_join(., fpv, by = "Country") %>%  # Reintroduce if FAO price site comes back online
  # left_join(., fpv_alt, by = "Country") %>% # not current
  # left_join(., artemis, by = "Country") %>% # not current
  left_join(., ag_ob, by = "Country") %>%
  left_join(., fao_wfp, by = "Country") %>%
  arrange(Country)

write.csv(food_sheet, "Risk_sheets/foodsecuritysheet.csv")
print("Food sheet written")

#
##
### ********************************************************************************************
####    (DEBT: CREATE DEBT SHEET USING A RANGE OF SOURCE INDICATORS) ----
### ********************************************************************************************
##
#

# #---------------------------—LOAD DEBT DATA----------------------------
# # SCRAPE DEBT DATA
# debtweb <- "https://www.worldbank.org/en/programs/debt-toolkit/dsa"
# debt <- read_html(debtweb)
# 
# debttab <- debt %>%
#   html_nodes("table") %>%
#   html_table(fill = TRUE)
# 
# debttab <- as.data.frame(debttab)
# colnames(debttab) <- debttab[1, ]
# debttab <- debttab[-1, ]
# 
# debttab <- debttab %>%
#   mutate(
#     Country = gsub("[0-9]+", "", Country),
#     Country = countrycode(Country,
#       origin = "country.name",
#       destination = "iso3c",
#       nomatch = NULL
#     )
#   ) %>%
#   filter(Country != c("TOTAL"))
# 
# colnames(debttab) <- c("Country", "D_WB_external_debt_distress", "D_overall_debt_distress", "D_debt_date")
# 
# debttab$D_WB_external_debt_distress_norm <- ifelse(debttab$D_WB_external_debt_distress == "In distress", 10,
#   ifelse(debttab$D_WB_external_debt_distress == "High", 10,
#     ifelse(debttab$D_WB_external_debt_distress == "Moderate", 7,
#       ifelse(debttab$D_WB_external_debt_distress == "Low", 3, NA)
#     )
#   )
# )
# 
# #----------------—IMF Debt forecasts---------------------------------
# imfdebt <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/imfdebt.csv")
# 
# imfdebt <- imfdebt %>%
#   mutate(Country = countrycode(Country,
#     origin = "country.name",
#     destination = "iso3c",
#     nomatch = NULL
#   )) %>%
#   dplyr::select(-X)
# 
# names <- c(
#   "D_IMF_debt2017", "D_IMF_debt2018", "D_IMF_debt2019",
#   "D_IMF_debt2020", "D_IMF_debt2021", "D_IMF_debt2020.2019"
# )
# 
# imfdebt[names] <- lapply(imfdebt[names], function(xx) {
#   suppressWarnings(as.numeric(as.character(xx)))
# })
# 
# # Calculate 2021 - 2020
# imfdebt <- imfdebt %>%
#   mutate(D_IMF_debt2021.2020 = D_IMF_debt2021 - D_IMF_debt2020)
# 
# # Normalise
# imfdebt <- normfuncneg(imfdebt, -5, 5, "D_IMF_debt2020.2019")
# imfdebt <- normfuncneg(imfdebt, -5, 5, "D_IMF_debt2021.2020")
# 
# #-------------------—IGC database--------------------------
# igc <- suppressWarnings(gsheet2tbl("https://docs.google.com/spreadsheets/d/1s46Oz_NtkyrowAracg9FTPYRdkP8GHRzQqM7w6Qa_2k/edit?ts=5e836726#gid=0"))
# 
# # Header as first row
# names(igc) <- as.matrix(igc[1, ])
# igc <- igc[-1, ]
# igc[] <- lapply(igc, function(x) type.convert(as.character(x)))
# 
# # Name fiscal column
# colnames(igc)[which(colnames(igc) == "Announced/ estimated fiscal support package\n") + 1] <- "fiscalgdp"
# igc <- igc[-which(is.na(colnames(igc)))]
# 
# # Extract numbers before the % text
# igc$fiscalgdpnum <- sub("\\%.*", "", igc$fiscalgdp)
# igc$fiscalgdpnum <- as.numeric(as.character(igc$fiscalgdpnum))
# 
# # Normalised scores
# upperrisk <- quantile(igc$fiscalgdpnum, probs = c(0.05), na.rm = T)
# lowerrisk <- quantile(igc$fiscalgdpnum, probs = c(0.95), na.rm = T)
# igc <- normfuncneg(igc, upperrisk, lowerrisk, "fiscalgdpnum")
# 
# # Add label tabs
# colnames(igc) <- paste0("D_", colnames(igc))
# igc <- as.data.frame(igc)
# igc <- igc[-which(colnames(igc) == "D_Other reputable links")]
# 
# igc <- igc %>%
#   rename(Country = D_iso3c) %>%
#   dplyr::select(Country, D_fiscalgdpnum, D_fiscalgdpnum_norm)
#   
# #------------------------------—COVID Economic Stimulus Index-------------------------
# # Load file
# url <- "http://web.boun.edu.tr/elgin/CESI_15.xlsx" # Note: may need to check for more recent versions
# destfile <- "Indicator_dataset/cesiraw.xlsx"
# curl::curl_download(url, destfile)
# cesi <- read_excel(destfile)
# 
# colnames(cesi) <- paste0("D_", colnames(cesi))
# colnames(cesi) <- gsub("_1.*", "", colnames(cesi))
# cesi <- cesi %>%
#   mutate(Country = countrycode(
#     D_Country,
#     origin = "country.name",
#     destination = "iso3c",
#     nomatch = NULL
#   )) %>%
#   dplyr::select(-D_Country, -D_Date)
# 
# # Perform PCA
# cesipca <- prcomp(cesi %>% dplyr::select(
#   D_fiscal, D_ratecut, D_reserve_req,
#   D_macrofin, D_othermonetary, D_bopgdp,
#   D_otherbop
#   ),
# center = TRUE,
# scale. = TRUE
# )
# 
# # Assign CESI Index as the first two PCs
# cesi$D_CESI_Index <- cesipca$x[, 1] + cesipca$x[, 2]
# 
# # Normalised scores
# upperrisk <- quantile(cesi$D_CESI_Index, probs = c(0.1), na.rm = T)
# lowerrisk <- quantile(cesi$D_CESI_Index, probs = c(0.95), na.rm = T)
# cesi <- normfuncneg(cesi, upperrisk, lowerrisk, "D_CESI_Index")
# 
# #------------------—Import Economic Stimulus Index from the Health Sheet---------------------
# Ox_fiscal <- Ox_cov_resp %>%
#   dplyr::select(Country, H_EconomicSupportIndexForDisplay_norm) %>%
#   rename(D_EconomicSupportIndexForDisplay_norm = H_EconomicSupportIndexForDisplay_norm)
# 
# #-----------------—CPIA----------------------------------------------------
# cpia_data <- read.csv("Indicator_dataset/cpia.csv")
# 
# cpia_data <- cpia_data %>%
#   rename(Country = ISO3,
#          D_CPIA.scores = CPIA.scores) %>%
#   mutate(
#     D_CPIA.scores = case_when(
#       D_CPIA.scores == 0 ~ NA_real_,
#       TRUE ~ D_CPIA.scores
#     )
#   )
# 
# cpia <- normfuncneg(cpia_data, 2.9, 5, "D_CPIA.scores")
# 
# #-------------------------—CREATE DEBT SHEET-----------------------------------
# countrylist <- read.csv("Indicator_dataset/countrylist.csv")
# countrylist <- countrylist %>%
#   dplyr::select(-X)
# 
# debtsheet <- left_join(countrylist, debttab, by = "Country") %>%
#   left_join(., imfdebt, by = "Country") %>%
#   left_join(., igc, by = "Country") %>%
#   left_join(., Ox_fiscal, by = "Country") %>%
#   left_join(., cesi, by = "Country") %>%
#   left_join(., cpia, by = "Country") %>%
#   arrange(Country)
# 
# write.csv(debtsheet, "Risk_sheets/debtsheet.csv")

#
##
### ********************************************************************************************
####    MACRO: CREATE MACRO  SHEET USING A RANGE OF SOURCE INDICATORS ----
### ********************************************************************************************
##
#
# 
# #—(MACRO DATA)---
# macro <- read.csv("Indicator_dataset/macro.csv")
# macro <- macro %>%
#   mutate(
#     M_Economic_Dependence_Score = rowMeans(dplyr::select(., c(M_Fuel_Imports_perc, M_Food_Imports_perc, M_Travel_Tourism_perc)), na.rm = T),
#     M_Financial_Resilience_Score = rowMeans(dplyr::select(., c(M_Remittance_perc, M_Reserves, M_ODA_perc, M_Gsavings_perc)), na.rm = T)) %>%
#   mutate(M_Economic_and_Financial_score = rowMeans(dplyr::select(., c(M_Economic_Dependence_Score, M_Financial_Resilience_Score)), na.rm = T)) %>%
#   dplyr::select(-X)
# upperrisk <- quantile(macro$M_Economic_and_Financial_score, probs = c(0.9), na.rm = T)
# lowerrisk <- quantile(macro$M_Economic_and_Financial_score, probs = c(0.1), na.rm = T)
# macro <- normfuncpos(macro, upperrisk, lowerrisk, "M_Economic_and_Financial_score")

# #-—(GDP forecast 2020)---
# gdp <- suppressMessages(read_csv("Indicator_dataset/gdp.csv"))
# gdp <- gdp %>%
#   dplyr::select(-X1)
# gdp <- normfuncneg(gdp, -5, 0, "M_GDP_WB_2019minus2020")
# gdp <- normfuncneg(gdp, -5, 0, "M_GDP_IMF_2019minus2020")
# 
# # Load GDP data for 2021
# wb_gdp_2021 <- suppressMessages(read_csv("Indicator_dataset/WB_GEP_Jan_2021.csv", 
#                                          skip = 1)) %>%
#   dplyr::select(-X1, -ISO3)
# 
# countrylist <- read.csv("Indicator_dataset/countrylist.csv")
# countrylist <- countrylist %>%
#   dplyr::select(-X)
# 
# wb_gdp_2021 <- wb_gdp_2021 %>%
#   mutate(
#     Country = countrycode(`Country Names`,
#                           origin = "country.name",
#                           destination = "iso3c",
#                           nomatch = NULL),
#     M_WB_gdp_diff = `2021f` - `2020e`) %>%
#   rename(M_WB_gdp_2021 =  `2021f`,
#          M_WB_gdp_2020 =  `2020e`) %>%
#   dplyr::select(-`Country Names`)
# 
# wb_gdp_2021 <- normfuncneg(wb_gdp_2021, 0, 10, "M_WB_gdp_diff")
# wb_gdp_2021 <- normfuncneg(wb_gdp_2021, 0, 5, "M_WB_gdp_2021")
# wb_gdp_2021 <- normfuncneg(wb_gdp_2021, -10, 5, "M_WB_gdp_2020")
# 
# # Create joint normalised value
# wb_gdp_2021 <- wb_gdp_2021 %>%
#   mutate(
#     M_WB_gdp_20_21_norm = rowMaxs(as.matrix(dplyr::select(wb_gdp_2021, 
#                                                      M_WB_gdp_2021_norm, 
#                                                      M_WB_gdp_2020_norm))
#                              , na.rm = T))
# 
# # Integrate into GDP data
# gdp <- left_join(gdp, 
#                  wb_gdp_2021 %>% dplyr::select(Country, M_WB_gdp_20_21_norm),
#                  by = "Country")
# 
# # Load IMF gdp data
# imf_gdp_2021 <- read.csv("Indicator_dataset/IMF_GDP.csv")
# 
# imf_gdp_2021 <- imf_gdp_2021 %>%
#   filter(WEO.Subject.Code == "NGDP_RPCH") %>%
#   mutate_at(
#     vars(contains("X19"), contains("X20")),
#     ~ as.numeric(as.character(.))
#           ) %>%
#   mutate(
#     M_IMF_gdp_2021 = X2021 ,
#     M_IMF_gdp_2020 = X2020) %>%
#   dplyr::select(ISO, M_IMF_gdp_2021, M_IMF_gdp_2020) %>%
#   rename(Country = ISO) 
# 
# # Normalise gdp change
# imf_gdp_2021 <- normfuncneg(imf_gdp_2021, 0, 5, "M_IMF_gdp_2021")
# imf_gdp_2021 <- normfuncneg(imf_gdp_2021, -10, 5, "M_IMF_gdp_2020")
# 
# # Join 20 and 21
# imf_gdp_2021 <- imf_gdp_2021 %>%
#   mutate(
#     M_imf_gdp_diff_norm = rowMaxs(as.matrix(dplyr::select(., 
#                                                      M_IMF_gdp_2021_norm, 
#                                                      M_IMF_gdp_2020_norm))
#                              , na.rm = T),
#     M_imf_gdp_diff_norm = case_when(is.infinite(M_imf_gdp_diff_norm) ~ NA_real_,
#                                TRUE ~ M_imf_gdp_diff_norm)
#     )
# 
# # Integrate into GDP data
# gdp <- left_join(gdp, 
#                  imf_gdp_2021 %>% dplyr::select(Country, M_imf_gdp_diff_norm),
#                  by = "Country")

# #---—(MACRO FIN REVIEW)
# data <- read.csv("Indicator_dataset/macrofin.csv")
# 
# macrofin <- data %>%
#   mutate_at(
#     vars(Monetary.and.financial.conditions, contains("risk")),
#     funs(case_when(
#       . == "Low" ~ 0,
#       . == "Medium" ~ 0.5,
#       . == "High" ~ 1,
#       TRUE ~ NA_real_
#     ))) %>%
#   mutate(macrofin_risk = dplyr::select(., Spillover.risks.from.the.external.environment.outside.the.region:Household.risks) %>% rowSums(na.rm=T)) %>%
#   rename_with(
#     .fn = ~ paste0("M_", .),
#     .cols = colnames(.)[!colnames(.) %in% c("Country.Name","ISO3")]
#   ) %>%
#   rename(Country = ISO3) %>%
#   dplyr::select(-Country.Name)
# 
# macrofin <- normfuncpos(macrofin, 2.1, 0, "M_macrofin_risk")

#---------------------------—Economist Intelligence Unit---------------------------------
url <- "https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/RBTracker.xls"
destfile <- "RBTracker.xls"
curl::curl_download(url, destfile)
eiu_data <- read_excel(destfile, sheet = "Data Values", skip = 3)

country_nam <- colnames(eiu_data) 
country_nam <- country_nam[4:length(country_nam)]

eiu_latest_month <- eiu_data %>%
  filter(MONTH == max(MONTH)) %>% 
  dplyr::select(-MONTH, -`SERIES CODE`) %>%
  #Pivot the database so countries are rows
  pivot_longer(
    !`SERIES NAME`,
    names_to = "Country",
    values_to = "Values"
  ) %>%
  pivot_wider(
    names_from = `SERIES NAME`,
    values_from = Values
  ) 

eiu_one_year <- eiu_data %>%
  filter(MONTH %in% unique(eiu_data$MONTH)[-1]) %>%
  group_by(`SERIES NAME`) %>%
  summarise_at(country_nam, mean, na.rm = T) %>%
  ungroup %>%
  distinct(`SERIES NAME`, .keep_all = T) %>% 
  #Pivot the database so countries are rows
  pivot_longer(
    !`SERIES NAME`,
    names_to = "Country",
    values_to = "Values"
  ) %>%
  pivot_wider(
    names_from = `SERIES NAME`,
    values_from = Values
  ) %>%
  rename_with(
    .col = c(contains("risk"), contains("Overall")),
    .fn  = ~ paste0(., "_12")
  ) 

eiu_three_month <- eiu_data %>%
  filter(MONTH %in% head(unique(MONTH)[-1], 3)) %>%
  group_by(MONTH, `SERIES NAME`) %>%
  summarise_at(country_nam, mean, na.rm = T) %>%
  ungroup %>%
  dplyr::select(-MONTH) %>%
  distinct(`SERIES NAME`, .keep_all = T) %>% 
  #Pivot the database so countries are rows
  pivot_longer(
    !`SERIES NAME`,
    names_to = "Country",
    values_to = "Values"
  ) %>%
  pivot_wider(
    names_from = `SERIES NAME`,
    values_from = Values
  ) %>%
  rename_with(
    .col = c(contains("risk"), contains("Overall")),
    .fn  = ~ paste0(., "_3")
  )

# Join datasets
eiu_joint <- left_join(eiu_latest_month, eiu_three_month, by = "Country") %>%
  left_join(., eiu_one_year, by = "Country") %>%
  mutate(
    EIU_3m_change = `Overall Evaluation` - `Overall Evaluation_3`,
    EIU_12m_change = `Overall Evaluation` - `Overall Evaluation_12`) %>%
  dplyr::select(contains("Country"), contains("Overall"), contains("EIU")) %>%
  rename_with(
    .col = c(contains("Overall"), contains("EIU")),
    .fn = ~ paste0("M_", .)
  ) %>%
  rename(M_EIU_Score = `M_Overall Evaluation`,
         M_EIU_Score_12m = `M_Overall Evaluation_12`) %>%
  # Add Country name
  mutate(
    Country = suppressWarnings(countrycode(Country,
                                      origin = "country.name",
                                      destination = "iso3c",
                                      nomatch = NULL))
    )
  
eiu_joint <- normfuncpos(eiu_joint, 70, 15, "M_EIU_Score")
eiu_joint <- normfuncpos(eiu_joint, 2, -2, "M_EIU_12m_change")
eiu_joint <- normfuncpos(eiu_joint, 70, 15, "M_EIU_Score_12m")

# #---—(Corporate Vulnerability Index)-
# cvi <- read.csv("Indicator_dataset/cvi.csv")
# 
# # Arrange dataset
# cvi <- cvi %>% 
#   dplyr::select(-Country.Name) %>%
#   rename(M_cvi_risk = CVI..higher.more.vulnerable..0.to.1.,
#          Country = ISO3)
# 
# # Normalise 
# cvi <- normfuncpos(cvi, 0.2, 0, "M_cvi_risk")

#-----------------------------—Create Combined Macro sheet-----------------------------------------
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")
countrylist <- countrylist %>%
  dplyr::select(-X)

macro_sheet <- #left_join(countrylist, macro, by = "Country") %>% # not current
  # left_join(., gdp, by = "Country") %>% # not current
  # left_join(., macrofin, by = "Country") %>% # not current
  left_join(countrylist, eiu_joint, by = "Country") %>%
  # left_join(., cvi, by = "Country") %>% # not current
  arrange(Country) 

write.csv(macro_sheet, "Risk_sheets/macrosheet.csv")
print("Macro sheet written")

#
##
### ********************************************************************************************
####    SOCIO: CREATE SOCIO-ECONOMIC SHEET USING A RANGE OF SOURCE INDICATORS ----
### ********************************************************************************************
##
#

# #--—(OCHA)--
# # Load OCHA database
# ocha <- suppressMessages(read_csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/OCHA_socioeconomic.csv"))
# ocha <- ocha %>%
#   dplyr::select(-X1) %>%
#   arrange(Country)
# 
# upperrisk <- quantile(ocha$S_OCHA_Covid.vulnerability.index, probs = c(0.95), na.rm = T)
# lowerrisk <- quantile(ocha$S_OCHA_Covid.vulnerability.index, probs = c(0.05), na.rm = T)
# ocha <- normfuncpos(ocha, upperrisk, lowerrisk, "S_OCHA_Covid.vulnerability.index")

#---------------------------—Alternative socio-economic data (based on INFORM)
inform_2021 <- suppressMessages(read_csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/INFORM_2021.csv", col_types = cols()))

inform_data <- inform_2021 %>%
  dplyr::select(Country, "Socio-Economic Vulnerability") %>%
  rename(S_INFORM_vul = "Socio-Economic Vulnerability")

inform_data <- normfuncpos(inform_data, 7, 0, "S_INFORM_vul")
inform_data <- normfuncpos(inform_data, 7, 0, "S_INFORM_vul")

#------------------------—Forward-looking socio-economic variables from INFORM---------------------------
socio_forward <- inform_covid_warning %>%
  dplyr::select(
    Country, H_gdp_change.Value,H_gdp_change.Rating, H_unemployment.Value,
    H_unemployment.Rating, H_income_support.Value, H_income_support.Rating
  ) %>%
  rename_with(
    .fn = ~ str_replace(., "H_", "S_"),
    .cols = colnames(.)[-1]
  ) %>%
  mutate_at(
    vars(S_gdp_change.Rating, S_unemployment.Rating),
    funs(norm = case_when(
      . == "High" ~ 10,
      . == "Medium" ~ 7,
      . == "Low" ~ 0,
      TRUE ~ NA_real_
    ))
  ) %>%
  mutate(
    S_income_support.Rating_crm_norm = case_when(
      S_income_support.Value == "No income support" ~ 7,
      S_income_support.Value == "Government is replacing more than 50% of lost salary (or if a flat sum, it  ..." ~ 3,
      S_income_support.Value == "Government is replacing less than 50% of lost salary (or if a flat sum, it  ..." ~ 0,
      TRUE ~ NA_real_
    ))

#--------------------------—MPO: Poverty projections----------------------------------------------------
# mpo <- read_dta("~/Google Drive/PhD/R code/Compound Risk/global.dta")

drive_download("Restricted_Data/global.dta", path = "tmp-global.dta", overwrite = T, verbose = F)
mpo <- read_dta("tmp-global.dta")
unlink("tmp-global.dta")

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
  )

#-----------------------------—HOUSEHOLD HEATMAP FROM MACROFIN-------------------------------------
# If Macro Fin Review is re-included above, we can reuse that. For clarity, moving data read here because it's not being used by macrosheet
data <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/macrofin.csv")

macrofin <- data %>%
  mutate_at(
    vars(Monetary.and.financial.conditions, contains("risk")),
    funs(case_when(
      . == "Low" ~ 0,
      . == "Medium" ~ 0.5,
      . == "High" ~ 1,
      TRUE ~ NA_real_
    ))) %>%
  mutate(macrofin_risk = dplyr::select(., Spillover.risks.from.the.external.environment.outside.the.region:Household.risks) %>% rowSums(na.rm=T)) %>%
  rename_with(
    .fn = ~ paste0("M_", .),
    .cols = colnames(.)[!colnames(.) %in% c("Country.Name","ISO3")]
  ) %>%
  rename(Country = ISO3) %>%
  dplyr::select(-Country.Name)

macrofin <- normfuncpos(macrofin, 2.1, 0, "M_macrofin_risk")

household_risk <- macrofin %>%
  dplyr::select(Country, M_Household.risks) %>%
  mutate(M_Household.risks = case_when(
    M_Household.risks == 0.5 ~ 7,
    M_Household.risks == 1 ~ 10,
    TRUE ~ M_Household.risks
  )) %>%
  rename(S_Household.risks = M_Household.risks)

#----------------------------—WB PHONE SURVEYS-----------------------------------------------------
# phone_data <- read_excel("~/Google Drive/PhD/R code/Compound Risk/Restricted_Data/Phone_surveys_Mar.xlsx",
#                          sheet = "2. Harmonized Indicators")

drive_download("Restricted_Data/Phone_surveys_Mar.xlsx", path = "tmp-Phone_surveys.xlsx", overwrite = T, verbose = F)
phone_data <- read_excel("tmp-Phone_surveys.xlsx",
                         sheet = "2. Harmonized Indicators")
unlink("tmp-Phone_surveys.xlsx")

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

#------------------------------—IMF FORECASTED UNEMPLOYMENT-----------------------------------------
imf_un <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/imf_unemployment.csv")

imf_un <- imf_un %>%
  mutate_at(
    vars(contains("X2")),
    ~ as.numeric(as.character(.))
  ) %>%
  mutate(change_unemp_21 = X2021 - X2020,
         change_unemp_20 = X2020 - X2019) %>%
  rename(
    Countryname = Country,
    Country = ISO3
  ) %>%
  rename_with(
    .fn = ~ paste0("S_", .),
    .cols = -contains("Country")
  ) %>%
  dplyr::select(-Countryname) %>%
  filter(S_Subject.Descriptor == "Unemployment rate")

# Normalise values
imf_un <- normfuncpos(imf_un, 1, 0, "S_change_unemp_21")
imf_un <- normfuncpos(imf_un, 3, 0, "S_change_unemp_20")

# Max values for index
imf_un <- imf_un %>%
  mutate(
    S_change_unemp_norm = rowMaxs(as.matrix(dplyr::select(.,
                                    S_change_unemp_21_norm,
                                    S_change_unemp_20_norm)),
            na.rm = T),
    S_change_unemp_norm = case_when(is.infinite(S_change_unemp_norm) ~ NA_real_,
                               TRUE ~ S_change_unemp_norm)
  )

#--------------------------—Create Socioeconomic sheet -------------------------------------------
socioeconomic_sheet <- #left_join(countrylist, ocha, by = "Country") %>% # not current
  #dplyr::select(-Countryname) %>%
  left_join(countrylist, inform_data, by = "Country") %>%
  left_join(., socio_forward, by = "Country") %>%
  left_join(., mpo_data, by = "Country") %>%
  left_join(., imf_un, by = "Country") %>%
  left_join(., household_risk, by = "Country") %>%
  left_join(., phone_index_data, by = "Country") %>%
  arrange(Country)

write.csv(socioeconomic_sheet, "Risk_sheets/Socioeconomic_sheet.csv")
print("Socioeconomic sheet written")

#
##
### ********************************************************************************************
####    NATURAL HAZARD: CREATE NATURAL HAZARDS SHEET USING A RANGE OF SOURCE INDICATORS -----
### ********************************************************************************************
##
#

#-------------------------------—NATURAL HAZARDS SHEET------------------------------------------------
# Load UKMO dataset
# nathaz <- suppressMessages(read_csv("Indicator_dataset/naturalhazards.csv"))
# nathaz <- nathaz %>%
#   dplyr::select(-X1) %>%
#   rename(Country = NH_UKMO_Country)
# upperrisk <- quantile(nathaz$NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS, probs = c(0.95), na.rm = T)
# lowerrisk <- quantile(nathaz$NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS, probs = c(0.05), na.rm = T)
# nathaz <- normfuncpos(nathaz, upperrisk, lowerrisk, "NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS")
# upperrisk <- quantile(nathaz$NH_UKMO_TOTAL.RISK.NEXT.12.MONTHS, probs = c(0.95), na.rm = T)
# lowerrisk <- quantile(nathaz$NH_UKMO_TOTAL.RISK.NEXT.12.MONTHS, probs = c(0.05), na.rm = T)
# nathaz <- normfuncpos(nathaz, upperrisk, lowerrisk, "NH_UKMO_TOTAL.RISK.NEXT.12.MONTHS")

#------------------------------—Load GDACS database--------------------------------------------------
gdacweb <- "https://www.gdacs.org/"
gdac <- read_html(gdacweb)

names <- c(
  ".alert_EQ_Green", ".alert_EQ_PAST_Green", ".alert_EQ_Orange", ".alert_EQ_PAST_Orange",
  ".alert_TC_Green", ".alert_TC_PAST_Green", ".alert_TC_Orange", ".alert_TC_PAST_Orange",
  ".alert_FL_Green", ".alert_FL_PAST_Green", ".alert_FL_Orange", ".alert_FL_PAST_Orange",
  ".alert_VO_Green", ".alert_VO_PAST_Green", ".alert_VO_Orange", ".alert_VO_PAST_Orange",
  ".alert_DR_Green", ".alert_DR_PAST_Green", ".alert_DR_Orange", ".alert_DR_PAST_Orange"
)

# Function to create database with hazard specific information
haz <- lapply(names, function(i) {
  names <- gdac %>%
    html_nodes(i) %>%
    html_nodes(".alert_item_name, .alert_item_name_past") %>%
    html_text()

  mag <- gdac %>%
    html_nodes(i) %>%
    html_nodes(".magnitude, .magnitude_past") %>%
    html_text() %>%
    str_trim()

  date <- gdac %>%
    html_nodes(i) %>%
    html_nodes(".alert_date, .alert_date_past") %>%
    html_text() %>%
    str_trim()
  date <- gsub(c("-  "), "", date)
  date <- gsub(c("\r\n       "), "", date)

  cbind.data.frame(names, mag, date)
})

# Labels
try(haz[[1]]$status <- paste("active"), silent = T)
try(haz[[2]]$status <- paste("past"), silent = T)
try(haz[[3]]$status <- paste("active"), silent = T)
try(haz[[4]]$status <- paste("past"), silent = T)
try(haz[[5]]$status <- paste("active"), silent = T)
try(haz[[6]]$status <- paste("past"), silent = T)
try(haz[[7]]$status <- paste("active"), silent = T)
try(haz[[8]]$status <- paste("past"), silent = T)
try(haz[[9]]$status <- paste("active"), silent = T)
try(haz[[10]]$status <- paste("past"), silent = T)
try(haz[[11]]$status <- paste("active"), silent = T)
try(haz[[12]]$status <- paste("past"), silent = T)
try(haz[[13]]$status <- paste("active"), silent = T)
try(haz[[14]]$status <- paste("past"), silent = T)
try(haz[[15]]$status <- paste("active"), silent = T)
try(haz[[16]]$status <- paste("past"), silent = T)
try(haz[[17]]$status <- paste("active"), silent = T)
try(haz[[18]]$status <- paste("past"), silent = T)
try(haz[[19]]$status <- paste("active"), silent = T)
try(haz[[20]]$status <- paste("past"), silent = T)

# Earthquake
eq1 <- try(rbind(haz[[1]], haz[[2]]), silent = T)
try(eq1$haz <- paste("green"), silent = T)
eq2 <- try(rbind(haz[[3]], haz[[4]]), silent = T)
try(eq2$haz <- paste("orange"), silent = T)
eq <- try(rbind(eq1, eq2), silent = T)
eq$hazard <- "earthquake"

# Cyclone
cy1 <- try(rbind(haz[[5]], haz[[6]]), silent = T)
try(cy1$haz <- paste("green"), silent = T)
cy2 <- try(rbind(haz[[7]], haz[[8]]), silent = T)
try(cy2$haz <- paste("orange"), silent = T)
cy <- try(rbind(cy1, cy2), silent = T)
cy$hazard <- "cyclone"

# Flood
fl1 <- try(rbind(haz[[9]], haz[[10]]), silent = T)
try(fl1$haz <- paste("green"), silent = T)
fl2 <- try(rbind(haz[[11]], haz[[12]]), silent = T)
try(fl2$haz <- paste("orange"), silent = T)
fl <- try(rbind(fl1, fl2), silent = T)
fl$hazard <- "flood"

# Volcano
vo1 <- try(rbind(haz[[13]], haz[[14]]), silent = T)
try(vo1$haz <- paste("green"), silent = T)
vo2 <- try(rbind(haz[[15]], haz[[16]]), silent = T)
try(vo2$haz <- paste("orange"), silent = T)
vo <- try(rbind(vo1, vo2), silent = T)
vo$hazard <- "volcano"
vo$names <- sub(".*in ", "", vo$names)

# Drought
dr1 <- try(rbind(haz[[17]], haz[[18]]), silent = T)
dr1$haz <- try(paste("green"), silent = T)
dr2 <- try(rbind(haz[[19]], haz[[20]]), silent = T)
dr2$haz <- try(paste("orange"), silent = T)
dr <- try(rbind(dr1, dr2), silent = T)
dr$hazard <- "drought"
dr$date <- try(str_sub(dr$names, start = -4), silent = T)
dr$names <- try(gsub(".{5}$", "", dr$names), silent = T)

# Combine into one dataframe
gdaclist <- rbind.data.frame(eq, cy, fl, vo, dr)

# change times
gdaclist$date <- ifelse(gdaclist$hazard != c("drought") & gdaclist$status == "active", paste(as.Date(parse_date_time(gdaclist$date, orders = c("dm HM")))),
  ifelse(gdaclist$hazard == c("drought"), paste(gdaclist$date),
    paste(as.Date(parse_date_time(gdaclist$date, orders = c("dmy"))))
  )
)

# Remove duplicate countries for drought
gdaclist$names <- as.character(gdaclist$names)
add <- gdaclist[which(gdaclist$hazard == "drought" & grepl("-", gdaclist$names)), ]
gdaclist[which(gdaclist$hazard == "drought" & grepl("-", gdaclist$names)), ]$names <- sub("-.*", "", gdaclist[which(gdaclist$hazard == "drought" & grepl("-", gdaclist$names)), ]$names)
add$names <- sub(".*-", "", add$names)
gdaclist <- rbind(gdaclist, add)

# Drought orange
gdaclist$status <- ifelse(gdaclist$hazard == "drought" & gdaclist$date == "2020", "active", gdaclist$status)

# Country names
gdaclist$namesiso <- suppressWarnings(countrycode(gdaclist$names, origin = "country.name", destination = "iso3c"))
gdaclist$namesfull <- suppressWarnings(countrycode(gdaclist$names, origin = "country.name", destination = "iso3c", nomatch = NULL))

# Create subset
gdac <- gdaclist %>%
  dplyr::select(date, status, haz, hazard, namesiso)

colnames(gdac) <- c("NH_GDAC_Date", "NH_GDAC_Hazard_Status", "NH_GDAC_Hazard_Severity", "NH_GDAC_Hazard_Type", "Country")

gdac <- gdac %>%
  mutate(NH_GDAC_Hazard_Score_Norm = case_when(
    NH_GDAC_Hazard_Status == "active" & NH_GDAC_Hazard_Severity == "orange" ~ 10,
    TRUE ~ 0
  )) %>%
  drop_na(Country)

write.csv(gdac, "Indicator_dataset/gdaclistnormalised.csv")

# #--—(ThinkHazard!)--
# # Find countries in the ThinkHazard database
# country <- read.csv("https://raw.githubusercontent.com/GFDRR/thinkhazardmethods/master/source/download/ADM0_TH.csv")
# country <- as.data.frame(country)
# country <- suppressWarnings(country[!is.na(as.numeric(gsub("[^0-9-]","", country[[1]]))),])
# country <- as.data.frame(country)
# 
# # Assign country to list
# country$list <- as.numeric(gsub("[^0-9-]","", country[[1]]))
# country$countrycode <- suppressWarnings(countrycode(
#   country$country,
#   origin = "country.name",
#   destination = "iso3c",
#   nomatch = NA
# ))
# 
# #Remove non-countries
# country <- country %>%
#   filter(!is.na(countrycode))
# 
# #Remove New Zealand duplicate
# country <- country %>% filter(countrycode != "179;New Zealand;;")
# 
# # Extract API data on ThinkHazard! (can be slow)
# think_data <- lapply(country$list[1:236], function(tt) {
#   dat <- fromJSON(paste0("http://thinkhazard.org/en/report/", tt, ".json"))
# })
# 
# # Compile by country
# think_join <- lapply(1:length(think_data), function(yy){
#   frame <- as.data.frame(think_data[yy])
#   frame$Country <- country$countrycode[yy]
#   do.call(data.frame, frame)
# })
# 
# # Join list to a single dataframe
# think_hazard <- do.call("rbind", think_join)
# 
# # Assign numberic values and calculate geometric mean
# think_hazard <- think_hazard %>%
#   mutate(hazard_num = case_when(hazardlevel.title == "High" ~ 4,
#                                 hazardlevel.title == "Medium" ~ 3,
#                                 hazardlevel.title == "Low" ~ 2,
#                                 hazardlevel.title == "Very low" ~ 1,
#                                 TRUE ~ NA_real_
#   )) %>%
#   group_by(Country) %>%
#   mutate(multihazard_risk = geoMean(hazard_num, na.rm = T)) %>%
#   ungroup %>%
#   rename_with(.fn = ~ paste0("NH_", .), 
#               .cols = -contains("Country")
#               ) %>%
#   distinct(Country, NH_multihazard_risk)
# 
# # Normalise values
# think_hazard <- normfuncpos(think_hazard, 4, 0, "NH_multihazard_risk")
# 
# # Save file
# write.csv(think_hazard, "Indicator_dataset/think_hazard.csv")

#----------------------—INFORM Natural Hazard and Exposure rating--------------------------

inform_2021 <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/INFORM_2021.csv")

# Rename country
informnathaz <- inform_2021 %>%
  dplyr::select(Country, Natural) %>%
  rename(NH_Hazard_Score = Natural) %>%
  drop_na(Country, NH_Hazard_Score)

# Normalise scores
informnathaz <- normfuncpos(informnathaz, 7, 1, "NH_Hazard_Score")

#—(UKMO La Nina)
# La_nina_data <- read.csv("Indicator_dataset/La_nina.csv", stringsAsFactors=FALSE)
# La_nina_data <- as_tibble(La_nina_data)
# 
# la_nina <- La_nina_data %>%
#   mutate(
#     risk = case_when(
#       risk == 3 ~ 10,
#       risk == 2 ~ 9, 
#       risk == 1 ~ 7,
#       TRUE ~ NA_real_
#     ),
#     Country = countrycode(
#       Country,
#       origin = "country.name",
#       destination = "iso3c",
#       nomatch = NULL
#     )) %>%
#   rename(NH_la_nina_risk = risk)

#---------------------------------- —IRI Seasonal Forecast ------------------------------------------
# Load from Github
seasonl_risk <- suppressWarnings(read_csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/seasonal_risk_list", col_types = cols()))
seasonl_risk <- seasonl_risk %>%
  dplyr::select(-X1) %>%
  rename(
    Country = "ISO3",
    NH_seasonal_risk_norm = risklevel
  )

#-------------------------------------—Locust outbreaks----------------------------------------------
# List of countries and risk factors associated with locusts (FAO), see:http://www.fao.org/ag/locusts/en/info/info/index.html

# high <- c("ETH", "KEN", "SOM", "SAU")#SUD", "ERI", "YEM", "SAU")
# med <- c()#"TZA", "UGA", "SSD", "DJI")
# low <- c("ERI", "IRN", "SSD", "SDN", "TZA", "UGA", "YEM", "DJI")

# Merge with countrylist
# countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")
# locust_risk <- countrylist %>%
#   dplyr::select(-X, -Countryname) %>%
#   mutate(
#     NH_locust_norm = case_when(
#     Country %in% high ~ 10,
#     Country %in% med ~ 7,
#     Country %in% low ~ 3,
#    TRUE ~ 0
#  ))

locust_risk <- suppressMessages(read_csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/locust_risk.csv", col_types = cols()))
locust_risk <- locust_risk %>%
  dplyr::select(-X1)
#---------------------------------—Natural Hazard ACAPS---------------------------------
acaps_nh <- acapssheet[,c("Country", "NH_natural_acaps")]

#-------------------------------------------—Create combined Natural Hazard sheet------------------------------
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")
countrylist <- countrylist %>%
  dplyr::select(-X)

nathazard_sheet <- left_join(countrylist, gdac, by = "Country") %>%
  left_join(., informnathaz, by = "Country") %>%
  left_join(., seasonl_risk, by = "Country") %>%
  left_join(., locust_risk, by = "Country") %>%
  left_join(., acaps_nh, by = "Country") %>%
  distinct(Country, .keep_all = TRUE) %>%
  drop_na(Country) %>%
  arrange(Country)

write.csv(nathazard_sheet, "Risk_sheets/Naturalhazards.csv")
print("Natural hazard sheet written")
#
##
### ********************************************************************************************
####    FRAGILITY: CREATE FRAGILITY  SHEET USING A RANGE OF SOURCE INDICATORS ----
### ********************************************************************************************
##
#


#-------------------------—FCS---------------------------------------------

fcv <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/Country_classification.csv") %>%
  dplyr::select(-X, Countryname, -IDA.status) %>%
  mutate(
    FCV_normalised = case_when(
      FCV_status == "High-Intensity conflict" ~ 10,
      FCV_status == "Medium-Intensity conflict" ~ 10,
      FCV_status == "High-Institutional and Social Fragility" ~ 10,
      TRUE ~ 0
    )
  )

#-----------------------------—IDPs--------------------------------------------------------
idp_data <- read_csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/population.csv",
                     col_types = cols(
                       `IDPs of concern to UNHCR` = col_number(),
                       `Refugees under UNHCR’s mandate` = col_number(),
                       Year = col_number()
                     ), skip = 14
)

# Calculate metrics
idp <- idp_data %>%
  group_by(`Country of origin (ISO)`, Year) %>%
  summarise(
    refugees = sum(`Refugees under UNHCR’s mandate`, na.rm = T),
    idps = sum(`IDPs of concern to UNHCR`, na.rm = T)
  ) %>%
  group_by(`Country of origin (ISO)`) %>%
  mutate(
    sd_refugees = sd(refugees, na.rm = T),
    mean_refugees = mean(refugees, na.rm = T),
    z_refugees = (refugees - mean_refugees) / sd(refugees),
    refugees_fragile = case_when(
      z_refugees > 1 ~ "Fragile",
      z_refugees < 1 ~ "Not Fragile",
      z_refugees == NaN ~ "Not Fragile",
      TRUE ~ NA_character_
    ),
    mean_idps = mean(idps, na.rm = T),
    z_idps = case_when(
      sd(idps) != 0 ~ (idps - mean_idps) / sd(idps),
      sd(idps) == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    idps_fragile = case_when(
      z_idps > 1 ~ "Fragile",
      z_idps < 1 ~ "Not Fragile",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(Year == 2019) %>%
  dplyr::select(`Country of origin (ISO)`, refugees, z_refugees, refugees_fragile, idps, z_idps, idps_fragile)

# Normalise scores
idp <- normfuncpos(idp, 1, 0, "z_refugees")
idp <- normfuncpos(idp, 1, 0, "z_idps")

# Correct for countries with 0
idp <- idp %>%
  mutate(
    z_refugees_norm = case_when(
      z_refugees == NaN ~ 0,
      TRUE ~ z_refugees_norm
    ),
    z_idps_norm = case_when(
      z_idps == NaN ~ 0,
      TRUE ~ z_idps_norm
    ),
    Country = countrycode(`Country of origin (ISO)`,
                          origin = "country.name",
                          destination = "iso3c",
                          nomatch = NULL
    )
  ) %>%
  dplyr::select(-`Country of origin (ISO)`)

#-------------------------—ACLED data---------------------------------------------
# Select date as three years plus two month (date to retrieve ACLED data)
three_year <- as.Date(as.yearmon(Sys.Date() - 45) - 3.2)

# Get ACLED API URL
acled_url <- paste0("https://api.acleddata.com/acled/read/?key=*9t-89Rn*bDb4qFXBAmO&email=ljones12@worldbank.org&event_date=",
       three_year,
       "&event_date_where=>&fields=iso3|fatalities|event_date&limit=0")

# #Get ACLED API URL
# acled_url <- paste0("https://api.acleddata.com/acled/read/?key=buJ7jaXjo71EBBB!!PmJ&email=bnotkin@worldbank.org&event_date=",
#                     three_year,
#                     "&event_date_where=>&fields=iso3|fatalities|event_date&limit=0")

# Retrieve information
acled_data <- fromJSON(acled_url)

# Progress conflict data
acled <- acled_data$data %>%
  mutate(
    fatalities = as.numeric(as.character(fatalities)),
    date = as.Date(event_date),
    month_yr = as.yearmon(date)
  ) %>%
  # Remove dates for the latest month (or month that falls under the prior 6 weeks)
  filter(date <= as.Date(as.yearmon(Sys.Date() - 45))) %>% 
  group_by(iso3, month_yr) %>%
  summarise(fatal_month = sum(fatalities, na.rm = T),
            fatal_month_log = log(fatal_month + 1)) %>%
  mutate(fatal_3_month = fatal_month + lag(fatal_month, na.rm= T) + lag(fatal_month, 2, na.rm= T),
         fatal_3_month_log = log(fatal_3_month + 1)) %>%
  group_by(iso3) %>%
  mutate(
    fatal_z = (fatal_3_month_log - mean(fatal_3_month_log, na.rm = T)) / sd(fatal_3_month_log, na.rm = T),
    sd = sd(fatal_3_month_log, na.rm = T),
    mean = mean(fatal_3_month_log, na.rm = T)
  ) %>%
  #Calculate month year based on present month (minus 6 weeks)
  filter(month_yr == paste(month.abb[month(format(Sys.Date() - 45))], year(format(Sys.Date() - 45)))) 

# To calculate daily lagged sums
#acled <- acled_data$data %>%
#  mutate(
#    fatalities = as.numeric(as.character(fatalities)),
#    date = as.Date(event_date),
#    month_yr = as.yearmon(date)
#  ) %>%
#  group_by(iso3, date) %>%
#  summarise(fatal_day = sum(fatalities, na.rm = T)) %>% 
#  group_by(iso3) %>%
#  mutate(previous_comments=lag(cumsum(fatal_day),k=90, default=0))

#

# Progress conflict data
# acled <- acled_data$data %>%
#  mutate(
#    fatalities = as.numeric(as.character(fatalities)),
#    date = as.Date(event_date),
#    month_yr = as.yearmon(date)
#  ) %>%
#  filter(date <= as.Date(as.yearmon(Sys.Date() - 45))) %>% 
#  group_by(iso3, month_yr) %>%
#  # Remove dates for the latest month (or month that falls under the prior 6 weeks)
#  summarise(fatal_month = sum(fatalities, na.rm = T),
#            fatal_month_log = log(fatal_month + 1)) %>%
#  mutate(fatal_3_month = (fatal_month + lag(fatal_month, na.rm= T) + lag(fatal_month, 2, na.rm= T))/3,
#         fatal_3_month_log = log(fatal_3_month + 1)) %>%
#  group_by(iso3) %>%
#  mutate(
#    fatal_z = (fatal_3_month_log - mean(fatal_3_month_log, na.rm = T)) / sd(fatal_3_month_log, na.rm = T),
#    sd = sd(fatal_3_month_log, na.rm = T),
#    mean = mean(fatal_3_month_log, na.rm = T)
#  ) 
#tt <- acled %>%
#  group_by(iso3) %>%
#  mutate(av_month = (fatal_month + lag(fatal_month, na.rm= T) + lead(fatal_month))/3,
#         av_month = log(av_month + 1),
#         mean_av = mean(av_month, na.rm = T),
#         sc_av = sd(av_month, na.rm = T)
#  ) %>%
#  group_by(iso3) %>%
#  mutate( 
#    z_av = (fatal_3_month_log - mean_av) / sc_av)

# Normalise scores
acled <- normfuncpos(acled, 1, -1, "fatal_z")

# Correct for countries with 0
acled <- acled %>%
  mutate(
    fatal_z_norm = case_when(
      is.nan(fatal_z) ~ 0,
      TRUE ~ fatal_z_norm
    ),
    Country = countrycode(
      iso3,
      origin = "country.name",
      destination = "iso3c",
      nomatch = NULL
    ),
    fatal_z_norm = case_when(
      fatal_3_month_log == 0 ~ 0,
      (fatal_3_month_log <= log(5 + 1) & fatal_3_month_log != 0 ) & fatal_z <= 1 ~ 0,
      (fatal_3_month_log <= log(5 + 1) & fatal_3_month_log != 0 ) & fatal_z >= 1 ~ 5,
      TRUE ~ fatal_z_norm
    )
  ) %>%
  ungroup() %>%
  dplyr::select(-iso3)

#--------------------------—REIGN--------------------------------------------
reign_data <- suppressMessages(read_csv("https://cdn.rawgit.com/OEFDataScience/REIGN.github.io/gh-pages/data_sets/REIGN_2021_4.csv", col_types = cols()))

reign_start <- reign_data %>%
  filter(year == max(year, na.rm= T)) %>%
  group_by(country) %>%
  slice(which.max(month)) %>%
  dplyr::select(country, month, pt_suc, pt_attempt, delayed, irreg_lead_ant, anticipation) %>%
  mutate(
    country = countrycode(country,
                          origin = "country.name",
                          destination = "iso3c",
                          nomatch = NULL
    )) %>%
  rename(Country = country)

# Add FSI/BRD threshold
reign <- left_join(reign_start, fcv %>% dplyr::select(Country, FCV_normalised), by = "Country") %>%
  mutate(
    irreg_lead_ant = case_when(
      FCV_normalised == 10 ~ irreg_lead_ant,
      TRUE ~ 0
    ),
    delayed_adj = case_when(
      FCV_normalised == 10 ~ delayed,
      TRUE ~ 0
    ),
    anticipation_adj = case_when(
      FCV_normalised == 10 ~ anticipation,
      TRUE ~ 0
    ),
    pol_trigger = case_when(
      pt_suc + pt_attempt + delayed_adj + irreg_lead_ant + anticipation_adj >= 1 ~ "Fragile",
      TRUE ~ "Not Fragile"
    ),
    pol_trigger_norm = case_when(
      pt_suc + pt_attempt + delayed_adj + irreg_lead_ant + anticipation_adj >= 1 ~ 10,
      TRUE ~ 0
    )
  ) %>%
  dplyr::select(-FCV_normalised)

#-----------------—Join all dataset-----------------------------------
conflict_dataset_raw <- left_join(fcv, reign, by = "Country") %>%
  left_join(., idp, by = "Country") %>%
  left_join(., acled, by = "Country") %>%
  dplyr::select(Countryname, FCV_normalised, pol_trigger_norm, z_idps_norm, fatal_z_norm) 

conflict_dataset <- conflict_dataset_raw %>%
  mutate(
    flag_count = as.numeric(unlist(row_count(
      .,
      pol_trigger_norm:fatal_z_norm,
      count = 10,
      append = F
    ))),
    fragile_1_flag = case_when(
      flag_count >= 1 ~ 10,
      TRUE ~ suppressWarnings(apply(conflict_dataset_raw %>% dplyr::select(pol_trigger_norm:fatal_z_norm), 
                   1,
                   FUN = max,
                   na.rm = T)
    )),
    fragile_1_flag = case_when(
      fragile_1_flag == -Inf ~ NA_real_,
      TRUE ~ fragile_1_flag
    )) %>%
  rename(FCS_Normalised = FCV_normalised, REIGN_Normalised = pol_trigger_norm,
         Displaced_UNHCR_Normalised = z_idps_norm, BRD_Normalised = fatal_z_norm,
         Number_of_High_Risk_Flags = flag_count, Overall_Conflict_Risk_Score = fragile_1_flag) 

#-------------------------------------—Create Fragility sheet--------------------------------------
# Compile joint database
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")

fragility_sheet <- left_join(countrylist, conflict_dataset, by = "Countryname") %>%
  dplyr::select(-X, -Number_of_High_Risk_Flags) %>%
  rename_with(
    .fn = ~ paste0("Fr_", .), 
    .cols = colnames(.)[!colnames(.) %in% c("Country", "Countryname") ]
  )

write.csv(fragility_sheet, "Risk_sheets/fragilitysheet.csv")
print("Fragility sheet written")

#
##
### ********************************************************************************************
####    WRITE MINIMAL DIMENSION SHEETS ----
### ********************************************************************************************
##
#

# Saves .csvs with only used indicators (normalized, not raw) to crm_excel folder

countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Indicator_dataset/countrylist.csv")
countrylist <- countrylist %>%
  dplyr::select(-X) %>% 
  arrange(Country)

# Load dimension data from github
# healthsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/healthsheet.csv")
# foodsecurity <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/foodsecuritysheet.csv")
# debtsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/debtsheet.csv")
# fragilitysheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/fragilitysheet.csv")
# macrosheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/macrosheet.csv")
# Naturalhazardsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/Naturalhazards.csv")
# Socioeconomic_sheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/Socioeconomic_sheet.csv")
# acapssheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/acapssheet.csv")

# healthsheet <- read.csv("Risk_sheets/healthsheet.csv")
# foodsecurity <- read.csv("Risk_sheets/foodsecuritysheet.csv")
# # debtsheet <- read.csv("Risk_sheets/debtsheet.csv")
# fragilitysheet <- read.csv("Risk_sheets/fragilitysheet.csv")
# macrosheet <- read.csv("Risk_sheets/macrosheet.csv")
# Naturalhazardsheet <- read.csv("Risk_sheets/Naturalhazards.csv")
# Socioeconomic_sheet <- read.csv("Risk_sheets/Socioeconomic_sheet.csv")

# Load dimension / data dictionary
indicators <- as.data.frame(read.csv("indicators.csv"))

# Compile list of all dimension data for Map function
# List names should match dimension names in `indicator` data frame
unique(indicators$Dimension)
sheetList <- list("Health" = health_sheet,
                  "Food Security" = food_sheet,
                  "Conflict and Fragility" = fragility_sheet, # Technical note uses various names; what do we want to use? Fragility and Conflict, Conflict and Fragility, Conflict Fragility and Institutional Risk
                  "Macro Fiscal" = macro_sheet,
                  "Natural Hazard" = nathazard_sheet,
                  "Socioeconomic Vulnerability" = socioeconomic_sheet
)

# —Write function to apply to each sheet ----
writeSourceCSV <- function(i) {
  # headerOffset <- 2 # current output has two header rows # VARIABLE
  
  # Define sheet and dimension name
  sheet <- sheetList[[i]]
  dimension <- names(sheetList)[i]
  
  sheet <- sheet[,c(which(colnames(sheet) == "Country"),which(colnames(sheet) %in% indicators$indicator_slug))]
  
  # columns <- colnames(sheet) %>%
  #   as.data.frame() %>%
  #   left_join(indicators, by = c(. = "indicator_slug"))
  # # Other option: use add_row() to add names as first row; would change classes
  # colnames(sheet) <- coalesce(columns$Indicator, columns$.) %>% 
  #   { gsub('^..?_', '', colnames(sheet)) } %>%
  #   { gsub('_', ' ', .) } #%>%
  #   # { gsub("\\.", " – ", .)} %>%
  #   # str_to_title()
  # # print(colnames(sheet))
  
  # Sort dataframes by country code to match, and check if length matches country list
  # if(nrow(sheet) != nrow(countrylist)) warning(paste(dimension, " sheet doesn't match number of countrylist rows"))
  sheet <- sheet[!duplicated(sheet$Country),]
  sheet <- arrange(sheet, Country)
  
  write_csv(sheet, paste0("crm-excel/", dimension, ".csv"))
  
  # Check if any sheets have empty columns
  empties <- empty_cols(sheet) %>% names()
  if(length(empties) > 0) {paste(dimension, " is empty on ", empties) } else {paste(dimension, " is filled") }
}

# —Run -----
Map(writeSourceCSV, 1:length(sheetList))
}
