######################################################################################################
#
#  CODE USED TO TO PRODUCE INDIVIDUAL INDICATOR DATASETS AND RISK COMPONENT SHEETS
#
######################################################################################################

#--------------------LOAD PACKAGES-----------------------------------------
# install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(
  cowplot, lubridate, rvest, viridis, countrycode,
  clipr, awalker89 / openxlsx, dplyr, tidyverse, readxl,
  gsheet, zoo, wppExplorer, haven, EnvStats, jsonlite
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

#
##
### ********************************************************************************************
####    CREATE HEALTH SHEET USING A RANGE OF SOURCE INDICATORS
### ********************************************************************************************
##
#

#--------------------HIS Score-----------------
HIS <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/HIS.csv")

HIS <- HIS %>%
  rename(Country = H_Country) %>%
  select(-X)

#Normalise scores
HIS <- normfuncneg(HIS, 20, 70, "H_HIS_Score")

#-----------------------Oxford rollback Score-----------------
OXrollback <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-scratchpad/master/rollback_checklist/rollback_checklist.csv")

colnames(OXrollback) <- paste0("H_", colnames(OXrollback))

OXrollback <- OXrollback %>%
  rename(
    H_Oxrollback_score = H_overall_checklist,
    Countryname = H_countryname
  ) %>%
  mutate(
    Country = countrycode(Countryname,
    origin = "country.name",
    destination = "iso3c",
    nomatch = NULL
  ))

upperrisk <- quantile(OXrollback$H_Oxrollback_score, probs = c(0.9), na.rm = T)
lowerrisk <- quantile(OXrollback$H_Oxrollback_score, probs = c(0.1), na.rm = T)

OXrollback <- normfuncpos(OXrollback, upperrisk, lowerrisk, "H_Oxrollback_score")

#------------------------COVID deaths and cases--------------------------
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
    meancase = mean(new_cases_per_million, na.rm = T)
  )

covidgrowth <- covidgrowth %>%
  group_by(iso_code) %>%
  filter(!is.na(meandeaths) & !is.na(meancase)) %>%
  filter(
    iso_code %in% 
      as.data.frame(covidgrowth %>% 
                      count(iso_code) %>% 
                      filter(n == 2) %>% 
                      select(iso_code))$iso_code
  ) %>%
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
  select(-previous2week, -growthcase, -growthdeath, -meandeaths, -meancase)

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
  select(iso_code, new_cases_smoothed_per_million, new_deaths_smoothed_per_million) %>%
  rename(Country = iso_code)

covidcurrent <- normfuncpos(covidcurrent, 50, 0, "new_cases_smoothed_per_million")
covidcurrent <- normfuncpos(covidcurrent, 2, 0, "new_deaths_smoothed_per_million")

# Rename columns
colnames(covidcurrent) <- c(
  "Country", "H_new_cases_smoothed_per_million", "H_new_deaths_smoothed_per_million",
  "H_new_cases_smoothed_per_million_norm", "H_new_deaths_smoothed_per_million_norm"
)

#-------------------Alternative COVID deaths---------------------------------
# Load COVID data
cov <- read.csv("https://raw.githubusercontent.com/scc-usc/ReCOVER-COVID-19/master/results/forecasts/global_deaths_current_0.csv")
cov_current <- read.csv("https://raw.githubusercontent.com/scc-usc/ReCOVER-COVID-19/master/results/forecasts/global_deaths.csv")

# Summarise country totals (forecast)
cov_dat <- cov %>%
  select(Country, colnames(cov)[10],colnames(cov)[9]) %>%
  rename(
    w8forecast = colnames(cov)[10], 
    w7forecast = colnames(cov)[9]
    ) %>%
  mutate(Country = suppressWarnings(countrycode(Country, 
                                                origin = "country.name",
                                                destination = "iso3c"
                                                )
         )) %>%
  drop_na(Country)

# Summarise country totals (current)
cov_cur <- cov_current %>%
  select(Country, last(colnames(cov_current))) %>%
  rename(
    current = last(colnames(cov_current)),
    ) %>%
  mutate(
    Country = suppressWarnings(countrycode(Country, 
                                                origin = "country.name",
                                                destination = "iso3c"
                                           )
      )) %>%
  drop_na(Country)

# Add population
pop <- wpp.by.year(wpp.indicator("tpop"), 2020)

pop$charcode <- suppressWarnings(countrycode(pop$charcode, 
                                             origin = "iso2c", 
                                             destination = "iso3c"
                                             )
                                 )

colnames(pop) <- c("Country", "Population")

# Join datasets
cov_forcast_alt <- left_join(cov_dat, pop, by = "Country", keep = F) %>%
  left_join(., cov_cur) %>%
  drop_na(Country) %>%
  mutate(
    week_increase = w8forecast - w7forecast,
    new_death_per_m = week_increase / (Population / 1000),
    add_death_prec_current = ((w8forecast / current) * 100) - 100
    ) %>%
  rename_with(.fn = ~ paste0("H_", .), 
              .cols = colnames(.)[-1]
              )

# Normalise
cov_forcast_alt <- normfuncpos(cov_forcast_alt, 100, 0, "H_add_death_prec_current")

#--------------------------Oxford Response Tracker----------------------------
Oxres <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv")

#Select latest data
Ox_cov_resp <- Oxres %>%
  group_by(CountryCode) %>%
  filter(Date == max(Date)) %>%
  select(
    CountryCode, Date, GovernmentResponseIndex, GovernmentResponseIndexForDisplay,
    EconomicSupportIndex, EconomicSupportIndexForDisplay, ContainmentHealthIndex,
    ContainmentHealthIndexForDisplay, E1_Income.support, E1_Flag
  )

colnames(Ox_cov_resp) <- c("Country", paste0("H_", colnames(Ox_cov_resp[,-1])))

#Create normalised scores
Ox_cov_resp <- normfuncneg(Ox_cov_resp, 15, 80, "H_GovernmentResponseIndexForDisplay")
Ox_cov_resp <- normfuncneg(Ox_cov_resp, 0, 100, "H_EconomicSupportIndexForDisplay")

#------------------------------INFORM COVID------------------------------------------------------
inform_cov <- read_html("https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Covid-19/INFORM-Covid-19-Warning-beta-version")

all_dat <- lapply(2:24, function(tt){
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
  select(-contains(".cname")) %>%
  mutate_at(
    vars(contains(".Rating")),
    funs(case_when(
      . == "background:#FF0000;" ~ "High",
      . == "background:#FFD800;" ~ "Medium",
      . == "background:#00FF00;" ~ "Low",
      TRUE ~ NA_character_
    ))
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
  select(-Countryname) %>%
  rename_with(
    .fn = ~ paste0("H_", .), 
    .cols = colnames(.)[!colnames(.) %in% c("Country", "Countryname") ]
  )

inform_covid_warning <- normfuncpos(inform_covid_warning, 6, 2, "H_INFORM_rating.Value")

write.csv(inform_covid_warning, "Indicator_dataset/inform_covid_warning.csv")

#----------------------------------Create combined Health Sheet-------------------------------------------
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")
countrylist <- countrylist %>%
  select(-X)

health <- left_join(countrylist, HIS, by = "Country") %>%
  left_join(., OXrollback, by = c("Country", "Countryname")) %>%
  left_join(., covidgrowth, by = "Country") %>%
  left_join(., covidcurrent, by = "Country") %>%
  left_join(., Ox_cov_resp, by = "Country") %>%
  left_join(., cov_forcast_alt, by = "Country") %>%
  left_join(., inform_covid_warning, by = "Country", "Countryname") %>%
  arrange(Country)

write.csv(health, "Risk_sheets/healthsheet.csv")

#
##
### ********************************************************************************************
####    CREATE FOOD SECURITY SHEET USING A RANGE OF SOURCE INDICATORS
### ********************************************************************************************
##
#

#---------------------------------LOAD FOOD SECURITY DATA---------------------------
# Proteus Index
proteus <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/proteus.csv")

proteus <- proteus %>%
  rename(F_Proteus_Score = Proteus.index) %>%
  select(-X) %>%
  mutate(
    Country = countrycode(Country,
    origin = "country.name",
    destination = "iso3c",
    nomatch = NULL
  ))

upperrisk <- quantile(proteus$F_Proteus_Score, probs = c(0.90), na.rm = T)
lowerrisk <- quantile(proteus$F_Proteus_Score, probs = c(0.10), na.rm = T)
proteus <- normfuncpos(proteus, upperrisk, lowerrisk, "F_Proteus_Score")

# Artemis
artemis <- read.csv("~/Google Drive/PhD/R code/Compound Risk/artemis.csv")

upperrisk <- 0.2
lowerrisk <- 0
artemis <- normfuncpos(artemis, upperrisk, lowerrisk, "F_Artemis_Score")

artemis <- artemis %>%
  mutate(
    Country = countrycode(Country,
                          origin = "country.name",
                          destination = "iso3c",
                          nomatch = NULL
      )
    ) %>%
  select(-X)

#------------------FEWSNET (with CRW threshold)-------------------------------
#Load database
fewswb <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/FEWS_raw.csv")

#Calculate country totals
fewsg <- fewswb %>%
  select(-X) %>%
  group_by(country, year_month) %>%
  mutate(countrypop = sum(pop)) %>%
  ungroup()

#Caluclate proportion and number of people in IPC class 
fewspop <- fewsg %>%
  group_by(country, year_month) %>%
  mutate(countryproportion = (pop / countrypop) * 100,
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
                                     TRUE ~ NA_real_))

#Functions to calculate absolute and geometric growth rates
pctabs <- function(x) x-lag(x)
pctperc <- function(x) x-lag(x)/lag(x)

#Summarise country totals per in last round of FEWS
fewssum <- fewspop %>%
  filter(year_month == "2020_06" | year_month == "2020_02") %>%
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
  select(-ipc3plusabsfor, -ipc3pluspercfor, -ipc4plusabsfor, -ipc4pluspercfor, 
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
  select(-fews_ipc, -fews_ha, -fews_proj_near, -fews_proj_near_ha, -fews_proj_med, 
         -fews_proj_med_ha, -fews_ipc_adjusted, -fews_proj_med_adjusted, -countryproportion) %>%
  filter(year_month == "2020_06")

# Find max ipc for any region in the country
fews_summary <- fewsg %>%
  group_by(country) %>%
  filter(year == 2020 & max(month, na.rm = T)) %>%
  summarise(max_ipc = max(fews_proj_med_adjusted, na.rm = T))

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
  select(-country) %>%
  rename_with(
    .fn = ~ paste0("F_", .),
    .cols = colnames(.)[!colnames(.) %in% c("Country", "country")]
  )

colnames(fews_dataset[-1]) <- paste0("F_", colnames(fews_dataset[-1])) 

#--------------Alternative Food price volatility scopes -----------------------
fao_fpma <- read_html("http://www.fao.org/giews/food-prices/home/en/")

#Highlight countries that are undergoing high FPV  
fpv_alt_raw <- fao_fpma %>%
  html_nodes(".maplist-country") %>%
  html_text() %>% 
  as.tibble() %>%
  mutate(Country = countrycode(value,
                               origin = "country.name",
                               destination = "iso3c",
                               nomatch = NULL)
  )

#Compile dataframe with 10 for high risk countries
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")

fpv_alt <- countrylist %>%
  select(-X, -Countryname) %>%
  mutate(F_fpv_alt = case_when(Country %in% fpv_alt_raw$Country ~ 10,
                               TRUE ~ 0)
  )

#Write csv and save to github
write.csv(fpv_alt, "Indicator_Dataset/FPV_alternative.csv")

#------------------------AG FOOD OBSERVATORY------------------------------------
ag_ob_data <- read.csv("Indicator_dataset/Food_Inflation_crosstab.csv")

ag_ob_data <- ag_ob_data %>%
  mutate_at(
    vars(contains("19"), contains("20"), contains("21")),
    ~ as.numeric(as.character(gsub(",", ".", .)))
  )

ag_ob <- ag_ob_data %>%
  filter(X == "Inflation") %>%
  select(-Income.Level, -Indicator, -X) %>%
  group_by(Country) %>%
  summarise(
    Apr = Apr.20[which(!is.na(Apr.20))[1]],
    May = May.20[which(!is.na(May.20))[1]],
    June = Jun.20[which(!is.na(Jun.20))[1]] 
  ) %>%
  mutate(fpv = case_when(
    !is.na(June) ~ June,
    is.na(June) & !is.na(May) ~ May,
    is.na(June) & is.na(May) & !is.na(Apr) ~ Apr,
    TRUE ~ NA_real_
  ),
  fpv_rating = case_when(
    fpv < 0.02 ~ 1,
    fpv >= 0.02 & fpv < 0.05 ~ 5,
    fpv >= 0.05 & fpv < 0.30 ~ 7,
    fpv >= 0.30 ~ 10,
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

#-------------------------FAO/WFP HOTSPOTS----------------------------
fao_wfp <- read_csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/WFP%3AFAO_food.csv") %>%
  select(-X2)

fao_wfp <- fao_wfp %>%
  mutate(Country = countrycode(Country,
                               origin = "country.name",
                               destination = "iso3c",
                               nomatch = NULL
  ))

fao_wfp$F_fao_wfp_warning <- 10

#------------------------CREATE FOOD SECURITY SHEET--------------------
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")
countrylist <- countrylist %>%
  select(-X)

foodsecurity <- left_join(countrylist, proteus, by = "Country") %>%
  left_join(., fews_dataset, by = "Country") %>%
  #left_join(., fpv, by = "Country") %>%  # Reintroduce if FAO price site comes back online
  left_join(., fpv_alt, by = "Country") %>%
  left_join(., artemis, by = "Country") %>%
  left_join(., ag_ob, by = "Country") %>%
  left_join(., fao_wfp, by = "Country") %>%
  arrange(Country)

write.csv(foodsecurity, "Risk_sheets/foodsecuritysheet.csv")

#
##
### ********************************************************************************************
####    CREATE DEBT SHEET USING A RANGE OF SOURCE INDICATORS
### ********************************************************************************************
##
#

#---------------------------LOAD DEBT DATA----------------------------
# SCRAPE DEBT DATA
debtweb <- "https://www.worldbank.org/en/programs/debt-toolkit/dsa"
debt <- read_html(debtweb)

debttab <- debt %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

debttab <- as.data.frame(debttab)
colnames(debttab) <- debttab[1, ]
debttab <- debttab[-1, ]

debttab <- debttab %>%
  mutate(
    Country = gsub("[0-9]+", "", Country),
    Country = countrycode(Country,
      origin = "country.name",
      destination = "iso3c",
      nomatch = NULL
    )
  ) %>%
  filter(Country != c("TOTAL"))

colnames(debttab) <- c("Country", "D_WB_external_debt_distress", "D_overall_debt_distress", "D_debt_date")

debttab$D_WB_external_debt_distress_norm <- ifelse(debttab$D_WB_external_debt_distress == "In distress", 10,
  ifelse(debttab$D_WB_external_debt_distress == "High", 10,
    ifelse(debttab$D_WB_external_debt_distress == "Moderate", 7,
      ifelse(debttab$D_WB_external_debt_distress == "Low", 3, NA)
    )
  )
)

#----------------IMF Debt forecasts---------------------------------
imfdebt <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/imfdebt.csv")

imfdebt <- imfdebt %>%
  mutate(Country = countrycode(Country,
    origin = "country.name",
    destination = "iso3c",
    nomatch = NULL
  )) %>%
  select(-X)

names <- c(
  "D_IMF_debt2017", "D_IMF_debt2018", "D_IMF_debt2019",
  "D_IMF_debt2020", "D_IMF_debt2021", "D_IMF_debt2020.2019"
)

imfdebt[names] <- lapply(imfdebt[names], function(xx) {
  suppressWarnings(as.numeric(as.character(xx)))
})

imfdebt <- normfuncneg(imfdebt, -5, 0, "D_IMF_debt2020.2019")

#-------------------IGC database--------------------------
igc <- suppressWarnings(gsheet2tbl("https://docs.google.com/spreadsheets/d/1s46Oz_NtkyrowAracg9FTPYRdkP8GHRzQqM7w6Qa_2k/edit?ts=5e836726#gid=0"))

# Header as first row
names(igc) <- as.matrix(igc[1, ])
igc <- igc[-1, ]
igc[] <- lapply(igc, function(x) type.convert(as.character(x)))

# Name fiscal column
colnames(igc)[which(colnames(igc) == "Announced/ estimated fiscal support package\n") + 1] <- "fiscalgdp"
igc <- igc[-which(is.na(colnames(igc)))]

# Extract numbers before the % text
igc$fiscalgdpnum <- sub("\\%.*", "", igc$fiscalgdp)
igc$fiscalgdpnum <- as.numeric(as.character(igc$fiscalgdpnum))

# Normalised scores
upperrisk <- quantile(igc$fiscalgdpnum, probs = c(0.05), na.rm = T)
lowerrisk <- quantile(igc$fiscalgdpnum, probs = c(0.95), na.rm = T)
igc <- normfuncneg(igc, upperrisk, lowerrisk, "fiscalgdpnum")

# Add label tabs
colnames(igc) <- paste0("D_", colnames(igc))
igc <- as.data.frame(igc)
igc <- igc[-which(colnames(igc) == "D_Other reputable links")]

igc <- igc %>%
  rename(Country = D_iso3c) %>%
  select(Country, D_fiscalgdpnum, D_fiscalgdpnum_norm)
  
#------------------------------COVID Economic Stimulus Index-------------------------
# Load file
url <- "http://web.boun.edu.tr/elgin/CESI_13.xlsx" # Note: may need to check for more recent versions
destfile <- "Indicator_dataset/cesiraw.xlsx"
curl::curl_download(url, destfile)
cesi <- read_excel(destfile)

colnames(cesi) <- paste0("D_", colnames(cesi))
colnames(cesi) <- gsub("_1.*", "", colnames(cesi))
cesi <- cesi %>%
  mutate(Country = countrycode(
    D_Country,
    origin = "country.name",
    destination = "iso3c",
    nomatch = NULL
  )) %>%
  select(-D_Country, -D_Date)

# Perform PCA
cesipca <- prcomp(cesi %>% select(
  D_fiscal, D_ratecut, D_reserve_req,
  D_macrofin, D_othermonetary, D_bopgdp,
  D_otherbop
  ),
center = TRUE,
scale. = TRUE
)

# Assign CESI Index as the first two PCs
cesi$D_CESI_Index <- cesipca$x[, 1] + cesipca$x[, 2]

# Normalised scores
upperrisk <- quantile(cesi$D_CESI_Index, probs = c(0.1), na.rm = T)
lowerrisk <- quantile(cesi$D_CESI_Index, probs = c(0.95), na.rm = T)
cesi <- normfuncneg(cesi, upperrisk, lowerrisk, "D_CESI_Index")

#------------------Import Economic Stimulus Index from the Health Sheet---------------------
Ox_fiscal <- Ox_cov_resp %>%
  select(Country, H_EconomicSupportIndexForDisplay_norm) %>%
  rename(D_EconomicSupportIndexForDisplay_norm = H_EconomicSupportIndexForDisplay_norm)

#-----------------CPIA----------------------------------------------------
cpia_data <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/cpia.csv")

cpia_data <- cpia_data %>%
  rename(Country = ISO3,
         D_CPIA.scores = CPIA.scores) %>%
  mutate(
    D_CPIA.scores = case_when(
      D_CPIA.scores == 0 ~ NA_real_,
      TRUE ~ D_CPIA.scores
    )
  )

cpia <- normfuncneg(cpia_data, 2.9, 5, "D_CPIA.scores")

#-------------------------CREATE DEBT SHEET-----------------------------------
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")
countrylist <- countrylist %>%
  select(-X)

debtsheet <- left_join(countrylist, debttab, by = "Country") %>%
  left_join(., imfdebt, by = "Country") %>%
  left_join(., igc, by = "Country") %>%
  left_join(., Ox_fiscal, by = "Country") %>%
  left_join(., cesi, by = "Country") %>%
  left_join(., cpia, by = "Country") %>%
  arrange(Country)

write.csv(debtsheet, "Risk_sheets/debtsheet.csv")

#
##
### ********************************************************************************************
####    CREATE MACRO  SHEET USING A RANGE OF SOURCE INDICATORS
### ********************************************************************************************
##
#

#--------------------------MACRO DATA---------------------------------------
macro <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/macro.csv")
macro <- macro %>%
  mutate(
    M_Economic_Dependence_Score = rowMeans(select(., c(M_Fuel_Imports_perc, M_Food_Imports_perc, M_Travel_Tourism_perc)), na.rm = T),
    M_Financial_Resilience_Score = rowMeans(select(., c(M_Remittance_perc, M_Reserves, M_ODA_perc, M_Gsavings_perc)), na.rm = T)) %>%
  mutate(M_Economic_and_Financial_score = rowMeans(select(., c(M_Economic_Dependence_Score, M_Financial_Resilience_Score)), na.rm = T)) %>%
  select(-X)
upperrisk <- quantile(macro$M_Economic_and_Financial_score, probs = c(0.9), na.rm = T)
lowerrisk <- quantile(macro$M_Economic_and_Financial_score, probs = c(0.1), na.rm = T)
macro <- normfuncpos(macro, upperrisk, lowerrisk, "M_Economic_and_Financial_score")

#---------------------------GDP forecast-------------------------------
gdp <- suppressMessages(read_csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/gdp.csv"))
gdp <- gdp %>%
  select(-X1)
upperrisk <- quantile(gdp$M_GDP_WB_2019minus2020, probs = c(0.2), na.rm = T)
lowerrisk <- quantile(gdp$M_GDP_WB_2019minus2020, probs = c(0.95), na.rm = T)
gdp <- normfuncneg(gdp, upperrisk, lowerrisk, "M_GDP_WB_2019minus2020")
upperrisk <- quantile(gdp$M_GDP_IMF_2019minus2020, probs = c(0.2), na.rm = T)
lowerrisk <- quantile(gdp$M_GDP_IMF_2019minus2020, probs = c(0.95), na.rm = T)
gdp <- normfuncneg(gdp, upperrisk, lowerrisk, "M_GDP_IMF_2019minus2020")

#-------------------------MACRO FIN REVIEW---------------------------------------------
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
  mutate(macrofin_risk = select(., Spillover.risks.from.the.external.environment.outside.the.region:Household.risks) %>% rowSums(na.rm=T)) %>%
  rename_with(   
    .fn = ~ paste0("M_", .),
    .cols = colnames(.)[!colnames(.) %in% c("Country.Name","ISO3")]
  ) %>%
  rename(Country = ISO3) %>%
  select(-Country.Name)

macrofin <- normfuncpos(macrofin, 2.1, 0, "M_macrofin_risk")

#-----------------------------CREATE MACRO SHEET-----------------------------------------
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")
countrylist <- countrylist %>%
  select(-X)

macrosheet <- left_join(countrylist, macro, by = "Country") %>%
  left_join(., gdp, by = "Country") %>%
  left_join(., macrofin, by = "Country") %>%
  arrange(Country) 

write.csv(macrosheet, "Risk_sheets/macrosheet.csv")

#
##
### ********************************************************************************************
####    CREATE SOCIO-ECONOMIC SHEET USING A RANGE OF SOURCE INDICATORS
### ********************************************************************************************
##
#

#--------------------------------SOCIO-ECONOMIC DATA and SHEET------------------------------------------
# Load OCHA database
ocha <- suppressMessages(read_csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/OCHA_socioeconomic.csv"))
ocha <- ocha %>%
  select(-X1) %>%
  arrange(Country)

upperrisk <- quantile(ocha$S_OCHA_Covid.vulnerability.index, probs = c(0.95), na.rm = T)
lowerrisk <- quantile(ocha$S_OCHA_Covid.vulnerability.index, probs = c(0.05), na.rm = T)
ocha <- normfuncpos(ocha, upperrisk, lowerrisk, "S_OCHA_Covid.vulnerability.index")

#---------------------------Alternative socio-economic data (based on INFORM)----------------------------
inform_2021 <- suppressMessages(read_csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/INFORM_2021.csv"))

inform_data <- inform_2021 %>%
  select(Country, "Socio-Economic Vulnerability") %>%
  rename(S_INFORM_vul = "Socio-Economic Vulnerability")

inform_data <- normfuncpos(inform_data, 7, 0, "S_INFORM_vul")
inform_data <- normfuncpos(inform_data, 7, 0, "S_INFORM_vul")

#------------------------Forward-looking socio-economic variables from INFORM---------------------------
socio_forward <- inform_covid_warning %>%
  select(
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

#--------------------------Poverty projections----------------------------------------------------
mpo <- read_dta("~/Google Drive/PhD/R code/Compound Risk/global.dta")

mpo_data <- mpo %>%
  rename(Country = Code) %>%
  left_join(., pop, by= "Country") %>%
  mutate_at(
    vars(contains("y20")),
    funs( prop = . / Population)
  ) %>%
  mutate(
    pov_prop_19_20 = ((y2020_prop / y2019_prop) -1) * 100,
    pov_abs_19_20 = (y2020_prop - y2019_prop) * 100
  ) %>%
  filter(Label == "International poverty rate ($1.9 in 2011 PPP)") %>%
  rename_with(
    .fn = ~ paste0("S_", .),
    .cols = colnames(.)[!colnames(.) %in% c("Country")]
  ) 

mpo_data <- normfuncpos(mpo_data, 50, 0, "S_pov_prop_19_20")
mpo_data <- normfuncpos(mpo_data, 0.05, 0, "S_pov_abs_19_20")

#-----------------------------HOUSEHOLD HEATMAP FROM MACROFIN-------------------------------------
household_risk <- macrosheet %>%
  select(Country, M_Household.risks) %>%
  mutate(M_Household.risks = case_when(
    M_Household.risks == 0.5 ~ 7,
    M_Household.risks == 1 ~ 10,
    TRUE ~ M_Household.risks
  )) %>%
  rename(S_Household.risks = M_Household.risks)

#----------------------------WB PHONE SURVEYS-----------------------------------------------------
phone_data <- read_excel("~/Google Drive/PhD/R code/Compound Risk/formatted_data17 Nov 2020_internal.xlsx")

phone_compile <- phone_data %>%
  filter(Gender == "All" & `URBAN/RURAL` == "National" & INDUSTRY == "All" ) %>%
  mutate(survey_no = as.numeric(as.character(str_replace(wave, "WAVE", "")))) %>%
  group_by(code) %>%
  mutate(last_survey = max(survey_no, na.rm=T)) %>%
  ungroup() %>%
  filter(last_survey == survey_no) 
  
phone_data <- phone_compile %>%
  select(code,IndicatorDescription, indicator_val) %>%
  pivot_wider(names_from = IndicatorDescription, values_from = indicator_val  )

phone_index <-phone_data %>%
  select(
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
phone_index <- normfuncpos(phone_index, 70, 0, "% of respondents currently employed/working")
phone_index <- normfuncpos(phone_index, 50, 0, "% of respondents who have stopped working since COVID-19 outbreak" )
phone_index <- normfuncneg(phone_index, 80, 100, "% able to access [staple food item] in the past 7 days when needed? - any staple food" )
phone_index <- normfuncpos(phone_index, 70, 0, "% of HHs that saw reduced their remittances" )
phone_index <- normfuncpos(phone_index, 25, 0, "% of HHs not able to perform normal farming activities (crop, livestock, fishing)")
phone_index <- normfuncneg(phone_index, 50, 100, "% of HHs able to pay rent for the next month")
phone_index <- normfuncpos(phone_index, 25, 0,  "% of respondents who were not able to work as usual last week")
phone_index <- normfuncpos(phone_index, 50, 0,  "Experienced decrease in wage income (% HHs with wage income as a source of livelihood in the past 12 months)")
phone_index <- normfuncpos(phone_index, 50, 0,  "% of HHs that experienced change in total income - decrease")
phone_index <- normfuncpos(phone_index, 25, 0,  "% of HHs used money saved for emergencies to cover basic living expenses" )
phone_index <- normfuncneg(phone_index, 5, 80,  "% of respondents received government assistance when experiencing labor income/job loss")
phone_index <- normfuncpos(phone_index, 20, 0,  "% of HHs sold assets such as property during the pandemic in order to pay for basic living expenses"  )
phone_index <- normfuncpos(phone_index, 50, 0,  "In the last 30 days, you skipped a meal because there was not enough money or other resources for food?(%)"  )
phone_index <- normfuncpos(phone_index, 50, 0,  "In the last 30 days, your household worried about running out of food because of a lack of money or other resources?(%)")
           
# Calculate index
phone_index_data <- phone_index %>%
  mutate(
    phone_average_index = select(., contains("_norm")) %>% rowMeans(na.rm=T)  ) %>%
  rename(Country = code) %>%
  rename_with(
    .fn = ~ paste0("S_", .), 
    .cols = -contains("Country")
  )

phone_index_data <- normfuncpos(phone_index_data, 8, 2, "S_phone_average_index")

#------------------------------IMF FORECASTED UNEMPLOYMENT-----------------------------------------
imf_un <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/imf_unemployment.csv")

imf_un <- imf_un %>%
  mutate_at(
    vars(contains("X2")),
    ~ as.numeric(as.character(.))
  ) %>%
  mutate(change_unemp = X2020 - X2019) %>%
  rename(
    Countryname = Country,
    Country = ISO3
  ) %>%
  rename_with(
    .fn = ~ paste0("S_", .), 
    .cols = -contains("Country")
  ) %>%
  select(-Countryname)

imf_un <- normfuncpos(imf_un, 5, 0, "S_change_unemp")

#--------------------------Create Socio-economic sheet -------------------------------------------
socioeconomic_sheet <- left_join(countrylist, ocha, by = "Country") %>%
  select(-Countryname) %>%
  left_join(., inform_data, by = "Country") %>%
  left_join(., socio_forward, by = "Country") %>%
  left_join(., mpo_data, by = "Country") %>%
  left_join(., imf_un, by = "Country") %>%
  left_join(., household_risk, by = "Country") %>%
  left_join(., phone_index_data, by = "Country") %>%
  arrange(Country)

write.csv(socioeconomic_sheet, "Risk_sheets/Socioeconomic_sheet.csv")

#
##
### ********************************************************************************************
####    CREATE NATURAL HAZARDS SHEET USING A RANGE OF SOURCE INDICATORS
### ********************************************************************************************
##
#

#-------------------------------NATURAL HAZARDS SHEET------------------------------------------------
# Load UKMO dataset
nathaz <- suppressMessages(read_csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/naturalhazards.csv"))
nathaz <- nathaz %>%
  select(-X1) %>%
  rename(Country = NH_UKMO_Country)
upperrisk <- quantile(nathaz$NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS, probs = c(0.95), na.rm = T)
lowerrisk <- quantile(nathaz$NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS, probs = c(0.05), na.rm = T)
nathaz <- normfuncpos(nathaz, upperrisk, lowerrisk, "NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS")
upperrisk <- quantile(nathaz$NH_UKMO_TOTAL.RISK.NEXT.12.MONTHS, probs = c(0.95), na.rm = T)
lowerrisk <- quantile(nathaz$NH_UKMO_TOTAL.RISK.NEXT.12.MONTHS, probs = c(0.05), na.rm = T)
nathaz <- normfuncpos(nathaz, upperrisk, lowerrisk, "NH_UKMO_TOTAL.RISK.NEXT.12.MONTHS")

# Load GDACS database
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
haz[[1]]$status <- paste("active")
haz[[2]]$status <- paste("past")
haz[[3]]$status <- paste("active")
haz[[4]]$status <- paste("past")
haz[[5]]$status <- paste("active")
haz[[6]]$status <- paste("past")
haz[[7]]$status <- paste("active")
haz[[8]]$status <- paste("past")
haz[[9]]$status <- paste("active")
haz[[10]]$status <- paste("past")
haz[[11]]$status <- paste("active")
haz[[12]]$status <- paste("past")
haz[[13]]$status <- paste("active")
haz[[14]]$status <- paste("past")
haz[[15]]$status <- paste("active")
haz[[16]]$status <- paste("past")
haz[[17]]$status <- paste("active")
haz[[18]]$status <- paste("past")
haz[[19]]$status <- paste("active")
haz[[20]]$status <- paste("past")

# Earthquake
eq1 <- rbind(haz[[1]], haz[[2]])
eq1$haz <- paste("green")
eq2 <- rbind(haz[[3]], haz[[4]])
eq2$haz <- paste("orange")
eq <- rbind(eq1, eq2)
eq$hazard <- "earthquake"

# Cyclone
cy1 <- rbind(haz[[5]], haz[[6]])
cy1$haz <- paste("green")
cy2 <- rbind(haz[[7]], haz[[8]])
cy2$haz <- paste("orange")
cy <- rbind(cy1, cy2)
cy$hazard <- "cyclone"

# Flood
fl1 <- rbind(haz[[9]], haz[[10]])
fl1$haz <- paste("green")
fl2 <- rbind(haz[[11]], haz[[12]])
fl2$haz <- paste("orange")
fl <- rbind(fl1, fl2)
fl$hazard <- "flood"

# Volcano
vo1 <- rbind(haz[[13]], haz[[14]])
vo1$haz <- paste("green")
vo2 <- rbind(haz[[15]], haz[[16]])
vo2$haz <- paste("orange")
vo <- rbind(vo1, vo2)
vo$hazard <- "volcano"
vo$names <- sub(".*in ", "", vo$names)

# Drought
dr1 <- rbind(haz[[17]], haz[[18]])
dr1$haz <- paste("green")
dr2 <- rbind(haz[[19]], haz[[20]])
dr2$haz <- paste("orange")
dr <- rbind(dr1, dr2)
dr$hazard <- "drought"
dr$date <- str_sub(dr$names, start = -4)
dr$names <- gsub(".{5}$", "", dr$names)

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
gdaclist$namesiso <- countrycode(gdaclist$names, origin = "country.name", destination = "iso3c")
gdaclist$namesfull <- countrycode(gdaclist$names, origin = "country.name", destination = "iso3c", nomatch = NULL)

# Create subset
gdac <- gdaclist %>%
  select(date, status, haz, hazard, namesiso)

colnames(gdac) <- c("NH_GDAC_Date", "NH_GDAC_Hazard_Status", "NH_GDAC_Hazard_Severity", "NH_GDAC_Hazard_Type", "Country")

gdac <- gdac %>%
  mutate(NH_GDAC_Hazard_Score_Norm = case_when(
    NH_GDAC_Hazard_Status == "active" & NH_GDAC_Hazard_Severity == "orange" ~ 10,
    TRUE ~ 0
  )) %>%
  drop_na(Country)

write.csv(gdac, "Indicator_dataset/gdaclistnormalised.csv")

#----------------------------ThinkHazard!------------------------------------------------------
# Find countries in the ThinkHazard database
country <- read.csv("https://raw.githubusercontent.com/GFDRR/thinkhazardmethods/master/source/download/ADM0_TH.csv")
country <- as.data.frame(country)
country <- suppressWarnings(country[!is.na(as.numeric(gsub("[^0-9-]","", country[[1]]))),])
country <- as.data.frame(country)

# Assign country to list
country$list <- as.numeric(gsub("[^0-9-]","", country[[1]]))
country$countrycode <- suppressWarnings(countrycode(
  country$country,
  origin = "country.name",
  destination = "iso3c",
  nomatch = NA
))

#Remove non-countries
country <- country %>%
  filter(!is.na(countrycode))

#Remove New Zealand duplicate
country <- country %>% filter(countrycode != "179;New Zealand;;")

# Extract API data on ThinkHazard! (can be slow)
think_data <- lapply(country$list, function(tt) {
  dat <- fromJSON(paste0("http://thinkhazard.org/en/report/", tt, ".json"))
})

# Compile by country
think_join <- lapply(1:length(think_data), function(yy){
  frame <- as.data.frame(think_data[yy])
  frame$Country <- country$countrycode[yy]
  do.call(data.frame, frame)
})

# Join list to a single dataframe
think_hazard <- do.call("rbind", think_join)

# Assign numberic values and calculate geometric mean
think_hazard <- think_hazard %>%
  mutate(hazard_num = case_when(hazardlevel.title == "High" ~ 4,
                                hazardlevel.title == "Medium" ~ 3,
                                hazardlevel.title == "Low" ~ 2,
                                hazardlevel.title == "Very low" ~ 1,
                                TRUE ~ NA_real_
  )) %>%
  group_by(Country) %>%
  mutate(multihazard_risk = geoMean(hazard_num, na.rm = T)) %>%
  ungroup %>%
  rename_with(.fn = ~ paste0("NH_", .), 
              .cols = -contains("Country")
              )

# Normalise values
think_hazard <- normfuncpos(think_hazard, 4, 0, "NH_multihazard_risk")

# Save file
write.csv(think_hazard, "Indicator_dataset/think_hazard.csv")

#----------------------INFORM Natural Hazard and Exposure rating--------------------------
inform_2021 <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/INFORM_2021.csv")

# Rename country
informnathaz <- inform_2021 %>%
  select(Country, Natural) %>%
  rename(NH_Hazard_Score = Natural) %>%
  drop_na(Country, NH_Hazard_Score)

# Normalise scores
informnathaz <- normfuncpos(informnathaz, 7, 1, "NH_Hazard_Score")

#----------------------UKMO La Nina--------------------------------------------------------
La_nina_data <- read.csv("~/Google Drive/PhD/R code/Compound Risk/Compound Risk/covid/compoundriskdata/Indicator_dataset/La_nina.csv", stringsAsFactors=FALSE)
La_nina_data <- as_tibble(La_nina_data)

la_nina <- La_nina_data %>%
  mutate(
    risk = case_when(
      risk == 3 ~ 10,
      risk == 2 ~ 9, 
      risk == 1 ~ 7,
      TRUE ~ NA_real_
    ),
    Country = countrycode(
      Country,
      origin = "country.name",
      destination = "iso3c",
      nomatch = NULL
    )) %>%
  rename(NH_la_nina_risk = risk)

#-------------------------------------------CREATE NATURAL HAZARD SHEET------------------------------
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")
countrylist <- countrylist %>%
  select(-X)

nathazardfull <- left_join(countrylist, nathaz, by = "Country") %>%
  left_join(., gdac, by = "Country") %>%
  left_join(., informnathaz, by = "Country") %>%
  left_join(., think_hazard, by = "Country") %>%
  left_join(., la_nina, by = "Country") %>%
  distinct(Country, .keep_all = TRUE) %>%
  drop_na(Country) %>%
  arrange(Country)

write.csv(nathazardfull, "Risk_sheets/Naturalhazards.csv")

#
##
### ********************************************************************************************
####    CREATE FRAGILITY  SHEET USING A RANGE OF SOURCE INDICATORS
### ********************************************************************************************
##
#

#--------------------------------FRAGILITY DATA-----------------------------------------
fsi <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/FSI.csv")

fsi <- fsi %>%
  select(-X) %>%
  drop_na(Country)

upperrisk <- quantile(fsi$Fr_FSI_Score, probs = c(0.9), na.rm = T)
lowerrisk <- quantile(fsi$Fr_FSI_Score, probs = c(0.1), na.rm = T)
fsi <- normfuncpos(fsi, upperrisk, lowerrisk, "Fr_FSI_Score")

upperrisk <- quantile(fsi$Fr_FSI_2019minus2020, probs = c(0.1), na.rm = T)
lowerrisk <- quantile(fsi$Fr_FSI_2019minus2020, probs = c(0.9), na.rm = T)

# Specific normalisation
fsinormneg <- function(df, upperrisk, lowerrisk, col1) {
  # Create new column col_name as sum of col1 and col2
  df[[paste0(col1, "_norm")]] <- ifelse(df[[col1]] <= upperrisk, 10,
                                        ifelse(df[[col1]] >= lowerrisk | df$Fr_FSI_Score_norm == 0, 0,
                                               ifelse(df[[col1]] > upperrisk & df[[col1]] < lowerrisk, 10 - 
                                                        (upperrisk - df[[col1]]) / (upperrisk - lowerrisk) * 10, NA)
                                        )
  )
  df
}

fsi <- fsinormneg(fsi, upperrisk, lowerrisk, "Fr_FSI_2019minus2020")

#----------------------REIGN---------------------
reign <- suppressMessages(read_csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/REIGN_2020_8.csv"))

reign <- reign %>%
  filter(year == 2020) %>%
  select(country, couprisk, month) %>%
  filter(month %in% (month(Sys.Date()) - 0:2)) %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(Fr_REIGN_couprisk3m = mean(couprisk, na.rm = T)) %>%
  dplyr::rename(Country = country) %>%
  dplyr::mutate(Country = countrycode(Country,
                                      origin = "country.name",
                                      destination = "iso3c",
                                      nomatch = NULL
  ))

upperrisk <- quantile(reign$Fr_REIGN_couprisk3m, probs = c(0.95), na.rm = T)
lowerrisk <- quantile(reign$Fr_REIGN_couprisk3m, probs = c(0.05), na.rm = T)

reign <- normfuncpos(reign, upperrisk, lowerrisk, "Fr_REIGN_couprisk3m")

#------------------------Load GPI data-----------------------------
gpi <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/GPI.csv")
gpi <- gpi %>%
  select(-X) %>%
  mutate(Country = countrycode(Country,
                               origin = "country.name",
                               destination = "iso3c",
                               nomatch = NULL
  )) %>%
  rename(Fr_GPI_Score = C_GPI_Score)
upperrisk <- quantile(gpi$Fr_GPI_Score, probs = c(0.90), na.rm = T)
lowerrisk <- quantile(gpi$Fr_GPI_Score, probs = c(0.10), na.rm = T)
gpi <- normfuncpos(gpi, upperrisk, lowerrisk, "Fr_GPI_Score")

#---------------------Load ACLED data----------------------------
acled <- suppressMessages(read_csv("~/Google Drive/PhD/R code/Compound Risk/ACLEDraw.csv"))

# summarise deaths
fatal <- acled %>%
  select(iso3, fatalities, event_date, event_type) %>%
  mutate(event_date = as.Date(parse_date_time(acled$event_date, orders = "dmy"))) %>%
  group_by(iso3, event_date) %>%
  tally(fatalities) %>%
  summarise(
    Fr_ACLED_fatal_last30d = sum(n[event_date >= max(event_date) - 30]),
    Fr_ACLED_fatal_oneyear30d = sum(n[event_date >= max(event_date) - 395 & event_date <= max(event_date) - 365]),
    Fr_ACLED_fatal_oneyearmonthlyav = sum(n[event_date >= max(event_date) - 365], na.rm = T) / 12,
    Fr_ACLED_fatal_threeyearmonthlyav = sum(n[event_date >= max(event_date) - 1095], na.rm = T) / 36,
    Fr_ACLED_fatal_same_month_difference = Fr_ACLED_fatal_last30d - Fr_ACLED_fatal_oneyear30d,
    Fr_ACLED_fatal_month_annual_difference = Fr_ACLED_fatal_last30d - Fr_ACLED_fatal_oneyearmonthlyav,
    Fr_ACLED_fatal_month_threeyear_difference = Fr_ACLED_fatal_last30d - Fr_ACLED_fatal_threeyearmonthlyav,
    Fr_ACLED_fatal_same_month_difference_perc = case_when(
      Fr_ACLED_fatal_oneyear30d == 0 ~ 0.01,
      Fr_ACLED_fatal_oneyear30d > 0 ~ ((Fr_ACLED_fatal_last30d - Fr_ACLED_fatal_oneyear30d) / Fr_ACLED_fatal_oneyear30d) * 100,
      TRUE ~ NA_real_
    ),
    Fr_ACLED_fatal_month_annual_difference_perc = case_when(
      Fr_ACLED_fatal_oneyearmonthlyav == 0 ~ 0.01,
      Fr_ACLED_fatal_oneyearmonthlyav > 0 ~ ((Fr_ACLED_fatal_last30d - Fr_ACLED_fatal_oneyearmonthlyav) / Fr_ACLED_fatal_oneyearmonthlyav) * 100,
      TRUE ~ NA_real_
    ),
    Fr_ACLED_fatal_month_threeyear_difference_perc = case_when(
      Fr_ACLED_fatal_threeyearmonthlyav == 0 ~ 0.01,
      Fr_ACLED_fatal_threeyearmonthlyav > 0 ~ ((Fr_ACLED_fatal_last30d - Fr_ACLED_fatal_threeyearmonthlyav) / Fr_ACLED_fatal_threeyearmonthlyav) * 100,
      TRUE ~ NA_real_
    )
  )

# summarise events
event <- acled %>%
  select(iso3, fatalities, event_date, event_type, event_id_cnty) %>%
  mutate(event_date = as.Date(parse_date_time(acled$event_date, orders = "dmy"))) %>%
  group_by(iso3, event_date) %>%
  count(event_id_cnty) %>%
  group_by(iso3) %>%
  summarise(
    Fr_ACLED_event_last30d = sum(n[event_date >= max(event_date) - 30]),
    Fr_ACLED_event_oneyear30d = sum(n[event_date >= max(event_date) - 395 & event_date <= max(event_date) - 365]),
    Fr_ACLED_event_oneyearmonthlyav = sum(n[event_date >= max(event_date) - 365], na.rm = T) / 12,
    Fr_ACLED_event_threeyearmonthlyav = sum(n[event_date >= max(event_date) - 1095], na.rm = T) / 36,
    Fr_ACLED_event_same_month_difference = Fr_ACLED_event_last30d - Fr_ACLED_event_oneyear30d,
    Fr_ACLED_event_month_annual_difference = Fr_ACLED_event_last30d - Fr_ACLED_event_oneyearmonthlyav,
    Fr_ACLED_event_month_threeyear_difference = Fr_ACLED_event_last30d - Fr_ACLED_event_threeyearmonthlyav,
    Fr_ACLED_event_same_month_difference_perc = case_when(
      Fr_ACLED_event_oneyear30d == 0 ~ 0.01,
      Fr_ACLED_event_oneyear30d > 0 ~ ((Fr_ACLED_event_last30d - Fr_ACLED_event_oneyear30d) / Fr_ACLED_event_oneyear30d) * 100,
      TRUE ~ NA_real_
    ),
    Fr_ACLED_event_month_annual_difference_perc = case_when(
      Fr_ACLED_event_oneyearmonthlyav == 0 ~ 0.01,
      Fr_ACLED_event_oneyearmonthlyav > 0 ~ ((Fr_ACLED_event_last30d - Fr_ACLED_event_oneyearmonthlyav) / Fr_ACLED_event_oneyearmonthlyav) * 100,
      TRUE ~ NA_real_
    ),
    Fr_ACLED_event_month_threeyear_difference_perc = case_when(
      Fr_ACLED_event_threeyearmonthlyav == 0 ~ 0.01,
      Fr_ACLED_event_threeyearmonthlyav > 0 ~ ((Fr_ACLED_event_last30d - Fr_ACLED_event_threeyearmonthlyav) / Fr_ACLED_event_threeyearmonthlyav) * 100,
      TRUE ~ NA_real_
    )
  )

# Join deaths and events
acledjoin <- full_join(fatal, event, by = "iso3")
acledjoin <- acledjoin %>%
  rename(Country = iso3)

# Normalise fatalities
aclednorm <- function(df, upperrisk, lowerrisk, col1, number) {
  # Create new column col_name as sum of col1 and col2
  df[[paste0(col1, "_norm")]] <- ifelse(df[[col1]] >= upperrisk, 10,
                                        ifelse(df[[col1]] <= lowerrisk | df[[number]] <= 20, 0,
                                               ifelse(df[[col1]] < upperrisk & df[[col1]] > lowerrisk, 10 - 
                                                        (upperrisk - df[[col1]]) / (upperrisk - lowerrisk) * 10, NA)
                                        )
  )
  df
}

acleddata <- aclednorm(acledjoin, 300, 0, "Fr_ACLED_fatal_same_month_difference_perc", "Fr_ACLED_fatal_last30d")
acleddata <- aclednorm(acleddata, 300, 0, "Fr_ACLED_fatal_month_annual_difference_perc", "Fr_ACLED_fatal_last30d")
acleddata <- aclednorm(acleddata, 600, 0, "Fr_ACLED_fatal_month_threeyear_difference_perc", "Fr_ACLED_fatal_last30d")

# Normalise events
aclednorm <- function(df, upperrisk, lowerrisk, col1, number) {
  # Create new column col_name as sum of col1 and col2
  df[[paste0(col1, "_norm")]] <- ifelse(df[[col1]] >= upperrisk, 10,
                                        ifelse(df[[col1]] <= lowerrisk | df[[number]] <= 50, 0,
                                               ifelse(df[[col1]] < upperrisk & df[[col1]] > lowerrisk, 10 - 
                                                        (upperrisk - df[[col1]]) / (upperrisk - lowerrisk) * 10, NA)
                                        )
  )
  df
}

acleddata <- aclednorm(acleddata, 400, 0, "Fr_ACLED_event_same_month_difference_perc", "Fr_ACLED_event_last30d")
acleddata <- aclednorm(acleddata, 400, 0, "Fr_ACLED_event_month_annual_difference_perc", "Fr_ACLED_event_last30d")
acleddata <- aclednorm(acleddata, 800, 0, "Fr_ACLED_event_month_threeyear_difference_perc", "Fr_ACLED_event_last30d")

write.csv(acleddata, "Indicator_Dataset/ACLEDnormalised.csv")

#---------------------- Load VIEWS data--------------------------------------
# Download and unzip file from View site
download.file("http://ucdp.uu.se/downloads/views/predictions_cm.zip", "Indicator_dataset/Viewsrawzip")

state_views <- read.csv(unz("Indicator_dataset/Viewsrawzip", "predictions_sb_cm/average_base_sb.csv"))
nonstate_views <- read.csv(unz("Indicator_dataset/Viewsrawzip", "predictions_ns_cm/average_base_ns.csv"))
oneside_views <- read.csv(unz("Indicator_dataset/Viewsrawzip", "predictions_os_cm/average_base_os.csv"))

# Calculate month converter
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

# Select next six months state
state_views_6m <- state_views %>%
  filter(month_id >= elapsed_months(Sys.time(), as.Date("1980-01-01")) &
           month_id <= elapsed_months(Sys.time(), as.Date("1980-01-01")) + 5) %>%
  group_by(isoab) %>%
  summarise(Fr_state6m = max(average_base_sb, na.rm = T))

# Select next six months nonstate
nonstate_views_6m <- nonstate_views %>%
  filter(month_id >= elapsed_months(Sys.time(), as.Date("1980-01-01")) &
           month_id <= elapsed_months(Sys.time(), as.Date("1980-01-01")) + 5) %>%
  group_by(isoab) %>%
  summarise(Fr_nonstate6m = max(average_base_ns, na.rm = T))

# Select next six months oneside
oneside_views_6m <- oneside_views %>%
  filter(month_id >= elapsed_months(Sys.time(), as.Date("1980-01-01")) &
           month_id <= elapsed_months(Sys.time(), as.Date("1980-01-01")) + 5) %>%
  group_by(isoab) %>%
  summarise(Fr_oneside6m = max(average_base_os, na.rm = T))

# Combine into one
views_6m_proj <- full_join(state_views_6m, nonstate_views_6m, by = "isoab") %>%
  full_join(., oneside_views_6m, by = "isoab") %>%
  rename(Country = isoab)

# Normalise values
views_6m_proj <- normfuncpos(views_6m_proj, 0.8, 0.1, "Fr_state6m")
views_6m_proj <- normfuncpos(views_6m_proj, 0.8, 0.1, "Fr_nonstate6m")
views_6m_proj <- normfuncpos(views_6m_proj, 0.8, 0.1, "Fr_oneside6m")

#-----------------------WB structural model----------------------------------
wbstructural <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/Fragility_WB_structural_model.csv", na.strings = c("", "NA"))

wbstructural <- wbstructural %>%
  mutate(
    Country = suppressWarnings(countrycode(Country, origin = "country.name", destination = "iso3c")),
    Fr_WB_structural_norm = case_when(
      Fr_at_risk == "At Risk" ~ 7,
      Fr_in_conflict == "In Conflict" ~ 10,
      Fr_not_at_risk == "Not At Risk" ~ 0,
      TRUE ~ NA_real_
    )
  )

wbstructural <- normfuncpos(wbstructural, 6, 0, "Fr_number_flags")

#------------------INFORM SEVERITY INDEX-------------------------------------------------
url <- read.xlsx("https://www.acaps.org/sites/acaps/files/crisis/gcsi-download/2020-11/20201105_inform_severity_-_october_2020_1.xlsx",
                 sheet = "INFORM Severity - all crises", 
                 startRow = 2)

url <- subset(url, !CRISIS %in% c("Weights", "(a-z)"))

#Add duplicates to countries listed in the same crisis
url_data <- url %>%
  mutate(ISO3 = strsplit(as.character(ISO3), ",")) %>%
  unnest(cols = ISO3) %>% 
  filter(ISO3 != "") %>%
  mutate(Country = trimws(ISO3)) %>%
  mutate_at(vars(Impact.of.the.crisis:Operating.environment, contains("INFORM", )),
            funs(case_when(. != "x" ~ as.numeric(as.character(.)),
                           TRUE ~ NA_real_))) %>%
  select(-COUNTRY, ISO3)

#Filter only severe and worsening crises
inform_crisis_worse <- url_data %>%
  mutate(
    worsening_crisis = case_when(
      INFORM.Severity.Index >= 3 & `Trend.(last.3.months)` == "Increasing" ~ "Crisis",
      INFORM.Severity.Index <= 3 & `Trend.(last.3.months)` != "Increasing" ~ "No Crisis",
      TRUE ~ NA_character_
    ),
    crisis_severe = case_when(
      INFORM.Severity.Index >= 4 ~ "Crisis",
      INFORM.Severity.Index < 4 ~ "No Crisis",
      TRUE ~ NA_character_
    ),
    combined_crisis = case_when(
      worsening_crisis == "Crisis" | crisis_severe == "Crisis" ~ "Crisis",
      TRUE ~ NA_character_
    ),
    combined_crisis_norm = case_when(
      combined_crisis == "Crisis" ~ 10,
      TRUE ~ NA_real_
    ),
  ) %>% rename_with(
    .fn = ~ paste0("Fr_", .), 
    .cols = colnames(.)[!colnames(.) %in% c("Country") ]
  ) %>%
  group_by(Country) %>%
  filter(Fr_INFORM.Severity.Index == max(Fr_INFORM.Severity.Index))


#-------------------------------------FRAGILITY SHEET--------------------------------------
# Compile joint database
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")

fragilitysheet <- left_join(countrylist, fsi, by = "Country") %>%
  left_join(., reign, by = "Country") %>%
  left_join(., gpi, by = "Country") %>%
  left_join(., acleddata, by = "Country") %>%
  left_join(., views_6m_proj, by = "Country") %>%
  left_join(., wbstructural, by = "Country") %>%
  left_join(., inform_fragile, by = "Country") %>%
  left_join(., inform_crisis_worse, by = "Country") %>%
  arrange(Country) %>%
  select(-X)

write.csv(fragilitysheet, "Risk_sheets/fragilitysheet.csv")

#
##
### ********************************************************************************************
####    CREATE ACAPS SHEET USING A RANGE OF SOURCE INDICATORS
### ********************************************************************************************
##
#

#--------------------LOAD ACAPS realtime database-------------------------------------------
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

# Find all countrys in the list and replace with correct country name (then fill in remaining NAs)
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
  select(gaplist)

conflictnams <- unique(conflictnams)

# Food security countries
foodnams <- acapslist[str_detect(acapslist$country, c("Food|food|famine|famine")), ] %>%
  filter(risk >= 4) %>%
  select(gaplist)

foodnams <- unique(foodnams)

# Natural hazard countries
naturalnams <- acapslist[str_detect(acapslist$country, c("Floods|floods|Drought|drought|Cyclone|cyclone|
                                                          Flooding|flooding|Landslides|landslides|
                                                          Earthquake|earthquake")), ] %>%
  filter(risk >= 3) %>%
  select(gaplist)

naturalnams <- unique(naturalnams)

# Epidemic countries
healthnams <- acapslist[str_detect(acapslist$country, c("Epidemic|epidemic")), ] %>%
  filter(risk >= 3) %>%
  select(gaplist)

healthnams <- unique(healthnams)

# Load countries in the CRM
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")

acapssheet <- countrylist %>%
  select(-X) %>%
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

  
