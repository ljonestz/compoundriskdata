# FRAGILE INDEX

# LOAD PACKAGES
# install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(
  ggplot2, cowplot, lubridate, rvest, dplyr, compositions, viridis,
  tidyverse, countrycode, clipr, sjmisc, awalker89 / openxlsx, EnvStats,
  gsheet, jsonlite, zoo
)

#------------------Function to normalise values----------------------------------
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

#-----------------------FSI score---------------------------------
fsi <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/fsi_2020.csv")

fsi <- fsi %>%
  select(-X) %>%
  drop_na(Country) %>%
  mutate(
    Country = countrycode(Country,
                          origin = "country.name",
                          destination = "iso3c",
                          nomatch = NULL)
    )

#-------------------------FCS---------------------------------------------
fcv <- read.csv("Indicator_dataset/Country_classification.csv") %>%
  select(-X, Countryname, -IDA.status) %>%
  mutate(
    FCV_normalised = case_when(
      FCV_status == "High-Intensity conflict" ~ 10,
      FCV_status == "Medium-Intensity conflict" ~ 10,
      FCV_status == "High-Institutional and Social Fragility" ~ 10,
      TRUE ~ 0
    )
  )

# Join
test <- left_join(fcv, fsi %>% select(FSI_Score, Country), by = "Country")
testing <- normfuncpos(test, 90, 0, "FSI_Score")
testing <- testing %>%
  mutate(
    Final_score = case_when(
      FCV_normalised == 10 | FSI_Score_norm == 10 ~ 10,
      TRUE ~ FSI_Score_norm
    ))

#-------------------UCDP data on conflict-----------------------------------
ucdp_data <- read.csv("~/Downloads/GEDEvent_v20_0_1.csv")

ucdp <- ucdp_data %>%
  group_by(country) %>%
  mutate(total_death = sum(best, na.rm = T))

#-----------------------------IDPs--------------------------------------------------------
idp_data <- read_csv("~/Downloads/query_data (4)/population.csv",
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
  select(`Country of origin (ISO)`, refugees, z_refugees, refugees_fragile, idps, z_idps, idps_fragile)

# Normalise scores
idp <- normfuncpos(idp, 1, -1, "z_refugees")
idp <- normfuncpos(idp, 1, -1, "z_idps")

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
  select(-`Country of origin (ISO)`)

#-------------------------ACLED data---------------------------------------------
acled_data <- fromJSON("https://api.acleddata.com/acled/read/?key=*9t-89Rn*bDb4qFXBAmO&email=ljones12@worldbank.org&event_date=2017-12-01&event_date_where=>&fields=iso3|fatalities|event_date&limit=0")

acled <- acled_data$data %>%
  mutate(
    fatalities = as.numeric(as.character(fatalities)),
    date = as.Date(event_date),
    month_yr = as.yearmon(date)
  ) %>%
  group_by(iso3, month_yr) %>%
  summarise(fatal_month = sum(fatalities, na.rm = T)) %>%
  mutate(fatal_3_month = fatal_month + lag(fatal_month, na.rm=T) + lag(fatal_month, 2, na.rm=T)) %>%
  group_by(iso3) %>%
  mutate(fatal_z = (fatal_3_month - mean(fatal_3_month, na.rm = T)) / sd(fatal_3_month, na.rm = T)) %>%
  filter(month_yr == "Nov 2020")

# Normalise scores
acled <- normfuncpos(acled, 1, -1, "fatal_z")

# Correct for countries with 0
acled <- acled %>%
  mutate(
    fatal_z_norm = case_when(
      is.nan(fatal_z) ~ 0,
      TRUE ~ fatal_z_norm
    ),
    Country = countrycode(iso3,
      origin = "country.name",
      destination = "iso3c",
      nomatch = NULL
    )
  ) %>%
  select(-iso3)

#--------------------------REIGN--------------------------------------------
reign_data <- suppressMessages(read_csv("~/Downloads/REIGN_2020_11.csv"))

reign_start <- reign_data %>%
  filter(year == 2020) %>%
  group_by(country) %>%
  slice(which.max(month)) %>%
  select(country, month, pt_suc, pt_attempt, delayed, irreg_lead_ant) %>%
  mutate(
    country = countrycode(country,
                          origin = "country.name",
                          destination = "iso3c",
                          nomatch = NULL
    )) %>%
  rename(Country = country)

# Add FSI/BRD threshold
reign <- left_join(reign_start, testing %>% select(Country, Final_score), by = "Country") %>%
  mutate(
    delayed_adj = case_when(
      Final_score == 10 ~ delayed,
      TRUE ~ 0
    ),
    pol_trigger = case_when(
      pt_suc + pt_attempt + delayed_adj + irreg_lead_ant >= 1 ~ "Fragile",
      TRUE ~ "Not Fragile"
    ),
    pol_trigger_norm = case_when(
      pt_suc + pt_attempt + delayed_adj + irreg_lead_ant >= 1 ~ 10,
      TRUE ~ 0
    )
  ) %>%
  select(-Final_score)

  
#-----------------Join all dataset-----------------------------------
conflict_dataset_raw <- left_join(testing, reign, by = "Country") %>%
  left_join(., idp, by = "Country") %>%
  left_join(., acled, by = "Country") %>%
  select(Countryname, Final_score, pol_trigger_norm, z_idps_norm, fatal_z_norm) 

conflict_dataset <- conflict_dataset_raw %>%
   mutate(
    flag_count = as.numeric(unlist(row_count(.,
      Final_score:fatal_z_norm,
      count = 10,
      append = F
      ))),
    fragile_1_flag = case_when(
      flag_count >= 1 ~ 10,
      TRUE ~ apply(conflict_dataset_raw %>% select(Final_score:fatal_z_norm), 1, FUN = max, na.rm = T)
    ),
    fragile_2_flags = case_when(
      flag_count >= 2 ~ 10,
      TRUE ~ 0
    ),
    fragile_flag_seq = case_when(
      flag_count >= 2 ~ 10,
      flag_count == 1 ~ 7,
      TRUE ~ 0
    )
  ) %>%
  rename(FCS_FSI_Normalised = Final_score, REIGN_Normalised = pol_trigger_norm,
         Displaced_UNHCR_Normalised = z_idps_norm, BRD_Normalised = fatal_z_norm,
         Number_of_High_Risk_Flags = flag_count, Overall_Conflict_Risk_Score = fragile_1_flag) %>%
  select(-fragile_2_flags, -fragile_flag_seq)


