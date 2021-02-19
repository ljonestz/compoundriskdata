# Preparedness database
prep_data <- left_join(riskflags, globalrisk, by = c("Country", "Countryname")) %>%
  select(Country, Countryname, EXISTING_RISK_FOOD_SECURITY, H_HIS_Score_norm, EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY)

# Load ThinkHazard data
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
think_data <- lapply(country$list[1:236], function(tt) {
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
  dplyr::select(Country, hazardtype.hazardtype, hazardlevel.title) %>%
  pivot_wider(names_from = hazardtype.hazardtype, values_from = c("hazardlevel.title")) 

# Merge datafiles together
preparedness <- left_join(prep_data, think_hazard, by = "Country") %>%
  mutate_at(vars(EXISTING_RISK_FOOD_SECURITY, EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY, H_HIS_Score_norm),
            ~ case_when(. == 10 ~ "High",
                        . < 10 & . >= 3 ~ "Medium",
                        . < 3 & . >= 1 ~ "Low",
                        . < 1 ~ "Very Low",
                        TRUE ~ NA_character_))

preparedness$`River flood` <- as.character(unlist(as.character(preparedness$`River flood`)))
preparedness$`Urban flood` <- as.character(unlist(as.character(preparedness$`Urban flood`)))
preparedness$`Coastal flood` <- as.character(unlist(as.character(preparedness$`Coastal flood`)))
preparedness$`Earthquake` <- as.character(unlist(as.character(preparedness$`Earthquake`)))
preparedness$`Landslide`  <- as.character(unlist(as.character(preparedness$`Landslide` )))
preparedness$`Tsunami` <- as.character(unlist(as.character(preparedness$`Tsunami`)))
preparedness$`Volcano` <- as.character(unlist(as.character(preparedness$`Volcano`)))
preparedness$`Cyclone`<- as.character(unlist(as.character(preparedness$`Cyclone`)))
preparedness$`Extreme heat` <- as.character(unlist(as.character(preparedness$`Extreme heat` )))
preparedness$`Wildfire` <- as.character(unlist(as.character(preparedness$`Wildfire` )))
preparedness$`Water scarcity` <- as.character(unlist(as.character(preparedness$`Water scarcity`)))

# Save to csv
write.csv(preparedness, "preparedness.csv")



