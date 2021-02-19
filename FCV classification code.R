#FSI score
fsi <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/fsi_2020.csv")

fsi <- fsi %>%
  select(-X) %>%
  drop_na(Country) %>%
  mutate(Country = countrycode(Country,
                        origin = "country.name",
                        destination = "iso3c",
                        nomatch = NULL
  ))

# FCS
fcv <- read.csv("Indicator_dataset/Country_classification.csv") %>%
  select(-X, Countryname, -IDA.status ) %>%
  mutate(FCV_normalised = case_when(FCV_status == "High-Intensity conflict" ~ 10,
                                    FCV_status == "Medium-Intensity conflict" ~ 10,
                                    FCV_status == "High-Institutional and Social Fragility" ~ 10,
                                    TRUE ~ 0))

# Join
test <- left_join(fcv, fsi %>% select(FSI_Score, Country), by = "Country")

testing <- normfuncpos(test,90,0, "FSI_Score")

testing <- testing %>%
  mutate(Final_score = case_when(FCV_normalised == 10 | FSI_Score_norm == 10 ~ 10,
                                 TRUE ~ FSI_Score_norm))

write.csv(testing, "Indicator_dataset/Fragile_list.csv")
