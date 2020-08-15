#--------------------LOAD PACKAGES--------------
#install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(ggplot2, cowplot, lubridate, rvest,dplyr, viridis, tidyverse, countrycode, clipr)

#--------------------FUNCTION TO CALCULATE NORMALISATION SCORES-----------------

#Function to normalise with upper and lower bounds (when high score = low vulnerability)
normfuncneg <- function(df,upperrisk, lowerrisk, col1){
  #Create new column col_name as sum of col1 and col2
  df[[paste0(col1, "_norm")]] <- ifelse(df[[col1]] >= upperrisk, 10,
                                       ifelse(df[[col1]] <= lowerrisk, 0,
                                              ifelse(df[[col1]]  < upperrisk & df[[col1]]  > lowerrisk,  10 - (upperrisk - df[[col1]] )/(upperrisk - lowerrisk)*10, NA)
                                              )
                                       )
  df
}

#Function to normalise with upper and lower bounds (when high score = high vulnerability)
normfuncpos <- function(df,upperrisk, lowerrisk, col1){
  #Create new column col_name as sum of col1 and col2
  df[[paste0(col1, "_norm")]] <- ifelse(df[[col1]] >= upperrisk, 10,
                                       ifelse(df[[col1]] <= lowerrisk, 0,
                                              ifelse(df[[col1]]  < upperrisk & df[[col1]]  > lowerrisk,  10 - (upperrisk - df[[col1]] )/(upperrisk - lowerrisk)*10, NA)
                                              )
                                       )
  df
}

#--------------------HIS Score-----------------
HIS <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/HIS.csv")

HIS <- HIS %>%
  rename(Country = H_Country) %>%
  select(-X)
  
HIS <- normfuncneg(HIS, 50, 20, "H_HIS_Score")

#-----------------------Oxford rollback Score-----------------
OXrollback <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/OXrollbackscore.csv")

OXrollback <- normfuncneg(OXrollback, 0.3, 0.8, "H_Oxrollback_score")
OXrollback <- OXrollback %>%
  mutate(Country = countrycode(Country, 
                               origin = 'country.name',
                               destination = 'iso3c', 
                               nomatch = NULL))
  
#-------------------------COVID projections--------------------
covid <- "https://covid19-projections.com/#view-projections"
covid <- read_html(covid)

#Scrape tables
covidtable <- covid %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

#Isolate regional tables
covideu <- covidtable[[9]]
covidworld <- covidtable[[10]]
covidus <- covidtable[[8]][1,]
colnames(covidus)[1] <- c("Country")
colnames(covideu)[1] <- c("Country")
colnames(covidworld)[1] <- c("Country")

#Merge tables
covidproj <- merge(covideu, covidworld, all=T)
covidproj <- merge(covidproj, covidus, all=T)
covidproj$Country <- countrycode(covidproj$Country, 
                                 origin = 'country.name', 
                                 destination = 'iso3c',
                                 nomatch = NULL)

#Convert to numeric
covidproj$`Additional Deaths (% of Current Deaths)` <- gsub("%", "", covidproj$`Additional Deaths (% of Current Deaths)`)
varname <- c("Current Deaths", "Projected Deaths - Mean", "Projected Deaths / 1M", 
             "Additional Deaths - Mean", "Additional Deaths (% of Current Deaths)", 
             "Projected Deaths - 2.5th Percentile", "Projected Deaths - 97.5th Percentile")

covidproj[varname] <- lapply(covidproj[varname],function(xx) {                                                                                                        
  gsub(",","",xx)
})

covidproj[varname] <- lapply(covidproj[varname],function(xx) {                                                                                                       
  as.numeric(as.character(xx))
})

#Change colnames to consistent format
colnames(covidproj) <- c("Country", "H_Covidproj_Current Deaths", "H_Covidproj_Projected Deaths - Mean", "H_Covidproj_Projected Deaths / 1M", 
                                    "H_Covidproj_Additional Deaths - Mean", "H_Covidproj_Additional Deaths (% of Current Deaths)", 
                                    "H_Covidproj_Projected Deaths - 2.5th Percentile", "H_Covidproj_Projected Deaths - 97.5th Percentile")
colnames(covidproj) <- gsub(" ", "_", colnames(covidproj))

#Add normalised values
upperrisk <- quantile(covidproj$`H_Covidproj_Projected_Deaths_/_1M`, probs = c(0.80))
lowerrisk <- quantile(covidproj$`H_Covidproj_Projected_Deaths_/_1M`, probs = c(0.10))
covidproj <- normfuncpos(covidproj, upperrisk, lowerrisk, "H_Covidproj_Projected_Deaths_/_1M")

#------------------------COVID deaths and cases--------------------------
covidweb <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")

covid <- covidweb %>%
  mutate(date = as.Date(date)) %>%
  filter(date > Sys.Date( ) - 28)

#bi-weekly growth rate for covid deaths and cases
covidgrowth <- covid %>%
  mutate(previous2week = case_when(date >= Sys.Date( ) - 13 ~ "twoweek", 
                                   TRUE ~ "lasttwoweek")) %>%
  group_by(iso_code, previous2week) %>%
  summarise(meandeaths = mean(new_deaths, na.rm=T),
            meancase = mean(new_cases, na.rm=T)) %>%
  group_by(iso_code) %>%
  mutate(growthdeath =  meandeaths[previous2week == 'twoweek'] - meandeaths,
         growthratedeaths = growthdeath/meandeaths *100,
         growthcase =  meancase[previous2week == 'twoweek'] - meancase,
         growthratecases = growthcase/meancase *100) %>%
  filter(previous2week != "twoweek") %>%
  select(- previous2week, -growthcase, -growthdeath, -meandeaths, -meancase)

#Find name of countries with few cases
namecase <- covid %>%
  filter(date== Sys.Date()-1) %>%
  filter(new_cases_per_million > 10) %>%
  select(iso_code)

#Function to normalise with upper and lower bounds (when high score = high vulnerability)
normfuncpos <- function(df,upperrisk, lowerrisk, col1){
  df[[paste0(col1, "_norm")]] <- ifelse(df[[col1]] >= upperrisk, 10,
                                        ifelse(df[[col1]] <= lowerrisk, 0,
                                               ifelse(df$iso_code %in% namecase, 0,
                                                      ifelse(df[[col1]]  < upperrisk & df[[col1]]  > lowerrisk,  10 - (upperrisk - df[[col1]] )/(upperrisk - lowerrisk)*10, NA)
                                        )))                                               
                                        
  df
}

#Normalised scores for deaths
covidgrowth <- normfuncpos(covidgrowth, 150, 0, "growthratedeaths")
covidgrowth <- normfuncpos(covidgrowth, 150, 0, "growthratecases")

#Rename columns
colnames(covidgrowth) <- c("Country", "H_Covidgrowth_biweeklydeaths", "H_Covidgrowth_biweeklycases", "H_Covidgrowth_deathsnorm", "H_Covidgrowth_casesnorm")

#----------------------------------Create combined Health Sheet-------------------------------------------
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/countrylist.csv")

health <- countrylist
health <- left_join(HIS, OXrollback, by="Country") %>%
  left_join(., covidproj,  by="Country") %>% 
  left_join(., covidgrowth, by="Country")

write.csv(health, "healthsheet.csv")

#



