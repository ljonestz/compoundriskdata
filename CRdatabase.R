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
                                        ))
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

#---------------------------------LOAD FOOD SECURITY DATA---------------------------
#Proteus Index
proteus <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/proteus.csv")

proteus <- proteus %>%
  rename(F_Proteus_Score = Proteus.index) %>%
  select(-X) %>%
  mutate(Country = countrycode(Country, 
                               origin = 'country.name',
                               destination = 'iso3c', 
                               nomatch = NULL))

upperrisk <- quantile(proteus$F_Proteus_Score, probs = c(0.90))
lowerrisk <- quantile(proteus$F_Proteus_Score, probs = c(0.10))
proteus <- normfuncpos(proteus,upperrisk, lowerrisk, "F_Proteus_Score") 

#Artemis
artemis <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/artemis.csv")

upperrisk <- 0.2
lowerrisk <- 0
artemis <- normfuncpos(artemis,upperrisk, lowerrisk, "F_Artemis_Score") 

artemis <- artemis %>%
  mutate(Country = countrycode(Country, 
                               origin = 'country.name',
                               destination = 'iso3c', 
                               nomatch = NULL)) %>%
         select(-X)

#FEWSNET
fews <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/fewsnet.csv")

fews <- fews %>%
  select(-X) %>%
  mutate(F_Fewsnet_Score_norm = case_when(F_Fewsnet_Score == 1 ~ 1,
                                          F_Fewsnet_Score == 2 ~ 7,
                                          F_Fewsnet_Score >= 3 ~ 10,
                                          TRUE ~ NA_real_),
         Country = countrycode(Country, 
                               origin = 'country.name',
                               destination = 'iso3c', 
                               nomatch = NULL))

#---------------------- Scrape food price data from FAO ---------------------------------
#Food price volatility
librarian::shelf(ggplot2, cowplot, lubridate, rvest,dplyr, viridis, tidyverse, countrycode)

#Website
fao <- "https://datalab.review.fao.org/scraped_prices/world.html"
faoweb <- read_html(fao)

#Scrape table
faoscrape <- faoweb %>% 
  html_nodes("td") %>%
  html_text()

#Create dataframe
faoprice <- as.data.frame(matrix(faoscrape, ncol=4, byrow=T), stringsAsFactors=T)
names(faoprice) <- c('Country', "Pc6m", "Pc30d", "Pc7d")

#Convert to numeric
faoprice[c( "Pc6m", "Pc30d", "Pc7d")] <- lapply(faoprice[c( "Pc6m", "Pc30d", "Pc7d")] , function(xx) {
  as.numeric(as.character(xx))
})

#Remove tag 
faoprice <- faoprice %>% filter(faoprice$Country!="Source: Numbeo.com\n") %>%
  mutate(Country = countrycode(Country, 
                                        origin = 'country.name',
                                        destination = 'iso3c', 
                                        nomatch = NULL)) %>%
  rename(F_FAO_6mFPV = Pc6m,
         F_FAO_30dFPV = Pc30d,
         F_FAO_7dFPV = Pc7d)

#Normalise scores
upperrisk <- quantile(faoprice$F_FAO_6mFPV, probs = c(0.95), na.rm=T)
lowerrisk <- quantile(faoprice$F_FAO_6mFPV, probs = c(0.05), na.rm=T)
fpv <- normfuncpos(faoprice,upperrisk, lowerrisk, "F_FAO_6mFPV") 


#------------------------CREATE FOOD SECURITY SHEET--------------------
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/countrylist.csv")

foodsecurity <- countrylist
foodsecurity <- left_join(proteus, fewsnet, by="Country") %>%
  left_join(., fpv,  by="Country") %>% 
  left_join(., artemis, by="Country")

write.csv(foodsecurity, "foodsecuritysheet.csv")

#---------------------------LOAD DEBT DATA----------------------------
#SCRAPE DEBT DATA
debtweb <- "https://www.worldbank.org/en/topic/debt/brief/covid-19-debt-service-suspension-initiative"
debt <- read_html(debtweb)

debttab <- debt %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

debttab <- as.data.frame(debttab)
colnames(debttab) <- debttab[1,]
debttab <- debttab[-1,] 

debttab <- debttab %>%
  mutate(Country4 = gsub('[0-9]+', '', Country4),
         Countryiso = countrycode(Country4, 
                                  origin = 'country.name',
                                  destination = 'iso3c', 
                                  nomatch = NULL)) %>%
  filter(Countryiso != c('TOTAL')) 

colnames(debttab) <- gsub('[0-9]+', '', colnames(debttab))

#----------------------IMF Debt forecasts--------------
imfdebt <- read.csv("https://pkgstore.datahub.io/core/imf-weo/values_csv/data/9ff413a778fd38ef165daa1b9754fb67/values_csv.csv")
imfdebt %>%
  filter(Indicator == "GGXWDG_NGDP") %>%
  select(-Indicator) %>%
  filter(Year > 2017)

