#--------------------LOAD PACKAGES--------------
#install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(ggplot2, cowplot, lubridate, rvest,dplyr, viridis, tidyverse, countrycode, clipr)

#--------------------FUNCTION TO CALCULATE NORMALISED SCORES-----------------

#Function to normalise with upper and lower bounds (when low score = high vulnerability)
normfuncneg <- function(df,upperrisk, lowerrisk, col1){
  #Create new column col_name as sum of col1 and col2
  df[[paste0(col1, "_norm")]] <- ifelse(df[[col1]] <= upperrisk, 10,
                                        ifelse(df[[col1]] >= lowerrisk, 0,
                                               ifelse(df[[col1]]  > upperrisk & df[[col1]]  < lowerrisk,  10 - (upperrisk - df[[col1]] )/(upperrisk - lowerrisk)*10, NA)
                                        ))
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
HIS <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/HIS.csv")

HIS <- HIS %>%
  rename(Country = H_Country) %>%
  select(-X)
  
HIS <- normfuncneg(HIS, 20, 50, "H_HIS_Score")

#-----------------------Oxford rollback Score-----------------
OXrollback <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/OXrollbackscore.csv")

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
normfuncposcovid <- function(df,upperrisk, lowerrisk, col1){
  df[[paste0(col1, "_norm")]] <- ifelse(df[[col1]] >= upperrisk, 10,
                                        ifelse(df[[col1]] <= lowerrisk, 0,
                                               ifelse(df$iso_code %in% namecase, 0,
                                                      ifelse(df[[col1]]  < upperrisk & df[[col1]]  > lowerrisk,  10 - (upperrisk - df[[col1]] )/(upperrisk - lowerrisk)*10, NA)
                                        )))                                               
                                        
  df
}

#Normalised scores for deaths
covidgrowth <- normfuncposcovid(covidgrowth, 150, 0, "growthratedeaths")
covidgrowth <- normfuncposcovid(covidgrowth, 150, 0, "growthratecases")

#Rename columns
colnames(covidgrowth) <- c("Country", "H_Covidgrowth_biweeklydeaths", "H_Covidgrowth_biweeklycases", "H_Covidgrowth_deathsnorm", "H_Covidgrowth_casesnorm")

#----------------------------------Create combined Health Sheet-------------------------------------------
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")

health <- countrylist
health <- left_join(HIS, OXrollback, by="Country") %>%
  left_join(., covidproj,  by="Country") %>% 
  left_join(., covidgrowth, by="Country")

write.csv(health, "Risk_sheets/healthsheet.csv")

#---------------------------------LOAD FOOD SECURITY DATA---------------------------
#Proteus Index
proteus <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/proteus.csv")

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
artemis <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/artemis.csv")

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
fews <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/fewsnet.csv")

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
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")

foodsecurity <- countrylist
foodsecurity <- left_join(proteus, fews, by="Country") %>%
  left_join(., fpv,  by="Country") %>% 
  left_join(., artemis, by="Country")

write.csv(foodsecurity, "Risk_sheets/foodsecuritysheet.csv")

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
  rename(Country = Country4) %>%
  mutate(Country = gsub('[0-9]+', '', Country),
         Country = countrycode(Country, 
                                  origin = 'country.name',
                                  destination = 'iso3c', 
                                  nomatch = NULL)) %>%
  filter(Country != c('TOTAL')) 

colnames(debttab) <- gsub('[0-9]+', '', colnames(debttab))
debttab <- debttab %>%
  select(Country, `DSSI Participation?`, `Risk of overall debt distress`, `Potential DSSI Savings   (in % of  GDP)`)
colnames(debttab) <- c('Country', 'D_DSSI', 'D_WB_Overall_debt_distress', 'D_WB_DSSI_Save')

debttab$D_WB_Overall_debt_distress_norm <- ifelse(debttab$D_WB_Overall_debt_distress == "In distress", 10,
                                                  ifelse(debttab$D_WB_Overall_debt_distress == "High", 10,
                                                         ifelse(debttab$D_WB_Overall_debt_distress == "Moderate", 7,
                                                                ifelse(debttab$D_WB_Overall_debt_distress == "Low", 3, NA)
                                                  )))
#IMF Debt forecasts
imfdebt <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/imfdebt.csv")
imfdebt <- imfdebt %>%
  mutate(Country = countrycode(Country, 
                               origin = 'country.name',
                               destination = 'iso3c', 
                               nomatch = NULL)) %>%
  select(-X)

names <- c("D_IMF_debt2017", "D_IMF_debt2018", "D_IMF_debt2019", 
"D_IMF_debt2020", "D_IMF_debt2021", "D_IMF_debt2020.2019")
imfdebt[names] <- lapply(imfdebt[names], function(xx) {
  as.numeric(as.character(xx))
})

upperrisk <- quantile(imfdebt$D_IMF_debt2020.2019, probs = c(0.05), na.rm=T)
lowerrisk <- quantile(imfdebt$D_IMF_debt2020.2019, probs = c(0.95), na.rm=T)
imfdebt <- normfuncneg(imfdebt,upperrisk, lowerrisk, "D_IMF_debt2020.2019") 

#-------------------------CREATE DEBT SHEET-----------------------------------
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")

debtsheet <- countrylist
debtsheet <- left_join(debttab, imfdebt , by="Country") 

write.csv(debtsheet, "Risk_sheets/debtsheet.csv")

#--------------------------MACRO DATA---------------------------------------
macro <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/macro.csv")
macro <- macro %>%
  mutate(M_Economic_Dependence_Score = rowMeans(select(., c(M_Fuel_Imports_perc, M_Food_Imports_perc,M_Travel_Tourism_perc)), 
                                                na.rm=T),
         M_Financial_Resilience_Score = rowMeans(select(., c(M_Remittance_perc, M_Reserves, M_ODA_perc, M_Gsavings_perc)), 
                                                 na.rm=T)) %>%
  mutate(M_Economic_and_Financial_score = rowMeans(select(., c(M_Economic_Dependence_Score,M_Financial_Resilience_Score)),
                                                   na.rm=T)) %>%
  select(-X)
upperrisk <- quantile(macro$M_Economic_and_Financial_score, probs = c(0.9), na.rm=T)
lowerrisk <- quantile(macro$M_Economic_and_Financial_score, probs = c(0.1), na.rm=T)
macro <- normfuncpos(macro,upperrisk, lowerrisk, "M_Economic_and_Financial_score") 

#GDP forecast
gdp <- read_csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/gdp.csv")
gdp <- gdp %>%
  select(-X1)
upperrisk <- quantile(gdp$M_GDP_WB_2019minus2020, probs = c(0.2), na.rm=T)
lowerrisk <- quantile(gdp$M_GDP_WB_2019minus2020, probs = c(0.95), na.rm=T)
gdp <- normfuncneg(gdp,upperrisk, lowerrisk, "M_GDP_WB_2019minus2020") 
upperrisk <- quantile(gdp$M_GDP_IMF_2019minus2020, probs = c(0.2), na.rm=T)
lowerrisk <- quantile(gdp$M_GDP_IMF_2019minus2020, probs = c(0.95), na.rm=T)
gdp <- normfuncneg(gdp,upperrisk, lowerrisk, "M_GDP_IMF_2019minus2020") 

#-----------------------------CREATE MACRO SHEET-----------------------------------------
macrosheet <- left_join(macro, gdp, by="Country") 
write.csv(macrosheet, "Indicator_dataset/macrosheet.csv")

#--------------------------------FRAGILITY DATA-----------------------------------------
fsi <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/FSI.csv")
fsi <- fsi %>% 
  select(-X) %>%
  drop_na(Country)
upperrisk <- quantile(fsi$Fr_FSI_2019minus2020, probs = c(0.1), na.rm=T)
lowerrisk <- quantile(fsi$Fr_FSI_2019minus2020, probs = c(0.9), na.rm=T)
fsi <- normfuncneg(fsi,upperrisk, lowerrisk, "Fr_FSI_2019minus2020") 
upperrisk <- quantile(fsi$Fr_FSI_Score , probs = c(0.1), na.rm=T)
lowerrisk <- quantile(fsi$Fr_FSI_Score , probs = c(0.9), na.rm=T)
fsi <- normfuncneg(fsi,upperrisk, lowerrisk, "Fr_FSI_Score") 

#INFORM
informfragile <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/INFORM_fragility")
informfragile <- informfragile %>% select(-X)
upperrisk <- quantile(informfragile$Fr_INFORM_Fragility_Score, probs = c(0.95), na.rm=T)
lowerrisk <- quantile(informfragile$Fr_INFORM_Fragility_Score, probs = c(0.05), na.rm=T)
informfragile <- normfuncpos(informfragile,upperrisk, lowerrisk, "Fr_INFORM_Fragility_Score") 

#REIGN
reign <- read_csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/REIGN_2020_8.csv")
reign <- reign %>%
  filter(year == 2020) %>%
  select(country, couprisk, month) %>%
  filter(month %in% (month(Sys.Date())-0:2)) %>%
  group_by(country) %>%
  summarise(Fr_REIGN_couprisk3m = mean(couprisk, na.rm=T)) %>%
  rename(Country = country) %>%
  mutate(Country = countrycode(Country, 
                               origin = 'country.name',
                               destination = 'iso3c', 
                               nomatch = NULL))
upperrisk <- quantile(reign$Fr_REIGN_couprisk3m, probs = c(0.95), na.rm=T)
lowerrisk <- quantile(reign$Fr_REIGN_couprisk3m, probs = c(0.05), na.rm=T)
reign <- normfuncpos(reign,upperrisk, lowerrisk, "Fr_REIGN_couprisk3m") 

#-------------------------------------FRAGILITY SHEET--------------------------------------
fragilitysheet <- full_join(fsi, informfragile , by="Country")  %>%
  full_join(., reign,  by="Country")
write.csv(fragilitysheet, "Risk_sheets/fragilitysheet.csv")

#-------------------------------------CONFLICT DATA-----------------------------
#Load GPI data
gpi <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/GPI.csv")
gpi <- gpi %>% 
  select(-X) %>%
  mutate(Country = countrycode(Country, 
                               origin = 'country.name',
                               destination = 'iso3c', 
                               nomatch = NULL))
upperrisk <- quantile(gpi$C_GPI_Score, probs = c(0.90))
lowerrisk <- quantile(gpi$C_GPI_Score, probs = c(0.10))
gpi <- normfuncpos(gpi, upperrisk, lowerrisk, "C_GPI_Score")

#Load ACLED data  
acled <- read_csv("~/Google Drive/PhD/R code/Compound Risk/ACLEDraw.csv")

#summarise deaths
fatal <- acled %>%
  select(iso3, fatalities, event_date, event_type) %>%
  mutate(event_date = as.Date(parse_date_time(acled$event_date, orders = "dmy"))) %>%
  group_by(iso3, event_date) %>%
  tally(fatalities) %>%
  summarise(C_ACLED_fatal_last30d = sum(n[event_date >= Sys.Date()-30]),
            C_ACLED_fatal_oneyear30d = sum(n[event_date >= Sys.Date()-395 & event_date <= Sys.Date()-365]),
            C_ACLED_fatal_oneyearmonthlyav = sum(n[event_date >= Sys.Date()-365], na.rm=T)/12,
            C_ACLED_fatal_threeyearmonthlyav = sum(n[event_date >= Sys.Date()-1095], na.rm=T)/36,
            C_ACLED_fatal_same_month_difference = C_ACLED_fatal_last30d - C_ACLED_fatal_oneyear30d,
            C_ACLED_fatal_month_annual_difference = C_ACLED_fatal_last30d - C_ACLED_fatal_oneyearmonthlyav,
            C_ACLED_fatal_month_threeyear_difference = C_ACLED_fatal_last30d - C_ACLED_fatal_threeyearmonthlyav,
            C_ACLED_fatal_same_month_difference_perc = ((C_ACLED_fatal_last30d - C_ACLED_fatal_oneyear30d) / C_ACLED_fatal_oneyear30d) * 100,
            C_ACLED_fatal_month_annual_difference_perc = ((C_ACLED_fatal_last30d - C_ACLED_fatal_oneyearmonthlyav) / C_ACLED_fatal_oneyearmonthlyav) *100,
            C_ACLED_fatal_month_threeyear_difference_perc = ((C_ACLED_fatal_last30d - C_ACLED_fatal_threeyearmonthlyav) / C_ACLED_fatal_threeyearmonthlyav)*100)

#summarise events
event <- acled %>%
  select(iso3, fatalities, event_date, event_type, event_id_cnty) %>%
  mutate(event_date = as.Date(parse_date_time(acled$event_date, orders = "dmy"))) %>%
  group_by(iso3, event_date) %>%
  count(event_id_cnty) %>%
  group_by(iso3) %>%
  summarise(C_ACLED_event_last30d = sum(n[event_date >= Sys.Date()-30]),
            C_ACLED_event_oneyear30d = sum(n[event_date >= Sys.Date()-395 & event_date <= Sys.Date()-365]),
            C_ACLED_event_oneyearmonthlyav = sum(n[event_date >= Sys.Date()-365], na.rm=T)/12,
            C_ACLED_event_threeyearmonthlyav = sum(n[event_date >= Sys.Date()-1095], na.rm=T)/36,
            C_ACLED_event_same_month_difference = C_ACLED_event_last30d - C_ACLED_event_oneyear30d,
            C_ACLED_event_month_annual_difference = C_ACLED_event_last30d - C_ACLED_event_oneyearmonthlyav,
            C_ACLED_event_month_threeyear_difference = C_ACLED_event_last30d - C_ACLED_event_threeyearmonthlyav,
            C_ACLED_event_same_month_difference_perc = ((C_ACLED_event_last30d - C_ACLED_event_oneyear30d) / C_ACLED_event_oneyear30d) * 100,
            C_ACLED_event_month_annual_difference_perc = ((C_ACLED_event_last30d - C_ACLED_event_oneyearmonthlyav) / C_ACLED_event_oneyearmonthlyav) *100,
            C_ACLED_event_month_threeyear_difference_perc = ((C_ACLED_event_last30d - C_ACLED_event_threeyearmonthlyav) / C_ACLED_event_threeyearmonthlyav)*100)

#Join deaths and events
acledjoin <- full_join(fatal, event, by="iso3")
acledjoin <- acledjoin %>% 
  rename(Country = iso3)

#Normalise fatalities
aclednorm <- function(df,upperrisk, lowerrisk, col1, number){
  #Create new column col_name as sum of col1 and col2
  df[[paste0(col1, "_norm")]] <- ifelse(df[[col1]] >= upperrisk, 10,
                                        ifelse(df[[col1]] <= lowerrisk | df[[number]] <= 5, 0,
                                               ifelse(df[[col1]]  < upperrisk & df[[col1]]  > lowerrisk,  10 - (upperrisk - df[[col1]] )/(upperrisk - lowerrisk)*10, NA)
                                        ))
  df
}
acleddata <- aclednorm(acledjoin, 200, 0, "C_ACLED_fatal_same_month_difference_perc", "C_ACLED_fatal_last30d")
acleddata <- aclednorm(acleddata, 60, 0, "C_ACLED_fatal_month_annual_difference_perc", "C_ACLED_fatal_last30d")
acleddata <- aclednorm(acleddata, 200, 0, "C_ACLED_fatal_month_threeyear_difference_perc", "C_ACLED_fatal_last30d")

#Normalise events
aclednorm <- function(df,upperrisk, lowerrisk, col1, number){
  #Create new column col_name as sum of col1 and col2
  df[[paste0(col1, "_norm")]] <- ifelse(df[[col1]] >= upperrisk, 10,
                                        ifelse(df[[col1]] <= lowerrisk | df[[number]] <= 25, 0,
                                               ifelse(df[[col1]]  < upperrisk & df[[col1]]  > lowerrisk,  10 - (upperrisk - df[[col1]] )/(upperrisk - lowerrisk)*10, NA)
                                        ))
  df
}
acleddata <- aclednorm(acleddata, 500, 0, "C_ACLED_event_same_month_difference_perc", "C_ACLED_event_last30d")
acleddata <- aclednorm(acleddata, 60, 0, "C_ACLED_event_month_annual_difference_perc", "C_ACLED_event_last30d")
acleddata <- aclednorm(acleddata, 300, 0, "C_ACLED_event_month_threeyear_difference_perc", "C_ACLED_event_last30d")

write.csv(acleddata, "Indicator_Dataset/ACLEDnormalised.csv")

#------------------------------CREATE CONFLICT SHEET-------------------------------------------
conflictsheet <- full_join(gpi, acleddata, by="Country") 
write.csv(conflictsheet, "Risk_sheets/conflictsheet.csv")

#--------------------------------SOCIO-ECONOMIC DATA and SHEET------------------------------------------
#Load OCHA database
ocha <- read_csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/OCHA_socioeconomic.csv")
ocha <- ocha %>%
  select(-X1)
upperrisk <- quantile(ocha$S_OCHA_Covid.vulnerability.index, probs = c(0.95), na.rm=T)
lowerrisk <- quantile(ocha$S_OCHA_Covid.vulnerability.index, probs = c(0.05), na.rm=T)
ocha <- normfuncpos(ocha,upperrisk, lowerrisk, "S_OCHA_Covid.vulnerability.index") 
write.csv(ocha, "Risk_sheets/Socioeconomic_sheet.csv")

#-------------------------------NATURAL HAZARDS SHEET------------------------------------------------
#Load UKMO dataset
nathaz <- read_csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/naturalhazards.csv")
nathaz <- nathaz %>%
  select(-X1) %>%
  rename(Country = NH_UKMO_Country)
upperrisk <- quantile(nathaz$NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS, probs = c(0.95), na.rm=T)
lowerrisk <- quantile(nathaz$NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS, probs = c(0.05), na.rm=T)
nathaz <- normfuncpos(nathaz,upperrisk, lowerrisk, "NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS") 
upperrisk <- quantile(nathaz$NH_UKMO_TOTAL.RISK.NEXT.12.MONTHS, probs = c(0.95), na.rm=T)
lowerrisk <- quantile(nathaz$NH_UKMO_TOTAL.RISK.NEXT.12.MONTHS, probs = c(0.05), na.rm=T)
nathaz <- normfuncpos(nathaz,upperrisk, lowerrisk, "NH_UKMO_TOTAL.RISK.NEXT.12.MONTHS") 

#Load GDACS database
gdacweb <- "https://www.gdacs.org/"
gdac <- read_html(gdacweb)

names <- c(".alert_EQ_Green",  ".alert_EQ_PAST_Green",".alert_EQ_Orange", ".alert_EQ_PAST_Orange",
           ".alert_TC_Green",  ".alert_TC_PAST_Green",".alert_TC_Orange", ".alert_TC_PAST_Orange",
           ".alert_FL_Green",  ".alert_FL_PAST_Green",".alert_FL_Orange", ".alert_FL_PAST_Orange",
           ".alert_VO_Green",  ".alert_VO_PAST_Green",".alert_VO_Orange", ".alert_VO_PAST_Orange",
           ".alert_DR_Green",  ".alert_DR_PAST_Green",".alert_DR_Orange", ".alert_DR_PAST_Orange")

#Function to create database with hazard specific information
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

#Labels
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

#Earthquake
eq1 <- rbind(haz[[1]], haz[[2]])
eq1$haz <- paste("green")
eq2 <- rbind(haz[[3]], haz[[4]])
eq2$haz <- paste("orange")
eq <- rbind(eq1, eq2)
eq$hazard <- "earthquake"


#Cyclone
cy1 <- rbind(haz[[5]], haz[[6]])
cy1$haz <- paste("green")
cy2 <- rbind(haz[[7]], haz[[8]])
cy2$haz <- paste("orange")
cy <- rbind(cy1, cy2)
cy$hazard <- "cyclone"

#Flood
fl1 <- rbind(haz[[9]], haz[[10]])
fl1$haz <- paste("green")
fl2 <- rbind(haz[[11]], haz[[12]])
fl2$haz <- paste("orange")
fl <- rbind(fl1, fl2)
fl$hazard <- "flood"

#Volcano
vo1 <- rbind(haz[[13]], haz[[14]])
vo1$haz <- paste("green")
vo2 <- rbind(haz[[15]], haz[[16]])
vo2$haz <- paste("orange")
vo <- rbind(vo1, vo2)
vo$hazard <- "volcano"
vo$names <- sub(".*in ", "", vo$names)

#Drought
dr1 <- rbind(haz[[17]], haz[[18]])
dr1$haz <- paste("green")
dr2 <- rbind(haz[[19]], haz[[20]])
dr2$haz <- paste("orange")
dr <- rbind(dr1, dr2)
dr$hazard <- "drought"
dr$date <- str_sub(dr$names, start= -4)
dr$names <- gsub('.{5}$', '', dr$names)

#Combine into one dataframe
gdaclist <- rbind.data.frame(eq, cy,fl, vo, dr)

#change times
gdaclist$date <- ifelse(gdaclist$hazard != c("drought")  & gdaclist$status == "active" , paste(as.Date(parse_date_time(gdaclist$date, orders=c("dm HM")))),
                        ifelse(gdaclist$hazard == c("drought"), paste(gdaclist$date),
                               paste(as.Date(parse_date_time(gdaclist$date, orders=c("dmy"))))))

#Remove duplicate countries for drought
gdaclist$names <- as.character(gdaclist$names)
add <- gdaclist[which(gdaclist$hazard == "drought" & grepl("-", gdaclist$names)),]
gdaclist[which(gdaclist$hazard == "drought" & grepl("-", gdaclist$names)),]$names <- sub("-.*", "", gdaclist[which(gdaclist$hazard == "drought" & grepl("-", gdaclist$names)),]$names)
add$names <- sub(".*-", "", add$names)
gdaclist <- rbind(gdaclist, add)

#Drought orange
gdaclist$status <- ifelse(gdaclist$hazard == "drought" & gdaclist$date == "2020", "active", gdaclist$status)

#Country names
gdaclist$namesiso <- countrycode(gdaclist$names, origin = 'country.name', destination = 'iso3c')
gdaclist$namesfull <- countrycode(gdaclist$names, origin = 'country.name', destination = 'iso3c', nomatch = NULL)

#Create subset
gdac <- gdaclist %>%
  select(date, status, haz, hazard, namesiso)

colnames(gdac) <- c("NH_GDAC_Date", "NH_GDAC_Hazard_Status", "NH_GDAC_Hazard_Severity", "NH_GDAC_Hazard_Type", "Country")

gdac <- gdac %>%
  mutate(NH_GDAC_Hazard_Score_Norm = case_when(NH_GDAC_Hazard_Status == "active" & NH_GDAC_Hazard_Severity == "orange" ~ 10,
                                               TRUE ~ 0)) %>%
  drop_na(Country)

write.csv(gdac, "Indicator_dataset/gdaclistnormalised.csv")

#INFORM CRISIS TRACKER
informcrisis <- read_csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/INFORM_Crisis_raw.csv")

#Add duplicates
informcrisis <- informcrisis %>%
  select(-X1, Country, NH_INFORM_Crisis_name, NH_INFORM_Crisis_Severity_Score, NH_INFORM_Crisis_Type_Number) %>%
  mutate(Country = strsplit(as.character(Country), ",")) %>%
  unnest() %>% 
  filter(Country != "") %>%
  select(Country, NH_INFORM_Crisis_name:NH_INFORM_Crisis_Type_Number) %>%
  mutate(Country = trimws(Country),
         NH_INFORM_Crisis_Norm = case_when(NH_INFORM_Crisis_Type_Number == 1 ~ 10, 
                                           TRUE ~ 0)) 

write.csv(informcrisis, "Indicator_dataset/INFORM_Crisis_normalised.csv")

#-------------------------------------------CREATE NATURAL HAZARD SHEET------------------------------
nathazardfull <- full_join(nathaz, gdac, by="Country") %>%
  full_join(., informcrisis)
write.csv(nathazardfull, "Risk_sheets/Naturalhazards.csv")


  