#--------------------LOAD PACKAGES-----------------------------------------
#install.packages("librarian")     #Run if librarian is not already installed (choose not to install dependencies from source)
librarian::shelf(cowplot, lubridate, rvest, viridis, countrycode,
                 clipr, awalker89/openxlsx, dplyr, tidyverse, readxl)

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
  
HIS <- normfuncneg(HIS, 20, 70, "H_HIS_Score")

#-----------------------Oxford rollback Score-----------------
OXrollback <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-scratchpad/master/rollback_checklist/rollback_checklist.csv")

colnames(OXrollback) <- paste0("H_", colnames(OXrollback))

OXrollback <- OXrollback %>%
  rename(H_Oxrollback_score = H_overall_checklist,
         Countryname = H_countryname) %>%
  mutate(Country = countrycode(Countryname, 
                               origin = 'country.name',
                               destination = 'iso3c', 
                               nomatch = NULL))

upperrisk <- quantile(OXrollback$H_Oxrollback_score, probs = c(0.9), na.rm=T)
lowerrisk <- quantile(OXrollback$H_Oxrollback_score, probs = c(0.1), na.rm=T)

OXrollback <- normfuncpos(OXrollback, upperrisk, lowerrisk, "H_Oxrollback_score")

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
                                    "H_Covidproj_Additional Deaths - Mean","Additional Deaths / 1M",  "H_Covidproj_Additional Deaths (% of Current Deaths)", 
                                    "H_Covidproj_Projected Deaths - 2.5th Percentile", "H_Covidproj_Projected Deaths - 97.5th Percentile")
colnames(covidproj) <- gsub(" ", "_", colnames(covidproj))

#Add normalised values
upperrisk <- quantile(covidproj$`H_Covidproj_Projected_Deaths_/_1M`, probs = c(0.80), na.rm=T)
lowerrisk <- quantile(covidproj$`H_Covidproj_Projected_Deaths_/_1M`, probs = c(0.10), na.rm=T)
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
  summarise(meandeaths = mean(new_deaths_per_million, na.rm=T),
            meancase = mean(new_cases_per_million, na.rm=T)) %>%
  group_by(iso_code) %>%
  filter(!is.na(meandeaths) & !is.na(meancase)) %>%
  mutate(growthdeath =  meandeaths[previous2week == 'twoweek'] - meandeaths,
         growthratedeaths = case_when(meandeaths[previous2week == 'lasttwoweek'] == 0 ~ 0.01,
                                      meandeaths > 0 ~ growthdeath/meandeaths[previous2week == 'lasttwoweek'] *100,
                                      TRUE ~ NA_real_),
         growthcase =  meancase[previous2week == 'twoweek'] - meancase,
         growthratecases = case_when(meandeaths[previous2week == 'lasttwoweek'] == 0 ~ 0.01, 
                                     meancase > 0 ~ growthcase/meancase[previous2week == 'lasttwoweek'] *100,
                                     TRUE ~ NA_real_)) %>%
  dplyr::filter(previous2week != "twoweek") %>%
  select(- previous2week, -growthcase, -growthdeath, -meandeaths, -meancase)

#Normalised scores for deaths
covidgrowth <- normfuncpos(covidgrowth, 150, 0, "growthratedeaths")
covidgrowth <- normfuncpos(covidgrowth, 150, 0, "growthratecases")

#Rename columns
colnames(covidgrowth) <- c("Country", "H_Covidgrowth_biweeklydeaths", "H_Covidgrowth_biweeklycases",
                           "H_Covidgrowth_deathsnorm", "H_Covidgrowth_casesnorm")

#Varibles on number of cases
covidcurrent <- covid %>%
  filter(date== Sys.Date()-1) %>%
  select(iso_code, new_cases_smoothed_per_million, new_deaths_smoothed_per_million) %>%
  rename(Country = iso_code)

covidcurrent <-  normfuncpos(covidcurrent, 50, 0, "new_cases_smoothed_per_million")
covidcurrent <-  normfuncpos(covidcurrent, 2, 0, "new_deaths_smoothed_per_million")

#Rename columns
colnames(covidcurrent) <- c("Country", "H_new_cases_smoothed_per_million", "H_new_deaths_smoothed_per_million", 
                            "H_new_cases_smoothed_per_million_norm", "H_new_deaths_smoothed_per_million_norm"
)

#----------------------------------Create combined Health Sheet-------------------------------------------
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")
countrylist <- countrylist %>% 
  select(-X)

health <- left_join(countrylist, HIS, by="Country") %>%
  left_join(., OXrollback, by=c("Country", "Countryname")) %>%
  left_join(., covidproj,  by="Country") %>% 
  left_join(., covidgrowth, by="Country") %>%
  left_join(., covidcurrent, by="Country") %>%
  arrange(Country)

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

upperrisk <- quantile(proteus$F_Proteus_Score, probs = c(0.90), na.rm=T)
lowerrisk <- quantile(proteus$F_Proteus_Score, probs = c(0.10), na.rm=T)
proteus <- normfuncpos(proteus,upperrisk, lowerrisk, "F_Proteus_Score") 

#Artemis
artemis <- read.csv("~/Google Drive/PhD/R code/Compound Risk/artemis.csv")

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
countrylist <- countrylist %>% 
  select(-X)

foodsecurity <- left_join(countrylist, proteus, by="Country") %>% 
  left_join(., fews, by="Country") %>%
  left_join(., fpv,  by="Country") %>% 
  left_join(., artemis, by="Country") %>%
  arrange(Country)

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

imfdebt <- normfuncneg(imfdebt,-5, 0, "D_IMF_debt2020.2019") 

#IGC database
igc <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1s46Oz_NtkyrowAracg9FTPYRdkP8GHRzQqM7w6Qa_2k/edit?ts=5e836726#gid=0")

#Header as first row
names(igc) <- as.matrix(igc[1, ])
igc <- igc[-1, ]
igc[] <- lapply(igc, function(x) type.convert(as.character(x)))

#Name fiscal column
colnames(igc)[which(colnames(igc)=="Announced/ estimated fiscal support package\n") +1] <- "fiscalgdp"
igc <- igc[-which(is.na(colnames(igc)))]

#Extract numbers before the % text
igc$fiscalgdpnum <- sub("\\%.*", "", igc$fiscalgdp)
igc$fiscalgdpnum <- as.numeric(as.character(igc$fiscalgdpnum))

#Normalised scores
upperrisk <- quantile(igc$fiscalgdpnum, probs = c(0.95), na.rm=T)
lowerrisk <- quantile(igc$fiscalgdpnum, probs = c(0.05), na.rm=T)
igc <- normfuncpos(igc, upperrisk, lowerrisk, "fiscalgdpnum") 

#Add label tabs 
colnames(igc) <- paste0("D_", colnames(igc))
igc <- as.data.frame(igc)
igc <- igc[-which(colnames(igc)=="D_Other reputable links" )]

#-------------------------CREATE DEBT SHEET-----------------------------------
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")
countrylist <- countrylist %>% 
  select(-X)

debtsheet <- left_join(countrylist, debttab, by="Country") %>%
  left_join(., imfdebt , by="Country") %>%
  left_join(., igc, by="D_iso3c") %>%
  arrange(Country)

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

#COVID Economic Stimulus Index
#Load file
url <- "http://web.boun.edu.tr/elgin/CESI_11.xlsx" #Note: may need to check for more recent versions
destfile <- "Indicator_dataset/cesiraw.xlsx"
curl::curl_download(url, destfile)
cesi <- read_excel(destfile)

colnames(cesi) <- paste0("M_", colnames(cesi))
cesi <- cesi %>%
  mutate(Country = countrycode(M_Country, 
                        origin = 'country.name',
                        destination = 'iso3c', 
                        nomatch = NULL)) 

#Perform PCA
cesipca <- prcomp(cesi %>% select(M_fiscal_11, M_ratecut_11, M_reserve_req_11, M_macrofin_11, M_othermonetary_11, M_bopgdp_11, M_otherbop_11), center = TRUE,scale. = TRUE)

#Assign CESI Index as the first two PCs
cesi$M_CESI_Index <- cesipca$x[,1] + cesipca$x[,2]

#Normalised scores
upperrisk <- quantile(cesi$M_CESI_Index, probs = c(0.1), na.rm=T)
lowerrisk <- quantile(cesi$M_CESI_Index, probs = c(0.95), na.rm=T)
cesi <- normfuncneg(cesi, upperrisk, lowerrisk, "M_CESI_Index") 

#-----------------------------CREATE MACRO SHEET-----------------------------------------
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")
countrylist <- countrylist %>% 
  select(-X)

macrosheet <- left_join(countrylist, macro, by="Country") %>%
  left_join(., gdp, by="Country") %>%
  left_join(., cesi, by="Country") %>%
  arrange(Country)

write.csv(macrosheet, "Risk_sheets/macrosheet.csv")

#--------------------------------FRAGILITY DATA-----------------------------------------
fsi <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/FSI.csv")
fsi <- fsi %>% 
  select(-X) %>%
  drop_na(Country)

upperrisk <- quantile(fsi$Fr_FSI_Score , probs = c(0.9), na.rm=T)
lowerrisk <- quantile(fsi$Fr_FSI_Score , probs = c(0.1), na.rm=T)
fsi <- normfuncpos(fsi,upperrisk, lowerrisk, "Fr_FSI_Score") 

upperrisk <- quantile(fsi$Fr_FSI_2019minus2020, probs = c(0.1), na.rm=T)
lowerrisk <- quantile(fsi$Fr_FSI_2019minus2020, probs = c(0.9), na.rm=T)
#Specific normalisation
fsinormneg <- function(df,upperrisk, lowerrisk, col1){
  #Create new column col_name as sum of col1 and col2
  df[[paste0(col1, "_norm")]] <- ifelse(df[[col1]] <= upperrisk, 10,
                                        ifelse(df[[col1]] >= lowerrisk | df$Fr_FSI_Score_norm == 0, 0,
                                               ifelse(df[[col1]]  > upperrisk & df[[col1]]  < lowerrisk,  10 - (upperrisk - df[[col1]] )/(upperrisk - lowerrisk)*10, NA)
                                        ))
  df
}
fsi <- fsinormneg(fsi,upperrisk, lowerrisk, "Fr_FSI_2019minus2020") 


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
  dplyr::group_by(country) %>%
  dplyr::summarise(Fr_REIGN_couprisk3m = mean(couprisk, na.rm=T)) %>%
  dplyr::rename(Country = country) %>%
  dplyr::mutate(Country = countrycode(Country, 
                               origin = 'country.name',
                               destination = 'iso3c', 
                               nomatch = NULL))

upperrisk <- quantile(reign$Fr_REIGN_couprisk3m, probs = c(0.95), na.rm=T)
lowerrisk <- quantile(reign$Fr_REIGN_couprisk3m, probs = c(0.05), na.rm=T)

reign <- normfuncpos(reign,upperrisk, lowerrisk, "Fr_REIGN_couprisk3m") 

#Load GPI data
gpi <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/GPI.csv")
gpi <- gpi %>% 
  select(-X) %>%
  mutate(Country = countrycode(Country, 
                               origin = 'country.name',
                               destination = 'iso3c', 
                               nomatch = NULL)) %>%
  rename(Fr_GPI_Score = C_GPI_Score)
upperrisk <- quantile(gpi$Fr_GPI_Score, probs = c(0.90), na.rm=T)
lowerrisk <- quantile(gpi$Fr_GPI_Score, probs = c(0.10), na.rm=T)
gpi <- normfuncpos(gpi, upperrisk, lowerrisk, "Fr_GPI_Score")

#Load ACLED data  
acled <- read_csv("~/Google Drive/PhD/R code/Compound Risk/ACLEDraw.csv")

#summarise deaths
fatal <- acled %>%
  select(iso3, fatalities, event_date, event_type) %>%
  mutate(event_date = as.Date(parse_date_time(acled$event_date, orders = "dmy"))) %>%
  group_by(iso3, event_date) %>%
  tally(fatalities) %>%
  summarise(Fr_ACLED_fatal_last30d = sum(n[event_date >= max(event_date) - 30]),
            Fr_ACLED_fatal_oneyear30d = sum(n[event_date >= max(event_date) -395 & event_date <= max(event_date) -365]),
            Fr_ACLED_fatal_oneyearmonthlyav = sum(n[event_date >= max(event_date) -365], na.rm=T)/12,
            Fr_ACLED_fatal_threeyearmonthlyav = sum(n[event_date >= max(event_date) -1095], na.rm=T)/36,
            Fr_ACLED_fatal_same_month_difference = Fr_ACLED_fatal_last30d - Fr_ACLED_fatal_oneyear30d,
            Fr_ACLED_fatal_month_annual_difference = Fr_ACLED_fatal_last30d - Fr_ACLED_fatal_oneyearmonthlyav,
            Fr_ACLED_fatal_month_threeyear_difference = Fr_ACLED_fatal_last30d - Fr_ACLED_fatal_threeyearmonthlyav,
            Fr_ACLED_fatal_same_month_difference_perc = case_when(Fr_ACLED_fatal_oneyear30d == 0 ~ 0.01,
                                                                 Fr_ACLED_fatal_oneyear30d > 0 ~ ((Fr_ACLED_fatal_last30d - Fr_ACLED_fatal_oneyear30d) / Fr_ACLED_fatal_oneyear30d) * 100,
                                                                 TRUE ~ NA_real_),
            Fr_ACLED_fatal_month_annual_difference_perc = case_when(Fr_ACLED_fatal_oneyearmonthlyav == 0 ~ 0.01,
                                                                   Fr_ACLED_fatal_oneyearmonthlyav > 0 ~ ((Fr_ACLED_fatal_last30d - Fr_ACLED_fatal_oneyearmonthlyav) / Fr_ACLED_fatal_oneyearmonthlyav) *100,
                                                                   TRUE ~ NA_real_),
            Fr_ACLED_fatal_month_threeyear_difference_perc = case_when(Fr_ACLED_fatal_threeyearmonthlyav ==0 ~ 0.01,
                                                                      Fr_ACLED_fatal_threeyearmonthlyav > 0 ~ ((Fr_ACLED_fatal_last30d - Fr_ACLED_fatal_threeyearmonthlyav) / Fr_ACLED_fatal_threeyearmonthlyav)*100,
                                                                      TRUE ~ NA_real_))

#summarise events
event <- acled %>%
  select(iso3, fatalities, event_date, event_type, event_id_cnty) %>%
  mutate(event_date = as.Date(parse_date_time(acled$event_date, orders = "dmy"))) %>%
  group_by(iso3, event_date) %>%
  count(event_id_cnty) %>%
  group_by(iso3) %>%
  summarise(Fr_ACLED_event_last30d = sum(n[event_date >= max(event_date) -30]),
            Fr_ACLED_event_oneyear30d = sum(n[event_date >= max(event_date) -395 & event_date <= max(event_date) -365]),
            Fr_ACLED_event_oneyearmonthlyav = sum(n[event_date >= max(event_date) -365], na.rm=T)/12,
            Fr_ACLED_event_threeyearmonthlyav = sum(n[event_date >= max(event_date) -1095], na.rm=T)/36,
            Fr_ACLED_event_same_month_difference = Fr_ACLED_event_last30d - Fr_ACLED_event_oneyear30d,
            Fr_ACLED_event_month_annual_difference = Fr_ACLED_event_last30d - Fr_ACLED_event_oneyearmonthlyav,
            Fr_ACLED_event_month_threeyear_difference = Fr_ACLED_event_last30d - Fr_ACLED_event_threeyearmonthlyav,
            Fr_ACLED_event_same_month_difference_perc = case_when(Fr_ACLED_event_oneyear30d == 0 ~ 0.01,
                                                                 Fr_ACLED_event_oneyear30d > 0 ~ ((Fr_ACLED_event_last30d - Fr_ACLED_event_oneyear30d) / Fr_ACLED_event_oneyear30d) * 100,
                                                                 TRUE ~ NA_real_),
            Fr_ACLED_event_month_annual_difference_perc = case_when(Fr_ACLED_event_oneyearmonthlyav == 0 ~ 0.01,
                                                                   Fr_ACLED_event_oneyearmonthlyav > 0 ~ ((Fr_ACLED_event_last30d - Fr_ACLED_event_oneyearmonthlyav) / Fr_ACLED_event_oneyearmonthlyav) *100,
                                                                   TRUE ~ NA_real_),
            Fr_ACLED_event_month_threeyear_difference_perc = case_when(Fr_ACLED_event_threeyearmonthlyav == 0 ~ 0.01,
                                                                      Fr_ACLED_event_threeyearmonthlyav > 0 ~ ((Fr_ACLED_event_last30d - Fr_ACLED_event_threeyearmonthlyav) / Fr_ACLED_event_threeyearmonthlyav)*100,
                                                                      TRUE ~ NA_real_))

#Join deaths and events
acledjoin <- full_join(fatal, event, by="iso3")
acledjoin <- acledjoin %>% 
  rename(Country = iso3)

#Normalise fatalities
#Normalise fatalities
aclednorm <- function(df,upperrisk, lowerrisk, col1, number){
  #Create new column col_name as sum of col1 and col2
  df[[paste0(col1, "_norm")]] <- ifelse(df[[col1]] >= upperrisk, 10,
                                        ifelse(df[[col1]] <= lowerrisk | df[[number]] <= 5, 0,
                                               ifelse(df[[col1]]  < upperrisk & df[[col1]]  > lowerrisk,  10 - (upperrisk - df[[col1]] )/(upperrisk - lowerrisk)*10, NA)
                                        ))
  df
}
acleddata <- aclednorm(acledjoin, 300, 0, "Fr_ACLED_fatal_same_month_difference_perc", "Fr_ACLED_fatal_last30d")
acleddata <- aclednorm(acleddata, 300, 0, "Fr_ACLED_fatal_month_annual_difference_perc", "Fr_ACLED_fatal_last30d")
acleddata <- aclednorm(acleddata, 600, 0, "Fr_ACLED_fatal_month_threeyear_difference_perc", "Fr_ACLED_fatal_last30d")

#Normalise events
aclednorm <- function(df,upperrisk, lowerrisk, col1, number){
  #Create new column col_name as sum of col1 and col2
  df[[paste0(col1, "_norm")]] <- ifelse(df[[col1]] >= upperrisk, 10,
                                        ifelse(df[[col1]] <= lowerrisk | df[[number]] <= 25, 0,
                                               ifelse(df[[col1]]  < upperrisk & df[[col1]]  > lowerrisk,  10 - (upperrisk - df[[col1]] )/(upperrisk - lowerrisk)*10, NA)
                                        ))
  df
}
acleddata <- aclednorm(acleddata, 400, 0, "Fr_ACLED_event_same_month_difference_perc", "Fr_ACLED_event_last30d")
acleddata <- aclednorm(acleddata, 400, 0, "Fr_ACLED_event_month_annual_difference_perc", "Fr_ACLED_event_last30d")
acleddata <- aclednorm(acleddata, 800, 0, "Fr_ACLED_event_month_threeyear_difference_perc", "Fr_ACLED_event_last30d")

write.csv(acleddata, "Indicator_Dataset/ACLEDnormalised.csv")

#-------------------------------------FRAGILITY SHEET--------------------------------------
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")
countrylist <- countrylist %>% 
  select(-X)

fragilitysheet <- left_join(countrylist, fsi, by="Country")  %>%
  left_join(., informfragile , by="Country")  %>%
  left_join(., reign,  by="Country") %>%
  left_join(., gpi, by="Country") %>%
  left_join(., acleddata, by="Country") %>%
  arrange(Country)

write.csv(fragilitysheet, "Risk_sheets/fragilitysheet.csv")

#--------------------------------SOCIO-ECONOMIC DATA and SHEET------------------------------------------
#Load OCHA database
ocha <- read_csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/OCHA_socioeconomic.csv")
ocha <- ocha %>%
  select(-X1) %>%
  arrange(Country)

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
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")
countrylist <- countrylist %>% 
  select(-X)

nathazardfull <- left_join(countrylist, nathaz, by="Country") %>%
  left_join(., gdac, by="Country") %>%
  left_join(., informcrisis) %>%
  distinct(Country, .keep_all = TRUE) %>%
  drop_na(Country) %>%
  arrange(Country)

write.csv(nathazardfull, "Risk_sheets/Naturalhazards.csv")










  