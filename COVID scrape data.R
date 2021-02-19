#-------------------Load libraries--------------------
#install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(ggplot2, cowplot, lubridate, rvest,dplyr, viridis, tidyverse, countrycode)

#------------------SCRAPE COVID PROJECTIONS-------------
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
covidproj$Country <- countrycode(covidproj$Country, origin = 'country.name', destination = 'iso3c', nomatch = NULL)

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

#Write csv
write.csv(covidproj, "Indicator_dataset/covidproj.csv")





