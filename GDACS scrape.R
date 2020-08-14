#-------------------Load libraries--------------------
#install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(lubridate, rvest,dplyr, viridis, tidyverse, countrycode)

#-----------------SCRAPE GDACS SITE------------------
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

#Write csv file
write.csv(gdaclist, "gdaclist.csv")