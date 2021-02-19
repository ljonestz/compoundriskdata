#-------------------Load libraries--------------------
#install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(ggplot2, cowplot, lubridate, rvest,dplyr, viridis, tidyverse, countrycode)

#---------------------- Scrape food price data from FAO ---------------------------------
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
faoprice <- faoprice %>% filter(faoprice$Country!="Source: Numbeo.com\n")
#Convert countries
faoprice$Country <- countrycode(faoprice$Country, origin = 'country.name', destination = 'iso3c')

#Save as csv
write.csv(faoprice, "Indicator_dataset/faoprice.csv")
