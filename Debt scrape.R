#--------------------LOAD PACKAGES--------------
#install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(ggplot2, cowplot, lubridate, rvest,dplyr, viridis, tidyverse, countrycode)

#---------------------SCRAPE DEBT DATA---------------------
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

write.csv(debttab, "Indicator_dataset/Debtfile.csv")

