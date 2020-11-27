######################################################################################################
#
#  Script to extract Country summaries from World Bank portal
#
######################################################################################################

#--------------------LOAD PACKAGES-----------------------------------------
# install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(
  cowplot, lubridate, rvest, viridis, countrycode,
  clipr, awalker89 / openxlsx, dplyr, tidyverse, readxl,
  gsheet, zoo, wppExplorer, haven, EnvStats, jsonlite
)

# Function to extract all
text <- function(tt){
  lapply(tt, function(country){
    #Start by omitting all entries where there is an error with the webpage
    if( identical(character(0), 
                  {
                    gdacweb <- paste0("https://www.worldbank.org/en/country/", country, "/overview")
                    gdac <- read_html(gdacweb)
                    gdac %>%
                      html_nodes(".flipboard-keep") %>%
                      html_text()
                  }))
      paste(NA) 
    else {
      gdacweb <- paste0("https://www.worldbank.org/en/country/", country, "/overview")
      gdac <- read_html(gdacweb)
      #Create list with countries
      country_list <- list()
      country_list[[1]] <- country
      #Extract WB information
      country_text <- gdac %>%
        html_nodes(".flipboard-keep") %>%
        html_text()
      # Add text
      country_list[[2]] <- country_text[[1]]
      # Add website
      country_list[[3]] <- gdacweb
      # Return list with all info
      return(country_list)
    }
  })
}

# Load countries in the database
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")
countrylist <- countrylist %>%
  select(-X)

# Extract all + those that don't render correctly on the WB site
missing_names <- c("drc", "png", "venezuela", "brunei-darussalam", "cambodia", "moldova",
                   "lao", "guineabissau", "cotedivoire", "unitedstates", "korea", "kyrgyzrepublic",
                   "russia")
country_overview <- text(c(tolower(countrylist$Countryname), missing_names))

#Remove NAs
country <- lapply(c(1:203), function(x) {country_overview[[x]][[1]]})
none <- unlist(country)
list <- which(!none == "NA")
country_overview <- country_overview[list]

# Create dataframe
country_descrip <- as.data.frame(matrix(unlist(country_overview), nrow=length(country_overview), byrow=TRUE))

# Convert countries
country_descrip <- country_descrip %>% 
  mutate(Country  = countrycode(V1,
                                origin = "country.name",
                                destination = "iso3c",
                                nomatch = NULL
  ))

# Load FCV and IDA country list
fcv <- read.csv("Indicator_dataset/Country_classification.csv") %>%
  select(-X, Countryname)

#Create new dataset with IDA and FCV list
full_text_desc <- left_join(countrylist %>% select(-Countryname), country_descrip, by = "Country") %>%
  left_join(., fcv, by = "Country") %>%
  mutate(
    FCV_status = case_when(is.na(FCV_status) ~ "Country not classed as FCV",
                           TRUE ~ as.character(FCV_status)),
    IDA.status = case_when(is.na(IDA.status) ~ "Non-IDA recipient",
                           IDA.status == "Yes" ~ "IDA-eligible",
                           TRUE ~ as.character(IDA.status)),
    Narrative = trimws(
      paste(trimws(paste0("FCV Status: ", FCV_status), which = "left"),
            trimws(paste0("IDA Status: ", IDA.status), which = "left"), 
            trimws(V2, which = "left"),
            sep= "\n \n"), 
      which = "left")
  ) %>%
  select(-V1, -V2, -FCV_status, -IDA.status)

#Write to xls
write.xlsx(full_text_desc, "Indicator_dataset/full_text_desc.xlsx")

# To Find Remaining countries
#countrynam <- tolower(countrylist$Countryname)
#country <- lapply(c(1:190), function(x) {country_overview[[x]][[1]]})
#none <- unlist(country)
#none <- country_nam[which(!country_nam %in% unlist(country))]


