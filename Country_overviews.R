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
        gsub( "’", "'",. ) %>%
        as.character() %>%
        iconv(., "UTF-8", "UTF-8")
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
  dplyr::select(-X)

# Extract all + those that don't render correctly on the WB site
missing_names <- c("drc", "png", "venezuela", "brunei-darussalam", "cambodia", "moldova",
                   "lao", "guineabissau", "cotedivoire", "unitedstates", "korea", "kyrgyzrepublic",
                   "russia")
country_overview <- text(c(str_replace(tolower(countrylist$Countryname), " ", ""), missing_names))

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

#Function to get regional overviews
reg_overview <- function(cc){
  gdacweb <- paste0("https://www.worldbank.org/en/country/", cc, "/overview")
  gdac <- read_html(gdacweb)
  #Create list with countries
  country_list <- list()
  country_list[[1]] <- country
  #Extract WB information
  country_text <- gdac %>%
    html_nodes(".flipboard-keep") %>%
    gsub( "’", "'",. ) %>%
    as.character() %>%
    iconv(., "UTF-8", "UTF-8")
  return(country_text[[1]])
}

EAP_sum <- reg_overview("pacificislands")
LAC_sum <- reg_overview("caribbean") 
MENA_sum <- reg_overview("gcc") 
ECA_sum <- reg_overview("eu")

# Replace missing country pages with a regional overview
country_descrip <- left_join(countrylist %>% dplyr::select(-Countryname), 
                             country_descrip, 
                             by = "Country") %>%
  mutate(EAP = "https://www.worldbank.org/en/country/pacificislands/overview",
         LAC = "https://www.worldbank.org/en/country/caribbean/overview",
         MENA = "https://www.worldbank.org/en/country/gcc/overview",
         ECA = "https://www.worldbank.org/en/country/eu/overview",
         Region = countrycode(Country, 
                              origin = "iso3c", 
                              destination = "region",
                              nomatch = NULL),
         V3 = case_when(is.na(as.character(V3)) & as.character(Region) == "East Asia & Pacific" ~ EAP,
                                   is.na(as.character(V3)) & as.character(Region) == "Europe & Central Asia" ~ ECA,
                                   is.na(as.character(V3)) & as.character(Region) == "Middle East & North Africa" ~ MENA,
                                   is.na(as.character(V3)) & as.character(Region) == "Latin America & Caribbean" ~ LAC,
                                   !is.na(as.character(V3)) ~ as.character(V3),
                                   TRUE ~ as.character(V3)
                                   ),
         V2 = case_when(is.na(as.character(V3)) & as.character(Region) == "East Asia & Pacific" ~ EAP_sum,
                        is.na(as.character(V3)) & as.character(Region) == "Europe & Central Asia" ~ ECA_sum,
                        is.na(as.character(V3)) & as.character(Region) == "Middle East & North Africa" ~ MENA_sum,
                        is.na(as.character(V3)) & as.character(Region) == "Latin America & Caribbean" ~ LAC_sum,
                        !is.na(as.character(V3)) ~ as.character(V2),
                        TRUE ~ as.character(V3)
         )) %>%
  dplyr::select(-EAP, -LAC, -MENA, -ECA, -Region)

# Load FCV and IDA country list
fcv <- read.csv("Indicator_dataset/Country_classification.csv") %>%
  dplyr::select(-X, Countryname, -IDA.status)

ida <- suppressWarnings(read_csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Annotation%20and%20Data%20Architecture/CLASS.csv", 
                skip = 4))

ida <- ida %>% 
  filter(Economy != "x") %>%
  dplyr::select(Code, `Lending category`) %>%
  rename(Country = Code,
         Lending_category = `Lending category`)

#Create new dataset with IDA and FCV list
full_text_desc <- left_join(country_descrip, fcv, by = "Country") %>%
  left_join(., ida, by = "Country") %>%
  mutate(
    FCV_status = case_when(is.na(FCV_status) ~ "Country not classed as FCV",
                           TRUE ~ as.character(FCV_status)),
    Narrative = trimws(
      paste(trimws(paste0("<p> <b>", "FCV Status: ","</b>", FCV_status, "</p>"), which = "left"),
            trimws(paste0("<p> <b>","Lending Category: ", "</b>", Lending_category, "</p>"), which = "left"), 
            trimws(paste0("<p> <b> <a href=", V3, 'target ="_blank">World Bank Country Page </b> </a> </p>'), which = "left"),
            trimws(paste("<p>", V2, "</p>"), which = "left"),
            sep= "\n \n"), 
      which = "left")
  ) %>%
  dplyr::select(-V1, -V2, -FCV_status, -Lending_category)




#Write to xls
write.csv(full_text_desc, "Annotation and Data Architecture/country_summaries_html.csv")

# To Find Remaining countries
#countrynam <- tolower(countrylist$Countryname)
#country <- lapply(c(1:190), function(x) {country_overview[[x]][[1]]})
#none <- unlist(country)
#none <- country_nam[which(!country_nam %in% unlist(country))]


