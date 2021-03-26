# 0. SET VARIABLES
# Search for VARIABLE and move them here

# 1. LOAD EVERYTHING IN ----

# Load full countries list (alternative is to use countries in globalrisk)
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Indicator_dataset/countrylist.csv")
countrylist <- countrylist %>%
  dplyr::select(-X) %>% 
  arrange(Country)

# Load dimension data from github
healthsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/healthsheet.csv")
foodsecurity <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/foodsecuritysheet.csv")
debtsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/debtsheet.csv")
fragilitysheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/fragilitysheet.csv")
macrosheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/macrosheet.csv")
Naturalhazardsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/Naturalhazards.csv")
Socioeconomic_sheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/Socioeconomic_sheet.csv")
acapssheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/acapssheet.csv")

# Socioecomic sheet is missing country name, add in (prob move to script that writes sheet initially)
# Socioeconomic_sheet <-  left_join(countrylist, Socioeconomic_sheet, by = "Country") %>%
# add_column(Rank = seq(), .before = "Countryname")

# Load dimension / data dictionary
indicators <- as.data.frame(read_csv("indicators.csv"))

# Compile list of all dimension data for Map function
# List names should match dimension names in `indicator` dataframe
# unique(indicators$dimension)
sheetList <- list("Health" = healthsheet,
                  "Food Security" = foodsecurity,
                  # "Debt" = debtsheet,
                  "Conflict and Fragility" = fragilitysheet, # Technical note uses various names; what do we want to use? Fragility and Conflict, Conflict and Fragility, Conflict Fragility and Institutional Risk
                  "Macro Fiscal" = macrosheet,
                  "Natural Hazard" = Naturalhazardsheet,
                  "Socioeconomic Vulnerability" = Socioeconomic_sheet
)




writeSourceCSV <- function(i) {
  # headerOffset <- 2 # current output has two header rows # VARIABLE
  
  # Define sheet and dimension name
  sheet <- sheetList[[i]]
  dimension <- names(sheetList)[i]
  
  sheet <- sheet[,c(which(colnames(sheet) == "Country"),which(colnames(sheet) %in% indicators$indicator_slug))]
  
  # columns <- colnames(sheet) %>%
  #   as.data.frame() %>%
  #   left_join(indicators, by = c(. = "indicator_slug"))
  # # Other option: use add_row() to add names as first row; would change classes
  # colnames(sheet) <- coalesce(columns$Indicator, columns$.) %>% 
  #   { gsub('^..?_', '', colnames(sheet)) } %>%
  #   { gsub('_', ' ', .) } #%>%
  #   # { gsub("\\.", " – ", .)} %>%
  #   # str_to_title()
  # # print(colnames(sheet))
  
  # Sort dataframes by country code to match, and check if length matches country list
  # if(nrow(sheet) != nrow(countrylist)) warning(paste(dimension, " sheet doesn't match number of countrylist rows"))
  sheet <- sheet[!duplicated(sheet$Country),]
  sheet <- arrange(sheet, Country)
  
  write_csv(sheet, paste0("crm-excel/", dimension, ".csv"))
}

Map(writeSourceCSV, 1:length(sheetList))
