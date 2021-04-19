# 1. Load everything in ----

# # install.packages("librarian")     #Run if librarian is not already installed
# librarian::shelf(
#   tidyverse
# )

# Load full countries list (alternative is to use countries in globalrisk)
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Indicator_dataset/countrylist.csv")
countrylist <- countrylist %>%
  dplyr::select(-X) %>% 
  arrange(Country)

# Load dimension data from github
# healthsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/healthsheet.csv")
# foodsecurity <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/foodsecuritysheet.csv")
# debtsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/debtsheet.csv")
# fragilitysheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/fragilitysheet.csv")
# macrosheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/macrosheet.csv")
# Naturalhazardsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/Naturalhazards.csv")
# Socioeconomic_sheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/Socioeconomic_sheet.csv")
# acapssheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Risk_sheets/acapssheet.csv")

healthsheet <- read.csv("Risk_sheets/healthsheet.csv")
foodsecurity <- read.csv("Risk_sheets/foodsecuritysheet.csv")
# debtsheet <- read.csv("Risk_sheets/debtsheet.csv")
fragilitysheet <- read.csv("Risk_sheets/fragilitysheet.csv")
macrosheet <- read.csv("Risk_sheets/macrosheet.csv")
Naturalhazardsheet <- read.csv("Risk_sheets/Naturalhazards.csv")
Socioeconomic_sheet <- read.csv("Risk_sheets/Socioeconomic_sheet.csv")

# Load dimension / data dictionary
indicators <- as.data.frame(read.csv("indicators.csv"))

# Compile list of all dimension data for Map function
# List names should match dimension names in `indicator` data frame
unique(indicators$Dimension)
sheetList <- list("Health" = healthsheet,
                  "Food Security" = foodsecurity,
                  "Conflict and Fragility" = fragilitysheet, # Technical note uses various names; what do we want to use? Fragility and Conflict, Conflict and Fragility, Conflict Fragility and Institutional Risk
                  "Macro Fiscal" = macrosheet,
                  "Natural Hazard" = Naturalhazardsheet,
                  "Socioeconomic Vulnerability" = Socioeconomic_sheet
)

# 2. Write function to apply to each sheet ----
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
  empties <- empty_cols(sheet) %>% names()
  if(length(empties) > 0) {paste(dimension, " is empty on ", empties) } else {paste(dimension, " is complete") }
}

Map(writeSourceCSV, 1:length(sheetList))
