lapStart <- Sys.time()
#
##
### ********************************************************************************************
####    SAVE RISK SHEETS TO EXTERNAL VOLUME ----
### ********************************************************************************************
##
#

# file.copy("Risk_sheets", "external", recursive = T)

# Put dated copy in /risk-sheets/archive
dir.create("archives")
# dir.create("external/risk-sheets")
# dir.create("external/risk-sheets/archive")
file.copy("Risk_sheets", "archives", recursive = T)
# file.copy("Risk_sheets", "external/risk-sheets", recursive = T)
setwd("archives/Risk_sheets")
file.rename(list.files(), paste0( Sys.Date(), "-", list.files()))
setwd("../..")
file.copy(paste0("archives/Risk_sheets/", list.files("archives/Risk_sheets")), paste0("external/risk-sheets/archive/", list.files("archives/Risk_sheets")))
file.copy(paste0("Risk_sheets/", list.files("Risk_sheets")), paste0("external/risk-sheets/", list.files("Risk_sheets")))

#
##
### ********************************************************************************************
####    WRITE MINIMAL DIMENSION SHEETS ----
### ********************************************************************************************
##
#

# Saves .csvs with only used indicators (normalized, not raw) to crm_excel folder
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/update_socio_eco/Indicator_dataset/countrylist.csv")
countrylist <- countrylist %>%
dplyr::select(-X) %>% 
arrange(Country)

# Load dimension / data dictionary
indicators <- as.data.frame(read.csv("indicators.csv"))

# Compile list of all dimension data for Map function
# List names should match dimension names in `indicator` data frame
unique(indicators$Dimension)
sheetList <- list("Health" = health_sheet,
            "Food Security" = food_sheet,
            "Conflict and Fragility" = fragility_sheet, # Technical note uses various names; what do we want to use? Fragility and Conflict, Conflict and Fragility, Conflict Fragility and Institutional Risk
            "Macro Fiscal" = macro_sheet,
            "Natural Hazard" = nathazard_sheet,
            "Socioeconomic Vulnerability" = socioeconomic_sheet
)

# Make directory from within script to enable volume sharing with docker
# (All contents are emptied when volume is shared)
# dir.create("external/crm-excel")
# dir.create("external/crm-excel/archive")

# —Write function to apply to each sheet ----
used_indicators <- countrylist

slugify <- function(x, alphanum_replace="", space_replace="-", tolower=TRUE) {
x <- gsub("[^[:alnum:] ]", alphanum_replace, x)
x <- gsub(" ", space_replace, x)
if(tolower) { x <- tolower(x) }

return(x)
}

writeSourceCSV <- function(i) {
# headerOffset <- 2 # current output has two header rows # VARIABLE

# Define sheet and dimension name
sheet <- sheetList[[i]]
sheet <- sheet[!duplicated(sheet$Country),]
sheet <- arrange(sheet, Country)
dimension <- names(sheetList)[i]

sheet_norm <- sheet[,c(which(colnames(sheet) == "Country"), which(colnames(sheet) %in% indicators$indicator_slug))]
sheet_raw <- sheet[,c(which(colnames(sheet) == "Country"), which(colnames(sheet) %in% unlist(strsplit(indicators$indicator_raw_slug, ", "))))]
colnames(sheet_raw) <- paste0(colnames(sheet_raw), "_raw")
sheet <- left_join(sheet_norm, sheet_raw, by = c("Country" = "Country_raw"), suffix = c("", "_raw"))

# write_csv(sheet_norm, paste0("Risk_sheets/indicators-normalised/", dimension, ".csv"))
write_csv(sheet, paste0("external/crm-excel/", slugify(dimension), ".csv"))
write_csv(sheet, paste0("external/crm-excel/archive/", Sys.Date(), "-", slugify(dimension), ".csv"))

# Write csvs with readable variable names
## Order sheet columns to match indicators.csv
inds <- indicators[which(indicators$Dimension == dimension), "indicator_slug"]
sheet_norm <- sheet_norm[c("Country", inds)]

## Rename columns
pairs <- subset(indicators, Timeframe == "Emerging" & Dimension == dimension, c(Indicator, indicator_slug))
names(sheet_norm)[match(pairs[ ,"indicator_slug"], names(sheet_norm))] <- paste0("Emerging_", dimension, "_", pairs$Indicator)

pairs <- subset(indicators, Timeframe == "Underlying" & Dimension == dimension, c(Indicator, indicator_slug))
names(sheet_norm)[match(pairs[ ,"indicator_slug"], names(sheet_norm))] <- paste0("Underlying_", dimension, "_", pairs$Indicator)  

# write_csv(sheet_norm, paste0("Risk_sheets/pretty-names/", dimension, ".csv") )
# Join sheets together (<<- hoists variable to parent environment)
used_indicators <<- left_join(used_indicators, sheet_norm, by = "Country")

# Check if any sheets have empty columns
empties <- empty_cols(sheet) %>% names()
if(length(empties) > 0) {print(paste(dimension, " is empty on ", empties)) } else {print(paste(dimension, " is filled")) }
}

# —Run -----
Map(writeSourceCSV, 1:length(sheetList))
write_csv(used_indicators, "crm-indicators.csv")