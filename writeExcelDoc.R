# Starting with health, and then generalizing

# 0. SET VARIABLES
# Search for VARIABLE and move them here

# 1. LOAD EVERYTHING IN ----

# Load full countries list (alternative is to use countries in globalrisk)
countrylist <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Indicator_dataset/countrylist.csv")
countrylist <- countrylist %>%
  dplyr::select(-X) %>% 
  arrange(Country)

# Load dimension data from github
healthsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/healthsheet.csv")
foodsecurity <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/foodsecuritysheet.csv")
debtsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/debtsheet.csv")
fragilitysheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/fragilitysheet.csv")
macrosheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/macrosheet.csv")
Naturalhazardsheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/Naturalhazards.csv")
Socioeconomic_sheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/Socioeconomic_sheet.csv")
acapssheet <- read.csv("https://raw.githubusercontent.com/ljonestz/compoundriskdata/master/Risk_sheets/acapssheet.csv")

# Socioecomic sheet is missing country name, add in (prob move to script that writes sheet initially)
Socioeconomic_sheet <-  left_join(countrylist, Socioeconomic_sheet, by = "Country") %>%
  add_column(Rank = seq(), .before = "Countryname")

# Create dimension / data dictionary (move elsewhere? do in csv editor?)
# indicators <- matrix(data = c("Health", "underlying", "H_HIS_Score_norm", "HIS Score",
#                               "Health", "underlying", "H_INFORM_rating.Value_norm", "INFORM Rating",
#                               "Health", "upcoming", "H_Oxrollback_score_norm", "Oxford Lockdown Rollback Index", 
#                               "Health", "upcoming", "H_Covidgrowth_casesnorm", "Covid Growth, Cases (biweekly)",
#                               "Health", "upcoming", "H_Covidgrowth_deathsnorm", "Covid Growth, Deaths (biweekly)",
#                               "Health", "upcoming", "H_new_cases_smoothed_per_million_norm", "New Covid Cases, per 1M",
#                               "Health", "upcoming", "H_new_deaths_smoothed_per_million_norm", "New Covid Deaths, per 1M",
#                               "Health", "upcoming", "H_GovernmentResponseIndexForDisplay_norm", "Oxford Gov Response Index",
#                               "Health", "upcoming", "H_health_acaps", "ACAPS",
#                               "Health", "upcoming", "H_wmo_don_alert", "WMO Diseas Outbreak News",
#                               "Fragility", "", "Fr_FCS_N", "",
#                               "Fragility", "", "", "",
#                               "Fragility", "", "", "",
#                               "Fragility", "", "", "",
#                               "Fragility", "", "", "",
#                               "Fragility", "", "", ""
#                               ),
#                      ncol = 4,
#                      byrow = T) %>% 
#   as.data.frame()
# colnames(indicators) <- c("dimension", "timeframe", "indicator", "prettyName")
# write_csv(indicators, "indicators.csv")
indicators <- as.data.frame(read_csv("indicators.csv"))

# List names should match dimension names in `indicator` dataframe
# unique(indicators$dimension)
sheetList <- list("Health" = healthsheet,
                  "Food Security" = foodsecurity,
                  # "Debt" = debtsheet,
                  "Fragility and Conflict" = fragilitysheet, # Technical note uses various names; what do we want to use? Fragility and Conflict, Conflict and Fragility, Conflict Fragility and Institutional Risk
                  "Macro Fiscal" = macrosheet,
                  "Natural Hazard" = Naturalhazardsheet,
                  "Socioeconomic Vulnerability" = Socioeconomic_sheet
                  )

# 2. SET UP "EMPTY" WORKBOOK ----
# Create empty excel workbook, add Risk sheet now for proper order
# (Later I might load a template workbook instead, with macros & buttons built in)
wb <- createWorkbook()
addWorksheet(wb, "CompoundAlert") # todo: rename tabs, add color: `tabColour = "color"`

# 3. PREPARE DIMENSION DATAFRAMES ----
# # Sort dataframes by country code to match, and check if length matches country list
# if(nrow(healthsheet) != nrow(countrylist)) warning("Health sheet length doesn't match countrylist length")
# healthsheet <- healthsheet[!duplicated(healthsheet$Country),]
# healthsheet <- arrange(healthsheet, Country)
# 
# # Create dimension alert score column, with Excel syntax, and empty columns
# # Organize columns (decide how later)
# healthsheet <- add_column(healthsheet, " " = NA, .after = "Country") %>%
#   add_column("Overall Alert" = NA, .after = " ") %>%
#   add_column("Underlying Vulnerability" = NA, .after = "Overall Alert") %>%
#   add_column("Upcoming Threat" = NA, .after = "Underlying Vulnerability") %>%
#   add_column("  " = NA, .after = "Upcoming Threat") #%>%
#   # separate normalized and raw, maybe also upcoming and underlying, variables
#   # better to create new dataframe instead to do so?
#   # add_column(healthsheet, "  " = NA, .after = "Country")

# Functions to write Excel formulae
# Calculate underlying and upcoming scores
dimensionFormulae <- function(sheet, dim, time.frame, headerOffset = 1) {
  dim_indicators <- subset(indicators, dimension == dim &
                             timeframe == time.frame)[,"indicator"]
  # print(dim_indicators)
  # Find column index of relevant variables, in alphabetic form (as Excel)
  # !!! Important for sheet to have same columns as output !!!
  colIndex <- which(colnames(sheet) %in% dim_indicators) %>%
    int2col()
  print(colIndex)
  # Find row index for each country in the relevant dimension sheet
  # !!! Be wary that header rows will throw off the row index !!! 
  # !!! Make sure to add all header rows before this point, either
  # into df (complex), or into sheet, while keeping track (create counter?)
  formulae <- sapply(countrylist$Country, function(cc) {
    rowIndex <- which(sheet$Country == cc)
   # x <- paste0(dimension, "!", colIndex, rowIndex + headerOffset) %>%
    x <- paste0(colIndex, rowIndex + headerOffset) %>%
      paste(collapse = ", ")
    # print(x)
    x <- paste0("MAX(", x, ")")
    # paste0("=MAX(", H_index, rowIndex),
  })
  return(formulae)
}

# Calculate overall dimension score
dimensionFormulaeOverall <- function(sheet, headerOffset = 1) {
  colIndex <- which(colnames(sheet) %in% c("Underlying Vulnerability", "Upcoming Threat")) %>%
    int2col()
  formulae <- sapply(countrylist$Country, function(cc) {
    rowIndex <- which(sheet$Country == cc)
    x <- paste0(colIndex, rowIndex + headerOffset) %>%
      paste(collapse = ", ")
    x <- paste0("IFERROR(GEOMEAN(", x, "), MAX(", x, "))")
  })
  return(formulae)
}

# make sapply-able by making table of sheet and dimension name, so makeSheet only requires one argument
makeSheet <- function(i) {
  headerOffset <- 2 # current output has two header rows # VARIABLE
  
  # Define sheet and dimension name
  sheet <- sheetList[[i]]
  dimension <- names(sheetList)[i]
  
  # Sort dataframes by country code to match, and check if length matches country list
  if(nrow(sheet) != nrow(countrylist)) warning(paste(dimension, " sheet doesn't match number of countrylist rows"))
  sheet <- sheet[!duplicated(sheet$Country),]
  sheet <- arrange(sheet, Country)
  
  # Create dimension alert score column, with Excel syntax, and empty columns
  # Organize columns (decide how later)
  sheet <- add_column(sheet, " " = NA, .after = "Country") %>%
    add_column("Overall Alert" = NA, .after = " ") %>%
    add_column("Underlying Vulnerability" = NA, .after = "Overall Alert") %>%
    add_column("Upcoming Threat" = NA, .after = "Underlying Vulnerability") %>%
    add_column("  " = NA, .after = "Upcoming Threat") #%>%
  # separate normalized and raw, maybe also upcoming and underlying, variables
  # better to create new dataframe instead to do so?
  # add_column(healthsheet, "  " = NA, .after = "Country")
  
  sheet[,"Underlying Vulnerability"] <- dimensionFormulae(sheet, dimension, "underlying", headerOffset)
  sheet[,"Upcoming Threat"] <- dimensionFormulae(sheet, dimension, "upcoming", headerOffset)
  sheet[,"Overall Alert"] <- dimensionFormulaeOverall(sheet, headerOffset)
  class(sheet[,"Underlying Vulnerability"]) <- c(class(sheet[,"Underlying Vulnerability"]), "formula")
  class(sheet[,"Upcoming Threat"]) <- c(class(sheet[,"Upcoming Threat"]), "formula")
  class(sheet[,"Overall Alert"]) <- c(class(sheet[,"Overall Alert"]), "formula")
  
  columns <- colnames(sheet) %>%
    as.data.frame() %>%
    left_join(indicators, by = c(. = "indicator"))
  # Other option: use add_row() to add names as first row; would change classes
  colnames(sheet) <- coalesce(columns$prettyName, columns$.) %>% 
    { gsub('^H_', '', colnames(sheet)) } %>%
    { gsub('_', ' ', .) } %>%
    # { gsub("\\.", " – ", .)} %>%
    str_to_title()
  
  # 4. WRITE DIMENSION DATAFRAMES TO WORKBOOK
  addWorksheet(wb, dimension)
  writeData(wb, dimension, sheet, colNames = TRUE, startRow = headerOffset)
}

Map(makeSheet, 1:length(sheetList))

# NOW A FOR LOOP OR SAPPLY TO DO THIS FOR BOTH TIME FRAMES AND EVERY DIMENSION
# headerOffset <- 2 # current output has two header rows 
# healthsheet[,"Underlying Vulnerability"] <- dimensionFormulae(healthsheet, "Health", "underlying", headerOffset)
# healthsheet[,"Upcoming Threat"] <- dimensionFormulae(healthsheet, "Health", "upcoming", headerOffset)
# healthsheet[,"Overall Alert"] <- dimensionFormulaeOverall(healthsheet, headerOffset)
# class(healthsheet[,"Underlying Vulnerability"]) <- c(class(healthsheet[,"Underlying Vulnerability"]), "formula")
# class(healthsheet[,"Upcoming Threat"]) <- c(class(healthsheet[,"Upcoming Threat"]), "formula")
# class(healthsheet[,"Overall Alert"]) <- c(class(healthsheet[,"Overall Alert"]), "formula")
# 
# # Rename columns pretty names (Maybe include both as separate header rows?)
# # uglyNames <- subset(indicators, dimension == "Health")[,"indicator"]
# # colnames(healthsheet)[colnames(healthsheet) %in% uglyNames] == uglyNames
# columns <- colnames(healthsheet) %>%
#   as.data.frame() %>%
#   left_join(indicators, by = c(. = "indicator"))
# # Other option: use add_row() to add names as first row; would change classes
# colnames(healthsheet) <- coalesce(columns$prettyName, columns$.) %>% 
#   { gsub('^H_', '', colnames(healthsheet)) } %>%
#   { gsub('_', ' ', .) } %>%
#   { gsub("\\.", " – ", .)} %>%
#   str_to_title()
# 
# # 4. WRITE DIMENSION DATAFRAMES TO WORKBOOK
# addWorksheet(wb, "Health")
# writeData(wb, "Health", healthsheet, colNames = TRUE, startRow = headerOffset)

# Format Sheets ----
# —Create formats
Map(function(number, tab) {
  # Sheet Wide / Default
  bodyStyle <- createStyle(
    fgFill = "white",
    border = "TopBottomLeftRight", # might drop Top to not over ride header black border
    borderColour = "white",
    halign = "right"
  )
  addStyle(wb,
           sheet = number,
           bodyStyle,
           rows = 1:200, #make more specific
           cols = 1:60, #make more specific
           gridExpand = TRUE
  )
  
  # Header formatting
  headerStyle <- createStyle(
    fontSize = 10,
    # fontColour = "#FFFFFF",
    # textDecoration = "bold",
    # halign = "right",
    # valign = "center",
    # fgFill = "#000000",
    border = c("bottom", "left", "right"),
    borderColour = c("black", "white", "white"),
    borderStyle = c("thin", "thick", "thick"),
    wrapText = TRUE
    # textRotation = 90
  )
  addStyle(wb,
           sheet = number,
           headerStyle,
           rows = 2,
           cols = 1:57, #fix to only relevant columns
           gridExpand = TRUE,
           stack = TRUE
  )
  # Left justify country name columns
  leftStyle <- createStyle(
    halign = "left"
  )
  addStyle(wb,
           sheet = number,
           leftStyle,
           rows = 1:200,
           cols = 1:3, #fix to only relevant columns
           gridExpand = TRUE,
           stack = TRUE
  )
  
  setColWidths(wb, number, cols = 2, widths = 22) ## set column width for country names column
  setColWidths(wb, number, cols = c(1,4,8), widths = 3) ## set column width for rank and empty cols

  #setRowHeights(wb, number, rows = 1, heights = 10) ## set column width for row names column
  
  modifyBaseFont(wb,
                 fontSize = 12,
                 fontColour = "black",
                 fontName = "Arial"
  )
    # addFilter(wb, number, row = 2, cols = 2:200)
  
  # Conditional formatting
  naStyle <- createStyle(fontColour = "white", bgFill = "white")
  
  ## Overall: Underlying and Emerging/Upcoming
  lowStyleOverall <- createStyle(fontColour = "#000000", bgFill = "#F2CAC4")
  medStyleOverall <- createStyle(fontColour = "#000000", bgFill = "#EE9F8F")
  higStyleOverall <- createStyle(fontColour = "#000000", bgFill = "#E35A3A")  

  # TODO: 
  conditionalFormatting(wb, number, cols = 5, rows = 1:(190+headerOffset), rule = "==10", style = higStyleOverall)
  conditionalFormatting(wb, number, cols = 5, rows = 1:(190+headerOffset), type = "between", rule = c(7.00, 9.99), style = medStyleOverall)
  conditionalFormatting(wb, number, cols = 5, rows = 1:(190+headerOffset), type = "between", rule = c(0, 6.999), style = lowStyleOverall)
  conditionalFormatting(wb, number, cols = 5, rows = 1:(190+headerOffset), rule = '=""', style = naStyle)
  
  ## Underlying
  lowStyleUnderlying <- createStyle(fontColour = "#000000", bgFill = "#FCDCDF")
  medStyleUnderlying <- createStyle(fontColour = "#000000", bgFill = "#F7A9AC")
  higStyleUnderlying <- createStyle(fontColour = "#000000", bgFill = "#F2676B")

  conditionalFormatting(wb, number, cols = 6, rows = 1:(190+headerOffset), rule = "==10", style = higStyleUnderlying)
  conditionalFormatting(wb, number, cols = 6, rows = 1:(190+headerOffset), type = "between", rule = c(7.00, 9.99), style = medStyleUnderlying)
  conditionalFormatting(wb, number, cols = 6, rows = 1:(190+headerOffset), type = "between", rule = c(0, 6.999), style = lowStyleUnderlying)
  conditionalFormatting(wb, number, cols = 6, rows = 1:(190+headerOffset), rule = '=""', style = naStyle)
  
  ##Emerging/Upcoming
  lowStyleUpcoming <- createStyle(fontColour = "#000000", bgFill = "#FCD5B4")
  medStyleUpcoming <- createStyle(fontColour = "#000000", bgFill = "#F9BF8E")
  higStyleUpcoming <- createStyle(fontColour = "#000000", bgFill = "#E06C25")
  
  conditionalFormatting(wb, number, cols = 7, rows = 1:(190+headerOffset), rule = "==10", style = higStyleUpcoming)
  conditionalFormatting(wb, number, cols = 7, rows = 1:(190+headerOffset), type = "between", rule = c(7.00, 9.99), style = medStyleUpcoming)
  conditionalFormatting(wb, number, cols = 7, rows = 1:(190+headerOffset), type = "between", rule = c(0, 6.999), style = lowStyleUpcoming)
  conditionalFormatting(wb, number, cols = 7, rows = 1:(190+headerOffset), rule = '=""', style = naStyle)
},2:(length(sheetList)+1))


# —Write Risk sheet ----
# ——Dimensional scores formulae
# For debt, which columns am I using?
# Later: add in flexibility to change included indicators from excel



# paste0("MAX("

# Add blank columns to Risk sheet
# (method doesn't when I'm writing Risk all in excel syntax)
# riskflagsblank <- riskset %>%
#   arrange(Country) %>%
#   add_column(" " = NA, .after = "...") %>%
#   add_column("  " = NA, .after = "...") %>%
#   add_column("   " = NA, .after = "...")


# Rename columns (needs to be after making the risk sheet)
# head(healthsheet)
# healthsheet2 <- healthsheet
# colnames(healthsheet2) %in%
#   select(subset(indicators, dimension == "Health"), indicator)
# colnames(healthsheet2)
# select(subset(indicators, dimension == "Health"), indicator)
# 
# which(colnames(healthsheet2) %in% indicators)

# —Format sheets

# Save workbook
saveWorkbook(wb, file = "Risk_sheets/db-draft.xlsx", overwrite = TRUE)




# compound <- mutate(countrylist, "Health, Underlying" = health_underlying[countrylist$Country])
# class(compound$`Health, Underlying`) <- c(class(compound$`Health, Underlying`), "formula")
# writeData(wb, "CompoundAlert", compound, colNames = TRUE)
