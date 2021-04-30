######################################################################################################
#           DATA AND ANALYSIS FOR KLIP KENYA SURVEY
#           AUTHOR: LINDSEY JONES, WORLD BANK
#           DATE: SEPT 2020
######################################################################################################

# Trouble with klip2$SECTION2HOUSEHOLDCHARACTERISTICS.Q201g
# Confusing SECTION2HOUSEHOLDCHARACTERISTICS.Q201g; SECTION2HOUSEHOLDCHARACTERISTICS.Q201g; SECTION2HOUSEHOLDCHARACTERISTICS.Q202a9; SECTION2HOUSEHOLDCHARACTERISTICS.Q202a9

#
##
### --------------------LOAD PACKAGES--------------------------
##
#

#install.packages("librarian")    # Install if librarian is not already loaded
librarian::shelf(
  sjPlot, estimatr, ggthemes, reshape, ggthemr, gridExtra,
  readxl, tidyr, forcats, huxtable, ggplot2, ggthemr, dplyr,
  lubridate, foreign, kableExtra, knitr, stargazer, haven, tab,
  styler
)

#
##
### --------------------SET UP KLIP DATABASE (BENEFICIARIES)--------------------
##
#

# Create beneficiary dataframe
klip2 <- read_dta("~/Dropbox/KLIP Consumer Value Lindsey/dta-files/Beneficiaries.dta")
klip2 <- as.data.frame(klip2)

# Make colnames as attributes
attr(klip2$endtime, "label") <- "endtime"
attr(klip2$today, "label") <- "today"

colnames(klip2) <- unlist(lapply(colnames(klip2), function(xx) {
  attr(klip2[, xx], "label")
}))

klip2$treat <- "Beneficiary"

# Load question responses
klipchoice <- read_excel("~/Dropbox/KLIP Consumer Value Lindsey/Titus data/Analysis frame KLIP Percep study/KLIP-Beneficiaries-Codebook.xlsx", sheet = "choices")

# format names NEED TO ADDRESS ISSUE WITH 201g and Q501c
klipnames <- gsub("choices_", "", klipchoice$`list name`)
klipnames2 <- gsub("_", ":", klipnames)
klipchoice$`list name` <- klipnames2

kliplevels <- klipchoice %>%
  spread(`list name`, `label::English`) %>%
  mutate_if(is.character, as.factor)
rownames(kliplevels) <- kliplevels$name

kliplevels <- kliplevels %>%
  dplyr::select(-Consent, -County, -`SECTION2HOUSEHOLDCHARACTERISTICS:Q201g`, -`Section5:Q501c`, -`Section5:Q506`)

colnames(kliplevels) <- make.names(colnames(kliplevels))
colnames(klip2) <- make.names(colnames(klip2))

# Fix blank space
klip2$`Section7FinancialServices.Q705` <- ifelse(klip2$`Section7FinancialServices.Q705` == "", "99", klip2$`Section7FinancialServices.Q705`)

# Convert character to factor
klip2[colnames(kliplevels[, 2:111])] <- lapply(klip2[colnames(kliplevels[, 2:111])], function(dd) {
  as.factor(dd)
})

# Merge factor levels between datasets (except for name)
for (i in colnames(kliplevels[, 2:111])) {
  lev <- kliplevels %>%
    dplyr::select(name, i) %>%
    drop_na()
  levels(klip2[[i]]) <- lev[[2]][lev[[1]] %in% levels(klip2[[i]])]
}

# characters to factors
klip2 <- klip2 %>%
  mutate_if(is.character, as.factor)

#
##
### --------------------SET UP KLIP DATABASE (NON-BENEFICIARIES)--------------------
##
#

# Create non-beneficiary dataframe
klipnon2 <- read_dta("~/Dropbox/KLIP Consumer Value Lindsey/dta-files/NonBeneficiaries.dta")
klipnon2 <- as.data.frame(klipnon2)

# Make colnames as attributes
attr(klipnon2[, 2], "label") <- colnames(klipnon2[2])
attr(klipnon2[, 3], "label") <- colnames(klipnon2[3])
attr(klipnon2[, 4], "label") <- colnames(klipnon2[4])
attr(klipnon2[, 5], "label") <- colnames(klipnon2[5])
attr(klipnon2[, 6], "label") <- colnames(klipnon2[6])
attr(klipnon2[, 7], "label") <- colnames(klipnon2[7])
attr(klipnon2[, 8], "label") <- colnames(klipnon2[8])
attr(klipnon2[, 9], "label") <- colnames(klipnon2[9])
attr(klipnon2[, 10], "label") <- colnames(klipnon2[10])
attr(klipnon2[, 11], "label") <- colnames(klipnon2[11])
attr(klipnon2[, 12], "label") <- colnames(klipnon2[12])
attr(klipnon2[, 238], "label") <- colnames(klipnon2[238])
attr(klipnon2[, 239], "label") <- colnames(klipnon2[239])
colnames(klipnon2) <- unlist(lapply(colnames(klipnon2), function(xx) {
  attr(klipnon2[, xx], "label")
}))

# Load question responses
klipnonchoice <- read_excel("~/Dropbox/KLIP Consumer Value Lindsey/Titus data/Analysis frame KLIP Percep study/KLIP-Non-Beneficiaries-Codebook.xlsx", sheet = "choices")

# format names NEED TO ADDRESS ISSUE WITH 201g and Q501c
klipnonnames <- gsub("choices_", "", klipnonchoice$`list name`)
klipnonnames2 <- gsub("_", ":", klipnonnames)
klipnonchoice$`list name` <- klipnonnames2
klipnonlevels <- klipnonchoice %>%
  spread(`list name`, `label::English`) %>%
  mutate_if(is.character, as.factor)
rownames(klipnonlevels) <- klipnonlevels$name
klipnonlevels <- klipnonlevels %>%
  dplyr::select(-Consent, -county, -`Section5:Q501c`, -`Section5:Q506`, -`SECTION2InsuranceAwarenessHouseholdAssets:Q201e`)
colnames(klipnonlevels) <- make.names(colnames(klipnonlevels))
colnames(klipnon2) <- make.names(colnames(klipnon2))

# Fix blank space (CHECK WHAT'S DRIVING THIS)
klipnon2$`Section7FinancialServices.Q703` <- ifelse(klipnon2$`Section7FinancialServices.Q703` == "", "99", klipnon2$`Section7FinancialServices.Q703`)

# Convert character to factor
klipnon2[colnames(klipnonlevels[, 2:94])] <- lapply(klipnon2[colnames(klipnonlevels[, 2:94])], function(dd) {
  as.factor(dd)
})

# Merge factor levels between datasets (except for name)
for (i in colnames(klipnonlevels[, 2:94])) {
  lev <- klipnonlevels %>%
    dplyr::select(name, i) %>%
    drop_na()
  levels(klipnon2[[i]]) <- lev[[2]][lev[[1]] %in% levels(klipnon2[[i]])]
}

# Assing beneficiary tag
klipnon2$treat <- "Non-beneficiary"

# characters to factors and rename
klipnon2 <- klipnon2 %>%
  mutate_if(is.character, as.factor)

# Replace name
names(klipnon2)[names(klipnon2) == "county"] <- "County"

#
##
### --------------------MERGE DATASETS----------------------------------------
##
#

samecol <- c(
  "SECTION2InsuranceAwarenessHouseholdAssets.Q204cow", "SECTION2InsuranceAwarenessHouseholdAssets.Q204camel",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q204goat", "SECTION2InsuranceAwarenessHouseholdAssets.Q204sheep",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q201a", "SECTION2InsuranceAwarenessHouseholdAssets.Q201b", "SECTION2InsuranceAwarenessHouseholdAssets.Q201c",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q201d", "SECTION2InsuranceAwarenessHouseholdAssets.cattleownedsep18",
  "SECTION2InsuranceAwarenessHouseholdAssets.camelownedsep18", "SECTION2InsuranceAwarenessHouseholdAssets.goatsownedsep18",
  "SECTION2InsuranceAwarenessHouseholdAssets.sheepownedsep18", "SECTION2InsuranceAwarenessHouseholdAssets.donkeyownedsep18",
  "SECTION2InsuranceAwarenessHouseholdAssets.otherlivestockowned", "SECTION2InsuranceAwarenessHouseholdAssets.otherlivestockcount",
  "SECTION2InsuranceAwarenessHouseholdAssets.cattleherdsepoct19", "SECTION2InsuranceAwarenessHouseholdAssets.camelherdsepcot19",
  "SECTION2InsuranceAwarenessHouseholdAssets.goatsherdsepoct19", "SECTION2InsuranceAwarenessHouseholdAssets.sheepherdsepoct19",
  "SECTION2InsuranceAwarenessHouseholdAssets.donkeyherdsepoct19", "SECTION2InsuranceAwarenessHouseholdAssets.otherlivestockherd",
  "SECTION2InsuranceAwarenessHouseholdAssets.otherlivestockherdcount", "SECTION2InsuranceAwarenessHouseholdAssets.Q205a", "SECTION2InsuranceAwarenessHouseholdAssets.Q205b",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q205c", "SECTION2InsuranceAwarenessHouseholdAssets.Q205d",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q205e", "SECTION2InsuranceAwarenessHouseholdAssets.Q205f",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q205f1", "SECTION2InsuranceAwarenessHouseholdAssets.Q206a",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q208a", "SECTION2InsuranceAwarenessHouseholdAssets.Q208b",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q208c", "SECTION2InsuranceAwarenessHouseholdAssets.Q208d",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q208e", "SECTION2InsuranceAwarenessHouseholdAssets.Q208f",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q208g", "SECTION2InsuranceAwarenessHouseholdAssets.Q208h",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q208i", "SECTION2InsuranceAwarenessHouseholdAssets.Q208i1",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q209", "SECTION2InsuranceAwarenessHouseholdAssets.Q209other",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q211a", "SECTION2InsuranceAwarenessHouseholdAssets.Q212a", "SECTION2InsuranceAwarenessHouseholdAssets.Q212b",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q212c", "SECTION2InsuranceAwarenessHouseholdAssets.Q212d",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q212e", "SECTION2InsuranceAwarenessHouseholdAssets.Q212f",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q212g", "SECTION2InsuranceAwarenessHouseholdAssets.Q212h",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q212i", "SECTION2InsuranceAwarenessHouseholdAssets.Q212j",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q212k", "SECTION2InsuranceAwarenessHouseholdAssets.Q213b",
  "SECTION3Localmarketperception.Q306a", "SECTION3Localmarketperception.Q306b",
  "SECTION3Localmarketperception.Q306c", "SECTION3Localmarketperception.Q306d",
  "SECTION3Localmarketperception.Q306e", "SECTION3Localmarketperception.Q306f",
  "SECTION3Localmarketperception.Q306g", "SECTION3Localmarketperception.Q306h",
  "SECTION4Schoolattendanceperception.Q401", "SECTION4Schoolattendanceperception.Q402",
  "SECTION4Schoolattendanceperception.Q403", "SECTION4Schoolattendanceperception.Comment4", "Section5.Q501",
  "Section5.life", "Section5.annuities", "Section5.property", "Section5.vehicle",
  "Section5.health", "Section5.agric", "Section5.land", "Section5.chamma",
  "Section5.otherInsTypes", "Section5.knowOtherInsTypes", "Section5.Q501b",
  "Section5.Q501c", "Section5.Q502.Q502a", "Section5.Q502.Q502b",
  "Section5.Q502.Q502c", "Section5.Q502.Q502d", "Section5.Q502.Q502e",
  "Section5.Q502.Q502f", "Section5.Q502.Q502g", "Section7FinancialServices.Q701", "SECTION8HUMANITAIRANASSISTANCE.Q801"
)

# same questions
# klipnon2[84] klip[88]
# klipnon2[88] klip[111]
# klipnon2[228:230] klip[278:280]
# klipnon2[232:233] klip[282:283]

# Change colnames in non-beniciary database to align with beneficiaries
colnames(klipnon2) <- gsub("InsuranceAwarenessHouseholdAssets", "HOUSEHOLDCHARACTERISTICS", colnames(klipnon2))
samecol <- gsub("InsuranceAwarenessHouseholdAssets", "HOUSEHOLDCHARACTERISTICS", samecol)
samecol <- gsub("choices_", "", samecol)
samecol <- gsub("_", ":", samecol)

# Perform merge
klip <- klip2 %>%
  full_join(klipnon2, by = intersect(c(colnames(klip2[1:84]), colnames(klip2[231:264]), "treat", samecol), c(colnames(klipnon2[1:84]), colnames(klipnon2[186:218]), "treat", samecol))) %>%
  group_by(`meta.instanceID`)
klip <- as.data.frame(klip)

# Change variable types
cat <- c(
  "SECTION1.HouseHoldMembers.HHGender", "SECTION1.HouseHoldMembers.Q104", "SECTION1.HouseHoldMembers.MaritalStatus",
  "Section5.Q501b"
)
klip[cat] <- lapply(klip[cat], function(xx) {
  as.factor(xx)
})
klipnon2[cat] <- lapply(klipnon2[cat], function(xx) {
  as.factor(xx)
})

# characters to factors
klip <- klip %>%
  mutate_if(is.character, as.factor)

# Sort factors
klip <- klip %>%
  mutate(
    SECTION1.HouseHoldMembers.Q104 = as.numeric(SECTION1.HouseHoldMembers.Q104),
    SECTION1.HouseHoldMembers.Q104 = case_when(
      SECTION1.HouseHoldMembers.Q104 == 7 ~ "No Response ",
      SECTION1.HouseHoldMembers.Q104 == 6 ~ "Secondary",
      SECTION1.HouseHoldMembers.Q104 == 5 ~ "No formal Edu",
      SECTION1.HouseHoldMembers.Q104 == 4 ~ "Post Grad",
      SECTION1.HouseHoldMembers.Q104 == 3 ~ "University",
      SECTION1.HouseHoldMembers.Q104 == 2 ~ "Secondary",
      SECTION1.HouseHoldMembers.Q104 == 1 ~ "Primary",
      TRUE ~ NA_character_
    ),
    SECTION1.HouseHoldMembers.Q104 = as.factor(SECTION1.HouseHoldMembers.Q104),
    SECTION1.HouseHoldMembers.Q104 = factor(SECTION1.HouseHoldMembers.Q104,
                                            levels = c(
                                              "No formal Edu", "Primary", "Secondary", 
                                              "University", "Post Grad", "No Response"
  )))
  
klip$SECTION1.HouseHoldMembers.Q104 <- fct_relevel(klip$SECTION1.HouseHoldMembers.Q104, "No formal Edu", "Primary", "Secondary", "University", "Post Grad", "No Response ")
klip$County <- as.factor(ifelse(klip$County == "", NA, paste(klip$County)))

# Calculate total pay
klip <- klip %>%
  mutate(
    totalpay = rowSums(klip[c("SECTION2HOUSEHOLDCHARACTERISTICS.Q202a2", "SECTION2HOUSEHOLDCHARACTERISTICS.Q202a4", "SECTION2HOUSEHOLDCHARACTERISTICS.Q202a6", "SECTION2HOUSEHOLDCHARACTERISTICS.Q202a8", "SECTION2HOUSEHOLDCHARACTERISTICS.Q202a10")], na.rm = T),
    totalpay = ifelse(treat == "Non-beneficiary", NA, totalpay)
  )

# cattle since (Note that those with 0 aren't factored in)
klip <- klip %>%
  mutate(
    cattlegain = SECTION2HOUSEHOLDCHARACTERISTICS.cattleherdsepoct19 - SECTION2HOUSEHOLDCHARACTERISTICS.cattleownedsep18,
    cattlegainpcraw = ((cattlegain / SECTION2HOUSEHOLDCHARACTERISTICS.cattleownedsep18) * 100),
    cattlegainpc = dplyr::case_when(
      cattlegainpcraw == NaN ~ 0,
      cattlegainpcraw == Inf ~ NA_real_,
      cattlegainpcraw <= -10000 ~ NA_real_,
      cattlegainpcraw >= 4000 ~ NA_real_,
      TRUE ~ cattlegainpcraw
    )
  )

# Perception merges
klip$SECTION2HOUSEHOLDCHARACTERISTICS.Q211bmerge <- ifelse(klip$treat == "Beneficiary", paste(klip$SECTION2HOUSEHOLDCHARACTERISTICS.Q211b.x),
  ifelse(klip$treat == "Non-beneficiary", paste(klip$SECTION2HOUSEHOLDCHARACTERISTICS.Q211b.y),
    NA
  )
)
klip$SECTION2HOUSEHOLDCHARACTERISTICS.Q211bmerge <- as.factor(ifelse(klip$SECTION2HOUSEHOLDCHARACTERISTICS.Q211bmerge == "NA", NA, klip$SECTION2HOUSEHOLDCHARACTERISTICS.Q211bmerge))
levels(klip$SECTION2HOUSEHOLDCHARACTERISTICS.Q211bmerge) <- levels(klip$SECTION2HOUSEHOLDCHARACTERISTICS.Q211b.x)

klip$Section5.Q503amerge <- ifelse(klip$treat == "Beneficiary", paste(klip$Section5.Q503b),
  ifelse(klip$treat == "Non-beneficiary", paste(klip$Section5.Q503a),
    NA
  )
)
klip$Section5.Q503amerge <- as.factor(ifelse(klip$Section5.Q503amerge == "NA", NA, klip$Section5.Q503amerge))
levels(klip$Section5.Q503amerge) <- levels(klip$Section5.Q503b)

klip$Section5.Q503bmerge <- ifelse(klip$treat == "Beneficiary", paste(klip$Section5.Q503c),
  ifelse(klip$treat == "Non-beneficiary", paste(klip$Section5.Q503b),
    NA
  )
)
klip$Section5.Q503bmerge <- as.factor(ifelse(klip$Section5.Q503bmerge == "NA", NA, klip$Section5.Q503bmerge))
levels(klip$Section5.Q503bmerge) <- levels(klip$Section5.Q503c)

#Remove cattle outliers
klip <- klip %>%
  mutate(cattlegain = case_when(
    cattlegain <= -1000 ~ NA_real_,
    TRUE ~ cattlegain
    ))

#
##
### --------------------ANALYSIS-------------------------------------------------
##
#

# GGtheme
ggthemr("fresh")

#-------------------Crosstab (can't include `SECTION1.HouseHoldMembers.Q104` for some reason)--------------------

tab <- tabmulti(`SECTION1.HouseHoldMembers.HHGender` + `SECTION1.HouseHoldMembers.Q103a` + `SECTION1.HouseHoldMembers.Q106` + +`SECTION1.HouseHoldMembers.MaritalStatus` + `SECTION1.HouseHoldMembers.Q106` +
  `SECTION2HOUSEHOLDCHARACTERISTICS.Q201a` + `Section5.Q501b` + SECTION2HOUSEHOLDCHARACTERISTICS.cattleownedsep18 + SECTION2HOUSEHOLDCHARACTERISTICS.Q205a ~ treat, data = klip)
tabh <- as_hux(tab)
quick_html(theme_article(tabh), file = "klipxtab.html")

#-------------------Comparison of socio-economic groups---------------------------------------

age <- ggplot(klip, aes(`SECTION1.HouseHoldMembers.Q103a`)) +
  geom_histogram() +
  facet_wrap(~treat) +
  theme_538() +
  theme(
    legend.position = "bottom",
    strip.text.x = element_text(
      size = 16,
      family = "",
      color = "Black",
      hjust = 0
    ),
    plot.title = element_text(
      size = 20,
      family = "",
      color = "Black",
      face = "bold",
      hjust = 0.5
    ),
    legend.text = element_text(size = 16),
    axis.text = element_text(
      size = 16,
      family = "",
      color = "Black",
      hjust = 0
    ),
    axis.title = element_text(
      size = 18,
      family = "",
      color = "Black",
      face = "bold",
      hjust = 0.5
    )
  ) +
  ggtitle("a) Age profile of respondents") +
  xlab("Age") +
  ylab("Count")

hhm <- ggplot(klip, aes(`SECTION1.HouseHoldMembers.Q106`)) +
  geom_histogram() +
  facet_wrap(~treat) +
  ggtitle("a) Household members")

# Gender difference heard of Klip before
barch <- function(cat, ans) {
  myaexp <- klip %>%
    dplyr::select(cat, ans)

  myaexp <- subset(myaexp, myaexp[[ans]] != c("No response"))
  myaexp[[ans]] <- droplevels(myaexp[[ans]])

  # myaexp <- subset(myaexp, myaexp[[ans]] !=c("Do Not Know"))
  # myaexp[[ans]] <- droplevels(myaexp[[ans]])

  mdfr <- reshape::melt(myaexp, id.vars = cat)

  a <- data.frame(prop.table(table(mdfr[[cat]], mdfr$value), 1) * 100)

  chplot <- ggplot(
    a,
    aes(
      x = Var1,
      y = Freq,
      fill = forcats::fct_rev(Var2)
    )
  ) +
    labs(x = "", y = "Pecentage") +
    geom_bar(stat = "identity") +
    theme_538() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(
        size = 20,
        family = "",
        color = "Black",
        face = "bold",
        hjust = 0.5
      ),
      legend.text = element_text(size = 16),
      legend.title = element_blank(),
      axis.text = element_text(
        size = 16,
        family = "",
        color = "Black",
        hjust = 0
      ),
      axis.title = element_text(
        size = 18,
        family = "",
        color = "Black",
        face = "bold",
        hjust = 0.5
      )
    ) +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE))
  chplot
}

genbar <- barch("SECTION1.HouseHoldMembers.HHGender", "SECTION2HOUSEHOLDCHARACTERISTICS.Q201a") +
  xlab("Have you ever heard of KLIP?") +
  ggtitle("c) Awareness of KLIP")

# Treat difference between
barch <- function(cat, ans) {
  myaexp <- klip %>%
    dplyr::select(cat, ans)

  myaexp <- subset(myaexp, myaexp[[ans]] != c("No Response "))
  myaexp[[ans]] <- droplevels(myaexp[[ans]])

  # myaexp <- subset(myaexp, myaexp[[ans]] !=c("Do Not Know"))
  # myaexp[[ans]] <- droplevels(myaexp[[ans]])

  mdfr <- reshape::melt(myaexp, id.vars = cat)
  a <- data.frame(prop.table(table(mdfr[[cat]], mdfr$value), 1) * 100)

  chplot <- ggplot(a, aes(x = Var1, y = Freq, fill = forcats::fct_rev(Var2))) +
    labs(x = "", y = "Pecentage")  +
    geom_bar(stat = "identity") +
    theme_538() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(
        size = 20,
        family = "",
        color = "Black",
        face = "bold",
        hjust = 0.5
      ),
      legend.text = element_text(size = 16),
      axis.text = element_text(
        size = 16,
        family = "",
        color = "Black",
        hjust = 0
      ),
      axis.title = element_text(
        size = 18,
        family = "",
        color = "Black",
        face = "bold",
        hjust = 0.5
      )
    ) +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE))
  chplot
}

edubar <- barch("treat", "SECTION1.HouseHoldMembers.Q104") +
  xlab("Level of education of HHH") +
  ggtitle("d) Difference in education")

cattle <- ggplot(klip2, aes(log(SECTION2HOUSEHOLDCHARACTERISTICS.Q205a + 1), SECTION2HOUSEHOLDCHARACTERISTICS.Q202a10)) +
  geom_point(size = 5) +
  geom_smooth(method = "lm") +
  xlab("Number of cattle lost  in 2019") +
  ylab("KLIP payout 2019") +
  theme_538() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(
      size = 20,
      family = "",
      color = "Black",
      face = "bold",
      hjust = 0.5
    ),
    legend.text = element_text(size = 16),
    axis.text = element_text(
      size = 16,
      family = "",
      color = "Black",
      hjust = 0
    ),
    axis.title = element_text(
      size = 18,
      family = "",
      color = "Black",
      face = "bold",
      hjust = 0.5
    )
  ) +
  ggtitle("b) Klip payouts 2019")

comb <- grid.arrange(age, cattle, genbar, edubar)

ggsave("KLIP/comb.pdf", comb, width = 19, height = 12)

#---------------------Education levels-------------------------------------------

dchar <- function(data, grouping, varia) {
  myaexp <- data %>%
    dplyr::select(grouping, varia)
  myaexp <- subset(myaexp, myaexp[[varia]] != c("No Response "))
  myaexp[[varia]] <- droplevels(myaexp[[varia]])
  # myaexp <- subset(myaexp, myaexp[[varia]] !=c("Do Not Know"))
  # myaexp[[varia]] <- droplevels(myaexp[[varia]])
  mdfr <- reshape::melt(myaexp, id.vars = grouping)
  a <- data.frame(prop.table(table(mdfr[[grouping]], mdfr$value), 1) * 100)
  a %>%
    ggplot(aes(
      x = Var1,
      y = Freq,
      fill = forcats::fct_rev(Var2)
    )) +
    geom_col() +
    coord_flip() +
    theme_fivethirtyeight() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    guides(fill = guide_legend(reverse = TRUE))
}

treat <- klip %>%
  filter(treat == "Beneficiary") %>%
  mutate(
    SECTION1.HouseHoldMembers.Q104 = case_when(
      SECTION1.HouseHoldMembers.Q104 == "Post Grad" ~ "University",
      TRUE ~ as.character(SECTION1.HouseHoldMembers.Q104)),
      SECTION1.HouseHoldMembers.Q104 = as.factor(SECTION1.HouseHoldMembers.Q104)
    )

no_treat<- klip %>%
  filter(treat == "Non-beneficiary") %>%
  mutate(
    SECTION1.HouseHoldMembers.Q104 = case_when(
      SECTION1.HouseHoldMembers.Q104 == "Post Grad" ~ "University",
      TRUE ~ as.character(SECTION1.HouseHoldMembers.Q104)),
    SECTION1.HouseHoldMembers.Q104 = as.factor(SECTION1.HouseHoldMembers.Q104)
    )

combeducou <- dchar(treat, "County", "SECTION1.HouseHoldMembers.Q104") +
  ggtitle("a) Education levels amongst beneficiaries")

combeducounont <- dchar(no_treat, "County", "SECTION1.HouseHoldMembers.Q104") +
  ggtitle("b) Education levels amongst non-beneficiaries")

edu_comb <- grid.arrange(combeducou, combeducounont, nrow = 2)

ggsave("KLIP/combeducou.pdf", edu_comb, width = 10, height = 10)

#-----------------------Payout year by County--------------------------------------------------

payoutyr <- klip2 %>%
  dplyr::rename(
    "2015" = SECTION2HOUSEHOLDCHARACTERISTICS.Q202a2, "2016" = SECTION2HOUSEHOLDCHARACTERISTICS.Q202a4,
    "2017" = SECTION2HOUSEHOLDCHARACTERISTICS.Q202a6, "2018" = SECTION2HOUSEHOLDCHARACTERISTICS.Q202a8,
    "2019" = SECTION2HOUSEHOLDCHARACTERISTICS.Q202a10
  ) %>%
  group_by(County) %>%
  dplyr::select("2015", "2016", "2017", "2018", "2019") %>%
  summarise_if(is.numeric, mean, na.rm = T) %>%
  gather(key, value, -County) %>%
  mutate(key = as.numeric(key)) %>%
  ggplot(aes(key, value, color = County)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~County) +
  theme_fivethirtyeight() +
  ggtitle("KLIP payout per year")

ggsave("KLIP/payoutyr.pdf", payoutyr, width = 6, height = 6)

#-------------------------Cattlegain/loss-------------------------------------

cattlegain <- ggplot(klip, aes(cattlegain, fill = treat, alpha = 0.7)) +
  geom_histogram() +
  xlim(-50, 50) +
  geom_histogram(aes()) +
  geom_vline(aes(xintercept = mean(subset(cattlegain, treat == "Beneficiary"), na.rm = T)),
             linetype = "dashed", size = 0.7
  ) +
  geom_vline(aes(xintercept = mean(subset(cattlegain, treat == "Non-beneficiary"), na.rm = T)),
             linetype = "dashed",
             color = "darkblue", 
             size = 0.7
  ) +
  theme_fivethirtyeight() + 
  ggtitle("a) Difference in cattle herd size between 2015-2019")

# Cattle lost
cattleloss <- ggplot(klip, aes(treat, log(SECTION2HOUSEHOLDCHARACTERISTICS.Q205a + 1))) +
  geom_jitter(width = 0.1) +
  stat_summary(
    fun.y = median,
    geom = "crossbar",
    color = "darkred",
    width = 0.1
  ) +
  geom_violin(width = 0.5, alpha = 0.2) +
  theme_fivethirtyeight() +
  ggtitle("b) Number of cattle lost in 2019 (log+1)")

cattlegainloss <- grid.arrange(cattlegain, cattleloss, nrow = 2)

ggsave("KLIP/cattlegainloss.pdf", cattlegainloss, height = 8, width = 8)

#-----------------Agree that KLIP payouts are helpful-------------------------------------------

barch <- function(grouping, varia) {
  myaexp <- klip %>%
    dplyr::select(grouping, varia)
  # myaexp <- subset(myaexp, myaexp[[varia]] !=c("No Response "))
  # myaexp[[varia]] <- droplevels(myaexp[[varia]])
  myaexp <- subset(myaexp, myaexp[[varia]] != c("Do Not Know"))
  myaexp[[varia]] <- droplevels(myaexp[[varia]])
  mdfr <- reshape::melt(myaexp, id.vars = grouping)
  a <- data.frame(prop.table(table(mdfr[[grouping]], mdfr$value), 1) * 100)
  neg <- a %>%
    filter(Var2 %in% levels(Var2)[1:3]) %>%
    mutate(Freq = case_when(
      Var2 == "Neutral" ~ Freq / 2,
      TRUE ~ Freq
    ))
  pos <- a %>%
    filter(Var2 %in% levels(Var2)[3:5]) %>%
    mutate(
      Freq = case_when(
        Var2 == "Neutral" ~ Freq / 2,
        TRUE ~ Freq
      ),
      Var2 = factor(Var2, levels = rev(levels(Var2)))
    )
  plot <- ggplot() +
    geom_col(data = pos, aes(
      x = Var1,
      y = Freq,
      fill = Var2
    )) +
    geom_col(data = neg, aes(
      x = Var1,
      y = -Freq,
      fill = Var2
    )) +
    coord_flip() +
    theme_fivethirtyeight() +
    scale_fill_brewer(
      name = "",
      limits = levels(neg$Var2),
      labels = levels(neg$Var2),
      palette = "RdBu"
    ) +
    theme(plot.title = element_text(
      size = 11,
      face = "bold"
    )) +
    ylim(-80, 100)
}

# Graph
one <- barch("treat", "SECTION2HOUSEHOLDCHARACTERISTICS.Q211a") + ggtitle("a) KLIP Insurance payouts are helpful for households during draught period") + theme(legend.position = "none")
two <- barch("treat", "SECTION2HOUSEHOLDCHARACTERISTICS.Q211bmerge") + ggtitle("b) KLIP beneficiaries’ households are much better off than those not registered in KLIP") + theme(legend.position = "none")
three <- barch("treat", "SECTION3Localmarketperception.Q306e") + ggtitle("Humanitarian assistance increased as a result of KLIP payouts")

comb <- cowplot::plot_grid(one, two, three,
  ncol = 1,
  align = "v",
  rel_heights = c(1, 1, 1.2)
)

ggsave("KLIP/combhelp.pdf", comb, width = 10, height = 7)

#---------------------Local market perceptions-------------------------------------------

dchar <- function(grouping, varia) {
  myaexp <- klip %>%
    dplyr::select(grouping, varia)
  # myaexp <- subset(myaexp, myaexp[[varia]] !=c("No Response "))
  # myaexp[[varia]] <- droplevels(myaexp[[varia]])
  myaexp <- subset(myaexp, myaexp[[varia]] != c("Do Not Know"))
  myaexp[[varia]] <- droplevels(myaexp[[varia]])
  mdfr <- reshape::melt(myaexp, id.vars = grouping)
  a <- data.frame(prop.table(table(mdfr[[grouping]], mdfr$value), 1) * 100)
  neg <- a %>%
    filter(Var2 %in% levels(Var2)[1:3]) %>%
    mutate(Freq = case_when(
      Var2 == "Neutral" ~ Freq / 2,
      TRUE ~ Freq
    ))
  pos <- a %>%
    filter(Var2 %in% levels(Var2)[3:5]) %>%
    mutate(
      Freq = case_when(
        Var2 == "Neutral" ~ Freq / 2,
        TRUE ~ Freq
      ),
      Var2 = factor(Var2, levels = rev(levels(Var2)))
    )
  plot <- ggplot() +
    geom_col(data = pos, aes(
      x = Var1,
      y = Freq,
      fill = Var2
    )) +
    geom_col(data = neg, aes(
      x = Var1,
      y = -Freq,
      fill = Var2
    )) +
    coord_flip() +
    theme_fivethirtyeight() +
    scale_fill_brewer(name = "", limits = levels(neg$Var2), labels = levels(neg$Var2), palette = "RdBu") +
    theme(
      legend.position = "none",
      plot.title = element_text(
        size = 11,
        face = "bold"
      )
    ) +
    ylim(-80, 60)
}

one <- dchar("treat", "SECTION3Localmarketperception.Q306a") + ggtitle("Have KLIP payouts affected the numbers of livestock traded at local market?")
two <- dchar("treat", "SECTION3Localmarketperception.Q306b") + ggtitle("Has KLIP payouts affected prices of livestock traded at local market?")
three <- dchar("treat", "SECTION3Localmarketperception.Q306c") + ggtitle("Has KLIP payouts affected human food prices at local market?") + theme(legend.position = "bottom")
four <- dchar("treat", "SECTION3Localmarketperception.Q306h") + ggtitle("KLIP payouts caused no changes at local market prices") + theme(legend.position = "bottom")

combine <- cowplot::plot_grid(one, two, three, four, ncol = 2, align = "v", rel_heights = c(1, 1, 1.4, 1.4))

ggsave("KLIP/combine.pdf", combine, width = 13.5, height = 6)

five <- dchar("treat", "SECTION3Localmarketperception.Q306d") +
  ggtitle("KLIP payout payouts led to additional livestock traded") +
  ylim(-90, 80)

six <- dchar("treat", "SECTION3Localmarketperception.Q306f") +
  ggtitle("Land/asset cost or rates decreased as a result of KLIP payouts") +
  ylim(-90, 80)

seven <- dchar("treat", "SECTION3Localmarketperception.Q306g") +
  ggtitle("Vaccines, medicines price Increased as a result of KLIP payouts") +
  theme(legend.position = "bottom") + ylim(-90, 80)

combineout <- cowplot::plot_grid(five, six, seven, ncol = 1, align = "v", rel_heights = c(1, 1, 1.3))

ggsave("KLIP/combineout.pdf", combineout, width = 10, height = 7)

#------------------------Education-----------------------------------------------------

dchar <- function(grouping, varia) {
  myaexp <- klip %>%
    dplyr::select(grouping, varia)
  # myaexp <- subset(myaexp, myaexp[[varia]] !=c("No Response "))
  # myaexp[[varia]] <- droplevels(myaexp[[varia]])
  myaexp <- subset(myaexp, myaexp[[varia]] != c("Do Not Know"))
  myaexp[[varia]] <- droplevels(myaexp[[varia]])
  mdfr <- reshape::melt(myaexp, id.vars = grouping)
  a <- data.frame(prop.table(table(mdfr[[grouping]], mdfr$value), 1) * 100)
  neg <- a %>%
    filter(Var2 %in% levels(Var2)[1:3]) %>%
    mutate(Freq = case_when(
      Var2 == "Neutral" ~ Freq / 2,
      TRUE ~ Freq
    ))
  pos <- a %>%
    filter(Var2 %in% levels(Var2)[3:5]) %>%
    mutate(
      Freq = case_when(
        Var2 == "Neutral" ~ Freq / 2,
        TRUE ~ Freq
      ),
      Var2 = factor(Var2, levels = rev(levels(Var2)))
    )
  plot <- ggplot() +
    geom_col(data = pos, aes(
      x = Var1,
      y = Freq,
      fill = Var2
    )) +
    geom_col(data = neg, aes(
      x = Var1,
      y = -Freq,
      fill = Var2
    )) +
    coord_flip() +
    theme_fivethirtyeight() +
    scale_fill_brewer(
      name = "",
      limits = levels(neg$Var2),
      labels = levels(neg$Var2),
      palette = "RdBu"
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(
        size = 12,
        face = "bold"
      )
    ) +
    ylim(-35, 90)
}

one <- dchar("treat", "SECTION4Schoolattendanceperception.Q401") +
  ggtitle("HHs receiving KLIP payout are more likely to keep their children in school compared to non-KLIP HHs")

two <- dchar("treat", "SECTION4Schoolattendanceperception.Q402") +
  ggtitle("School attendance for children from KLIP HHs is higher than those which have not received a KLIP payout")

three <- dchar("treat", "SECTION4Schoolattendanceperception.Q403") +
  ggtitle("KLIP payouts support general well-being of children within Households") + theme(legend.position = "bottom")

combineedu <- cowplot::plot_grid(one, two, three, ncol = 1, align = "v", rel_heights = c(1, 1, 1.3))

ggsave("KLIP/combineedu.pdf", combineedu, width = 10, height = 7)

# Insurance perc
dchar <- function(grouping, varia) {
  myaexp <- klip %>%
    dplyr::select(grouping, varia)

  # myaexp <- subset(myaexp, myaexp[[varia]] !=c("No Response "))
  # myaexp[[varia]] <- droplevels(myaexp[[varia]])

  myaexp <- subset(myaexp, myaexp[[varia]] != c("Do Not Know"))
  myaexp[[varia]] <- droplevels(myaexp[[varia]])

  mdfr <- reshape::melt(myaexp, id.vars = grouping)
  a <- data.frame(prop.table(table(mdfr[[grouping]], mdfr$value), 1) * 100)

  neg <- a %>%
    filter(Var2 %in% levels(Var2)[1:3]) %>%
    mutate(Freq = case_when(
      Var2 == "Neutral" ~ Freq / 2,
      TRUE ~ Freq
    ))

  pos <- a %>%
    filter(Var2 %in% levels(Var2)[3:5]) %>%
    mutate(
      Freq = case_when(
        Var2 == "Neutral" ~ Freq / 2,
        TRUE ~ Freq
      ),
      Var2 = factor(Var2, levels = rev(levels(Var2)))
    )

  plot <- ggplot() +
    geom_col(
      data = pos,
      aes(
        x = Var1,
        y = Freq,
        fill = Var2
      )
    ) +
    geom_col(
      data = neg,
      aes(
        x = Var1,
        y = -Freq,
        fill = Var2
      )
    ) +
    coord_flip() +
    theme_fivethirtyeight() +
    scale_fill_brewer(
      name = "",
      limits = levels(neg$Var2),
      labels = levels(neg$Var2),
      palette = "RdBu"
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(
        size = 14,
        face = "bold"
      )
    ) +
    ylim(-65, 45)
}

one <- dchar("treat", "Section5.Q503amerge") +
  ggtitle("KLIP Payout procedure from insurance is frustrating")
two <- dchar("treat", "Section5.Q503bmerge") +
  ggtitle("Insurance contracts typically favors the  insurer, i.e. the insurance company") +
  theme(legend.position = "bottom")

combineinsu <- cowplot::plot_grid(one, two, ncol = 1, align = "v", rel_heights = c(1, 1.15))

ggsave("KLIP/combineinsu.pdf", combineinsu, width = 10, height = 6)

#---------------------Perceptions of use---------------------------------------------

dchar <- function(grouping, varia) {
  myaexp <- klip2 %>%
    dplyr::select(grouping, varia)

  # myaexp <- subset(myaexp, myaexp[[varia]] !=c("No Response "))
  # myaexp[[varia]] <- droplevels(myaexp[[varia]])

  myaexp <- subset(myaexp, myaexp[[varia]] != c("Do Not Know"))
  myaexp[[varia]] <- droplevels(myaexp[[varia]])

  mdfr <- reshape::melt(myaexp, id.vars = grouping)
  a <- data.frame(prop.table(table(mdfr[[grouping]], mdfr$value), 1) * 100)

  neg <- a %>%
    filter(Var2 %in% levels(Var2)[1:3]) %>%
    mutate(Freq = case_when(
      Var2 == "Neutral" ~ Freq / 2,
      TRUE ~ Freq
    ))

  pos <- a %>%
    filter(Var2 %in% levels(Var2)[3:5]) %>%
    mutate(
      Freq = case_when(
        Var2 == "Neutral" ~ Freq / 2,
        TRUE ~ Freq
      ),
      Var2 = factor(Var2, levels = rev(levels(Var2)))
    )

  plot <- ggplot() +
    geom_col(
      data = pos,
      aes(
        x = Var1,
        y = Freq,
        fill = Var2
      )
    ) +
    geom_col(
      data = neg,
      aes(
        x = Var1,
        y = -Freq,
        fill = Var2
      )
    ) +
    coord_flip() +
    theme_fivethirtyeight() +
    scale_fill_brewer(
      name = "",
      limits = levels(neg$Var2),
      labels = levels(neg$Var2),
      palette = "RdBu"
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(
        size = 14,
        face = "bold"
      )
    ) +
    ylim(-90, 90)
}

one <- dchar("County", "SECTION2HOUSEHOLDCHARACTERISTICS.Q202ei") +
  ggtitle("KLIP cash payouts are released at Expected Trigger Moments ?")

two <- dchar("County", "SECTION2HOUSEHOLDCHARACTERISTICS.Q202eii") +
  ggtitle("Communication/Feedback on KLIP related matters from the provider is regular") +
  theme(legend.position = "bottom")

combinerate <- cowplot::plot_grid(one, two, ncol = 1, align = "v", rel_heights = c(1, 1.15))

ggsave("KLIP/combrate.pdf", combinerate, width = 10, height = 6)

# Where do get info from
info <- klip2 %>%
  dplyr::count(SECTION2HOUSEHOLDCHARACTERISTICS.Q203c, sort = TRUE) %>%
  drop_na() %>%
  filter(n > 7) %>%
  ggplot(aes(
    x = reorder(SECTION2HOUSEHOLDCHARACTERISTICS.Q203c, -n),
    y = n,
    fill = rownames(.)
  )) +
  geom_bar(stat = "identity") +
  coord_flip() +
  guides(fill = FALSE) +
  scale_fill_brewer(palette = "Spectral") +
  theme_538() +
  ggtitle("If a payout is triggered how would you receive information about this?") +
  xlab("") +
  ylab("")

# Employment type by County
klip2 %>%
  dplyr::count(SECTION2HOUSEHOLDCHARACTERISTICS.Q209, County, sort = TRUE) %>%
  drop_na() %>%
  ggplot(aes(x = SECTION2HOUSEHOLDCHARACTERISTICS.Q209, y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  guides(fill = FALSE) +
  theme_538() +
  ggtitle("What is the primary economic activity of this Household?") +
  xlab("") +
  ylab("") +
  facet_grid(~County)

#----------------------Local market perceptions---------------------------------------
dchar <- function(grouping, varia) {
  myaexp <- klip2 %>%
    dplyr::select(grouping, varia)
  # myaexp <- subset(myaexp, myaexp[[varia]] !=c("No Response "))
  # myaexp[[varia]] <- droplevels(myaexp[[varia]])
  myaexp <- subset(myaexp, myaexp[[varia]] != c("Do Not Know"))
  myaexp[[varia]] <- droplevels(myaexp[[varia]])
  mdfr <- reshape::melt(myaexp, id.vars = grouping)
  a <- data.frame(prop.table(table(mdfr[[grouping]], mdfr$value), 1) * 100)
  neg <- a %>%
    filter(Var2 %in% levels(Var2)[1:3]) %>%
    mutate(Freq = case_when(
      Var2 == "Neutral" ~ Freq / 2,
      TRUE ~ Freq
    ))
  pos <- a %>%
    filter(Var2 %in% levels(Var2)[3:5]) %>%
    mutate(
      Freq = case_when(
        Var2 == "Neutral" ~ Freq / 2,
        TRUE ~ Freq
      ),
      Var2 = factor(Var2, levels = rev(levels(Var2)))
    )
  plot <- ggplot() +
    geom_col(
      data = pos,
      aes(
        x = Var1,
        y = Freq,
        fill = Var2
      )
    ) +
    geom_col(
      data = neg,
      aes(
        x = Var1,
        y = -Freq,
        fill = Var2
      )
    ) +
    coord_flip() +
    theme_fivethirtyeight() +
    scale_fill_brewer(
      name = "",
      limits = levels(neg$Var2),
      labels = levels(neg$Var2),
      palette = "RdBu"
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 11, face = "bold")
    ) +
    ylim(-100, 100)
}

one <- dchar("County", "SECTION3Localmarketperception.Q303a") + ggtitle("The payout from KLIP came at exactly the right time")
two <- dchar("County", "SECTION3Localmarketperception.Q303b") + ggtitle("The payout was too late for it to be useful for my livestock")
three <- dchar("County", "SECTION3Localmarketperception.Q303c") + ggtitle("The payout was made at the moment I was expecting it to be made") + theme(legend.position = "bottom")
four <- dchar("County", "SECTION3Localmarketperception.Q303e") + ggtitle("If the payout arrived earlier, I could better take care of my livestock") + theme(legend.position = "bottom")

combinetime <- cowplot::plot_grid(one, two, three, four, ncol = 2, align = "v", rel_heights = c(1, 1, 1.7, 1.7))

ggsave("KLIP/combinetime.pdf", combinetime, width = 13.5, height = 6)

one <- dchar("County", "SECTION3Localmarketperception.Q303f") +
  ggtitle("All pastoralists have heard about KLIP")

two <- dchar("County", "SECTION3Localmarketperception.Q304l") +
  ggtitle("I had difficulty accessing the payout at the bank")

three <- dchar("County", "SECTION3Localmarketperception.Q304k") +
  ggtitle("The method of payment of the payout is the best method for me")

four <- dchar("County", "SECTION3Localmarketperception.Q304") +
  ggtitle("There were registration criteria that I did not completely understand")

five <- dchar("County", "SECTION3Localmarketperception.Q304n") +
  ggtitle("The trigger gave a good representation of the actual quality of pasture") +
  theme(legend.position = "bottom")

six <- dchar("County", "SECTION3Localmarketperception.Q304m") +
  ggtitle("The payouts I have received were the very amounts I expected") +
  theme(legend.position = "bottom")

combineinfo <- cowplot::plot_grid(one, two, three, four, five, six,
  ncol = 2,
  align = "vh",
  rel_heights = c(0.8, 0.8, 1, 0.8, 0.8, 1)
)

ggsave("KLIP/combineinfo.pdf", combineinfo, width = 13.5, height = 6)

#-----------------How did you receive payment-------------------------------------
klip2 %>%
  dplyr::count(Section7FinancialServices.Q701a, County, sort = TRUE) %>%
  filter(n >= 3) %>%
  drop_na() %>%
  ggplot(aes(x = Section7FinancialServices.Q701a, y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  guides(fill = FALSE) +
  theme_538() +
  ggtitle("What is the primary economic activity of this Household?") +
  xlab("") +
  ylab("") +
  facet_wrap(~County)

# Preferred payment by County
klip2 %>%
  dplyr::count(Section7FinancialServices.Q702a, County, sort = TRUE) %>%
  filter(n >= 3) %>%
  drop_na() %>%
  ggplot(aes(x = Section7FinancialServices.Q702a, y = n, )) +
  geom_bar(stat = "identity") +
  coord_flip() +
  guides(fill = FALSE) +
  theme_538() +
  ggtitle("What is the primary economic activity of this Household?") +
  xlab("") +
  ylab("") +
  facet_wrap(~County)

# Preference on assistance
# Local market perceptions
dchar <- function(grouping, varia) {
  myaexp <- klip2 %>%
    dplyr::select(grouping, varia)

  # myaexp <- subset(myaexp, myaexp[[varia]] !=c("No Response "))
  # myaexp[[varia]] <- droplevels(myaexp[[varia]])

  myaexp <- subset(myaexp, myaexp[[varia]] != c("Do Not Know"))
  myaexp[[varia]] <- droplevels(myaexp[[varia]])

  mdfr <- reshape::melt(myaexp, id.vars = grouping)
  a <- data.frame(prop.table(table(mdfr[[grouping]], mdfr$value), 1) * 100)

  neg <- a %>%
    filter(Var2 %in% levels(Var2)[1:3]) %>%
    mutate(Freq = case_when(
      Var2 == "Neutral" ~ Freq / 2,
      TRUE ~ Freq
    ))

  pos <- a %>%
    filter(Var2 %in% levels(Var2)[3:5]) %>%
    mutate(
      Freq = case_when(
        Var2 == "Neutral" ~ Freq / 2,
        TRUE ~ Freq
      ),
      Var2 = factor(Var2, levels = rev(levels(Var2)))
    )

  plot <- ggplot() +
    geom_col(
      data = pos,
      aes(
        x = Var1,
        y = Freq,
        fill = Var2
      )
    ) +
    geom_col(
      data = neg,
      aes(
        x = Var1,
        y = -Freq,
        fill = Var2
      )
    ) +
    coord_flip() +
    theme_fivethirtyeight() +
    scale_fill_brewer(name = "", limits = levels(neg$Var2), labels = levels(neg$Var2), palette = "RdBu") +
    theme(legend.position = "none", plot.title = element_text(size = 12, face = "bold")) +
    ylim(-100, 100)
}

one <- dchar("County", "SECTION8HUMANITAIRANASSISTANCE.Q805") + ggtitle("Insurance Payouts are more reliable than Humanitarian Assistance ")
two <- dchar("County", "SECTION8HUMANITAIRANASSISTANCE.Q805") + ggtitle("With KLIP you can be certain that they will issue Payout when there is severe drought")
three <- dchar("County", "SECTION8HUMANITAIRANASSISTANCE.Q809") + ggtitle("I prefer the HNSP over KLIP because they give more assistance") + theme(legend.position = "bottom")
four <- dchar("County", "SECTION8HUMANITAIRANASSISTANCE.Q811") + ggtitle("The KLIP payout is always sufficient to help us with our livestock during the seasons of draught or short rains") + theme(legend.position = "bottom")
five <- dchar("County", "SECTION8HUMANITAIRANASSISTANCE.Q812") + ggtitle("I would prefer KLIP to humanitarian aid  (msaada) issued by Government of other Agencies") + theme(legend.position = "bottom")
six <- dchar("County", "SECTION8HUMANITAIRANASSISTANCE.Q806") + ggtitle("With the HNSP I know in advance if I will receive assistance or not") + theme(legend.position = "bottom")

combinecomp <- cowplot::plot_grid(one, two, three, four, five, six, ncol = 2, align = "v", rel_heights = c(1, 1, 1, 1, 1.2, 1.2))

ggsave("KLIP/combinecomp.pdf", combinecomp, width = 16, height = 12)

#-------------------------Regression------------------------------------------
cattlelm <- lm_robust(log(cattlegainpc + 1) ~ SECTION1.HouseHoldMembers.HHGender + SECTION1.HouseHoldMembers.Q103a + SECTION1.HouseHoldMembers.Q106 +
                        SECTION1.HouseHoldMembers.Q104 + SECTION1.HouseHoldMembers.MaritalStatus + SECTION1.HouseHoldMembers.Q106 +
                        SECTION2HOUSEHOLDCHARACTERISTICS.Q201a + Section5.Q501b + klip$SECTION2HOUSEHOLDCHARACTERISTICS.cattleownedsep18, 
                      clusters = SubCounty, 
                      data = klip)
cattleplot <- plot_model(cattlelm,
  prefix.labels = "varname",
  vline.color = "black"
  ) +
  theme_fivethirtyeight() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) +
  ggtitle("Associations with cattle gain/loss over entire KLIP period")

ggsave("KLIP/cattleplot.pdf", cattleplot, width = 13, height = 10)

#-------------------------------------Regression--------------------------------------------
paylm <- lm_robust(
  totalpay ~ SECTION1.HouseHoldMembers.HHGender + SECTION1.HouseHoldMembers.Q103a + SECTION1.HouseHoldMembers.Q106 +
    SECTION1.HouseHoldMembers.Q104 + SECTION1.HouseHoldMembers.MaritalStatus + SECTION1.HouseHoldMembers.Q106 +
    SECTION2HOUSEHOLDCHARACTERISTICS.Q201a + Section5.Q501b + SECTION2HOUSEHOLDCHARACTERISTICS.Q205a,
  clusters = SubCounty,
  klip
  )

payplot <- plot_model(paylm,
  prefix.labels = "varname",
  vline.color = "black"
  ) +
  theme_fivethirtyeight() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) +
  ggtitle("Breakdown of total KLIP payouts (across all years)")

ggsave("KLIP/payplot.pdf", payplot, width = 10, height = 10)

#========================Type of payout ===================================================
type_payout <- klip2 %>% 
  filter(!is.na(SECTION2HOUSEHOLDCHARACTERISTICS.Q201f)) %>%
  ggplot(aes(SECTION2HOUSEHOLDCHARACTERISTICS.Q201f)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  coord_flip() +
  theme_538() + 
  facet_wrap(~County) + 
  ylab("Proportion") + 
  xlab("") +
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(
      size = 12,  face = "bold"
      )
    )

receive_payout <- klip2 %>% 
  filter(!is.na(SECTION2HOUSEHOLDCHARACTERISTICS.Q201e)) %>%
  ggplot(aes(SECTION2HOUSEHOLDCHARACTERISTICS.Q201e)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  coord_flip() +
  theme_538() + 
  facet_wrap(~County) + 
  ylab("Proportion") + 
  xlab("") +
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(
      size = 12,  face = "bold"
      )
    )

ggsave("KLIP/receive_payout.pdf", receive_payout, width = 9, height = 6)
ggsave("KLIP/type_payout.pdf", type_payout, width = 11, height = 6)

#--------------Beneficiaries are better off -------------------------------
dchar <- function(grouping, varia) {
  myaexp <- klip2 %>%
    dplyr::select(grouping, varia)
  
  # myaexp <- subset(myaexp, myaexp[[varia]] !=c("No Response "))
  # myaexp[[varia]] <- droplevels(myaexp[[varia]])
  
  myaexp <- subset(myaexp, myaexp[[varia]] != c("Do Not Know"))
  myaexp[[varia]] <- droplevels(myaexp[[varia]])
  
  mdfr <- reshape::melt(myaexp, id.vars = grouping)
  a <- data.frame(prop.table(table(mdfr[[grouping]], mdfr$value), 1) * 100)
  
  neg <- a %>%
    filter(Var2 %in% levels(Var2)[1:3]) %>%
    mutate(Freq = case_when(
      Var2 == "Neutral" ~ Freq / 2,
      TRUE ~ Freq
    ))
  
  pos <- a %>%
    filter(Var2 %in% levels(Var2)[3:5]) %>%
    mutate(
      Freq = case_when(
        Var2 == "Neutral" ~ Freq / 2,
        TRUE ~ Freq
      ),
      Var2 = factor(Var2, levels = rev(levels(Var2)))
    )
  
  plot <- ggplot() +
    geom_col(
      data = pos,
      aes(
        x = Var1,
        y = Freq,
        fill = Var2
      )
    ) +
    geom_col(
      data = neg,
      aes(
        x = Var1,
        y = -Freq,
        fill = Var2
      )
    ) +
    coord_flip() +
    theme_fivethirtyeight() +
    scale_fill_brewer(
      name = "",
      limits = levels(neg$Var2),
      labels = levels(neg$Var2),
      palette = "RdBu"
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(
        size = 14,
        face = "bold"
      )
    ) +
    ylim(-30, 100)
}

one <- dchar("County", "SECTION2HOUSEHOLDCHARACTERISTICS.Q211c") +
  ggtitle("KLIP beneficiaries are much better off than those not registered in KLIP")

ggsave("KLIP/beneficiary_better.pdf", one, width = 9, height = 6)

#---------Reason for animal loss --------------------
dchar <- function(grouping, varia) {
  myaexp <- klip %>%
    dplyr::select(grouping, varia)
  # myaexp <- subset(myaexp, myaexp[[varia]] !=c("No Response "))
  # myaexp[[varia]] <- droplevels(myaexp[[varia]])
  myaexp <- subset(myaexp, myaexp[[varia]] != c("Do Not Know"))
  myaexp[[varia]] <- droplevels(myaexp[[varia]])
  mdfr <- reshape::melt(myaexp, id.vars = grouping)
  a <- data.frame(prop.table(table(mdfr[[grouping]], mdfr$value), 1) * 100)
  neg <- a %>%
    filter(Var2 %in% levels(Var2)[1:3]) %>%
    mutate(Freq = case_when(
      Var2 == "Neutral" ~ Freq / 2,
      TRUE ~ Freq
    ))
  pos <- a %>%
    filter(Var2 %in% levels(Var2)[3:5]) %>%
    mutate(
      Freq = case_when(
        Var2 == "Neutral" ~ Freq / 2,
        TRUE ~ Freq
      ),
      Var2 = factor(Var2, levels = rev(levels(Var2)))
    )
  plot <- ggplot() +
    geom_col(data = pos, aes(
      x = Var1,
      y = Freq,
      fill = Var2
    )) +
    geom_col(data = neg, aes(
      x = Var1,
      y = -Freq,
      fill = Var2
    )) +
    coord_flip() +
    theme_fivethirtyeight() +
    scale_fill_brewer(name = "", limits = levels(neg$Var2), labels = levels(neg$Var2), palette = "RdBu") +
    theme(
      legend.position = "none",
      plot.title = element_text(
        size = 11,
        face = "bold"
      )
    ) +
    ylim(-100, 20)
}

one <- dchar("treat", "SECTION2HOUSEHOLDCHARACTERISTICS.Q206a") + 
  ggtitle("Have KLIP payouts affected the numbers of livestock traded at local market?") + 
  theme(legend.position = "bottom")

#------- Understand how KLIP works -----------------
dchar <- function(grouping, varia) {
  myaexp <- klip2 %>%
    dplyr::select(grouping, varia)
  
  # myaexp <- subset(myaexp, myaexp[[varia]] !=c("No Response "))
  # myaexp[[varia]] <- droplevels(myaexp[[varia]])
  
  myaexp <- subset(myaexp, myaexp[[varia]] != c("Do Not Know"))
  myaexp[[varia]] <- droplevels(myaexp[[varia]])
  
  mdfr <- reshape::melt(myaexp, id.vars = grouping)
  a <- data.frame(prop.table(table(mdfr[[grouping]], mdfr$value), 1) * 100)
  
  neg <- a %>%
    filter(Var2 %in% levels(Var2)[1:3]) %>%
    mutate(Freq = case_when(
      Var2 == "Neutral" ~ Freq / 2,
      TRUE ~ Freq
    ))
  
  pos <- a %>%
    filter(Var2 %in% levels(Var2)[3:5]) %>%
    mutate(
      Freq = case_when(
        Var2 == "Neutral" ~ Freq / 2,
        TRUE ~ Freq
      ),
      Var2 = factor(Var2, levels = rev(levels(Var2)))
    )
  
  plot <- ggplot() +
    geom_col(
      data = pos,
      aes(
        x = Var1,
        y = Freq,
        fill = Var2
      )
    ) +
    geom_col(
      data = neg,
      aes(
        x = Var1,
        y = -Freq,
        fill = Var2
      )
    ) +
    coord_flip() +
    theme_fivethirtyeight() +
    scale_fill_brewer(
      name = "",
      limits = levels(neg$Var2),
      labels = levels(neg$Var2),
      palette = "RdBu"
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(
        size = 14,
        face = "bold"
      )
    ) +
    ylim(-100, 100)
}

one <- dchar("County", "SECTION3Localmarketperception.Q303g") +
  ggtitle("All pastoralists understand how KLIP works")

two <- dchar("County", "SECTION3Localmarketperception.Q303h") +
  ggtitle("Most pastoralists do not even know if they're registered for KLIP")

three <- dchar("County", "SECTION3Localmarketperception.Q304j") +
  ggtitle("I am aware of exactly when I will be recieving a KLIP payout")

market <- grid.arrange(one, two, three, ncol = 1)

ggsave("KLIP/market.pdf", market, width = 12, height = 10)

#------------ Insurance products -----------------
# Gender difference heard of Klip before
barch <- function(data, cat, ans) {
  myaexp <- data %>%
    dplyr::select(cat, ans)
  
  myaexp <- subset(myaexp, myaexp[[ans]] != c("No response"))
  myaexp[[ans]] <- droplevels(myaexp[[ans]])
  
  # myaexp <- subset(myaexp, myaexp[[ans]] !=c("Do Not Know"))
  # myaexp[[ans]] <- droplevels(myaexp[[ans]])
  
  mdfr <- reshape::melt(myaexp, id.vars = cat)
  
  a <- data.frame(prop.table(table(mdfr[[cat]], mdfr$value), 1) * 100)
  
  chplot <- ggplot(
    a,
    aes(
      x = Var1,
      y = Freq,
      fill = forcats::fct_rev(Var2)
    )
  )  +
    labs(x = "", y = "Pecentage") +
    scale_fill_manual(values=c("#1372a4", "#e57137")) +
    geom_bar(stat = "identity") +
    theme_538() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(
        size = 20,
        family = "",
        color = "Black",
        face = "bold",
        hjust = 0.5
      ),
      legend.text = element_text(size = 16),
      axis.text = element_text(
        size = 16,
        family = "",
        color = "Black",
        hjust = 0
      ),
      axis.title = element_text(
        size = 18,
        family = "",
        color = "Black",
        face = "bold",
        hjust = 0.5
      )
    ) +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE))
  chplot
}

dat <- klip %>%
  filter(Section5.Q501b != "Don't know") %>%
  mutate(Section5.Q501b = droplevels(Section5.Q501b))

one <- barch( dat, "treat", "SECTION2HOUSEHOLDCHARACTERISTICS.Q201a") +
  xlab("") +
  ggtitle("Have you ever heard of KLIP?")  +
  theme(legend.position = "none") 

two <- barch( dat, "SECTION1.HouseHoldMembers.HHGender", "SECTION2HOUSEHOLDCHARACTERISTICS.Q201a") +
  xlab("") +
  ggtitle("Have you ever heard of KLIP?")  +
  theme(legend.position = "none") 

three <- barch( dat, "County", "SECTION2HOUSEHOLDCHARACTERISTICS.Q201a") +
  xlab("") +
  ggtitle("Have you ever heard of KLIP?") + 
  theme(legend.title = element_blank())

comb <- cowplot::plot_grid(one,two,three, ncol=1, align = "vh") 

ggsave("KLIP/comb_insu.pdf", comb, width = 10, height = 12)




