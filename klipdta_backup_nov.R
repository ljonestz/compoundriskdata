#--------------------LOAD PACKAGES--------------------
packages <- c("forcats", "huxtable", "ggplot2", 'ggthemr', 'dplyr', 'lubridate', 'foreign', 'kableExtra', 'knitr','stargazer', 'haven', 'tab')

# To install packages for the first time use the function below:
# lapply(packages, function(x) {install.packages(x)}) 

# If already installed, use this function
sapply(packages, library, character.only=T)

#Create beneficiary dataframe
klip2 <- read_dta("~/Dropbox/KLIP Consumer Value Lindsey/dta-files/Beneficiaries.dta")
klip2 <- as.data.frame(klip2)

#Make colnames as attributes
attr(klip2$endtime, "label") <- "endtime"
attr(klip2$today, "label") <- "today"
colnames(klip2)<- unlist(lapply(colnames(klip2), function(xx) {attr(klip2[,xx], "label")}))
klip2$treat <- "Beneficiary" 

#Change to factors
cat <- c("SECTION1:HouseHoldMembers:HHGender", "SECTION1:HouseHoldMembers:Q104", "SECTION1:HouseHoldMembers:MaritalStatus",
         "Section5:Q501b")
klip2[cat] <- lapply(klip2[cat], function(xx) {as.factor(xx)})

#Load choices
klipchoice <- read_excel("~/Dropbox/KLIP Consumer Value Lindsey/Titus data/Analysis frame KLIP Percep study/KLIP-Beneficiaries-Codebook.xlsx",  sheet = "choices")

#format names NEED TO ADDRESS ISSUE WITH 201g and Q501c
klipnames <- gsub("choices_", "", klipchoice$`list name`)
klipnames2 <- gsub("_", ":", klipnames)
klipchoice$`list name` <- klipnames2
kliplevels <- klipchoice %>%
  spread(`list name`, `label::English`) %>%
  mutate_if(is.character, as.factor) %>%
  select(-name, -Consent, -County, -`SECTION2HOUSEHOLDCHARACTERISTICS:Q201g`, -`Section5:Q501c`, -`Section5:Q506`)

#Reorder levels to align
nam <- colnames(kliplevels)
kliplevels[nam] <- lapply(kliplevels[nam], function(cc){
  fct_inorder(cc)
})

#Combine levels
klip2[colnames(kliplevels)] <- lapply(klip2[colnames(kliplevels)], function(dd){
  as.factor(dd)
})

#Merge levels
for( i in colnames(kliplevels)){
  levels(klip2[[i]])<- levels(kliplevels[[i]])
}

#Create non-beneficiary dataframe
klipnon2 <- read_dta("~/Dropbox/KLIP Consumer Value Lindsey/dta-files/NonBeneficiaries.dta")
klipnon2 <- as.data.frame(klipnon2)
#Make colnames as attributes
attr(klipnon2[,2], "label") <- colnames(klipnon2[2])
attr(klipnon2[,3], "label") <- colnames(klipnon2[3])
attr(klipnon2[,4], "label") <- colnames(klipnon2[4])
attr(klipnon2[,5], "label") <- colnames(klipnon2[5])
attr(klipnon2[,6], "label") <- colnames(klipnon2[6])
attr(klipnon2[,7], "label") <- colnames(klipnon2[7])
attr(klipnon2[,8], "label") <- colnames(klipnon2[8])
attr(klipnon2[,9], "label") <- colnames(klipnon2[9])
attr(klipnon2[,10], "label") <- colnames(klipnon2[10])
attr(klipnon2[,11], "label") <- colnames(klipnon2[11])
attr(klipnon2[,12], "label") <- colnames(klipnon2[12])
attr(klipnon2[,238], "label") <- colnames(klipnon2[238])
attr(klipnon2[,239], "label") <- colnames(klipnon2[239])
colnames(klipnon2)<- unlist(lapply(colnames(klipnon2), function(xx) {attr(klipnon2[,xx], "label")}))
klipnon2$treat <- "Non-beneficiary" 
colnames(klipnon2) <- gsub("InsuranceAwarenessHouseholdAssets", "HOUSEHOLDCHARACTERISTICS", colnames(klipnon2))






#Merge dataset
klip <- klip2 %>%
  full_join(klipnon2, by = intersect(c(colnames(klip2[1:84]),colnames(klip2[231:264]), "treat"), c(colnames(klipnon2[1:84]),colnames(klipnon2[186:218]), "treat"))) %>%
  group_by(`meta:instanceID`)
klip <- as.data.frame(klip)

#Change variable types
cat <- c("SECTION1:HouseHoldMembers:HHGender", "SECTION1:HouseHoldMembers:Q104", "SECTION1:HouseHoldMembers:MaritalStatus",
         "Section5:Q501b")
klip[cat] <- lapply(klip[cat], function(xx) {as.factor(xx)})
klipnon2[cat] <- lapply(klipnon2[cat], function(xx) {as.factor(xx)})

#Crosstab
tab <- tabmulti(`SECTION1:HouseHoldMembers:HHGender` + `SECTION1:HouseHoldMembers:Q103a`  + `SECTION1:HouseHoldMembers:Q106` +
           `SECTION1:HouseHoldMembers:Q104` + `SECTION1:HouseHoldMembers:MaritalStatus` + `SECTION1:HouseHoldMembers:Q106` + 
           `SECTION2HOUSEHOLDCHARACTERISTICS:Q201a` + `Section5:Q501b`  ~ treat, data=klip)
tabh <- as_hux(tab)
quick_html(theme_article(tabh), file="klipxtab.html")

  
