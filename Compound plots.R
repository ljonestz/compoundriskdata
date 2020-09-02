######################################################################################################
#
#  CODE USED TO TO PRODUCE COMPARISON PLOTS FOR THE COMPOUND RISK MONITOR
#  (to be run after the global database has been generated)
#
######################################################################################################

#------------------------LOAD PACKAGES-------------------------
#install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(ggplot2, cowplot, lubridate, rvest,dplyr, viridis, tidyverse, 
                 countrycode, corrplot, cttobin/ggthemr,  ggalt, gridExtra, ggcorrplot,
                 ggExtra, ggrepel, knitr, kableExtra, grid, wppExplorer)

#Load themes
theme_set(theme_classic(base_size = 16))
ggthemr("fresh")

#------------------Global plots -------------------------
#Loading world database
world <- map_data("world")
world <- world %>%
  dplyr::rename(Country = region) %>%
  dplyr::mutate(Country = countrycode(Country, origin = 'country.name', destination = 'iso3c'))

#Join datasets with risk flags
worldmap <- inner_join(world, riskflags, by="Country")

#Map theme
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
  plot.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F") , 
  legend.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
  text = element_text(colour = "lightgrey")
)

#Draw map one
map <- ggplot(data = worldmap, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM)) +
  scale_fill_distiller(palette ="Blues", direction = 1) + # or direction=1
  ggtitle("Total Existing Compound Risk Score") +
  plain +
  labs(fill = "Total # of risks")

#Draw map two
map2 <- ggplot(data = worldmap, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("Total Emerging Compound Risk Score") +
  plain +
  labs(fill = "Total # of risks")

#Save maps
ggsave("Plots/globalmapone.pdf", map, width = 8, height = 12)
ggsave("Plots/globalmaptwo.pdf", map2, width = 8, height = 12)

#--------------Correlations of all source indicators that feed into the compound risk monitor--------
#Subset data
vars <- globalrisk %>%
  select(S_OCHA_Covid.vulnerability.index_norm, H_Oxrollback_score_norm, H_Covidgrowth_casesnorm,
         H_Covidgrowth_deathsnorm, H_HIS_Score_norm,F_Proteus_Score_norm, F_Fewsnet_Score_norm, 
         F_Artemis_Score_norm,F_FAO_6mFPV_norm, Fr_ACLED_event_same_month_difference_perc_norm,
         Fr_ACLED_fatal_same_month_difference_perc_norm,D_WB_Overall_debt_distress_norm,  D_IMF_debt2020.2019_norm,
         M_Economic_and_Financial_score_norm, M_GDP_IMF_2019minus2020_norm, M_GDP_WB_2019minus2020_norm,
         NH_UKMO_TOTAL.RISK.NEXT.6.MONTHS_norm,NH_GDAC_Hazard_Score_Norm, Fr_INFORM_Fragility_Score_norm, Fr_FSI_Score_norm,
         Fr_FSI_2019minus2020_norm, Fr_REIGN_couprisk3m_norm)

colnames(vars) <- c("S_VI", "H_OX", "H_CGN","H_CGD", "H_HIS_norm", "F_PS", "F_FS", "F_AS","F_FPV", 
                    "Fr_AE",'Fr_AF',"D_WBD",  'D_IMFD',"M_EFS", 'M_IMFG', "M_WBG",
                    "NH_UKMO", "NH_GDAC", "Fr_IFS", 'Fr_FSI', "Fr_FSID", "Fr_RE")

#Datasets for the small sector plots
varsone <- vars %>%
  select(H_OX, H_CGN,H_CGD, H_HIS_norm,)

varstwo <- vars %>%
  select(F_FS, F_AS,F_FPV)

varsthree <- vars %>%
  select( Fr_IFS, Fr_FSI, Fr_FSID, Fr_RE, Fr_AE, Fr_AF)

#Correlations
corr <- round(cor(vars, na.rm=T, use='pairwise.complete.obs'), 1)
corrone <- round(cor(varsone, na.rm=T, use='pairwise.complete.obs'), 1)
corrtwo <- round(cor(varstwo, na.rm=T, use='pairwise.complete.obs'), 1)
corrthree <- round(cor(varsthree, na.rm=T, use='pairwise.complete.obs'), 1)

#Pvalues
p.mat <- cor_pmat(vars, na.rm=T, use='pairwise.complete.obs') 
p.matone <- cor_pmat(varsone, na.rm=T, use='pairwise.complete.obs') 
p.mattwo <- cor_pmat(varstwo, na.rm=T, use='pairwise.complete.obs') 
p.matthree <- cor_pmat(varsthree, na.rm=T, use='pairwise.complete.obs') 

#Plots
plot <- ggcorrplot(corr, 
           hc.order = FALSE,
           type = "lower", 
           p.mat = p.mat,
           outline.col = "white",
           colors = c("#6D9EC1", "white", "#E46726")) +
  theme(plot.margin=unit(c(1,1,1, -0.5),"cm"), 
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.key.width = unit(3, "cm"),
        legend.title = element_blank())

plotone <- ggcorrplot(corrone,
                   type = "lower", 
                   p.mat = p.matone,
                   outline.col = "white",
                   colors = c("#6D9EC1", "white", "#E46726")) +
  theme(legend.position = "none",
        plot.margin=unit(c(1,-0.5,1, 1),"cm"))

plottwo <- ggcorrplot(corrtwo, 
                   hc.order = FALSE,
                   type = "lower", 
                   p.mat = p.mattwo,
                   outline.col = "white",
                   colors = c("#6D9EC1", "white", "#E46726")) +
  theme(legend.position = "none",
        plot.margin=unit(c(1,-0.5,1, 1),"cm"))

plotthree <- ggcorrplot(corrthree, 
                   hc.order = FALSE,
                   type = "lower", 
                   p.mat = p.matthree,
                   outline.col = "white",
                   colors = c("#6D9EC1", "white", "#E46726")) +
  theme(legend.position = "none", 
        plot.margin=unit(c(1,-0.5,1, 1),"cm"))

#Join and save
col <- grid.arrange(plotone, plottwo, plotthree, ncol=1,  heights = c(1.5, 1.2, 1.5))
colone <- plot_grid(col, plot,  ncol=2, rel_widths = c(1,5), rel_heights=c(6,2), align = "h")
ggsave("Plots/corrplot.pdf", colone, width = 12, height = 10, units="in")

#------------------------------Comparison between different overall risk scores--------------------
#Subset dataset
rankco <- riskflags %>%
  select(Countryname, EMERGING_RISK_COVID_RESPONSE_CAPACITY_SQ_ALT, EMERGING_RISK_COVID_RESPONSE_CAPACITY_SQ) 
rankco$sign <- ifelse(rankco$EMERGING_RISK_COVID_RESPONSE_CAPACITY_SQ_ALT - rankco$EMERGING_RISK_COVID_RESPONSE_CAPACITY_SQ > 0, "red" ,
                      "green")
#Country labels
left_label <- paste(rankco$Countryname, round(rankco$EMERGING_RISK_COVID_RESPONSE_CAPACITY_SQ), sep=", ")
right_label <-  paste(rankco$Countryname, round(rankco$EMERGING_RISK_COVID_RESPONSE_CAPACITY_SQ_ALT),sep=", ")
#Find one from each integer
subset <- unlist(lapply(c(0,3,6,10), function(xx){which(round(rankco$EMERGING_RISK_COVID_RESPONSE_CAPACITY_SQ, 0) ==xx)[1]}))
left_label[-subset] <- " "
right_label[-subset] <- " "

theme_set(theme_classic())

#First plot
one <- ggplot(rankco) + 
  geom_segment(aes(x=1, xend=2, y=`EMERGING_RISK_COVID_RESPONSE_CAPACITY_SQ`, yend=`EMERGING_RISK_COVID_RESPONSE_CAPACITY_SQ_ALT`, col=sign), size=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="#00ba38", "red"="#f8766d")) +
  labs(x="", y="") +
  xlim(.5, 2.5) + ylim(0,10.3) +
  geom_text(label=left_label, y=rankco$EMERGING_RISK_COVID_RESPONSE_CAPACITY_SQ, x=rep(1, NROW(rankco)), hjust=1.1, size=5) + 
  geom_text(label=right_label, y=rankco$EMERGING_RISK_COVID_RESPONSE_CAPACITY_SQ_ALT, x=rep(2, NROW(rankco)), hjust=-0.1, size=5) +
  geom_text(label="GeoAv Outcome", x=0.8, y=10.3, size= 5)  +
  geom_text(label="GeoAv All", x=2, y=10.3, hjust=-0.1, size=5) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        line = element_blank(),
        axis.text = element_blank(),
        plot.margin=unit(c(1,-0.5 ,1, 1),"cm"),
        title = element_text(size = 20, hjust = 0.5, face = "bold")) +
  ggtitle("COVID Response risk")

#Second subset for fragility and institutions
rankco <- riskflags %>%
  select(Countryname, EMERGING_RISK_FRAGILITY_INSTITUTIONS_MULTIDIMENSIONAL, EMERGING_RISK_FRAGILITY_INSTITUTIONS) 
rankco$sign <- ifelse(rankco$EMERGING_RISK_FRAGILITY_INSTITUTIONS_MULTIDIMENSIONAL - rankco$EMERGING_RISK_FRAGILITY_INSTITUTIONS > 0, "red" ,
                      "green")
left_label <- paste(rankco$Countryname, round(rankco$EMERGING_RISK_FRAGILITY_INSTITUTIONS), sep=", ")
right_label <-  paste(rankco$Countryname, round(rankco$EMERGING_RISK_FRAGILITY_INSTITUTIONS_MULTIDIMENSIONAL),sep=", ")
#Find one from each integer
subset <- unlist(lapply(c(0,3,6,10), function(xx){which(round(rankco$EMERGING_RISK_FRAGILITY_INSTITUTIONS, 0) ==xx)[1]}))
left_label[-subset] <- " "
right_label[-subset] <- " "

#Plot second 
theme_set(theme_classic())
two <- ggplot(rankco) + 
  geom_segment(aes(x=1, xend=2, y=`EMERGING_RISK_FRAGILITY_INSTITUTIONS`, yend=`EMERGING_RISK_FRAGILITY_INSTITUTIONS_MULTIDIMENSIONAL`, col=sign), size=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="#00ba38", "red"="#f8766d")) +
  labs(x="", y="") +
  xlim(.5, 2.5) + ylim(0,10.3) +
  geom_text(label=left_label, y=rankco$EMERGING_RISK_FRAGILITY_INSTITUTIONS, x=rep(1, NROW(rankco)), hjust=1.1, size=5) + 
  geom_text(label=right_label, y=rankco$EMERGING_RISK_FRAGILITY_INSTITUTIONS_MULTIDIMENSIONAL, x=rep(2, NROW(rankco)), hjust=-0.1, size=5) +
  geom_text(label="Single", x=0.8, y=10.3, size=5)  +
  geom_text(label="Multiple", x=2, y=10.3, hjust=-0.1, size=5) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(1,2,1,2), "cm"),
        line = element_blank(),
        axis.text = element_blank(),
        title = element_text(size = 20, hjust = 0.5, face = "bold")) +
  ggtitle("Fragility and Institutional risk")

#Join and save
together <- cowplot::plot_grid(one, two, ncol=2)
ggsave("Plots/rankplot.pdf", together, width = 18, height = 10)

#--------------Correlation plots-----------------------------------
theme_set(theme_classic())

#Convert to continents
riskflags$Continent <- countrycode(riskflags$Country, origin = 'iso3c', destination = 'continent')

one <- ggplot(riskflags, aes(TOTAL_EXISTING_COMPOUND_RISK_SCORE, TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM, color=Continent)) +
  geom_count() +
  geom_line(stat="smooth", method="lm", se = F, alpha = 0.6)+
  xlab("Risk score with max value flags") +
  ylab("Risk score with max and medium flags") +
  ggtitle("Existing Compound Risk") +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        legend.text = element_text(size = 14),
        title = element_text(size = 16, hjust = 0.5, face = "bold")) +
  ggrepel::geom_text_repel(data = riskflags  %>%
                             sample_n(5),
                           aes(label = Countryname),
                           arrow = arrow(length = unit(0.01, 'npc')),
                           size = 5,
                           box.padding = 3)
#Add histogram
one <- ggMarginal(one, type = "histogram", fill="transparent")

two <- ggplot(riskflags, aes(TOTAL_EMERGING_COMPOUND_RISK_SCORE, TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM, color= Continent)) +
  geom_count() +
  geom_line(stat="smooth", method="lm", se = F, alpha = 0.6) +
  xlab("Risk score with max value flags") +
  ylab("Risk score with max and medium flags") +
  ggtitle("Emerging Compound Risk") +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        legend.text = element_text(size = 14),
        title = element_text(size = 16, hjust = 0.5, face = "bold")) +
  ggrepel::geom_text_repel(data = riskflags  %>%
                             sample_n(5),
                           aes(label = Countryname),
                           arrow = arrow(length = unit(0.01, 'npc')),
                           size = 5,
                           box.padding = 3)

two <- ggMarginal(two, type = "histogram", fill="transparent")

join <- cowplot::plot_grid(one, two, ncol=2)
ggsave("Plots/compareriskscore.pdf", join, width = 15, height = 6)

#--------------------Reliability test ------------------------------
one <- ggplot(riskflags, aes(TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM, RELIABILITY_SCORE_EMERGING_RISK, color = Continent)) +
  geom_point() +
  geom_line(stat="smooth", method="lm", se = F, alpha = 0.6) +
  xlab("Emerging risk score") +
  ylab("")  +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=18)) 
#Add histogram
one <- ggMarginal(one, type = "histogram", fill="transparent")

two <- ggplot(riskflags, aes(TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM, RELIABILITY_SCORE_EMERGING_RISK, color = Continent)) +
  geom_point() +
  geom_line(stat="smooth", method="lm", se = F, alpha = 0.6) +
  xlab("Existing risk score") +
  ylab("Reliability Score")  +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=18)) 

two <- ggMarginal(two, type = "histogram", fill="transparent")

rel <- cowplot::plot_grid(two, one, align="h")
ggsave("Plots/reliabilescore.pdf", rel, width = 12, height = 6)

#------------------Comparison between current and future risk----------------------------------
#Correlations of all source indicators that feed into the compound risk monitor
rvar <- riskflags %>%
  select(EXISTING_RISK_COVID_RESPONSE_CAPACITY, EXISTING_RISK_FOOD_SECURITY,
         EXISTING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID, 
         EXISTING_RISK_FISCAL, EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY, 
         EXISTING_RISK_NATURAL_HAZARDS, EXISTING_RISK_FRAGILITY_INSTITUTIONS, 
         EMERGING_RISK_COVID_RESPONSE_CAPACITY, EMERGING_RISK_FOOD_SECURITY, 
         EMERGING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID, EMERGING_RISK_FISCAL, 
         EMERGING_RISK_NATURAL_HAZARDS, EMERGING_RISK_FRAGILITY_INSTITUTIONS)

colnames(rvar) <- c("EX_Covid", "EX_FoodS", "EX_Macro", "EX_Fiscal", "EX_Socio", "EX_Natural", "EX_Fragile",
"EM_Covid", "EM_FoodS", "EM_Macro", "EM_Fiscal",  "EM_Natural", "EM_Fragile")

#Calculate correlations
rcorr <- round(cor(rvar, na.rm=T, use='pairwise.complete.obs'), 1)
rp.mat <- cor_pmat(rvar, na.rm=T, use='pairwise.complete.obs') 

#Plot
plot <- ggcorrplot(rcorr, 
                   hc.order = FALSE, 
                   p.mat = rp.mat,
                   outline.col = "white",
                   colors = c("#6D9EC1", "white", "#E46726")) +
  theme(plot.margin=unit(c(1,1,1, -0.5),"cm"), 
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.key.width = unit(3, "cm"),
        legend.title = element_blank())

ggsave("Plots/Riskcorr.pdf", plot, width = 10, height = 10)
  
#----------------Compare emerging and existing risk------------------------------------
ggthemr_reset()
theme_set(theme_classic(base_size = 16))

comb <- riskflags %>%
  select(Countryname, TOTAL_EXISTING_COMPOUND_RISK_SCORE, TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM) %>%
  arrange(TOTAL_EXISTING_COMPOUND_RISK_SCORE)

#Pick labels to show subset of countries
a <- 1:190
tennum <- a[c(seq(1, length(a), 30), 190)]
countrylab <- as.character(comb$Countryname)
countrylab[-tennum] <- ""

#Plot
ploty <- ggplot(comb, aes(x=TOTAL_EXISTING_COMPOUND_RISK_SCORE, xend=TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM, y=reorder(Countryname, TOTAL_EXISTING_COMPOUND_RISK_SCORE), group=Countryname)) + 
  geom_dumbbell(color="#a3c4dc", 
                size=0.75, 
                colour_xend = "darkred") + 
  scale_y_discrete(labels = countrylab) +
  ylab("Country") +
  xlab("Change in risk score") +
  theme(axis.ticks = element_blank(),
        axis.title = element_text(size = 20, hjust = 0.5),
        axis.text = element_text(size=16)) +
  geom_text_repel(data = comb %>% sample_n(10), 
                  aes(label = Countryname) , 
                  hjust = "left", 
                  fontface = "bold", 
                  color = "darkred",
                  segment.color = "black",
                  segment.alpha = 0.7,
                  segment.size = 0.3,
                  size = 6, 
                  nudge_x = 2, 
                  nudge_y = -1,
                  direction = "y")  +
  theme(axis.text.y = element_blank())

ggsave("Plots/changerisk.pdf", ploty, height = 10, width = 12)
  
#--------------Comparing max and geometric averages----------------------------------------
#Set theme
gtheme <- theme(axis.ticks = element_blank(),
      axis.title = element_text(size = 20, hjust = 0.5),
      axis.text = element_text(size=16),
      legend.text = element_text(size=16),
      title = element_text(size=18, face = "bold" ))

#Plot each graph
one <- ggplot(riskflags, aes(EMERGING_RISK_FRAGILITY_INSTITUTIONS, EMERGING_RISK_FRAGILITY_INSTITUTIONS_AV, color = Continent)) +
  geom_count(alpha = 0.7) + 
  xlab("Max value") +
  ylab("Geometric mean") +
  ggtitle("a) Fragility and Institutions") +
gtheme +
  geom_line(stat="smooth", method="lm", se = F, alpha = 0.6)+
  ggrepel::geom_text_repel(data = riskflags  %>%
                             filter(Countryname=="Sudan" | Countryname=="Turkey"),
                           aes(label = Countryname),
                           arrow = arrow(length = unit(0.01, 'npc')),
                           size = 5,
                           box.padding = 7)
#Add histograms
one <- ggMarginal(one, type = "histogram", fill="transparent")

three <- ggplot(riskflags, aes(EMERGING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID , EMERGING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID_AV, color = Continent)) +
  geom_count(alpha = 0.7) + 
  xlab("Max value") +
  ylab("Geometric mean") +
  ggtitle("c) Macroeconomic risk") +
  gtheme +
  geom_line(stat="smooth", method="lm", se = F, alpha = 0.6)+
  ggrepel::geom_text_repel(data = riskflags  %>%
                             sample_n(2),
                           aes(label = Countryname),
                           arrow = arrow(length = unit(0.01, 'npc')),
                           size = 5,
                           box.padding = 7)

three <- ggMarginal(three, type = "histogram", fill="transparent")

four <- ggplot(riskflags, aes(EMERGING_RISK_COVID_RESPONSE_CAPACITY , EMERGING_RISK_COVID_RESPONSE_CAPACITY_AV, color = Continent)) +
  geom_count(alpha = 0.7) + 
  xlab("Max value") +
  ylab("Geometric mean") +
  ggtitle("d) COVID response") +
  gtheme +
  geom_line(stat="smooth", method="lm", se = F, alpha = 0.6)+
  ggrepel::geom_text_repel(data = riskflags  %>%
                             filter(Countryname=="Italy" | Countryname=="Benin"),
                           aes(label = Countryname),
                           arrow = arrow(length = unit(0.01, 'npc')),
                           size = 5,
                           box.padding = 7)

four <- ggMarginal(four, type = "histogram", fill="transparent")

#Merge graphs and save
comp <- plot_grid(one, three, four, nrow=2)
ggsave("Plots/compareriskcalcs.pdf", comp, width = 14, height = 10)

#-------Ranking exercise--------------------
#Function to create top 20 ranked countries
names <- colnames(riskflags[3:17])
rankcountry <- lapply(riskflags[names], function(xx){
  paste(riskflags$Countryname[order(-xx)][1:30], round(xx[order(-xx)][1:30], 1))
})

#Combine list
rankcountry <- bind_rows(rankcountry)

#Draw table for existing risks
rankcountry %>%
  select(contains("EXISTING")) %>%
  kable %>%
  kable_styling() %>%
  save_kable(file = "Plots/top20countriesexisting.html", self_contained = T)

#Draw table for emerging risks
rankcountry %>%
  select(contains("EMERGING")) %>%
  kable %>%
  kable_styling() %>%
  save_kable(file = "Plots/top20countriesemerging.html", self_contained = T)

#---------------------Slope graph--------------------------------
MySpecial <- list(  
  # move the x axis labels up top
  scale_x_discrete(position = "top"),
  theme_bw(),
  # Format tweaks
  # Remove the legend
  theme(legend.position = "none"),
  # Remove the panel border
  theme(panel.border     = element_blank()),
  # Remove just about everything from the y axis
  theme(axis.title.y     = element_blank()),
  theme(axis.text.y      = element_blank()),
  theme(panel.grid.major.y = element_blank()),
  theme(panel.grid.minor.y = element_blank()),
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x     = element_blank()),
  theme(panel.grid.major.x = element_blank()),
  theme(axis.text.x.top      = element_text(size=12)),
  # Remove x & y tick marks
  theme(axis.ticks       = element_blank()),
  # Format title & subtitle
  theme(plot.title       = element_text(size=14, face = "bold", hjust = 0.5)),
  theme(plot.subtitle    = element_text(hjust = 0.5))
)

li <- riskflags[c(1, 9:14)]

colnames(li) <- c("Country","EM_C", "EM_FS","EM_MACRO","EM_FISC","EM_NH", "EM_FR")

countries <- c("Afghanistan", "Algeria", "Nigeria", "Ghana", "Serbia", "Tunisia")

li <- li %>%
  filter(Country %in% countries)

longli <- gather(li, key="risk", value = "new", -Country)
longli$new <- round(longli$new, 1)

slope <- ggplot(data = longli, aes(x = risk, y = new, group = Country)) +
  geom_line(aes(color = Country, alpha = 1), size = 1) +
  geom_text_repel(data = longli %>% filter(risk == "EM_C"), 
                  aes(label = Country) , 
                  hjust = "left", 
                  fontface = "bold", 
                  size = 4, 
                  nudge_x = -.45, 
                  direction = "y") +
  geom_text_repel(data = longli %>% filter(risk == "EM_NH"), 
                  aes(label = Country) , 
                  hjust = "right", 
                  fontface = "bold", 
                  size = 4, 
                  nudge_x = .5, 
                  direction = "y")  +
  geom_label(aes(label = new), 
             size = 3.5, 
             label.padding = unit(0.05, "lines"), 
             label.size = 0.0,
             alpha = 0.6) +
  MySpecial +
  scale_colour_brewer(palette = "Set1")

ggsave("Plots/slopeg.pdf", slope, width = 11.5, height = 6)

#----------------Different between risk flags---------------------------
#Calculate differences between scores
diffs <- riskflags %>%
  mutate(EMERGING_RISK_COVID_RESPONSE_CAPACITY_diff = EMERGING_RISK_COVID_RESPONSE_CAPACITY_SQ - EMERGING_RISK_COVID_RESPONSE_CAPACITY,
         EMERGING_RISK_FOOD_SECURITY_diff = EMERGING_RISK_FOOD_SECURITY_SQ - EMERGING_RISK_FOOD_SECURITY,
         EMERGING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID_diff  = EMERGING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID_SQ - EMERGING_RISK_MACROECONOMIC_EXPOSURE_TO_COVID,
         EMERGING_RISK_FISCAL_diff = EMERGING_RISK_FISCAL_SQ - EMERGING_RISK_FISCAL,
         EMERGING_RISK_FRAGILITY_INSTITUTIONS_diff = EMERGING_RISK_FRAGILITY_INSTITUTIONS_SQ - EMERGING_RISK_FRAGILITY_INSTITUTIONS)

diffs <- diffs %>% select(contains("diff")) %>% select(contains("EMERGING"))
colnames(diffs) <- c("EM_COV", "EM_FS", "EM_MACRO", "EM_FIS",  "EM_FRAG" )
                                              
#Draw plots
plots <- list()
nm <- colnames(diffs)
for (i in seq_along(nm)) {
  ggthemr("fresh")
  theme_set(theme_classic(base_size = 16))
  plots[[i]] <- print(ggplot(diffs, aes_string(x = nm[i])) +
                        geom_histogram(aes(y =..density..),
                                       colour="black", fill="white") +
                        ggtitle(print(nm[i])) +
                        xlab("Change in CR score (sqrt - max)") +
                        ylab("") +
                        geom_vline(aes(xintercept = mean(nm[i])), 
                                   linetype = "dashed", size = 0.6) +
                        geom_density(alpha = 0.2) +
                        theme(plot.title = element_text(hjust=0.5))) 
}

#Arrange the plots
n <- length(plots) 
nCol <- floor(sqrt(n))
up <- do.call("grid.arrange", c(plots, ncol=nCol))

ggsave("Plots/diffscores.pdf", up, height = 12, width = 10)

#----------------Table with biggest differences between risk flags as sqrt and max ----------------
#Function to create top 20 ranked countries
diffy <- cbind.data.frame(riskflags$Countryname, diffs) 
diffy <- rename(diffy, Countryname = `riskflags$Countryname`)
diffycol <- colnames(diffy[-1])

rankcountry <- lapply(diffy[diffycol], function(xx){
    test <- abs(xx)
    paste(diffy$Countryname[order(-test)][1:30], round(xx[order(-test)][1:30], 1))
})
  
#Combine list
rankcountry <- bind_rows(rankcountry)
  
#Draw table for existing risks
rankcountry %>%
    select(contains("EM")) %>%
    kable %>%
    kable_styling() %>%
    save_kable(file = "Plots/top20countriesdiffs.html", self_contained = T)
  
#----------------Compare max and sq risk------------------------------------
ggthemr_reset()
theme_set(theme_classic(base_size = 16))

comb <- riskflags %>%
  select(Countryname, TOTAL_EMERGING_COMPOUND_RISK_SCORE, TOTAL_EMERGING_COMPOUND_RISK_SCORE_SQ) %>%
  arrange(-TOTAL_EMERGING_COMPOUND_RISK_SCORE) %>%
  head(60)

#Pick labels to show subset of countries
a <- 1:30
tennum <- a[c(seq(1, length(a), 30), 30)]
countrylab <- as.character(comb$Countryname)
countrylab[-tennum] <- ""

#Plot
plotysq <- ggplot(comb, aes(x=TOTAL_EMERGING_COMPOUND_RISK_SCORE, xend=TOTAL_EMERGING_COMPOUND_RISK_SCORE_SQ, y=reorder(Countryname, TOTAL_EMERGING_COMPOUND_RISK_SCORE), group=Countryname)) + 
  geom_dumbbell(color="#a3c4dc", 
                size=0.75, 
                colour_xend = "darkred") + 
  scale_y_discrete(labels = countrylab) +
  ylab("Country") +
  xlab("Change in risk score") +
  theme(axis.ticks = element_blank(),
        axis.title = element_text(size = 20, hjust = 0.5),
        axis.text = element_text(size=16)) +
  geom_text_repel(data = comb %>% sample_n(15), 
                  aes(label = Countryname) , 
                  hjust = "left", 
                  fontface = "bold", 
                  color = "darkred",
                  segment.color = "black",
                  segment.alpha = 0.7,
                  segment.size = 0.3,
                  size = 5, 
                  nudge_x = -1, 
                  nudge_y = 1,
                  direction = "y")  +
  theme(axis.text.y = element_blank())

ggsave("Plots/changerisk.pdf", ploty, height = 10, width = 12)

#-----------------Regional population------------------------------------
gtheme <- theme(axis.ticks = element_blank(),
                axis.title = element_text(size = 20, hjust = 0.5),
                axis.text = element_text(size=16),
                legend.text = element_text(size=16),
                title = element_text(size=18, face = "bold" ))

#Identify population at risk and merge

pop <- wpp.by.year(wpp.indicator("tpop"), 2020)
pop$charcode <- suppressWarnings(countrycode(pop$charcode, origin = "iso2c", destination = 'iso3c'))
colnames(pop) <- c("Country", "Population")
risky <- left_join(riskflags, pop, by= "Country", keep=F)

# Find regional values
riskypop <- risky %>%
  mutate(Region =  countrycode(Country, origin = "iso3c", destination = 'region')) %>%
  group_by(Region, TOTAL_EMERGING_COMPOUND_RISK_SCORE) %>%
  summarise(Riskpop = sum(Population, na.rm=T)) %>%
  rename(Riskcat = TOTAL_EMERGING_COMPOUND_RISK_SCORE) %>%
  mutate(Riskcat = as.factor(Riskcat)) %>%
  filter(Riskcat != "0")

#Find regional proprotions
riskypoptot <- risky %>%
  mutate(Region =  countrycode(Country, origin = "iso3c", destination = 'region')) %>%
  group_by(Region) %>%
  summarise(Regionpop = sum(Population, na.rm =T )) 

riskypop <- left_join(riskypop, riskypoptot, by=c("Region")) %>%
  mutate(Riskprop = Riskpop / Regionpop)

#Plot and save
gtheme <- theme( axis.title = element_text(size = 20, hjust = 0.5),
                axis.text = element_text(size=16),
                legend.text = element_text(size=16),
                title = element_text(size=18, face = "bold" ))

riskpopg <- riskypop %>%
  filter(Riskcat != "0") %>%
  ggplot(aes(Region, Riskpop, fill=Riskcat)) + geom_histogram(position="stack", stat="identity") +
  coord_flip() +
  ylab("Total population at risk (000s)") +
  scale_fill_ordinal() + 
  gtheme + 
  labs(fill = "# Risk flags")

riskpopprop <- ggplot(riskypop, aes(Region, Riskprop, fill=Riskcat)) + geom_histogram(position="stack", stat="identity") +
  coord_flip() +
  ylab("Proportion of population at risk") +
  scale_fill_ordinal() + 
  gtheme + 
  labs(fill = "# Risk flags")

comb <- grid.arrange(riskpopg, riskpopprop, nrow = 2)

ggsave("Plots/popatrisk.pdf", comb, width = 12, height = 10)



  






  
  
  
