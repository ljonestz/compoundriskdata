######################################################################################################
#
#  CODE USED TO TO PRODUCE COMPARISON PLOTS FOR THE COMPOUND RISK MONITOR
#  (to be run after the global database has been generated)
#
######################################################################################################

#------------------------LOAD PACKAGES-------------------------
# install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(
  ggplot2, cowplot, lubridate, rvest, dplyr, viridis,
  countrycode, corrplot, cttobin / ggthemr, ggalt, gridExtra, ggcorrplot,
  ggExtra, ggrepel, knitr, kableExtra, grid, wppExplorer, alluvial, ggforce,
  ggalluvial, ggparallel, styler, mapview, geojsonio, scales, tidyverse, ggpubr
)

# Load themes
theme_set(theme_classic(base_size = 16))
ggthemr("fresh")

#------------------Global plots -------------------------
# Loading world database
world <- map_data("world")
world <- world %>%
  dplyr::rename(Country = region) %>%
  dplyr::mutate(Country = suppressWarnings(
    countrycode(
      Country,
      origin = "country.name",
      destination = "iso3c",
      nomatch = NULL
    )
  ))

# Change Greenland to Denmark scores
riskflags[nrow(riskflags) +1,] <- riskflags[which(riskflags$Countryname == "Denmark"),]
riskflags$Country <- as.character(riskflags$Country)
riskflags$Countryname <- as.character(riskflags$Countryname)
riskflags[which(riskflags$Countryname == "Denmark"),]$Countryname <- c("Denmark", "Greenland")
riskflags[which(riskflags$Country == "DNK"),]$Country <- c("DNK", "GRL")
riskflags$Country <- as.factor(riskflags$Country)
riskflags$Countryname <- as.factor(riskflags$Countryname)

# Join datasets with risk flags
worldmap <- inner_join(riskflags, world, by = "Country") 

# Map theme
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
  plot.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
  legend.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
  text = element_text(colour = "lightgrey")
)

#Colour scheme
# colour <- c("#FDFEFE","#F0F3F4", "#E5E7E9", "#BDC3C7", "#EC7063",  "#922B21", "#641E16") # For grey colours
#colour <- c("#FDFEFE","#F0F3F4",  "#BDC3C7", "#f5a864", "#EC7063",  "#922B21", "#641E16") 
colour <- c("#b3dfdf", "#66C1C1", "#3C89A1",  "#d95174","#B5284C","#761141", "#33071c") 

# Draw map one
map <- ggplot(data = worldmap, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.factor(floor(TOTAL_EXISTING_COMPOUND_RISK_SCORE_MED))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of Risks Flags") +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6))) 

# Draw map two
map2 <- ggplot(data = worldmap, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.factor(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE_MED))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of Risk Flags") +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6))) 

# Draw map two
map3 <- ggplot(data = worldmap, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.character(floor(OVERALL_FLAGS_GEO_MED))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of Risk Flags") +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6))) 

# Join all three maps
join_map <- ggarrange(map, map2, map3, ncol=1, nrow=3, common.legend = TRUE, legend="bottom")

# Save maps
ggsave("Plots/globalmapone.pdf", map, width = 10, height = 5)
ggsave("Plots/globalmaptwo.pdf", map2, width = 10, height = 5)
ggsave("Plots/globalmapthree.pdf", map3, width = 10, height = 5)

#----------------Map with more than three---------------------
# Draw map one
mapcomb <- ggplot(data = filter(worldmap %>% 
                              mutate(
                                TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM = case_when(TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM >= 4 ~ TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM,
                                                                                         TRUE ~ NA_real_))),
              mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM)) +
  scale_fill_distiller(palette = "Blues", direction = 1) + # or direction=1
  theme_map() +
  labs(fill = "Total # of risks flags") +
  theme(legend.position = "none")
              
# Draw map two
map2comb <- ggplot(data = filter(worldmap %>% 
                               mutate(
                                 TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM = case_when(TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM >= 3.5 ~ TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM,
                                                                                          TRUE ~ NA_real_))),
               mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM)) +
  scale_fill_distiller(palette = "Reds", direction = 1) + # or direction=1
  theme_map() +
  labs(fill = "Total # of risk flags") +
  theme(legend.position = "none")

# Save maps
ggsave("Plots/globalmap_hotspot.pdf", mapcomb, width = 8, height = 12)
ggsave("Plots/globalmaptwo_hotspot.pdf", map2comb, width = 8, height = 12)

maphigh <- ggplot(data = filter(worldmap %>% 
                                   mutate(
                                     TOTAL_EXISTING_COMPOUND_RISK_SCORE = case_when(TOTAL_EXISTING_COMPOUND_RISK_SCORE >= 3 ~ TOTAL_EXISTING_COMPOUND_RISK_SCORE,
                                                                                    TRUE ~ NA_real_))),
                   mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = TOTAL_EXISTING_COMPOUND_RISK_SCORE)) +
  scale_fill_distiller(palette = "Reds", direction = 1) + # or direction=1
  theme_map() +
  labs(fill = "Total # of risk flags") +
  theme(legend.position = "none")

map2high <- ggplot(data = filter(worldmap %>% 
                                   mutate(
                                     TOTAL_EMERGING_COMPOUND_RISK_SCORE = case_when(TOTAL_EMERGING_COMPOUND_RISK_SCORE >= 3 ~ TOTAL_EMERGING_COMPOUND_RISK_SCORE,
                                                                                    TRUE ~ NA_real_))),
                   mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = TOTAL_EMERGING_COMPOUND_RISK_SCORE)) +
  scale_fill_distiller(palette = "Reds", direction = 1) + # or direction=1
  theme_map() +
  labs(fill = "Total # of risk flags") +
  theme(legend.position = "none")

# Save maps
ggsave("Plots/globalmap_hotspot_high.pdf", maphigh, width = 8, height = 12)
ggsave("Plots/globalmaptwo_hotspot_high.pdf", map2high, width = 8, height = 12)


#--------------Correlations of all source indicators that feed into the compound risk monitor--------
# Subset data
vars <- globalrisk %>%
  dplyr::select(
     H_HIS_Score_norm ,  H_Oxrollback_score_norm ,  H_new_cases_smoothed_per_million_norm , 
     H_new_deaths_smoothed_per_million_norm ,  H_GovernmentResponseIndexForDisplay_norm , 
     H_EconomicSupportIndexForDisplay_norm ,  H_add_death_prec_current_norm , 
     H_INFORM_rating.Value_norm ,  F_Proteus_Score_norm ,  F_fews_crm_norm , 
     F_Artemis_Score_norm ,  D_WB_external_debt_distress_norm ,  D_IMF_debt2020.2019_norm , 
     D_fiscalgdpnum_norm ,  D_EconomicSupportIndexForDisplay_norm , 
     D_CESI_Index_norm ,  D_CPIA.scores_norm ,  Fr_FCS_Normalised , 
     Fr_REIGN_Normalised ,  Fr_Displaced_UNHCR_Normalised ,  Fr_BRD_Normalised , 
     M_Economic_and_Financial_score_norm ,  M_GDP_WB_2019minus2020_norm , 
     M_GDP_IMF_2019minus2020_norm ,  M_WB_gdp_20_21_norm ,  M_imf_gdp_diff_norm , 
     M_macrofin_risk_norm ,  M_EIU_risk_norm ,  M_cvi_risk_norm , 
     NH_GDAC_Hazard_Score_Norm ,  NH_Hazard_Score_norm ,  NH_multihazard_risk_norm , 
     NH_seasonal_risk_norm ,  NH_locust_norm ,  S_OCHA_Covid.vulnerability.index_norm , 
     S_INFORM_vul_norm ,  S_gdp_change.Rating_norm ,  S_unemployment.Rating_norm , 
     S_income_support.Rating_crm_norm ,  S_pov_prop_21_20_norm , 
     S_pov_prop_20_19_norm ,  S_pov_comb_norm ,  S_change_unemp_21_norm , 
     S_change_unemp_20_norm ,  S_change_unemp_norm ,  S_..of.respondents.who.have.stopped.working.since.COVID.19.outbreak_norm , 
     S_..able.to.access..staple.food.item..in.the.past.7.days.when.needed....any.staple.food_norm , 
     S_..of.HHs.used.money.saved.for.emergencies.to.cover.basic.living.expenses_norm , 
     S_..of.respondents.received.government.assistance.when.experiencing.labor.income.job.loss_norm , 
     S_In.the.last.30.days..you.skipped.a.meal.because.there.was.not.enough.money.or.other.resources.for.food...._norm , 
     S_phone_average_index_norm 
  )

colnames(vars) <- substring(colnames(vars), 1, 5)

# Correlations
corr <- round(cor(vars, na.rm = T, use = "pairwise.complete.obs"), 1)

# Pvalues
p.mat <- ggcorrplot::cor_pmat(vars, na.rm = T, use = "pairwise.complete.obs")

# Plots
plot <- ggcorrplot(
  corr,
  hc.order = FALSE,
  type = "lower",
  p.mat = p.mat,
  outline.col = "white",
  colors = c("#6D9EC1", "white", "#E46726")
) +
  theme(
    plot.margin = unit(c(1, 1, 1, -0.5), "cm"),
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.key.width = unit(3, "cm"),
    legend.title = element_blank()
  )

# Join and save
ggsave("Plots/corrplot.pdf", plot, width = 12, height = 10, units = "in")

#------------------------------Comparison between different overall risk scores--------------------
# Subset dataset
rankco <- riskflags %>%
  dplyr::select(Countryname, EMERGING_RISK_HEALTH_SQ, EMERGING_RISK_HEALTH)
rankco$sign <- ifelse(rankco$EMERGING_RISK_HEALTH_SQ - 
                        rankco$EMERGING_RISK_HEALTH > 0, "red", "green"
)

# Country labels
left_label <- paste(rankco$Countryname, round(rankco$EMERGING_RISK_HEALTH),
                    sep = ", "
)
right_label <- paste(rankco$Countryname, round(rankco$EMERGING_RISK_HEALTH_SQ), 
                     sep = ", "
)

# Find one from each integer
subset <- unlist(lapply(c(0, 3, 7, 9), function(xx) {
  which(round(rankco$EMERGING_RISK_HEALTH, 0) == xx)[1]
}))

left_label[-subset] <- " "
right_label[-subset] <- " "

theme_set(theme_classic())

# First plot
one <- ggplot(rankco) +
  geom_segment(aes(x = 1, xend = 2, y = `EMERGING_RISK_HEALTH`,  
                   yend = `EMERGING_RISK_HEALTH_SQ`, col = sign), 
               size = .75, 
               show.legend = F) +
  geom_vline(xintercept = 1, linetype = "dashed", size = .1) +
  geom_vline(xintercept = 2, linetype = "dashed", size = .1) +
  scale_color_manual(
    labels = c("Up", "Down"),
    values = c("green" = "#00ba38", "red" = "#f8766d")
  ) +
  labs(x = "", y = "") +
  xlim(.5, 2.5) +
  ylim(0, 10.3) +
  geom_text(label = left_label, y = rankco$EMERGING_RISK_HEALTH, x = rep(1, NROW(rankco)), hjust = 1.1, size = 7) +
  geom_text(label = right_label, y = rankco$EMERGING_RISK_HEALTH_SQ, x = rep(2, NROW(rankco)), hjust = -0.1, size = 7) +
  geom_text(label = "Max value", x = 0.8, y = 10.3, size = 7) +
  geom_text(label = "Geometric M", x = 2, y = 10.3, hjust = -0.1, size = 7) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    panel.border = element_blank(),
    line = element_blank(),
    axis.text = element_blank(),
    plot.margin = unit(c(1, -0.5, 1, 1), "cm"),
    title = element_text(size = 20, hjust = 0.5, face = "bold")
  ) +
  ggtitle("Health system risk")

# Save plot
ggsave("Plots/rankplot.pdf", one, width = 14, height = 10)

#--------------Correlation plots-----------------------------------
theme_set(theme_classic())

# Convert to continents
riskflags$Continent <- countrycode(riskflags$Country, origin = "iso3c", destination = "continent")

one <- ggplot(riskflags, aes(TOTAL_EXISTING_COMPOUND_RISK_SCORE, TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM, color = Continent)) +
  geom_count() +
  geom_line(stat = "smooth", method = "lm", se = F, alpha = 0.6) +
  xlab("Risk score with max value flags") +
  ylab("Risk score with max and medium flags") +
  ggtitle("Underlying Vulnerability") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    title = element_text(size = 16, hjust = 0.5, face = "bold")
  ) +
  ggrepel::geom_text_repel(
    data = as.data.frame(riskflags) %>%
      sample_n(3, replace = T),
    aes(label = Countryname),
    arrow = arrow(length = unit(0.01, "npc")),
    size = 5,
    box.padding = 3
  )

# Add histogram
one <- ggMarginal(one, type = "histogram", fill = "transparent")

two <- ggplot(riskflags, aes(TOTAL_EMERGING_COMPOUND_RISK_SCORE, TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM, color = Continent)) +
  geom_count() +
  geom_line(stat = "smooth", method = "lm", se = F, alpha = 0.6) +
  xlab("Risk score with max value flags") +
  ylab("Risk score with max and medium flags") +
  ggtitle("Emerging Threats") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    title = element_text(size = 16, hjust = 0.5, face = "bold")
  ) +
  ggrepel::geom_text_repel(
    data = as.data.frame(riskflags) %>%
      sample_n(3),
    aes(label = Countryname),
    arrow = arrow(length = unit(0.01, "npc")),
    size = 5,
    box.padding = 3
  )

two <- ggMarginal(two, type = "histogram", fill = "transparent")

join <- cowplot::plot_grid(one, two, ncol = 2)
ggsave("Plots/compareriskscore.pdf", join, width = 15, height = 6)

#--------------------Reliability test ------------------------------
# Find continents
cont <- riskflags %>%
  mutate(Continent = countrycode(Country, 
                                 origin = "iso3c",
                                 destination = "continent"))

one <- ggplot(cont, aes(TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM, RELIABILITY_SCORE_EMERGING_RISK, color = Continent)) +
  geom_point() +
  geom_line(
    stat = "smooth",
    method = "lm",
    se = F,
    alpha = 0.6
  ) +
  xlab("Emerging threat score") +
  ylab("") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    panel.grid = element_blank()
  )

# Add histogram
one <- ggMarginal(one, type = "histogram", fill = "transparent")

two <- ggplot(cont, aes(TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM, RELIABILITY_SCORE_EMERGING_RISK, color = Continent)) +
  geom_point() +
  geom_line(stat = "smooth", 
            method = "lm",
            se = F, 
            alpha = 0.6
  ) +
  xlab("Underlying Vulnerability score") +
  ylab("Reliability Score") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    panel.grid = element_blank()
  )

two <- ggMarginal(two, type = "histogram", fill = "transparent")

rel <- cowplot::plot_grid(two, one, align = "h")

ggsave("Plots/reliabilescore.pdf", rel, width = 12, height = 6)

#------------------Comparison between current and future risk----------------------------------
# Correlations of all source indicators that feed into the compound risk monitor
rvar <- riskflags %>%
  dplyr::select(
    EXISTING_RISK_HEALTH, EXISTING_RISK_FOOD_SECURITY,
    EXISTING_RISK_MACRO_FISCAL, EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY,
    EXISTING_RISK_NATURAL_HAZARDS, EXISTING_RISK_FRAGILITY_INSTITUTIONS,
    EMERGING_RISK_HEALTH, EMERGING_RISK_FOOD_SECURITY,EMERGING_RISK_MACRO_FISCAL,
    EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY, EMERGING_RISK_NATURAL_HAZARDS, 
    EMERGING_RISK_FRAGILITY_INSTITUTIONS
  )

colnames(rvar) <- c(
  "EX_Health", "EX_FoodS", "EX_Macro", "EX_Socio", "EX_Natural", "EX_Fragile",
  "EM_Health", "EM_FoodS", "EM_Macro","EM_Socio",  "EM_Natural", "EM_Fragile"
)

# Calculate correlations
rcorr <- round(cor(rvar, na.rm = T, use = "pairwise.complete.obs"), 1)
rp.mat <- cor_pmat(rvar, na.rm = T, use = "pairwise.complete.obs")

# Plot
plot <- ggcorrplot(
  rcorr,
  hc.order = FALSE,
  p.mat = rp.mat,
  outline.col = "white",
  colors = c("#6D9EC1", "white", "#E46726")
) +
  theme(
    plot.margin = unit(c(1, 1, 1, -0.5), "cm"),
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.key.width = unit(3, "cm"),
    legend.title = element_blank()
  )

ggsave("Plots/Riskcorr.pdf", plot, width = 10, height = 10)

#----------------Compare emerging and Underlying Vulnerability------------------------------------
ggthemr_reset()
theme_set(theme_classic(base_size = 16))

comb <- riskflags %>%
  dplyr::select(Countryname, TOTAL_EXISTING_COMPOUND_RISK_SCORE, TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM) %>%
  arrange(TOTAL_EXISTING_COMPOUND_RISK_SCORE)

# Pick labels to show subset of countries
a <- 1:190
tennum <- a[c(seq(1, length(a), 30), 190)]
countrylab <- as.character(comb$Countryname)
countrylab[-tennum] <- ""

# Plot
ploty <- ggplot(comb, aes(
  x = TOTAL_EXISTING_COMPOUND_RISK_SCORE, 
  xend = TOTAL_EXISTING_COMPOUND_RISK_SCORE_INCMEDIUM,
  y = reorder(Countryname, TOTAL_EXISTING_COMPOUND_RISK_SCORE), 
  group = Countryname
)) +
  geom_dumbbell(
    color = "#a3c4dc",
    size = 0.75,
    colour_xend = "darkred"
  ) +
  scale_y_discrete(labels = countrylab) +
  ylab("Country") +
  xlab("Change in risk score") +
  theme(
    axis.ticks = element_blank(),
    axis.title = element_text(size = 20, hjust = 0.5),
    axis.text = element_text(size = 16),
    axis.line = element_blank()
  ) +
  geom_text_repel(
    data = as.data.frame(comb) %>% sample_n(10),
    aes(label = Countryname),
    hjust = "left",
    fontface = "bold",
    color = "black",
    segment.color = "black",
    segment.alpha = 0.7,
    segment.size = 0.3,
    size = 6,
    nudge_x = 2,
    nudge_y = -1,
    direction = "y"
  ) +
  theme(axis.text.y = element_blank())

ggsave("Plots/changerisk.pdf", ploty, height = 10, width = 12)

#--------------Comparing max and geometric averages----------------------------------------
# Set theme
gtheme <- theme(
  axis.ticks = element_blank(),
  axis.title = element_text(size = 20, hjust = 0.5),
  axis.text = element_text(size = 16),
  legend.text = element_text(size = 16),
  title = element_text(size = 18, face = "bold")
)

# Plot each graph
one <- ggplot(riskflags, aes(EMERGING_RISK_FRAGILITY_INSTITUTIONS, EMERGING_RISK_FRAGILITY_INSTITUTIONS_AV, color = Continent)) +
  geom_count(alpha = 0.7) +
  xlab("Max value") +
  ylab("Geometric mean") +
  ggtitle("a) Fragility and Institutions") +
  gtheme +
  geom_line(stat = "smooth", method = "lm", se = F, alpha = 0.6) +
  ggrepel::geom_text_repel(
    data = riskflags %>%
      filter(Countryname == "Sudan" | Countryname == "Turkey"),
    aes(label = Countryname),
    arrow = arrow(length = unit(0.01, "npc")),
    size = 5,
    box.padding = 7
  )
# Add histograms
one <- ggMarginal(one, type = "histogram", fill = "transparent")

three <- ggplot(riskflags, aes(EMERGING_RISK_MACRO_FISCAL, EMERGING_RISK_MACRO_FISCAL_AV, color = Continent)) +
  geom_count(alpha = 0.7) +
  xlab("Max value") +
  ylab("Geometric mean") +
  ggtitle("c) Macroeconomic risk") +
  gtheme +
  geom_line(stat = "smooth", method = "lm", se = F, alpha = 0.6) +
  ggrepel::geom_text_repel(
    data = riskflags %>%
      sample_n(2),
    aes(label = Countryname),
    arrow = arrow(length = unit(0.01, "npc")),
    size = 5,
    box.padding = 7
  )

three <- ggMarginal(three, type = "histogram", fill = "transparent")

four <- ggplot(riskflags, aes(EMERGING_RISK_HEALTH, EMERGING_RISK_HEALTH_AV, color = Continent)) +
  geom_count(alpha = 0.7) +
  xlab("Max value") +
  ylab("Geometric mean") +
  ggtitle("d) COVID response") +
  gtheme +
  geom_line(stat = "smooth", method = "lm", se = F, alpha = 0.6) +
  ggrepel::geom_text_repel(
    data = riskflags %>%
      filter(Countryname == "Italy" | Countryname == "Benin"),
    aes(label = Countryname),
    arrow = arrow(length = unit(0.01, "npc")),
    size = 5,
    box.padding = 7
  )

four <- ggMarginal(four, type = "histogram", fill = "transparent")

# Merge graphs and save
comp <- plot_grid(one, three, four, nrow = 2)
ggsave("Plots/compareriskcalcs.pdf", comp, width = 14, height = 10)

#-------Ranking exercise--------------------
# Function to create top 20 ranked countries
rankcountry <- lapply(riskset %>% dplyr::select(contains(c("EXISTING_", "EMERGING_"))), function(xx) {
  paste(riskset$Country[order(-xx)][1:28], round(xx[order(-xx)][1:28], 1))
})

# Combine list
rankcountry <- bind_rows(rankcountry)

# Draw table for Underlying Vulnerabilitys
tab <- rankcountry %>%
  dplyr::select(contains("EXISTING"))
colnames(tab) <- gsub("EXISTING_RISK_", "", colnames(tab))
colnames(tab) <- gsub("EMERGING_RISK_", "", colnames(tab))
colnames(tab) <- c(
  "HEALTH", "FOOD", "MACRO", "SOCIO_VUL", "NATURAL", "CONFLICT",
  "TOTAL", "TOTAL(+MEDIUM)",
  "RELIABILITY"
)
tab %>%
  kable() %>%
  kable_styling(full_width = F, html_font = "Arial") %>%
  save_kable(file = "Plots/top20countriesexisting.html", self_contained = T)

# Draw table for Emerging threats
tab <- rankcountry %>%
  dplyr::select(contains("EMERGING"), -contains("_SQ"))
colnames(tab) <- gsub("EXISTING_RISK_", "", colnames(tab))
colnames(tab) <- gsub("EMERGING_RISK_", "", colnames(tab))
colnames(tab) <- c(
  "HEALTH", "FOOD", "MACRO", "SOCIO_VUL", "NATURAL", "CONFLICT",
  "TOTAL", "TOTAL(+MEDIUM)",
  "RELIABILITY"
)
tab %>%
  kable() %>%
  kable_styling(full_width = F, html_font = "Arial") %>%
  save_kable(file = "Plots/top20countriesemerging.html", self_contained = T)

# Draw table for geometric risks
sq <- c(
  "EMERGING_RISK_HEALTH_SQ", "EMERGING_RISK_FOOD_SECURITY_SQ",
  "EMERGING_RISK_MACRO_FISCAL_SQ", "EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY_SQ",
  "EMERGING_RISK_NATURAL_HAZARDS_SQ", "EMERGING_RISK_FRAGILITY_INSTITUTIONS_SQ",
  "OVERALL_FLAGS_GEO_MED"
)

# Function to create top 20 ranked countries
rankcountry <- lapply(alt %>% dplyr::select(sq), function(xx) {
  paste(alt$Country[order(-xx)][1:28], round(xx[order(-xx)][1:28], 1))
})

# Combine list
rankcountry <- bind_rows(rankcountry)

# relabel
tab <- rankcountry %>%
  dplyr::select(contains("EMERGING"))
colnames(tab) <- c(
  "HEALTH", "FOOD","SOCIO_VUL",  "MACRO",
   "NATURAL", "CONFLICT",
  "TOTAL"
)
tab %>%
  kable() %>%
  kable_styling(full_width = F, html_font = "Arial") %>%
  save_kable(file = "Plots/top20countriesesgeo.html", self_contained = T)

#---------------------Slope graph--------------------------------
MySpecial <- list(
  # move the x axis labels up top
  scale_x_discrete(position = "top"),
  theme_bw(),
  # Format tweaks
  # Remove the legend
  theme(legend.position = "none"),
  # Remove the panel border
  theme(panel.border = element_blank()),
  # Remove just about everything from the y axis
  theme(axis.title.y = element_blank()),
  theme(axis.text.y = element_blank()),
  theme(panel.grid.major.y = element_blank()),
  theme(panel.grid.minor.y = element_blank()),
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x = element_blank()),
  theme(panel.grid.major.x = element_blank()),
  theme(axis.text.x.top = element_text(size = 12)),
  # Remove x & y tick marks
  theme(axis.ticks = element_blank()),
  # Format title & subtitle
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)),
  theme(plot.subtitle = element_text(hjust = 0.5))
)

li <- riskflags %>% 
  dplyr::select(
    Countryname, contains("EMERGING") & contains("_SQ"), -contains("SQ_SQ"), 
    -contains("SQ_ALT"), -contains("MULTIDIMENSIONAL"), -contains("TOTAL")
  ) 

colnames(li) <- c("Country", "EM_C", "EM_FS", "EM_MACRO", "EM_SOCIO", "EM_FISC", "EM_NH", "EM_FR")

countries <- c("Afghanistan", "Algeria", "Nigeria", "Ghana", "Serbia", "Tunisia")

li <- li %>%
  filter(Country %in% countries)

longli <- gather(li, key = "risk", value = "new", -Country)
longli$new <- round(longli$new, 1)

slope <- ggplot(data = longli, aes(x = risk, y = new, group = Country)) +
  geom_line(aes(color = Country, alpha = 1), size = 1) +
  geom_text_repel(
    data = longli %>% filter(risk == "EM_C"),
    aes(label = Country),
    hjust = "left",
    fontface = "bold",
    size = 4,
    nudge_x = -.45,
    direction = "y"
  ) +
  geom_text_repel(
    data = longli %>% filter(risk == "EM_NH"),
    aes(label = Country),
    hjust = "right",
    fontface = "bold",
    size = 4,
    nudge_x = .5,
    direction = "y"
  ) +
  geom_label(aes(label = new),
             size = 3.5,
             label.padding = unit(0.05, "lines"),
             label.size = 0.0,
             alpha = 0.6
  ) +
  MySpecial +
  scale_colour_brewer(palette = "Set1")

ggsave("Plots/slopeg.pdf", slope, width = 11.5, height = 6)

#----------------Different between risk flags---------------------------
# Calculate differences between scores
diffs <- riskflags %>%
  mutate(
    EMERGING_RISK_HEALTH_diff = EMERGING_RISK_HEALTH_SQ - EMERGING_RISK_HEALTH,
    EMERGING_RISK_FOOD_SECURITY_diff = EMERGING_RISK_FOOD_SECURITY_SQ - EMERGING_RISK_FOOD_SECURITY,
    EMERGING_RISK_MACRO_FISCAL_diff = EMERGING_RISK_MACRO_FISCAL_SQ - EMERGING_RISK_MACRO_FISCAL,
    EMERGING_RISK_FRAGILITY_INSTITUTIONS_diff = EMERGING_RISK_FRAGILITY_INSTITUTIONS_SQ - EMERGING_RISK_FRAGILITY_INSTITUTIONS,
    EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY_diff = EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY_SQ - EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY,
  )

diffs <- diffs %>%
  dplyr::select(contains("diff")) %>%
  dplyr::select(contains("EMERGING"))
colnames(diffs) <- c("EM_COV", "EM_FS", "EM_MACRO", "EM_FRAG", "EM_SOCIO")

# Draw plots
plots <- list()
nm <- colnames(diffs)
for (i in seq_along(nm)) {
  ggthemr("fresh")
  theme_set(theme_classic(base_size = 16))
  plots[[i]] <- print(ggplot(diffs, aes_string(x = nm[i])) +
                        geom_histogram(aes(y = ..density..),
                                       colour = "black", fill = "white"
                        ) +
                        ggtitle(print(nm[i])) +
                        xlab("Change in CR score (geomean - max)") +
                        ylab("") +
                        geom_vline(aes(xintercept = mean(nm[i])),
                                   linetype = "dashed", size = 0.6
                        ) +
                        geom_density(alpha = 0.2) +
                        theme(plot.title = element_text(hjust = 0.5)))
}

# Arrange the plots
n <- length(plots)
nCol <- floor(sqrt(n))
up <- do.call("grid.arrange", c(plots, ncol = nCol))

ggsave("Plots/diffscores.pdf", up, height = 12, width = 10)

#----------------Table with biggest differences between risk flags as sqrt and max ----------------
# Function to create top 20 ranked countries
diffy <- cbind.data.frame(riskflags$Countryname, diffs)
diffy <- rename(diffy, Countryname = `riskflags$Countryname`)
diffycol <- colnames(diffy[-1])

rankcountry <- lapply(diffy[diffycol], function(xx) {
  test <- abs(xx)
  paste(diffy$Countryname[order(-test)][1:30], round(xx[order(-test)][1:30], 1))
})

# Combine list
rankcountry <- bind_rows(rankcountry)

# Draw table for Underlying Vulnerabilitys
rankcountry %>%
  dplyr::select(contains("EM")) %>%
  kable() %>%
  kable_styling() %>%
  save_kable(file = "Plots/top20countriesdiffs.html", self_contained = T)

#----------------Compare max and sq risk------------------------------------
ggthemr_reset()
theme_set(theme_classic(base_size = 16))

comb <- riskflags %>%
  dplyr::select(Countryname, TOTAL_EMERGING_COMPOUND_RISK_SCORE, OVERALL_FLAGS_GEO_MED) %>%
  arrange(-TOTAL_EMERGING_COMPOUND_RISK_SCORE) %>%
  head(60)

# Pick labels to show subset of countries
a <- 1:30
tennum <- a[c(seq(1, length(a), 30), 30)]
countrylab <- as.character(comb$Countryname)
countrylab[-tennum] <- ""

# Plot
plotysq <- ggplot(comb, aes(x = TOTAL_EMERGING_COMPOUND_RISK_SCORE,
                            xend = OVERALL_FLAGS_GEO_MED, 
                            y = reorder(Countryname, TOTAL_EMERGING_COMPOUND_RISK_SCORE,OVERALL_FLAGS_GEO_MED ), 
                            group = Countryname)) +
  geom_dumbbell(
    color = "#a3c4dc",
    size = 0.75,
    colour_xend = "darkred"
  ) +
  scale_y_discrete(labels = countrylab) +
  ylab("Country") +
  xlab("Change in risk score") +
  theme(
    axis.ticks = element_blank(),
    axis.title = element_text(size = 20, hjust = 0.5),
    axis.text = element_text(size = 16)
  ) +
  geom_text_repel(
    data = comb %>% sample_n(15),
    aes(label = Countryname),
    hjust = "left",
    fontface = "bold",
    color = "darkred",
    segment.color = "black",
    segment.alpha = 0.7,
    segment.size = 0.3,
    size = 5,
    nudge_x = -1,
    nudge_y = 1,
    direction = "y"
  ) +
  theme(axis.text.y = element_blank())

ggsave("Plots/changerisk.pdf", ploty, height = 10, width = 12)

#-----------------Regional population------------------------------------
gtheme <- theme(
  axis.ticks = element_blank(),
  axis.title = element_text(size = 20, hjust = 0.5),
  axis.text = element_text(size = 16),
  legend.text = element_text(size = 16),
  title = element_text(size = 18, face = "bold")
)

# Identify population at risk and merge
pop <- wpp.by.year(wpp.indicator("tpop"), 2020)
pop$charcode <- suppressWarnings(countrycode(pop$charcode, origin = "iso2c", destination = "iso3c"))
colnames(pop) <- c("Country", "Population")
risky <- left_join(riskflags, pop, by = "Country", keep = F)

# Find regional values
riskypop <- risky %>%
  mutate(Region = countrycode(Country, origin = "iso3c", destination = "region")) %>%
  group_by(Region, TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM) %>% 
  summarise(Riskpop = sum(Population, na.rm = T)) %>%
  rename(Riskcat = TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM) %>%
  # Remove floor if what to compare high risk only
  mutate(Riskcat = as.factor(floor(Riskcat))) %>%
  filter(Riskcat != "0")

# Find regional proprotions
riskypoptot <- risky %>%
  mutate(Region = countrycode(Country, origin = "iso3c", destination = "region")) %>%
  group_by(Region) %>%
  summarise(Regionpop = sum(Population, na.rm = T))

riskypop <- left_join(riskypop, riskypoptot, by = c("Region")) %>%
  mutate(Riskprop = Riskpop / Regionpop)

riskypop <-  riskypop %>%
  mutate(Riskcat = factor(Riskcat, levels = c("0", "1", "2", "3", "4", "5", "6"))) 

# Plot and save
gtheme <- theme(
  axis.title = element_text(size = 20, hjust = 0.5),
  axis.text = element_text(size = 16),
  legend.text = element_text(size = 16),
  title = element_text(size = 18, face = "bold")
)

# Number of people chart
riskpopg <- riskypop %>%
  filter(Riskcat != "0") %>%
  ggplot(aes(Region, Riskpop, fill = Riskcat)) +
  geom_histogram(position = "stack", stat = "identity") +
  coord_flip() +
  ylab("Total population at risk (000s)") +
  scale_fill_ordinal() +
  gtheme +
  labs(fill = "# Risk flags")+
  theme(legend.position = "none") + 
  ggtitle("a) Emerging Threats")

# Find regional values
riskypop <- risky %>%
  mutate(Region = countrycode(Country, origin = "iso3c", destination = "region")) %>%
  group_by(Region, TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM) %>% 
  summarise(Riskpop = sum(Population, na.rm = T)) %>%
  rename(Riskcat = TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM) %>%
  # Remove floor if what to compare high risk only
  mutate(Riskcat = as.factor(floor(Riskcat))) 

# Find regional proprotions
riskypoptot <- risky %>%
  mutate(Region = countrycode(Country, origin = "iso3c", destination = "region")) %>%
  group_by(Region) %>%
  summarise(Regionpop = sum(Population, na.rm = T))

riskypop <- left_join(riskypop, riskypoptot, by = c("Region")) %>%
  mutate(Riskprop = Riskpop / Regionpop)

riskypop <-  riskypop %>%
  mutate(Riskcat = factor(Riskcat, levels = c("0", "1", "2", "3", "4", "5", "6"))) 

# Draw proportions map
riskpopprop <- ggplot(riskypop, aes(Region, Riskprop, fill = Riskcat)) +
  geom_histogram(position = "stack", stat = "identity") +
  coord_flip() +
  ylab("Proportion of population at risk") +
  scale_fill_ordinal() +
  gtheme +
  labs(fill = "# Risk flags") +
  theme(legend.position = "none")
  

comb <- grid.arrange(riskpopg, riskpopprop, nrow = 2)

# Identify population at risk and merge
pop <- wpp.by.year(wpp.indicator("tpop"), 2020)
pop$charcode <- suppressWarnings(countrycode(pop$charcode, origin = "iso2c", destination = "iso3c"))
colnames(pop) <- c("Country", "Population")
risky <- left_join(riskflags, pop, by = "Country", keep = F)

# Find regional values
riskypop <- risky %>%
  mutate(Region = countrycode(Country, origin = "iso3c", destination = "region")) %>%
  group_by(Region, OVERALL_FLAGS_GEO_MED) %>% 
  summarise(Riskpop = sum(Population, na.rm = T)) %>%
  rename(Riskcat = OVERALL_FLAGS_GEO_MED) %>%
  # Remove floor if what to compare high risk only
  mutate(Riskcat = as.factor(floor(Riskcat))) %>%
  filter(Riskcat != "0")

# Find regional proprotions
riskypoptot <- risky %>%
  mutate(Region = countrycode(Country, origin = "iso3c", destination = "region")) %>%
  group_by(Region) %>%
  summarise(Regionpop = sum(Population, na.rm = T))

riskypop <- left_join(riskypop, riskypoptot, by = c("Region")) %>%
  mutate(Riskprop = Riskpop / Regionpop)

riskypop <-  riskypop %>%
  mutate(Riskcat = factor(Riskcat, levels = c("0", "1", "2", "3", "4", "5", "6"))) 

# Plot and save
gtheme <- theme(
  axis.title = element_text(size = 20, hjust = 0.5),
  axis.text = element_text(size = 16),
  legend.text = element_text(size = 16),
  title = element_text(size = 18, face = "bold")
)

# Number of people chart
riskpopg <- riskypop %>%
  filter(Riskcat != "0") %>%
  ggplot(aes(Region, Riskpop, fill = Riskcat)) +
  geom_histogram(position = "stack", stat = "identity") +
  coord_flip() +
  ylab("Total population at risk (000s)") +
  scale_fill_ordinal() +
  gtheme +
  labs(fill = "# Risk flags") +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank()) +
  ggtitle("b) Overall Alert Flags")

# Find regional values
riskypop <- risky %>%
  mutate(Region = countrycode(Country, origin = "iso3c", destination = "region")) %>%
  group_by(Region, OVERALL_FLAGS_GEO_MED) %>% 
  summarise(Riskpop = sum(Population, na.rm = T)) %>%
  rename(Riskcat = OVERALL_FLAGS_GEO_MED) %>%
  # Remove floor if what to compare high risk only
  mutate(Riskcat = as.factor(floor(Riskcat))) 

# Find regional proprotions
riskypoptot <- risky %>%
  mutate(Region = countrycode(Country, origin = "iso3c", destination = "region")) %>%
  group_by(Region) %>%
  summarise(Regionpop = sum(Population, na.rm = T))

riskypop <- left_join(riskypop, riskypoptot, by = c("Region")) %>%
  mutate(Riskprop = Riskpop / Regionpop)

riskypop <-  riskypop %>%
  mutate(Riskcat = factor(Riskcat, levels = c("0", "1", "2", "3", "4", "5", "6"))) 

# Draw proportions map
riskpopprop <- ggplot(riskypop, aes(Region, Riskprop, fill = Riskcat)) +
  geom_histogram(position = "stack", stat = "identity") +
  coord_flip() +
  ylab("Proportion of population at risk") +
  scale_fill_ordinal() +
  gtheme +
  labs(fill = "# Risk flags") +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank())

comb_overall <- grid.arrange(riskpopg, riskpopprop, nrow = 2)

# Join the two together
combined_at_risk <- cowplot::plot_grid(comb, comb_overall, ncol = 2, align = "vh", rel_widths = c(1.2, 1))

# Save plot
ggsave("Plots/popatrisk.pdf", 
       combined_at_risk, 
       width = 14, 
       height = 10,
       )

#-----------------Compare emerging and Underlying Vulnerability---------------------------
gtheme <- theme(
  axis.title = element_text(size = 20, hjust = 0.5),
  axis.text = element_text(size = 16),
  legend.text = element_text(size = 16),
  title = element_text(size = 18, face = "bold"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

# Find continents
complot_data <- riskflags %>%
  mutate(Continent = countrycode(Country, 
                     origin = "iso3c",
                     destination = "continent"))
# Plot
complot <- ggplot(complot_data, aes(TOTAL_EXISTING_COMPOUND_RISK_SCORE,
                                 TOTAL_EMERGING_COMPOUND_RISK_SCORE,
                                 color = Continent)) +
  geom_count() +
  geom_smooth(method = "lm", se = F) +
  gtheme +
  ylab("Emerging Threats") +
  xlab("Underlying Vulnerability") +
  ggrepel::geom_text_repel(
    data = as.data.frame(riskflags) %>%
      filter(TOTAL_EXISTING_COMPOUND_RISK_SCORE >= 3) %>%
      sample_n(2),
    aes(label = Countryname),
    arrow = arrow(length = unit(0.01, "npc")),
    size = 6,
    box.padding = 3,
    color = "Black"
  ) +
  geom_smooth(complot_data, aes(TOTAL_EXISTING_COMPOUND_RISK_SCORE,
                           TOTAL_EMERGING_COMPOUND_RISK_SCORE),
              method = "lm", se = F)

# Add histogram
complot <- ggMarginal(complot, type = "histogram", fill = "transparent")

ggsave("Plots/complot.pdf", complot, width = 8, height = 6)

#----------Coefficient of variation plot------------------------------------
gtheme <- theme(
  axis.title = element_text(size = 20, hjust = 0.5),
  axis.text = element_text(size = 16),
  legend.text = element_text(size = 16),
  title = element_text(size = 18, face = "bold")
)

one <- ggplot(riskflags, aes(EMERGING_RISK_HEALTH, H_coefvar)) +
  geom_point() +
  geom_smooth(method = "lm") +
  gtheme +
  ylab("Coef. of Variation") +
  xlab("Health")

three <- ggplot(riskflags, aes(EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY, S_coefvar)) +
  geom_point() +
  geom_smooth(method = "lm") +
  gtheme +
  ylab("Coef. of Variation") +
  xlab("S_Vul") +
  xlim(5, 10)

top <- cowplot::plot_grid(one, three, ncol = 2)

ggsave("Plots/covar.pdf", top, width = 8, height = 4)

#------Polar Diagram---------------
plot <- riskflags %>%
  dplyr::select(
    Countryname,
    EMERGING_RISK_HEALTH, EMERGING_RISK_FOOD_SECURITY,
    EMERGING_RISK_MACRO_FISCAL, EMERGING_RISK_NATURAL_HAZARDS, EMERGING_RISK_FRAGILITY_INSTITUTIONS,
    EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY
  )

colnames(plot) <- c("Countryname", "Health", "Food", "Macro", "Natural", "Fragile", "Socio")

plotone <- plot %>%
  gather(one, two, Health:Socio) %>%
  mutate(
    Risk_cat = as.factor(case_when(
      two == 10 ~ "high",
      two >= 7 & two < 10 ~ "med",
      TRUE ~ "low"
    )),
    Risk_cat = fct_relevel(Risk_cat, "high", "med", "low"),
    twona = case_when(
      two == 0 ~ NA_real_,
      TRUE ~ two
    )
  )

polar <- ggplot(plotone %>% filter(Countryname %in% c("Brazil", "Ethiopia", "Spain"))) +
  geom_col(aes(
    y = two,
    x = one,
    fill = Risk_cat,
    label = two
  ),
  width = 0.95,
  position = "identity"
  ) +
  coord_polar(start = pi) +
  facet_grid(cols = vars(Countryname)) +
  theme_gray() +
  theme(
    axis.text = element_text(size = 14),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = NA),
    panel.ontop = TRUE,
    axis.title = element_blank(),
    legend.text = element_text(size = 16),
    strip.text = element_text(size = 18, face = "bold"),
    strip.background = element_blank(),
    title = element_text(size = 18, face = "bold"),
    panel.grid.minor.x = element_line(colour = "white", size = 2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_line(colour = "white", size = 0.5),
    panel.grid.major.y = element_line(colour = "white", size = 0.5),
    legend.position = "bottom"
  ) +
  scale_y_sqrt() +
  scale_fill_manual(values = c("#f26c64", "#ffdd71", "#9fcd99")) +
  geom_text(aes(
    y = two,
    x = one,
    fill = Risk_cat,
    label = round(twona, 1)
  ),
  nudge_y = -0.01,
  nudge_x = 0.1,
  fontface = "bold",
  size = 5.5
  )

ggsave("Plots/polar.pdf", polar, width = 18, height = 9)

#----------Sankey Diagram---------
test <- risky %>%
  group_by(TOTAL_EXISTING_COMPOUND_RISK_SCORE, TOTAL_EMERGING_COMPOUND_RISK_SCORE) %>%
  dplyr::count(Population)

testing <- test %>%
  mutate(new = paste(TOTAL_EXISTING_COMPOUND_RISK_SCORE, TOTAL_EMERGING_COMPOUND_RISK_SCORE)) %>%
  group_by(new) %>%
  summarise(tt = sum(Population, na.rm = T)) %>%
  ungroup()

testing$Emerging_Threats <- str_sub(testing$new, 2, 3)
testing$Underlying_Vulnerability <- str_sub(testing$new, 0, 1)

dat <- testing %>%
  dplyr::select(Underlying_Vulnerability, Emerging_Threats, tt)

dat_ggforce <- dat %>%
  gather_set_data(1:2) %>% # <- ggforce helper function
  arrange(x, Underlying_Vulnerability, desc(Emerging_Threats)) %>%
  mutate(x = as.factor(x))

alpha <- 0.7

glob <- ggplot(dat_ggforce, aes(x = x, id = id, split = y, value = tt)) +
  geom_parallel_sets(aes(fill = Underlying_Vulnerability),
                     alpha = alpha, axis.width = 0.2,
                     n = 100, strength = 0.5
  ) +
  geom_parallel_sets_axes(
    axis.width = 0.25, fill = "gray95",
    color = "gray80", size = 0.15
  ) +
  geom_parallel_sets_labels(colour = "gray35", size = 4.5, angle = 0, fontface = "bold") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.margin = unit(c(0, -3, 0, -2), "cm")
  ) +
  scale_x_discrete(limits = rev(levels(dat_ggforce$x))) +
  ggtitle("Global")

sankyreg <- function(reg) {
  test <- risky %>%
    mutate(Continent = countrycode(Country, origin = "iso3c", destination = "continent")) %>%
    filter(Continent == reg) %>%
    group_by(TOTAL_EXISTING_COMPOUND_RISK_SCORE, TOTAL_EMERGING_COMPOUND_RISK_SCORE) %>%
    dplyr::count(Population)
  
  testing <- test %>%
    mutate(new = paste(TOTAL_EXISTING_COMPOUND_RISK_SCORE, TOTAL_EMERGING_COMPOUND_RISK_SCORE)) %>%
    group_by(new) %>%
    summarise(tt = sum(Population, na.rm = T)) %>%
    ungroup()
  
  testing$Emerging_Threats <- str_sub(testing$new, 2, 3)
  testing$Underlying_Vulnerability <- str_sub(testing$new, 0, 1)
  
  dat <- testing %>%
    dplyr::select(Underlying_Vulnerability, Emerging_Threats, tt)
  
  dat_ggforce <- dat %>%
    gather_set_data(1:2) %>% # <- ggforce helper function
    arrange(x, Underlying_Vulnerability, desc(Emerging_Threats)) %>%
    mutate(x = as.factor(x))
  
  ggplot(dat_ggforce, aes(x = x, id = id, split = y, value = tt)) +
    geom_parallel_sets(aes(fill = Underlying_Vulnerability),
                       alpha = alpha, axis.width = 0.2,
                       n = 100, strength = 0.5
    ) +
    geom_parallel_sets_axes(
      axis.width = 0.25, fill = "gray95",
      color = "gray80", size = 0.15
    ) +
    geom_parallel_sets_labels(colour = "gray35", size = 4.5, angle = 0, fontface = "bold") +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.title.x = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    scale_x_discrete(limits = rev(levels(dat_ggforce$x))) +
    ggtitle(paste(reg))
}

afr <- sankyreg("Africa") 
asia <- sankyreg("Asia")  
eur <- sankyreg("Europe")  
oce <- sankyreg("Oceania")  
ame <- sankyreg("Americas") 

one <- cowplot::plot_grid(afr, asia, ncol = 1) + theme(plot.margin = unit(c(0, -5, 0, -3), "cm"))
two <- cowplot::plot_grid(ame, oce, ncol = 1)

sank <- cowplot::plot_grid(one, glob, nrow = 1, rel_widths = c(1, 2))

ggsave("Plots/sankey.pdf", sank, width = 16, height = 8)

#----------------- Dynamic hazard plots-----------------------
#tt <- geojson_sf("https://www.gdacs.org/contentdata/xml/gdacs.geojson")
tt <- geojson_sf("https://www.gdacs.org/contentdata/xml/gdacsTC.geojson")
#Label different shape types
tt$polygontype <- gsub("\\_.*","",tt$polygontype)

#ggplot
ggplot() + 
  coord_fixed(1.3) +
  geom_polygon(data = worldmap, aes(x = long, y = lat, group = group)) + 
  geom_sf(data = tt, aes(fill = polygontype))

#Leaflet
mapview(tt, zcol = tt$polygontype)

#-------------------Regional Maps-----------------------------------------
# Map theme
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
  plot.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
  legend.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
  text = element_text(colour = "lightgrey"),
  legend.position = "left"
)

#------------------Maps with med and high rounded down to the nearest integer---------------------------
# colour <- c("#FDFEFE","#F0F3F4", "#E5E7E9", "#BDC3C7", "#EC7063",  "#922B21", "#641E16") # For grey colours
# colour <- c("#FDFEFE","#F0F3F4",  "#BDC3C7", "#f5a864", "#EC7063",  "#922B21", "#641E16") # For grey red
colour <- c("#b3dfdf", "#66C1C1", "#3C89A1",  "#d95174","#B5284C","#761141", "#33071c") 

# Convert countries
worldmap$Region <- countrycode(worldmap$Country, origin = "iso3c", destination = "region")

# Generate maps
globalmap <- worldmap  %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.factor(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6)))

seamap <- worldmap %>%
  filter(Region == "South Asia") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.factor(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6)))

menamap <- worldmap %>%
  filter(Region == "Middle East & North Africa") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.character(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6)))

ssamap <- worldmap %>%
  filter(Region == "Sub-Saharan Africa") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.character(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6)))

lacmap <- worldmap %>%
  filter(Region == "Latin America & Caribbean") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.character(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6)))

eapmap <- worldmap %>%
  filter(Region == "East Asia & Pacific") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.character(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6))) +
  xlim(70,200)

ecamap <- worldmap %>%
  filter(Region == "Europe & Central Asia") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.character(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6)))

ggsave("Plots/Snapshots/Globalmap_high+med.jpeg", globalmap, width = 8, height = 5)
ggsave("Plots/Snapshots/Seamap_high+med.jpeg", seamap, width = 8, height = 5)
ggsave("Plots/Snapshots/Menamap_high+med.jpeg", menamap, width = 8, height = 5)
ggsave("Plots/Snapshots/Ssaamap_high+med.jpeg", ssamap, width = 8, height = 5)
ggsave("Plots/Snapshots/LACamap_high+med.jpeg", lacmap, width = 8, height = 5)
ggsave("Plots/Snapshots/ECAmap_high+med.jpeg", ecamap, width = 14, height = 10)
ggsave("Plots/Snapshots/EAPmap_high+med.jpeg", eapmap,  width = 8, height = 5)

#------------------Overall compound risk score with med and high rounded down to the nearest integer---------------------------
# colour <- c("#FDFEFE","#F0F3F4", "#E5E7E9", "#BDC3C7", "#EC7063",  "#922B21", "#641E16") # For grey colours
#colour <- c("#FDFEFE","#F0F3F4",  "#BDC3C7", "#f5a864", "#EC7063",  "#922B21", "#641E16") 
colour <- c("#b3dfdf", "#66C1C1", "#3C89A1",  "#d95174","#B5284C","#761141", "#33071c") 

# Plot maps
globalmap <- worldmap  %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.factor(floor(OVERALL_FLAGS_GEO_MED))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6)))

seamap <- worldmap %>%
  filter(Region == "South Asia") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.factor(floor(OVERALL_FLAGS_GEO_MED))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6)))

menamap <- worldmap %>%
  filter(Region == "Middle East & North Africa") %>%
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.character(floor(OVERALL_FLAGS_GEO_MED))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6)))

ssamap <- worldmap %>%
  filter(Region == "Sub-Saharan Africa") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.character(floor(OVERALL_FLAGS_GEO_MED))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6)))

lacmap <- worldmap %>%
  filter(Region == "Latin America & Caribbean") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.character(floor(OVERALL_FLAGS_GEO_MED))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6)))

eapmap <- worldmap %>%
  filter(Region == "East Asia & Pacific") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.character(floor(OVERALL_FLAGS_GEO_MED))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6))) +
  xlim(70,200)

ecamap <- worldmap %>%
  filter(Region == "Europe & Central Asia") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.character(floor(OVERALL_FLAGS_GEO_MED))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6)))

ggsave("Plots/Snapshots/Globalmap_overall_risk.jpeg", globalmap, width = 8, height = 5)
ggsave("Plots/Snapshots/Seamap_overall_risk.jpeg", seamap, width = 8, height = 5)
ggsave("Plots/Snapshots/Menamap_overall_risk.jpeg", menamap, width = 8, height = 5)
ggsave("Plots/Snapshots/Ssaamap_overall_risk.jpeg", ssamap, width = 8, height = 5)
ggsave("Plots/Snapshots/LACamap_overall_risk.jpeg", lacmap, width = 8, height = 5)
ggsave("Plots/Snapshots/ECAmap_overall_risk.jpeg", ecamap, width = 14, height = 10)
ggsave("Plots/Snapshots/EAPmap_overall_risk.jpeg", eapmap,  width = 8, height = 5)

#------------------Maps with high risks as a full spectrum---------------------------
globalmap <- worldmap  %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.factor(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6)))

seamap <- worldmap %>%
  filter(Region == "South Asia") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.factor(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6)))

eapmap <- worldmap %>%
  filter(Region == "East Asia & Pacific") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.factor(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6))) +
  xlim(70,200)

ecamap <- worldmap %>%
  filter(Region == "Europe & Central Asia") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.factor(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6)))

menamap <- worldmap %>%
  filter(Region == "Middle East & North Africa") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.character(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6)))

ssamap <- worldmap %>%
  filter(Region == "Sub-Saharan Africa") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.character(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6)))

lacmap <- worldmap %>%
  filter(Region == "Latin America & Caribbean") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.character(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colour,  limits = as.character(c(0:6)))

ggsave("Plots/Snapshots/Globalmap.pdf", globalmap, width = 8, height = 5)
ggsave("Plots/Snapshots/Seamap.pdf", seamap, width = 8, height = 5)
ggsave("Plots/Snapshots/Menamap.pdf", menamap, width = 8, height = 5)
ggsave("Plots/Snapshots/Ssaamap.pdf", ssamap, width = 8, height = 5)
ggsave("Plots/Snapshots/LACamap.pdf", lacmap, width = 8, height = 5)
ggsave("Plots/Snapshots/EAPmap.pdf", eapmap, width = 8, height = 5)
ggsave("Plots/Snapshots/ECAmap.pdf", ecamap, width = 14, height = 10)

#------------------Maps with med_high as a full threshold---------------------------
colour_full <- c("#FDFEFE", "#F7F9F9", "#F8F9F9", "#F0F3F4", "#E5E7E9", "#CACFD2", "#BDC3C7",
            "#B4B3B3", "#EC7063",  "#CB4335",  "#B03A2E", "#78281F", "#3b0101") 

globalmap <- worldmap %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.factor(TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM))) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5))  +
  scale_fill_manual(values = colour_full,
                    labels = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6),
                    limits = as.factor(c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6)))

seamap <- worldmap  %>%
  filter(Region == "South Asia") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.factor(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5))  +
  scale_fill_manual(values = colour_full,
                    labels = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6),
                    limits = as.factor(c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6)))

menamap <- worldmap  %>%
  filter(Region == "Middle East & North Africa") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.character(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5))  +
  scale_fill_manual(values = colour_full,
                    labels = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6),
                    limits = as.factor(c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6)))

ssamap <- worldmap  %>%
  filter(Region == "Sub-Saharan Africa") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.character(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5))  +
  scale_fill_manual(values = colour_full,
                    labels = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6),
                    limits = as.factor(c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6)))

lacmap <- worldmap  %>%
  filter(Region == "Latin America & Caribbean") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.character(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5))   +
  scale_fill_manual(values = colour_full,
                    labels = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6),
                    limits = as.factor(c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6)))

eapmap <- worldmap  %>%
  filter(Region == "East Asia & Pacific") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.character(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM))), colour = "black", size = 0.1) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5))   +
  scale_fill_manual(values = colour_full,
                    labels = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6),
                    limits = as.factor(c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6))) +
  xlim(70,200)

ecamap <- worldmap  %>%
  filter(Region == "Europe & Central Asia") %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.character(floor(TOTAL_EMERGING_COMPOUND_RISK_SCORE_INCMEDIUM)))) +
  theme_map() +
  labs(fill = "# of risks") +
  theme(plot.title = element_text(hjust = 0.5))   +
  scale_fill_manual(values = colour_full,
                    labels = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6),
                    limits = as.factor(c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6)))

ggsave("Plots/Snapshots/Globalmap_high+med_full.pdf", globalmap, width = 8, height = 5)
ggsave("Plots/Snapshots/Seamap_high+med_full.pdf", seamap, width = 8, height = 5)
ggsave("Plots/Snapshots/Menamap_high+med_full.pdf", menamap, width = 8, height = 5)
ggsave("Plots/Snapshots/Ssaamap_high+med_full.pdf", ssamap, width = 8, height = 5)
ggsave("Plots/Snapshots/LACamap_high+med_full.pdf", lacmap, width = 8, height = 5)
ggsave("Plots/Snapshots/EAPmap_high+med_full.pdf", eapmap, width = 8, height = 5)
ggsave("Plots/Snapshots/ECAmap_high+med_full.pdf", ecamap, width = 14, height = 10)

#To use asymetric colour gradient use:
#scale_fill_gradientn(
#  colours = c("white", "grey", "#D0553D", "#4D0909"),
#  values = rescale(c(0,4,6)),
#  guide = "colorbar",
#  limits=c(0,6))


#-----------------------------Quadrant plot-------------------------------------------

# Function to create geometric mean plots
quadrant_plot_geo <- function(xname, yname) {
  
  # Names to label
  one <- as.data.frame(riskflags) %>%
    dplyr::select(Countryname, !!xname, !!yname) %>%
    filter( !!xname > 7 & !!yname > 7) %>%
    sample_n(1)
  two <- as.data.frame(riskflags) %>%
    dplyr::select(Countryname, !!xname, !!yname) %>%
    filter( !!xname < 7 & !!yname > 7) %>%
    sample_n(1)
  three <- as.data.frame(riskflags) %>%
    dplyr::select(Countryname, !!xname, !!yname) %>%
    filter( !!xname > 7 & !!yname < 7) %>%
    sample_n(1)
  nams <- full_join(countrylist %>% dplyr::select(-Country), one) %>%
    full_join(., two) %>%
    full_join(., three) %>%
    dplyr::select(-X) 
  
  nams[2] <- as.numeric(unlist(tidyr::replace_na(as.list(nams[2], ""))))
  nams[3] <- as.numeric(unlist(tidyr::replace_na(as.list(nams[3], ""))))
  
  # Graph
  plot <- ggplot() +
    theme_minimal() +
    labs(x = expression('Vulnerability (high score = high vul)' %->% "") ,
         y = expression("Emerging threats (high score = severe threat)" %->% "")) +
    theme(panel.border = element_rect(colour = "white",
                                      fill= NA, 
                                      size= 0.5),
          axis.ticks.x=element_blank(), 
          axis.text.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title = element_text(hjust = 0, 
                                    vjust= 4, 
                                    colour= "black",
                                    size= 10,
                                    face= "bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")  
  
  # To add Quadrant use this
  # plot <- plot + annotate("rect", xmin = 7, xmax = 10, ymin = 7, ymax = 10, fill= "red", alpha = 0.2)  +  annotate("rect", xmin = 0, xmax = 7, ymin = 0, ymax = 7 , fill= "darkgreen", alpha = 0.2) + annotate("rect", xmin = 7, xmax = 10, ymin = 0, ymax = 7, fill= "orange", alpha = 0.2) + annotate("rect", xmin = 0, xmax = 7, ymin = 7, ymax = 10, fill= "orange", alpha = 0.2) +
  
    # To use arithmetic/geometric mean use this
    #Draw contour lines
    cc <- emdbook::curve3d(
      geometricmean(c(x,y)),  # To use of geometric mean process
      #mean(c(x,y)),  # To change to a mean process
      xlim=c(0,10), 
      ylim=c(0,10), 
      sys3d="none"
    )
  
  dimnames(cc$z) <- list(cc$x,cc$y)
  mm <- reshape2::melt(cc$z)
  
  #Draw contour lines on plot
  plot <- plot + 
    geom_contour_filled(
      data=mm,
      aes(x=Var1,y=Var2,z=value),
      breaks = c(0,5,7,10),
      colour="white",
      alpha = 0.3,
      show.legend = F
    ) +
    scale_fill_manual(
      values = c("#9fcd99", 
                 "#ffdd71",
                 "#f26c64")
    )
  
  # Add other country data to plot
  plot <- plot +
    geom_count(data = riskset, 
               aes(x =  !!xname,
                   y = !!yname),
               alpha = 0.3,
               colour = "black") +
    ylim(0,10) +
    xlim(0,10)
  
  # Add country labels
  plot <- plot +
    ggrepel::geom_text_repel(
      data = as.data.frame(nams),
      aes(x = unlist(nams[2]),
          y = unlist(nams[3]),
          label = unlist(nams[1])),
      size = 4,
      box.padding = 3,
      fontface= "bold"
    ) 
  
  plot
  
}

# Create plot for arithmetic mean
quadrant_plot_arithmetic <- function(xname, yname) {
  
  # Names to label
  one <- as.data.frame(riskflags) %>%
    dplyr::select(Countryname, !!xname, !!yname) %>%
    filter(!!xname > 7 & !!yname > 7) %>%
    sample_n(1)
  two <- as.data.frame(riskflags) %>%
    dplyr::select(Countryname, !!xname, !!yname) %>%
    filter(!!xname < 7 & !!yname > 7) %>%
    sample_n(1)
  three <- as.data.frame(riskflags) %>%
    dplyr::select(Countryname, !!xname, !!yname) %>%
    filter(!!xname > 7 & !!yname < 7) %>%
    sample_n(1)
  nams <- full_join(countrylist %>% dplyr::select(-Country), one) %>%
    full_join(., two) %>%
    full_join(., three) %>%
    dplyr::select(-X) 
  
  nams[2] <- as.numeric(unlist(tidyr::replace_na(as.list(nams[2], ""))))
  nams[3] <- as.numeric(unlist(tidyr::replace_na(as.list(nams[3], ""))))
  
  # Graph
  plot <- ggplot() +
    theme_minimal() +
    labs(x = expression('Vulnerability (high score = high vul)' %->% "") ,
         y = expression("Emerging threats (high score = severe threat)" %->% "")) +
    theme(panel.border = element_rect(colour = "white",
                                      fill= NA, 
                                      size= 0.5),
          axis.ticks.x=element_blank(), 
          axis.text.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title = element_text(hjust = 0, 
                                    vjust= 4, 
                                    colour= "black",
                                    size= 10,
                                    face= "bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")  
  
  # To add Quadrant use this
  # plot <- plot + annotate("rect", xmin = 7, xmax = 10, ymin = 7, ymax = 10, fill= "red", alpha = 0.2)  +  annotate("rect", xmin = 0, xmax = 7, ymin = 0, ymax = 7 , fill= "darkgreen", alpha = 0.2) + annotate("rect", xmin = 7, xmax = 10, ymin = 0, ymax = 7, fill= "orange", alpha = 0.2) + annotate("rect", xmin = 0, xmax = 7, ymin = 7, ymax = 10, fill= "orange", alpha = 0.2) +
  
  # To use arithmetic/geometric mean use this
  #Draw contour lines
  cc <- emdbook::curve3d(
    #geometricmean(c(x,y)),  # To use of geometric mean process
    mean(c(x,y)),  # To change to a mean process
    xlim=c(0,10), 
    ylim=c(0,10), 
    sys3d="none"
  )
  
  dimnames(cc$z) <- list(cc$x,cc$y)
  mm <- reshape2::melt(cc$z)
  
  #Draw contour lines on plot
  plot <- plot + 
    geom_contour_filled(
      data=mm,
      aes(x=Var1,y=Var2,z=value),
      breaks = c(0,5,7,10),
      colour="white",
      alpha = 0.3,
      show.legend = F
    ) +
    scale_fill_manual(
      values = c("#9fcd99", 
                 "#ffdd71",
                 "#f26c64")
    )
  
 # Change NAs in 0s
  riskset_na <- riskset %>%
    mutate(
      !!xname := case_when(is.na(!!xname) & !is.na(!!yname) ~ 0, TRUE ~ as.numeric(!!xname)),
      !!yname := case_when(is.na(!!yname) & !is.na(!!xname) ~ 0, TRUE ~ as.numeric(!!yname))
    )
  
   # Add other country data to plot
  plot <- plot +
    geom_count(data = riskset_na, 
               aes(x = !!xname,
                   y = !!yname),
               alpha = 0.3,
               colour = "black") +
    ylim(0,10) +
    xlim(0,10)
  
  # Add country labels
  plot <- plot +
    ggrepel::geom_text_repel(
      data = as.data.frame(nams),
      aes(x = unlist(nams[2]),
          y = unlist(nams[3]),
          label = unlist(nams[1])),
      size = 4,
      box.padding = 3,
      fontface= "bold"
    ) 
  
  plot
  
}

# Draw plots
covid <- quadrant_plot_geo(
  quo(EXISTING_RISK_HEALTH), 
  quo(EMERGING_RISK_HEALTH)
) +
  ggtitle("a) HEALTH") +
  theme(plot.title = element_text(colour= "black",
                                  size= 16,
                                  face= "bold"))

food <- quadrant_plot_geo(
  quo(EXISTING_RISK_FOOD_SECURITY), 
  quo(EMERGING_RISK_FOOD_SECURITY)
) +
  ggtitle("b) FOOD SECURITY") +
  labs(x = "", y = "") +
  theme(plot.title = element_text(colour= "black",
                                  size= 16,
                                  face= "bold"))

socio <- quadrant_plot_geo(
  quo(EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY),
  quo(EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY)
) +
  ggtitle("c) SOCIO_VUL") +
  theme(plot.title = element_text(colour= "black",
                                  size= 16,
                                  face= "bold"))

macro <- quadrant_plot_geo(
  quo(EXISTING_RISK_MACRO_FISCAL), 
  quo(EMERGING_RISK_MACRO_FISCAL)
) +
  labs(x = "", y = "") +
  ggtitle("d) MACRO") +
  theme(plot.title = element_text(colour= "black",
                                  size= 16,
                                  face= "bold"))

conflict <- quadrant_plot_arithmetic(
  quo(EXISTING_RISK_FRAGILITY_INSTITUTIONS),
  quo(EMERGING_RISK_FRAGILITY_INSTITUTIONS)
) +
  ggtitle("e) CONFLICT") +
  theme(plot.title = element_text(colour= "black",
                                  size= 16,
                                  face= "bold"))

natural <- quadrant_plot_geo(
  quo(EXISTING_RISK_NATURAL_HAZARDS), 
  quo(EMERGING_RISK_NATURAL_HAZARDS)
) +
  labs(x = "", y = "") +
  ggtitle("f) NAT_HAZ") +
  theme(plot.title = element_text(colour= "black",
                                  size= 16,
                                  face= "bold"))

# Arrange together
ploty <- cowplot::plot_grid(covid, food, socio, macro, conflict, natural, ncol = 2, nrow = 3)

ggsave("Plots/quad_plot.pdf", ploty, height = 12, width = 8)

#------------------------------Highlight country-----------------------------------  

quad_plot <- function(country_plot){
  
  country_name <- country_plot
  
  quadrant_plot <- function(xg, yg, xname, yname, countrynam) {
    
    # Names to label
    nams <- as.data.frame(riskflags) %>%
      filter(Countryname == countrynam)
    # To use arithmetic/geometric mean use this
    #Draw contour lines
    cc <- emdbook::curve3d(
      geometricmean(c(x,y)),  # To use of geometric mean process
      #mean(c(x,y)),  # To change to a mean process
      xlim=c(0,10), 
      ylim=c(0,10), 
      sys3d="none"
    )
    
    dimnames(cc$z) <- list(cc$x,cc$y)
    mm <- reshape2::melt(cc$z)
    
    #Draw contour lines on plot
    plot <- ggplot() +
      geom_contour_filled(
      data=mm,
      aes(x=Var1,y=Var2,z=value),
      breaks = c(0,5,7,10),
      colour="white",
      alpha = 0.3,
      show.legend = F
    ) +
      scale_fill_manual(
        values = c("#9fcd99", 
                   "#ffdd71",
                   "#f26c64")
      )
    
    # Graph
    plot <- plot +
      theme_minimal() +
      labs(x = expression('Underlying vulnerability (high score = high vul)' %->% "") ,
           y = expression("Emerging threats (high score = severe threat)" %->% "")) +
      theme(panel.border = element_rect(colour = "white",
                                        fill= NA, 
                                        size= 0.5),
            axis.ticks.x=element_blank(), 
            axis.text.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title = element_text(hjust = 0, 
                                      vjust= 4, 
                                      colour= "black",
                                      size= 10,
                                      face= "bold"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none") +
      geom_count(data = riskset, 
                 aes(x = xg,
                     y = yg),
                 alpha = 0.1,
                 colour = "black") +
      ylim(0,10) +
      xlim(0,10)
    
    # Need to include if statement for countries where there is an NA in either Emerging or Existing
    if ( !is.na(nams[xname]) & !is.na(nams[yname])) {
      
      plot <- plot  +
        geom_count(aes(x = as.numeric(nams[xname]),
                       y = as.numeric(nams[yname]))) +
        ggrepel::geom_text_repel(
          data = as.data.frame(nams),
          aes(x = as.numeric(nams[xname]),
              y = as.numeric(nams[yname]),
              label = countrynam),
          size = 4,
          box.padding = 3,
          fontface= "bold"
        ) 
    }
    
    plot
    
  }
  
  quadrant_plot_geo <- function(xg, yg, xname, yname, countrynam) {
    
    # Names to label
    nams <- as.data.frame(riskflags) %>%
      filter(Countryname == countrynam)
    
    # To use arithmetic/geometric mean use this
    #Draw contour lines
    cc <- emdbook::curve3d(
      #geometricmean(c(x,y)),  # To use of geometric mean process
      mean(c(x,y)),  # To change to a mean process
      xlim=c(0,10), 
      ylim=c(0,10), 
      sys3d="none"
    )
    
    dimnames(cc$z) <- list(cc$x,cc$y)
    mm <- reshape2::melt(cc$z)
    
    #Draw contour lines on plot
    plot <- ggplot() +
      geom_contour_filled(
      data=mm,
      aes(x=Var1,y=Var2,z=value),
      breaks = c(0,5,7,10),
      colour="white",
      alpha = 0.3,
      show.legend = F
    ) +
      scale_fill_manual(
        values = c("#9fcd99", 
                   "#ffdd71",
                   "#f26c64")
      )
    
    # Graph
    plot <- plot +
      theme_minimal() +
      labs(x = expression('Underlying vulnerability (high score = high vul)' %->% "") ,
           y = expression("Emerging threats (high score = severe threat)" %->% "")) +
      theme(panel.border = element_rect(colour = "white",
                                        fill= NA, 
                                        size= 0.5),
            axis.ticks.x=element_blank(), 
            axis.text.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title = element_text(hjust = 0, 
                                      vjust= 4, 
                                      colour= "black",
                                      size= 10,
                                      face= "bold"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none") +
      geom_count(data = riskset, 
                 aes(x = xg,
                     y = yg),
                 alpha = 0.1,
                 colour = "black") +
      ylim(0,10) +
      xlim(0,10)
    
    # Need to include if statement for countries where there is an NA in either Emerging or Existing
    if ( !is.na(nams[xname]) & !is.na(nams[yname])) {
      
      plot <- plot  +
        geom_count(aes(x = as.numeric(nams[xname]),
                       y = as.numeric(nams[yname]))) +
        ggrepel::geom_text_repel(
          data = as.data.frame(nams),
          aes(x = as.numeric(nams[xname]),
              y = as.numeric(nams[yname]),
              label = countrynam),
          size = 4,
          box.padding = 3,
          fontface= "bold"
        ) 
    }
    
    plot
    
  }
  
  # Draw plots
  covid <- quadrant_plot(riskset$EXISTING_RISK_HEALTH,
                         riskset$EMERGING_RISK_HEALTH, 
                         "EXISTING_RISK_HEALTH", 
                         "EMERGING_RISK_HEALTH",
                         country_name) +
    ggtitle("a) HEALTH") +
    theme(plot.title = element_text(colour= "black",
                                    size= 16,
                                    face= "bold"))
  
  food <- quadrant_plot(riskset$EXISTING_RISK_FOOD_SECURITY,
                        riskset$EMERGING_RISK_FOOD_SECURITY, 
                        "EXISTING_RISK_FOOD_SECURITY", 
                        "EMERGING_RISK_FOOD_SECURITY",
                        country_name) +
    ggtitle("b) FOOD SECURITY") +
    labs(x = "", y = "") +
    theme(plot.title = element_text(colour= "black",
                                    size= 16,
                                    face= "bold"))
  
  socio <- quadrant_plot(riskset$EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY,
                         riskset$EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY,
                         "EXISTING_RISK_SOCIOECONOMIC_VULNERABILITY",
                         'EMERGING_RISK_SOCIOECONOMIC_VULNERABILITY',
                         country_name) +
    ggtitle("c) SOCIO_VUL") +
    theme(plot.title = element_text(colour= "black",
                                    size= 16,
                                    face= "bold"))
  
  macro <- quadrant_plot(riskset$EXISTING_RISK_MACRO_FISCAL, 
                         riskset$EMERGING_RISK_MACRO_FISCAL,
                         "EXISTING_RISK_MACRO_FISCAL", 
                         "EMERGING_RISK_MACRO_FISCAL",
                         country_name) +
    labs(x = "", y = "") +
    ggtitle("d) MACRO") +
    theme(plot.title = element_text(colour= "black",
                                    size= 16,
                                    face= "bold"))
  
  conflict <- quadrant_plot_geo(riskset$EXISTING_RISK_FRAGILITY_INSTITUTIONS,
                            riskset$EMERGING_RISK_FRAGILITY_INSTITUTIONS,
                            "EXISTING_RISK_FRAGILITY_INSTITUTIONS",
                            "EMERGING_RISK_FRAGILITY_INSTITUTIONS",
                            country_name) +
    ggtitle("e) CONFLICT") +
    theme(plot.title = element_text(colour= "black",
                                    size= 16,
                                    face= "bold"))
  
  natural <- quadrant_plot(riskset$EXISTING_RISK_NATURAL_HAZARDS, 
                           riskset$EMERGING_RISK_NATURAL_HAZARDS,
                           "EXISTING_RISK_NATURAL_HAZARDS", 
                           "EMERGING_RISK_NATURAL_HAZARDS",
                           country_name) +
    labs(x = "", y = "") +
    ggtitle("f) NAT_HAZ") +
    theme(plot.title = element_text(colour= "black",
                                    size= 16,
                                    face= "bold"))
  
  # Arrange together
  ploty_country <- cowplot::plot_grid(covid, food, socio, macro, conflict, natural, ncol = 2, nrow = 3)
  
  ggsave(paste0("Plots/Heatmaps/quad_plot_", country_name, ".pdf"), ploty_country, height = 10, width = 8)
  
}

# Run function with all the countries that work in the countrlist database and save graph
get_data2 <- possibly(quad_plot, otherwise = NA)

for(i in countrylist$Countryname) {
  res <- get_data2(i)
}

