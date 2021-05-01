######################################################################################################
#
#  CODE USED TO TO PRODUCE COUNTRY-LEVEL SEASONAL FORECASTS 
#
######################################################################################################

# install.packages("librarian")     #Run if librarian is not already installed
librarian::shelf(ggalt, raster, sf, mapview, maptools, ggthemes, tidyverse)

# Load seasonal forecast image
t <- raster("Plots/IRI_forecast_march.tiff")

# Edit values by colour band (values can be accessed via attr(t[[1]], "colors"))
values(t)[values(t == 0)] <- NA
values(t)[values(t >=1 & t <=43)] <- -5
values(t)[values(t >= 44 & t <=55)] <- -4
values(t)[values(t >= 56 & t <=68)] <- -3
values(t)[values(t >= 69 & t<=74)] <- -2
values(t)[values(t >= 75 & t<=80)] <- -1
values(t)[values(t >= 81 & t<=126)] <- 0
values(t)[values(t >= 127 & t<=129)] <- 0
values(t)[values(t >= 130 & t<=175)] <- 0
values(t)[values(t >= 176 & t<=181)] <- 1
values(t)[values(t >= 182 & t<=187)] <- 2
values(t)[values(t >= 188 & t<=201)] <- 3
values(t)[values(t >= 201 & t<=212)] <- 4
values(t)[values(t >= 213 & t<=254)] <- 5
values(t)[values(t >= 254)] <- NA

# Create dataframe
tt <- as.data.frame(t, xy= T)

# Load world map data
data("wrld_simpl")
wrld_simpl_sf <- sf::st_as_sf(wrld_simpl) 

# Plot seasonal forecast over world map
seasonal_plot <- ggplot(tt) +
  geom_raster(aes(x = x, y = y, fill = as.factor(IRI_forecast_march))) +
  coord_cartesian(ylim = c(-50, 90)) +
coord_quickmap() +
  geom_sf(data = wrld_simpl_sf, 
          fill = NA,
          colour = "black", 
          size = 0.2) + 
  coord_sf(ylim = c(-50, 90)) +
  scale_fill_brewer(name = "",
                    type = "div",
                    labels = c("70+ Above", "60 Above", "50 Above", "45 Above", "40 Above", "Normal",
                               "40 Below", "45 Below", "50 Below", "60 Below", "70+ Below",
                               "No data")) +
  theme(legend.position = "below") +
  theme_map() +
  ylab("") +
  xlab("") +
  ggtitle("Seasonal forecast")

# Print seasonal map
ggsave("Plots/Seasonal/seasonal_forecast.pdf", seasonal_plot, width = 10, height = 6.5)

# Function to calculate cells above/below range (60 likelihood)
ex <- raster::extract(t, wrld_simpl, 
              fun=function(x,...)(sum(na.omit(x) >= 3) / length(x)), 
              na.rm= TRUE, 
              df= T,
              weights = F)

xe <- raster::extract(t, wrld_simpl,
              fun=function(x,...)(sum(na.omit(x) <= -3) / length(x)), 
              na.rm= TRUE, 
              df= T, 
              weights = F)

# Create and join the coverage datasets
fortyhigh <- cbind.data.frame(wrld_simpl_sf$ISO3, ex)
fortylow <- cbind.data.frame(wrld_simpl_sf$ISO3, xe)
join <- left_join(fortyhigh,
                  fortylow, 
                  by = "wrld_simpl_sf$ISO3")

# Calculate area coverage either wet or dry
ex_cov <- join %>% 
  mutate(Forecast = IRI_forecast_march.x + IRI_forecast_march.y) 

# Bind max coverage to world sf
binding <- cbind(wrld_simpl_sf, ex_cov$Forecast)  

# Plot
seasonal_risk <- binding %>%
  mutate(risklevel = case_when(ex_cov.Forecast < 0.2 ~ 1,
                               ex_cov.Forecast >= 0.2 & ex_cov.Forecast < 0.8 ~ 2,
                               ex_cov.Forecast >= 0.8 ~ 3,
                               TRUE ~ NA_real_),
         risklevel = as.factor(risklevel)) %>%
  ggplot() + 
  geom_sf(aes(fill = risklevel),
          size = 0.2,
          color = "black") + 
  coord_sf(ylim = c(-50, 90))  +
  scale_fill_manual(name = "",
                    values = c("#E5E7E9","#EC7063", "#943126"),
                    labels = c("Low", "Medium", "High")) +
  theme_map() +
  ylab("") +
  xlab("") 

ggsave("Plots/Seasonal/seasonal_risk.pdf", seasonal_risk, width = 10, height = 6.5)

#------------------------ Generate population estimates---------------------
tr <- raster("Plots/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_2pt5_min.tif")
t_points = rasterToPoints(tr)
t_df = data.frame(t_points)

# Change legend
pop_data <- t_df %>%
  rename(value = gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_2pt5_min) %>%
  mutate(value_cat = case_when(
    value <= 25 ~ 1,
    value > 25 & value <= 125 ~ 2,
    value > 125 & value <= 625 ~ 3,
    value > 625 & value <= 6250 ~ 4,
    value > 6250 & value <= 25000 ~ 5,
    value >= 25000 ~ 6,
    TRUE ~ NA_real_
  ))
    
# Map population estimates
pop <- ggplot(pop_data) +
  geom_raster(aes(x = x,
                  y = y,
                  fill = value_cat)) +
  theme(legend.position = "none")

# Save plot
ggsave("Plots/Seasonal/Population_map.pdf", pop, width = 10, height = 6.5)

#---------------------- Stack rasters----------------
# Load population raster
#tr <- raster("Plots/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_2pt5_min.tif")

# Make the population data the same resolution as climate
tr_reshape <- resample(tr, t, method = 'bilinear')

# Change values in population raster
values(tr_reshape)[values(tr_reshape <= 25)] <- 1
values(tr_reshape)[values(tr_reshape > 25 & tr_reshape <= 125)] <- 2
values(tr_reshape)[values(tr_reshape > 125 & tr_reshape <= 625)] <- 3
values(tr_reshape)[values(tr_reshape > 625 & tr_reshape <= 6250)] <- 4
values(tr_reshape)[values(tr_reshape > 6250 & tr_reshape<= 25000)] <- 5
values(tr_reshape)[values(tr_reshape >= 25000)] <- 6

# Collect crop and livestock land data (from http://www.earthstat.org/)
crop_tif <- raster("Plots/Cropland2000_5m.tif")
livestock_tif <- raster("Plots/Pasture2000_5m.tif")

# Get same resolution as IRI data
crop_reshape <- projectRaster(crop_tif, t, method = 'bilinear')
livestock_reshape <- projectRaster(livestock_tif, t, method = 'bilinear')

# Stack
land_stack <- stack(crop_reshape, livestock_reshape)

# Function to add crop and pasture
land_calc <- raster::overlay(
  land_stack,
  fun = function(x, y) {
    return(ifelse(x >= 0.3 | y >= 0.3, 1, 0))
  }
)

# Collect persistence in rainfall data
persist_raw <- raster("Plots/Seasonal/persist.tiff")

# Merge resolution with forecast (0 = NA, 2 = dry greatly, 3 = dry enhanced, 4 = wet enhanced, 6 = wet greatly)
persist_shape <- resample(persist_raw, t, method = 'ngb')

# Change persist map to align with the values in the IRI forecast
values(persist_shape)[values(persist_shape == 2)] <- -3
values(persist_shape)[values(persist_shape == 3)] <- -4
values(persist_shape)[values(persist_shape == 4)] <- 3
values(persist_shape)[values(persist_shape == 6)] <- 4

# Create a full stack combining pop, land, persistance and rain
iri_stack <- stack(persist_shape, t, tr_reshape, land_calc)

# Function to compare forecast with population density
join_raster <- raster::overlay(
  iri_stack,
  fun = function(w, x, y, z) {
    return(ifelse((z == 1 | y >= 3) & (x >= 3 | x <= -3), x,
                  ifelse((w == -3 | w == -4) & x >= -3, w, 
                         ifelse((z == 1 | y >= 3) & (x < 3 & x > -3), 0, NA)
                  )
    ))
  }
)

ex <- raster::extract(join_raster, wrld_simpl, 
                      fun = function(x,...) (sum(na.omit(x) >= 3) / length(na.omit(x == 0))), 
                      na.rm = TRUE, 
                      df = T,
                      weights = F)

xe <- raster::extract(join_raster, wrld_simpl,
                      fun = function(x,...)(sum(na.omit(x) <= -3) / length(na.omit(x == 0))), 
                      na.rm = TRUE, 
                      df = T, 
                      weights = F)

na <- raster::extract(join_raster, wrld_simpl,
                      fun = function(x,...)(sum(is.na(x)) / length(x)), 
                      na.rm = TRUE, 
                      df = T, 
                      weights = F)

na_num <- raster::extract(join_raster, wrld_simpl,
                      fun = function(x,...)(sum(na.omit(x))), 
                      na.rm = TRUE, 
                      df = T, 
                      weights = F)

# Create and join the coverage datasets
iri_map <- cbind(wrld_simpl_sf, ex$layer)
iri_map <- cbind(iri_map, xe$layer)
iri_map <- cbind(iri_map, na$layer)
iri_map <- cbind(iri_map, na_num$layer)

# Calculate area coverage either wet or dry
iri_map <- iri_map %>% 
  mutate(Forecast = ex$layer + xe$layer) 

# Draw global map
seasonal_map <- iri_map %>% 
  mutate(
    Forecast = case_when(na$layer >= 0.8 & na_num$layer >= -30 ~ NA_real_,
                         TRUE ~ Forecast),
    risklevel = case_when(Forecast < 0.2 ~ 1,
                          Forecast >= 0.2 & Forecast < 0.8 ~ 2,
                          Forecast >= 0.8 ~ 3,
                          TRUE ~ NA_real_),
    risklevel = as.factor(risklevel)
  ) %>%
  ggplot() + 
  geom_sf(aes(fill = risklevel),
          size = 0.2,
          color = "black") + 
  coord_sf(ylim = c(-50, 90))  +
  scale_fill_manual(name = "",
                    values = c("#E5E7E9","#EC7063", "#943126"),
                    labels = c("Low", "Medium", "High")) +
  theme_map() +
  ylab("") +
  xlab("") +
  ggtitle("Risk categories")

ggsave("Plots/Seasonal/Risk_categories.pdf", seasonal_plot, width = 10, height = 6.5)

#-------------------Plot forecast as included pixels---------------------------------
forecast_plot <- as.data.frame(join_raster, xy = T)

for_plot <- ggplot(forecast_plot) +
  geom_raster(aes(x = x, y = y, fill = as.factor(layer))) +
  coord_cartesian(ylim = c(-50, 90)) +
  coord_quickmap() +
  geom_sf(data = wrld_simpl_sf, 
          fill = NA,
          colour = "black", 
          size = 0.2) + 
  coord_sf(ylim = c(-50, 90)) +
  theme(legend.position = "below") +
  theme_map() +
  ylab("") +
  xlab("") +
  ggtitle("Forecast for included pixels") 

ggsave("Plots/Seasonal/Forecast_pixels.pdf", for_plot, width = 10, height = 6.5)

#------------------------- Plot map of crop/livestock + 25 ppl------------------------------
forecast_plot <- as.data.frame(land_calc, xy = T)

# create dataframe of global population
ppl_plot <- as.data.frame(tr_reshape, xy = T)
ppl_plot <- ppl_plot %>%
  mutate(
    pop_dat = case_when(
    gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_2pt5_min >= 3 ~ 2,
                   TRUE ~ NA_real_
    ))

# Join population and crop/livestock dataframes
data <- full_join(forecast_plot, ppl_plot, by = c("x", "y"))
data <- data %>%
  mutate(
    layer = case_when(
      layer == 0 ~ NA_real_,
      TRUE ~ layer
    ),
    pop_land = case_when(
      layer == 1 & is.na(pop_dat) ~ "crop/livestock",
      is.na(layer) & pop_dat == 2 ~ "25_ppl+",
      layer == 1 & pop_dat == 2 ~ "crop/livestock_and_25_ppl+",
      TRUE ~ NA_character_
    ))

# Plot map
glob_plot <- ggplot(data) +
  geom_raster(aes(x = x, y = y, fill = pop_land)) +
  coord_cartesian(ylim = c(-50, 90)) +
  coord_quickmap() +
  geom_sf(data = wrld_simpl_sf, 
          fill = NA,
          colour = "black", 
          size = 0.2) + 
  coord_sf(ylim = c(-50, 90)) +
  theme(legend.position = "below") +
  theme_map() +
  ylab("") +
  xlab("") +
  ggtitle("Pixels for land/pop inclusion") 

ggsave("Plots/Seasonal/Global_plot.pdf", glob_plot, width = 10, height = 6.5)


