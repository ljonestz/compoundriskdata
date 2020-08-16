#--------------------LOAD PACKAGES--------------
#install.packages("librarian")
librarian::shelf(ggplot2, cowplot, lubridate, rvest,dplyr, viridis, tidyverse, countrycode)

#--------------------------DRAW MAPS--------------------------------------
world <- map_data("world")
world <- world %>%
  dplyr::rename(Country = region) %>%
  dplyr::mutate(Country = countrycode(Country, origin = 'country.name', destination = 'iso3c'))

#Join datasets
worldmap <- full_join(world, covidproj, by="Country")

#ggplot theme
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill = "lightgrey", colour = "lightgrey"),
  plot.background = element_rect(fill = "lightgrey", colour = "lightgrey") , 
  legend.background = element_rect(fill = "lightgrey", colour = "lightgrey"),
  text = element_text(colour = "black")
)

#Plot different maps
one <- ggplot(data = worldmap, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = log(`Current Deaths`+1))) +
  scale_fill_viridis(direction=-1, option="D") + # or direction=1
  ggtitle("COVID current deaths (log)") +
  plain +
  labs(fill = "Deaths (log)")

three <- ggplot(data = worldmap, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = `Additional Deaths (% of Current Deaths)`)) +
  scale_fill_viridis(direction=-1, option="D") + # or direction=1
  ggtitle("Additional COVID Deaths by Nov 2020 (as % of Current Deaths)") +
  plain +
  labs(fill = "Additional Deaths (%)")

two <- ggplot(data = worldmap, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = `Projected Deaths / 1M`)) +
  scale_fill_viridis(direction=-1, option="D") + # or direction=1
  ggtitle("COVID Projected Deaths by Nov 2020 (per 1M)") +
  plain

#Display all maps together
joinmap <- cowplot::plot_grid(one,two,three, align= "v", ncol=1)
joinmap

#Save map
ggsave("covidmap.pdf", joinmap, width=12, height = 10, units="in")