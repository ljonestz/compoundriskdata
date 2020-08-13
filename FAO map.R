#Load packages 
#install.packages("librarian")
librarian::shelf(ggplot2, cowplot, lubridate, rvest,dplyr, viridis, tidyverse, countrycode)

#-------------------------Draw a Map of Food Price Volatilty--------------
world <- map_data("world")
world <- world %>%
  dplyr::rename(Country = region) %>%
  dplyr::mutate(Country = countrycode(Country, origin = 'country.name', destination = 'iso3c'))

#Join datasets
worldmap <- inner_join(world, faoprice, by="Country")

#Plot world map
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

worldfood <- ggplot(data = worldmap, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Pc6m)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("Food price volatility (% Change in last 6 months)") +
  plain

worldfood

ggsave("worldfood.pdf", worldfood, width=10, height = 10)
