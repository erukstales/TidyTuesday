### Tidy Tuesday 2021 Week 15: Global Deforestation
### Created by: Emily Rukstales
### Created on: 2021-04-06
################################################################################


### Load Libraries #############################################################

library(tidyverse)
library(here)
library(plotly)
library(maps)



### Load Data ##################################################################

forest <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest.csv')



### Data Wrangling #############################################################

world <- map_data("world") #world map lats and longs

forest  <- forest %>%
  rename(region = entity) %>% #call entity region to match world map data
  mutate(region = recode(region, #rename several regions to match world map data
                         "Congo" = "Republic of Congo",
                         "French Guyana" = "French Guiana",
                         "United Kingdom" = "UK",
                         "United States" ="USA")) %>%
  filter(region != "Gibraltar" & region != "Tokelau" & region != "World") #filter out some regions not included in world map data

world_forest <- inner_join(world, forest, by = "region") #join the 2 datasets by region



### Data Viz ###################################################################

world_forest %>%
  filter(year == "1990") %>% #make map of 1990 data
  ggplot() +
  geom_polygon(data = world, #first make regular world map (to show countries w/out data as grey in background)
               aes(x = long,
                   y = lat,
                   group = group),
               color = "black",
               fill = "#cccccc") +
  geom_polygon(aes(x = long, #make map on top of grey map showing deforestation
                   y = lat,
                   group = group,
                   fill = net_forest_conversion),
               color = "black") +
  coord_fixed(1.3) + 
  scale_fill_continuous(low = "#1e55eb", high = "#eb1e96", #use pink/blue gradient
                        name = "Net forest\nconversion (hectares)", #title legend
                        label = scales::comma, #don't use scientiic notation on legend
                        limits = c(-4000000, 2400000)) + #set standard legend scale
  theme_minimal() +
  theme(panel.grid = element_blank(), #remove gridlines
        axis.text = element_blank(), #remove axis text
        axis.ticks = element_blank()) + #remove tick marks
  labs(x = " ", #no lat/long axis labels
       y = " ")


min(world_forest[,9]) #get min and max values for standardizing legend scales
max(world_forest[,9])












