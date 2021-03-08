### Tidy Tuesday 2021 Week 9: Superbowl Commercials
### Created by: Emily Rukstales
### Created on: 2021-03-05
################################################################################



### Load Libraries #############################################################

library(tidyverse)
library(here)
library(tidytuesdayR)
library(waffle)
library(cowplot)


### Load Data ##################################################################

tuesdata <- tt_load('2021-03-02') #load this week's data

youtube <- tuesdata$youtube %>% #store youtube data as object
  mutate(brand = recode(brand, "Hynudai" = "Hyundai")) #noticed that Hyundai was spelled wrong
view(youtube)



### Data wrangling #############################################################

#for each brand across all years, calculate percent of ads with each theme and multiply by 100
youtube_percents <- youtube %>%
  group_by(brand) %>%
  summarise(sum_funny = sum(funny), #calculate sums of ads by brand
            sum_product = sum(show_product_quickly),
            sum_patriotic = sum(patriotic),
            sum_celebrity = sum(celebrity),
            sum_animals = sum(animals),
            sum_sex = sum(use_sex),
            sum_danger = sum(danger))
#probably could have made this all one pipe...
youtube_percents <- youtube_percents %>%
  mutate(total = rowSums(youtube_percents[2:8])) %>% #create a column with the total across each row (total ads per brand)
  mutate(percent_funny = 100*(sum_funny/total), #calculate percent of each theme for each brand and multiply by 100
         percent_product = 100*(sum_product/total),
         percent_patriotic = 100*(sum_patriotic/total),
         percent_celebrity = 100*(sum_celebrity/total),
         percent_animals = 100*(sum_animals/total),
         percent_sex = 100*(sum_sex/total),
         percent_danger = 100*(sum_danger/total)) %>%
  select(brand, 10:16) #remove sum columns
 
#adjust some of the numbers by small amounts so that when percents are rounded they add up to 100 (idk if there is a better way to do this??)
youtube_percents[2, 8] = 6.501481
youtube_percents[3, 4] = 9.533962
youtube_percents[5, 3] = 22.49065
youtube_percents[6, 2] = 24.50980
youtube_percents[6, 3] = 24.50980
youtube_percents[7, 8] = 13.493514
youtube_percents[9, 8] = 8.51
youtube_percents[10, 5] = 7.5

#round each theme to whole number so that brand total = 100
youtube_rounded <- youtube_percents %>%
  mutate(rounded_funny = round(percent_funny, digits = 0), #round each percent to whole number
         rounded_product = round(percent_product, digits = 0),
         rounded_patriotic = round(percent_patriotic, digits = 0),
         rounded_celebrity = round(percent_celebrity, digits = 0),
         rounded_animals = round(percent_animals, digits = 0),
         rounded_sex = round(percent_sex, digits = 0),
         rounded_danger = round(percent_danger, digits = 0)) %>%
  select(brand, 9:15) #remove unrounded columns

youtube_rounded <- youtube_rounded[, c(1, 2, 3, 8, 7, 6, 5, 4)] #reorder theme columns

#pivot dataframe so that there is a column for each brand and one column for themes
youtube_rounded <- youtube_rounded %>%
  pivot_longer(cols = 2:8,
               names_to = "theme",
               values_to = "percent") %>%
  pivot_wider(names_from = brand,
              values_from = percent) %>%
  rename("BudLight" = "Bud Light",
         "CocaCola" = "Coca-Cola",
         "ETrade" = "E-Trade")

colSums(youtube_rounded[2:11]) #making sure columns add up to exactly 100



### Data Viz ###################################################################

#store fill specifications as an object
colScale <- scale_fill_manual(values = c("rounded_funny" = "#FF61C3", #choose colors for each theme
                              "rounded_product" = "#DB72FB",
                              "rounded_danger" = "#619CFF",
                              "rounded_sex" = "#00B9E3",
                              "rounded_animals" = "#00C19F",
                              "rounded_celebrity" = "#D39200",
                              "rounded_patriotic" = "#F8766D"),
                              breaks = c("rounded_funny", "rounded_product", "rounded_danger", "rounded_sex", "rounded_animals", "rounded_celebrity", "rounded_patriotic"), #reorder the themes as they will appear in legend
                              labels = c("funny", "showed product quickly", "danger", "used sex", "animals", "celebrity", "patriotic")) #rename themes


#Bud Light waffle chart
BudLight_waffle <- youtube_rounded %>% #use rounded percents
  ggplot(aes(fill = theme, #fill colors by ad theme
              values = BudLight)) + #use percents for Bud Light
  geom_waffle(n_rows = 10, size = 0.33, color = "white", flip = FALSE) + #make a 10x10 waffle chart with vertical columns (instead of rows)
  coord_fixed() + #make it a square
  theme_bw() +
  theme(legend.position = "none", #remove legend
        panel.grid = element_blank(), #remove gridlines
        panel.border = element_blank(), #remove panel border
        plot.background = element_blank(), #remove plot background
        plot.title = element_text(hjust = 0.5), #center plot title
        axis.ticks = element_blank(), #remove axis ticks
        axis.text = element_blank()) + #remove axis text
  labs(title = "Bud Light") + #give plot a title
  colScale #use color palette previously specified
BudLight_waffle #view waffle chart

#Budweiser waffle chart
Budweiser_waffle <- youtube_rounded %>%
  ggplot(aes(fill = theme,
             values = Budweiser)) +
  geom_waffle(n_rows = 10, size = 0.33, color = "white", flip = FALSE) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  labs(title = "Budweiser") +
  colScale
Budweiser_waffle

#Coca-Cola waffle chart
CocaCola_waffle <- youtube_rounded %>%
  ggplot(aes(fill = theme,
             values = CocaCola)) +
  geom_waffle(n_rows = 10, size = 0.33, color = "white", flip = FALSE) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "bottom", #move legend below plot
        legend.background = element_blank(), #remove legend background
        legend.box.spacing = unit(1.3, "cm"), #add some space between legend and bottom of plot
        legend.title = element_blank(), #remove legend title
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) + #all legend elements in one row
  labs(title = "Coca-Cola") +
  colScale
CocaCola_waffle

#Doritos waffle chart
Doritos_waffle <- youtube_rounded %>%
  ggplot(aes(fill = theme,
             values = Doritos)) +
  geom_waffle(n_rows = 10, size = 0.33, color = "white", flip = FALSE) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  labs(title = "Doritos") +
  colScale
Doritos_waffle

#E-Trade waffle chart
ETrade_waffle <- youtube_rounded %>%
  ggplot(aes(fill = theme,
             values = ETrade)) +
  geom_waffle(n_rows = 10, size = 0.33, color = "white", flip = FALSE) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  labs(title = "E-Trade") +
  colScale
ETrade_waffle

#Hyundai waffle chart
Hyundai_waffle <- youtube_rounded %>%
  ggplot(aes(fill = theme,
             values = Hyundai)) +
  geom_waffle(n_rows = 10, size = 0.33, color = "white", flip = FALSE) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  labs(title = "Hyundai") +
  colScale
Hyundai_waffle

#Kia waffle chart
Kia_waffle <- youtube_rounded %>%
  ggplot(aes(fill = theme,
             values = Kia)) +
  geom_waffle(n_rows = 10, size = 0.33, color = "white", flip = FALSE) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  labs(title = "Kia") +
  colScale
Kia_waffle

#NFL waffle chart
NFL_waffle <- youtube_rounded %>%
  ggplot(aes(fill = theme,
             values = NFL)) +
  geom_waffle(n_rows = 10, size = 0.33, color = "white", flip = FALSE) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  labs(title = "NFL") +
  colScale
NFL_waffle

#Pepsi waffle chart
Pepsi_waffle <- youtube_rounded %>%
  ggplot(aes(fill = theme,
             values = Pepsi)) +
  geom_waffle(n_rows = 10, size = 0.33, color = "white", flip = FALSE) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  labs(title = "Pepsi", #add title and caption (couldn't figure out how to center caption text, used a bunch of spaces in lieu of better solution)
       caption = "\n\nPercent of Super Bowl ads from 2000-2020 that were humorous, showed the advertised product quickly,             \ninvolved danger, used sexuality, involved animals, had celebrity appearances, or were patriotic.                    \n\n\n\nCreated by Emily Rukstales | Source: FiveThirtyEight | #TidyTuesday                                            ") +
  colScale
Pepsi_waffle

#Toyota waffle chart
Toyota_waffle <- youtube_rounded %>%
  ggplot(aes(fill = theme,
             values = Toyota)) +
  geom_waffle(n_rows = 10, size = 0.33, color = "white", flip = FALSE) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  labs(title = "Toyota") +
  colScale
Toyota_waffle


#stitch plots together
all_plots <- plot_grid(BudLight_waffle, Budweiser_waffle, CocaCola_waffle, Doritos_waffle, ETrade_waffle, Hyundai_waffle, Kia_waffle, NFL_waffle, Pepsi_waffle, Toyota_waffle,
                     nrow = 2, #2 rows with 5 plots each
                     align = "h", #align plots horizontally
                     axis = "b") #align plots in reference to bottom (x) axis
all_plots #view all_plots

#give all plots a joint title
title <- ggdraw() + 
  draw_label("Super Bowl Ads 2000-2020", #title
              fontface = 'bold',
              size = 18,
              x = 0.5, #place origin in center
              hjust = 0.5) #center title over origin

#join title to all plots
title_plot <- plot_grid(title, all_plots, #join title with all_plots
                        ncol = 1, #place title in a colum above all_plots
                        rel_heights = c(0.1, 1)) #adjust spacing of title
title_plot #view title_plot


#save final plot
ggsave2(plot = title_plot,
        here("2021-03-02_Superbowl", "Outputs", "2021-03-02_Superbowl.png"),
        width = 11,
        height = 7)

