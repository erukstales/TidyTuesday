### Tidy Tuesday 2021 Week 12: Video Games
### Created by: Emily Rukstales
### Created on: 2021-02-16
################################################################################


### Load Libraries #############################################################
library(tidyverse)
library(here)
library(tidytuesdayR)
library(ggdark)


### Load Data ##################################################################
tuesdata <- tt_load('2021-03-16')

games <- tuesdata$games


### Data Viz ###################################################################
games$month <- as.factor(games$month) #change month from character to factor so we can rename levels
games$month <- recode_factor(games$month, #rename month levels to numbers
                             January = "1",
                             February = "2",
                             March = "3",
                             April = "4",
                             May = "5",
                             June = "6",
                             July = "7",
                             August = "8",
                             September = "9",
                             October = "10",
                             November = "11",
                             December = "12")

games$month <- as.numeric(games$month) #change month to numeric so that we can make a line graph

games %>%
  select("gamename", "year", "month", "avg") %>%
  filter(complete.cases(.),
         year > "2017" & year < "2021",
         gamename == "PLAYERUNKNOWN'S BATTLEGROUNDS" | gamename == "Counter-Strike: Global Offensive" | gamename == "Dota 2") %>%
ggplot(aes(x = month,
           y = avg,
           color = gamename)) +
  geom_line(size = 1.3) +
  facet_wrap(~year) +
  dark_theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(color = "#555555"),
        panel.grid.major = element_line(color = "#555555"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45,
                                   vjust = 0.7),
        panel.spacing = unit(1, "lines"),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        plot.margin = margin(30, 30, 20, 30)) +
  scale_x_continuous(expand = c(0, 0), #remove empty spaces on edges of each plot
                     breaks = seq(1, 12, 1),  #specify x-axis spacing
                     labels = c("Jan.", "Feb.", "March", "April", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec.")) + #rename months
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = c("Counter-Strike: Global Offensive" = "#f3ff82",
                                "Dota 2" = "#67d294",
                                "PLAYERUNKNOWN'S BATTLEGROUNDS" = "#009a9c")) +
  labs(x = " ",
       y = "Average number of simultaneous players",
       color = " ",
       title = "Average simultaneous players of 3 popular Steam video games \n",
       caption = "\n\n\nCreated by Emily Rukstales | Source: SteamCharts | #TidyTuesday") +
  ggsave(here("2021-03-16_VideoGames", "Outputs", "2021-03-16_VideoGames.png"),
         width = 10,
         height = 7)
























