### Tidy Tuesday 2021 Week 18: CEO Departures
### Created by: Emily Rukstales
### Created on: 2021-04-27
################################################################################


### Load Libraries #############################################################
library(tidyverse)
library(here)
library(tidytuesdayR)
library(ggtext)


### Load Data ##################################################################
tuesdata <- tt_load('2021-04-27')

departures <- tuesdata$departures


### Data Wrangling #############################################################

departures_clean <- departures %>%
  select(4, 7) %>%
  filter(departure_code < 7,
         fyear >= 1995 & fyear <= 2018) %>%
  mutate(departure_code = as.factor(departure_code),
         fyear = as.integer(fyear))

departures_clean$departure_code <- departures_clean$departure_code %>%
  recode("1" = "Involuntary",
         "2" = "Involuntary",
         "3" = "Involuntary",
         "4" = "Involuntary",
         "5" = "Voluntary",
         "6" = "Voluntary") 

levels(departures_clean$departure_code)

departures_summary <- departures_clean %>%
  group_by(fyear, departure_code) %>%
  summarise(dep_count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = departure_code,
              values_from = dep_count)


### Data Viz ###################################################################
departures_summary %>%
  ggplot() +
  geom_segment(aes(x = fyear,
                   xend = fyear,
                   y = Involuntary,
                   yend = Voluntary),
               color = "grey",
               size = 1) +
  geom_point(aes(x = fyear,
                 y = Involuntary),
             color = "#e3aa74",
             size = 5) +
  geom_point(aes(x = fyear,
                 y = Voluntary),
             color = "#798dd4",
             size = 5) +
  scale_x_continuous(breaks = seq(1995, 2018, 5)) +
  scale_y_continuous(breaks = seq(20, 200, 40)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_markdown(size = 15, face = "bold", hjust = 0.5),
        plot.margin = margin(15, 15, 15, 15),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 13)) +
  labs(x = "\n Fiscal year", 
       y = "Number of departures \n",
       title = "<span style = 'color: #3e5296'>Voluntary</span> and <span style = 'color: #c27934'>involuntary</span> CEO departures from S&P 1500 firms from 1995 - 2018") +
  ggsave(here("2021-04-27_CEODepartures", "Outputs", "2021-04-27_CEODepartures.png"),
         width = 10, height = 7)
  





















































