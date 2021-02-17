### Tidy Tuesday 2021 Week 8: WEB DuBois Challenge
### Created by: Emily Rukstales
### Created on: 2021-02-16
################################################################################


### Load Libraries #############################################################
library(tidyverse)
library(here)
library(tidytuesdayR)
library(ggplot2)
library(pBrackets)


### Load Data ##################################################################
tuesdata <- tt_load('2021-02-16')

georgia_pop <- tuesdata$georgia_pop # store Georgia population data as an object
view(georgia_pop)


### Data Viz ###################################################################
# Chart: "Comparative increase of white and colored population in Georgia"

georgia_pop %>% #use georgia_pop data
  gather(key = "variable", value = "value", -Year) %>% # create a variable ("colored" or "white") column and a value (percent) column
  ggplot(aes(x = Year, # put year on x axis and percent on y axis
             y = value)) +
  geom_line(aes(group = variable,
                linetype = variable)) + # create line chart with 2 lines
  coord_flip() + # swap x and y axes
  scale_y_reverse(expand = c(0, 0), breaks = seq(0, 100, 5)) + # flip the scale on the x (previously the y) axis
  scale_x_continuous(expand = c(0, 0), breaks = seq(1790, 1890, 10)) + # adjust the scale on the y (previously the x) axis
  scale_linetype_manual(values = c("solid", "longdash"), labels = c("COLORED", "WHITE")) + # adjust the type of line and the legend labels
  labs(x = "", # no label on y axis
       y = "PERCENTS", # label x axis
       title = "COMPARATIVE INCREASE OF WHITE AND COLORED\nPOPULATION IN GEORGIA.\n\n",
       caption = "Created by Emily Rukstales | Source: Starks, Hillery, and Tyler - W.E.B. DuBois Data Challenge | #TidyTuesday #DuBoisChallenge") +
  theme(plot.background = element_rect(fill = "#f0d9c2"), # change background color
        panel.border = element_rect(color = "black", fill = NA, size = 0.1), # adjust color and size of border
        panel.background = element_rect(fill = "#f0d9c2"), # change panel color
        panel.grid.major = element_line(color = "#dc143c", size = 0.1), # change color and size of major grid lines
        panel.grid.minor = element_blank(), # get ride of minor grid lines
        axis.ticks = element_blank(), # get rid of axis ticks
        axis.text.y = element_text(size = 12), # change size of y axis text
        axis.title.x = element_text(size = 9), # change size of x axis title
        legend.position = "bottom", # move legend to the bottom
        legend.background = element_blank(), # remove legend background
        legend.key = element_blank(), # remove legend key background
        legend.title = element_blank(), # remove legend title
        legend.key.width = unit(3.2, "cm"), # adjust legend key width
        legend.spacing.x = unit(0.5, "cm"), # adjust legend spacing
        legend.text = element_text(margin = margin(r = 3, unit = "cm")), # adjust legend margins
        plot.margin = margin(30, 110, 30, 90), # put a margin around entire plot
        plot.title = element_text(face = "bold", size = 23, hjust = 0.5)) + # adjust plot title
 ggsave(here("2021-02-16_DuBois", "Outputs", "2021-02-16_DuBois.png"),
         dpi = 72,
         width = unit(11, "in"),
         height = unit(14, "in"))


  
  
  
  
  