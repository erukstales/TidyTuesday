### Tidy Tuesday 2021 Week 9: Employed Status
### Created by: Emily Rukstales
### Created on: 2021-02-28
################################################################################


### Load Libraries #############################################################
library(tidyverse)
library(here)
library(tidytuesdayR)


### Load Data ##################################################################
tuesdata <- tt_load('2021-02-23')

earnings <- tuesdata$earn #store earn.csv data as an object


### Data Viz ###################################################################
earnings <- earnings %>%
  filter(sex != "Both Sexes", #filter only male and female
         race != "All Races", #filter each racial category
         age == "16 years and over") %>% #filter ages
  group_by(sex, race, year) %>% 
  summarise(median_weekly_earn_mean = mean(median_weekly_earn)) %>% #calculate mean earnings by sex, race, and year
  mutate(race = factor(race, levels = c("Asian", "White", "Black or African American"))) #change order of race categories from greatest income to lowest income


ggplot(earnings, #use earnings data
       aes(x = year, #years on x-axis
          y = median_weekly_earn_mean, #earnings on y-axis
          fill = sex)) + #fill color by sex
  geom_area(alpha = 0.6, #make fill transparent
            color = "#33454e") + #specify color of lines
  facet_wrap(~race) + #make a plot for each race
  theme_bw() +
  theme(panel.grid.major = element_line(color = "grey"), #make gridlines grey
        panel.grid.minor = element_blank(), #remove minor gridlines
        panel.border = element_rect(color = "black", fill = NA), #change color of panel border and remove fill
        panel.background = element_rect(fill = "#f5f5f5"), #change panel background
        strip.background = element_rect(fill = "#f5f5f5"), #change background color of facet title strips
        plot.background = element_rect(fill = "#e6e6e6"), #change plot background
        legend.background = element_rect(fill = NA), #remove white legend box background
        plot.margin = margin(30, 30, 30, 30)) + #give a margin around entire plot
  scale_x_continuous(expand = c(0, 0), #remove empty spaces on edges of each plot
                     breaks = seq(2011, 2020, 2)) + #specify x-axis spacing
  scale_y_continuous(expand = c(0, 0)) + #remove white spaces on edges of each plot
  scale_fill_manual(values = c("#81a9ad", "#537380")) + #define colors for each sex
  labs(x = "Year", #change x-axis label
       y = "Median Weekly Earnings", #change y-axis label
       fill = "Sex", #change legend title
       title = "Income Inequality by Race and Sex", #give plot a title
       subtitle = "Median weekly earnings from 2010-2020 for individuals >16 years old", #give plot a subtitle
       caption = "Created by Emily Rukstales | Source: U.S. Bureau of Labor Statistics") + #give plot a caption with name and source info
  ggsave(here("2021-02-23_Earnings", "Outputs", "2021-02-23_Earnings.png"), #save!
         width = 14, height = 5.5)














