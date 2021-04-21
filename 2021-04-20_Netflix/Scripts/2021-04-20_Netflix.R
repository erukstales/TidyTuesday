### Tidy Tuesday 2021 Week 17: Netflix Shows
### Created by: Emily Rukstales
### Created on: 2021-04-20
################################################################################


### Load Libraries #############################################################

library(tidyverse)
library(here)
library(tidytuesdayR)
library(tidytext)
library(gganimate)
library(PNWColors)



### Load Data ##################################################################

tuesdata <- tt_load('2021-04-20')

netflix <- tuesdata$netflix



### Data Wrangling #############################################################

netflix_clean <- netflix %>%
  select("title", "type", "country", "date_added", "release_year", "rating", "listed_in") #select columns I'm interested in

netflix_clean$listed_in <- netflix_clean$listed_in %>% #remove all spaces and special characters from genre column
  str_replace_all(" ", "") %>%
  str_replace_all(",", " ") %>% #replace commas separating genres with a single space
  str_replace_all("-", "") %>%
  str_replace_all("&", "") %>%
  str_replace_all("'", "")

netflix_clean <- netflix_clean %>%
  unnest_tokens(output = genre, input = listed_in) %>% #make a new observation for each word (genre)
  mutate(genre = as.factor(genre)) #turn genre into a factor

levels(netflix_clean$genre)

netflix_clean$genre <- netflix_clean$genre %>%
  recode("actionadventure" = "Action", #group and rename genres that I want to plot
         "animefeatures" = "Anime",
         "animeseries" = "Anime",
         "comedies" = "Comedy",
         "crimetvshows" = "Crime",
         "documentaries" = "Documentary",
         "docuseries" = "Documentary",
         "dramas" = "Dramas",
         "horrormovies" = "Horror",
         "internationalmovies" = "International",
         "internationaltvshows" = "International",
         "lgbtqmovies" = "LGBTQIA+",
         "realitytv" = "Reality",
         "sciencenaturetv" = "Science",
         "scififantasy" = "Sci-Fi/Fantasy",
         "standupcomedy" = "Comedy",
         "standupcomedytalkshows" = "Comedy",
         "tvactionadventure" = "Action",
         "tvcomedies" = "Comedy",
         "tvdramas" = "Dramas",
         "tvhorror" = "Horror",
         "tvscififantasy" = "Sci-Fi/Fantasy") 

netflix_clean <- netflix_clean %>% #filter out genres I want to plot
  filter(genre == "Action" | genre == "Anime" | genre == "Comedy" | genre == "Crime" | genre == "Documentary" | genre == "Dramas" | genre == "Horror" | genre == "International" | genre == "LGBTQIA+" | genre == "Reality" | genre == "Science" | genre == "Sci-Fi/Fantasy") %>%
  droplevels() %>% #remove the remaining levels 
  separate(date_added, #separate year from month and year
           into = c("Month.Day", "Year"),
           sep = ", ", remove = TRUE) %>%
  filter(Year >= 2010 & Year <= 2020) #filter for years 2010-2020


netflix_summary <- netflix_clean %>%
  group_by(genre, Year) %>% 
  summarise(genre_count = n()) #for each year, get the counts of titles listed under each genre


### Data Viz ###################################################################

pal <- pnw_palette("Sailboat", 12)

ggplot(netflix_summary,
       aes(x = genre, #genre on x-axis
           y = genre_count, #number of titles on y-axis
           fill = genre)) + #color each genre just for fun
  geom_bar(stat = "identity") +
  coord_flip() + #flip axes
  transition_states(
    Year, #animate plot so that it transitions from year to year
    transition_length = 2, #time it takes to transition
    state_length = 1) + #time it stays on the same plot
  theme_bw() +
  theme(legend.position = "none", #remove legend
        panel.grid = element_blank(), #remove gridlines
        axis.text = element_text(size = 12), #edit axis text size
        axis.title = element_text(size = 14), #edit title text size
        plot.caption = element_text(hjust = 0.5), #center caption below plot
        plot.title = element_text(hjust = 0.5), #center title above plot
        plot.margin = margin(20, 30, 20, 10)) + #add a small margin around plot
  scale_fill_manual(values = pal) +
  labs(title = 'Netflix titles by genre in {closest_state}', #give it a transition title
       y = "Number of titles",
       x = "Genre",
       caption = "Created by Emily Rukstales | Source: Kaggle | #TidyTuesday") +
  anim_save(here("2021-04-20_Netflix", "Outputs", "2021-04-20_Netflix.gif"))






























