
### Load Libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(here)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)


### Load Data
water <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')


### User Interface
ui <- fluidPage(theme = shinytheme("spacelab"),
                
                # Application title
                titlePanel("Water Access Points in Africa"),
                
                # Sidebar with radio buttons for choosing which type of water system
                sidebarLayout(
                    sidebarPanel(
                        checkboxGroupInput(inputId = "water_tech",
                                     label = "Water system:",
                                     choices = c("Hand Pump", "Hydram", "Kiosk", "Mechanized Pump", "Rope and Bucket", "Tapstand"),
                                     selected = "Hand Pump")
                    ),
                    
                    mainPanel(
                        plotOutput("water_plot")
                    )
                )
)


server <- function(input, output) {
    
    water_clean <- water %>%
        drop_na(water_tech) %>%
        mutate(water_tech = ifelse(str_detect(water_tech, "Hand Pump"), "Hand Pump", water_tech),
               water_tech = ifelse(str_detect(water_tech, "Mechanized Pump"), "Mechanized Pump", water_tech),
               water_tech = as.factor(water_tech)) %>%
        select(2, 3, 7, 9) %>%
        filter(lon_deg > -25 & lon_deg < 52 & lat_deg > -40 & lat_deg < 35)
    
    africa <- ne_countries(scale = "medium", returnclass = "sf", continent = "Africa")
    
    colScale <- scale_color_manual(values = c("Hand Pump" = "#278B9A",
                                             "Hydram" = "#e75b64",
                                             "Kiosk" = "#d8af39",
                                             "Mechanized Pump" = "#6FB382",
                                             "Rope and Bucket" = "#AE93BE",
                                             "Tapstand" = "#5C5992"))
    
    rwater <- reactive({
        water_clean %>%
            filter(water_tech == input$water_tech)
    })
    
    output$water_plot <- renderPlot({
            ggplot() +
            geom_sf(data = africa,
                    fill = "#ffffff") +
            geom_point(data = rwater(),
                       aes(x = lon_deg,
                           y = lat_deg,
                           color = water_tech),
                       alpha = 0.5) +
            theme_bw() +
            theme(panel.grid = element_blank(),
                  axis.text = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  panel.border = element_blank()) +
            colScale +
            labs(x = "",
                 y = "")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

