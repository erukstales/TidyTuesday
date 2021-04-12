#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(here)
library(maps)

forest <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest.csv')


ui <- fluidPage(theme = shinytheme("spacelab"),

    # Application title
    titlePanel("Global Deforestation 1990 - 2015"),

    # Sidebar with radio buttons for choosing which baby
    sidebarLayout(
        sidebarPanel(
            radioButtons(inputId = "year",
                         label = "Year:",
                         choices = c("1990", "2000", "2010", "2015"),
                         selected = "1990")
        ),
        
        mainPanel(
           plotOutput("deforestation_plot")
        )
    )
)


server <- function(input, output) {
    world <- map_data("world")
    
    forest <- forest %>%
        rename(region = entity) %>%
        mutate(region = recode(region,
                               "Congo" = "Republic of Congo",
                               "French Guyana" = "French Guiana",
                               "United Kingdom" = "UK",
                               "United States" ="USA")) %>%
        filter(region != "Gibraltar" & region != "Tokelau" & region != "World")
    
    world_forest <- inner_join(world, forest, by = "region")
    
    rdeforest <- reactive({
        world_forest %>%
            filter(year == input$year)
    })
    output$deforestation_plot <- renderPlot({
        rdeforest() %>%
            ggplot() +
            geom_polygon(data = world,
                         aes(x = long,
                             y = lat,
                             group = group),
                         color = "black",
                         fill = "#cccccc") +
            geom_polygon(aes(x = long,
                             y = lat,
                             group = group,
                             fill = net_forest_conversion),
                         color = "black") +
            coord_fixed(1.3) +
            scale_fill_continuous(low = "#1e55eb", high = "#eb1e96", 
                                  name = "Net forest\nconversion (hectares)",
                                  label = scales::comma,
                                  limits = c(-4000000, 2400000)) + 
            theme_minimal() +
            theme(panel.grid = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank()) +
            labs(x = " ",
                 y = " ")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
