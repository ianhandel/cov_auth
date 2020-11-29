#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(RcppRoll)
library(ggrepel)
library(ggthemr)

source("R/load_data.R")
source("R/plot_cases.R")
source("R/plot_positivity.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covid in some places"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("cases_plot"),
           plotOutput("positivity_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ggthemr(palette = "flat dark", text_size = 22, type = "outer")
    
    
    authorities <- c("City of Edinburgh", "Argyll and Bute",
                     "Scottish Borders", "Midlothian")
    
    dat <- get_la_data()
    
    dat <- dat %>%
      dplyr::filter(date > now() - days(120) & date < now() - days(3)) %>%
      dplyr::filter(ca_name %in% authorities) %>%
      group_by(ca_name) %>%
      mutate(label = if_else(date == max(date, na.rm = TRUE),
        str_glue("{ca_name} ({round(positive_7d_rate)})"), NA_character_
      )) %>%
      mutate(label_pos = if_else(date == max(date, na.rm = TRUE),
        str_glue("{ca_name} ({round(positive_test_rate_7d, 1)}%)"), NA_character_
      )) %>%
      ungroup()
    

    output$cases_plot <- renderPlot({
        plot_cases(dat)
    })
    
    output$positivity_plot <- renderPlot({
        plot_positivity(dat)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
