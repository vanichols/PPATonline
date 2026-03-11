library(shiny)
library(rhandsontable)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(ggnewscale)
library(ggrepel)
library(scales)
library(patchwork)

# global ------------------------------------------------------------------

data_betas <- read_rds("data/processed/data_betas.RDS")
data_example <- read_rds("data/processed/data_example.RDS")

# Source utility functions (results plots)
source("R/utils.R")

# ui ----------------------------------------------------------------------

ui <- shinydashboard::dashboardPage(
  ###### Header ##################################################################
  shinydashboard::dashboardHeader(title = "PPATonline"),
  
  ###### Sidebar #################################################################
  shinydashboard::dashboardSidebar(
    ### Menu ###
    shinydashboard::sidebarMenu(
      id = "sidebar_menu",
      menuItem("  Welcome", tabName = "welcome", icon = icon("campground")),
      menuItem("  Data entry", tabName = "data", icon = icon("bug"))
      
    ),
    
    
    ### Credit info, ADOPT IPM logo ###
    div(
      style = "position: fixed;
               bottom: 15px;
               left: 15px;
               font-size: 12px;
               color: #888;
               z-index: 1000;",
      # Try different approaches for the image
      # # Option 1: Standard approach (what you have)
      img(
        src = "adopt-ipm_logo-clean.png",
        #src = "test.png",
        height = "50px",
        width = "auto",
        style = "margin-bottom: 5px;",
        onerror = "this.style.display='none'; console.log('Image failed to load');"
      ),
      br(),
      HTML(
        "<a href='https://adopt-ipm.eu/' target='_blank'>adopt-ipm.eu</a><br>
         ADOPT-IPM team (2026)<br>
         Last updated: March 2026<br>"
      )
    )
  ),
  #--end of sidebar
  
  
  ###### Body ####################################################################
  shinydashboard::dashboardBody(
    tags$head(tags$style(
      HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      ")
    )),
    
    tabItems(
      ###### Welcome tab ######
      tabItem(tabName = "welcome", fluidRow(
        # Custom green title
        box(
          title = "Welcome to the online version of the Pxx Pxx Assessment Tool (PPAT)",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          h3("Getting Started", icon("person-walking")),
          p(
            "Welcome to our dashboard! Below are directions and some useful resources:",
            style = "font-size: 16px; margin-bottom: 20px;"
          ),
          
          h4("Directions", style = "color: #2c3e50; margin-top: 25px;"),
          tags$ul(
            style = "line-height: 1.8; font-size: 15px;",
            tags$li(
              tags$strong("Data", style = "color: #f39c12;"),
              " tab allows users to enter qualitative ratings (1 to 5) and confidence levels regarding XX categories",
              tags$em("Pxx Pxx Assessment Tool", style = "color: #8e44ad;"),
              " and get graphical results instantly!"
            ),
            
            hr(style = "margin: 30px 0; border-top: 2px solid #bdc3c7;"),
            
            h4("Additional Resources", style = "color: #2c3e50; margin-bottom: 15px;"),
            tags$ul(
              style = "line-height: 2; font-size: 15px;",
              
              tags$li(
                "Read the ",
                tags$strong("accompanying publication", style = "color: #2980b9;"),
                " for the PPAT: ",
                tags$a(
                  "Publication in progress, here is the project website",
                  href = "https://adopt-ipm.eu/",
                  target = "_blank",
                  style = "color: #eb5e23; text-decoration: none; font-weight: bold;
                          border-bottom: 1px dotted #27ae60;"
                )
              )
            )
          )
          
          
        )
      )),
      #--end welcome tab
      
      ###### Enter data tab ######
      tabItem(
        tabName = "data",
        # First row
        fluidRow(
          box(
            title = "System #1",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            height = "300px",
            rHandsontableOutput("sys1_hottable")
          ),
          box(
            title = "System #2",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            height = "300px",
            rHandsontableOutput("sys2_hottable")
          )
        ),
        #--second row
        fluidRow(
          box(
            title = "System #1 Visual",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            height = "500px",
            plotOutput("sys1_plot", height = "400px")
          ),
          box(
            title = "System #2 Visual",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            height = "500px",
            plotOutput("sys2_plot", height = "400px")
          )
        )
        
      )
      #--end of data tab
      
      
    ) #--end of tabItems
  ) #--end of dashboard body
) #--end of dashboard page

# server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  # Get user input system #1
  values1 <- reactiveValues()
  
  # Initialize data frame
  observe({
    if (is.null(values1$data)) {
      values1$data <- data.frame( 
        SystemTitle = rep("", 6),
        Metric = c("Crop losses", 
                   "Direct costs", 
                   "Environmental impact",
                   "Health and safety",
                   "Time/management", 
                   "Third party coordination requirements"),
        Weight = c(50, 12.5, 12.5, 12.5, 6.25, 6.25),
        Rating = rep("", 6),
        Confidence = rep("", 6),
        stringsAsFactors = FALSE
      )
    }
  })
  
  # Render table
  output$sys1_hottable <- renderRHandsontable({
    if (!is.null(values1$data)) {
      
      rhandsontable(
        values1$data,
        rowHeaders = TRUE,
        height = 250,
        colWidths = c(180, 180, 100, 100, 100)
      ) %>%
        hot_col(
          "SystemTitle", 
          type = "text"
        ) %>% 
        hot_col(
          "Metric",
          readOnly = TRUE, 
          type = "text"
        ) %>%
        hot_col(
          "Weight",
          readOnly = TRUE,  
          type = "numeric", 
          format = "0.00"
        ) %>%
        hot_col(
          "Rating",
          type = "dropdown",
          source = as.numeric(c(1, 2, 3, 4, 5)),
          allowInvalid = FALSE
        ) %>%
        hot_col(
          "Confidence",
          type = "dropdown",
          source = as.character(c("Low", "Medium", "High", "Very high")),
          allowInvalid = FALSE
        ) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    }
  })
  
  # Observe changes to the table and auto-fill SystemTitle
  observeEvent(input$sys1_hottable, {
    # Get the current table data
    current_data <- hot_to_r(input$sys1_hottable)
    
    # Check if the first row of SystemTitle column has changed
    if (!is.null(current_data) && nrow(current_data) > 0) {
      first_row_value <- current_data$SystemTitle[1]
      
      # If first row has a value (not empty), fill it down
      if (!is.na(first_row_value) && first_row_value != "") {
        current_data$SystemTitle <- first_row_value
      }
      
      # Update the reactive values
      values1$data <- current_data
    }
  })
  
  
}

# run app -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
