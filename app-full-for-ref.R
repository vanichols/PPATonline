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
      menuItem("  Data entry", tabName = "data", icon = icon("leaf")),
      menuItem("  Resources", tabName = "resources", icon = icon("book"))
      # FIXED: Removed trailing comma after last menuItem
    ),
    
    
    ### Credit info, ADOPT IPM logo ###
    div(
      style = "position: fixed;
               bottom: 15px;
               left: 15px;
               font-size: 12px;
               color: #888;
               z-index: 1000;",
      img(
        src = "test.png",
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
        box(
          title = "Welcome to the online version of the Package Performance Assessment Tool (PPAT)",
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
              tags$strong("Data entry", style = "color: #f39c12;"),
              " tab allows users to enter qualitative ratings (1 to 5) and confidence levels regarding XX categories",
              tags$em("Pxx Pxx Assessment Tool", style = "color: #8e44ad;"),
              " and get graphical results instantly!"
            )
          )
        )
      )),
      
      ###### Enter data tab ######
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Package #1",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            height = "300px",
            rHandsontableOutput("sys1_hottable")
          ),
          box(
            title = "Package #2",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            height = "300px",
            rHandsontableOutput("sys2_hottable")
          )
        ),
        fluidRow(
          box(
            status = "primary",
            width = 12,
            height = "150px",
            
            # Center using column offset
            column(
              width = 12,
              align = "center",
              style = "margin-top: 30px;",
              actionButton(
                inputId = "create_plots_btn",
                label = "Create Visuals",
                class = "btn-primary btn-lg",  # btn-lg makes it larger
                style = "font-size: 24px; padding: 20px 50px;"
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Package #1 Visual",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            height = "500px",
            plotOutput("sys1_plot", height = "400px")
          ),
          box(
            title = "Overlapping Visual",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            height = "500px",
            plotOutput("overlapping_plot", height = "400px")
          ),
          box(
            title = "Package #2 Visual",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            height = "500px",
            plotOutput("sys2_plot", height = "400px")
          )
        )
      ),
      
      ###### Resources tab ######
      tabItem(tabName = "resources", fluidRow(
        box(
          title = "Accompanying material",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          h3("xxx", icon("heart")),
          p(
            "Below are downloadable PDFs of the questionaires to help users create ratings for each metric",
            style = "font-size: 16px; margin-bottom: 20px;"
          )
        ),
        box(
          title = "Literature",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          p(
            "Below are resources that help give context to this tool:",
            style = "font-size: 16px; margin-bottom: 20px;"
          ),
          
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
      ))
    )
  )
)
# server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  # Get user input Package #1
  values1 <- reactiveValues()
  
  # Initialize data frame
  observe({
    if (is.null(values1$data)) {
      values1$data <- data.frame( 
        PackageTitle = rep("", 6),
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
        colWidths = c(120, 150, 100, 150, 100)
      ) %>%
        hot_col(
          "PackageTitle", 
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
          source = as.character(c("1 - Not acceptable",
                                  "2 - Likely to dissuade from use", 
                                  "3 - Will be a consideration in decision to use", 
                                  "4 - Acceptable",
                                  "5 - Highly acceptable or improved")),
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
  
  # Observe changes to the table and auto-fill PackageTitle
  observeEvent(input$sys1_hottable, {
    # Get the current table data
    current_data <- hot_to_r(input$sys1_hottable)
    
    # Check if the first row of PackageTitle column has changed
    if (!is.null(current_data) && nrow(current_data) > 0) {
      first_row_value <- current_data$PackageTitle[1]
      
      # If first row has a value (not empty), fill it down
      if (!is.na(first_row_value) && first_row_value != "") {
        current_data$PackageTitle <- first_row_value
      }
      
      # Update the reactive values
      values1$data <- current_data
    }
  })
  
  
  # Check if all required cells are filled
  table_complete <- reactive({
    if (is.null(values1$data)) return(FALSE)
    
    data <- values1$data
    
    # Check if all required columns have non-empty values
    package_filled <- all(!is.na(data$PackageTitle) & data$PackageTitle != "")
    rating_filled <- all(!is.na(data$Rating) & data$Rating != "")
    confidence_filled <- all(!is.na(data$Confidence) & data$Confidence != "")
    
    return(package_filled & rating_filled & confidence_filled)
  })
  
  # Conditional UI for the button
  output$create_plots_btn <- renderUI({
    if (table_complete()) {
      actionButton("create_plots_btn", "Create Visuals", 
                   class = "btn-success", 
                   style = "margin-top: 15px;")
    }
  })
  
  # Handle button click
  observeEvent(input$create_plot1, {
    # Your figure creation logic here
    showNotification("Creating figure...", type = "message")
    
    # Example: You can access the complete data with values1$data
    # and create your figure here
  })
  
}

# run app -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
