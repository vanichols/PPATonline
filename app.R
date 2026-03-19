library(shiny)
library(shinyjs)
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
    useShinyjs(),
    tags$head(tags$style(
      HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      ")
    )),
    
    tabItems(
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
            title = "Individual package visuals",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "500px",
            plotOutput("sys_plot", height = "400px")
          )
          ),
        fluidRow(
          box(
            title = "Overlapping Visual",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "500px",
            plotOutput("overlapping_plot", height = "400px")
          )  
        )
        
        
      ) #--end data tab
      
    ) #--end tabItems
  
  ) #--end dashboard body
)

# server ------------------------------------------------------------------

dummy1 <-
  data.frame(
  PackageTitle = rep("TEST", 6),
  Metric = c("Crop losses",
             "Direct costs",
             "Environmental impact",
             "Health and safety",
             "Time/management",
             "Third party coordination requirements"),
  Weight = c(50, 12.5, 12.5, 12.5, 6.25, 6.25),
  Rating = rep("1 - Not acceptable", 6),
  Confidence = rep("Medium", 6),
  stringsAsFactors = FALSE
)

dummy2 <-
  data.frame(
    PackageTitle = rep("TEST2", 6),
    Metric = c("Crop losses",
               "Direct costs",
               "Environmental impact",
               "Health and safety",
               "Time/management",
               "Third party coordination requirements"),
    Weight = c(50, 12.5, 12.5, 12.5, 6.25, 6.25),
    Rating = rep("4 - Acceptable", 6),
    Confidence = rep("Very high", 6),
    stringsAsFactors = FALSE
  )

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
  
  
  # Get user input Package #2
  values2 <- reactiveValues()
  
  # Initialize data frame
  observe({
    if (is.null(values2$data)) {
      values2$data <- data.frame( 
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
  output$sys2_hottable <- renderRHandsontable({
    if (!is.null(values2$data)) {
      
      rhandsontable(
        values2$data,
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
  observeEvent(input$sys2_hottable, {
    # Get the current table data
    current_data <- hot_to_r(input$sys2_hottable)
    
    # Check if the first row of PackageTitle column has changed
    if (!is.null(current_data) && nrow(current_data) > 0) {
      first_row_value <- current_data$PackageTitle[1]
      
      # If first row has a value (not empty), fill it down
      if (!is.na(first_row_value) && first_row_value != "") {
        current_data$PackageTitle <- first_row_value
      }
      
      # Update the reactive values
      values2$data <- current_data
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
  
  # Enable/disable button based on table completion
  observe({
    if (table_complete()) {
      shinyjs::enable("create_plots_btn")
    } else {
      shinyjs::disable("create_plots_btn")
    }
  })
  

  # Create a reactive value to store the plot
  plot_to_display <- reactiveVal(NULL)
  
  # Handle button click
  observeEvent(input$create_plots_btn, {
    showNotification("Creating figure...", type = "message")
    
    plotdata <- 
      values1$data |>
      bind_rows(values2$data) |>
      mutate(rating_numeric = as.numeric(str_sub(Rating, 1, 1))) |> 
      rename(title = PackageTitle,
             metric = Metric,
             weight = Weight,
             confidence_text = Confidence)
    
    # Generate the plot and store it in the reactive value
    generated_plot <- fxn_Make_Paired_Ridge_Plots(data = plotdata, betas = data_betas)
    plot_to_display(generated_plot)
    
    showNotification("Figure created!", type = "message", duration = 3)
  })
  
  # Render the plot for display in the UI
  output$sys_plot <- renderPlot({
    req(plot_to_display())  # Only render if plot exists
    plot_to_display()
  })
  
}

# run app -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
