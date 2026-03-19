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
data_example_utility <- read_rds("data/processed/data_example_utility.RDS")

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
            title = "Performance visuals",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "500px",
            plotOutput("sys_plot", height = "400px")
          )
          ),
        fluidRow(
          box(
            title = "Performance summary",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              # Left side: plot
              column(
                width = 6,
                plotOutput("summary_plot", height = "400px")
              ),
              # Right side: 2x2 grid of value boxes
              column(
                id = "value_boxes_container",  # Add ID for shinyjs
                width = 6,
                fluidRow(
                  column(6, valueBoxOutput("pkg1_utility", width = 12)),
                  column(6, valueBoxOutput("pkg1_confidence", width = 12))
                ),
                fluidRow(
                  column(6, valueBoxOutput("pkg2_utility", width = 12)),
                  column(6, valueBoxOutput("pkg2_confidence", width = 12))
                )
              )
            )
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
  
  # Initially hide the value boxes
  observe({
    shinyjs::hide("value_boxes_container")
  })
  
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
                   "User health and safety",
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
                   "User health and safety",
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
    if (is.null(values1$data) || is.null(values2$data)) return(FALSE)
    
    # Check values1$data
    data1 <- values1$data
    package_filled_1 <- all(!is.na(data1$PackageTitle) & data1$PackageTitle != "")
    rating_filled_1 <- all(!is.na(data1$Rating) & data1$Rating != "")
    confidence_filled_1 <- all(!is.na(data1$Confidence) & data1$Confidence != "")
    
    # Check values2$data
    data2 <- values2$data
    package_filled_2 <- all(!is.na(data2$PackageTitle) & data2$PackageTitle != "")
    rating_filled_2 <- all(!is.na(data2$Rating) & data2$Rating != "")
    confidence_filled_2 <- all(!is.na(data2$Confidence) & data2$Confidence != "")
    
    # Return TRUE only if both tables are complete
    return(package_filled_1 & rating_filled_1 & confidence_filled_1 &
             package_filled_2 & rating_filled_2 & confidence_filled_2)
  })
  
  # Enable/disable button based on table completion
  observe({
    if (table_complete()) {
      shinyjs::enable("create_plots_btn")
    } else {
      shinyjs::disable("create_plots_btn")
    }
  })
  

  # # Create a reactive value to store the plot
  # plot_to_display <- reactiveVal(NULL)
  # 
  # observeEvent(input$create_plots_btn, {
  #   showNotification("Creating figure...", type = "message")
  #   
  #   plotdata <- 
  #     values1$data |>
  #     bind_rows(values2$data) |>
  #     mutate(rating_numeric = as.numeric(str_sub(Rating, 1, 1))) |> 
  #     rename(title = PackageTitle,
  #            metric = Metric,
  #            weight = Weight,
  #            confidence_text = Confidence)
  #   
  #   # Generate the plot and store it in the reactive value
  #   generated_plot <- fxn_Make_Plots(data = plotdata, betas = data_betas)
  #   plot_to_display(generated_plot)
  #   
  #   # Show the value boxes
  #   shinyjs::show("value_boxes_container")
  #   
  #   showNotification("Figure created!", type = "message", duration = 3)
  # })
  
  # Create reactive values to store both plots
  plot1_to_display <- reactiveVal(NULL)
  plot2_to_display <- reactiveVal(NULL)
  
  # Create reactive values to store the utility and confidence values for each pkg
  util1_to_display <- reactiveVal(NULL)
  conf1_to_display <- reactiveVal(NULL)
  
  util2_to_display <- reactiveVal(NULL)
  conf2_to_display <- reactiveVal(NULL)
  
  
  observeEvent(input$create_plots_btn, {
    showNotification("Creating figures...", type = "message")
    
    # Prepare data 
    plotdata <- 
      values1$data |>
      bind_rows(values2$data) |> 
      mutate(rating_numeric = as.numeric(str_sub(Rating, 1, 1))) |> 
      rename(title = PackageTitle,
             metric = Metric,
             weight = Weight,
             confidence_text = Confidence)
    
    
    theutility <- fxn_Calc_Overall_Utility(data = plotdata, nsim = 1000)
    
    # Generate both plots and store them in the reactive values
    generated_plot1 <- fxn_Make_Plots(data = plotdata, betas = data_betas)
    generated_plot2 <- fxn_Make_Overall_Utility_Fig(data = theutility, betas = data_betas)
    
    plot1_to_display(generated_plot1)
    plot2_to_display(generated_plot2)
    
    # Get values and store them in the reactive values
    util1 <- round(theutility$utility[1], 2)
    util2 <- round(theutility$utility[2], 2)
    
    conf1 <- theutility$conf[1]
    conf2 <- theutility$conf[2]
    
    util1_to_display(util1)
    util2_to_display(util2)
    
    conf1_to_display(conf1)
    conf2_to_display(conf2)
    
    # Show the value boxes
    shinyjs::show("value_boxes_container")
    
    showNotification("Figures created!", type = "message", duration = 3)
  })
  
  # Render the plot for display in the UI
  output$sys_plot <- renderPlot({
    req(plot1_to_display())  # Only render if plot exists
    plot1_to_display()
  })
  
  # Render the plot for display in the UI
  output$summary_plot <- renderPlot({
    req(plot2_to_display())  # Only render if plot exists
    plot2_to_display()
  })
  
  # Render the util1 box
  output$pkg1_utility <- renderValueBox({
  
    req(util1_to_display)
      valueBox(
        value = format(util1_to_display(), digits = 2, nsmall = 0),
        subtitle = "Package 1 Utility",
        icon = icon("exclamation-triangle"),
        color = "black"
      )
    }
  )

}

# run app -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
