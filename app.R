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
      menuItem("  Welcome", tabName = "welcome", icon = icon("mug-hot")),
      menuItem("  Data entry", tabName = "data", icon = icon("leaf"))
      #menuItem("  Resources", tabName = "resources", icon = icon("book"))
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
      ###### welcome tab ######
      tabItem(
        tabName = "welcome", 
        fluidRow(
          box(
            title = "Welcome to the online version of the Package Performance Assessment Tool (PPAT)",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            # Getting Started Section
            h3(icon("cookie-bite"), "Getting Started"),
            p(
              "The", 
              tags$strong("Data Entry", style = "color: #f39c12;"),
              "tab allows users to enter qualitative ratings (1 to 5) and confidence levels for six performance categories and get visual results instantly!",
              style = "font-size: 16px; line-height: 1.8; margin-bottom: 30px;"
            ),
            
            hr(style = "border-top: 1px solid #ddd; margin: 25px 0;"),
            
            # Accompanying Material Section
            h3(icon("book"), "Accompanying Material"),
            p(
              "Download a PDF of the questionnaire to help create ratings for each metric:",
              style = "font-size: 16px; margin-bottom: 15px;"
            ),
            
            # Download button
            downloadButton(
              "download_questionnaire",
              "Download Questionnaire",
              class = "btn-info",
              icon = icon("file-pdf"),
              style = "margin-bottom: 25px;"
            ),
            
            hr(style = "border-top: 1px solid #ddd; margin: 25px 0;"),
            
            # Publication Section
            h3(icon("newspaper"), "Publication"),
            p(
              "Read the",
              tags$strong("accompanying publication", style = "color: #2980b9;"),
              "for the PPAT:",
              style = "font-size: 16px; margin-bottom: 10px;"
            ),
            
            tags$a(
              icon("external-link-alt"),
              " Publication in progress - Visit the project website",
              href = "https://adopt-ipm.eu/",
              target = "_blank",
              class = "btn btn-default",
              style = "background-color: #eb5e23; color: white; border: none; 
                 font-size: 15px; padding: 10px 20px; margin-top: 10px;"
            )
          )
        )
      ), #--end welcome tab
      
      
      ###### Enter data tab ######
      tabItem(
        tabName = "data",
        # Directions box at the top
        fluidRow(
          box(
            title = "Instructions",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            HTML("
      <ol style='font-size: 16px; line-height: 1.8;'>
        <li>Enter a name for <strong>Package #1</strong> in the first cell of the table below, and it will auto-populate down</li>
        <li>Enter your value ratings and confidence levels for <strong>Package #1</strong></li>
        <li>Do the same for <strong>Package #2</strong></li>
        <li>Once all data is entered, the <strong>'Create Visuals'</strong> button will activate</li>
        <li>Click the <strong>'Create Visuals'</strong> button to generate performance visuals</li>
        <li>Compare <strong>utility</strong> and <strong>confidence</strong> metrics between packages</li>
        <li><strong>Download</strong> the visuals if you wish</li>
      </ol>
    ")
          )
        ),
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
          column(
            width = 12,
            align = "center",
            style = "margin-top: 20px; margin-bottom: 20px;",
            actionButton(
              inputId = "create_plots_btn",
              label = "Create Visuals",
              class = "btn-lg",
              style = "font-size: 20px; padding: 20px 50px; background-color: #eb5e23; border-color: black;"
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
                ),
                fluidRow(
                  column(
                    12, 
                    align = "center",
                    style = "margin-top: 20px;",  # Add some spacing above the button
                    downloadButton(
                      "download_plots",
                      "Download Plots",
                      class = "btn-primary",
                      icon = icon("download")
                    )
                  )
                )
              )
            )
          )
        )
      ), #--end data tab
      
      
      ###### Resources tab ######
      tabItem(tabName = "resources", ) #--end resources tab
      
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
  
  output$download_questionnaire <- downloadHandler(
    filename = function() {
      "questionnaire.pdf"
    },
    content = function(file) {
      file.copy("www/questionnaire.pdf", file)
    }
  )
  
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
  
  # Initially disable the download button
  shinyjs::disable("download_plots")
  
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
    shinyjs::enable("download_plots")
    
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
  
  # Render the util boxes
  output$pkg1_utility <- renderValueBox({
  
    req(util1_to_display)
      valueBox(
        value = format(util1_to_display(), digits = 2, nsmall = 0),
        subtitle = "Package 1 Utility",
        icon = icon("hammer"),
        color = "yellow"
      )
    })
  
  output$pkg2_utility <- renderValueBox({
    
    req(util2_to_display)
    valueBox(
      value = format(util2_to_display(), digits = 2, nsmall = 0),
      subtitle = "Package 2 Utility",
      icon = icon("wrench"),
      color = "green"
    )
  })
  # Render the util boxes
  output$pkg1_confidence <- renderValueBox({
    
    req(conf1_to_display)
    valueBox(
      value = conf1_to_display(),
      subtitle = "Package 1 confidence",
      icon = icon("gauge-simple-high"),
      color = "yellow"
    )
  })
  
  output$pkg2_confidence <- renderValueBox({
    
    req(conf2_to_display)
    valueBox(
      value = conf2_to_display(), 
      subtitle = "Package 2 Confidence",
      icon = icon("gauge-simple"),
      color = "green"
    )
  })
  
  # Your download handler
  output$download_plots <- downloadHandler(
    filename = function() {
      paste0("ppat_plots_",
             "_",
             Sys.Date(),
             ".png")
    },
    content = function(file) {
      req(plot1_to_display()) 
      
      p <- plot1_to_display()
      
      #--Explicitly add a white background
      p <- p + theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
      
      # Save the plot
      ggsave(
        file,
        plot = p,
        device = "png",
        width = 15,
        height = 8,
        dpi = 300,
        bg = "white"
      )
    }
  )
  
}

# run app -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
