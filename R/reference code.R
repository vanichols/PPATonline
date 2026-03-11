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
      menuItem("  Data", tabName = "data", icon = icon("bug"))
      
    ),
    
    # Plot option to be overlapping or not
    conditionalPanel(
      condition = "input.sidebar_menu == 'data'",
      h4("Plot Options"),
      checkboxInput("overlapping_view", "Overlapping view", value = FALSE)
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
  
  ###### Display load visualization as rose plot ######
  output$rose_plot <- renderPlot({
    req(input$substance_single)
    if (input$detailed_view) {
      fxn_Make_Detailed_Rose_Plot(compound_name = input$substance_single,
                                  data = data_details)
    } else {
      fxn_Make_Rose_Plot(compound_name = input$substance_single,
                         data = data_compartments)
    }
    
    
  })
  
  ###### Download rose plot ######
  output$download_rose_plot <- downloadHandler(
    filename = function() {
      paste0("rose_plot_",
             input$substance_single,
             "_",
             Sys.Date(),
             ".png")
    },
    content = function(file) {
      req(input$substance_single)
      
      # Create the same plot as in renderPlot
      p <- if (input$detailed_view) {
        fxn_Make_Detailed_Rose_Plot(compound_name = input$substance_single,
                                    data = data_details)
      } else {
        fxn_Make_Rose_Plot(compound_name = input$substance_single,
                           data = data_compartments)
      }
      
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
        width = 10,
        height = 8,
        dpi = 300,
        bg = "white"
      )
    }
  )
  
  ###### Display load on distribution ######
  output$dist_plot <- renderPlot({
    req(input$substance_single)
    fxn_Make_Distribution_Plot(compound_names = input$substance_single,
                               data = data_details)
  })
  
  ###### Download data option ######
  output$download_ai_data <- downloadHandler(
    filename = function() {
      req(input$substance_single)
      paste0(
        "load_score_details_",
        gsub("[^A-Za-z0-9]", "_", input$substance_single),
        "_",
        Sys.Date(),
        ".tsv"
      )
    },
    content = function(file) {
      req(input$substance_single)
      data_sub <- single_substance_data()
      display_data <-
        data_sub |>
        dplyr::mutate_if(is.numeric, round, 3) #|>
      #dplyr::select(-xmax, -xmin, -xmid, -trunk)
      
      write.table(
        display_data,
        file,
        sep = "\t",
        row.names = FALSE,
        col.names = TRUE,
        quote = FALSE
      )
    }
  )
  
  
  ###### Display costs ######
  output$cost_plot <- renderPlot({
    req(input$substance_single)
    fxn_Make_Costs_Plot(
      compound_name = input$substance_single,
      data = data_compartments,
      data2 = data_peacou,
      country_adjuster = "EU"
    )
  })
  
  ###### Download costs plot ######
  output$download_cost_plot <- downloadHandler(
    filename = function() {
      paste0("cost_plot_",
             input$substance_single,
             "_",
             Sys.Date(),
             ".png")
    },
    content = function(file) {
      req(input$substance_single)
      
      p <- fxn_Make_Costs_Plot(
        compound_name = input$substance_single,
        data = data_compartments,
        data2 = data_peacou,
        country_adjuster = "EU"
      )
      
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
        width = 10,
        height = 8,
        dpi = 300,
        bg = "white"
      )
    }
  )
  
  
  
  # double substances tab =====================================================
  
  ###### Populate filter lists (runs once at app startup) ######
  
  observeEvent(TRUE, {
    # Substance category filter
    updateSelectInput(
      session,
      "substance_category1",
      choices = unique(data_details$compound_category) |>
        sort()
    )
    # Substance origin filter
    updateSelectInput(
      session,
      "substance_origins1",
      choices = unique(data_details$compound_origin) |>
        sort()
    )
    
  }, once = TRUE)
  
  observeEvent(TRUE, {
    # Substance type filter
    updateSelectInput(
      session,
      "substance_category2",
      choices = unique(data_details$compound_category) |>
        sort()
    )
    # Substance origin filter
    updateSelectInput(
      session,
      "substance_origins2",
      choices = unique(data_details$compound_origin) |>
        sort()
    )
    
  }, once = TRUE)
  
  
  
  ###### Populate list of substance (reacts on filters) ######
  #--data for second tab, 1st choice
  substance_choices1 <- reactive({
    data_details_filtered1 <- data_details
    
    # Filter by origin only if an origin is selected
    if (!is.null(input$substance_origins1) &&
        length(input$substance_origins1) > 0) {
      data_details_filtered1 <-
        data_details_filtered1 |>
        dplyr::filter(compound_origin %in% input$substance_origins1)
    }
    
    # Filter by type only if a category is selected
    if (!is.null(input$substance_category1) &&
        length(input$substance_category1) > 0) {
      data_details_filtered1 <-
        data_details_filtered1 |>
        dplyr::filter(compound_category %in% input$substance_category1)
    }
    
    
    
    # Format final substance list
    data_details_filtered1 |>
      dplyr::pull(compound) |>
      unique() |>
      sort()
  })
  
  #--data for second tab, 2nd choice
  substance_choices2 <- reactive({
    data_details_filtered2 <- data_details
    
    # Filter by origin only if an origin is selected
    if (!is.null(input$substance_origins2) &&
        length(input$substance_origins2) > 0) {
      data_details_filtered2 <-
        data_details_filtered2 |>
        dplyr::filter(compound_origin %in% input$substance_origins2)
    }
    
    # Filter by type only if a category is selected
    if (!is.null(input$substance_category2) &&
        length(input$substance_category2) > 0) {
      data_details_filtered2 <-
        data_details_filtered2 |>
        dplyr::filter(compound_category %in% input$substance_category2)
    }
    
    
    
    # Format final substance list
    data_details_filtered2 |>
      dplyr::pull(compound) |>
      unique() |>
      sort()
  })
  
  ###### Selected substance1 based on user choice ######
  observe({
    choices1 <- substance_choices1()
    selected1 <- isolate(input$substance_double1)
    if (!is.null(selected1))
      selected1 <- selected1[selected1 %in% choices1]
    updateSelectInput(session,
                      "substance_double1",
                      choices = choices1,
                      selected = selected1)
  })
  
  # If current selection is no longer valid (e.g. after a new filter is applied), clear it
  observe({
    valid_choices <- substance_choices1()
    current <- input$substance_double1
    if (!is.null(current) && !current %in% valid_choices) {
      updateSelectInput(session, "substance_double1", selected = "")
    }
  })
  
  ###### Selected substance2 based on user choice ######
  observe({
    choices2 <- substance_choices2()
    selected2 <- isolate(input$substance_double2)
    if (!is.null(selected2))
      selected2 <- selected2[selected2 %in% choices2]
    updateSelectInput(session,
                      "substance_double2",
                      choices = choices2,
                      selected = selected2)
  })
  
  # If current selection is no longer valid (e.g. after a new filter is applied), clear it
  observe({
    valid_choices <- substance_choices2()
    current <- input$substance_double2
    if (!is.null(current) && !current %in% valid_choices) {
      updateSelectInput(session, "substance_double2", selected = "")
    }
  })
  
  
  ###### Display HPL visualisation graph ######
  output$rose_plot1 <- renderPlot({
    req(input$substance_double1)
    fxn_Make_Rose_Plot(compound_name = input$substance_double1,
                       data = data_compartments)
  })
  
  output$rose_plot2 <- renderPlot({
    req(input$substance_double2)
    fxn_Make_Rose_Plot(compound_name = input$substance_double2,
                       data = data_compartments)
  })
  
  output$dist_plot_both <- renderPlot({
    req(input$substance_double1)
    fxn_Make_Distribution_Plot(
      compound_names = c(input$substance_double1, input$substance_double2),
      data = data_details
    )
  })
  
  
  
  ###### Download data option ######
  #--something is funky here
  # output$download_data2 <- downloadHandler(
  #   filename = function() {
  #     req(input$substance_double1)
  #     req(input$substance_double2)
  #     paste0(
  #       "load_score_details_", #--make sure only allowed characters in name
  #       gsub(
  #         "[^A-Za-z0-9]",
  #         input$substance_double1),
  #         "_",
  #       gsub(
  #           "[^A-Za-z0-9]",
  #           input$substance_double2),
  #       "_",
  #       Sys.Date(),
  #       ".tsv"
  #     )
  #   },
  #   content = function(file) {
  #     req(input$substance_double1)
  #     #--what should go here?
  #     data_sub <-
  #       data_details |>
  #       filter(compound_name %in% c(input$substance_double1, input$substance_double2))
  #
  #     display_data2 <-
  #       data_sub |>
  #       dplyr::mutate_if(is.numeric, round, 3) |>
  #       dplyr::select(
  #         compound,
  #         compound_type,
  #         env_raw,
  #         eco.terr_raw,
  #         eco.aqua_raw,
  #         hum_raw,
  #         load_score,
  #         missing_share
  #       )
  #
  #     write.table(
  #       display_data2,
  #       file,
  #       sep = "\t",
  #       row.names = FALSE,
  #       col.names = TRUE,
  #       quote = FALSE
  #     )
  #   }
  # )
  
  # Calculate load =======================================================
  # Initialize reactive values for both tables
  values <- reactiveValues()
  
  # Initialize data frame
  observe({
    if (is.null(values$data)) {
      initial_rows <- 8
      values$data <- data.frame(
        Compound = rep("", initial_rows),
        Compound_Load = rep(0, initial_rows),
        SocietalCost = rep(0, initial_rows),
        ecotoxicity_aquatic = rep(0, initial_rows),
        ecotoxicity_terrestrial = rep(0, initial_rows),
        environmental_fate = rep(0, initial_rows),
        human_health = rep(0, initial_rows),
        QuantityApplied_kgperarea = rep(0, initial_rows),
        EcoAqu_Load = rep(0, initial_rows),
        EcoTerr_Load = rep(0, initial_rows),
        EnvPers_Load = rep(0, initial_rows),
        HumHea_Load = rep(0, initial_rows),
        Total_Load = rep(0, initial_rows),
        Total_SocietalCosts = rep(0, initial_rows),
        stringsAsFactors = FALSE
      )
    }
  })
  
  # Add row functionality
  observeEvent(input$add_row, {
    if (nrow(values$data) < 50) {
      new_row <- data.frame(
        Compound = "",
        Compound_Load = 0,
        SocietalCost = 0,
        ecotoxicity_aquatic = 0,
        ecotoxicity_terrestrial = 0,
        environmental_fate = 0,
        human_health = 0,
        QuantityApplied_kgperarea = 0,
        EcoAqu_Load = 0,
        EcoTerr_Load = 0,
        EnvPers_Load = 0,
        HumHea_Load = 0,
        Total_Load = 0,
        Total_SocietalCosts = 0,
        stringsAsFactors = FALSE
      )
      values$data <- rbind(values$data, new_row)
    }
  })
  
  # Remove row functionality
  observeEvent(input$remove_row, {
    if (nrow(values$data) > 1) {
      values$data <- values$data[-nrow(values$data), ]
    }
  })
  
  # Helper function to update calculations
  update_calculations <- function(data) {
    for (i in 1:nrow(data)) {
      if (data$Compound[i] != "" && !is.na(data$Compound[i])) {
        matching_row <- data_totloads[data_totloads$compound == data$Compound[i], ]
        if (nrow(matching_row) > 0) {
          # Populate hidden intermediate values
          data$ecotoxicity_aquatic[i] <- matching_row$ecotoxicity_aquatic[1]
          data$ecotoxicity_terrestrial[i] <- matching_row$ecotoxicity_terrestrial[1]
          data$environmental_fate[i] <- matching_row$environmental_fate[1]
          data$human_health[i] <- matching_row$human_health[1]
          data$Compound_Load[i] <- matching_row$tot_load_score[1]
          data$SocietalCost[i] <- matching_row$totcost_euros_kg_ref[1] * 0.5701703
          
          # Calculate loads only if quantity is applied
          if (!is.na(data$QuantityApplied_kgperarea[i]) &&
              data$QuantityApplied_kgperarea[i] > 0) {
            data$EcoAqu_Load[i] <- data$ecotoxicity_aquatic[i] * data$QuantityApplied_kgperarea[i]
            data$EcoTerr_Load[i] <- data$ecotoxicity_terrestrial[i] * data$QuantityApplied_kgperarea[i]
            data$EnvPers_Load[i] <- data$environmental_fate[i] * data$QuantityApplied_kgperarea[i]
            data$HumHea_Load[i] <- data$human_health[i] * data$QuantityApplied_kgperarea[i]
            data$Total_Load[i] <- data$Compound_Load[i] * data$QuantityApplied_kgperarea[i]
            data$Total_SocietalCosts[i] <- data$SocietalCost[i] * data$QuantityApplied_kgperarea[i]
          } else {
            data$EcoAqu_Load[i] <- 0
            data$EcoTerr_Load[i] <- 0
            data$EnvPers_Load[i] <- 0
            data$HumHea_Load[i] <- 0
            data$Total_Load[i] <- 0
            data$Total_SocietalCosts[i] <- 0
          }
        }
      } else {
        data$EcoAqu_Load[i] <- 0
        data$EcoTerr_Load[i] <- 0
        data$EnvPers_Load[i] <- 0
        data$HumHea_Load[i] <- 0
        data$Total_Load[i] <- 0
        data$Total_SocietalCosts[i] <- 0
      }
    }
    return(data)
  }
  
  # Render table - ONLY SHOW COLUMNS YOU WANT VISIBLE
  output$pest_hottable <- renderRHandsontable({
    if (!is.null(values$data)) {
      values$data <- update_calculations(values$data)
      
      # Select only the columns to display (hidden columns won't show)
      display_data <- values$data[, c(
        "Compound",
        "Compound_Load",
        "QuantityApplied_kgperarea",
        "EcoAqu_Load",
        "EcoTerr_Load",
        "EnvPers_Load",
        "HumHea_Load",
        "Total_Load",
        "Total_SocietalCosts"
      )]
      
      rhandsontable(
        display_data,
        rowHeaders = TRUE,
        height = 250,
        colWidths = c(160, 120, 120, 100, 100, 100, 100, 100, 120)
      ) %>%
        hot_col(
          "Compound",
          type = "dropdown",
          source = as.character(unique(data_totloads$compound)),
          allowInvalid = FALSE
        ) %>%
        hot_col("Compound_Load",
                readOnly = TRUE,
                format = "0.000") %>%
        hot_col("QuantityApplied_kgperarea",
                type = "numeric",
                format = "0.000") %>%
        hot_col("EcoAqu_Load", readOnly = TRUE, format = "0.000") %>%
        hot_col("EcoTerr_Load", readOnly = TRUE, format = "0.000") %>%
        hot_col("EnvPers_Load", readOnly = TRUE, format = "0.000") %>%
        hot_col("HumHea_Load", readOnly = TRUE, format = "0.000") %>%
        hot_col("Total_Load", readOnly = TRUE, format = "0.000") %>%
        hot_col("Total_SocietalCosts",
                readOnly = TRUE,
                format = "0.00") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    }
  })
  
  
  # Update data when table is edited
  observeEvent(input$pest_hottable, {
    if (!is.null(input$pest_hottable)) {
      updated_data <- hot_to_r(input$pest_hottable)
      
      # Merge the updated visible columns back with the full data
      values$data$Compound <- updated_data$Compound
      values$data$QuantityApplied_kgperarea <- updated_data$QuantityApplied_kgperarea
      
      # Trigger recalculation
      values$data <- update_calculations(values$data)
    }
  })
  
  
  # Summary output
  output$pest_insight <- renderText({
    if (!is.null(values$data)) {
      # Filter to only filled rows (compounds that have been selected)
      filled_data <- values$data[values$data$Compound != "" &
                                   !is.na(values$data$Compound), ]
      
      if (nrow(filled_data) > 0) {
        grand_total <- sum(values$data$Total_Load, na.rm = TRUE)
        
        # Find min and max risk scores among filled rows
        load_min <- min(filled_data$Compound_Load, na.rm = TRUE)
        load_max <- max(filled_data$Compound_Load, na.rm = TRUE)
        risk_min <- min(filled_data$Total_Load, na.rm = TRUE)
        risk_max <- max(filled_data$Total_Load, na.rm = TRUE)
        
        # Find compounds with min and max risk scores
        min_compound <- filled_data$Compound[which(filled_data$Compound_Load == load_min)[1]]
        max_compound <- filled_data$Compound[which(filled_data$Compound_Load == load_max)[1]]
        
        # Find applications with min and max risk scores
        min_applic <- filled_data$Compound[which(filled_data$Total_Load == risk_min)[1]]
        max_applic <- filled_data$Compound[which(filled_data$Total_Load == risk_max)[1]]
        
        paste(
          # "Lowest Load Compound:",
          # "\n",
          # min_compound,
          # " (",
          # format(load_min, digits = 2, nsmall = 3),
          # ")",
          "Highest Load Compound:",
          "\n",
          max_compound,
          " (",
          format(load_max, digits = 2, nsmall = 3),
          ")",
          "\n",
          
          # "\nLowest Load Application:",
          # "\n",
          # min_applic,
          # " (",
          # format(risk_min, digits = 1, nsmall = 2),
          # " ha-1 )",
          "\n\nHighest Load Application:",
          "\n",
          max_applic,
          " (",
          format(risk_max, digits = 1, nsmall = 2),
          " ha-1 )"
          
        )
      } else {
        "No compounds have been selected yet."
      }
    }
  })
  
  
  # Value boxes for dashboard display---NEED TO ADD THE COMPARTMENTS
  #--total
  output$pest_totalload <- renderValueBox({
    if (!is.null(values$data)) {
      grand_total <- sum(values$data$Total_Load, na.rm = TRUE)
      valueBox(
        value = format(grand_total, digits = 2, nsmall = 0),
        subtitle = "Total Package Load Per Hectare",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    }
  })
  
  #--EcoAqu
  output$pest_ecoaqu <- renderValueBox({
    if (!is.null(values$data)) {
      grand_total <- sum(values$data$EcoAqu_Load, na.rm = TRUE)
      valueBox(
        value = format(grand_total, digits = 2, nsmall = 0),
        subtitle = "Ecotox-Aquatic Load (1/6 weight)",
        icon = icon("fish"),
        color = "blue"
      )
    }
  })
  
  #--EcoTerr
  output$pest_ecoterr <- renderValueBox({
    if (!is.null(values$data)) {
      grand_total <- sum(values$data$EcoTerr_Load, na.rm = TRUE)
      valueBox(
        value = format(grand_total, digits = 2, nsmall = 0),
        subtitle = "Ecotox-Terrestrial Load (1/6 weight)",
        icon = icon("crow"),
        color = "aqua"
      )
    }
  })
  
  
  #--EnvPers
  output$pest_envpers <- renderValueBox({
    if (!is.null(values$data)) {
      grand_total <- sum(values$data$EnvPers_Load, na.rm = TRUE)
      valueBox(
        value = format(grand_total, digits = 2, nsmall = 0),
        subtitle = HTML("&nbsp;<br>Environmental Persistance Load (1/3 weight)"),
        #subtitle = "Environ Persis Load (1/3 weight)\nBLANK",
        icon = icon("glass-water"),
        color = "yellow"
      )
    }
  })
  
  output$pest_humhea <- renderValueBox({
    if (!is.null(values$data)) {
      total_items <- sum(values$data$HumHea_Load, na.rm = TRUE)
      valueBox(
        value = format(total_items, digits = 2, nsmall = 0),
        subtitle = HTML("&nbsp;<br>Human Health Load (1/3 weight)"),
        #subtitle = "Human Health Load (1/3 weight)",
        icon = icon("person-breastfeeding"),
        color = "orange"
      )
    }
  })
  
  output$pest_costs <- renderValueBox({
    if (!is.null(values$data)) {
      total_costs <- round(sum(values$data$Total_SocietalCosts, na.rm = TRUE), 2)
      valueBox(
        value = paste(total_costs, "€/ha"),
        subtitle = "Total Societal Costs of Package Per Hectare",
        icon = icon("coins"),
        color = "green"
        #color = "red"
      )
    }
  })
  
  output$download_pest_table <- downloadHandler(
    filename = function() {
      paste0("pesticide_load_table_", Sys.Date(), ".tsv")
    },
    content = function(file) {
      if (!is.null(values$data)) {
        # Get the full data with all calculations
        export_data <- values$data
        
        # Filter to only show rows with compounds selected
        export_data <- export_data[export_data$Compound != "" &
                                     !is.na(export_data$Compound), ]
        
        # Round numeric columns for cleaner export
        export_data <- export_data %>%
          mutate(across(where(is.numeric), ~ round(.x, 3)))
        
        write.table(
          export_data,
          file,
          sep = "\t",
          row.names = FALSE,
          col.names = TRUE,
          quote = FALSE
        )
      }
    }
  )
  
}

# run app -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
