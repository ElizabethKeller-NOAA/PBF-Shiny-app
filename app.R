##################################
# R Shiny app to make PBF tables #
##################################

# load packages
library(data.table)
library(shiny)
library(bslib)
library(writexl) 
library(DT) 
library(flextable)

# read in data
PBFs <- read.csv("data/All PBFs - Stressor Categories.csv") 

# data wrangling
PBFs$Species_full <- ifelse(
  is.na(PBFs$ESU_DPS) | PBFs$ESU_DPS == "",
  PBFs$Species,
  paste0(PBFs$Species, " – ", PBFs$ESU_DPS)
)
PBFs <- PBFs[order(PBFs$Species_full), ]

# Define Stressors globally
Stressors <- as.character(colnames(PBFs)[15:length(PBFs)])

# UI Definition ---

ui <- page_fillable(
  
  tags$head(
    tags$style(HTML("
      #checkSpecies .checkbox, #checkSpecies_init .checkbox, 
      #checkStressors .checkbox, #checkStressors_init .checkbox {
        display: block;
        margin-right: 0px;
      }
      .wide-layout-species-card #checkSpecies_init .checkbox {
          white-space: nowrap; 
      }
      .wide-layout-species-card .card-body {
        max-height: 400px;
        overflow-y: auto;
        overflow-x: hidden; 
      }
      .scrollable-card .card-body {
        max-height: 400px;
        overflow-y: auto;
        overflow-x: hidden;
      }
    "))
  ),
  
  # 1. INITIAL STATE
  conditionalPanel(
    condition = "input.preview == 0",
    
    layout_columns(
      col_widths = c(6, 3, 3), 
      
      card(
        class = "wide-layout-species-card",
        card_header("Critical Habitats"),
        p("Choose the species whose critical habitat may be affected by the action."),
        checkboxGroupInput("checkSpecies_init", label = NULL, choices = unique(PBFs$Species_full)),
      ),
      
      layout_columns(
        col_widths = 12, 
        card(
          card_header("Table Organization"),
          checkboxInput("BySpecies", label = "Create one table of all PBFs by species"),
          checkboxInput("ByStressors", label = "Create tables of PBFs per stressor/category"),
          # ADDED: New Checkbox Option
          checkboxInput("ByOthers", label = "Create table of PBFs outside selected stressors/categories"),
        ),
        
        card(
          card_header("Table Preview"),
          actionButton("preview", label = "View Table(s) Now"),
        )
      ),
      
      card(
        card_header("Downloads"),
        downloadButton("downloadxlsx", label = "Download Table(s) as xlsx"),
        downloadButton("downloaddocx", label = "Download Table(s) as docx"),
      )
    ), 
    
    conditionalPanel(
      condition = "input.ByStressors == true",
      layout_columns(
        card(
          class = "scrollable-card",
          card_header("Select Stressors/Categories"),
          checkboxGroupInput(
            "checkStressors_init",
            label = NULL,
            choices = Stressors
          )
        )
      ))
  ), 
  
  # 2. ACTIVE STATE
  conditionalPanel(
    condition = "input.preview > 0",
    
    layout_sidebar(
      fillable = TRUE, 
      
      sidebar = sidebar(
        width = 400, 
        position = "left",
        
        card(
          card_header("Downloads"),
          downloadButton("downloadxlsx_sidebar", label = "Download Table(s) as xlsx"),
          downloadButton("downloaddocx_sidebar", label = "Download Table(s) as docx"),
        ),
        
        conditionalPanel(
          condition = "input.ByStressors == true",
          card(
            class = "scrollable-card",
            card_header("Select Stressors/Categories"),
            checkboxGroupInput("checkStressors", label = NULL, choices = Stressors)
          )
        ),
        
        card(
          class = "scrollable-card",
          card_header("Critical Habitats"),
          checkboxGroupInput("checkSpecies", label = NULL, choices = unique(PBFs$Species_full)),
        )
      ),
      
      card(
        full_screen = TRUE, 
        height = "100%", 
        card_header("PBF Table(s)"),
        uiOutput("main_tabs") 
      )
    )
  ) 
)

# Server Logic ----

server <- function(input, output, session) { 
  
  base_filtered_data <- reactive({
    spp_selection <- if(input$preview == 0) input$checkSpecies_init else input$checkSpecies
    if (is.null(spp_selection)) return(PBFs[0, ]) 
    PBFs[PBFs$Species_full %in% spp_selection, ]
  })
  
  observeEvent(input$preview, {
    updateCheckboxGroupInput(session, "checkSpecies", selected = input$checkSpecies_init)
    updateCheckboxGroupInput(session, "checkStressors", selected = input$checkStressors_init)
  }, ignoreInit = TRUE)
  
  generate_stressor_data <- function(stressor_name) {
    data_to_filter <- base_filtered_data() 
    req(stressor_name %in% colnames(data_to_filter))
    filter_condition <- (data_to_filter[[stressor_name]] == 1) & (!is.na(data_to_filter[[stressor_name]]))
    data_to_filter[filter_condition, c("Species_full", "PBF_category", "PBF")]
  }
  
  # ADDED: New function to generate "Outside Selection" data
  generate_other_pbfs_data <- function() {
    data_to_filter <- base_filtered_data()
    current_stressors <- if(input$preview == 0) input$checkStressors_init else input$checkStressors
    
    # If no stressors are selected, then ALL PBFs are technically "outside"
    if (is.null(current_stressors) || length(current_stressors) == 0) {
      return(data_to_filter[, c("Species_full", "PBF_category", "PBF")])
    }
    
    # Logic: Row sum of selected columns is 0 (None of the selected stressors are '1')
    selected_cols <- data_to_filter[, current_stressors, drop = FALSE]
    selected_cols[is.na(selected_cols)] <- 0
    is_outside <- rowSums(selected_cols == 1) == 0
    
    data_to_filter[is_outside, c("Species_full", "PBF_category", "PBF")]
  }
  
  # Dynamic Tab Rendering
  output$main_tabs <- renderUI({
    req(input$BySpecies | input$ByStressors | input$ByOthers)
    
    tab_list <- list()
    
    if (isTRUE(input$BySpecies)) {
      tab_list <- append(tab_list, list(
        nav_panel(title = "Combined Species Data", value = "tab_species",
                  tags$div(style = "height: 100%;", DT::dataTableOutput("PBFtable_BySpecies", height = "100%")))
      ))
    } 
    
    if (isTRUE(input$ByStressors) && !is.null(input$checkStressors)) {
      stressor_tabs <- lapply(input$checkStressors, function(stressor_name) {
        output_id <- paste0("table_", gsub("[^[:alnum:]]", "_", stressor_name))
        output[[output_id]] <- DT::renderDataTable({
          DT::datatable(generate_stressor_data(stressor_name),
                        caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; font-size: 1.2em;', stressor_name),
                        rownames = FALSE, options = list(pageLength = -1, dom = 't'))
        })
        nav_panel(title = stressor_name, value = paste0("tab_", output_id),
                  tags$div(style = "height: 100%;", DT::dataTableOutput(output_id, height = "100%")))
      })
      tab_list <- append(tab_list, stressor_tabs)
    }
    
    # ADDED: Render the "Others" tab
    if (isTRUE(input$ByOthers)) {
      output$PBFtable_Others <- DT::renderDataTable({
        DT::datatable(generate_other_pbfs_data(),
                      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; font-size: 1.2em;', "PBFs Outside Selection"),
                      rownames = FALSE, options = list(pageLength = -1, dom = 't'))
      })
      tab_list <- append(tab_list, list(
        nav_panel(title = "Outside Selection", value = "tab_others",
                  tags$div(style = "height: 100%;", DT::dataTableOutput("PBFtable_Others", height = "100%")))
      ))
    }
    
    if (length(tab_list) > 0) return(navset_card_tab(!!!tab_list))
    return(NULL)
  })
  
  output$PBFtable_BySpecies <- DT::renderDataTable({
    data <- base_filtered_data()[, c("Species_full", "PBF_category", "PBF")]
    DT::datatable(data, rownames = FALSE, options = list(pageLength = -1, dom = 't'))
  })
  
  # Download XLSX
  download_logic_xlsx <- function(file) {
    output_list <- list()
    if (isTRUE(input$BySpecies)) {
      data_species <- base_filtered_data() 
      if (nrow(data_species) > 0) output_list[["Combined_PBFs"]] <- data_species[, c("Species_full","PBF_category", "PBF")]
    }
    
    current_stressors <- if(input$preview == 0) input$checkStressors_init else input$checkStressors
    if (isTRUE(input$ByStressors) && !is.null(current_stressors)) {
      for (stressor_name in current_stressors) {
        data_stressor <- generate_stressor_data(stressor_name)
        if (nrow(data_stressor) > 0) {
          sheet_name <- substr(gsub("[^[:alnum:]]", "_", stressor_name), 1, 31)
          output_list[[sheet_name]] <- data_stressor
        }
      }
    }
    
    # ADDED: Include "Others" in XLSX
    if (isTRUE(input$ByOthers)) {
      data_others <- generate_other_pbfs_data()
      if (nrow(data_others) > 0) output_list[["Outside_Selection"]] <- data_others
    }
    
    if (length(output_list) > 0) writexl::write_xlsx(output_list, path = file)
  }
  
  output$downloadxlsx <- output$downloadxlsx_sidebar <- downloadHandler(
    filename = function() { paste("PBF_Tables-", Sys.Date(), ".xlsx", sep="") },
    content = download_logic_xlsx
  )
  
  # Download DOCX
  output$downloaddocx <- output$downloaddocx_sidebar <- downloadHandler(
    filename = function() { paste("PBF_Report-", Sys.Date(), ".docx", sep="") },
    content = function(file) {
      final_list <- list()
      if(isTRUE(input$BySpecies)) final_list[["Species Summary"]] <- base_filtered_data()
      
      current_stressors <- if(input$preview == 0) input$checkStressors_init else input$checkStressors
      if (isTRUE(input$ByStressors) && !is.null(current_stressors)) {
        for (st in current_stressors) {
          st_data <- generate_stressor_data(st)
          if (nrow(st_data) > 0) final_list[[st]] <- st_data
        }
      }
      
      # ADDED: Include "Others" in DOCX
      if (isTRUE(input$ByOthers)) {
        data_others <- generate_other_pbfs_data()
        if (nrow(data_others) > 0) final_list[["Outside Selection"]] <- data_others
      }
      
      params <- list(data_list = final_list)
      rmarkdown::render("report_template.Rmd", 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
}

shinyApp(ui = ui, server = server)