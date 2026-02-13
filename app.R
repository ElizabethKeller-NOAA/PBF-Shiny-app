##################################
# R Shiny app to make PBF tables #
##################################

library(data.table)
library(shiny)
library(bslib)
library(writexl) 
library(DT) 
library(flextable)

# 0. Load and Wrangle Data
PBFs <- read.csv("data/All PBFs - Stressor Categories.csv") 

PBFs$Species_full <- ifelse(
  is.na(PBFs$ESU_DPS) | PBFs$ESU_DPS == "",
  PBFs$Species,
  paste0(PBFs$Species, " – ", PBFs$ESU_DPS)
)
PBFs <- PBFs[order(PBFs$Species_full), ]
Stressors <- as.character(colnames(PBFs)[15:length(PBFs)])

# UI Definition ---
ui <- page_fillable(
  tags$head(
    tags$style(HTML("
      #checkSpecies .checkbox, #checkSpecies_init .checkbox, 
      #checkStressors .checkbox, #checkStressors_init .checkbox { display: block; margin-right: 0px; }
      .wide-layout-species-card #checkSpecies_init .checkbox { white-space: nowrap; }
      .wide-layout-species-card .card-body, .scrollable-card .card-body {
        max-height: 400px; overflow-y: auto; overflow-x: hidden; 
      }
    "))
  ),
  
  # =========================================================
  # 1. INITIAL STATE (Input Cards Fill Space)
  # =========================================================
  conditionalPanel(
    condition = "input.preview == 0",
    layout_columns(
      col_widths = c(8, 4), # Adjusted widths since Downloads are gone
      
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
          checkboxInput("ByOthers", label = "Create table of PBFs outside selected stressors"),
        ),
        card(
          card_header("Table Preview"),
          actionButton("preview", label = "View Table(s) Now", class = "btn-primary"),
        )
      )
    ), 
    
    conditionalPanel(
      condition = "input.ByStressors == true",
      layout_columns(
        card(
          class = "scrollable-card",
          card_header("Select Stressors/Categories"),
          checkboxGroupInput("checkStressors_init", label = NULL, choices = Stressors)
        )
      ))
  ), 
  
  # =========================================================
  # 2. ACTIVE STATE (Sidebar + Wide Table Area)
  # =========================================================
  conditionalPanel(
    condition = "input.preview > 0",
    layout_sidebar(
      fillable = TRUE, 
      sidebar = sidebar(
        width = 400, 
        
        # DOWNLOADS ONLY APPEAR HERE NOW
        card(
          card_header("Downloads"),
          downloadButton("downloadxlsx_sidebar", label = "Download XLSX", class = "btn-outline-secondary"),
          downloadButton("downloaddocx_sidebar", label = "Download DOCX", class = "btn-outline-secondary"),
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
        full_screen = TRUE, height = "100%", 
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
  
  generate_other_pbfs_data <- function() {
    data_to_filter <- base_filtered_data()
    current_stressors <- if(input$preview == 0) input$checkStressors_init else input$checkStressors
    if (is.null(current_stressors) || length(current_stressors) == 0) return(data_to_filter[, c("Species_full", "PBF_category", "PBF")])
    
    selected_cols <- data_to_filter[, current_stressors, drop = FALSE]
    selected_cols[is.na(selected_cols)] <- 0
    is_outside <- rowSums(selected_cols == 1) == 0
    data_to_filter[is_outside, c("Species_full", "PBF_category", "PBF")]
  }
  
  output$main_tabs <- renderUI({
    req(input$BySpecies | input$ByStressors | input$ByOthers)
    tab_list <- list()
    
    if (isTRUE(input$BySpecies)) {
      tab_list <- append(tab_list, list(nav_panel(title = "Combined Species", DT::dataTableOutput("PBFtable_BySpecies"))))
    } 
    
    if (isTRUE(input$ByStressors) && !is.null(input$checkStressors)) {
      stressor_tabs <- lapply(input$checkStressors, function(st) {
        id <- paste0("table_", gsub("[^[:alnum:]]", "_", st))
        output[[id]] <- DT::renderDataTable({
          DT::datatable(generate_stressor_data(st), rownames = FALSE, options = list(pageLength = -1, dom = 't'))
        })
        nav_panel(title = st, DT::dataTableOutput(id))
      })
      tab_list <- append(tab_list, stressor_tabs)
    }
    
    if (isTRUE(input$ByOthers)) {
      output$PBFtable_Others <- DT::renderDataTable({
        DT::datatable(generate_other_pbfs_data(), rownames = FALSE, options = list(pageLength = -1, dom = 't'))
      })
      tab_list <- append(tab_list, list(nav_panel(title = "Outside Selection", DT::dataTableOutput("PBFtable_Others"))))
    }
    
    if (length(tab_list) > 0) return(navset_card_tab(!!!tab_list))
  })
  
  output$PBFtable_BySpecies <- DT::renderDataTable({
    DT::datatable(base_filtered_data()[, c("Species_full", "PBF_category", "PBF")], rownames = FALSE, options = list(pageLength = -1, dom = 't'))
  })
  
  # Download Handler Logic
  output$downloadxlsx_sidebar <- downloadHandler(
    filename = function() { paste0("PBF_Tables_", Sys.Date(), ".xlsx") },
    content = function(file) {
      out <- list()
      if (input$BySpecies) out[["Combined"]] <- base_filtered_data()[, c("Species_full","PBF_category", "PBF")]
      if (input$ByStressors && !is.null(input$checkStressors)) {
        for (st in input$checkStressors) out[[substr(st, 1, 31)]] <- generate_stressor_data(st)
      }
      if (input$ByOthers) out[["Outside_Selection"]] <- generate_other_pbfs_data()
      writexl::write_xlsx(out, path = file)
    }
  )
  
  output$downloaddocx_sidebar <- downloadHandler(
    filename = function() { paste0("PBF_Report_", Sys.Date(), ".docx") },
    content = function(file) {
      final_list <- list()
      if(input$BySpecies) final_list[["Species Summary"]] <- base_filtered_data()
      if (input$ByStressors && !is.null(input$checkStressors)) {
        for (st in input$checkStressors) final_list[[st]] <- generate_stressor_data(st)
      }
      if (input$ByOthers) final_list[["Outside Selection"]] <- generate_other_pbfs_data()
      
      rmarkdown::render("report_template.Rmd", output_file = file, params = list(data_list = final_list))
    }
  )
}

shinyApp(ui, server)