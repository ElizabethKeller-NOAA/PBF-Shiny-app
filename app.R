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
      /* 1. FIX WRAPPING: Allow checkbox text to use the FULL width of the card */
      .shiny-input-checkboxgroup, .form-group {
        width: 100% !important;
        max-width: none !important;
      }
      
      .shiny-input-checkboxgroup .checkbox label {
        display: flex !important;
        align-items: flex-start;
        width: 100% !important;
      }
      
      .shiny-input-checkboxgroup .checkbox label span {
        white-space: normal !important; 
        width: 100%;
        margin-left: 10px;
      }

      /* 2. FIX HEIGHT: Ensure the cards and their bodies stretch */
      .selection-card {
        height: 650px !important;
      }
      
      .selection-card .card-body {
        overflow-y: auto;
      }

      /* 3. TABLE HEIGHT FIX: Force the UI area to fill space */
      .tab-content, .tab-pane {
        height: 100% !important;
      }
      
      /* Hide horizontal scroll on cards */
      .card-body {
        overflow-x: hidden !important;
      }
    "))
  ),
  
  # =========================================================
  # 1. INITIAL STATE
  # =========================================================
  conditionalPanel(
    condition = "input.preview == 0",
    
    layout_columns(
      col_widths = c(5, 4, 3),
      card(
        card_header("1. Table Organization"),
        layout_columns(
          col_widths = 12,
          checkboxInput("BySpecies", "Combine all PBFs by Species", TRUE),
          checkboxInput("ByStressors", "Create individual tables per Stressor", FALSE),
          checkboxInput("ByOthers", "Include PBFs outside selected Stressors", FALSE)
        )
      ),
      card(
        card_header("2. PBF Text Detail"),
        radioButtons("pbf_length_init", NULL,
                     choices = list("Full Designation Text" = "PBF", "Summary PBFs" = "Shorter_PBFs"),
                     selected = "PBF")
      ),
      card(
        card_header("3. Preview"),
        actionButton("preview", "View Table(s) Now", 
                     class = "btn-primary w-100 h-100", 
                     style = "font-size: 1.2rem;")
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "selection-card",
        card_header("Choose Critical Habitats"),
        checkboxGroupInput("checkSpecies_init", label = NULL, choices = unique(PBFs$Species_full))
      ),
      conditionalPanel(
        condition = "input.ByStressors == true",
        card(
          class = "selection-card",
          card_header("Select Stressors/Categories"),
          checkboxGroupInput("checkStressors_init", label = NULL, choices = Stressors)
        )
      )
    )
  ),
  
  # =========================================================
  # 2. ACTIVE STATE
  # =========================================================
  conditionalPanel(
    condition = "input.preview > 0",
    layout_sidebar(
      fillable = TRUE, 
      sidebar = sidebar(
        width = 400,
        card(
          card_header("Downloads"),
          downloadButton("downloadxlsx_sidebar", "XLSX", class = "btn-outline-secondary w-100 mb-2"),
          downloadButton("downloaddocx_sidebar", "DOCX", class = "btn-outline-secondary w-100")
        ),
        card(
          card_header("Text Detail"),
          radioButtons("pbf_length", NULL,
                       choices = list("Full" = "PBF", "Summary" = "Shorter_PBFs"),
                       selected = "PBF")
        ),
        conditionalPanel(
          condition = "input.ByStressors == true",
          card(
            class = "selection-card",
            card_header("Stressors"),
            checkboxGroupInput("checkStressors", NULL, choices = Stressors)
          )
        ),
        card(
          class = "selection-card",
          card_header("Critical Habitats"),
          checkboxGroupInput("checkSpecies", NULL, choices = unique(PBFs$Species_full))
        )
      ),
      # Content Area: Using card_body(fillable = TRUE) to maximize space
      card(
        full_screen = TRUE, 
        card_body(
          fillable = TRUE,
          uiOutput("main_tabs")
        )
      )
    )
  )
)

# Server Logic ----
server <- function(input, output, session) { 
  
  target_pbf_col <- reactive({
    val <- if(input$preview == 0) input$pbf_length_init else input$pbf_length
    req(val)
    val
  })
  
  base_filtered_data <- reactive({
    spp_selection <- if(input$preview == 0) input$checkSpecies_init else input$checkSpecies
    if (is.null(spp_selection)) return(PBFs[0, ]) 
    PBFs[PBFs$Species_full %in% spp_selection, ]
  })
  
  observeEvent(input$preview, {
    updateCheckboxGroupInput(session, "checkSpecies", selected = input$checkSpecies_init)
    updateCheckboxGroupInput(session, "checkStressors", selected = input$checkStressors_init)
    updateRadioButtons(session, "pbf_length", selected = input$pbf_length_init)
  }, ignoreInit = TRUE)
  
  # Logic functions remain identical
  generate_stressor_data <- function(st) {
    col <- target_pbf_col()
    df <- base_filtered_data()
    req(st %in% colnames(df), col %in% colnames(df))
    df[df[[st]] == 1 & !is.na(df[[st]]), c("Species_full", "PBF_category", col)]
  }
  
  generate_other_pbfs_data <- function() {
    col <- target_pbf_col()
    df <- base_filtered_data()
    req(col %in% colnames(df))
    st_sel <- if(input$preview == 0) input$checkStressors_init else input$checkStressors
    if (is.null(st_sel) || length(st_sel) == 0) return(df[, c("Species_full", "PBF_category", col)])
    sel_cols <- df[, st_sel, drop = FALSE]
    sel_cols[is.na(sel_cols)] <- 0
    df[rowSums(sel_cols == 1) == 0, c("Species_full", "PBF_category", col)]
  }
  
  output$main_tabs <- renderUI({
    req(input$preview > 0)
    req(input$BySpecies | input$ByStressors | input$ByOthers)
    
    tabs <- list()
    # Table heights increased to 85vh (85% of viewport height)
    if (input$BySpecies) {
      tabs <- append(tabs, list(nav_panel("Combined Species", DT::dataTableOutput("PBFtable_BySpecies", height = "85vh"))))
    }
    
    if (input$ByStressors && !is.null(input$checkStressors)) {
      st_tabs <- lapply(input$checkStressors, function(st) {
        id <- paste0("table_", gsub("[^[:alnum:]]", "_", st))
        output[[id]] <- DT::renderDataTable({
          generate_stressor_data(st)
        }, server = TRUE, rownames = FALSE, options = list(pageLength = -1, dom = 't', scrollY = "70vh"))
        
        nav_panel(st, DT::dataTableOutput(id, height = "85vh"))
      })
      tabs <- append(tabs, st_tabs)
    }
    
    if (input$ByOthers) {
      output$PBFtable_Others <- DT::renderDataTable({
        generate_other_pbfs_data()
      }, server = TRUE, rownames = FALSE, options = list(pageLength = -1, dom = 't', scrollY = "70vh"))
      
      tabs <- append(tabs, list(nav_panel("Outside Selection", DT::dataTableOutput("PBFtable_Others", height = "85vh"))))
    }
    
    navset_card_tab(!!!tabs)
  })
  
  output$PBFtable_BySpecies <- DT::renderDataTable({
    col <- target_pbf_col()
    df <- base_filtered_data()
    req(col %in% colnames(df))
    df[, c("Species_full", "PBF_category", col)]
  }, server = TRUE, rownames = FALSE, options = list(pageLength = -1, dom = 't', scrollY = "70vh"))
  
  # Downloads... (logic remains the same)
  output$downloadxlsx_sidebar <- downloadHandler(
    filename = function() { paste0("PBF_Tables_", Sys.Date(), ".xlsx") },
    content = function(file) {
      out <- list()
      col <- target_pbf_col()
      if (input$BySpecies) out[["Combined"]] <- base_filtered_data()[, c("Species_full","PBF_category", col)]
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
      col <- target_pbf_col()
      if(input$BySpecies) final_list[["Species Summary"]] <- base_filtered_data()[, c("Species_full", "PBF_category", col)]
      if (input$ByStressors && !is.null(input$checkStressors)) {
        for (st in input$checkStressors) final_list[[st]] <- generate_stressor_data(st)
      }
      if (input$ByOthers) final_list[["Outside Selection"]] <- generate_other_pbfs_data()
      rmarkdown::render("report_template.Rmd", output_file = file, params = list(data_list = final_list, pbf_column = col))
    }
  )
}

shinyApp(ui, server)