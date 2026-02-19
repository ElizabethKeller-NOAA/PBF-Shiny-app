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
PBFs <- read.csv("data/All PBFs - Working Copy.csv") 

PBFs$Species <- ifelse(
  is.na(PBFs$ESU_DPS) | PBFs$ESU_DPS == "",
  PBFs$Species_Name,
  paste0(PBFs$Species_Name, " – ", PBFs$ESU_DPS)
)
PBFs <- PBFs[order(PBFs$Species_Name), ]
Stressors <- as.character(colnames(PBFs)[12:(length(PBFs)-1)])

# UI Definition ---
ui <- page_fillable(
  tags$head(
    tags$style(HTML("
      /* CSS for INITIAL UI only (using the preview condition) */
      [data-display-if='input.preview == 0'] .selection-card {
        height: 600px !important;
      }
      
      /* CSS for ACTIVE UI only (to fix the white space) */
      [data-display-if='input.preview > 0'] .card {
        height: 100% !important;
      }
      
      /* Fix for the wrapping in initial UI habitat list */
      .shiny-input-checkboxgroup .checkbox label {
        display: flex !important;
        align-items: flex-start;
      }
      .shiny-input-checkboxgroup .checkbox label span {
        white-space: normal !important;
        margin-left: 10px;
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
        checkboxInput("BySpecies", "Create a table of all PBFs by Species", TRUE),
        checkboxInput("ByStressors", "Create individual tables per Stressor/Category", FALSE),
        checkboxInput("ByOthers", "Include Table of PBFs outside selected Stressors", FALSE)
      ),
      card(
        card_header("2. PBF Text Detail"),
        radioButtons("pbf_length_init", NULL,
                     choices = list("Full Designation Text" = "PBF", "Summary PBFs" = "Shorter_PBFs"),
                     selected = "PBF")
      ),
      card(
        card_header("3. Preview"),
        actionButton("preview", "View Table(s) Now", class = "btn-primary w-100 h-100")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(class = "selection-card", card_header("Choose Critical Habitats"),
           checkboxGroupInput("checkSpecies_init", label = NULL, choices = unique(PBFs$Species))),
      conditionalPanel(
        condition = "input.ByStressors == true",
        card(class = "selection-card", card_header("Select Stressors/Categories"),
             checkboxGroupInput("checkStressors_init", label = NULL, choices = Stressors))
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
        card(card_header("Downloads"),
             downloadButton("downloadxlsx_sidebar", "XLSX", class = "btn-outline-secondary w-100 mb-2"),
             downloadButton("downloaddocx_sidebar", "DOCX", class = "btn-outline-secondary w-100")),
        card(card_header("Text Detail"),
             radioButtons("pbf_length", NULL, choices = list("Full" = "PBF", "Summary" = "Shorter_PBFs"), selected = "PBF")),
        conditionalPanel(condition = "input.ByStressors == true",
                         card(card_header("Stressors"), checkboxGroupInput("checkStressors", NULL, choices = Stressors))),
        card(card_header("Critical Habitats"), checkboxGroupInput("checkSpecies", NULL, choices = unique(PBFs$Species)))
      ),
      card(full_screen = TRUE, uiOutput("main_tabs"))
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
    PBFs[PBFs$Species %in% spp_selection, ]
  })
  
  observeEvent(input$preview, {
    updateCheckboxGroupInput(session, "checkSpecies", selected = input$checkSpecies_init)
    updateCheckboxGroupInput(session, "checkStressors", selected = input$checkStressors_init)
    updateRadioButtons(session, "pbf_length", selected = input$pbf_length_init)
  }, ignoreInit = TRUE)
  
  # Enhanced helper to prevent the 'PBF column not found' warning
  render_my_datatable <- function(data_func) {
    DT::renderDataTable({
      df <- data_func()
      col <- target_pbf_col()
      # Only proceed if the column exists in the dataframe provided
      req(col %in% colnames(df))
      df
    }, server = TRUE, rownames = FALSE, 
    options = list(pageLength = -1, dom = 't', scrollY = "75vh", scrollCollapse = TRUE))
  }
  
  generate_stressor_data <- function(st) {
    col <- target_pbf_col()
    df <- base_filtered_data()
    req(st %in% colnames(df), col %in% colnames(df))
    df[df[[st]] == 1 & !is.na(df[[st]]), c("Species", "PBF_category", col)]
  }
  
  generate_other_pbfs_data <- function() {
    col <- target_pbf_col()
    df <- base_filtered_data()
    req(col %in% colnames(df))
    st_sel <- if(input$preview == 0) input$checkStressors_init else input$checkStressors
    if (is.null(st_sel) || length(st_sel) == 0) return(df[, c("Species", "PBF_category", col)])
    sel_cols <- df[, st_sel, drop = FALSE]
    sel_cols[is.na(sel_cols)] <- 0
    df[rowSums(sel_cols == 1) == 0, c("Species", "PBF_category", col)]
  }
  
  output$main_tabs <- renderUI({
    req(input$preview > 0)
    req(input$BySpecies | input$ByStressors | input$ByOthers)
    tabs <- list()
    if (input$BySpecies) tabs <- append(tabs, list(nav_panel("Combined Species", DT::dataTableOutput("PBFtable_BySpecies"))))
    if (input$ByStressors && !is.null(input$checkStressors)) {
      st_tabs <- lapply(input$checkStressors, function(st) {
        id <- paste0("table_", gsub("[^[:alnum:]]", "_", st))
        output[[id]] <- render_my_datatable(function() generate_stressor_data(st))
        nav_panel(st, DT::dataTableOutput(id))
      })
      tabs <- append(tabs, st_tabs)
    }
    if (input$ByOthers) {
      output$PBFtable_Others <- render_my_datatable(generate_other_pbfs_data)
      tabs <- append(tabs, list(nav_panel("Outside Selection", DT::dataTableOutput("PBFtable_Others"))))
    }
    navset_card_tab(!!!tabs)
  })
  
  output$PBFtable_BySpecies <- render_my_datatable(function() {
    col <- target_pbf_col()
    df <- base_filtered_data()
    df[, c("Species", "PBF_category", col)]
  })
  
  # [Download handlers remain the same...]
  output$downloadxlsx_sidebar <- downloadHandler(
    filename = function() { paste0("PBF_Tables_", Sys.Date(), ".xlsx") },
    content = function(file) {
      out <- list()
      col <- target_pbf_col()
      if (input$BySpecies) out[["Combined"]] <- base_filtered_data()[, c("Species","PBF_category", col)]
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
      if(input$BySpecies) final_list[["Species Summary"]] <- base_filtered_data()[, c("Species", "PBF_category", col)]
      if (input$ByStressors && !is.null(input$checkStressors)) {
        for (st in input$checkStressors) final_list[[st]] <- generate_stressor_data(st)
      }
      if (input$ByOthers) final_list[["Outside Selection"]] <- generate_other_pbfs_data()
      rmarkdown::render("report_template.Rmd", output_file = file, params = list(data_list = final_list, pbf_column = col))
    }
  )
}

shinyApp(ui, server)