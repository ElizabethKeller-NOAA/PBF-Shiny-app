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

# Load the stressor/category UI lookup file
ui_values <- read.csv("data/All PBFs - UI values.csv")
# Extract stressor columns safely from the lookup sheet
# Handle grouping names nicely
ui_values$Category <- ifelse(is.na(ui_values$Category) | trimws(ui_values$Category) == "", 
                             "General Parameters", 
                             trimws(ui_values$Category))
# Keep ui_df as a dataframe for our loop, but we still need a global vector 
# of all raw column names so your server's download/filtering logic doesn't break:
Stressors <- as.character(ui_values$Data_sheet_values)

PBFs$Species <- ifelse(
  is.na(PBFs$ESU_DPS) | PBFs$ESU_DPS == "",
  PBFs$Species_Name,
  paste0(PBFs$Species_Name, " – ", PBFs$ESU_DPS)
)
PBFs <- PBFs[order(PBFs$Species), ]

Habitat_Types <- c("Freshwater", "Estuarine", "Marine", "Land")

# UI Definition ---
ui <- page_fillable(
  tags$head(
    tags$style(HTML("
      [data-display-if='input.preview == 0'] .selection-card { height: 600px !important; }
      [data-display-if='input.preview > 0'] .card { height: 100% !important; }
      .shiny-input-checkboxgroup .checkbox label { display: flex !important; align-items: flex-start; }
      .shiny-input-checkboxgroup .checkbox label span { white-space: normal !important; margin-left: 10px; }
      .bulk-action-link { font-size: 0.8rem; text-decoration: none; cursor: pointer; color: #007bff; margin-bottom: 5px; display: block; }
    "))
  ),
  
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
        card_header("2. Habitat Areas"),
        checkboxGroupInput("checkHabitats_init", label = NULL, 
                           choices = Habitat_Types, selected = Habitat_Types)
      ),
      layout_columns(
        col_widths = 12,
        card(
          card_header("3. PBF Text Detail"),
          radioButtons("pbf_length_init", NULL,
                       choices = list("Full Designation Text" = "PBF", "Summary PBFs" = "Shorter_PBF"),
                       selected = "PBF")
        ),
        card(
          card_header("4. Preview"),
          actionButton("preview", "View Table(s) Now", class = "btn-primary w-100 h-100", style = "font-size: 1.1rem;")
        )
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "selection-card", 
        card_header("Choose Critical Habitats"),
        actionLink("all_spp", "Select All", class = "bulk-action-link"),
        checkboxGroupInput("checkSpecies_init", label = NULL, choices = unique(PBFs$Species))
      ),
      # Clean structural injection area
      uiOutput("stressors_init_ui")
    )
  ),
  
  conditionalPanel(
    condition = "input.preview > 0",
    layout_sidebar(
      fillable = TRUE, 
      sidebar = sidebar(
        width = 400,
        card(card_header("Downloads"),
             downloadButton("downloadxlsx_sidebar", "XLSX", class = "btn-outline-secondary w-100 mb-2"),
             downloadButton("downloaddocx_sidebar", "DOCX", class = "btn-outline-secondary w-100")),
        card(card_header("Habitat Areas"),
             checkboxGroupInput("checkHabitats", NULL, choices = Habitat_Types, selected = Habitat_Types)),
        card(card_header("Text Detail"),
             radioButtons("pbf_length", NULL, choices = list("Full" = "PBF", "Summary" = "Shorter_PBF"), selected = "PBF")),
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
  
  # Dynamically gather inputs from all stressor category chunks back into a single vector
  current_checked_stressors <- reactive({
    if (input$preview == 0) {
      unique_cats <- unique(ui_values$Category)
      chunk_ids <- paste0("checkStressors_init_chunk_", gsub("[^[:alnum:]]", "_", unique_cats))
      
      checked_list <- lapply(chunk_ids, function(id) input[[id]])
      return(unlist(checked_list))
    } else {
      return(input$checkStressors)
    }
  })
  
  observeEvent(input$all_spp, {
    updateCheckboxGroupInput(session, "checkSpecies_init", selected = unique(PBFs$Species))
  })
  
  target_pbf_col <- reactive({
    val <- if(input$preview == 0) input$pbf_length_init else input$pbf_length
    req(val)
    val
  })
  
  base_filtered_data <- reactive({
    spp_selection <- if(input$preview == 0) input$checkSpecies_init else input$checkSpecies
    st_sel <- current_checked_stressors()
    hab_selection <- if(input$preview == 0) input$checkHabitats_init else input$checkHabitats
    col <- target_pbf_col()
    
    if (is.null(spp_selection)) return(PBFs[0, ]) 
    
    df <- PBFs[PBFs$Species %in% spp_selection, ]
    
    # Filter by Habitat Types (OR logic)
    if (!is.null(hab_selection) && length(hab_selection) > 0) {
      habitat_logical <- rowSums(df[, hab_selection, drop = FALSE] == 1, na.rm = TRUE) > 0
      df <- df[habitat_logical, ]
    } else {
      return(df[0, ])
    }
    
    # Filter out rows where the selected PBF text column is empty or NA
    df <- df[!is.na(df[[col]]) & trimws(df[[col]]) != "", ]
    
    return(df)
  })
  
  # Render the scrolling checkbox blocks with category headers
  output$stressors_init_ui <- renderUI({
    req(input$ByStressors)
    
    unique_cats <- unique(ui_values$Category)
    
    checkbox_blocks <- lapply(unique_cats, function(cat_name) {
      sub_df <- ui_values[ui_values$Category == cat_name, ]
      
      # Pair Display Labels (UI_values) with Data Columns (Data_sheet_values)
      chunk_choices <- setNames(as.character(sub_df$Data_sheet_values), 
                                as.character(sub_df$UI_values))
      
      tagList(
        tags$div(
          style = "margin-top: 15px; margin-bottom: 5px; font-weight: bold; border-bottom: 1px solid #e9ecef; color: #495057;",
          cat_name
        ),
        checkboxGroupInput(
          inputId = paste0("checkStressors_init_chunk_", gsub("[^[:alnum:]]", "_", cat_name)), 
          label = NULL, 
          choices = chunk_choices,
          selected = input$checkStressors
        )
      )
    })
    
    card(
      class = "selection-card", 
      style = "overflow-y: auto;",
      card_header("Select Stressors/Categories"),
      do.call(tagList, checkbox_blocks)
    )
  })
  
  # Handle preview transitions for everything else cleanly
  observeEvent(input$preview, {
    updateCheckboxGroupInput(session, "checkSpecies", selected = input$checkSpecies_init)
    updateCheckboxGroupInput(session, "checkStressors", selected = current_checked_stressors())
    updateCheckboxGroupInput(session, "checkHabitats", selected = input$checkHabitats_init)
    updateRadioButtons(session, "pbf_length", selected = input$pbf_length_init)
  }, ignoreInit = TRUE)
  
  render_my_datatable <- function(data_func) {
    DT::renderDataTable({
      df <- data_func()
      col <- target_pbf_col()
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
    st_sel <- current_checked_stressors()
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