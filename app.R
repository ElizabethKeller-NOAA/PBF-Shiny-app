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
ui_values$Category <- ifelse(is.na(ui_values$Category) | trimws(ui_values$Category) == "", 
                             "General Parameters", 
                             trimws(ui_values$Category))
Stressors <- as.character(ui_values$Data_sheet_values)

# Load the Basin lookup file 
basin_values <- read.csv("data/All PBFs - Basins.csv")
basin_values <- basin_values[!is.na(basin_values$basin) & trimws(basin_values$basin) != "", ]
Unique_Basins <- unique(basin_values$basin)

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
      col_widths = c(3, 3, 3, 3), 
      card(
        card_header("Table Organization"),
        checkboxInput("BySpecies", "Create a table of all PBFs by Species", TRUE),
        checkboxInput("ByStressors", "Create individual tables per Stressor/Category", FALSE),
        checkboxInput("ByOthers", "Include Table of PBFs outside selected Stressors", FALSE)
      ),
      card(
        card_header("Habitat Areas"),
        checkboxGroupInput("checkHabitats_init", label = NULL, 
                           choices = Habitat_Types, selected = Habitat_Types)
      ),
      uiOutput("basins_init_ui"),
      layout_columns(
        col_widths = 12,
        card(
          card_header("Preview"),
          actionButton("preview", "View Table(s) Now", class = "btn-primary w-100 h-100", style = "font-size: 1.1rem;")
        )
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      uiOutput("species_init_ui"),
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
        card(card_header("Basins/Oceans"), checkboxGroupInput("checkBasins_sidebar", label = NULL, choices = Unique_Basins)),
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
  
  # ANCHOR: This storage locker cleanly holds selection values chosen on Page 1 
  # to break up feedback loops across pages completely.
  locked_selections <- reactiveValues(
    species = NULL,
    habitats = NULL,
    basins = NULL,
    stressors = NULL
  )
  
  # Aggregator: Gathers Stressors chosen on Page 1
  current_checked_stressors <- reactive({
    unique_cats <- unique(ui_values$Category)
    chunk_ids <- paste0("checkStressors_init_chunk_", gsub("[^[:alnum:]]", "_", unique_cats))
    checked_list <- lapply(chunk_ids, function(id) input[[id]])
    return(unlist(checked_list))
  })
  
  # Bulk action to Select All visible species inside the reactive card
  observeEvent(input$all_spp, {
    updateCheckboxGroupInput(session, "checkSpecies_init", selected = available_species())
  })
  
  target_pbf_col <- reactive({
    "PBF"
  })
  
  # Dynamic Helper: Computes which species match active metadata filters before clicking Preview
  available_species <- reactive({
    df <- PBFs
    
    # 1. Filter choices by Habitat Areas (OR logic)
    hab_selection <- input$checkHabitats_init
    if (!is.null(hab_selection) && length(hab_selection) > 0) {
      habitat_logical <- rowSums(df[, hab_selection, drop = FALSE] == 1, na.rm = TRUE) > 0
      df <- df[habitat_logical, ]
    } else {
      return(character(0))
    }
    
    # 2. Filter choices by Basins (OR logic via comma-separated string searching with word boundaries)
    basin_sel <- input$checkBasins_init
    if (!is.null(basin_sel) && length(basin_sel) > 0) {
      basin_match <- sapply(df$basin_ocean, function(row_val) {
        if (is.na(row_val) || row_val == "") return(FALSE)
        any(sapply(basin_sel, function(b) grepl(paste0("\\b", b, "\\b"), row_val, ignore.case = TRUE)))
      })
      df <- df[basin_match, ]
    }
    
    # 3. Strip out hardcoded exclusion criteria rows globally
    df <- df[is.na(df$Area_Designated_Yes_No) | df$Area_Designated_Yes_No != 0, ]
    
    return(unique(df$Species))
  })
  
  # --- Dynamic UI: Renders the Critical Habitat selection based on Basin filters
  output$species_init_ui <- renderUI({
    choices_list <- available_species()
    
    card(
      class = "selection-card", 
      card_header("Choose Critical Habitats"),
      actionLink("all_spp", "Select All Visible", class = "bulk-action-link"),
      tags$div(
        style = "overflow-y: auto; height: 100%;",
        checkboxGroupInput("checkSpecies_init", label = NULL, 
                           choices = choices_list, 
                           selected = input$checkSpecies_init)
      )
    )
  })
  
  # LOCK IN TRANSITION SNAPS: Captures state cleanly only when button is pressed
  observeEvent(input$preview, {
    locked_selections$species <- input$checkSpecies_init
    locked_selections$habitats <- input$checkHabitats_init
    locked_selections$basins <- input$checkBasins_init
    locked_selections$stressors <- current_checked_stressors()
    
    # Push starting states cleanly to sidebar targets without binding updaters backwards
    updateCheckboxGroupInput(session, "checkSpecies", selected = locked_selections$species, choices = unique(PBFs$Species))
    updateCheckboxGroupInput(session, "checkHabitats", selected = locked_selections$habitats)
    updateCheckboxGroupInput(session, "checkBasins_sidebar", selected = locked_selections$basins)
    updateCheckboxGroupInput(session, "checkStressors", selected = locked_selections$stressors)
  }, ignoreInit = TRUE)
  
  # Base Dataset Engine (Handles what populates final data views)
  base_filtered_data <- reactive({
    # Decide cleanly whether we evaluate the frozen snapshot state or active post-preview sidebar shifts
    spp_selection <- if(input$preview == 0) locked_selections$species else input$checkSpecies
    hab_selection <- if(input$preview == 0) locked_selections$habitats else input$checkHabitats
    basin_sel <- if(input$preview == 0) locked_selections$basins else input$checkBasins_sidebar
    col <- target_pbf_col()
    
    req(spp_selection, length(spp_selection) > 0)
    
    df <- PBFs[PBFs$Species %in% spp_selection, ]
    
    # 1. Filter by Habitat Types (OR logic)
    if (!is.null(hab_selection) && length(hab_selection) > 0) {
      habitat_logical <- rowSums(df[, hab_selection, drop = FALSE] == 1, na.rm = TRUE) > 0
      df <- df[habitat_logical, ]
    } else {
      return(df[0, ])
    }
    
    # 2. Filter by Basins & Oceans via partial word matches
    if (!is.null(basin_sel) && length(basin_sel) > 0) {
      basin_match <- sapply(df$basin_ocean, function(row_val) {
        if (is.na(row_val) || row_val == "") return(FALSE)
        any(sapply(basin_sel, function(b) grepl(paste0("\\b", b, "\\b"), row_val, ignore.case = TRUE)))
      })
      df <- df[basin_match, ]
    }
    
    # 3. Clean empty text rows
    df <- df[!is.na(df[[col]]) & trimws(df[[col]]) != "", ]
    
    # 4. Drop Designated exclusions
    df <- df[is.na(df$Area_Designated_Yes_No) | df$Area_Designated_Yes_No != 0, ]
    
    return(df)
  })
  
  # --- UI Builders ---
  output$basins_init_ui <- renderUI({
    card(
      card_header("Basins/Oceans"),
      checkboxGroupInput(
        inputId = "checkBasins_init",
        label = NULL,
        choices = Unique_Basins,
        selected = input$checkBasins_init
      )
    )
  })
  
  output$stressors_init_ui <- renderUI({
    req(input$ByStressors)
    unique_cats <- unique(ui_values$Category)
    
    checkbox_blocks <- lapply(unique_cats, function(cat_name) {
      sub_df <- ui_values[ui_values$Category == cat_name, ]
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
    st_sel <- if(input$preview == 0) locked_selections$stressors else input$checkStressors
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
    
    st_active <- if(input$preview == 0) locked_selections$stressors else input$checkStressors
    if (input$ByStressors && !is.null(st_active)) {
      st_tabs <- lapply(st_active, function(st) {
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
      
      st_active <- if(input$preview == 0) locked_selections$stressors else input$checkStressors
      if (input$ByStressors && !is.null(st_active)) {
        for (st in st_active) out[[substr(st, 1, 31)]] <- generate_stressor_data(st)
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
      
      st_active <- if(input$preview == 0) locked_selections$stressors else input$checkStressors
      if (input$ByStressors && !is.null(st_active)) {
        for (st in st_active) final_list[[st]] <- generate_stressor_data(st)
      }
      if (input$ByOthers) final_list[["Outside Selection"]] <- generate_other_pbfs_data()
      rmarkdown::render("report_template.Rmd", output_file = file, params = list(data_list = final_list, pbf_column = col))
    }
  )
}

shinyApp(ui, server)