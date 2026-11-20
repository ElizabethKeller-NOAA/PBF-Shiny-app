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
ui_values$Category <- ifelse(is.na(ui_values$Category) | trimws(ui_values$Category) == "" | trimws(ui_values$Category) == "General Parameters", 
                             "Other Stressors", 
                             trimws(ui_values$Category))
Stressors <- setNames(as.character(ui_values$Data_sheet_values), as.character(ui_values$UI_values))
# Lookup mapping data sheet values back to friendly UI names for display (e.g., table tabs and reports)
stressor_ui_lookup <- setNames(as.character(ui_values$UI_values), as.character(ui_values$Data_sheet_values))

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
Global_Species_List <- unique(PBFs$Species)

# Robust helper function to filter PBF dataset by Habitat Areas and Basins/Oceans
# Safely handles empty/NULL selections and 0-row subsets without subscript errors
filter_pbf_by_hab_basin <- function(df, hab_selection, basin_sel) {
  if (is.null(df) || nrow(df) == 0) return(df[0, ])
  
  # 1. Habitat Areas
  if (is.null(hab_selection) || length(hab_selection) == 0) {
    return(df[0, ])
  }
  valid_habs <- intersect(hab_selection, colnames(df))
  if (length(valid_habs) == 0) {
    return(df[0, ])
  }
  hab_mat <- df[, valid_habs, drop = FALSE]
  habitat_logical <- rowSums(hab_mat == 1, na.rm = TRUE) > 0
  df <- df[habitat_logical, ]
  
  if (nrow(df) == 0) return(df[0, ])
  
  # 2. Basins/Oceans
  if (is.null(basin_sel) || length(basin_sel) == 0) {
    return(df[0, ])
  }
  basin_match <- vapply(df$basin_ocean, function(row_val) {
    if (is.na(row_val) || row_val == "") return(FALSE)
    any(vapply(basin_sel, function(b) grepl(paste0("\\b", b, "\\b"), row_val, ignore.case = TRUE), FUN.VALUE = logical(1)))
  }, FUN.VALUE = logical(1))
  df <- df[basin_match, ]
  
  # 3. Strip exclusion criteria rows globally
  df <- df[is.na(df$Area_Designated_Yes_No) | df$Area_Designated_Yes_No != 0, ]
  return(df)
}

# Helper function to generate categorized stressor checkbox blocks
make_stressor_ui_blocks <- function(prefix = "init") {
  unique_cats <- unique(ui_values$Category)
  lapply(seq_along(unique_cats), function(i) {
    cat_name <- unique_cats[i]
    sub_df <- ui_values[ui_values$Category == cat_name, ]
    chunk_choices <- setNames(as.character(sub_df$Data_sheet_values), 
                              as.character(sub_df$UI_values))
    tagList(
      tags$div(
        class = paste0("stressor-subhead", if(i == 1) " first-subhead" else ""),
        cat_name
      ),
      tags$div(
        class = "stressor-chunk",
        checkboxGroupInput(
          inputId = paste0("checkStressors_", prefix, "_chunk_", gsub("[^[:alnum:]]", "_", cat_name)), 
          label = NULL, 
          choices = chunk_choices,
          selected = NULL
        )
      )
    )
  })
}

# UI Definition ---
ui <- page_fillable(
  tags$head(
    tags$style(HTML("
      [data-display-if='!input.preview || input.preview == 0'] .selection-card,
      [data-display-if='input.preview == 0'] .selection-card { height: 600px !important; }
      [data-display-if='input.preview > 0'] .card { height: 100% !important; }
      
      .shiny-input-checkboxgroup .checkbox label,
      .shiny-input-checkboxgroup .form-check { 
        display: flex !important; 
        align-items: flex-start !important; 
      }
      .shiny-input-checkboxgroup input[type='checkbox'] {
        width: 16px !important;
        height: 16px !important;
        min-width: 16px !important;
        min-height: 16px !important;
        flex-shrink: 0 !important;
        margin-top: 3px !important;
        cursor: pointer;
      }
      .shiny-input-checkboxgroup .checkbox label span,
      .shiny-input-checkboxgroup .form-check-label { 
        white-space: normal !important; 
        margin-left: 10px !important; 
      }
      
      /* Bulk action bar (Select All / Deselect All) */
      .bulk-action-bar {
        margin-top: 0 !important;
        margin-bottom: 4px !important;
        padding-bottom: 0 !important;
        line-height: 1.2 !important;
      }
      .bulk-action-link { 
        font-size: 0.8rem; 
        text-decoration: none; 
        cursor: pointer; 
        color: #007bff; 
        display: inline-block; 
      }
      
      /* Completely hide empty shiny labels so they do not add blank space */
      .shiny-input-container label:empty,
      label.shiny-label-null,
      label.control-label:empty {
        display: none !important;
        margin: 0 !important;
        padding: 0 !important;
        height: 0 !important;
        line-height: 0 !important;
      }
      
      /* Reduce bslib card flex gap so bulk action bar and checkboxes sit together */
      .card {
        --bslib-card-body-gap: 4px !important;
      }
      .card-body {
        gap: 4px !important;
      }
      
      /* Remove excess margin from input containers inside cards */
      .card .shiny-input-container {
        margin-top: 0 !important;
        margin-bottom: 4px !important;
      }
      
      .shiny-input-checkboxgroup .shiny-options-group {
        margin-top: 0 !important;
        padding-top: 0 !important;
      }
      
      .shiny-input-checkboxgroup .checkbox:first-child,
      .shiny-input-checkboxgroup .form-check:first-child {
        margin-top: 0 !important;
      }
      
      /* Stressor subheadings: original text size, bold, tight clean margins */
      .stressor-subhead {
        font-weight: bold;
        font-size: 1rem;
        color: #495057;
        border-bottom: 1px solid #e9ecef;
        padding-bottom: 2px;
        margin-top: 8px;
        margin-bottom: 3px;
      }
      .stressor-subhead.first-subhead {
        margin-top: 2px !important;
      }
      
      /* Stressor chunks: seamless fit under each subhead */
      .stressor-chunk .shiny-input-container {
        margin-top: 0 !important;
        margin-bottom: 0 !important;
      }
      .stressor-chunk .shiny-options-group {
        margin-top: 0 !important;
        padding-top: 0 !important;
      }
      .stressor-chunk .form-check,
      .stressor-chunk .checkbox {
        margin-top: 1px !important;
        margin-bottom: 2px !important;
      }
      
      /* Inline validation text */
      .validation-warning {
        color: #dc3545;
        font-size: 0.8rem;
        font-weight: 600;
        margin-top: 3px;
        line-height: 1.2;
      }
      .validation-info {
        color: #6c757d;
        font-size: 0.82rem;
        font-style: italic;
        margin-top: 5px;
      }
      
      /* Habitats selected count in card header */
      #spp_count_init, #spp_count_sidebar {
        font-weight: normal;
        font-size: 0.85rem;
        color: #6c757d;
        margin-left: 4px;
      }
      
      /* Preview card styling: comfortable button height, centered, no scroll bar */
      .preview-card .card-body {
        display: flex !important;
        flex-direction: column !important;
        justify-content: center !important;
        overflow: hidden !important;
      }
      #preview {
        height: auto !important;
        min-height: 44px !important;
      }
    ")),
    tags$script(HTML("
      // Client-side guard: Disables preview button if Habitats, Basins, or Critical Habitats has 0 selections
      function updatePreviewButtonState() {
        var habs = $('input[name=\"checkHabitats_init\"]:checked').length;
        var basins = $('input[name=\"checkBasins_init\"]:checked').length;
        var spp = $('input[name=\"checkSpecies_init\"]:checked').length;
        var btn = $('#preview');
        if (habs === 0 || basins === 0 || spp === 0) {
          btn.prop('disabled', true).addClass('disabled').css({'cursor': 'not-allowed', 'opacity': '0.65'});
        } else {
          btn.prop('disabled', false).removeClass('disabled').css({'cursor': 'pointer', 'opacity': '1'});
        }
      }
      $(document).on('change', 'input[name=\"checkHabitats_init\"], input[name=\"checkBasins_init\"], input[name=\"checkSpecies_init\"]', function() {
        setTimeout(updatePreviewButtonState, 30);
      });
      $(document).on('shiny:inputchanged shiny:value', function(e) {
        if (e.name === 'checkHabitats_init' || e.name === 'checkBasins_init' || e.name === 'checkSpecies_init') {
          setTimeout(updatePreviewButtonState, 30);
        }
      });
      $(document).ready(function() {
        setTimeout(updatePreviewButtonState, 150);
      });
    "))
  ),
  
  conditionalPanel(
    condition = "!input.preview || input.preview == 0",
    layout_columns(
      col_widths = c(3, 3, 3, 3), 
      card(
        card_header("Table Options"),
        checkboxInput("BySpecies", "Create a table of all PBFs by Species", TRUE),
        checkboxInput("ByStressors", "Create individual tables per Stressor", FALSE),
        checkboxInput("ByOthers", "Include Table of PBFs outside selected Stressors", FALSE)
      ),
      card(
        card_header("Habitat Areas"),
        tags$div(
          class = "bulk-action-bar d-flex gap-2 align-items-center",
          actionLink("all_habitats_init", "Select All", class = "bulk-action-link"),
          tags$span("|", style = "font-size: 0.8rem; color: #6c757d;"),
          actionLink("none_habitats_init", "Deselect All", class = "bulk-action-link")
        ),
        checkboxGroupInput("checkHabitats_init", label = NULL, 
                           choices = Habitat_Types, selected = Habitat_Types),
        uiOutput("hab_warn_init")
      ),
      card(
        card_header("Basins/Oceans"),
        tags$div(
          class = "bulk-action-bar d-flex gap-2 align-items-center",
          actionLink("all_basins_init", "Select All", class = "bulk-action-link"),
          tags$span("|", style = "font-size: 0.8rem; color: #6c757d;"),
          actionLink("none_basins_init", "Deselect All", class = "bulk-action-link")
        ),
        checkboxGroupInput("checkBasins_init", label = NULL, choices = Unique_Basins, selected = Unique_Basins),
        uiOutput("basin_warn_init")
      ),
      card(
        class = "preview-card",
        card_header("Preview"),
        actionButton("preview", "View Table(s) Now", class = "btn-primary w-100", style = "font-size: 1.05rem; padding: 10px 14px;"),
        uiOutput("preview_guard_msg")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "selection-card", 
        style = "overflow-y: auto; padding-top: 10px;", 
        card_header("Choose Critical Habitats ", textOutput("spp_count_init", inline = TRUE)),
        tags$div(
          class = "bulk-action-bar d-flex gap-2 align-items-center",
          actionLink("all_spp", "Select All Visible", class = "bulk-action-link"),
          tags$span("|", style = "font-size: 0.8rem; color: #6c757d;"),
          actionLink("none_spp", "Deselect All", class = "bulk-action-link")
        ),
        uiOutput("spp_warn_init"),
        checkboxGroupInput("checkSpecies_init", label = NULL, choices = Global_Species_List, selected = Global_Species_List)
      ),
      conditionalPanel(
        condition = "input.ByStressors == true",
        card(
          class = "selection-card", 
          style = "overflow-y: auto; padding-top: 10px;",
          card_header("Select Stressors"),
          tags$div(
            class = "bulk-action-bar d-flex gap-2 align-items-center",
            actionLink("all_stressors_init", "Select All", class = "bulk-action-link"),
            tags$span("|", style = "font-size: 0.8rem; color: #6c757d;"),
            actionLink("none_stressors_init", "Deselect All", class = "bulk-action-link")
          ),
          tagList(make_stressor_ui_blocks("init"))
        )
      )
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
        card(
          card_header("Table Options"),
          checkboxInput("BySpecies_sidebar", "Create a table of all PBFs by Species", TRUE),
          checkboxInput("ByStressors_sidebar", "Create individual tables per Stressor", FALSE),
          checkboxInput("ByOthers_sidebar", "Include Table of PBFs outside selected Stressors", FALSE)
        ),
        card(card_header("Habitat Areas"),
             tags$div(
               class = "bulk-action-bar d-flex gap-2 align-items-center",
               actionLink("all_habitats_sidebar", "Select All", class = "bulk-action-link"),
               tags$span("|", style = "font-size: 0.8rem; color: #6c757d;"),
               actionLink("none_habitats_sidebar", "Deselect All", class = "bulk-action-link")
             ),
             checkboxGroupInput("checkHabitats", NULL, choices = Habitat_Types, selected = Habitat_Types),
             uiOutput("hab_warn_sidebar")),
        card(card_header("Basins/Oceans"), 
             tags$div(
               class = "bulk-action-bar d-flex gap-2 align-items-center",
               actionLink("all_basins_sidebar", "Select All", class = "bulk-action-link"),
               tags$span("|", style = "font-size: 0.8rem; color: #6c757d;"),
               actionLink("none_basins_sidebar", "Deselect All", class = "bulk-action-link")
             ),
             checkboxGroupInput("checkBasins_sidebar", label = NULL, choices = Unique_Basins, selected = Unique_Basins),
             uiOutput("basin_warn_sidebar")),
        conditionalPanel(
          condition = "input.ByStressors_sidebar == true",
          card(
            style = "max-height: 400px; overflow-y: auto;",
            card_header("Select Stressors"),
            tags$div(
              class = "bulk-action-bar d-flex gap-2 align-items-center",
              actionLink("all_stressors_sidebar", "Select All", class = "bulk-action-link"),
              tags$span("|", style = "font-size: 0.8rem; color: #6c757d;"),
              actionLink("none_stressors_sidebar", "Deselect All", class = "bulk-action-link")
            ),
            tagList(make_stressor_ui_blocks("sidebar"))
          )
        ),
        card(card_header("Critical Habitats ", textOutput("spp_count_sidebar", inline = TRUE)),
             tags$div(
               class = "bulk-action-bar d-flex gap-2 align-items-center",
               actionLink("all_spp_sidebar", "Select All Visible", class = "bulk-action-link"),
               tags$span("|", style = "font-size: 0.8rem; color: #6c757d;"),
               actionLink("none_spp_sidebar", "Deselect All", class = "bulk-action-link")
             ),
             uiOutput("spp_warn_sidebar"),
             checkboxGroupInput("checkSpecies", NULL, choices = Global_Species_List, selected = Global_Species_List))
      ),
      card(full_screen = TRUE, uiOutput("main_tabs"))
    )
  )
)

# Server Logic ----
server <- function(input, output, session) { 
  
  is_first_run <- reactiveVal(TRUE)
  
  locked_selections <- reactiveValues(
    species = NULL,
    habitats = NULL,
    basins = NULL,
    stressors = NULL
  )
  
  # Aggregators: Gathering checked stressors across chunks
  current_checked_stressors <- reactive({
    unique_cats <- unique(ui_values$Category)
    chunk_ids <- paste0("checkStressors_init_chunk_", gsub("[^[:alnum:]]", "_", unique_cats))
    checked_list <- lapply(chunk_ids, function(id) input[[id]])
    return(unlist(checked_list))
  })
  
  current_checked_stressors_sidebar <- reactive({
    unique_cats <- unique(ui_values$Category)
    chunk_ids <- paste0("checkStressors_sidebar_chunk_", gsub("[^[:alnum:]]", "_", unique_cats))
    checked_list <- lapply(chunk_ids, function(id) input[[id]])
    return(unlist(checked_list))
  })
  
  # Aggregator: Gathering Basins
  current_checked_basins <- reactive({
    if (input$preview == 0) input$checkBasins_init else input$checkBasins_sidebar
  })
  
  # UI Validation warnings (Initial Stage)
  output$hab_warn_init <- renderUI({
    if (is.null(input$checkHabitats_init) || length(input$checkHabitats_init) == 0) {
      tags$div(class = "validation-warning", "⚠️ You must choose at least one Habitat Area.")
    }
  })
  
  output$basin_warn_init <- renderUI({
    if (is.null(input$checkBasins_init) || length(input$checkBasins_init) == 0) {
      tags$div(class = "validation-warning", "⚠️ You must choose at least one Basin/Ocean.")
    }
  })
  
  output$spp_warn_init <- renderUI({
    hab_empty <- is.null(input$checkHabitats_init) || length(input$checkHabitats_init) == 0
    basin_empty <- is.null(input$checkBasins_init) || length(input$checkBasins_init) == 0
    if (hab_empty || basin_empty) {
      tags$div(class = "validation-info", "No critical habitats match. Please select at least one Habitat Area and Basin/Ocean.")
    } else if (is.null(input$checkSpecies_init) || length(input$checkSpecies_init) == 0) {
      tags$div(class = "validation-warning", "⚠️ You must choose at least one Critical Habitat.")
    }
  })
  
  # Dynamic counts in card headers
  output$spp_count_init <- renderText({
    n <- if (is.null(input$checkSpecies_init)) 0 else length(input$checkSpecies_init)
    paste0("(", n, if (n == 1) " Habitat Selected)" else " Habitats Selected)")
  })
  
  output$spp_count_sidebar <- renderText({
    n <- if (is.null(input$checkSpecies)) 0 else length(input$checkSpecies)
    paste0("(", n, if (n == 1) " Habitat Selected)" else " Habitats Selected)")
  })
  
  # Inline warning directly under the Preview button
  output$preview_guard_msg <- renderUI({
    hab_empty <- is.null(input$checkHabitats_init) || length(input$checkHabitats_init) == 0
    basin_empty <- is.null(input$checkBasins_init) || length(input$checkBasins_init) == 0
    spp_empty <- is.null(input$checkSpecies_init) || length(input$checkSpecies_init) == 0
    
    if (hab_empty || basin_empty || spp_empty) {
      missing <- c()
      if (hab_empty) missing <- c(missing, "Habitat Area")
      if (basin_empty) missing <- c(missing, "Basin/Ocean")
      if (spp_empty) missing <- c(missing, "Critical Habitat")
      tags$div(
        class = "validation-warning text-center mt-2",
        paste0("⚠️ Choose at least 1 ", paste(missing, collapse = ", "), " to proceed")
      )
    }
  })
  
  # UI Validation warnings (Sidebar Stage)
  output$hab_warn_sidebar <- renderUI({
    if (is.null(input$checkHabitats) || length(input$checkHabitats) == 0) {
      tags$div(class = "validation-warning", "⚠️ You must choose at least one Habitat Area.")
    }
  })
  
  output$basin_warn_sidebar <- renderUI({
    if (is.null(input$checkBasins_sidebar) || length(input$checkBasins_sidebar) == 0) {
      tags$div(class = "validation-warning", "⚠️ You must choose at least one Basin/Ocean.")
    }
  })
  
  output$spp_warn_sidebar <- renderUI({
    hab_empty <- is.null(input$checkHabitats) || length(input$checkHabitats) == 0
    basin_empty <- is.null(input$checkBasins_sidebar) || length(input$checkBasins_sidebar) == 0
    if (hab_empty || basin_empty) {
      tags$div(class = "validation-info", "No critical habitats match. Please select at least one Habitat Area and Basin/Ocean.")
    } else if (is.null(input$checkSpecies) || length(input$checkSpecies) == 0) {
      tags$div(class = "validation-warning", "⚠️ You must choose at least one Critical Habitat.")
    }
  })
  
  # INITIAL UI: Filter species choices whenever habitats or basins update
  # Uses observe() to ensure execution whenever ANY dependency updates (no ignoreNULL issues)
  observe({
    hab_sel <- input$checkHabitats_init
    basin_sel <- input$checkBasins_init
    
    df <- filter_pbf_by_hab_basin(PBFs, hab_sel, basin_sel)
    updated_choices <- as.character(unique(df$Species))
    
    still_checked <- if (is_first_run()) {
      is_first_run(FALSE)
      updated_choices
    } else {
      intersect(input$checkSpecies_init, updated_choices)
    }
    
    updateCheckboxGroupInput(
      session, 
      "checkSpecies_init", 
      choices = updated_choices, 
      selected = still_checked
    )
  })
  
  # SECOND STAGE (SIDEBAR) UI: Filter species choices when habitats or basins update
  observe({
    req(input$preview > 0)
    hab_sel <- input$checkHabitats
    basin_sel <- input$checkBasins_sidebar
    
    df <- filter_pbf_by_hab_basin(PBFs, hab_sel, basin_sel)
    updated_choices <- as.character(unique(df$Species))
    still_checked <- intersect(input$checkSpecies, updated_choices)
    
    updateCheckboxGroupInput(
      session, 
      "checkSpecies", 
      choices = updated_choices, 
      selected = still_checked
    )
  })
  
  # Bulk actions for Initial UI filters
  observeEvent(input$all_habitats_init, {
    updateCheckboxGroupInput(session, "checkHabitats_init", selected = Habitat_Types)
  })
  observeEvent(input$none_habitats_init, {
    updateCheckboxGroupInput(session, "checkHabitats_init", selected = character(0))
  })
  
  observeEvent(input$all_basins_init, {
    updateCheckboxGroupInput(session, "checkBasins_init", selected = Unique_Basins)
  })
  observeEvent(input$none_basins_init, {
    updateCheckboxGroupInput(session, "checkBasins_init", selected = character(0))
  })
  
  observeEvent(input$all_stressors_init, {
    unique_cats <- unique(ui_values$Category)
    for (cat_name in unique_cats) {
      sub_df <- ui_values[ui_values$Category == cat_name, ]
      init_id <- paste0("checkStressors_init_chunk_", gsub("[^[:alnum:]]", "_", cat_name))
      updateCheckboxGroupInput(session, init_id, selected = as.character(sub_df$Data_sheet_values))
    }
  })
  observeEvent(input$none_stressors_init, {
    unique_cats <- unique(ui_values$Category)
    for (cat_name in unique_cats) {
      init_id <- paste0("checkStressors_init_chunk_", gsub("[^[:alnum:]]", "_", cat_name))
      updateCheckboxGroupInput(session, init_id, selected = character(0))
    }
  })
  
  observeEvent(input$all_spp, {
    df <- filter_pbf_by_hab_basin(PBFs, input$checkHabitats_init, input$checkBasins_init)
    updateCheckboxGroupInput(session, "checkSpecies_init", selected = as.character(unique(df$Species)))
  })
  observeEvent(input$none_spp, {
    updateCheckboxGroupInput(session, "checkSpecies_init", selected = character(0))
  })
  
  # Bulk actions for Sidebar filters
  observeEvent(input$all_habitats_sidebar, {
    updateCheckboxGroupInput(session, "checkHabitats", selected = Habitat_Types)
  })
  observeEvent(input$none_habitats_sidebar, {
    updateCheckboxGroupInput(session, "checkHabitats", selected = character(0))
  })
  
  observeEvent(input$all_basins_sidebar, {
    updateCheckboxGroupInput(session, "checkBasins_sidebar", selected = Unique_Basins)
  })
  observeEvent(input$none_basins_sidebar, {
    updateCheckboxGroupInput(session, "checkBasins_sidebar", selected = character(0))
  })
  
  observeEvent(input$all_stressors_sidebar, {
    unique_cats <- unique(ui_values$Category)
    for (cat_name in unique_cats) {
      sub_df <- ui_values[ui_values$Category == cat_name, ]
      side_id <- paste0("checkStressors_sidebar_chunk_", gsub("[^[:alnum:]]", "_", cat_name))
      updateCheckboxGroupInput(session, side_id, selected = as.character(sub_df$Data_sheet_values))
    }
  })
  observeEvent(input$none_stressors_sidebar, {
    unique_cats <- unique(ui_values$Category)
    for (cat_name in unique_cats) {
      side_id <- paste0("checkStressors_sidebar_chunk_", gsub("[^[:alnum:]]", "_", cat_name))
      updateCheckboxGroupInput(session, side_id, selected = character(0))
    }
  })
  
  observeEvent(input$all_spp_sidebar, {
    df <- filter_pbf_by_hab_basin(PBFs, input$checkHabitats, input$checkBasins_sidebar)
    updateCheckboxGroupInput(session, "checkSpecies", selected = as.character(unique(df$Species)))
  })
  observeEvent(input$none_spp_sidebar, {
    updateCheckboxGroupInput(session, "checkSpecies", selected = character(0))
  })
  
  target_pbf_col <- reactive({ "PBF" })
  
  # Lock in snapshots when preview is pressed
  observeEvent(input$preview, {
    hab_sel <- input$checkHabitats_init
    basin_sel <- input$checkBasins_init
    spp_sel <- input$checkSpecies_init
    
    # Server-side validation guard
    if (is.null(hab_sel) || length(hab_sel) == 0 || 
        is.null(basin_sel) || length(basin_sel) == 0 || 
        is.null(spp_sel) || length(spp_sel) == 0) {
      showNotification("⚠️ You must select at least one Habitat Area, Basin/Ocean, and Critical Habitat before proceeding.", type = "error")
      return()
    }
    
    locked_selections$species <- spp_sel
    locked_selections$habitats <- hab_sel
    locked_selections$basins <- basin_sel
    locked_selections$stressors <- current_checked_stressors()
    
    # Sync Table Options to sidebar
    updateCheckboxInput(session, "BySpecies_sidebar", value = input$BySpecies)
    updateCheckboxInput(session, "ByStressors_sidebar", value = input$ByStressors)
    updateCheckboxInput(session, "ByOthers_sidebar", value = input$ByOthers)
    
    # Sync Habitats and Basins to sidebar
    updateCheckboxGroupInput(session, "checkHabitats", selected = locked_selections$habitats)
    updateCheckboxGroupInput(session, "checkBasins_sidebar", selected = locked_selections$basins)
    
    # Sync filtered species choices and selection to sidebar
    df_init <- filter_pbf_by_hab_basin(PBFs, locked_selections$habitats, locked_selections$basins)
    valid_choices <- as.character(unique(df_init$Species))
    still_checked <- intersect(locked_selections$species, valid_choices)
    updateCheckboxGroupInput(session, "checkSpecies", choices = valid_choices, selected = still_checked)
    
    # Sync Stressor chunks to sidebar
    unique_cats <- unique(ui_values$Category)
    for (cat_name in unique_cats) {
      init_id <- paste0("checkStressors_init_chunk_", gsub("[^[:alnum:]]", "_", cat_name))
      side_id <- paste0("checkStressors_sidebar_chunk_", gsub("[^[:alnum:]]", "_", cat_name))
      updateCheckboxGroupInput(session, side_id, selected = input[[init_id]])
    }
  }, ignoreInit = TRUE)
  
  # Base Dataset Engine
  base_filtered_data <- reactive({
    spp_selection <- if(input$preview == 0) locked_selections$species else input$checkSpecies
    hab_selection <- if(input$preview == 0) locked_selections$habitats else input$checkHabitats
    basin_sel <- if(input$preview == 0) locked_selections$basins else input$checkBasins_sidebar
    col <- target_pbf_col()
    
    validate(
      need(length(hab_selection) > 0, "Please select at least one Habitat Area to display PBF tables."),
      need(length(basin_sel) > 0, "Please select at least one Basin/Ocean to display PBF tables."),
      need(length(spp_selection) > 0, "Please select at least one Critical Habitat to display PBF tables.")
    )
    
    df <- PBFs[PBFs$Species %in% spp_selection, ]
    df <- filter_pbf_by_hab_basin(df, hab_selection, basin_sel)
    
    df <- df[!is.na(df[[col]]) & trimws(df[[col]]) != "", ]
    return(df)
  })
  
  render_my_datatable <- function(data_func) {
    DT::renderDataTable({
      df <- data_func()
      col <- target_pbf_col()
      req(col %in% colnames(df))
      df
    }, server = TRUE, rownames = FALSE, 
    options = list(
      pageLength = -1, 
      dom = 't', 
      scrollY = "75vh", 
      scrollCollapse = TRUE,
      order = list(list(0, 'asc'), list(1, 'asc'))
    ))
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
    st_sel <- if(input$preview == 0) locked_selections$stressors else current_checked_stressors_sidebar()
    if (is.null(st_sel) || length(st_sel) == 0) return(df[, c("Species", "PBF_category", col)])
    valid_cols <- intersect(st_sel, colnames(df))
    if (length(valid_cols) == 0) return(df[, c("Species", "PBF_category", col)])
    sel_cols <- df[, valid_cols, drop = FALSE]
    sel_cols[is.na(sel_cols)] <- 0
    df[rowSums(sel_cols == 1) == 0, c("Species", "PBF_category", col)]
  }
  
  output$main_tabs <- renderUI({
    req(input$preview > 0)
    by_spp <- if (input$preview == 0) input$BySpecies else input$BySpecies_sidebar
    by_stressors <- if (input$preview == 0) input$ByStressors else input$ByStressors_sidebar
    by_others <- if (input$preview == 0) input$ByOthers else input$ByOthers_sidebar
    
    req(isTRUE(by_spp) | isTRUE(by_stressors) | isTRUE(by_others))
    tabs <- list()
    if (isTRUE(by_spp)) {
      tabs <- append(tabs, list(nav_panel("Combined Species", DT::dataTableOutput("PBFtable_BySpecies"))))
    }
    
    st_active <- if(input$preview == 0) locked_selections$stressors else current_checked_stressors_sidebar()
    if (isTRUE(by_stressors) && !is.null(st_active) && length(st_active) > 0) {
      st_tabs <- lapply(st_active, function(st) {
        id <- paste0("table_", gsub("[^[:alnum:]]", "_", st))
        ui_title <- stressor_ui_lookup[[st]]
        if (is.null(ui_title) || is.na(ui_title) || trimws(ui_title) == "") {
          ui_title <- st
        }
        output[[id]] <- render_my_datatable(function() generate_stressor_data(st))
        nav_panel(ui_title, DT::dataTableOutput(id))
      })
      tabs <- append(tabs, st_tabs)
    }
    if (isTRUE(by_others)) {
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
      by_spp <- if (input$preview == 0) input$BySpecies else input$BySpecies_sidebar
      by_stressors <- if (input$preview == 0) input$ByStressors else input$ByStressors_sidebar
      by_others <- if (input$preview == 0) input$ByOthers else input$ByOthers_sidebar
      st_active <- if(input$preview == 0) locked_selections$stressors else current_checked_stressors_sidebar()
      
      out <- list()
      col <- target_pbf_col()
      if (isTRUE(by_spp)) out[["Combined"]] <- base_filtered_data()[, c("Species","PBF_category", col)]
      
      if (isTRUE(by_stressors) && !is.null(st_active) && length(st_active) > 0) {
        for (st in st_active) {
          ui_title <- stressor_ui_lookup[[st]]
          sheet_name <- if (!is.null(ui_title) && !is.na(ui_title) && trimws(ui_title) != "") ui_title else st
          out[[substr(sheet_name, 1, 31)]] <- generate_stressor_data(st)
        }
      }
      if (isTRUE(by_others)) out[["Outside_Selection"]] <- generate_other_pbfs_data()
      writexl::write_xlsx(out, path = file)
    }
  )
  
  output$downloaddocx_sidebar <- downloadHandler(
    filename = function() { paste0("PBF_Report_", Sys.Date(), ".docx") },
    content = function(file) {
      by_spp <- if (input$preview == 0) input$BySpecies else input$BySpecies_sidebar
      by_stressors <- if (input$preview == 0) input$ByStressors else input$ByStressors_sidebar
      by_others <- if (input$preview == 0) input$ByOthers else input$ByOthers_sidebar
      st_active <- if(input$preview == 0) locked_selections$stressors else current_checked_stressors_sidebar()
      
      final_list <- list()
      col <- target_pbf_col()
      if (isTRUE(by_spp)) final_list[["Species Summary"]] <- base_filtered_data()[, c("Species", "PBF_category", col)]
      
      if (isTRUE(by_stressors) && !is.null(st_active) && length(st_active) > 0) {
        for (st in st_active) {
          ui_title <- stressor_ui_lookup[[st]]
          report_name <- if (!is.null(ui_title) && !is.na(ui_title) && trimws(ui_title) != "") ui_title else st
          final_list[[report_name]] <- generate_stressor_data(st)
        }
      }
      if (isTRUE(by_others)) final_list[["Outside Selection"]] <- generate_other_pbfs_data()
      rmarkdown::render("report_template.Rmd", output_file = file, params = list(data_list = final_list, pbf_column = col))
    }
  )
}

shinyApp(ui, server)
