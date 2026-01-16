##################################
# R Shiny app to make PBF tables #
##################################

# load packages
library(data.table)
library(shiny)
library(bslib)
library(writexl) # Required for writing XLSX files
library(DT) # Ensure DT is loaded if you use DT::renderDataTable

# read in data; using csv for now
PBFs <- read.csv("data/All PBFs - Stressor Categories.csv") 

# data wrangling to make it the right format
# creates species+ESU/DPS; no separator if no ESU/DPS
PBFs$Species_full <- ifelse(
  is.na(PBFs$ESU_DPS) | PBFs$ESU_DPS == "",
  PBFs$Species,
  paste0(PBFs$Species, " – ", PBFs$ESU_DPS)
)
# Sort the PBFs data frame alphabetically by Species_full
PBFs <- PBFs[order(PBFs$Species_full), ]

# Define Stressors globally
Stressors <- as.character(colnames(PBFs)[15:length(PBFs)])

# UI Definition ---

ui <- page_fillable(
  
  tags$head(
    tags$style(HTML("
      /* 1. Base style for vertical stacking (applies to all checkboxes) */
      #checkSpecies .checkbox, #checkSpecies_init .checkbox, 
      #checkStressors .checkbox, #checkStressors_init .checkbox {
        display: block;
        margin-right: 0px;
      }
      
      /* 2. Rule for the WIDE INITIAL LAYOUT: Force NO WRAPPING in the wide card */
      .wide-layout-species-card #checkSpecies_init .checkbox {
          white-space: nowrap; 
      }

      /* 3. SCROLLBAR FOR INITIAL STATE (using the new class) */
      .wide-layout-species-card .card-body {
        max-height: 400px;
        overflow-y: auto;
        overflow-x: hidden; 
      }
      
      /* 4. SCROLLBAR FOR ACTIVE STATE (using the scrollable-card class) */
      .scrollable-card .card-body {
        max-height: 400px;
        overflow-y: auto;
        overflow-x: hidden;
      }
    "))
  ),
  
  # =========================================================
  # 1. INITIAL STATE (Input Cards Fill Space - No Table)
  #    Condition: input.preview == 0 (Button not clicked)
  # =========================================================
  conditionalPanel(
    condition = "input.preview == 0",
    
    layout_columns(
      # Arrange inputs fluidly across the top
      col_widths = c(6, 3, 3), 
      
      # CARD 1: Critical Habitats 
      card(
        class = "wide-layout-species-card",
        card_header("Critical Habitats"),
        p("Choose the species whose critical habitat may be affected by the action."),
        checkboxGroupInput("checkSpecies_init", label = NULL, choices = unique(PBFs$Species_full)),
      ),
      
      # CARD 2 & 3: Organization and Preview (Nested for stacking)
      layout_columns(
        col_widths = 12, 
        card(
          card_header("Table Organization"),
          checkboxInput("BySpecies", label = "Create one table of all PBFs by species"),
          checkboxInput("ByStressors", label = "Create tables of PBFs per stressor/category"),
        ),
        
        card(
          card_header("Table Preview"),
          actionButton("preview", label = "View Table(s) Now"),
        )
      ),
      
      # CARD 4: Downloads
      card(
        card_header("Downloads"),
        downloadButton("downloadxlsx", label = "Download Table(s) as xlsx"),
        downloadButton("downloaddocx", label = "Download Table(s) as docx"),
      )
    ), # End layout_columns
    
    # --- STRESSOR CHECKBOXES (Initial State) ---
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
  ), # END Initial State
  
  # =========================================================
  # 2. ACTIVE STATE (Sidebar + Wide Table Area)
  #    Condition: input.preview > 0 (Button clicked)
  # =========================================================
  conditionalPanel(
    condition = "input.preview > 0",
    
    layout_sidebar(
      fillable = TRUE, 
      
      # --- Sidebar for Controls (wider column) ---
      sidebar = sidebar(
        width = 400, 
        position = "left",
        
        # 1. Downloads
        card(
          card_header("Downloads"),
          downloadButton("downloadxlsx_sidebar", label = "Download Table(s) as xlsx"),
          downloadButton("downloaddocx_sidebar", label = "Download Table(s) as docx"),
        ),
        
        # 2. Stressor Checkboxes (Now Static in UI)
        conditionalPanel(
          condition = "input.ByStressors == true",
          card(
            class = "scrollable-card",
            card_header("Select Stressors/Categories"),
            checkboxGroupInput("checkStressors", label = NULL, choices = Stressors)
          )
        ),
        
        # 3. Critical Habitats
        card(
          class = "scrollable-card",
          card_header("Critical Habitats"),
          checkboxGroupInput("checkSpecies", label = NULL, choices = unique(PBFs$Species_full)),
        )
      ),
      
      # --- Main Content Area ---
      card(
        full_screen = TRUE, 
        height = "100%", 
        card_header("PBF Table(s)"),
        uiOutput("main_tabs") 
      )
    )
  ) # END Active State
)

# Define server logic ----

server <- function(input, output, session) { 
  
  # 1. CORE DATA DEFINITIONS
  # Listen to initial inputs until preview is clicked, then listen to sidebar inputs
  base_filtered_data <- reactive({
    spp_selection <- if(input$preview == 0) input$checkSpecies_init else input$checkSpecies
    
    if (is.null(spp_selection)) {
      return(PBFs[0, ]) 
    }
    PBFs[PBFs$Species_full %in% spp_selection, ]
  })
  
  # 2. INPUT SYNCHRONIZATION
  # When preview button is clicked, push values from _init to the sidebar inputs
  observeEvent(input$preview, {
    updateCheckboxGroupInput(session, "checkSpecies", selected = input$checkSpecies_init)
    updateCheckboxGroupInput(session, "checkStressors", selected = input$checkStressors_init)
  }, ignoreInit = TRUE)
  
  # 3. OUTPUT RENDERING LOGIC
  
  # Function to generate the data for a specific stressor tab 
  generate_stressor_data <- function(stressor_name) {
    data_to_filter <- base_filtered_data() 
    
    req(stressor_name %in% colnames(data_to_filter))
    
    filter_condition <- (data_to_filter[[stressor_name]] == 1) & 
      (!is.na(data_to_filter[[stressor_name]]))
    
    final_data <- data_to_filter[filter_condition, c("Species_full", "PBF")]
    return(final_data)
  }
  
  # --- Dynamic Tab and Table Rendering ---
  output$main_tabs <- renderUI({
    req(input$BySpecies | input$ByStressors)
    
    tab_list <- list()
    
    if (isTRUE(input$BySpecies)) {
      tab_list <- append(tab_list, list(
        nav_panel(
          title = "Combined Species Data",
          value = "tab_species",
          tags$div(style = "height: 100%;",
                   DT::dataTableOutput("PBFtable_BySpecies", height = "100%")
          )
        )
      ))
    } 
    
    # Listen to the active sidebar stressors
    if (isTRUE(input$ByStressors) && !is.null(input$checkStressors)) {
      
      stressor_tabs <- lapply(input$checkStressors, function(stressor_name) {
        output_id <- paste0("table_", gsub("[^[:alnum:]]", "_", stressor_name))
        
        output[[output_id]] <- DT::renderDataTable({
          final_data <- generate_stressor_data(stressor_name)
          DT::datatable(
            data = final_data,
            caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; font-size: 1.2em;',
                                              stressor_name),
            rownames = FALSE,
            options = list(pageLength = -1, dom = 't') 
          )
        })
        
        nav_panel(
          title = stressor_name,
          value = paste0("tab_", output_id),
          tags$div(style = "height: 100%;",
                   DT::dataTableOutput(output_id, height = "100%")
          )
        )
      })
      tab_list <- append(tab_list, stressor_tabs)
    }
    
    if (length(tab_list) > 0) {
      return(navset_card_tab(!!!tab_list))
    }
    return(NULL)
  })
  
  output$PBFtable_BySpecies <- DT::renderDataTable({
    data <- base_filtered_data()[, c("Species_full", "PBF")]
    DT::datatable(
      data = data,
      rownames = FALSE,
      options = list(pageLength = -1, dom = 't') 
    )
  })
  
  # 4. DOWNLOAD HANDLER FOR XLSX (Linked to both buttons)
  download_logic_xlsx <- function(file) {
    output_list <- list()
    if (isTRUE(input$BySpecies)) {
      data_species <- base_filtered_data() 
      if (nrow(data_species) > 0) {
        output_list[["Combined_PBFs"]] <- data_species[, c("Species_full", "PBF")]
      }
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
    if (length(output_list) > 0) writexl::write_xlsx(output_list, path = file)
  }
  
  output$downloadxlsx <- output$downloadxlsx_sidebar <- downloadHandler(
    filename = function() { paste("PBF_Tables-", Sys.Date(), ".xlsx", sep="") },
    content = download_logic_xlsx
  )
  
  # (Docx handler left as-is for now until we integrate your specific pivot request)
  output$downloaddocx <- output$downloaddocx_sidebar <- downloadHandler(
    filename = function() { paste("PBF_Tables-", Sys.Date(), ".docx", sep="") },
    content = function(file) { # Your original docx logic would go here
    }
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)