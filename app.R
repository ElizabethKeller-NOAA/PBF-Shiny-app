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
# NOTE: Replace "data/PBFs for Shiny app test.csv" with your actual path
PBFs <- read.csv("data/PBFs for Shiny app test.csv")
 
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
# This variable definition MUST be correct for your data:
Stressors <- as.character(colnames(PBFs)[15:41])

# UI Definition ---

ui <- page_fillable(
 
  tags$head(
    tags$style(HTML("
      /* 1. Base style for vertical stacking (applies to all checkboxes) */
      #checkSpecies .checkbox {
        display: block;
        margin-right: 0px;
      }
      
      /* 2. Rule for the WIDE INITIAL LAYOUT: Force NO WRAPPING in the wide card */
      .wide-layout-species-card #checkSpecies .checkbox {
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
      # 1. Table Organization (Top of stack)
      card(
        class = "wide-layout-species-card",
        card_header("Critical Habitats"),
        p("Choose the species whose critical habitat may be affected by the action."),
        checkboxGroupInput("checkSpecies", label = NULL, choices = unique(PBFs$Species_full)),
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
          actionButton("preview", label = "Preview Table(s) Now"),
        )
      ),
      
      # CARD 4: Downloads
      card(
        card_header("Downloads"),
        downloadButton("downloadxlsx", label = "Download Table(s) as xlsx"),
        downloadButton("downloaddocx", label = "Download Table(s) as docx"),
      )
    ), # End layout_columns
    
    # --- STRESSOR CHECKBOXES (Immediate visibility in Initial State) ---
    conditionalPanel(
      condition = "input.ByStressors == true",
      layout_columns(
      card(
        card_header("Select Stressors"),
        checkboxGroupInput(
          "checkStressors_init",
          label = NULL,
          choices = Stressors
        )
      )
    ))
    # ------------------------------------------------------------------
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
        
        # 1. Table Preview
        card(
          card_header("Table Preview"),
          actionButton("preview_update", label = "Update Table(s)"), 
        ),
        
        # 2. Downloads
        card(
          card_header("Downloads"),
          downloadButton("downloadxlsx", label = "Download .xlsx"),
          downloadButton("downloaddocx", label = "Download .docx"),
        ),
        
        # 3. Conditional Stressor Checkboxes (DYNAMIC INPUT - RESTORED)
        conditionalPanel(
          condition = "input.ByStressors == true",
          card(
            card_header("Select Stressors"),
            # Revert to uiOutput to pass dynamic 'selected' argument from server
            uiOutput("stressor_inputs") 
          )
        ),
        
        # 4. Critical Habitats (Long list, bottom of stack)
        card(
          class = "scrollable-card",
          card_header("Critical Habitats"),
          checkboxGroupInput("checkSpecies", label = NULL, choices = unique(PBFs$Species_full)),
        )
      ),
      
      # --- Main Content Area (Wide Tabbed Table) ---
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
  
  # 1. TRIGGER AND BASE DATA DEFINITIONS (MUST BE FIRST)
  # ----------------------------------------------------------------------------------
  
  # Trigger Logic: Listens to BOTH buttons
  table_trigger <- reactive({
    input$preview
    input$preview_update
    max(input$preview, input$preview_update)
  })
  
  # Base Filtered Data: Filters by species only, runs ONLY on button click (Visible to all outputs)
  base_filtered_data <- eventReactive(table_trigger(), {
    if (is.null(input$checkSpecies)) {
      return(PBFs[0, ]) 
    }
    PBFs[PBFs$Species_full %in% input$checkSpecies, ]
  })
  
  # Reactive value to store the stressor selections
  current_stressor_selections <- reactiveVal(NULL)
  
  # ----------------------------------------------------------------------------------
  
  
  # 2. INPUT SYNCHRONIZATION LOGIC (Observe blocks)
  # ----------------------------------------------------------------------------------
  
  # Observe 1: Stores the stressor selections immediately
  observe({
    # We check both the initial input and the dynamic input, prioritizing the dynamic one if present
    # This also helps capture the initial state when the layout switches (as 'checkStressors' will then exist)
    if (!is.null(input$checkStressors)) {
        current_stressor_selections(input$checkStressors)
    }
  })
  
  # Observe 2: Initializes inputs upon layout switch
  observeEvent(input$preview, {
    
    # Value to be transferred is read from the INITIAL ID
    initial_stressor_values <- input$checkStressors_init
    
    # 1. Update Critical Habitat/Species Selections (Static Input):
    # This ensures the selection is maintained across states
    updateCheckboxGroupInput(
      session = session,
      inputId = "checkSpecies",
      selected = input$checkSpecies
    )
    
    # 2. Update Stressor Selections (Dynamic Input):
    if (isTRUE(input$ByStressors)) {
      # Update the reactive storage with the value from the INITIAL input
      current_stressor_selections(initial_stressor_values)
      
      # Force the renderUI to execute once with the correct initial selections
      output$stressor_inputs <- renderUI({
        
        req(Stressors) 
        
        checkboxGroupInput(
          "checkStressors", # <--- Use the FINAL, CORRECT ID here
          label = NULL, 
          choices = Stressors,
          selected = initial_stressor_values # Use the initial value for the first render
        )
      })
    }
  }, ignoreInit = TRUE)
  # ----------------------------------------------------------------------------------
  
  
  # 3. OUTPUT RENDERING LOGIC (Uses the reactives defined above)
  # ----------------------------------------------------------------------------------
  
  # --- Stressor Input Content (Dynamic, handles subsequent updates) ---
  output$stressor_inputs <- renderUI({
    
    req(Stressors) 
    
    initial_selections <- current_stressor_selections()
    
    checkboxGroupInput(
      "checkStressors",
      label = NULL, 
      choices = Stressors,
      selected = initial_selections
    )
  })
  
  # --- Dynamic Tab and Table Rendering ---
  
  # Function to generate the data for a specific stressor tab (Used for both display and download)
  generate_stressor_data <- function(stressor_name) {
      data_to_filter <- base_filtered_data() 
      
      filter_condition <- (data_to_filter[[stressor_name]] == 1) & 
          (!is.na(data_to_filter[[stressor_name]]))
      
      # Note: We return a standard data frame/tibble here for writexl
      final_data <- data_to_filter[filter_condition, c("Species_full", "PBF")]
      return(final_data)
  }
  
  output$main_tabs <- renderUI({
    
    # Dependencies...
    req(input$BySpecies | input$ByStressors)
    if (isTRUE(input$ByStressors)) {
      input$checkStressors 
    }
    
    tab_list <- list()
    
    # CASE 1: "By Species" is checked - ADD TO TAB LIST
    if (isTRUE(input$BySpecies)) {
      tab_list <- append(tab_list, list(
        nav_panel(
          title = "Combined Species Data",
          value = "tab_species",
          DT::dataTableOutput("PBFtable_BySpecies")
        )
      ))
    } 
    
    # CASE 2: "By Stressors" is checked AND specific stressors are selected - ADD TO TAB LIST
    if (isTRUE(input$ByStressors) && !is.null(input$checkStressors)) {
      
      stressor_tabs <- lapply(input$checkStressors, function(stressor_name) {
        
        output_id <- paste0("table_", gsub("[^[:alnum:]]", "_", stressor_name))
        
        # Define the rendering for the current table dynamically
        output[[output_id]] <- DT::renderDataTable({
          
          final_data <- generate_stressor_data(stressor_name)
          
          DT::datatable(
            data = final_data,
            caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; font-size: 1.2em;',
                                             stressor_name),
            rownames = FALSE,
            options = list(pageLength = 100, lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')))
          )
        })
        
        # Return the actual tab UI element
        nav_panel(
          title = stressor_name,
          value = paste0("tab_", output_id),
          DT::dataTableOutput(output_id)
        )
      })
      
      tab_list <- append(tab_list, stressor_tabs)
    }
    
    # If tabs were generated, wrap them in the card tabset
    if (length(tab_list) > 0) {
      return(navset_card_tab(
        !!!tab_list 
      ))
    }
    
    # No valid options selected
    return(NULL)
  })
  
  # --- Dedicated Output for the "Combined Species Data" Tab ---
  output$PBFtable_BySpecies <- DT::renderDataTable({
    # base_filtered_data() is now correctly defined and accessible
    data <- base_filtered_data()[, c("Species_full", "PBF")]
    
    DT::datatable(
      data = data,
      rownames = FALSE,
      options = list(pageLength = 100, lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')))
    )
  })
  
  # 4. DOWNLOAD HANDLER FOR XLSX (NEW)
  # ----------------------------------------------------------------------------------
  output$downloadxlsx <- downloadHandler(
    filename = function() {
      paste("PBF_Tables-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      
      # 1. Initialize an empty list to hold data frames for each sheet
      output_list <- list()
      
      # 2. Add the "Combined Species Data" table if checked
      if (isTRUE(input$BySpecies)) {
        # Ensure base_filtered_data is triggered and contains data
        data_species <- base_filtered_data() 
        if (nrow(data_species) > 0) {
            output_list[["Combined_PBFs"]] <- data_species[, c("Species_full", "PBF")]
        }
      }
      
      # 3. Add the "By Stressor" tables if checked
      if (isTRUE(input$ByStressors) && !is.null(input$checkStressors)) {
        
        for (stressor_name in input$checkStressors) {
            
            # Generate the filtered data frame using the helper function
            data_stressor <- generate_stressor_data(stressor_name)
            
            # Only add the sheet if the resulting data table has rows
            if (nrow(data_stressor) > 0) {
                
                # Clean up the stressor name for the sheet tab
                sheet_name <- gsub("[^[:alnum:]]", "_", stressor_name)
                # Shorten the name if it's too long (Excel sheet name limit is 31 characters)
                if (nchar(sheet_name) > 31) {
                    sheet_name <- substr(sheet_name, 1, 28)
                    sheet_name <- paste0(sheet_name, "...")
                }
                
                output_list[[sheet_name]] <- data_stressor
            }
        }
      }
      
      # 4. Write the list of data frames to a multi-sheet XLSX file
      if (length(output_list) > 0) {
          writexl::write_xlsx(output_list, path = file)
      } else {
          # Handle the case where no tables were generated (e.g., if no species were selected)
          stop("No data selected to download.")
      }
    }
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)