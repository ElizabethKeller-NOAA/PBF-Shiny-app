##################################
# R Shiny app to make PBF tables #
##################################

# load packages
library(data.table)
library(shiny)
library(bslib)
library(writexl) # Required for writing XLSX files

# read in data
# use csv for now
PBFs <- read.csv("data/PBFs for Shiny app test.csv")

# any data wrangling to make it the right format for what I want
# filter any? *only keep what you will use for the app; don't have it load anything extraneous

# creates species+ESU/DPS; no separator if no ESU/DPS
  # Convert to data.table for optimized operations if working with a data frame
  # Or just use fifelse on the vectors
PBFs$Species_full <- ifelse(
  is.na(PBFs$ESU_DPS) | PBFs$ESU_DPS == "", 
  PBFs$Species, 
  paste0(PBFs$Species, " – ", PBFs$ESU_DPS)
)
# Sort the PBFs data frame alphabetically by Species_full
PBFs <- PBFs[order(PBFs$Species_full), ]

Stressors <- colnames(PBFs)[15:41]

#############
# Shiny app #
#############

ui <- page_fillable(
  
  # --- CSS BLOCK ---
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
        /* This restores the scrollbar for the non-wrapping card when first loading */
        .wide-layout-species-card .card-body {
          max-height: 400px;
          overflow-y: auto;
          overflow-x: hidden; 
        }
        
        /* 4. SCROLLBAR FOR ACTIVE STATE (using the scrollable-card class) */
        /* This allows wrapping and ensures the scrollbar exists in the narrow sidebar */
        .scrollable-card .card-body {
          max-height: 400px;
          overflow-y: auto;
          overflow-x: hidden;
        }
      "))
  ),
  # ------------------
  
  # =========================================================
  # 1. INITIAL STATE (Input Cards Fill Space - No Table)
  #    Condition: input.preview == 0 (Button not clicked)
  # =========================================================
  conditionalPanel(
    condition = "input.preview == 0",
    
    layout_columns(
      # Arrange inputs fluidly across the top
      col_widths = c(6, 3, 3), 
      
      # CARD 1: Critical Habitats (long list)
      card(
        class = "wide-layout-species-card", # <--- NEW CLASS HERE
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
    )
  ), # END Initial State
  
  # =========================================================
  # 2. ACTIVE STATE (Sidebar + Wide Table Area)
  #    Condition: input.preview > 0 (Button clicked)
  # =========================================================
  conditionalPanel(
    condition = "input.preview > 0",
    
    layout_sidebar(
      fillable = TRUE, 
      
      # --- Sidebar for Controls (skinny column) ---
      sidebar = sidebar(
        width = 400, 
        position = "left",
        
        # 1. Table Organization (New Top)
        card(
          card_header("Table Organization"),
          checkboxInput("BySpecies", label = "Create one table of all PBFs by species"),
          checkboxInput("ByStressors", label = "Create tables of PBFs per stressor/category"),
        ),
        
        # 2. Table Preview
        card(
          card_header("Table Preview"),
          # Button to UPDATE the table
          actionButton("preview_update", label = "Update Table(s)"), 
        ),
        
        # 3. Downloads
        card(
          card_header("Downloads"),
          downloadButton("downloadxlsx", label = "Download .xlsx"),
          downloadButton("downloaddocx", label = "Download .docx"),
        ),
        
        # 4. Critical Habitats (Long list, now at the bottom of the stack)
        card(
          class = "scrollable-card", # <--- USES EXISTING CLASS
          card_header("Critical Habitats"),
          checkboxGroupInput("checkSpecies", label = NULL, choices = unique(PBFs$Species_full)),
        )
      ),
      
      # --- Main Content Area (Wide Table) ---
      card(
        full_screen = TRUE, 
        height = "100%", 
        card_header("PBF Table(s)"),
        DT::dataTableOutput("PBFtable")
      )
    )
  ) # END Active State
)

# create table by species ONLY IF that box is checked
# create table by stressors ONLY IF that box is checked

# put all PBFs in one cell (not separate rows) # format PBFs as bulletted (for Word version)
# want to combine species in a row if all the PBFs are the same


# --- Data Preparation (Place outside server function if PBFs is a global object) ---
PBFs <- PBFs[order(PBFs$Species_full), ]
# ---------------------------------------------------------------------------------

# Define server logic ----
server <- function(input, output) {
  
  # 1. Create a dynamic reactive trigger based on EITHER button being clicked
  # The trigger value itself is not used, only its reactivity
  table_trigger <- reactive({
    # Listen to the initial button OR the update button
    input$preview
    input$preview_update
    
    # Return the maximum click count to ensure it only runs once per click
    max(input$preview, input$preview_update)
  })
  
  # 2. Filtered data that ONLY runs when the table_trigger changes
  filtered_data <- eventReactive(table_trigger(), {
    
    # The filtering logic remains the same
    if (is.null(input$checkSpecies)) {
      return(PBFs[0, c("Species_full", "PBF")]) 
    }
    
    PBFs[PBFs$Species_full %in% input$checkSpecies, c("Species_full", "PBF")]
  })
  
  # 3. Data Rendering
  output$PBFtable <- DT::renderDataTable({
    
    DT::datatable(
      data = filtered_data(),
      rownames = FALSE,
      options = list(
        pageLength = 100, 
        lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All'))
      )
    )
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)