#PBFs <- read.csv("data/PBFs for Shiny app test.csv")
PBFs <- read.csv("C:/Users/elizabeth.keller/Documents/GitHub/PBF-Shiny-app/data/All PBFs - Stressor Categories.csv") # updated file

# data wrangling to make it the right format
# creates species+ESU/DPS; no separator if no ESU/DPS
PBFs$Species_full <- ifelse(
  is.na(PBFs$ESU_DPS) | PBFs$ESU_DPS == "",
  PBFs$Species,
  paste0(PBFs$Species, " – ", PBFs$ESU_DPS)
)
# Sort the PBFs data frame alphabetically by Species_full
PBFs <- PBFs[order(PBFs$Species_full), ]



# row format for Species tables
library(dplyr)

# Define the species you want to keep
selected_species <- c("Atlantic Salmon – Gulf of Maine DPS","Atlantic Sturgeon – Carolina DPS", "Atlantic Sturgeon – Chesapeake Bay DPS") # test species


library(dplyr)
library(flextable)

# 1. Prepare data with the bullet character
final_table_formatted <- PBFs %>%
  filter(Species_full %in% selected_species) %>%
  group_by(Species_full) %>%
  summarize(
    # \u2022 is the bullet, \t is a tab (Word recognizes this!)
    PBF_text = paste0("\u2022\t", unique(PBF), collapse = "\n"), 
    .groups = "drop"
  )

# 2. Create the Flextable
ft <- flextable(final_table_formatted) %>%
  # Basic formatting
  theme_booktabs() %>%
  valign(j = "PBF_text", valign = "top") %>%
  
  # Set the header names
  set_header_labels(Species_full = "Species", PBF_text = "PBF") %>%
  
  # Control the width so Word has room to wrap the text
  width(j = "Species_full", width = 2) %>%
  width(j = "PBF_text", width = 4.5) %>%
  
  # Add simple padding to the cell for a clean look
  padding(j = "PBF_text", padding.left = 10, part = "body")

# 3. View the table
ft

# let's see how it looks in Excel
# Install the package if you haven't already
# install.packages("writexl")
library(writexl)

# Save the table
write_xlsx(final_table, "Species_PBF_Report.xlsx")

#RMarkdown format with bullets


# ***add PBF categories?!!


# row format for Stressors/Categories tables
# *need to ALSO filter by species here

# 1. Define the category you want to subset by
selected_category <- "Food" # Replace with your actual column name

# 2. Process the dataframe
category_table <- PBFs %>%
  # Keep only the species in your list
  filter(Species_full %in% selected_species) %>%
  # Use filter with .data[[ ]] to handle the column name as a variable
  filter(.data[[selected_category]] == 1) %>%
  # Group by PBF value
  group_by(PBF) %>%
  # Collapse all Species sharing that PBF into one cell
  summarize(Species_full = paste(unique(Species_full), collapse = "\n"), .groups = "drop") %>%
  # Reorder columns to match your desired output
  select(Species_full, PBF)

# Save the single category table
write_xlsx(category_table, "Food_PBF_Report.xlsx")




# automate for a list of stressors/categories
# current_stressor_selections **is this the right value for list of stressors?
library(dplyr)
library(purrr) # Part of tidyverse, great for lists

# 1. Define your list of categories
target_categories <- c("Category_A", "Category_C", "Category_E")

# 2. Create a function to process a single category
get_pbf_summary <- function(cat_name, df) {
  df %>%
    filter(.data[[cat_name]] == 1) %>%
    group_by(PBF) %>%
    summarize(Species = paste(unique(Species), collapse = ", "), .groups = "drop") %>%
    select(Species, PBF)
}

# 3. Use 'map' to apply this function to your list of categories
# This creates a list where each element is one of your tables
all_tables <- set_names(target_categories) %>% 
  map(~get_pbf_summary(.x, your_dataframe))

# 4. Access your tables by name
# For example, to see the table for Category_A:
print(all_tables[["Category_A"]])