install.packages("tidyverse")
library(tidyverse)

# Excel Tidied Data
tanks_data <- read.csv("data/watercorp_tanks_qpcr_raw_data.csv")
view(tanks_data)

# Tidying Data with 'R' 
# Working on Waresh's qPCR Data 

library(readxl)
waresh_qpcr_data <- read_xlsx("data/waresh_sequencing_data.xlsx")
waresh_qpcr_data

# Combinging in multiple sheets from the excel file 
excel_sheets("data/waresh_sequencing_data.xlsx" ) %>% 
  map_df(~read_xlsx("data/waresh_sequencing_data.xlsx",.)) # Combining everything into a single table 

waresh_sheets_df <- excel_sheets("data/waresh_sequencing_data.xlsx" ) %>% 
  map(~read_xlsx("data/waresh_sequencing_data.xlsx",.)) # Making each sheet into a seperate table 

waresh_sheets_df


# Cleaning up each of the sheets 
library(dplyr)

# Table 1
waresh_e_coli_df <- waresh_sheets_df[1] %>% # Assigning the first tibble to its own variable 
 bind_rows(waresh_e_coli) %>% # Creating a tibble out of a list
  rename(ID = "SAMPLE ID") %>%  # Renaming the column to ease use 
  select(ID, Ct) %>% # Selecting for just the two columns
  rename(waresh_e_coli = Ct) %>% # Renaming the Ct column
  mutate(waresh_e_coli = toupper(waresh_e_coli)) %>% # Putting all of the words into capitals
  mutate(waresh_e_coli = if_else(waresh_e_coli != "NEG", "POS", "NEG")) %>%  # Replacing any values with 'POS' 
  fill(ID) %>% # Filling in NA's with the name above it 
  mutate(waresh_e_coli = as_factor(waresh_e_coli)) %>% # Changing the column from characters to factors 
  slice(1:162, .preserve = FALSE) %>% # Cutting out the blanks at the bottom of the page
  view()

# Changing all of the French Road (5A - B) Rows 
waresh_e_coli_df <- fix(waresh_e_coli_df) 
waresh_e_coli_df$ID <- replace(waresh_e_coli_df$ID, waresh_e_coli_df$ID == "French 5A", "French 5")
waresh_e_coli_df$ID <- replace(waresh_e_coli_df$ID, waresh_e_coli_df$ID == "French 5B", "French 5")
waresh_e_coli_df$ID <- replace(waresh_e_coli_df$ID, waresh_e_coli_df$ID == "French 5C", "French 5")

view(waresh_e_coli_df)
  
# Combining the replicates 
waresh_e_coli_df <- waresh_e_coli_df %>% 
  group_by(ID) %>% 
  summarise(waresh_e_coli = any(waresh_e_coli == "POS")) %>% # Dealing with both POS and NEG's
  mutate(waresh_e_coli = if_else(waresh_e_coli == "TRUE", "POS", "NEG")) %>% 
  view()

view(waresh_e_coli_df)

## Trying to repicate with the second table -> Enterococci

waresh_ent_df <- waresh_sheets_df[2] %>% # Assigning the first tibble to its own variable 
  bind_rows(waresh_ent_df) %>%  # Creating a tibble out of a list
  rename(ID = "SAMPLE ID") %>%  # Renaming the column to ease use 
  select(ID, Ct) %>% # Selecting for just the two columns
  rename(waresh_ent = Ct) %>% # Renaming the Ct column
  mutate(waresh_ent = toupper(waresh_ent)) %>% # Putting all of the words into capitals
  mutate(waresh_ent = if_else(waresh_ent != "NEG", "POS", "NEG")) %>%  # Replacing any values with 'POS' 
  fill(ID) %>% # Filling in NA's with the name above it 
  mutate(waresh_ent = as_factor(waresh_ent)) %>% # Changing the column from characters to factors 
  slice(1:162, .preserve = FALSE) %>% # Cutting out the blanks at the bottom of the page
  view()

# Changing all of the French Road (5A - B) Rows 
waresh_ent_df <- fix(waresh_e_coli_df) 
waresh_ent_df$ID <- replace(waresh_ent_df$ID, waresh_ent_df$ID == "French 5A", "French 5")
waresh_ent_df$ID <- replace(waresh_ent_df$ID, waresh_ent_df$ID == "French 5B", "French 5")
waresh_ent_df$ID <- replace(waresh_ent_df$ID, waresh_ent_df$ID == "French 5C", "French 5")

view(waresh_ent_df)

# Combining the replicates 
waresh_ent_df <- waresh_ent_df %>% 
  group_by(ID) %>% 
  summarise(waresh_ent = any(waresh_ent == "POS")) %>% # Dealing with both POS and NEG's
  mutate(waresh_ent = if_else(waresh_ent == "TRUE", "POS", "NEG")) %>% 
  view()

## Combinging the E.coli and Enterococci data frames together

waresh_ecoli_ent <- inner_join(waresh_e_coli_df, waresh_ent_df)

view(waresh_ecoli_ent)

# Apply to all other sheets
# Combine all of the sheets together 


