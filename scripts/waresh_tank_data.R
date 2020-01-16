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






# Table 1 -> E.coli 
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






## Table 2 -> Enterococci

waresh_ent_df <- waresh_sheets_df[2] %>% # Assigning the tibble to its own variable 
  bind_rows() %>%  # Creating a tibble out of a list
  rename(ID = "SAMPLE ID") %>%  # Renaming the column to ease use 
  select(ID, Ct) %>% # Selecting for just the two columns
  rename(waresh_enterococcus = Ct) %>% # Renaming the Ct column
  mutate(waresh_enterococcus = toupper(waresh_enterococcus)) %>% # Putting all of the words into capitals
  mutate(waresh_enterococcus = if_else(waresh_enterococcus != "NEG", "POS", "NEG")) %>%  # Replacing any values with 'POS' 
  fill(ID) %>% # Filling in NA's with the name above it 
  mutate(waresh_enterococcus = as_factor(waresh_enterococcus)) %>% # Changing the column from characters to factors 
  slice(1:162, .preserve = FALSE) %>% # Cutting out the blanks at the bottom of the page
  view()

# Changing all of the French Road (5A - B) Rows 
waresh_ent_df <- fix(waresh_ent_df) 
waresh_ent_df$ID <- replace(waresh_ent_df$ID, waresh_ent_df$ID == "French 5A", "French 5")
waresh_ent_df$ID <- replace(waresh_ent_df$ID, waresh_ent_df$ID == "French 5B", "French 5")
waresh_ent_df$ID <- replace(waresh_ent_df$ID, waresh_ent_df$ID == "French 5C", "French 5")

view(waresh_ent_df)

# Combining the replicates 
waresh_ent_df <- waresh_ent_df %>% 
  group_by(ID) %>% 
  summarise(waresh_enterococcus = any(waresh_enterococcus == "POS")) %>% # Dealing with both POS and NEG's
  mutate(waresh_enterococcus = if_else(waresh_enterococcus == "TRUE", "POS", "NEG")) %>% 
  view()

# Combinging the data frames together

waresh_ecoli_ent <- inner_join(waresh_e_coli_df, waresh_ent_df)

view(waresh_ecoli_ent)






# Table 3 -> Pseudomonas aeruginosa
waresh_psa_df <- waresh_sheets_df[3] %>% # Assigning the tibble to its own variable 
  bind_rows() %>% # Creating a tibble out of a list
  rename(ID = "SAMPLE ID") %>%  # Renaming the column to ease use 
  select(ID, Ct) %>% # Selecting for just the two columns
  rename(waresh_pseudomonas_aeruginosa = Ct) %>% # Renaming the Ct column
  mutate(waresh_pseudomonas_aeruginosa = toupper(waresh_pseudomonas_aeruginosa)) %>% # Putting all of the words into capitals
  mutate(waresh_pseudomonas_aeruginosa = if_else(waresh_pseudomonas_aeruginosa != "NEG", "POS", "NEG")) %>%  # Replacing any values with 'POS' 
  fill(ID) %>% # Filling in NA's with the name above it 
  mutate(waresh_pseudomonas_aeruginosa = as_factor(waresh_pseudomonas_aeruginosa)) %>% # Changing the column from characters to factors 
  slice(1:162, .preserve = FALSE) %>% # Cutting out the blanks at the bottom of the page
  view()

# Changing all of the French Road (5A - B) Rows 
waresh_psa_df <- fix(waresh_psa_df) 
waresh_psa_df$ID <- replace(waresh_psa_df$ID, waresh_psa_df$ID == "French 5A", "French 5")
waresh_psa_df$ID <- replace(waresh_psa_df$ID, waresh_psa_df$ID == "French 5B", "French 5")
waresh_psa_df$ID <- replace(waresh_psa_df$ID, waresh_psa_df$ID == "French 5C", "French 5")

view(waresh_psa_df)

# Combining the replicates 
waresh_psa_df <- waresh_psa_df %>% 
  group_by(ID) %>% 
  summarise(waresh_pseudomonas_aeruginosa = any(waresh_pseudomonas_aeruginosa == "POS")) %>% # Dealing with both POS and NEG's
  mutate(waresh_pseudomonas_aeruginosa = if_else(waresh_pseudomonas_aeruginosa == "TRUE", "POS", "NEG")) %>% 
  view()

## Combinging the data frames together

waresh_ecoli_ent_psa <- inner_join(waresh_ecoli_ent, waresh_psa_df)

view(waresh_ecoli_ent_psa)






# Table 4 -> Mycobacterium In tracellulare
waresh_MI_df <- waresh_sheets_df[4] %>% # Assigning the tibble to its own variable 
  bind_rows() %>% # Creating a tibble out of a list
  rename(ID = "SAMPLE ID") %>%  # Renaming the column to ease use 
  select(ID, Ct) %>% # Selecting for just the two columns
  rename(waresh_mycobacterium_intracellulare = Ct) %>% # Renaming the Ct column
  mutate(waresh_mycobacterium_intracellulare = toupper(waresh_mycobacterium_intracellulare)) %>% # Putting all of the words into capitals
  mutate(waresh_mycobacterium_intracellulare = if_else(waresh_mycobacterium_intracellulare != "NEG", "POS", "NEG")) %>%  # Replacing any values with 'POS' 
  fill(ID) %>% # Filling in NA's with the name above it 
  mutate(waresh_mycobacterium_intracellulare = as_factor(waresh_mycobacterium_intracellulare)) %>% # Changing the column from characters to factors 
  slice(1:162, .preserve = FALSE) %>% # Cutting out the blanks at the bottom of the page
  view()

# Changing all of the French Road (5A - B) Rows 
waresh_MI_df <- fix(waresh_psa_df) 
waresh_MI_df$ID <- replace(waresh_MI_df$ID, waresh_MI_df$ID == "French 5A", "French 5")
waresh_MI_df$ID <- replace(waresh_MI_df$ID, waresh_MI_df$ID == "French 5B", "French 5")
waresh_MI_df$ID <- replace(waresh_MI_df$ID, waresh_MI_df$ID == "French 5C", "French 5")

view(waresh_MI_df)

# Combining the replicates 
waresh_MI_df <- waresh_MI_df %>% 
  group_by(ID) %>% 
  summarise(waresh_mycobacterium_intracellulare = any(waresh_mycobacterium_intracellulare == "POS")) %>% # Dealing with both POS and NEG's
  mutate(waresh_mycobacterium_intracellulare = if_else(waresh_mycobacterium_intracellulare == "TRUE", "POS", "NEG")) %>% 
  view()

## Combinging the data frames together

waresh_ecoli_ent_psa_MI <- inner_join(waresh_ecoli_ent_psa, waresh_MI_df)

view(waresh_ecoli_ent_psa_MI)








# Table 5 -> Mycobcterium Avium
waresh_MA_df <- waresh_sheets_df[5] %>% # Assigning the tibble to its own variable 
  bind_rows() %>% # Creating a tibble out of a list
  rename(ID = "SAMPLE ID") %>%  # Renaming the column to ease use 
  select(ID, Ct) %>% # Selecting for just the two columns
  rename(waresh_mycobacterium_avium = Ct) %>% # Renaming the Ct column
  mutate(waresh_mycobacterium_avium = toupper(waresh_mycobacterium_avium)) %>% # Putting all of the words into capitals
  mutate(waresh_mycobacterium_avium = if_else(waresh_mycobacterium_avium != "NEG", "POS", "NEG")) %>%  # Replacing any values with 'POS' 
  fill(ID) %>% # Filling in NA's with the name above it 
  mutate(waresh_mycobacterium_avium = as_factor(waresh_mycobacterium_avium)) %>% # Changing the column from characters to factors 
  slice(1:162, .preserve = FALSE) %>% # Cutting out the blanks at the bottom of the page
  view()

# Changing all of the French Road (5A - B) Rows 
waresh_MA_df$ID <- replace(waresh_MA_df$ID, waresh_MA_df$ID == "French 5A", "French 5")
waresh_MA_df$ID <- replace(waresh_MA_df$ID, waresh_MA_df$ID == "French 5B", "French 5")
waresh_MA_df$ID <- replace(waresh_MA_df$ID, waresh_MA_df$ID == "French 5C", "French 5")

view(waresh_MA_df)

# Combining the replicates 
waresh_MA_df <- waresh_MA_df %>% 
  group_by(ID) %>% 
  summarise(waresh_mycobacterium_avium = any(waresh_mycobacterium_avium == "POS")) %>% # Dealing with both POS and NEG's
  mutate(waresh_mycobacterium_avium = if_else(waresh_mycobacterium_avium == "TRUE", "POS", "NEG")) %>% 
  view()

## Combinging the data frames together

waresh_ecoli_ent_psa_MI_MA <- inner_join(waresh_ecoli_ent_psa_MI, waresh_MA_df)

view(waresh_ecoli_ent_psa_MI_MA)







# Table 6 -> Legionella spp. 
waresh_lspp_df <- waresh_sheets_df[6] %>% # Assigning the tibble to its own variable 
  bind_rows() %>% # Creating a tibble out of a list
  rename(ID = "SAMPLE ID") %>%  # Renaming the column to ease use 
  select(ID, Ct) %>% # Selecting for just the two columns
  rename(waresh_legionella_spp = Ct) %>% # Renaming the Ct column
  mutate(waresh_legionella_spp = toupper(waresh_legionella_spp)) %>% # Putting all of the words into capitals
  mutate(waresh_legionella_spp = if_else(waresh_legionella_spp != "NEG", "POS", "NEG")) %>%  # Replacing any values with 'POS' 
  fill(ID) %>% # Filling in NA's with the name above it 
  mutate(waresh_legionella_spp = as_factor(waresh_legionella_spp)) %>% # Changing the column from characters to factors 
  slice(1:162, .preserve = FALSE) %>% # Cutting out the blanks at the bottom of the page
  view()

# Changing all of the French Road (5A - B) Rows 
waresh_lspp_df$ID <- replace(waresh_lspp_df$ID, waresh_lspp_df$ID == "French 5A", "French 5")
waresh_lspp_df$ID <- replace(waresh_lspp_df$ID, waresh_lspp_df$ID == "French 5B", "French 5")
waresh_lspp_df$ID <- replace(waresh_lspp_df$ID, waresh_lspp_df$ID == "French 5C", "French 5")

view(waresh_lspp_df)

# Combining the replicates 
waresh_lspp_df <- waresh_lspp_df %>% 
  group_by(ID) %>% 
  summarise(waresh_legionella_spp = any(waresh_legionella_spp == "POS")) %>% # Dealing with both POS and NEG's
  mutate(waresh_legionella_spp = if_else(waresh_legionella_spp == "TRUE", "POS", "NEG")) %>% 
  view()

## Combinging the data frames together

waresh_ecoli_ent_psa_MI_MA_lspp <- inner_join(waresh_ecoli_ent_psa_MI_MA, waresh_lspp_df)

view(waresh_ecoli_ent_psa_MI_MA_lspp)





# Table 6 -> Legionella spp. 
waresh_lspp_df <- waresh_sheets_df[6] %>% # Assigning the tibble to its own variable 
  bind_rows() %>% # Creating a tibble out of a list
  rename(ID = "SAMPLE ID") %>%  # Renaming the column to ease use 
  select(ID, Ct) %>% # Selecting for just the two columns
  rename(waresh_legionella_spp = Ct) %>% # Renaming the Ct column
  mutate(waresh_legionella_spp = toupper(waresh_legionella_spp)) %>% # Putting all of the words into capitals
  mutate(waresh_legionella_spp = if_else(waresh_legionella_spp != "NEG", "POS", "NEG")) %>%  # Replacing any values with 'POS' 
  fill(ID) %>% # Filling in NA's with the name above it 
  mutate(waresh_legionella_spp = as_factor(waresh_legionella_spp)) %>% # Changing the column from characters to factors 
  slice(1:162, .preserve = FALSE) %>% # Cutting out the blanks at the bottom of the page
  view()

# Changing all of the French Road (5A - B) Rows 
waresh_lspp_df$ID <- replace(waresh_lspp_df$ID, waresh_lspp_df$ID == "French 5A", "French 5")
waresh_lspp_df$ID <- replace(waresh_lspp_df$ID, waresh_lspp_df$ID == "French 5B", "French 5")
waresh_lspp_df$ID <- replace(waresh_lspp_df$ID, waresh_lspp_df$ID == "French 5C", "French 5")

view(waresh_lspp_df)

# Combining the replicates 
waresh_lspp_df <- waresh_lspp_df %>% 
  group_by(ID) %>% 
  summarise(waresh_legionella_spp = any(waresh_legionella_spp == "POS")) %>% # Dealing with both POS and NEG's
  mutate(waresh_legionella_spp = if_else(waresh_legionella_spp == "TRUE", "POS", "NEG")) %>% 
  view()

## Combinging the data frames together

waresh_ecoli_ent_psa_MI_MA_lspp <- inner_join(waresh_ecoli_ent_psa_MI_MA, waresh_lspp_df)

view(waresh_ecoli_ent_psa_MI_MA_lspp)






# Table 7 -> Legionella pneumophila 
waresh_lpneu_df <- waresh_sheets_df[7] %>% # Assigning the tibble to its own variable 
  bind_rows() %>% # Creating a tibble out of a list
  rename(ID = "SAMPLE ID") %>%  # Renaming the column to ease use 
  select(ID, Ct) %>% # Selecting for just the two columns
  rename(waresh_legionella_pneumophila = Ct) %>% # Renaming the Ct column
  mutate(waresh_legionella_pneumophila = toupper(waresh_legionella_pneumophila)) %>% # Putting all of the words into capitals
  mutate(waresh_legionella_pneumophila = if_else(waresh_legionella_pneumophila != "NEG", "POS", "NEG")) %>%  # Replacing any values with 'POS' 
  fill(ID) %>% # Filling in NA's with the name above it 
  mutate(waresh_legionella_pneumophila = as_factor(waresh_legionella_pneumophila)) %>% # Changing the column from characters to factors 
  slice(1:162, .preserve = FALSE) %>% # Cutting out the blanks at the bottom of the page
  view()

# Changing all of the French Road (5A - B) Rows 
waresh_lpneu_df$ID <- replace(waresh_lpneu_df$ID, waresh_lpneu_df$ID == "French 5A", "French 5")
waresh_lpneu_df$ID <- replace(waresh_lpneu_df$ID, waresh_lpneu_df$ID == "French 5B", "French 5")
waresh_lpneu_df$ID <- replace(waresh_lpneu_df$ID, waresh_lpneu_df$ID == "French 5C", "French 5")

view(waresh_lpneu_df)

# Combining the replicates 
waresh_lpneu_df <- waresh_lpneu_df %>% 
  group_by(ID) %>% 
  summarise(waresh_legionella_pneumophila = any(waresh_legionella_pneumophila == "POS")) %>% # Dealing with both POS and NEG's
  mutate(waresh_legionella_pneumophila = if_else(waresh_legionella_pneumophila == "TRUE", "POS", "NEG")) %>% 
  view()

## Combinging the data frames together

waresh_ecoli_ent_psa_MI_MA_lspp_lpneu <- inner_join(waresh_ecoli_ent_psa_MI_MA_lspp, waresh_lpneu_df)

view(waresh_ecoli_ent_psa_MI_MA_lspp_lpneu)







# Table 8 -> Acanthamoeba spp. 
waresh_acanth_df <- waresh_sheets_df[8] %>% # Assigning the tibble to its own variable 
  bind_rows() %>% # Creating a tibble out of a list
  rename(ID = "SAMPLE ID") %>%  # Renaming the column to ease use 
  select(ID, Ct) %>% # Selecting for just the two columns
  rename(waresh_acanthamoeba_spp = Ct) %>% # Renaming the Ct column
  mutate(waresh_acanthamoeba_spp = toupper(waresh_acanthamoeba_spp)) %>% # Putting all of the words into capitals
  mutate(waresh_acanthamoeba_spp = if_else(waresh_acanthamoeba_spp != "NEG", "POS", "NEG")) %>%  # Replacing any values with 'POS' 
  fill(ID) %>% # Filling in NA's with the name above it 
  mutate(waresh_acanthamoeba_spp = as_factor(waresh_acanthamoeba_spp)) %>% # Changing the column from characters to factors 
  slice(1:162, .preserve = FALSE) %>% # Cutting out the blanks at the bottom of the page
  view()

# Changing all of the French Road (5A - B) Rows 
waresh_acanth_df$ID <- replace(waresh_acanth_df$ID, waresh_acanth_df$ID == "French 5A", "French 5")
waresh_acanth_df$ID <- replace(waresh_acanth_df$ID, waresh_acanth_df$ID == "French 5B", "French 5")
waresh_acanth_df$ID <- replace(waresh_acanth_df$ID, waresh_acanth_df$ID == "French 5C", "French 5")

view(waresh_acanth_df)

# Combining the replicates 
waresh_acanth_df <- waresh_acanth_df %>% 
  group_by(ID) %>% 
  summarise(waresh_acanthamoeba_spp = any(waresh_acanthamoeba_spp == "POS")) %>% # Dealing with both POS and NEG's
  mutate(waresh_acanthamoeba_spp = if_else(waresh_acanthamoeba_spp == "TRUE", "POS", "NEG")) %>% 
  view()

## Combinging the data frames together

waresh_ecoli_ent_psa_MI_MA_lspp_lpneu_acanth <- inner_join(waresh_ecoli_ent_psa_MI_MA_lspp_lpneu, waresh_acanth_df)

view(waresh_ecoli_ent_psa_MI_MA_lspp_lpneu_acanth)


## Final tidied dataset!! 
waresh_qpcr_tidied_data <- waresh_ecoli_ent_psa_MI_MA_lspp_lpneu_acanth

waresh_qpcr_tidied_data <- waresh_qpcr_tidied_data %>% 
  rename(tank = "ID") %>% 
  view()

waresh_qpcr_tidied_data$tank <- replace(waresh_qpcr_tidied_data$tank, waresh_qpcr_tidied_data$tank == "Walliston LHL 1", "WallistonLHL 1")
waresh_qpcr_tidied_data$tank <- replace(waresh_qpcr_tidied_data$tank, waresh_qpcr_tidied_data$tank == "Walliston LHL 2", "WallistonLHL 2")
waresh_qpcr_tidied_data$tank <- replace(waresh_qpcr_tidied_data$tank, waresh_qpcr_tidied_data$tank == "Walliston LHL 3", "WallistonLHL 3")
waresh_qpcr_tidied_data$tank <- replace(waresh_qpcr_tidied_data$tank, waresh_qpcr_tidied_data$tank == "Walliston LHL 4", "WallistonLHL 4")

waresh_qpcr_tidied_data$tank <- replace(waresh_qpcr_tidied_data$tank, waresh_qpcr_tidied_data$tank == "West Terrace 1", "WestTerrace 1")
waresh_qpcr_tidied_data$tank <- replace(waresh_qpcr_tidied_data$tank, waresh_qpcr_tidied_data$tank == "West Terrace 2", "WestTerrace 2")
waresh_qpcr_tidied_data$tank <- replace(waresh_qpcr_tidied_data$tank, waresh_qpcr_tidied_data$tank == "West Terrace 3", "WestTerrace 3")
waresh_qpcr_tidied_data$tank <- replace(waresh_qpcr_tidied_data$tank, waresh_qpcr_tidied_data$tank == "West Terrace 4", "WestTerrace 4")
waresh_qpcr_tidied_data$tank <- replace(waresh_qpcr_tidied_data$tank, waresh_qpcr_tidied_data$tank == "West Terrace 5", "WestTerrace 5")
waresh_qpcr_tidied_data$tank <- replace(waresh_qpcr_tidied_data$tank, waresh_qpcr_tidied_data$tank == "West Terrace 6", "WestTerrace 6")

view(waresh_qpcr_tidied_data)

waresh_qpcr_tidied_data <- waresh_qpcr_tidied_data %>% 
  separate(tank, into = c("tank", "replicate"), convert = TRUE) %>% 
  view()











