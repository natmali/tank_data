library(tidyverse)
library(dplyr)


# Tidying Data with 'R' 
# Working on our qPCR Data 

library(readxl)
watercorp_qpcr_data <- read_xlsx("data/watercorp_tanks_qpcr_raw_data.xlsx")
view(watercorp_qpcr_data)


## Things that I need to do 
#1 Change the names of the tanks to match the waresh dataset 
#2 Carry down the information in most of the columns (remove the 'merge')
#3 Rename 'Sample' to 'Sample Type' 
#4 Remove unnecessary columns 
#5 Sort out the missing sample types 

# Renaming the column names 
watercorp_qpcr_data <- watercorp_qpcr_data %>% 
  rename(tank = "Tank", sample_date = "Date", temperature = "Temperature", conductivity = "Cond", free_chlorine = "FCL", total_chlorine = "TCL", sample_type = "Sample") %>% 
  view()

#Changing the tank names
watercorp_qpcr_data$tank <- replace(watercorp_qpcr_data$tank, watercorp_qpcr_data$tank == "Raymond Road, Walliston LHL Tanks 1 + 2", "Walliston LHL")
watercorp_qpcr_data$tank <- replace(watercorp_qpcr_data$tank, watercorp_qpcr_data$tank == "Weston Road Tank 2", "Werton")
watercorp_qpcr_data$tank <- replace(watercorp_qpcr_data$tank, watercorp_qpcr_data$tank == "Waterwheel Road Tank", "Waterwhell")
watercorp_qpcr_data$tank <- replace(watercorp_qpcr_data$tank, watercorp_qpcr_data$tank == "West Terrace Tank", "West Terrace")
watercorp_qpcr_data$tank <- replace(watercorp_qpcr_data$tank, watercorp_qpcr_data$tank == "Carawartha Tank", "Carawarth")
watercorp_qpcr_data$tank <- replace(watercorp_qpcr_data$tank, watercorp_qpcr_data$tank == "Armadale UHL Tank - Spinebill Cl", "Armadale")
watercorp_qpcr_data$tank <- replace(watercorp_qpcr_data$tank, watercorp_qpcr_data$tank == "Melville French Road Tank", "French")
watercorp_qpcr_data$tank <- replace(watercorp_qpcr_data$tank, watercorp_qpcr_data$tank == "Raymond Road Tank, Walliston", "Walliston")
watercorp_qpcr_data$tank <- replace(watercorp_qpcr_data$tank, watercorp_qpcr_data$tank == "Canning Road Tank, Lesmurdie", "Lesmurdie")

#Getting rid of the 'merge' function and removing unnecessary columns 
watercorp_qpcr_data <- watercorp_qpcr_data %>% 
  fill(tank, sample_date, temperature, conductivity, free_chlorine, total_chlorine) %>% # Filling in NA's with the name above it 
  select(-"Label on Tubes", -"Comment", -"Balmuthia-qPCR...14", -"Balmuthia-qPCR...22") %>% 
  view()

  


# NNA qPCR column names 
watercorp_qpcr_data <- watercorp_qpcr_data %>% 
  rename(NNA_42_degrees = "Growth on NNA at 42 degrees", NNA_30_degrees = "Growth on NNA at 30 degrees", NNA_naegleria_general = "General primers qPCR...11", NNA_naegleria_specific = "Specific primers qPCR...12", NNA_acanthamoeba_spp = "Acanthamoebae-qPCR...13", NNA_NTM = "NTM-qPCR...15", NNA_meiothermus = "Meio-qPCR", NNA_legionella_spp = "Legionella-qPCR...17") %>% 
  view()

watercorp_qpcr_data$NNA_30_degrees <- replace(watercorp_qpcr_data$NNA_30_degrees, watercorp_qpcr_data$NNA_30_degrees == "POS (scraped)", "POS")
watercorp_qpcr_data$NNA_naegleria_general <- replace(watercorp_qpcr_data$NNA_naegleria_general, watercorp_qpcr_data$NNA_naegleria_general == "POS (2/3) Vermamoeba", "VERMAMOEBA")
watercorp_qpcr_data$NNA_acanthamoeba_spp <- replace(watercorp_qpcr_data$NNA_acanthamoeba_spp, watercorp_qpcr_data$NNA_acanthamoeba_spp == "POS (3/3) 1 Peak (83) (Sanger)", "POS")
watercorp_qpcr_data$NNA_NTM <- replace(watercorp_qpcr_data$NNA_NTM, watercorp_qpcr_data$NNA_NTM == "Possible", "POS")


view(watercorp_qpcr_data)



	





  
