library(tidyverse)
library(dplyr)


# Tidying Data with 'R' 
# Working on our qPCR Data 

library(readxl)
watercorp_qpcr_data <- read_xlsx("data/watercorp_tanks_qpcr_raw_data.xlsx") %>% 
view(watercorp_qpcr_data)



# Renaming the column names 
watercorp_qpcr_data <- watercorp_qpcr_data %>% 
  rename(tank = "Tank", sample_date = "Date", temperature = "Temperature", conductivity = "Cond", free_chlorine = "FCL", total_chlorine = "TCL", sample_type = "Sample", replicate = "Replicate") %>% 
  view()

#Changing the tank names
watercorp_qpcr_data$tank <- replace(watercorp_qpcr_data$tank, watercorp_qpcr_data$tank == "Raymond Road, Walliston LHL Tanks 1 + 2", "RRTLHL")
watercorp_qpcr_data$tank <- replace(watercorp_qpcr_data$tank, watercorp_qpcr_data$tank == "Weston Road Tank 2", "WT")
watercorp_qpcr_data$tank <- replace(watercorp_qpcr_data$tank, watercorp_qpcr_data$tank == "Waterwheel Road Tank", "WWRT")
watercorp_qpcr_data$tank <- replace(watercorp_qpcr_data$tank, watercorp_qpcr_data$tank == "West Terrace Tank", "WTT")
watercorp_qpcr_data$tank <- replace(watercorp_qpcr_data$tank, watercorp_qpcr_data$tank == "Carawartha Tank", "CT")
watercorp_qpcr_data$tank <- replace(watercorp_qpcr_data$tank, watercorp_qpcr_data$tank == "Armadale UHL Tank - Spinebill Cl", "AT")
watercorp_qpcr_data$tank <- replace(watercorp_qpcr_data$tank, watercorp_qpcr_data$tank == "Melville French Road Tank", "FRT")
watercorp_qpcr_data$tank <- replace(watercorp_qpcr_data$tank, watercorp_qpcr_data$tank == "Raymond Road Tank, Walliston", "RRT")
watercorp_qpcr_data$tank <- replace(watercorp_qpcr_data$tank, watercorp_qpcr_data$tank == "Canning Road Tank, Lesmurdie", "CRT")

#Getting rid of the 'merge' function and removing unnecessary columns 
watercorp_qpcr_data <- watercorp_qpcr_data %>% 
  fill(tank, sample_date, temperature, conductivity, free_chlorine, total_chlorine) %>% # Filling in NA's with the name above it 
  select(-"Label on Tubes", -"Comment", -"Balmuthia-qPCR...15", -"Balmuthia-qPCR...23") %>% 
  slice(1:53, .preserve = FALSE) %>% # Cutting out the blanks at the bottom of the page
  view()




# NNA qPCR column names 
watercorp_qpcr_data <- watercorp_qpcr_data %>% 
  rename(NNA_42_degrees = "Growth on NNA at 42 degrees", NNA_30_degrees = "Growth on NNA at 30 degrees", NNA_naegleria_spp = "General primers qPCR...12", NNA_naegleria_fowleri = "Specific primers qPCR...13", NNA_acanthamoeba_spp = "Acanthamoebae-qPCR...14", NNA_NTM = "NTM-qPCR...16", NNA_meiothermus = "Meio-qPCR", NNA_legionella_spp = "Legionella-qPCR...18") %>% 
  view()

watercorp_qpcr_data$NNA_30_degrees <- replace(watercorp_qpcr_data$NNA_30_degrees, watercorp_qpcr_data$NNA_30_degrees == "POS (scraped)", "POS")
watercorp_qpcr_data$NNA_naegleria_spp <- replace(watercorp_qpcr_data$NNA_naegleria_spp, watercorp_qpcr_data$NNA_naegleria_spp == "POS (2/3) Vermamoeba", "Vermamoeba")
watercorp_qpcr_data$NNA_acanthamoeba_spp <- replace(watercorp_qpcr_data$NNA_acanthamoeba_spp, watercorp_qpcr_data$NNA_acanthamoeba_spp == "POS (3/3) 1 Peak (83) (Sanger)", "POS")
watercorp_qpcr_data$NNA_NTM <- replace(watercorp_qpcr_data$NNA_NTM, watercorp_qpcr_data$NNA_NTM == "Possible", "POS")


view(watercorp_qpcr_data)



# Total DNA qPCR column names 
watercorp_qpcr_data <- watercorp_qpcr_data %>% 
  rename(total_DNA_naegleria_spp = "General primers qPCR...20", total_DNA_naegleria_fowleri = "Specific primers qPCR...21", total_DNA_acanthamoeba_spp = "Acanthamoebae-qPCR...22", total_DNA_NTM = "NTM-qPCR...24", total_DNA_legionella_spp = "Legionella-qPCR...25") %>% 
  view()

watercorp_qpcr_data$total_DNA_naegleria_spp <- replace(watercorp_qpcr_data$total_DNA_naegleria_spp, watercorp_qpcr_data$total_DNA_naegleria_spp == "POS (1/3) N. dobsoni", "N. dobsoni")
watercorp_qpcr_data$total_DNA_naegleria_spp <- replace(watercorp_qpcr_data$total_DNA_naegleria_spp, watercorp_qpcr_data$total_DNA_naegleria_spp == "POS (2/3) N. andersoni (Sanger)", "N. andersoni")
watercorp_qpcr_data$total_DNA_naegleria_spp <- replace(watercorp_qpcr_data$total_DNA_naegleria_spp, watercorp_qpcr_data$total_DNA_naegleria_spp == "POS (3/3) N. andersoni (Sanger)", "N. andersoni")
watercorp_qpcr_data$total_DNA_naegleria_spp <- replace(watercorp_qpcr_data$total_DNA_naegleria_spp, watercorp_qpcr_data$total_DNA_naegleria_spp == "POS (3/3) 4 Peak", "TA")
watercorp_qpcr_data$total_DNA_naegleria_spp <- replace(watercorp_qpcr_data$total_DNA_naegleria_spp, watercorp_qpcr_data$total_DNA_naegleria_spp == "POS (1/3) 4 Peak", "TA")
watercorp_qpcr_data$total_DNA_naegleria_spp <- replace(watercorp_qpcr_data$total_DNA_naegleria_spp, watercorp_qpcr_data$total_DNA_naegleria_spp == "POS (3/3) N. andersoni", "N. andersoni")
watercorp_qpcr_data$total_DNA_naegleria_spp <- replace(watercorp_qpcr_data$total_DNA_naegleria_spp, watercorp_qpcr_data$total_DNA_naegleria_spp == "POS Tetramitus or Vahlkampfia (3/3)", "Tetramitus & Vahlkampfia")
watercorp_qpcr_data$total_DNA_naegleria_spp <- replace(watercorp_qpcr_data$total_DNA_naegleria_spp, watercorp_qpcr_data$total_DNA_naegleria_spp == "POS (2/3) and (1/3)", "TA")


watercorp_qpcr_data$total_DNA_acanthamoeba_spp <- replace(watercorp_qpcr_data$total_DNA_acanthamoeba_spp, watercorp_qpcr_data$total_DNA_acanthamoeba_spp == "POS (2/3) 2 Peak (Sanger)", "POS")
watercorp_qpcr_data$total_DNA_acanthamoeba_spp <- replace(watercorp_qpcr_data$total_DNA_acanthamoeba_spp, watercorp_qpcr_data$total_DNA_acanthamoeba_spp == "POS (1/3) 2 Peak (Sanger)", "POS")
watercorp_qpcr_data$total_DNA_acanthamoeba_spp <- replace(watercorp_qpcr_data$total_DNA_acanthamoeba_spp, watercorp_qpcr_data$total_DNA_acanthamoeba_spp == "POS (2/3) 2 Peak", "POS")
watercorp_qpcr_data$total_DNA_acanthamoeba_spp <- replace(watercorp_qpcr_data$total_DNA_acanthamoeba_spp, watercorp_qpcr_data$total_DNA_acanthamoeba_spp == "POS (2/3) 3 Peak and 4 Peak (Sanger)", "POS")
watercorp_qpcr_data$total_DNA_acanthamoeba_spp <- replace(watercorp_qpcr_data$total_DNA_acanthamoeba_spp, watercorp_qpcr_data$total_DNA_acanthamoeba_spp == "POS (1/3) 1 Peak (82) (Sanger)", "POS")
watercorp_qpcr_data$total_DNA_acanthamoeba_spp <- replace(watercorp_qpcr_data$total_DNA_acanthamoeba_spp, watercorp_qpcr_data$total_DNA_acanthamoeba_spp == "POS (3/3) 3 Peak (Sanger)", "POS")
watercorp_qpcr_data$total_DNA_acanthamoeba_spp <- replace(watercorp_qpcr_data$total_DNA_acanthamoeba_spp, watercorp_qpcr_data$total_DNA_acanthamoeba_spp == "POS (3/3) 4 Peak (Sanger)", "POS")
watercorp_qpcr_data$total_DNA_acanthamoeba_spp <- replace(watercorp_qpcr_data$total_DNA_acanthamoeba_spp, watercorp_qpcr_data$total_DNA_acanthamoeba_spp == "POS (1/3) 1 Peak (80)", "POS")
watercorp_qpcr_data$total_DNA_acanthamoeba_spp <- replace(watercorp_qpcr_data$total_DNA_acanthamoeba_spp, watercorp_qpcr_data$total_DNA_acanthamoeba_spp == "POS (3/3) 1 Peak (82)", "POS")
watercorp_qpcr_data$total_DNA_acanthamoeba_spp <- replace(watercorp_qpcr_data$total_DNA_acanthamoeba_spp, watercorp_qpcr_data$total_DNA_acanthamoeba_spp == "POS (2/3) 1 Peak (91)", "POS")

watercorp_qpcr_data$total_DNA_NTM <- replace(watercorp_qpcr_data$total_DNA_NTM, watercorp_qpcr_data$total_DNA_NTM == "POS (1/3)", "POS")
watercorp_qpcr_data$total_DNA_NTM <- replace(watercorp_qpcr_data$total_DNA_NTM, watercorp_qpcr_data$total_DNA_NTM == "Possible", "POS")
watercorp_qpcr_data$total_DNA_NTM <- replace(watercorp_qpcr_data$total_DNA_NTM, watercorp_qpcr_data$total_DNA_NTM == "Weird MC", "POS")

watercorp_qpcr_data$sample_type <- replace(watercorp_qpcr_data$sample_type, watercorp_qpcr_data$sample_type == "Pre", "bulk_water")
watercorp_qpcr_data$sample_type <- replace(watercorp_qpcr_data$sample_type, watercorp_qpcr_data$sample_type == "Post", "sediment")

view(watercorp_qpcr_data)

watercorp_qpcr_tidied_data_rmd <- watercorp_qpcr_data

# Changing the table for a presentation 

watercorp_qpcr_tidied_data_rmd <- watercorp_qpcr_tidied_data_rmd %>% 
  select(-"sample_date", -"NNA_naegleria_spp", -"NNA_naegleria_fowleri", -"NNA_acanthamoeba_spp", -"NNA_NTM", -"NNA_meiothermus", -"NNA_legionella_spp", - "NNA_42_degrees" , - "NNA_30_degrees", - "replicate") %>% 
  view()

watercorp_qpcr_tidied_data_rmd <- watercorp_qpcr_tidied_data_rmd %>% 
  rename(Tank = "tank", Temperature = "temperature", Conductivity = "conductivity", Free_Chlorine = "free_chlorine", Total_Chlorine = "total_chlorine", Sample_Type = "sample_type", Naegleria_spp = "total_DNA_naegleria_spp" ,  Naegleria_fowleri = "total_DNA_naegleria_fowleri" , Acanthamoeba_spp = "total_DNA_acanthamoeba_spp" , NTM = "total_DNA_NTM" , Legionella_spp = "total_DNA_legionella_spp" ) %>% 
  view()

group_by(watercorp_qpcr_tidied_data_rmd, Tank, Sample_Type) %>% 
  view()

write.csv(watercorp_qpcr_tidied_data_rmd, "data/watercorp_qpcr_tidied_data_rmd.csv")

