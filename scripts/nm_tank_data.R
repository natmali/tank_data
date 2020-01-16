install.packages("tidyverse")
library(tidyverse)

# Tidying Data with 'R' 
# Working on my qPCR Data

library(readxl)
nm_qpcr_data <- read_xlsx("data/watercorp_tanks_qpcr_raw_data.xlsx")

view(nm_qpcr_data)

