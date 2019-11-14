library(tidyverse)
library(dplyr)
library(readxl)

waresh_qpcr_tidied_data
watercorp_qpcr_tidied_data


## Combinging the data frames together

tanks_qpcr_combined_data <- inner_join(watercorp_qpcr_tidied_data, waresh_qpcr_tidied_data)

view(tanks_qpcr_combined_data)
