library(tidyverse)
library(dplyr)
library(readxl)

waresh_qpcr_tidied_data
watercorp_qpcr_tidied_data


## Combinging the data frames together

tanks_qpcr_combined_data <- inner_join(watercorp_qpcr_tidied_data, waresh_qpcr_tidied_data)

view(tanks_qpcr_combined_data)


## Visualisation 

tanks_qpcr_combined_data %>% 
  ggplot(aes(x = waresh_acanthamoeba_spp,
           y = temperature,
           colour = tank,
           shape = sample_type)) +
  geom_point()
