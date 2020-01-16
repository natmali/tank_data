library(tidyverse)
library(dplyr)
library(readxl)

waresh_qpcr_tidied_data
watercorp_qpcr_tidied_data


## Combinging the data frames together

tanks_qpcr_combined_data <- inner_join(watercorp_qpcr_tidied_data, waresh_qpcr_tidied_data)

view(tanks_qpcr_combined_data)

write_csv(tanks_qpcr_combined_data, "data/tanks_qpcr_combined_data.csv")

## Visualisation 

tanks_qpcr_combined_data %>% 
  ggplot(aes(x = waresh_acanthamoeba_spp,
           y = temperature,
           z = free_chlorine,
           colour = sample_type)) +
  geom_point() +
  facet_wrap(~tank)


tanks_qpcr_combined_data %>% 
  count(tanks_qpcr_combined_data$waresh_acanthamoeba_spp == "POS") %>% 
  group_by(sample_type)


tanks_qpcr_combined_data %>% 
  ggplot(aes(x = sample_type,
             y = waresh_acanthamoeba_spp,
             colour = tank)) +
  geom_point()



