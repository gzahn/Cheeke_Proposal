library(tidyverse)
library(edibble)

# Experiment 1: Observational 
# correlate plant properties with root microbiome and soil properties

exp1 <- 
  design("Experiment 1")

exp1 <- 
exp1 %>% 
  set_units(seedling = 150,
            root_microbiome = nested_in(seedling,1),
            soil_properties = nested_in(seedling,1)) %>% 
  set_rcrds(location_lat=seedling,
            location_lon=seedling,
            leaf_size=seedling,
            chlorophyll=seedling,
            height=seedling,
            vis_health_index=seedling,
            leaf_count=seedling,
            fwd_filepath=root_microbiome,
            rev_filepath=root_microbiome,
            texture=soil_properties,
            TOC=soil_properties,
            NO3=soil_properties,
            NH4=soil_properties,
            TDN=soil_properties,
            TN=soil_properties,
            TC=soil_properties,
            DOC=soil_properties,
            perc_SOM=soil_properties,
            pH=soil_properties) 
exp1 %>% plot
tab <- 
  exp1 %>% 
  allot_table()


write_csv(tab,"./Data/Exp_1_design_csv.csv")
