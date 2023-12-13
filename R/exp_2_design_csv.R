library(tidyverse)
library(edibble)

burn_trts <- rep(c("1","2","3","4"), each = 20) %>% 
  c(paste0("synthetic_",1:20)) %>% 
  c("control")
site <- rep(1:20,4) %>% c(paste0("synthetic_",1:20)) %>% c("control")

# number of blocks (greenhouse tables?)
n.blocks <- 15


x <- 
  data.frame(
    treatment = rep(burn_trts,each=15),
    site = rep(site,each=15),
    replicate = rep(1:15,length(burn_trts))
  ) %>% 
  mutate(inoculum = paste0(treatment,".",site),
         block = NA,
         location_lat = NA,
         location_log = NA,
         pot_id = paste(treatment,site,replicate,sep="."),
         leaf_size=NA,
         chlorophyll=NA,
         height=NA,
         vis_health_index=NA,
         leaf_count=NA,
         root_biomass_g=NA,
         shoot_biomass_g=NA,
         perc_ecm=NA,
         fwd_filepath=NA,
         rev_filepath=NA) %>% 
  dplyr::select(pot_id,everything())
x

write_csv(x,"./Data/Exp_2_design_csv.csv")
names(x)
exp2 <- x %>% 
  as_edibble()

exp2 %>% 
  set_units(treatment,site,replicate,inoculum,pot_id) %>% 
  set_trts()

exp2 <- 
design("Experiment 2") %>% 
  set_units(treatment=c(1:4,paste0("synthetic_",1:20),"control"),
            inoc=nested_in(treatment,
                           c("1","2","3","4") ~ 20,
                           . ~ 1),
            pot = nested_in(inoc,15)) %>% 
  set_rcrds(location_lat=inoc,
            location_lon=inoc,
            leaf_size=pot,
            chlorophyll=pot,
            height=pot,
            vis_health_index=pot,
            leaf_count=pot,
            root_biomass_g=pot,
            shoot_biomass_g=pot,
            perc_ecm=pot,
            fwd_filepath=pot,
            rev_filepath=pot) %>% 
  expect_rcrds(location_lat > 40, location_lat < 50,
               location_lon < 100, location_lon > 130,
               leaf_size > 0,
               chlorophyll > 0,
               height > 0,
               vis_health_index >= 0, vis_health_index <= 100,
               leaf_count > 0,
               root_biomass_g >= 0,
               shoot_biomass_g >= 0,
               perc_ecm >= 0, perc_ecm <= 1
               ) %>% 
  set_trts(block=15) %>% 
  allot_trts(block ~ pot) %>% 
  assign_trts(seed = 666,order = 'random')

class(exp2)
plot(exp2)
edibble::is_edibble(exp2)
is_edibble_table(exp2)

set.seed(666)
tab <- 
exp2 %>% 
  allot_table() %>% 
  as.data.frame() %>% 
  select(pot,treatment,inoc,block,everything()) 
  
tab <- 
tab %>% 
mutate(ecm_block = case_when(block %in% c(pluck(.,"block") %>% unique %>% sample(5,replace = FALSE)) ~ TRUE)) %>% 
  select(pot,treatment,inoc,block,location_lat,
         location_lon,leaf_size,chlorophyll,height,vis_health_index,
         leaf_count,root_biomass_g,shoot_biomass_g,ecm_block,perc_ecm,	
         fwd_filepath,rev_filepath)
write_csv(tab,"./Data/Exp_2_design_csv.csv")
tab %>% 
  filter(ecm_block) %>% 
  pluck('block') %>% unique
