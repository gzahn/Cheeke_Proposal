# devtools::install_github("https://github.com/taowenmicro/ggClusterNet/")
library(ggClusterNet)
library(phyloseq)
library(igraph)
library(tidyverse)
x <- readRDS("./Output/cross-domain_igraph_1_out.RDS")

net_properties(x)
node_properties(x)

node_properties(x) %>% 
  as.tibble() %>% 
  ggplot(aes(x=igraph.degree,y=igraph.betweenness)) +
  geom_point()

# build functions to help access data from lists ####
get_net_props <- function(x){
  props <- x %>% net_properties() %>% as.data.frame()
  props$measure <- row.names(props)
  # props$inoc <- names(x)
  return(props)
}

add_list_names_as_column <- function(x){
  for(nm in names(x)){
    x[[nm]]$inoc <- nm  
  }
  return(x)
}


# pull in all cross-domain igraph objects as list
igraph_files <- list.files("./Output",full.names = TRUE,pattern = "cross-domain_igraph")
cross_domain_igraphs <- map(igraph_files,readRDS)
names(cross_domain_igraphs) <- as.character(c(1:6,"Sterile"))

# get network properties for all igraph objects
net_props <- 
cross_domain_igraphs %>% 
  map(get_net_props) %>% 
  map(as.data.frame) %>%
  add_list_names_as_column() %>% 
  reduce(full_join)

# plot all measures
net_props %>% 
  ggplot(aes(x=inoc,y=value)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~measure,scales = 'free')
ggsave("./Output/figs/cross-domain_network_properties.png",height = 8,width = 10,dpi=300)

# Same for fungal and bacterial networks, individually ####

# pull in all fungal igraph objects as list
igraph_files <- list.files("./Output",full.names = TRUE,pattern = "ITS_igraph")
cross_domain_igraphs <- map(igraph_files,readRDS)
names(cross_domain_igraphs) <- as.character(c(1:6,"Sterile"))
# get network properties for all igraph objects
net_props <- 
  cross_domain_igraphs %>% 
  map(get_net_props) %>% 
  map(as.data.frame) %>%
  add_list_names_as_column() %>% 
  reduce(full_join)
# plot
net_props %>% 
  ggplot(aes(x=inoc,y=value)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~measure,scales = 'free')
ggsave("./Output/figs/ITS_network_properties.png",height = 8,width = 10,dpi=300)

# pull in all bacterial igraph objects as list
igraph_files <- list.files("./Output",full.names = TRUE,pattern = "V6V8_igraph")
cross_domain_igraphs <- map(igraph_files,readRDS)
names(cross_domain_igraphs) <- as.character(c(1:6,"Sterile"))
# get network properties for all igraph objects
net_props <- 
  cross_domain_igraphs %>% 
  map(get_net_props) %>% 
  map(as.data.frame) %>%
  add_list_names_as_column() %>% 
  reduce(full_join)
# plot
net_props %>% 
  ggplot(aes(x=inoc,y=value)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~measure,scales = 'free')
ggsave("./Output/figs/16S_network_properties.png",height = 8,width = 10,dpi=300)
