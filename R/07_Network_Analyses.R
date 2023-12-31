# -----------------------------------------------------------------------------#
# Network Analyses for both bacteria and fungi
# Author: Geoffrey Zahn
# Software versions:  R v 4.2.2
#                     tidyverse v 1.3.2
#                     vegan v 2.6.4
#                     phyloseq v 1.42.0
#                     broom v 1.0.3
# -----------------------------------------------------------------------------#

# SETUP ####
# devtools::install_github("zdk123/SpiecEasi")
# packages 
library(tidyverse); packageVersion("tidyverse")
library(vegan); packageVersion("vegan")
library(phyloseq); packageVersion("phyloseq")
library(broom); packageVersion("broom")
library(corncob); packageVersion("corncob")
library(patchwork); packageVersion("patchwork")
library(igraph); packageVersion("igraph")
library(SpiecEasi); packageVersion("SpiecEasi")

# functions
source("./R/plot_bar2.R")

# Options
options(scipen=999)
options(warn=0)

# Data
bact <- readRDS("./Output/16S_clean_phyloseq_object.RDS") %>% 
  subset_samples(species == "GrandFir") 
bact <- bact %>% subset_taxa(taxa_sums(bact) > 0)

fung <- readRDS("./Output/ITS_clean_phyloseq_object.RDS") %>% 
  subset_samples(species == "GrandFir")
fung <- fung %>% subset_taxa(taxa_sums(fung) > 0)

# join the two for 'full' microbiome (16S + ITS2)
full <- merge_phyloseq(bact,fung)

# Barcharts of bact and fung ####

# merge samples by inoc source for plotting
bact_merged <- bact %>% merge_samples("inoculum_site")
fung_merged <- fung %>% merge_samples("inoculum_site")
# repair metadata
bact_merged@sam_data$inoculum_site <- row.names(bact_merged@sam_data)
fung_merged@sam_data$inoculum_site <- row.names(fung_merged@sam_data)
# transform to relabund and plot
phylum_barplot_by_inoc_bact <- 
bact_merged %>% 
  transform_sample_counts(function(x){x/sum(x)}) %>% 
  plot_bar2(fill = "Phylum") +
  theme_minimal() +
  scale_fill_viridis_d(end=.8) +
  labs(x="Inoculum source",y="Relative abundance",title = "Bacteria")
phylum_barplot_by_inoc_fung <- 
fung_merged %>% 
  transform_sample_counts(function(x){x/sum(x)}) %>% 
  plot_bar2(fill = "Phylum") +
  theme_minimal() +
  scale_fill_viridis_d(end=.8,begin=.2) +
  labs(x="Inoculum source",y="Relative abundance",title = "Fungi")

saveRDS(phylum_barplot_by_inoc_bact,"./Output/phylum_barplot_by_inoc_bact.RDS")
saveRDS(phylum_barplot_by_inoc_fung,"./Output/phylum_barplot_by_inoc_fung.RDS")





# NETWORK ANALYSES ####

# build presence/absence version for co-occurrence networks
full_pa <- transform_sample_counts(full,function(x){ifelse(x>0,1,0)})

# build relative abundance version
full_ra <- transform_sample_counts(full,function(x){x/sum(x)})




## SpiecEasi ####
se.params <- list(rep.num=20, ncores=(parallel::detectCores()-1))

se.mb.fung <- SpiecEasi::spiec.easi(data = fung,
                                       method='mb',
                                       sel.criterion = "bstars",
                                       pulsar.params=se.params)
saveRDS(se.mb.fung,"./Output/ITS_SpiecEasi_out.RDS")
se.mb.fung <- readRDS("./Output/ITS_SpiecEasi_out.RDS")
se.mb.bact <- SpiecEasi::spiec.easi(data = bact,
                                       method='mb',
                                       sel.criterion = "bstars",
                                       pulsar.params=se.params)
saveRDS(se.mb.bact,"./Output/16S_SpiecEasi_out.RDS")
se.mb.bact <- readRDS("./Output/16S_SpiecEasi_out.RDS")

# get best model and build igraph
fung_igraph <- adj2igraph(getRefit(se.mb.fung), vertex.attr = list(name=NA))
bact_igraph <- adj2igraph(getRefit(se.mb.bact), vertex.attr = list(name=NA))

# Plot with igraph
## set size of vertex proportional to clr-mean
vsize_fung    <- transform_sample_counts(fung,function(x){x/sum(x)}) %>% taxa_sums() + 3
am.coord_fung <- layout.auto(fung_igraph)
plot(fung_igraph, layout=am.coord_fung, vertex.size=vsize_fung, vertex.label=NA)
vsize_bact    <- transform_sample_counts(bact,function(x){x/sum(x)}) %>% taxa_sums() + 3
am.coord_bact <- layout.auto(bact_igraph)
plot(bact_igraph, layout=am.coord_bact, vertex.size=vsize_bact, vertex.label=NA)

# Run Specieasi for each marker and inoculum source combination
# moved to separate script for clarity
source("./R/SpiecEasi_by_inoc_sources.R")
# CROSS-DOMAIN NETWORKS ####
# These are run in the above script as well


# identify RDS object files generated by above script
igraph_files <- list.files("./Output", full.names = TRUE, pattern = "igraph.*out.RDS")
se
se <- readRDS("./Output/ITS_SpiecEasi_4_out.RDS")
fung_igraph <- adj2igraph(getRefit(se),vertex.attr = )
readRDS("Output/ITS_SpiecEasi_1_out.RDS") %>% plot

###########################################################

# NETWORK ANALYSIS ####
set.seed(666)
fung_network <- ps %>% 
  make_network(keep.isolates = TRUE)

# quick plot
plot_net(ps,distance = 'jaccard',color = 'inoculum_site',rescale = TRUE,maxdist = .7,point_label = "inoculum_burn_freq")

fung_network
igraph_ITS_1

data.frame(inoculum_site = c(1,2,3,4,5,6,"Sterile"),
           mean_degree = c(igraph::degree(igraph_ITS_1) %>% mean(),
                           igraph::degree(igraph_ITS_2) %>% mean(),
                           igraph::degree(igraph_ITS_3) %>% mean(),
                           igraph::degree(igraph_ITS_4) %>% mean(),
                           igraph::degree(igraph_ITS_5) %>% mean(),
                           igraph::degree(igraph_ITS_6) %>% mean(),
                           igraph::degree(igraph_ITS_Sterile) %>% mean()),
           hub_score = c(igraph::hub_score(igraph_ITS_1)$value,
                         igraph::hub_score(igraph_ITS_2)$value,
                         igraph::hub_score(igraph_ITS_3)$value,
                         igraph::hub_score(igraph_ITS_4)$value,
                         igraph::hub_score(igraph_ITS_5)$value,
                         igraph::hub_score(igraph_ITS_6)$value,
                         igraph::hub_score(igraph_ITS_Sterile)$value)) %>% 
  arrange((mean_degree)) %>% 
  right_join(fung %>% microbiome::meta(), by="inoculum_site") %>% 
  ggplot(aes(x=mean_degree,y=leaf_number)) +
  geom_smooth(method='lm',color = "#6e4618",fill="#c9c9c7") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  labs(y="Leaf number",x="Network complexity") 
ggsave("./Output/figs/new_leaf_number_plot.png",height = 4,width = 12)








igraph::alpha_centrality(igraph_ITS_1)
igraph::assortativity_degree(fung_network,directed = FALSE)
deg <- igraph::degree(fung_network)
tmax <- igraph::centr_degree_tmax((fung_network),loops = FALSE)
igraph::centralize(deg, tmax)
igraph::cliques(fung_network)
igraph::assortativity(fung_network,
                      types1 = ps@sam_data$inoculum_site %>% factor %>% as.numeric)
igraph::largest_cliques(fung_network)


# find hub taxa (in inoc 4)

fung4 <- 
fung %>% 
  subset_samples(inoculum_site == "4") 
fung4 %>% 
  subset_taxa(taxa_sums(fung4) > 1)
igraph::betweenness(igraph_ITS_4)[igraph::betweenness(igraph_ITS_4) > 1000]
tax_table(fung4)[which(igraph::betweenness(igraph_ITS_4) > 1000)] %>% unname %>% 
  as.data.frame() %>% 
  reduce(paste)
tax_table(fung4)[which(igraph::degree(igraph_ITS_4) > 4)]



fung %>% ntaxa()
# grab response of each plant variable
leaf_mod <- fung %>% 
  microbiome::meta() %>% 
  select(leaf_number,bud_number,height,shoot_dm,final_root_dm,inoculum_site) %>% 
  glm(data=., formula = leaf_number ~ inoculum_site)
coef(leaf_mod)

log(coef(leaf_mod)[1] + coef(leaf_mod)[2]) - log(coef(leaf_mod)[1])

beepr::beep(sound = 4)

fung %>% 
  microbiome::meta() %>% 
  select(leaf_number,bud_number,height,shoot_dm,final_root_dm,inoculum_site) %>% 
  mutate(response_coef = case_when(inoculum_site == "Sterile" ~ coef(leaf_mod)[7],
                                   inoculum_site == "6" ~ coef(leaf_mod)[6],
                                   inoculum_site == "5" ~ coef(leaf_mod)[5],
                                   inoculum_site == "4" ~ coef(leaf_mod)[4],
                                   inoculum_site == "3" ~ coef(leaf_mod)[3],
                                   inoculum_site == "2" ~ coef(leaf_mod)[2],
                                   inoculum_site == "1" ~ median(leaf_number))) %>%
  mutate(scaled_leaf_number = scale(leaf_number),
         scaled_bud_number = scale(bud_number),
         scaled_height = scale(height)) %>% 
  pivot_longer(starts_with("scaled_"),names_to = "measure",values_to = "response",names_prefix = "scaled_",values_transform = as.numeric) %>% 
  # mutate(z_leaf_number = zscore(leaf_number) %>% log,
  #        z_bud_number = zscore(bud_number) %>% log,
  #        z_height = zscore(height) %>% log) %>% 
  # pivot_longer(starts_with("z_"),names_to = "measure",values_to = "log_zscore",names_prefix = "z_") %>% 
  ggplot(aes(x=factor(inoculum_site,levels=c("Sterile","3","2","5","6","1","4")),
             y=response,
             fill = measure)) +
  geom_hline(yintercept = 0,linetype=2) +
  geom_boxplot() + # fill="#6e4618"
  # stat_summary(fun='mean',fill="#6e4618",geom = 'bar') +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face='bold',size=18),panel.grid = element_blank(),
        legend.position = 'bottom',legend.text = element_text(size=12,face='bold'),
        legend.title = element_text(size=18,face='bold')) +
  labs(y="Plant response (scaled/centered)",fill="Measure") +
  lims(y=c(-1.25,2)) +
  scale_fill_manual(values = c("#6e4618","#97b031","#86808c"),labels = c("Bud number","Height","Leaf number"))

ggsave("./Output/figs/new_leaf_number_plot3.png",height = 4,width = 12)
