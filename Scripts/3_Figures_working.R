# Read me -----
# Producing figures for grasslands project, using data created in the other scripts
# Packages -----
library(plyr)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(wesanderson)

# Habitat associations -----
# How many species rely on grasslands compared to other habitats?
# Habitat associations: Import data and clean up ----
rm(list=ls())
regional_hab_data <- read.csv("Data/NoSppGrasslandReliantByIUCNRegion_working.csv",
                              header = TRUE,
                              stringsAsFactors = FALSE)

# Habitat associations: Order factors, cut NAs -----
regional_hab_data <- regional_hab_data %>%
  # Cut Antarctica and NAs
  filter(!(IUCN_region %in% c(0, "Antarctic")),
         category != 0) %>%
  # Add nicer x-axis labels
  mutate(hab_label = ifelse(habitat_reliance == "forest_specialist",
                            yes = "Forest\nSpecialist",
                            no = ifelse(habitat_reliance == "grassland_reliant",
                                        yes = "Grassland\nReliant",
                                        no = "Other")),
         region_label = ifelse(IUCN_region == "South and Southeast Asia",
                               yes = "S / SE Asia",
                               no = ifelse(IUCN_region == "Caribbean Islands",
                                           yes = "Caribbean",
                                           no = ifelse(IUCN_region == "West and Central Asia",
                                                       yes = "W / C Asia",
                                                       no = IUCN_region))))
# Order factors
regional_hab_data$hab_label <- factor(regional_hab_data$hab_label,
                                      levels = c("Forest\nSpecialist",
                                                 "Grassland\nReliant",
                                                 "Other"),
                                      ordered = TRUE)

regional_hab_data$category <- factor(regional_hab_data$category,
                                      levels = c("CR","EN", "VU", 
                                                 "NT", "LC", "DD"),
                                      ordered = TRUE)

regional_hab_data$region_label <- factor(regional_hab_data$region_label,
                                     levels = c( "Europe",  "W / C Asia", "North Asia", "East Asia", 
                                                 "North Africa", "Sub-Saharan Africa", "S / SE Asia", "Oceania",
                                                "North America", "Caribbean", "Mesoamerica", "South America"),
                                     ordered = TRUE)

# Habitat associations: Create figure -----
hab_assoc_fig <-
  ggplot(data = regional_hab_data,
       aes(x = hab_label,
           y = no_spp, 
           fill = category)) + 
  facet_wrap(~region_label) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values= c("#DE2523", "#FF7B4E", "#FFD948", 
                              "#D3D449", "#63BB62", "#D2D0C7"), 
                    labels = c("CR      ", "EN", "VU",
                               "NT", "LC", "DD")) + 
  guides(fill = guide_legend(ncol = 2)) + 
  labs(x = NULL, y = "Number of species", fill = "IUCN Category") + 
  theme_classic(14) + 
  theme(panel.grid.major.y = element_line(colour = "grey"),
        legend.position = c(0.01,0.99),
        legend.justification = c(0,1))
hab_assoc_fig

ggsave(hab_assoc_fig,
       filename = "Outputs/ExploratoryFigs/SpeciesReliantOnHabs_regional.pdf",
       height = 20, width = 25, units = "cm")



