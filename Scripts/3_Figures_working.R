# Read me -----
# Author: David Williams
# Producing figures for grasslands project, using data created in the other scripts:
# 1) Regional habitat associations, split by forest specialist, grassland reliant, other
# 1.1) Regional habitat associations, only using major habitats
# 2) Non-regional habitat associations
# 3) Relationships between grassland reliance and species richness
# Packages -----
library(plyr)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(wesanderson)

# 1) Regional habitat associations -----
# How many species rely on grasslands compared to other habitats?
# 1) Import data and clean up ----
rm(list=ls())
regional_hab_data <- read.csv("Data/NoSppGrasslandReliantByIUCNRegion_working.csv",
                              header = TRUE,
                              stringsAsFactors = FALSE)

# Group regions ----
region_lookup <- data.frame(region = unique(regional_hab_data$region)) %>%
  arrange(region) %>%
  mutate(grouped_region = c("Antarctica", "Latin Am.&\nCarib.", "Asia",
                            "Europe", "Latin Am.&\nCarib.", "N.Af, W.Asia",
                            "North America", "Asia", "Oceania",
                            "Latin Am.&\nCarib.", "Asia",
                            "Sub-Saharan\nAfrica", "N.Af, W.Asia", 
                            NA)) %>%
  as.data.frame()

# 1) Sort plotting data -----
# Order factors, cut NAs 
regional_hab_data <- regional_hab_data %>%
  left_join(., region_lookup) %>%
  # Cut Antarctica and NAs
  filter(grouped_region != "Antarctica",
         !is.na(grouped_region)) %>%
  # Add nicer x-axis labels
  mutate(hab_label = ifelse(habitat_reliance == "forest_specialist",
                            yes = "Forest\nSpecialist",
                            no = ifelse(habitat_reliance == "grassland_reliant",
                                        yes = "Grassland\nReliant",
                                        no = "Other"))) %>%
  group_by(class, grouped_region, category, hab_label) %>%
  summarise(no_spp = sum(no_spp)) %>%
  as.data.frame()

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

regional_hab_data$grouped_region <- factor(regional_hab_data$grouped_region,
                                     levels = c( "Europe",  "North America", 
                                                 "Latin Am.&\nCarib.", "Asia", "Oceania",
                                                 "N.Af, W.Asia", "Sub-Saharan\nAfrica"),
                                     ordered = TRUE)

# 1) Create figure -----
hab_assoc_fig <-
  ggplot(data = regional_hab_data,
       aes(x = hab_label,
           y = no_spp, 
           fill = category)) + 
  facet_grid(class~grouped_region, scales = "free") + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values= c("#DE2523", "#FF7B4E", "#FFD948", 
                              "#D3D449", "#63BB62", "#D2D0C7"), 
                    labels = c("CR      ", "EN", "VU",
                               "NT", "LC", "DD")) + 
  guides(fill = guide_legend(ncol = 2)) + 
  labs(x = NULL, y = "Number of species", fill = "category") + 
  theme_classic(14) + 
  theme(panel.grid.major.y = element_line(colour = "grey"),
        legend.position = c(0.01,0.99),
        legend.justification = c(0,1))
hab_assoc_fig

ggsave(hab_assoc_fig,
       filename = "Outputs/ExploratoryFigs/SpeciesReliantOnHabs_AllSpecies_regional.pdf",
       height = 20, width = 30, units = "cm")

# 1.1) Import data and clean up ----
rm(list=ls()[ls() != "region_lookup"])
regional_major_hab_data <- read.csv("Data/NoSppGrasslandReliantByIUCNRegion_MajorHabsOnly_working.csv",
                              header = TRUE,
                              stringsAsFactors = FALSE)

# 1) Sort plotting data -----
# Order factors, cut NAs 
regional_major_hab_data <- regional_major_hab_data %>%
  left_join(., region_lookup) %>%
  # Cut Antarctica and NAs
  filter(grouped_region != "Antarctica",
         !is.na(grouped_region)) %>%
  # Add nicer x-axis labels
  mutate(hab_label = ifelse(habitat_reliance == "forest_specialist",
                            yes = "Forest\nSpecialist",
                            no = ifelse(habitat_reliance == "grassland_reliant",
                                        yes = "Grassland\nReliant",
                                        no = "Other"))) %>%
  group_by(class, grouped_region, category, hab_label) %>%
  summarise(no_spp = sum(no_spp)) %>%
  as.data.frame()

# Order factors
regional_major_hab_data$hab_label <- factor(regional_major_hab_data$hab_label,
                                      levels = c("Forest\nSpecialist",
                                                 "Grassland\nReliant",
                                                 "Other"),
                                      ordered = TRUE)

regional_major_hab_data$category <- factor(regional_major_hab_data$category,
                                     levels = c("CR","EN", "VU", 
                                                "NT", "LC", "DD"),
                                     ordered = TRUE)

regional_major_hab_data$grouped_region <- factor(regional_major_hab_data$grouped_region,
                                           levels = c( "Europe",  "North America", 
                                                       "Latin Am.&\nCarib.", "Asia", "Oceania",
                                                       "N.Af, W.Asia", "Sub-Saharan\nAfrica"),
                                           ordered = TRUE)

# 1) Create figure -----
major_hab_assoc_fig <-
  ggplot(data = regional_major_hab_data,
         aes(x = hab_label,
             y = no_spp, 
             fill = category)) + 
  facet_grid(class~grouped_region, scales = "free") + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values= c("#DE2523", "#FF7B4E", "#FFD948", 
                              "#D3D449", "#63BB62", "#D2D0C7"), 
                    labels = c("CR      ", "EN", "VU",
                               "NT", "LC", "DD")) + 
  guides(fill = guide_legend(ncol = 2)) + 
  labs(x = NULL, y = "Number of species", fill = "category") + 
  theme_classic(14) + 
  theme(panel.grid.major.y = element_line(colour = "grey"),
        legend.position = c(0.01,0.99),
        legend.justification = c(0,1))
major_hab_assoc_fig

ggsave(major_hab_assoc_fig,
       filename = "Outputs/ExploratoryFigs/SpeciesReliantOnHabs_MajorHabsOnly_regional.pdf",
       height = 20, width = 30, units = "cm")

# 2) Associations across all habitats, not regionally split [not currently running] ----- 
rm(list=ls())
regional_hab_data <- read.csv("Data/NoSppReliantOnDifferentHabitats_NotRegional_working.csv",
                              header = TRUE,
                              stringsAsFactors = FALSE)

# 2) Sort plotting data ----
# Order fill variable
no_spp_habs$no_habitats <- factor(no_spp_habs$no_habitats,
                                  levels = c(">2", "2", "1"),
                                  ordered = TRUE)
# Rename and order the habitats
lu <- data.frame(grouped_habitat = unique(no_spp_habs$grouped_habitat),
                 label_habitats = c("Artificial", "Caves", "Desert",
                                    "Forest", "Grasslands", "Marine", 
                                    "Other", "Rocky\nareas", "Wetlands"))
no_spp_habs <- left_join(no_spp_habs, lu)

# 2) Create figure -----
hab_association_plot <- 
  ggplot(data = no_spp_habs,
         aes(x = label_habitats,
             y = no_spp_reliant,
             fill = no_habitats)) + 
  facet_wrap(~class) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = rev(wes_palette(n=3, name = "Zissou"))) +
  labs(x = NULL,
       y = "No. spp. reliant",
       fill = paste("No. of major\nhabitats")) + 
  theme_classic(14) + 
  theme(panel.grid.major.y = element_line(colour = "grey90"),
        legend.position = c(1,1),
        legend.justification = c(1,1))
hab_association_plot

ggsave(hab_association_plot,
       file = "Outputs/ExploratoryFigs/SpeciesReliantOnHabs_Not.pdf",
       height = 20, width = 20, units = "cm")



# 3) Grassland reliance vs. projected agricultural expansion ----
