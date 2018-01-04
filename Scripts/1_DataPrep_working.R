# Read me -----
# Author: David Williams
# Description: Setting up data for getting the number of species reliant on grassland etc.
# Sections:
# 1) Loading the data from the IUCN and BirdLife
# 2) Standardising habitat and country names, combining bird and mammal data
# 3) Grouping habitats into "grasslands", "forests", "other"
# 4) Looking at how many species don't have "major habitats" listed
# 5) Basic habitat associations (I don't think we need this any more)
# 6) Seasonal habitat associations
# Data: Requires data from the IUCN / BirdLife on threats and habitat associations

# Packages -----
library(plyr)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(countrycode)
library(wesanderson)

# Clean up ----
rm(list = ls())

# 1) Load basic data -----
bird_threat_data_orig <- read.csv("/Users/Me/Documents/Datasets/IUCN_BirdLife/BirdLifeThreats2015.csv", 
                                 header = TRUE,
                                 stringsAsFactors = FALSE)
bird_hab_data_orig <- read.csv("/Users/Me/Documents/Datasets/IUCN_BirdLife/BirdLife_HabitatAltitude/Habitats.csv",
                              header = TRUE,
                              stringsAsFactors = FALSE)
bird_range_data_orig <- read.csv("/Users/Me/Documents/Datasets/IUCN_BirdLife/IUCN_Chordata_CountryPresence.csv",
                       header = TRUE,
                       stringsAsFactors = FALSE)

mam_threat_data_orig <- read.csv("/Users/Me/Documents/Datasets/IUCN_BirdLife/MammalData2017/MammalThreats2017.csv", 
                             header = TRUE,
                             stringsAsFactors = FALSE)
mam_hab_data_orig <- read.csv("/Users/Me/Documents/Datasets/IUCN_BirdLife/MammalData2017/MammalHabitats2017.csv", 
                          header = TRUE,
                          stringsAsFactors = FALSE)
mam_range_data_orig <- read.csv("/Users/Me/Documents/Datasets/IUCN_BirdLife/MammalData2017/MammalCoO2017.csv", 
                            header = TRUE,
                            stringsAsFactors = FALSE)

# 2) Get data into same format ----
# 2) Get standardised habitat names -----
# from http://www.iucnredlist.org/technical-documents/classification-schemes/habitats-classification-scheme-ver3
# Note: I have changed some of the names from the url above, to fit with BirdLife's descriptions. The altered 
#       level 1 descriptions are: 
# [1] "Artificial/Terrestrial"                          "Artificial/Aquatic & Marine"                    
# [3] "Marine Neritic"                                  "Rocky areas (eg. inland cliffs, mountain peaks)"
# [5] "Introduced vegetation"    

# The Level 2 classifications are too tedious to deal with right now (and I don't think)
#     we need them for the time being

habitats_l1 <- read.csv("/Users/Me/Documents/Datasets/IUCN_BirdLife/BirdLife_HabitatAltitude/HabitatCodes_L1.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE)
habitats_l1$hab_code_level_1 <- as.character(habitats_l1$hab_code_level_1)
bird_hab_data <- bird_hab_data_orig %>%
  left_join(., habitats_l1, by = c("Habitats.Classification.Scheme.Level.1" = "description")) %>%
  as.data.frame()

mam_hab_data <- mam_hab_data_orig
# Replace blanks in habitat code with 18 - unknown
mam_hab_data[mam_hab_data$Habitat.code == "", "Habitat.code"] <- 18
mam_hab_data$hab_code_level_1 <- sub("\\..*$", "", mam_hab_data$Habitat.code)
mam_hab_data <- left_join(mam_hab_data, habitats_l1)

# 2) Combine habitat data -----
bird_hab_data <- bird_hab_data %>%
  mutate(class = "AVES") %>%
  select(class,
         order = Order,
         family = Family, 
         species = Scientific.name,
         category = X2016.IUCN.Red.List.Category,
         hab_code_level_1,
         hab_level_1 = Habitats.Classification.Scheme.Level.1,
         hab_description = Habitats.Classification.Scheme.Level.2, 
         major_importance = Major.importance., 
         suitability = Suitability, 
         season = Season) %>%
  as.data.frame()

mam_hab_data <- mam_hab_data %>%
  select(class = Class,
         order = Order,
         family = Family, 
         species = Species,
         category = Category,
         hab_code_level_1,
         hab_level_1 = description,
         hab_description = Habitat.description, 
         major_importance = Major.importance., 
         suitability = Suitability, 
         season = Season) %>%
  as.data.frame()

all_hab_data <- bind_rows(mam_hab_data, bird_hab_data)
rm(mam_hab_data, bird_hab_data)
# 2) Standardise and combine range data ----
# Cut down to useful columns, add ISO3 and standardise country names
mam_range_data <- mam_range_data_orig %>%
  filter(Country != "", 
         Species != "Homo sapiens") %>%
  mutate(ISO3 = countrycode(Country, 
                            origin = "country.name",
                            destination = "iso3c"),
         country = countrycode(ISO3, 
                               origin = "iso3c",
                               destination = "country.name")) %>%
  select(class = Class,
         species = Species,
         ISO3,
         country,
         region = Region,
         presence = Presence,
         origin = Origin) %>%
  filter(!is.na(country)) %>%
  as.data.frame()

bird_range_data <- bird_range_data_orig %>%
  filter(Class == "AVES", 
         Country != "") %>%
  mutate(ISO3 = countrycode(Country, 
                            origin = "country.name",
                            destination = "iso3c"),
         country = countrycode(ISO3, 
                               origin = "iso3c",
                               destination = "country.name")) %>%
  select(class = Class,
         species = Species,
         ISO3,
         country,
         region = Region,
         presence = Presence,
         origin = Origin) %>%
  filter(!is.na(country)) %>%
  as.data.frame()

all_range_data <- bind_rows(bird_range_data, mam_range_data)
rm(bird_range_data, mam_range_data)
# 3) Habitat associations -----
# 3) Group habitats -----
# We want to take a broad look at habitats, so lump a lot of habitats together
all_hab_data <- all_hab_data %>%
  # 2 = Savanna, 3 = Shrubland, 4 = Grassland
  mutate(grouped_habitat = ifelse(hab_code_level_1 %in% c(2, 3, 4),
                                  yes = "grassland_broad",
  # 9 = "Marine Neritic", 10 "Marine Oceanic",
  # 11 = " Marine Deep Ocean Floor (Benthic and Demersal)"
  # 12 = "Marine Intertidal", 13 = "Marine Coastal/Supratidal",
                                  no = ifelse(hab_code_level_1 %in% c(9, 10, 11, 12, 13),
                                              yes = "marine_broad",
                                              no = ifelse(hab_code_level_1 %in% c(14, 15, 16),
                                                          yes = "artificial_broad",
                                                          no = ifelse(hab_code_level_1 %in% c(17, 18),
                                                                      yes = "other_broad",
                                                                      no = tolower(hab_level_1)))))) %>%
  as.data.frame()

# 3) Combine habitat associations with range data -----
range_hab_data <- all_range_data %>%
  # Select only extant species (currently including "Possibly Extinct")
  filter(presence != "Extinct Post-1500") %>%
  # Join to habitat data, selecting only extant species
  full_join(., all_hab_data %>%
              filter(category != "EX")) %>%
  as.data.frame()
  
# 4) How many species do not have a "major importance" habitat ----
range_hab_data %>%
  filter(major_importance == "Yes") %>%
  group_by(class) %>%
  summarise(with_maj_hab = length(unique(species))) %>%
  left_join(., range_hab_data %>%
              group_by(class) %>%
              summarise(total = length(unique(species)))) %>%
  mutate(without_maj_hab = total - with_maj_hab) %>%
  as.data.frame()
# ~2000 birds and ~4000 mammals (70% of all) do not have a habitat of major importance

# What are these species?
missing_maj_hab <- range_hab_data %>%
  filter(major_importance == "Yes") %>%
  group_by(class, order) %>%
  summarise(with_maj_hab = length(unique(species))) %>%
  left_join(., range_hab_data %>%
              group_by(class, order) %>%
              summarise(total = length(unique(species)))) %>%
  mutate(without_maj_hab = total - with_maj_hab,
         prop_without = without_maj_hab / total) %>%
  as.data.frame()
# Biggest groups are rodents, insectivores (EULIPOTYPHLA - translates as "truly fat and blind"!), bats,
#     and passerines. Other groups have very high proportions without major habitats. This is a bit of a 
#    bugger
range_hab_data %>%
  group_by(class) %>%
  do(as.data.frame(table(.$major_importance)))

# But... most of these are left blank, rather than being described as NOT of major importance.
# At the moment I am not dropping anything, on the basis that we cannot assume that habitats
#     without a "Yes" are major

# 5) Habitat associations: Which species are habitat 'endemics'-----
# We want to know the number of species reliant on grassland at some point and 
#     then compare this to the number solely reliant on forest for their whole
#     lives and the number reliant on other habitats

# Get a vector of grassland reliant species
grass_sp <- unique(filter(range_hab_data,
                          # Cut to grasslands
                          grouped_habitat == "grassland_broad")$species)

# Get a vector of forest-reliant species
forest_sp <- unique(filter(range_hab_data,
                          # Cut to forests
                          grouped_habitat == "forest")$species)

# How many of these are also reliant on another habitat
no_habs <- range_hab_data %>%
  # Sum up the number of habitats
  group_by(species) %>%
  summarise(no_habs = length(unique(grouped_habitat))) %>%
  as.data.frame()

# Now add habitat reliance data to range data
range_hab_data <- range_hab_data %>%
  # Join with the number of habitats
  left_join(., no_habs) %>%
  # Put in whether species are grassland reliant, forest specialists, or mixed
  mutate(habitat_reliance = ifelse(species %in% grass_sp,
                                   yes = "grassland_reliant",
                                   no = ifelse(species %in% forest_sp & no_habs == 1,
                                               yes = "forest_specialist",
                                               no = "other")),
         # Say whether they are specialists or not - if unknown, assume they aren't
         habitat_specialism = ifelse(no_habs == 1, 
                                     yes = "specialist",
                                     no = "non-specialist")) %>%
  as.data.frame()

# Sum up the number in each group by region
regional_hab_data <- range_hab_data %>%
  group_by(region, habitat_reliance, habitat_specialism, category) %>%
  summarise(no_spp = length(unique(species))) %>%
  as.data.frame()

# 5) Habitat associations: save data ----
write.csv(regional_hab_data, 
          file = "Data/NoSppGrasslandReliantByIUCNRegion_working.csv",
          row.names = FALSE)

# 6) Seasonal associations ----
# Sum up the number of species with associations with each habitat type
# First do this for species with only one habitat for each season, 
#     repeat for those with two and then for those with more
single_habs <- all_hab_data %>%
  # Sum up
  group_by(class, species, season) %>%
  summarise(no_habitats = length(unique(grouped_habitat))) %>%
  mutate(unique = paste(species, season)) %>%
  as.data.frame()

two_habs <- single_habs %>%
  filter(no_habitats == 2) %>%
  as.data.frame()

more_habs <- single_habs %>%
  filter(no_habitats > 2) %>%
  as.data.frame()

single_habs <- single_habs %>%
  filter(no_habitats == 1) %>%
  as.data.frame()

# Now total up the number of species in each habitat (don't need to split by season)
no_spp_habs <- all_hab_data %>%
  # Get unique ID
  mutate(unique = paste(species, season)) %>%
  # Filter to species in a single habitat and only major habitats
  filter(unique %in% single_habs$unique) %>%
  # Group by habitat
  group_by(class, grouped_habitat) %>%
  # Count the number of species reliant
  summarise(no_spp_reliant = length(unique(species))) %>%
  # Put in which grouping it is (single, two or more) 
  mutate(no_habitats = "1") %>%
  # Repeat for two and then multiple habitats
  bind_rows(., all_hab_data %>%
              # Get unique ID
              mutate(unique = paste(species, season)) %>%
              # Filter to species in two habitats and only major habitats
              filter(unique %in% two_habs$unique) %>%
              # Group by habitat
              group_by(class, grouped_habitat) %>%
              # Count the number of species reliant
              summarise(no_spp_reliant = length(unique(species))) %>%
              # Put in which grouping it is (single, two or more) 
              mutate(no_habitats = "2")) %>%
  bind_rows(., all_hab_data %>%
              # Get unique ID
              mutate(unique = paste(species, season)) %>%
              # Filter to species in a single habitat and only major habitats
              filter(unique %in% more_habs$unique) %>%
              # Group by habitat
              group_by(class, grouped_habitat) %>%
              # Count the number of species reliant
              summarise(no_spp_reliant = length(unique(species))) %>%
              # Put in which grouping it is (single, two or more) 
              mutate(no_habitats = ">2")) %>%
  as.data.frame()
  
# Habitat associations: Sort plotting data ----
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

# Habitat associations: Plot number of species data -----
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



# Threats to species -----


