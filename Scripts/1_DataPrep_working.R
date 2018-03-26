# Read me -----
# Author: David Williams
# Description: Setting up data for getting the number of species reliant on grassland etc.
# Sections:
# 1) Loading the data from the IUCN and BirdLife
# 2) Standardising habitat and country names, combining bird and mammal data
# 3) Grouping habitats into "grasslands", "forests", "other"
# 4) Looking at how many species don't have "major habitats" listed
# 5) Basic habitat associations 
# 6) Habitat associations not split by region (which I'm not sure we want any more)
# 7) Threats across different habitat associations NO LONGER DOING 
# 8) Historical loss of grasslands
# 9) Loading analyses from previous project to estimate amount of agricultural land required
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

# 1) Load basic habitat requirements data -----
bird_threat_data_orig <- read.csv("/Users/Me/Documents/Datasets/IUCN_BirdLife/BirdLifeThreats2015.csv", 
                                 header = TRUE,
                                 stringsAsFactors = FALSE)
bird_hab_data_orig <- read.csv("/Users/Me/Documents/Datasets/IUCN_BirdLife/BirdLife_HabitatAltitude/Habitats.csv",
                              header = TRUE,
                              stringsAsFactors = FALSE)
bird_range_data_orig <- read.csv("/Users/Me/Documents/Datasets/IUCN_BirdLife/IUCN_Chordata_CountryPresence.csv",
                       header = TRUE,
                       stringsAsFactors = FALSE)

mam_threat_data_orig <- read.csv("/Users/Me/Documents/Datasets/IUCN_BirdLife/MammalData2017/MammalThreats2017AndStresses.csv", 
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

habitats_l1 <- read.csv("/Users/Me/Documents/Datasets/IUCN_BirdLife/Codes/HabitatCodes_L1.csv",
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

all_hab_data <- bind_rows(mam_hab_data, bird_hab_data) %>%
  mutate(species = gsub(" ", "_", species)) %>%
  as.data.frame()

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

all_range_data <- bind_rows(bird_range_data, mam_range_data) %>%
  mutate(species = gsub(" ", "_", species)) %>%
  as.data.frame()

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
  group_by(class, region, habitat_reliance, habitat_specialism, category) %>%
  summarise(no_spp = length(unique(species))) %>%
  as.data.frame()

# 5) Habitat associations: save data ----
write.csv(range_hab_data, 
          file = "Data/RangeHabitatAssociationData_working.csv",
          row.names = FALSE)

write.csv(regional_hab_data, 
          file = "Data/NoSppGrasslandReliantByIUCNRegion_working.csv",
          row.names = FALSE)
# 5b) Habitat associations - MAJOR HABITATS ONLY -----
# We want to know the number of species reliant on grassland at some point and 
#     then compare this to the number solely reliant on forest for their whole
#     lives and the number reliant on other habitats
# This is the same as above, but I am restricting it to habitats of major 
#     importance (cutting out the majority of mammals in the process - see above)

# Cut to just major habitats
range_major_hab_data <- filter(range_hab_data, major_importance == "Yes")

# Get a vector of grassland reliant species
grass_sp <- unique(filter(range_major_hab_data,
                          # Cut to grasslands
                          grouped_habitat == "grassland_broad")$species)

# Get a vector of forest-reliant species
forest_sp <- unique(filter(range_major_hab_data,
                           # Cut to forests
                           grouped_habitat == "forest")$species)

# How many of these are also reliant on another habitat
no_major_habs <- range_major_hab_data %>%
  # Sum up the number of habitats
  group_by(species) %>%
  summarise(no_habs = length(unique(grouped_habitat))) %>%
  as.data.frame()

# Now add habitat reliance data to range data
range_major_hab_data <- range_major_hab_data %>%
  # Join with the number of habitats
  left_join(., no_major_habs) %>%
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
regional_major_hab_data <- range_major_hab_data %>%
  group_by(class, region, habitat_reliance, habitat_specialism, category) %>%
  summarise(no_spp = length(unique(species))) %>%
  as.data.frame()


# 5b) Habitat associations - MAJOR HABITATS ONLY : save data ----
write.csv(regional_major_hab_data, 
          file = "Data/NoSppGrasslandReliantByIUCNRegion_MajorHabsOnly_working.csv",
          row.names = FALSE)

# 6) Non-regional associations across all habitats (old) ----
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
# 6) Save data ------
write.csv(no_spp_habs,
          file = "Data/NoSppReliantOnDifferentHabitats_NotRegional_working")

# Clean up -----
rm(single_habs, two_habs, no_spp_habs, more_habs, regional_major_hab_data, regional_hab_data)

# 7) Threats across different habitat associations NO LONGER DOING ----
# 7) Standardise threat data ----
# Get threat, timing, scope, severity and stress look ups
threat_lookup_l1 <- read.csv("/Users/Me/Documents/Datasets/IUCN_BirdLife/Codes/ThreatCodes_L1.csv", 
                             header = TRUE,
                             stringsAsFactors = FALSE)
threat_lookup_l1$l1_code <- as.character(threat_lookup_l1$l1_code)
threat_lookup_l2 <- read.csv("/Users/Me/Documents/Datasets/IUCN_BirdLife/Codes/ThreatCodes_L2.csv", 
                             header = TRUE,
                             stringsAsFactors = FALSE)
stress_lookup_l2 <- read.csv("/Users/Me/Documents/Datasets/IUCN_BirdLife/Codes/StressCodes_L2.csv", 
                             header = TRUE,
                             stringsAsFactors = FALSE)
timing_lookup <- read.csv("/Users/Me/Documents/Datasets/IUCN_BirdLife/Codes/TimingScopeSeverityCodes.csv", 
                          header = TRUE,
                          stringsAsFactors = FALSE)

# Join bird data to threat data, select useful columns and reshape to join to stress data
bird_threat_data <- bird_threat_data_orig %>%
  left_join(., threat_lookup_l1, by = c("LEVEL1" = "description")) %>%
  left_join(., threat_lookup_l2, by = c("LEVEL2" = "description")) %>%
  # Select useful columns
  select(species = Scientific.name, 
         thr_level_1 = l1_code,
         thr_level_2 = l2_code,
         timing = Timing, 
         scope = Scope, 
         severity = Severity,
         Ecosystem.conversion, Ecosystem.degradation, Indirect.ecosystem.effects, 
         Species.mortality, Species.disturbance, 
         Hybridisation, Competition, Loss.of.mutualism, 
         Inbreeding, Skewed.sex.ratios, Reduced.reproductive.success,
         Other) %>%
  # Reshape the dataframe into long form
  gather(stress, presence, -species, -thr_level_1, -thr_level_2) %>%
  # Drop those rows without the stress present
  filter(presence != "") %>%
  # Drop "presence" column
  select(-presence) %>%
  # Join to stress look-up
  left_join(., stress_lookup_l2, by = c("stress" = "description")) %>%
  rename(stress_code = l2_code) %>%
  # Join to timing, scope and severity codes
  left_join(., filter(timing_lookup, type == "timing"), 
            by = c())
  as.data.frame()

# Get the different levels of threat for mammals (probably not the most efficient way to do this!)
mam_threat_data <- mam_threat_data_orig %>%
  # Split threat levels
  mutate(thr_level_1 = sub("\\..*$", "", Threat.code),
         # How many "." are there? If 2 then drop everything after the last
         thr_level_2 = ifelse(nchar(gsub("[^.]","", Threat.code)) == 2,
                              yes = sub("\\.([^\\.]*)$", "", Threat.code),
                              no = Threat.code)) %>%
  # Drop unnecessary columns
  select(species = Species,
         thr_level_1,
         thr_level_2,
         stress = Stress.description,
         stress_code = Stress.code, 
         i) %>%
  as.data.frame()

# 7) Join range and habitat data to threat data ----
all_threat_data <- bind_rows(bird_threat_data, mam_threat_data)
thr_range_hab_data <- range_hab_data %>%
  full_join(., all_threat_data) %>%
  as.data.frame()

# 8) Historical loss of grasslands -----
# 8) Clean up, load packages ----
rm(list = ls())
library(rgdal)
library(rgeos)
library(raster)
library(ncdf4)

# 8) Load easy data ----
# Want to have both the underlying habitats and land-cover and then combine them
# Countries: 
world_map <- readOGR(dsn = "/Users/Me/Documents/Datasets/WorldMaps/NaturalEarth",
                     layer = "ne_50m_admin_0_countries", 
                     stringsAsFactors = FALSE)
# Ramankutty-Foley potential vegetation
rf_pot_veg <- raster("/Users/Me/Documents/Datasets/RamankuttyFoley1999_PotentialVeg/data/potential_veg_hd.asc",
                     crs = crs(world_map))

# # Ramankutty-Foley cropland / pastureland
# rf_cropland <- raster("/Users/Me/Documents/Datasets/Ramankutty2008_CropPasture/croparea_2000.asc")
# rf_pasture <- raster("/Users/Me/Documents/Datasets/Ramankutty2008_CropPasture/pasturearea_2000.asc")
# 
# # Ramankutty-Foley historic
# tmp <- raster("/Users/Me/Documents/Datasets/Ramankutty_Foley_1999_HistoricCropland/")

# 8) Load HYDE data ----
# Hyde land-use data (from https://daac.ornl.gov/VEGETATION/guides/Land_Use_Harmonization_V1.html)
hist_lu_files <- list.files(path = "/Users/Me/Documents/Datasets/Chini_HistoricLandUse/data/", 
                            full.names = TRUE)
# Drop projections
hist_lu_files <- hist_lu_files[-grep(paste(c("message", "image", "minicam", "aim"), 
                                           collapse = "|"),
                                    hist_lu_files)]
# Drop aux files
hist_lu_files <- hist_lu_files[-grep("aux", hist_lu_files)]
# Cut to just the land covers (gcrop, gothr, gpast, gsecd, gurbn)
hist_lu_files <- hist_lu_files[grep(paste(c("gcrop", "gothr", "gpast", "gsecd", "gurbn"), 
                                           collapse = "|"),
                                     hist_lu_files)]
# I want the longest running dataset (1500-2000) and to include urban land: LUHa_u2.v1
# NB: there is a typo in the documentation, which calls this dataset LUH_u2
hist_lu_files <- hist_lu_files[grep("LUHa_u2.v1", hist_lu_files)]
# Leaves five files: cropland, pastureland, primary, secondary and urban
# Now have to load these files

hist_data_list <- list()
for(i in 1:length(hist_lu_files)) hist_data_list[[i]] <- nc_open(hist_lu_files[i])
names(hist_data_list) <- gsub("(^.*v1_)(.*)(.nc4)", "\\2", hist_lu_files)
# Get dimensions (all are global, so just need the number of rows and columns)
nlon <- dim(ncvar_get(hist_data_list[[1]], "lon"))
nlat <- dim(ncvar_get(hist_data_list[[1]], "lat", verbose = F))
rm(i)
# Get the actual variable 
t1 <- Sys.time()
extracted_hist_data <- llply(hist_data_list, function(f){
  tmp.array <- ncvar_get(f, names(f$var))
  # If you want its attributes:
  # dlname <- ncatt_get(f, names(f$var), "long_name")
  # dunits <- ncatt_get(f, names(f$var), "units")
  # What is the missing value?
  fillvalue <- ncatt_get(f, names(f$var), "_FillValue")
  # Replace the missing values with NAs
  tmp.array[tmp.array == fillvalue$value] <- NA
  tmp.array
})
Sys.time() - t1 # < 1 minute
nlayers <- dim(extracted_hist_data[[1]])[3] # 506!
# Check that the dimensions are right
dim(extracted_hist_data[[1]])
# All OK

# close the ncdf4 files
for(i in 1:length(hist_data_list)) nc_close(hist_data_list[[i]])

# 8) Extract the data we want from HYDE ----
# Now create a raster brick with the correct dimensions and put the values in it
b <- brick(nrows=nlat, ncols=nlon, 
           xmn=-180, xmx=180, 
           ymn=-90, ymx=90, 
           crs=crs(world_map), 
           nl=nlayers)

t1 <- Sys.time()
hist_lu_brick_list <- llply(extracted_hist_data, function(g){
  brick(g,
        xmn=slot(extent(b),'xmin'),
        xmx=slot(extent(b),'xmax'),
        ymn=slot(extent(b),'ymin'),
        ymx=slot(extent(b),'ymax'),
        crs=projection(b),
        transpose = TRUE)
})
Sys.time() - t1 # 50s
rm(t1, extracted_hist_data, hist_data_list, b, hist_lu_files)

# Right now, just get 1500 and 2006 data - the first and last rasters in the brick
years <- c(1, 506)
subset_hist_lu_brick_list <- hist_lu_brick_list
for(i in 1:length(subset_hist_lu_brick_list)){
  subset_hist_lu_brick_list[[i]] <- subset_hist_lu_brick_list[[i]][[years]]
}
# Rename rasters
names(subset_hist_lu_brick_list) <- c("cropland", "primary", "pasture", "secondary", "urban")
for(i in 1:length(subset_hist_lu_brick_list)){
  names(subset_hist_lu_brick_list[[i]]) <- c("year_1500", "year_2006")
}

# 8) Convert country map into raster, get cell area -----
# All the rasters are 360 * 720 and WGS84 
# Create blank raster in correct dimensions
blank_raster <- raster(nrows = nlat,
                             ncols = nlon,
                             ext = extent(rf_pot_veg),
                             crs = crs(rf_pot_veg),
                             vals = NA)
area_raster <- area(blank_raster)
country_id_raster <- rasterize(world_map, 
                               blank_raster, 
                               field = world_map$UN_numeric,
                               update = TRUE)

writeRaster(country_id_raster, 
            filename = "/Users/Me/Documents/Datasets/WorldMaps/CountryIDRaster_360by720_WGS84.tif",
            format = "GTiff", 
            overwrite = TRUE, 
            prj = TRUE)

# 8) Sum up the area of each habitat in each country for different years -----
# a) potential b) 1500 c) 2006
# Create vectors of country codes and each land use
# Create the blank dataframe first in case we use finer-scale data at some point
all_lu_data <- as.data.frame(matrix(nrow = ncell(country_id_raster),
                                    ncol = 14))
all_lu_data[,1] <- 1:nrow(all_lu_data)
all_lu_data[,2] <- as.vector(area_raster$layer)
all_lu_data[,3] <- as.vector(country_id_raster$layer)
all_lu_data[,4] <- as.vector(rf_pot_veg$potential_veg_hd)
names(all_lu_data)[1:4] <- c("cell_id", "cell_area", "un_code", "pot_veg_code")
for(i in 1:length(subset_hist_lu_brick_list)){
  all_lu_data[i + 4] <- as.vector(subset_hist_lu_brick_list[[i]][[1]])
  names(all_lu_data)[i + 4] <- paste(names(subset_hist_lu_brick_list)[i],
                                     names(subset_hist_lu_brick_list[[i]])[1],
                                     sep = "_")
}
for(i in 1:length(subset_hist_lu_brick_list)){
  all_lu_data[i + 9] <- as.vector(subset_hist_lu_brick_list[[i]][[2]])
  names(all_lu_data)[i + 9] <- paste(names(subset_hist_lu_brick_list)[i],
                                     names(subset_hist_lu_brick_list[[i]])[2],
                                     sep = "_")
}
# Get the total proportion of each cell that has data (<1 along coasts)
all_lu_data <- all_lu_data %>%
  mutate(total_prop = cropland_year_1500 + 
           pasture_year_1500 + 
           primary_year_1500 + 
           secondary_year_1500 + 
           urban_year_1500,
         effective_area = cell_area * total_prop) %>%
  as.data.frame()

# Add vegetation names to make it easier
habs <- read.csv("/Users/Me/Documents/Datasets/RamankuttyFoley1999_PotentialVeg/ClassLookUp.csv",
                 header = TRUE, 
                 stringsAsFactors = FALSE)
habs[grep("Water", habs$description),"description"] <- "Water"
habs[grep("No data over", habs$description),"description"] <- "No data"

all_lu_data <- left_join(all_lu_data, habs, by = c("pot_veg_code" = "code"))

# Total up the area in each country
lu_country_summary <- all_lu_data %>%
  rename(pot_veg = description) %>%
  group_by(un_code, pot_veg) %>%
  # Summarise across countries and vegetation types
  summarise(area_potential = sum(effective_area, na.rm = TRUE),
            area_primary_1500 = sum(primary_year_1500 * cell_area, na.rm = TRUE),
            area_secondary_1500 = sum(secondary_year_1500 * cell_area, na.rm = TRUE),
            area_cropland_1500 = sum(cropland_year_1500 * cell_area, na.rm = TRUE),
            area_pasture_1500 = sum(pasture_year_1500 * cell_area, na.rm = TRUE),
            area_urban_1500 = sum(urban_year_1500 * cell_area, na.rm = TRUE),
            area_primary_2006 = sum(primary_year_2006 * cell_area, na.rm = TRUE),
            area_secondary_2006 = sum(secondary_year_2006 * cell_area, na.rm = TRUE),
            area_cropland_2006 = sum(cropland_year_2006 * cell_area, na.rm = TRUE),
            area_pasture_2006 = sum(pasture_year_2006 * cell_area, na.rm = TRUE),
            area_urban_2006 = sum(urban_year_2006 * cell_area, na.rm = TRUE)) %>%
  ungroup() %>%
  # Reshape
  gather(land_use_year, area, -un_code, -pot_veg, -area_potential) %>%
  # Split year off, so I can group by it to check things
  separate(col = land_use_year,
           into = c("land_use", "year"),
           sep = -5,
           remove = TRUE) %>%
  mutate(land_use = sub("(^.*_)(.*)(_)", "\\2", land_use)) %>%
  # Calculate the proportion of potential vegetation in each land use for each year
  mutate(prop_of_potential = area / area_potential,
         # Add ISO3 codes and country names
         ISO3 = countrycode(un_code, 
                            origin = "un",
                            destination = "iso3c"),
         country = countrycode(ISO3, 
                               origin = "iso3c",
                               destination = "country.name")) %>%
  dplyr::select(ISO3, country, year, pot_veg, land_use, area_potential, area, prop_of_potential) %>%
  # Drop NA countries
  filter(!is.na(ISO3)) %>%
  as.data.frame()
  
# Check 
tmp <- lu_country_summary %>%
  group_by(ISO3, year, pot_veg) %>%
  summarise(check = sum(prop_of_potential, na.rm = TRUE))
range(tmp$check) # 0 and 1
rm(tmp)

# 8) Group habitats and calculate the proportion lost -----
tmp <- lu_country_summary %>%
  # Group habitats
  mutate(grouped_habitat = ifelse(pot_veg %in% c("Grassland/Steppe", "Open Shrubland",
                                                 "Savanna", "Tundra",
                                                 "Dense Shrubland"),
                                  yes = "broad_grasslands",
                                  no = ifelse(grepl("Forest", pot_veg),
                                              yes = "broad_forest",
                                              no = "other"))) %>%
  # Sum grouped habitats across countries and years
  group_by(ISO3, country, grouped_habitat, land_use, year) %>%
  summarise(area_potential = sum(area_potential, na.rm = TRUE),
            area_actual = sum(area, na.rm = TRUE)) %>%
  ungroup() %>%
  # Calculate the proportion of potential area that each land-use occupies
  mutate(prop_of_potential = area_actual / area_potential) %>%
  dplyr::select(-area_potential, -area_actual) %>%
  # Get, and rename, separate columns for each year
  spread(year, prop_of_potential) %>%
  rename(prop_1500 = `1500`,
         prop_2006 = `2006`) %>%
  # Calculate the change from 1500-2006, and the total loss
  mutate(change_1500_2006 = prop_2006 - prop_1500,
         total_change = ifelse(land_use == "primary",
                               yes = -(1 - prop_2006),
                               no = prop_2006)) %>%
  as.data.frame()

# 8) Save data ----
write.csv(lu_country_summary, 
          file = "Data/GrasslandLossByCountry_HYDE.csv",
          row.names = FALSE)
  
# 9) Extinction risk in different countries -----
# This is largely lifted from another project: 
# - I am focussing on birds and mammals as they have the best data
# - I'll split by mass-class because that's a huge factor
# - I exclude marine species, small island nations, and Hawaiian endemics
# 9) Clean up, load data ----
rm(list=ls())
detach("package:raster", unload = TRUE) # Otherwise dplyr::select() is over-written

# Species ranges and threat level (from previous project)
sp_range_data <- read.csv("Data/IUCNCountryOccurence_BirdsMammals_WithMasses.csv", 
                          header = TRUE, 
                          stringsAsFactors = FALSE)
# Hawaiian species
hawaii <- read.csv("/Users/Me/Documents/Datasets/IUCN_BirdLife/HawaiiList.csv",
                   header = TRUE,
                   stringsAsFactors = FALSE)

# Grassland specialism (from above)
range_hab_data <- read.csv("Data/RangeHabitatAssociationData_working.csv", 
                           header = TRUE, 
                           stringsAsFactors = FALSE)

# Function to calculate mean risk:
iucn_risk_fun <- function(data){
  thr <- length(unique(data[data$IUCN_value >= 2,]$species))
  sev_thr <- length(unique(data[data$IUCN_value >= 3,]$species))
  total <- length(unique(data$species))
  mean_risk <- mean(data$IUCN_value, na.rm = TRUE)
  df <- data.frame(mean_ex_risk = mean_risk,
                   total = total,
                   thr = thr,
                   non_thr = total - thr,
                   sev_thr = sev_thr,
                   non_sev_thr = total - sev_thr,
                   prop_thr = thr / total,
                   prop_sev_thr = sev_thr / total)
  return(df)
}


# 9) Clean up IUCN data ----
names(sp_range_data) <- tolower(gsub("\\.", "_", names(sp_range_data)))
# Standardise country names
sp_range_data <- sp_range_data %>%
  # Drop Hawaiian species and species without a country, vagrant and introduced species,
  #     and DD species
  filter(!(species %in% hawaii$species),
         !is.na(iso3),
         !is.na(country),
         !(origin %in% c("Vagrant", "Introduced")),
         category != "DD") %>%
  as.data.frame()

# 9) Summarise extinction risk in each country -----
# Need to split by class, mass class and whether the species is reliant on grassland or not
mean_grassland_ex_risk <- range_hab_data %>%
  filter(species %in% sp_range_data$species,
         grouped_habitat == "grassland_broad") %>%
  left_join(., select(sp_range_data, species, mass_class)) %>%
  left_join(., data.frame(category = c("LC", "NT", 
                                       "VU", "EN", "CR", 
                                       "EW", "EX", 
                                       "DD"),
                          IUCN_value = c(0:5,5,NA))) %>%
  group_by(region, country, ISO3, class, mass_class) %>%
  do(iucn_risk_fun(.)) %>%
  as.data.frame()

# And major habitats only 
mean_grassland_ex_risk_major_only <- range_hab_data %>%
  filter(species %in% sp_range_data$species,
         grouped_habitat == "grassland_broad", 
         major_importance == "Yes") %>%
  left_join(., select(sp_range_data, species, mass_class)) %>%
  left_join(., data.frame(category = c("LC", "NT", 
                                       "VU", "EN", "CR", 
                                       "EW", "EX", 
                                       "DD"),
                          IUCN_value = c(0:5,5,NA))) %>%
  group_by(region, country, ISO3, class, mass_class) %>%
  do(iucn_risk_fun(.)) %>%
  as.data.frame()

# 9) Save files -----
write.csv(mean_grassland_ex_risk, 
          file = "Data/NationalMeanExRisk_GrasslandSpecies.csv",
          row.names = FALSE)

write.csv(mean_grassland_ex_risk_major_only, 
          file = "Data/NationalMeanExRisk_GrasslandSpecies_MajorImportanceOnly.csv",
          row.names = FALSE)



