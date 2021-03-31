# LIBRARIES

# Loading necessary libraries.
library(tidyverse)

# SPATIAL ANALYSIS

# Loading main data

load(file = "temperature-size/data/fish_data.RData")

# Selecting and renaming relevant variables.

selected_data <- main_data %>%
                 rename("TaxonomicName" = "TAXONOMIC_NAME", "SpeciesEpithet" = "SPECIES_EPITHET",
                        "Genus" = "GENUS", "Family" = "FAMILY", "Order" = "ORDER", "Day" = "day",
                        "Month" = "month", "Year" = "year", "Geogroup" = "geogroup", 
                        "SpeciesMedianSST" = "midpoint", "MeanSST" = "meansst") %>%
                 select(Geogroup, SiteLat, SiteLong, SiteCode, Location, Day, Month, Year, Diver,
                        SurveyID, TaxonomicName, SpeciesEpithet,Genus, Family, Order, SizeClass, 
                        MaxSizeObs, MeanSST, SpeciesMedianSST) %>%
                 arrange(Geogroup, SiteCode, Day, Month, Year)
                
# Renaming selected data as spatial data.

spatial_data <- selected_data

# Saving the data

save(spatial_data, file = "temperature-size/data/spatial_data.RData")

# TEMPORAL ANALYSIS

# Loading temporal data.

load(file = "temperature-size/data/temporal_fish_data.RData")

# Selecting and renaming relevant variables.

filtered_data <- temporal_data %>%
                 rename("TaxonomicName" = "TAXONOMIC_NAME", "SpeciesEpithet" = "SPECIES_EPITHET",
                        "Genus" = "GENUS", "Family" = "FAMILY", "Order" = "ORDER", "Day" = "day",
                        "Month" = "month", "Year" = "year", "Geogroup" = "geogroup", 
                        "SpeciesMedianSST" = "midpoint") %>%
                 select(Location, Geogroup, SiteLat, SiteLong, SiteCode, Day, Month, Year, Diver,
                        SurveyID, TaxonomicName, SpeciesEpithet,Genus, Family, Order, SizeClass, 
                        MaxSizeObs, SpeciesMedianSST) %>%
                 arrange(Location, Geogroup, SiteCode, Day, Month, Year)

# Renaming filtered data as temporal data.

temporal_data <- filtered_data

# Saving the data

save(temporal_data, file = "temperature-size/data/temporal_data.RData")







