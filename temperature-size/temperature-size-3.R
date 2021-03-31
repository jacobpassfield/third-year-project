# Load libraires
library(tidyverse)

# Load spatial data.
load(file = "temperature-size/data/spatial_data.RData")

# Check if there are 335 species.
length(unique(spatial_data$TaxonomicName)) # 335

# Count occurences of each species.
speciesCount <- spatial_data %>% count(TaxonomicName)

# Find species with maximum observations
which.max(speciesCount$n) # 329
speciesCount[329,] # Trachurus novaezelandiae 914584

# Find species with minimum observations
which.min(speciesCount$n) # 142
speciesCount[142,] # Halichoeres margaritaceus 1008

# HALICHOERES MARGARITACEUS (Pearly Wrasse)

PW_data <- spatial_data %>% filter(TaxonomicName %in% "Halichoeres margaritaceus")

# Checking to see if data was filtered the way described
dim(PW_data) # 1008 19
# More than 1000 observations...
length(unique(PW_data$Geogroup)) # 37
# ...in at least 10 geographic cells...
length(unique(PW_data$Year)) # 11
# ...over at least 5 years.

# Save the Pearly Wrasse data frame
save(PW_data, file = "temperature-size/data/PW_data.RData")











