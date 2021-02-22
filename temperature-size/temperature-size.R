data = load(file = "temperature-size/_data/fish_data.RData")
data = main_data

length(unique(data$TAXONOMIC_NAME))
# 335 species

# want to perform simple linear regression model on one species
# then to perform multiple linear regression model
# need to study mutliple linear regression models but need something to show tomorrow

speciesCount = as.data.frame(table(data$TAXONOMIC_NAME))
# Trachurus novaezelandiae appear 914,584 times

# create a data frame with just the trachurus novaezelandiae species
library(dplyr)

# no create, new data frame inspired by the temporal_data file
# Temporal data set with likely erroneous records and lower 10% filtered out and 
# only for the 9 locations with long-term data available 

# I tried, ask Gustav tomorrow

attempt1 = semi_join(data, temporal_data)
# no
attempt2 = left_join(data, temporal_data)
# no
attempt3 = right_join(data, temporal_data)
# no 

## in their code
load(file="temperature-size/_data/VertData_Temporal.RData")

## remove rare observations
df <- VertData_temporal 

# decide which records to retain for analyses
df_summary <- df %>%
  group_by(TAXONOMIC_NAME, Loc, year) %>%
  summarise(records = n()) %>%
  filter(records >= 20) # must have 20 obs per year at a location

# must be at least 20 obervations at a site within a given year
df_ok <- semi_join(df, df_summary, 
                   by = c("TAXONOMIC_NAME", "Loc", "year"))

df_summary <- df_ok %>%
  group_by(TAXONOMIC_NAME, Loc) %>%
  summarise(
    num_years = length(unique(year)) # years per location
  ) %>%
  filter(num_years >= 7) %>% # must have 8 years at a loc 
  arrange(TAXONOMIC_NAME, Loc)

# remove data 
df_ok <- semi_join(df_ok, df_summary, 
                   by = c("TAXONOMIC_NAME", "Loc")) 

#select only relevant columns
temporal_data <- df_ok %>% select(Location, SurveyID, SiteCode, Diver, TAXONOMIC_NAME, 
                                  SPECIES_EPITHET, GENUS, FAMILY, ORDER, SizeClass, day, month, year, 
                                  geogroup, SiteLat, SiteLong, midpoint, annSST, LWa, LWb, MaxLenFB, 
                                  MaxSizeObs, mpa, Loc)

##Add full location names for figures
locnames <- list(NA)
locnames$Loc <- unique(df_ok$Loc)
locnames$LocFig <- c("Port Phillip Bay", "Maria Island", "Port Davey", "Jervis Bay", "Bicheno", "Tinderbox", "Bass Strait", "Jurien Bay", "Ninepin")

temporal_data$LocFig <- locnames$LocFig[match(temporal_data$Loc, locnames$Loc)]

# this is a copy of their code, with the end changed, plagarism?!
# doesn't include 4 columns I want included, just in case

# create data frame with the specified species
# will use this new thing anyway

# "Size-classes of total fish length (from snout to tip of tail, or 
# longest distance, including for stingrays) used are 2.5, 5.0, 7.5, 
# 10.0, 12.5, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 62.5 cm, and 
# above. Lengths of fish larger than 62.5 cm should be estimated to 
# the nearest 12.5 cm and individually recorded."


















# https://ucdavis-bioinformatics-training.github.io/2018-September-Bioinformatics-Prerequisites/friday/linear_models.html

# 1. Linear Models
# mu = beta_0 + beta_1*x_1 + \epsilon
# x: covariates, a continuous variable or a dummy variable
# beta: unknown parameters to be estimated
# epsilon: error term assumed to be normally distributed with constant variance

# 2. Linear models in R
str(data)
# fit linear model using meansst as the 