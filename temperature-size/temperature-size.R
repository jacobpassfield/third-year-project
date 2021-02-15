load(file = "temperature-size/_data/fish_data.RData")
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



















# https://ucdavis-bioinformatics-training.github.io/2018-September-Bioinformatics-Prerequisites/friday/linear_models.html

# 1. Linear Models
# mu = beta_0 + beta_1*x_1 + \epsilon
# x: covariates, a continuous variable or a dummy variable
# beta: unknown parameters to be estimated
# epsilon: error term assumed to be normally distributed with constant variance

# 2. Linear models in R
str(data)
# fit linear model using meansst as the 