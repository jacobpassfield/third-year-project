# LOADING LIBRARIES
library(tidyverse)
library(ggplot2)
library(lme4)
library(ggeffects)
library(sjPlot)

# DATA FILTERING

load(file = "temperature-size/_data/fish_data.RData")

# Select and rename relevant variables
selected_data <- main_data %>%
                    rename("TaxonomicName" = "TAXONOMIC_NAME", "SpeciesEpithet" = "SPECIES_EPITHET",
                           "Genus" = "GENUS", "Family" = "FAMILY", "Order" = "ORDER", "Day" = "day",
                           "Month" = "month", "Year" = "year", "Geogroup" = "geogroup", 
                           "Midpoint" = "midpoint", "MeanSST" = "meansst") %>%
                      select(Location, SurveyID, SiteCode, Diver, TaxonomicName, SpeciesEpithet,
                             Genus, Family, Order, SizeClass, Day, Month, Year, Geogroup, 
                             SiteLat, SiteLong, Midpoint, MeanSST, MaxSizeObs)

# At least 10 obervations at a geogroup within a given year

recorded_data <- selected_data %>% # copied
                  group_by(TaxonomicName, Geogroup, Year) %>%
                    summarise(records = n()) %>%
                      filter(records >= 10) # must have 10 obs per year at a geogroup
                                            # ran into issues with only 1 or no observations at a given geogroup

joined_data <- semi_join(selected_data, recorded_data, by = c("TaxonomicName", "Geogroup", "Year"))

# At least 5 years worth of observations at a geogroup

yearly_data <- joined_data %>% # copied
                group_by(TaxonomicName, Geogroup) %>%
                 summarise(total_years = length(unique(Year))) %>% # years per geogroup
                  filter(total_years >= 5) %>% # must have 5 years at a geogroup 
                    arrange(TaxonomicName, Geogroup)

data <- semi_join(joined_data, yearly_data, by = c("TaxonomicName", "Geogroup")) 

length(unique(data$Geogroup)) # 53
length(unique(data$TaxonomicName)) # 208

# DATA EXPLORATION

# Yellow Horsetail Mackerel

YHM_data <- data %>% filter(TaxonomicName %in% "Trachurus novaezelandiae")
length(unique(YHM_data$Geogroup)) # 9

# First basic linear model

YHM.lm <- lm(SizeClass ~ MeanSST, data = YHM_data)
summary(YHM.lm)
ggplot(YHM_data, aes(x=MeanSST, y=SizeClass)) +
  labs(y="Size class (cm)", x="Mean SST (°C)") +
    geom_point(alpha = 0.1) + 
      geom_smooth(method="lm") +
        theme_classic()

# Residual versus fitted values
plot(YHM.lm, which=1)
# QQ-plot
plot(YHM.lm, which=2)

# Transforming data to hopefully lead to better modelling
# Distribution of size
YHM_size <- YHM_data %>% group_by(SizeClass) %>% summarise(count=n())
ggplot(YHM_size, aes(x=factor(SizeClass), y=count)) + 
  geom_point() + 
    geom_line(group=1) +
      scale_y_continuous(labels = scales::comma) + 
        theme_classic()
# Log-normally distributed
YHM_data <- mutate(YHM_data, logSizeClass = log(SizeClass))
YHM_logSize <- YHM_data %>% 
                group_by(logSizeClass) %>% 
                  summarise(count=n())
ggplot(YHM_logSize, aes(x=logSizeClass, y=count)) + 
  geom_point() + 
    geom_line(group=1) +
      scale_y_continuous(labels = scales::comma) + 
        theme_classic()
# Standardising explanatory variable didn't work.
# Model failed to converge with standard normalising procedure.
# Normalised explanatory variable by subtracting SST mean from median
# Will reduce the correlation between fixed effects
# Model failed to converge with standard normalising procedure
YHM_data <- YHM_data %>% mutate(NormalisedSST = MeanSST - Midpoint) 

# Repeat modelling

YHM.lm2 <- lm(logSizeClass ~ NormalisedSST, data = YHM_data)
summary(YHM.lm2)

ggplot(YHM_data, aes(x=NormalisedSST, y=logSizeClass)) +
  labs(y="Size Class (cm)", x="Mean SST (°C)") +
    geom_point() + 
      geom_smooth(method="lm")

plot(YHM.lm2, which=1)
plot(YHM.lm2, which=2)

# Dependency checking

# Boxplot
ggplot(YHM_data, aes(x=as.factor(Geogroup), y=SizeClass)) +
  labs(y="Size class (cm)", x="Geogroup") +
    geom_boxplot() + 
      theme_classic() +
        coord_flip()
# Not the same spread O.o

# Colour points by geogroup

ggplot(YHM_data, aes(x=NormalisedSST, y=logSizeClass, colour=as.factor(Geogroup))) +
  labs(y="Log-transformed size class (cm)", x="Scaled mean SST (°C)") +
    geom_point(alpha=0.1) + 
      theme_classic() + 
        theme(legend.position="none")

# Run many separate analyses and fit a regression for each geogroup.

ggplot(YHM_data, aes(x=NormalisedSST, y=logSizeClass)) + 
  facet_wrap(~Geogroup) +
    labs(y="Log-transformed size class (cm)", x="Scaled mean SST (°C)") +
      geom_point(alpha=0.1, size=0.5) + 
        theme_classic() +
          theme(legend.position="none")

YHM_data$Geogroup <- factor(YHM_data$Geogroup)
geo.lm <- lm(logSizeClass ~ NormalisedSST + Geogroup, data = YHM_data)
summary(geo.lm)

# Mixed model

# Treating geogroup as a random effect

YHM.lmer <- lmer(logSizeClass ~ NormalisedSST +  (1|Geogroup), data = YHM_data)
summary(YHM.lmer)
# Variance of Geogroup
0.05109/(0.05109+0.06508) # 0.4397865
# Variance of Residuals
0.06508/(0.05109+0.06508) # 0.5602135

plot(YHM.lmer)
qqnorm(resid(YHM.lmer))
qqline(resid(YHM.lmer))

# Fixed: SST
# Random: Geogroup, Year, SurveyID (all crossed effects)

YHM_data$Year <- factor(YHM_data$Year)
YHM_data$SurveyID <- factor(YHM_data$SurveyID)

YHM.lmer2 <- lmer(logSizeClass ~ NormalisedSST +  (1|Geogroup) + (1|Year) + (1|SurveyID), data = YHM_data)
summary(YHM.lmer2)
# Variance of SurveyID
0.044620/(0.044620+0.009058+0.027927+0.019583) # 0.4409614
# Variance of Year
0.009058/(0.044620+0.009058+0.027927+0.019583) # 0.08951654
# Variance of Geogroup
0.027927/(0.044620+0.009058+0.027927+0.019583) # 0.2759912
# Variance of residuals
0.019583/(0.044620+0.009058+0.027927+0.019583) # 0.1935309

# Extract the prediction data frame
pred.mm <- ggpredict(YHM.lmer2, terms = c("NormalisedSST"))  # this gives overall predictions for the model

# Plot the predictions 
ggplot(pred.mm) + 
  geom_line(aes(x = x, y = predicted)) + # slope
   geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
              fill = "lightgrey", alpha = 0.5) +  # error band
      geom_point(data = YHM_data,  # adding the raw data (scaled values)
                 aes(x = NormalisedSST, y = logSizeClass, colour = Geogroup)) + 
        labs(y="Log-transformed size class (cm)", x="Normalised mean SST (°C)", 
             title = "How temperature affects the body size of Yellow Horsetail Mackerel") + 
          theme_minimal() + 
            theme(legend.position="none")

# Threespot dascyllus (domino damsel) #

TSD_data <- data %>% filter(TaxonomicName %in% "Dascyllus trimaculatus")
length(unique(TSD_data$Geogroup)) # 2

TSD_data <- mutate(TSD_data, logSizeClass = log(SizeClass))
TSD_logSize <- TSD_data %>% 
                group_by(logSizeClass) %>% 
                  summarise(count=n())
ggplot(TSD_logSize, aes(x=logSizeClass, y=count)) + 
 geom_point() + 
  geom_line(group=1) +
    scale_y_continuous(labels = scales::comma) + 
    theme_classic()

TSD_data <- TSD_data %>% mutate(NormalisedSST = MeanSST - Midpoint) 

TSD_data$Geogroup <- factor(TSD_data$Geogroup)
TSD_data$Year <- factor(TSD_data$Year)
TSD_data$SurveyID <- factor(TSD_data$SurveyID)

TSD.lmer <- lmer(logSizeClass ~ NormalisedSST +  (1|Geogroup) + (1|Year) + (1|SurveyID), control = lmerControl(optimizer ="Nelder_Mead"), data = TSD_data)
# MODEL FAILED TO CONVERGE
# https://stats.stackexchange.com/questions/242109/model-failed-to-converge-warning-in-lmer
# https://biologyforfun.wordpress.com/2018/04/09/help-i-have-convergence-warnings/
# https://arxiv.org/pdf/1701.04858.pdf







