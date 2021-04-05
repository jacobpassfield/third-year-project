# Load libraries

library(tidyverse)
library(ggplot2)
library(nlme)
library(ggeffects)

# DATA

# Load spatial data
load(file = "temperature-size/data/spatial_data.RData")

# Confirm there are 335 species
length(unique(spatial_data$TaxonomicName)) # 335

# Count occurences of each species
speciesCount <- spatial_data %>% count(TaxonomicName)

# The specific values of Geogroup and SurveyID are not important in this analysis, only that there
# are differences between them.
# Index values these two.
# Included year for now.

PW_data <- PW_data %>% 
  mutate(SurveyDate = as.Date(paste(Year, Month, Day, sep = "-")),
         Survey = paste(Geogroup, SurveyDate, sep = "/")) %>%
  arrange(Geogroup, SurveyIndex) %>%
  mutate(GeoIndex = as.integer(factor(Geogroup))) %>%
  mutate(YearIndex = as.integer(factor(Year))) %>%
  mutate(SurveyIndex = as.integer(factor(Survey)))

# Find species with maximum observations
which.max(speciesCount$n) # 329
speciesCount[329,] # Trachurus novaezelandiae 914584

# Find species with minimum observations
which.min(speciesCount$n) # 142
speciesCount[142,] # Halichoeres margaritaceus  1008

## HALICHOERES MARGARITACEUS (PEARLY WRASSE)

# Create data frame containing only observations from halichoeres margaritaceus
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
load(file = "temperature-size/data/PW_data.RData")

# SIMPLE LINEAR REGRESSION

# Do I put this above? Yes.

# Dependent variable
# Histogram
hist(PW_data$SizeClass)
# ggplot histogram
PW_size <- PW_data %>% group_by(SizeClass) %>% summarise(count=n())
ggplot(PW_size, aes(x=factor(SizeClass), y=count)) + 
  geom_point() + 
  geom_line(group=1) +
  scale_y_continuous(labels = scales::comma) + 
  theme_classic()
# Positive skew

# Create the logarithm of size class
PW_data <- mutate(PW_data, logSizeClass = log(SizeClass))
PW_logSize <- PW_data %>% 
  group_by(logSizeClass) %>% 
  summarise(count=n())
ggplot(PW_logSize, aes(x=logSizeClass, y=count)) + 
  geom_point() + 
  geom_line(group=1) +
  scale_y_continuous(labels = scales::comma) + 
  theme_classic()
# Better

# Independent variable
# Scale and center
PW_data <- PW_data %>% mutate(ScaledMeanSST = scale(MeanSST, center = T, scale = T))

# How temperature affects body size
PW.lm <- lm(logSizeClass ~ ScaledMeanSST, data = PW_data)

# Plot model
ggplot(PW_data, aes(x=ScaledMeanSST, y=logSizeClass)) +
  labs(y="Log-transformed size class (cm)", x="Scaled Mean SST (°C)") +
  geom_point(alpha = 0.1) + 
  geom_smooth(method="lm") +
  theme_classic()
# alpha = 0.1 shows how many observations have the same size class at the same temperature.
# The darker the point, the more observations have the same statistics.
# Initially, high temperature, small size. How can we trust the model?

# Model validation

# Homogeneity.
plot(PW.lm, which=1)
# Should be flat.
E <- resid(PW.lm)
# Normality.
# https://stats.stackexchange.com/questions/60410/normality-of-dependent-variable-normality-of-residuals
# PW_size <- PW_data %>% group_by(SizeClass) %>% summarise(count=n())
# ggplot(PW_size, aes(x=factor(SizeClass), y=count)) + 
#  geom_point() + 
#  geom_line(group=1) +
#  scale_y_continuous(labels = scales::comma) + 
#  theme_classic()
hist(E, xlab = "Residuals", main = "")
plot(PW.lm, which=2)
# Independence.
plot(PW_data$MeanSST, E, xlab = "MeanSST", ylab= "Residuals")

# Where to go from here?

# Independence
# Would like to see if other variables are independent. 

# The specific values of Geogroup and SurveyID are not important in this analysis, only that there
# are differences between them.
# Index values these two.
# Included year for now.
# Choose survey geogroup and year because ... and included in reference article.

PW_data <- PW_data %>% 
  mutate(SurveyDate = as.Date(paste(Year, Month, Day, sep = "-")),
         Survey = paste(Geogroup, SurveyDate, sep = "/")) %>%
  arrange(Geogroup, Survey) %>%
  mutate(GeoIndex = as.integer(factor(Geogroup))) %>%
  mutate(YearIndex = as.integer(factor(Year))) %>%
  mutate(SurveyIndex = as.integer(factor(Survey)))

# Nomial variables so factor.
PW_data$GeoIndex <- factor(PW_data$GeoIndex)
PW_data$SurveyIndex <- factor(PW_data$SurveyIndex)
PW_data$YearIndex <- factor(PW_data$YearIndex)

# Boxplot
# Use ggplot so GeoIndex can be put on the vertical axis
# Geogroup
ggplot(PW_data, aes(x=GeoIndex, y=logSizeClass)) +
  labs(y="Log-transformed size class (cm)", x="Geogroup") +
  geom_boxplot() + 
  theme_classic() +
  coord_flip()
# Survey
ggplot(PW_data, aes(x=SurveyIndex, y=logSizeClass)) +
  labs(y="Log-transformed size class (cm)", x="Geogroup") +
  geom_boxplot() + 
  theme_classic() +
  coord_flip()
# Year
ggplot(PW_data, aes(x=YearIndex, y=logSizeClass)) +
  labs(y="Log-transformed size class (cm)", x="Geogroup") +
  geom_boxplot() + 
  theme_classic() +
  coord_flip()
# Not the same spread. Some only indlude one observation.
# Year Index is slightly better than the others but not perfect.

# To illustrate further, geogroup.

ggplot(PW_data, aes(x=ScaledMeanSST, y=logSizeClass, colour=GeoIndex)) +
  labs(y="Log-transformed size class (cm)", x="Scaled mean SST (°C)") +
  geom_point() + 
  theme_classic() + 
  theme(legend.position="none")
# Pink dots fall to the right on the horixontal axis.
# Orange dots fall to the left.

ggplot(PW_data, aes(x=ScaledMeanSST, y=logSizeClass, colour=SurveyIndex)) +
  labs(y="Log-transformed size class (cm)", x="Scaled mean SST (°C)") +
  geom_point() + 
  theme_classic() + 
  theme(legend.position="none")

ggplot(PW_data, aes(x=ScaledMeanSST, y=logSizeClass, colour=YearIndex)) +
  labs(y="Log-transformed size class (cm)", x="Scaled mean SST (°C)") +
  geom_point() + 
  theme_classic() + 
  theme(legend.position="none")

# Could include these three terms as explanatory variables.
# First run many seperate analyses and fit a regression for each geogroup.
ggplot(PW_data, aes(x=ScaledMeanSST, y=logSizeClass)) + 
  facet_wrap(~GeoIndex) +
  labs(y="Log-transformed size class (cm)", x="Scaled mean SST (°C)") +
  geom_point(alpha=0.1, size=0.5) + 
  theme_classic() +
  theme(legend.position="none")
# We'd have to fun 37 seperate anslyses, that's 74 (111) and adding the effects of SurveyIndex,
# even year would drastically increase number of estimated parameters.
# Some geogroups only a few observations! Not important analysis.
# Leads us astray from our original question.

# MIXED EFFECTS MODELLING

# Using nlme

f1 <- formula(SizeClass ~ ScaledMeanSST)
M1 <- gls(f1, method = "REML", data = PW_data) # use gls in order to compare models

M2 <- lme(f1, random = ~1 | GeoIndex / SurveyIndex,
          data = PW_data, method = "REML")

anova(M1, M2)
#    Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# M1     1  3 4748.695 4763.436 -2371.347                        
# M2     2  5 4360.609 4385.178 -2175.305 1 vs 2 392.0856  <.0001

# Homogeneity.
plot(M2, which=1)
# Should be flat.
E <- resid(M2)
# Normality.
hist(E, xlab = "Residuals", main = "")
plot(M2, which=2)
# Independence.
plot(PW_data$ScaledMeanSST, E, xlab = "ScaledMeanSST", ylab= "Residuals")

# Difficult to add crossed random effects using nlme.
 
MM1  <- lmer(SizeClass ~ ScaledMeanSST + (1|GeoIndex), data = PW_data, REML = T)
MM2  <- lmer(SizeClass ~ ScaledMeanSST + (1|YearIndex), data = PW_data, REML = T)
MM3  <- lmer(SizeClass ~ ScaledMeanSST + (1|SurveyIndex), data = PW_data, REML = T)

MM4  <- lmer(SizeClass ~ ScaledMeanSST + (1|GeoIndex) + (1|YearIndex) + (1|SurveyIndex), data = PW_data, REML = T)
MM5  <- lmer(SizeClass ~ ScaledMeanSST + (1|GeoIndex) + (1|SurveyIndex), data = PW_data, REML = T)
MM6  <- lmer(SizeClass ~ ScaledMeanSST + (1|GeoIndex) + (1|YearIndex), data = PW_data, REML = T)
MM7  <- lmer(SizeClass ~ ScaledMeanSST + (1|YearIndex) + (1|SurveyIndex), data = PW_data, REML = T)

MM8  <- lmer(SizeClass ~ ScaledMeanSST + (1|GeoIndex/YearIndex/SurveyIndex), data = PW_data, REML = T)
MM9  <- lmer(SizeClass ~ ScaledMeanSST + (1|GeoIndex/SurveyIndex), data = PW_data, REML = T)
MM10 <- lmer(SizeClass ~ ScaledMeanSST + (1|GeoIndex/YearIndex), data = PW_data, REML = T)
MM11 <- lmer(SizeClass ~ ScaledMeanSST + (1|YearIndex/SurveyIndex), data = PW_data, REML = T)

BIC(MM1, MM2, MM3, MM4, MM5, MM6, MM7, MM8, MM9, MM10, MM11)

#      df      BIC
# MM1   4 4416.070
# MM2   4 4747.702
# MM3   4 4382.043
# MM4   6 4391.323
# MM5   5 4385.188
# MM6   5 4405.636
# MM7   5 4388.283
# MM8   6 4387.722
# MM9   5 4385.188
# MM10  5 4394.367
# MM11  5 4388.283

# MM5, MM9

PW_data <- PW_data %>% mutate(NormalisedSST = MeanSST - SpeciesMedianSST) 
MM91  <- lmer(SizeClass ~ NormalisedSST + (1|GeoIndex/SurveyIndex), data = PW_data, REML = T)

# It is nested. So MM9. Makes sense YearIndex is not used as spread wasn't drastically different.

summary(MM9)
summary(MM91)

PW.pred.mm <- ggpredict(MM9, terms = c("ScaledMeanSST"))  # this gives overall predictions for the model

# Plot the predictions 
ggplot(PW.pred.mm) + 
  geom_line(aes(x = x, y = predicted)) + # slope
  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
              fill = "lightgrey", alpha = 0.5) +  # error band
  geom_point(data = PW_data,  # adding the raw data (scaled values)
             aes(x = ScaledMeanSST, y = SizeClass), alpha = 0.1) + 
  labs(y="Size class (cm)", x="Scaled Mean SST (°C)", 
       title = "How temperature affects the body size of Pearly Wrasse") + 
  theme_minimal() + 
  theme(legend.position="none")

# Homogeneity.
plot(MM9, which=1)
# Should be flat.
E <- resid(MM9)
# Normality.
hist(E, xlab = "Residuals", main = "")
# Independence.
plot(PW_data$ScaledMeanSST, E, xlab = "ScaledMeanSST", ylab= "Residuals")

# TRACHURUS NOVAEZELANDIAE (Yellow Horsetail Mackerel)

YHM_data <- spatial_data %>% filter(TaxonomicName %in% "Trachurus novaezelandiae")
YHM_data <- YHM_data %>% mutate(ScaledMeanSST = scale(MeanSST, center = T, scale = T))
YHM_data <- YHM_data %>% mutate(NormalisedSST = MeanSST - SpeciesMedianSST) 

YHM_data <- YHM_data %>% 
  mutate(SurveyDate = as.Date(paste(Year, Month, Day, sep = "-")),
         Survey = paste(Geogroup, SurveyDate, sep = "/")) %>%
  arrange(Geogroup, Survey) %>%
  mutate(GeoIndex = as.integer(factor(Geogroup))) %>%
  mutate(YearIndex = as.integer(factor(Year))) %>%
  mutate(SurveyIndex = as.integer(factor(Survey)))

# Nomial variables so factor.
YHM_data$GeoIndex <- factor(YHM_data$GeoIndex)
YHM_data$SurveyIndex <- factor(YHM_data$SurveyIndex)
YHM_data$YearIndex <- factor(YHM_data$YearIndex)

YHM.mm <- lmer(SizeClass ~ ScaledMeanSST + (1|GeoIndex/SurveyIndex), data = YHM_data, REML = T)

YHM.pred.mm <- ggpredict(YHM.mm, terms = c("ScaledMeanSST"))  # this gives overall predictions for the model

# Plot the predictions 
ggplot(YHM.pred.mm) + 
  geom_line(aes(x = x, y = predicted)) + # slope
  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
              fill = "lightgrey", alpha = 0.5) +  # error band
  geom_point(data = YHM_data,  # adding the raw data (scaled values)
             aes(x = ScaledMeanSST, y = SizeClass), alpha = 0.1) + 
  labs(y="Size class (cm)", x="Scaled Mean SST (°C)", 
       title = "How temperature affects the body size of Yellow Horsetail Mackerel") + 
  theme_minimal() + 
  theme(legend.position="none")

# Homogeneity.
plot(YHM.mm, which=1)
# Should be flat.
E <- resid(YHM.mm)
# Normality.
hist(E, xlab = "Residuals", main = "")
# Independence.
plot(YHM_data$ScaledMeanSST, E, xlab = "ScaledMeanSST", ylab= "Residuals")

# THREESPOT DASCYLLUS (Domino Damsel)

TSD_data <- spatial_data %>% filter(TaxonomicName %in% "Dascyllus trimaculatus")
TSD_data <- TSD_data %>% mutate(ScaledMeanSST = scale(MeanSST, center = T, scale = T))

TSD_data <- TSD_data %>% 
  mutate(SurveyDate = as.Date(paste(Year, Month, Day, sep = "-")),
         Survey = paste(Geogroup, SurveyDate, sep = "/")) %>%
  arrange(Geogroup, Survey) %>%
  mutate(GeoIndex = as.integer(factor(Geogroup))) %>%
  mutate(YearIndex = as.integer(factor(Year))) %>%
  mutate(SurveyIndex = as.integer(factor(Survey)))

# Nomial variables so factor.
TSD_data$GeoIndex <- factor(TSD_data$GeoIndex)
TSD_data$SurveyIndex <- factor(TSD_data$SurveyIndex)
TSD_data$YearIndex <- factor(TSD_data$YearIndex)

TSD.mm <- lmer(SizeClass ~ ScaledMeanSST + (1|GeoIndex/SurveyIndex), data = TSD_data, REML = T)

TSD.pred.mm <- ggpredict(TSD.mm, terms = c("ScaledMeanSST"))  # this gives overall predictions for the model

# Plot the predictions 
ggplot(TSD.pred.mm) + 
  geom_line(aes(x = x, y = predicted)) + # slope
  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
              fill = "lightgrey", alpha = 0.5) +  # error band
  geom_point(data = TSD_data,  # adding the raw data (scaled values)
             aes(x = ScaledMeanSST, y = SizeClass), alpha = 0.1) + 
  labs(y="Size class (cm)", x="Scaled Mean SST (°C)", 
       title = "How temperature affects the body size of Yellow Horsetail Mackerel") + 
  theme_minimal() + 
  theme(legend.position="none")

# Homogeneity.
plot(TSD.mm, which=1)
# Should be flat.
E <- resid(TSD.mm)
# Normality.
hist(E, xlab = "Residuals", main = "")
# Independence.
plot(TSD_data$ScaledMeanSST, E, xlab = "ScaledMeanSST", ylab= "Residuals")

summary(MM9)
summary(YHM.mm)
summary(TSD.mm)
