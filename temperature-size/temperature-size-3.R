# NOTES
# Coplot?

# Load libraires
library(tidyverse)
library(ggplot2)
library(lme4)
library(ggeffects)

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
load(file = "temperature-size/data/PW_data.RData")

# Applying a linear model

# Model selection.
PW.lm <- lm(SizeClass ~ MeanSST, data = PW_data)

ggplot(PW_data, aes(x=MeanSST, y=SizeClass)) +
  labs(y="Size class (cm)", x="Mean SST (°C)") +
  geom_point(alpha = 0.1) + 
  geom_smooth(method="lm") +
  theme_classic()
# Initially, high temperature, small size. How can we trust the model?

# Homogeneity.
plot(PW.lm, which=1)
# Should be flat.
E <- resid(PW.lm)
# Normality.
hist(E, xlab = "Residuals", main = "")
plot(PW.lm, which=2)
# Independence.
plot(PW_data$MeanSST, E, xlab = "MeanSST", ylab= "Residuals")

# Transforming data to hopefully lead to better modelling
# Distribution of size
PW_size <- PW_data %>% group_by(SizeClass) %>% summarise(count=n())
ggplot(PW_size, aes(x=factor(SizeClass), y=count)) + 
  geom_point() + 
  geom_line(group=1) +
  scale_y_continuous(labels = scales::comma) + 
  theme_classic()
# Log-normally distributed
PW_data <- mutate(PW_data, logSizeClass = log(SizeClass))
PW_logSize <- PW_data %>% 
  group_by(logSizeClass) %>% 
  summarise(count=n())
ggplot(PW_logSize, aes(x=logSizeClass, y=count)) + 
  geom_point() + 
  geom_line(group=1) +
  scale_y_continuous(labels = scales::comma) + 
  theme_classic()
# Standardising explanatory variable didn't work.
# Model failed to converge with standard normalising procedure.
# Normalised explanatory variable by subtracting SST mean from median
# Will reduce the correlation between fixed effects
# Model failed to converge with standard normalising procedure
PW_data <- PW_data %>% mutate(NormalisedSST = MeanSST - SpeciesMedianSST) 

# Repeat modelling

# Model selection
PW.lm2 <- lm(logSizeClass ~ NormalisedSST, data = PW_data)

ggplot(PW_data, aes(x=NormalisedSST, y=logSizeClass)) +
  labs(y="Size class (cm)", x="Normalised SST (°C)") +
  geom_point(alpha = 0.1) + 
  geom_smooth(method="lm") +
  theme_classic()
# Initially, high temperature, small size. How can we trust the model?

# Model validation
op <- par(mfrow = c(2,2), mar = c(5,4,2,2))  # sets graphical window with four panels and a certain amount of white space around each panel
# Homogeneity.
plot(PW.lm2, which=1)
# Should be flat.
E2 <- resid(PW.lm2)
# Normality.
hist(E2, xlab = "Residuals", main = "")
plot(PW.lm2, which=2)
# Independence.
plot(PW_data$NormalisedSST, E, xlab = "NormalisedSST", ylab= "Residuals")
par(op) # sets graphical setting back to the default values

# Could fix the variance structure of residuals but heterogeneity.
# So reject the model.

# Independence.

# Boxplot
ggplot(PW_data, aes(x=as.factor(Geogroup), y=SizeClass)) +
  labs(y="Size class (cm)", x="Geogroup") +
  geom_boxplot() + 
  theme_classic() +
  coord_flip()
# Not the same spread O.o

# Add geogroup as a fixed effect.
# Factor as nomial variable.

# Colour points by geogroup.

PW_data$Geogroup <- factor(PW_data$Geogroup)

ggplot(PW_data, aes(x=NormalisedSST, y=logSizeClass, colour=Geogroup)) +
  labs(y="Log-transformed size class (cm)", x="Scaled mean SST (°C)") +
  geom_point() + 
  theme_classic() + 
  theme(legend.position="none")

# Run many separate analyses and fit a regression for each geogroup.

ggplot(PW_data, aes(x=NormalisedSST, y=logSizeClass)) + 
  facet_wrap(~Geogroup) +
  labs(y="Log-transformed size class (cm)", x="Scaled mean SST (°C)") +
  geom_point(alpha=0.1, size=0.5) + 
  theme_classic() +
  theme(legend.position="none")

# Before adding a Geogroup as a fixed effect, see that multiple parameters and 
# tiny sample size.

geo.lm <- lm(logSizeClass ~ NormalisedSST + Geogroup, data = PW_data)

# MIXED EFFECTS

PW_data$SurveyID <- factor(PW_data$SurveyID)

PW.lmer1 <- lmer(logSizeClass ~ NormalisedSST + (1|Geogroup), data = PW_data)
PW.lmer2 <- lmer(logSizeClass ~ NormalisedSST + (1|SurveyID), data = PW_data)
PW.lmer3 <- lmer(logSizeClass ~ NormalisedSST + (1|Geogroup/SurveyID), data = PW_data)
PW.lmer4 <- lmer(logSizeClass ~ NormalisedSST + (1|Geogroup) + (1|SurveyID), data = PW_data)
PW.lmer5 <- lmer(logSizeClass ~ NormalisedSST + (1|fYear), data = PW_data)
PW.lmer6 <- lmer(logSizeClass ~ NormalisedSST + (1|Geogroup) + (1|SurveyID) + (1|fYear), data = PW_data)

# Test Geogroup, SurveyID separately and then together.
# In original article, used year and survey as random effects.
# But..

anova(PW.lmer1, PW.lmer2, PW.lmer3, PW.lmer4, PW.lmer5, PW.lmer6)

# For nested random effects, the factor appears ONLY within a particular level of 
# another factor (each site belongs to a specific mountain range and only to that 
# range)

# Crossed effects, a given factor appears in more than one level of another factor
# (dragons appearing within more than one mountain range)

# An observation has to be in a specific SurveyID to be in a specific Geogroup





