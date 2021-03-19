library(tidyverse)
library(ggplot2)

load(file = "temperature-size/_data/fish_data.RData")

# Checking that there are 335 species
length(unique(main_data$TAXONOMIC_NAME))
# Creating a data frame to check how many occurances each species appear in the main data file
speciesCount <- main_data %>% count(TAXONOMIC_NAME)

# Creating data frame including yellow horse tail mackerel only
YHM_data <- main_data %>% 
                filter(TAXONOMIC_NAME %in% "Trachurus novaezelandiae")

YHM_data$geogroup <- as.factor(YHM_data$geogroup)
# Saving as a data file
# save(YHM_data, file = "YHM_data.RData")
# load(file = "temperature-size/_data/YHM_data.RData")

# First basic linear model
basic.lm <- lm(SizeClass ~ meansst, data = YHM_data)
summary(basic.lm)

ggplot(YHM_data, aes(x=meansst, y=SizeClass)) +
  labs(y="Size class (cm)", x="Mean SST (°C)") +
    geom_point() + geom_smooth(method="lm")

# A plot of residuals against fitted values
plot(basic.lm, which=1)

# Q-Q plot
plot(basic.lm, which=2)

# Transforming data to hopefully lead to better modelling
hist(YHM_data$SizeClass)

YHM_data <- mutate(YHM_data, logSizeClass = log(SizeClass))
hist(YHM_data$logSizeClass)

YHM_data$scaledMSST <- scale(YHM_data$meansst, center=T, scale=T)

basic.lm2 <- lm(logSizeClass ~ scaledMSST, data = YHM_data)
summary(basic.lm2)

ggplot(YHM_data, aes(x=scaledMSST, y=logSizeClass)) +
  labs(y="Size Class (cm)", x="Mean SST (°C)") +
    geom_point() + geom_smooth(method="lm")

plot(basic.lm2, which=1)

plot(basic.lm2, which=2)

length(unique(YHM_data$geogroup))

#
boxplot(SizeClass ~ geogroup, data = YHM_data, xlab="Geogroup", ylab="Size Class (cm)")

ggplot(YHM_data, aes(x=as.factor(geogroup), y=SizeClass)) +
  labs(y="Size class (cm)", x="Geogroup") +
  geom_boxplot() + coord_flip()

ggplot(YHM_data, aes(x=as.factor(Location), y=SizeClass)) +
  labs(y="Size class (cm)", x="Location") +
  geom_boxplot() + coord_flip()

#
ggplot(YHM_data, aes(x=scaledMSST, y=logSizeClass, colour=Location)) +
  labs(y="Log-transformed size class (cm)", x="Scaled mean SST (°C)") +
    geom_point(alpha=0.1, size=3) + theme_classic() + theme(legend.position="none")

ggplot(YHM_data, aes(x=scaledMSST, y=logSizeClass, colour="red")) + facet_wrap(~Location) +
  theme(strip.text.x = element_text(size = 6.5)) +
    labs(y="Log-transformed size class (cm)", x="Scaled mean SST (°C)") +
      geom_point(alpha=0.1, size=0.5) + theme(legend.position="none")

geo.lm <- lm(logSizeClass ~ scaledMSST + Location, data = YHM_data)
summary(geo.lm)

library(lme4)

mixed.lmer <- lmer(logSizeClass ~ scaledMSST +  (1|geogroup), data=YHM_data)
summary(mixed.lmer)
# Variance of geogroup
0.18659/(0.18659+0.06451)
# 0.7430904

plot(mixed.lmer)

qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))

# possibly wrong
mixed.lmer2 <- lmer(logSizeClass ~ scaledMSST +  (1|geogroup) + (1|year), data=YHM_data)
summary(mixed.lmer2)
# Geogroup variance
0.09219 / (0.09219+0.02929+0.05541) # 0.5211713
# Year variance
0.02929 / (0.09219+0.02929+0.05541) # 0.1655831

# seeing if the different geogroups pop up in the same location
PPH <- YelHorMac %>% filter(Location %in% "Sydney")
length(unique(PPH$geogroup))
SDY <- PPH %>% filter(geogroup > 1400)

length(unique(YHM_data$geogroup))


          