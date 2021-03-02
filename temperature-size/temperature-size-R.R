library(tidyverse)
library(ggplot2)

load(file = "_data/fish_data.RData")

# Checking that there are 335 species
length(unique(main_data$TAXONOMIC_NAME))
# Creating a data frame to check how many occurances each species appear in the main data file
speciesCount = as.data.frame(table(main_data$TAXONOMIC_NAME))

# Creating data frame including yellow horsetail mackerel only
YelHorMac <- main_data %>% filter(TAXONOMIC_NAME %in% "Trachurus novaezelandiae")

# First basic linear model
basic.lm <- lm(SizeClass ~ meansst, data = YelHorMac)
summary(basic.lm)

ggplot(YelHorMac, aes(x=meansst, y=SizeClass)) + geom_point() + geom_smooth(method="lm")


# A plot of residuals against fitted values
plot(basic.lm, which=1)

# Q-Q plot
plot(basic.lm, which=2)

# Transforming data to hopefully lead to better modelling
hist(YelHorMac$SizeClass)

YelHorMac <- mutate(YelHorMac, logSizeClass = log(SizeClass))
hist(YelHorMac$logSizeClass)

YelHorMac$scaledMSST <- scale(YelHorMac$meansst, center=T, scale=T)

basic.lm2 <- lm(logSizeClass ~ scaledMSST, data = YelHorMac)
summary(basic.lm2)

(prelim_plot <- ggplot(YelHorMac, aes(x=scaledMSST, y=logSizeClass)) 
   + geom_point() + geom_smooth(method="lm"))

plot(basic.lm2, which=1)

plot(basic.lm2, which=2)

length(unique(YelHorMac$geogroup))

boxplot(SizeClass ~ geogroup, data = YelHorMac)

(colour_plot <- ggplot(YelHorMac, aes(x=scaledMSST, y=logSizeClass, colour=geogroup)) +
    geom_point(size=2) + theme_classic() + theme(legend.position="none"))

(split_plot <- ggplot(aes(scaledMSST, logSizeClass), data=YelHorMac) + geom_point() +
    facet_wrap(~geogroup) + xlab("scaled mean sea surface temperature") + ylab("log transformed size class"))

geo.lm <- lm(logSizeClass ~ scaledMSST + geogroup, data=YelHorMac)
summary(geo.lm)

library(lme4)

mixed.lmer <- lmer(logSizeClass ~ scaledMSST + (1|geogroup), data=YelHorMac)
summary(mixed.lmer)

plot(mixed.lmer)

qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))

          