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
YHM_data$SurveyID <- as.factor(YHM_data$SurveyID)
YHM_data$year <- as.factor(YHM_data$year)

# Saving as a data file
# save(YHM_data, file = "YHM_data.RData")
# load(file = "temperature-size/_data/YHM_data.RData")
length(unique(YHM_data$SurveyID))



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


# boxplot(SizeClass ~ geogroup, data = YHM_data, xlab="Geogroup", ylab="Size Class (cm)")

ggplot(YHM_data, aes(x=as.factor(geogroup), y=SizeClass)) +
  labs(y="Size class (cm)", x="Geogroup") +
  geom_boxplot() + coord_flip()

ggplot(YHM_data, aes(x=as.factor(Location), y=SizeClass)) +
  labs(y="Size class (cm)", x="Location") +
  geom_boxplot() + coord_flip()

#
# ggplot(YHM_data, aes(x=scaledMSST, y=logSizeClass, colour=Location)) +
#  labs(y="Log-transformed size class (cm)", x="Scaled mean SST (°C)") +
#    geom_point(alpha=0.1, size=3) + theme_classic() + theme(legend.position="none")

ggplot(YHM_data, aes(x=scaledMSST, y=logSizeClass, colour="red")) + facet_wrap(~Location) +
  theme(strip.text.x = element_text(size = 6.5)) +
    labs(y="Log-transformed size class (cm)", x="Scaled mean SST (°C)") +
      geom_point(alpha=0.1, size=0.5) + theme(legend.position="none")

loc.lm <- lm(logSizeClass ~ scaledMSST + Location, data = YHM_data)
summary(loc.lm)

geo.lm <- lm(logSizeClass ~ scaledMSST + geogroup, data = YHM_data)
summary(geo.lm)

library(lme4)

mixed.lmer <- lmer(logSizeClass ~ scaledMSST +  (1|Location), data=YHM_data)
summary(mixed.lmer)
# Variance of geogroup
0.18659/(0.18659+0.06451) # 0.7430904
# Variance of Location
0.13940/(0.13940+0.06361) # 0.6866657

plot(mixed.lmer)

qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))

# CROSSED EFFECTS
mixed.lmer2 <- lmer(logSizeClass ~ scaledMSST +  (1|Location) + (1|year), data=YHM_data)
summary(mixed.lmer2)
# Variance of location
0.08785/(0.08785+0.02012+0.05485) # 0.5395529
# Variance for year
0.02012/(0.08785+0.02012+0.05485) # 0.123572
plot(mixed.lmer2)

# Geogroup variance
0.09219 / (0.09219+0.02929+0.05541) # 0.5211713
# Year variance
0.02929 / (0.09219+0.02929+0.05541) # 0.1655831

# NESTED EFFECTS
mixed.lmer3 <- lmer(logSizeClass ~ scaledMSST + (1|Location/geogroup) + (1|year), data = YHM_data)
summary(mixed.lmer3)
# Geogroup within location variance 
0.01927/(0.01927+0.07913+0.01925+0.05444) # 0.1119763
# Location variance
0.07913/(0.01927+0.07913+0.01925+0.05444) # 0.4598175
# Year variance 
0.01925/(0.01927+0.07913+0.01925+0.05444) # 0.1118601
plot(mixed.lmer3)

ggplot(YHM_data, aes(x=scaledMSST, y=logSizeClass, colour=geogroup)) +
  facet_wrap(~Location) +   # a panel for each mountain range
    geom_point(alpha = 0.1) +
      theme_classic()  + theme(legend.position="none") + 
        geom_line(data = cbind(YHM_data, pred = predict(mixed.lmer3)), aes(y = pred), size = 1)

# Decided geogroup only, colour by year
mixed.lmer4 <- lmer(logSizeClass ~ scaledMSST +  (1|geogroup) + (1|year), data=YHM_data)
summary(mixed.lmer4)

# Geogroup variance
0.09219 / (0.09219+0.02929+0.05541) # 0.5211713
# Year variance
0.02929 / (0.09219+0.02929+0.05541) # 0.1655831

# Following reference article, adding survey
# ? Random variation among the surveys themselves (αi, for example, 
# observer, site or weather differences). In this study a ‘survey’ is 
# treated as a specific day in a given cell and may comprise several 50 m 
# transect surveys.
mixed.lmer5 <- lmer(logSizeClass ~ scaledMSST +  (1|geogroup) + (1|year) + (1|SurveyID), data=YHM_data)
summary(mixed.lmer5) # does reduce correlation of fixed effects
# SurveyID variance
0.045014/(0.045014+0.071063+0.008575+0.019627) # 0.3119927
# Geogroup
0.071063/(0.045014+0.071063+0.008575+0.019627) # 0.4925388
# Year variance
0.008575/(0.045014+0.071063+0.008575+0.019627) # 0.05943346

library(ggeffects)  # install the package first if you haven't already, then load it

# Extract the prediction data frame
pred.mm <- ggpredict(mixed.lmer5, terms = c("scaledMSST"))  # this gives overall predictions for the model
# Plot the predictions 
ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = YHM_data,                      # adding the raw data (scaled values)
               aes(x = scaledMSST, y = logSizeClass, colour = geogroup)) + 
  labs(y="Log-transformed size class (cm)", x="Scaled mean SST (°C)", 
         title = "Jacob") + 
    theme_minimal()


