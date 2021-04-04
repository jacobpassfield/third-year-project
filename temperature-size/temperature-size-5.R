library(tidyverse)

load(file = "temperature-size/data/spatial_data.RData")
PW_data <- spatial_data %>% filter(TaxonomicName %in% "Halichoeres margaritaceus")

PW_data <- PW_data %>% 
  mutate(SurveyDate = as.Date(paste(Year, Month, Day, sep = "-")),
                              Survey = paste(Geogroup, SurveyDate, sep = "/")) %>%
  arrange(Geogroup, SurveyIndex) %>%
  mutate(GeoIndex = as.integer(factor(Geogroup))) %>%
  mutate(YearIndex = as.integer(factor(Year))) %>%
  mutate(SurveyIndex = as.integer(factor(Survey)))

PW_data$GeoIndex <- factor(PW_data$GeoIndex)
PW_data$SurveyIndex <- factor(PW_data$SurveyIndex)
PW_data$YearIndex <- factor(PW_data$YearIndex)

boxplot(SizeClass ~ GeoIndex, data = PW_data)
# Some Geogroups only include one observation
boxplot(SizeClass ~ SurveyIndex, data = PW_data)
# Some Survey only include one observation
boxplot(SizeClass ~ YearIndex, data = PW_data)
# Some Year only includes one observation
# Justifies adding them as random effects

library(nlme)
f1 <- formula(SizeClass ~ MeanSST)
M1 <- gls(f1, method = "REML", data = PW_data)
M2 <- lme(f1, random = ~1 | GeoIndex / SurveyIndex,
          data = PW_data, method = "REML")

anova(M1, M2)
#    Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# M1     1  3 4749.874 4764.615 -2371.937                        
# M2     2  5 4361.788 4386.357 -2175.894 1 vs 2 392.0856  <.0001

# Adding variance structure make epsilon_{ijk} ~ N(0, sigma_{s}^2) Allows residuals from different years?

summary(M2)

op <- par(mfrow = c(2,2), mar = c(5,4,2,2))  # sets graphical window with four panels and a certain amount of white space around each panel
# Homogeneity.
plot(M2, which=1)
# Should be flat.
E <- resid(M2)
# Normality.
hist(E, xlab = "Residuals", main = "")
plot(M2, which=2)
# Independence.
plot(PW_data$MeanSST, E, xlab = "MeanSST", ylab= "Residuals")
par(op)

# Using lme4

library(lme4)

MM1 <- lmer(SizeClass ~ MeanSST + (1|GeoIndex), data = PW_data, REML = T)
MM2 <- lmer(SizeClass ~ MeanSST + (1|YearIndex), data = PW_data, REML = T)
MM3 <- lmer(SizeClass ~ MeanSST + (1|SurveyIndex), data = PW_data, REML = T)

MM4 <- lmer(SizeClass ~ MeanSST + (1|GeoIndex) + (1|SurveyIndex) + (1|YearIndex), data = PW_data, REML = T)
MM5 <- lmer(SizeClass ~ MeanSST + (1|GeoIndex) + (1|SurveyIndex), data = PW_data, REML = T)
MM6 <- lmer(SizeClass ~ MeanSST + (1|GeoIndex) + (1|YearIndex), data = PW_data, REML = T)

MM7 <- lmer(SizeClass ~ MeanSST + (1|GeoIndex/YearIndex/SurveyIndex), data = PW_data, REML = T)
MM8 <- lmer(SizeClass ~ MeanSST + (1|GeoIndex/SurveyIndex), data = PW_data, REML = T)
MM9 <- lmer(SizeClass ~ MeanSST + (1|GeoIndex/YearIndex), data = PW_data, REML = T)

MM10 <- lmer(SizeClass ~ MeanSST + (1|GeoIndex/SurveyIndex) + (1|YearIndex), data = PW_data, REML = T)
MM11 <- lmer(SizeClass ~ MeanSST + (1|GeoIndex/YearIndex) + (1|SurveyIndex), data = PW_data, REML = T)

BIC(MM1, MM2, MM3, MM4, MM5, MM6, MM7, MM8, MM9, MM10, MM11, MM12)

BIC(MM5, MM8, MM7, MM11)
#      df      BIC
# MM5   5 4386.367
# MM8   5 4386.367
# MM7   6 4388.901
# MM11  6 4388.901

# MM7

library(ggeffects)

pred.mm <- ggpredict(MM7, terms = c("MeanSST"))  # this gives overall predictions for the model

# Plot the predictions 
ggplot(pred.mm) + 
  geom_line(aes(x = x, y = predicted)) + # slope
  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
              fill = "lightgrey", alpha = 0.5) +  # error band
  geom_point(data = PW_data,  # adding the raw data (scaled values)
             aes(x = MeanSST, y = SizeClass)) + 
  labs(y="Size class (cm)", x="Mean SST (°C)", 
       title = "How temperature affects the body size of Pearly Wrasse") + 
  theme_minimal() + 
  theme(legend.position="none")

