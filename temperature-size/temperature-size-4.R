load(file = "temperature-size/data/spatial_data.RData")
library(tidyverse)

# Following Chapter 4's Protocol

PW_data$Geogroup <- factor(PW_data$Geogroup)
PW_data$SiteCode <- factor(PW_data$SiteCode)
PW_data$Location <- factor(PW_data$Location)
PW_data$Day <- factor(PW_data$Day)
PW_data$Month <- factor(PW_data$Month)
PW_data$Year <- factor(PW_data$Year)
PW_data$Diver <- factor(PW_data$Diver)

# SORT OF COPY
PW_data <- PW_data %>% mutate(survey_date = as.Date(paste(Year, Month, Day, sep = "-")),
  survey = paste(Geogroup, survey_date, sep = "-")) %>%
  arrange(Geogroup) %>%
  mutate(SurveyIndex = as.integer(factor(survey)))
PW_data$SurvyIndex <- factor(PW_data$SurveyIndex)
PW_data <- PW_data %>% mutate(SurveyIndex = as.integer(factor(SurveyID)))

# Fit a linear regression model with as many explanatory variables and their interactions as possible.

boxplot(SizeClass ~ Geogroup * SiteCode, PW_data)
boxplot(SizeClass ~ SiteCode, PW_data)
boxplot(SizeClass ~ Location, PW_data)
boxplot(SizeClass ~ Day, PW_data)
boxplot(SizeClass ~ Month, PW_data)
boxplot(SizeClass ~ Year, PW_data)
boxplot(SizeClass ~ Diver, PW_data)
boxplot(SizeClass ~ SurvyIndex, PW_data)

M0 <- lm(SizeClass ~ MeanSST * Geogroup + MeanSST * SiteCode + MeanSST * Location + MeanSST * Day
         + MeanSST * Month + MeanSST * Year + MeanSST * Diver + MeanSST * SurvyIndex, data = PW_data)
plot(M0, which = c(1), add.smooth = F)

# Above step using gls

library(nlme)
f1 <- formula(SizeClass ~ MeanSST * Geogroup + MeanSST * Day
              + MeanSST * Month + MeanSST * Year + MeanSST * Diver + MeanSST * SurvyIndex)
M0 <- gls(f1, data = PW_data)

library(lme4)

PW.lmer1 <- lmer(logSizeClass ~ NormalisedSST + (1|Geogroup), data = PW_data)
PW.lmer2 <- lmer(logSizeClass ~ NormalisedSST + (1|SurvyIndex), data = PW_data)
PW.lmer3 <- lmer(logSizeClass ~ NormalisedSST + (1|Geogroup/SurvyIndex), data = PW_data)
PW.lmer4 <- lmer(logSizeClass ~ NormalisedSST + (1|Geogroup) + (1|SurvyIndex), data = PW_data)
PW.lmer5 <- lmer(logSizeClass ~ NormalisedSST + (1|Year), data = PW_data)
PW.lmer6 <- lmer(logSizeClass ~ NormalisedSST + (1|Geogroup) + (1|SurvyIndex) + (1|Year), data = PW_data)

anova(PW.lmer1, PW.lmer2, PW.lmer3, PW.lmer4, PW.lmer5, PW.lmer6)


df <- PW_data
df$y <- log(df$SizeClass)
df$X1 <- (df$MeanSST - mean(df$MeanSST)) / sd(df$MeanSST)
df$X2 <- df$X1^2
yVals <- sort(unique(df$SizeClass)) # fish size classes
I     <- length(yVals)              # number of fish size classes
yCuts <- rep(0, I-1)                # fish sizes that split size classes
for (i in 2:I) {
  yCuts[i-1] <- log(0.5*(yVals[i-1] + yVals[i])) 
}
###
df_stan <- df %>% 
  mutate(z1 = MeanSST - mean(MeanSST)) %>% # rescale size for fitting
  dplyr::select(Year, Month, Day, Geogroup, z1, SizeClass) %>%
  mutate(
    survey_date = as.Date(paste(Year, Month, Day, sep = "-")),
    survey = paste(survey_date, Geogroup, sep = "-")
  ) %>%
  arrange(survey) %>%
  mutate(indx_srv = as.integer(factor(survey)))
# add random factor indexes for year, location, and sample
years    <- sort(unique(df_stan$Year))
df_years <- tibble(Year = years, indx_year = 1:length(years))
grps    <- sort(unique(df_stan$Geogroup))
df_grps <- tibble(Geogroup = grps, indx_grp = 1:length(grps))
df_yVals <- tibble(SizeClass = yVals, indx_sc = 1:length(yVals))
df_stan <- left_join(df_stan, df_years, by = "Year")
df_stan <- left_join(df_stan, df_grps,  by = "Geogroup")
df_stan <- left_join(df_stan, df_yVals, by = "SizeClass")
df_n <- df_stan %>%
  group_by(indx_srv, indx_sc) %>%
  summarise(n = n())
max_sc <- length(yVals) # max(df_n$indx_sc)
max_srv <- max(df_n$indx_srv)
m_obs <- matrix(data = 0, nrow = max_srv, ncol = max_sc)
for (i in 1:nrow(df_n)) {
  m_obs[df_n$indx_srv[i],df_n$indx_sc[i]] <- df_n$n[i] 
}
df_srv <- df_stan %>%
  group_by(indx_srv) %>%
  summarise(
    z1   = median(z1),
    year = median(indx_year),
    grp  = median(indx_grp)
  )
stan_dat <- list(
  N = nrow(df_srv),               # surveys
  J = max(df_srv$grp),            # locations
  K = length(yCuts) + 1,          # fish size classes
  L = max(df_srv$year),           # years of data 
  cutoff = yCuts,                 # size class cut-offs
  y    = m_obs,                   # observations per size class
  x    = df_srv$z1,               # predictor variable
  gloc = df_srv$grp,              # locations
  yr   = df_srv$year              # year
)











