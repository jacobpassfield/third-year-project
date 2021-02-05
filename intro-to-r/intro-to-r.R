library(tidyverse)
library(lubridate)

### LOADING AND CLEANING DATA

landings_data <- read_csv("_data/sample_landings_data_raw.csv")
landings_data

#start with the landings_data data frame
landings_data <- landings_data %>%
  #rename the columns
  rename(Year = yy,
         Date = dat,
         Trip_ID = trip,
         Effort_Hours = effort,
         Gear = gr,
         Species = sp,
         Length_cm = l_cm,
         Weight_g = w_cm) %>%
  #turn the date column into a date format that R recognizes
  mutate(Date = mdy(Date)) 
landings_data

#checking for missing values
landings_data[!complete.cases(landings_data),]
#removing observations with missing values
landings_data <- na.omit(landings_data)
landings_data

#checking for typos
unique(landings_data$Gear)
#fixing this by changing all to lowercase letters
landings_data <- landings_data %>%
  mutate(Gear = tolower(Gear))
unique(landings_data$Gear)

#checking another variable
unique(landings_data$Species)
#checking how many times each of the 2 species spellings occurs
landings_data %>%
  filter(Species == "Caesoi cunning") %>%
  nrow()
landings_data %>%
  filter(Species == "Caesio cuning") %>%
  nrow()
#replacing misspelled values
landings_data <- landings_data %>%
  mutate(Species = replace(Species,Species == "Caesoi cunning", "Caesio cuning"))
unique(landings_data$Species)

#looking at the range and distribution of a numeric variable
summary(landings_data$Length_cm)
#visualising data to idenitfy errors
plot(landings_data$Length_cm)
#removing error
landings_data <- landings_data %>%
  filter(Length_cm < 100)
plot(landings_data$Length_cm)
#try with Weight_g and Effort_Hours

#saving this dataset using a new name so we have a copy of raw and clean data
write_csv(landings_data,"_data/sample_landings_data_clean.csv")

### BASIC FISHERIES STATITSICS

#start with the landings data frame
annual_landings <- landings_data %>% 
  #adding column for kilograms by dividing gram column by 1000
  mutate(Weight_kg = Weight_g / 1000) %>%
  #grouping data by year
  group_by(Year) %>% 
  #summarising the total annual landings per year
  summarize(Annual_Landings_kg = sum(Weight_kg,na.rm=TRUE))

#displaying a table of the annual landings data
annual_landings

#na.rm = TRUE tells R what to do with NA values in your data
#here we are remobing the values before summing Weight_kg
#many functions will return NA if any value is NA

#starting with the landings data frame
annual_gear_landings <- landings_data %>% 
  #adding column for kilograms by dividing gram column by 1000
  mutate(Weight_kg = Weight_g / 1000) %>%
  #grouping data by year and gear type
  group_by(Year,Gear) %>% 
  #summarising the total annual landings per year and gear type
  summarize(Annual_Landings_kg = sum(Weight_kg,na.rm=TRUE))

#displaying a table of the annual landings data by gear type
annual_gear_landings

#calculating catch-per-unit-effort (CPUE)
#CPUE is calculated by dividing the catch of each fishing trip by the number of
#hours fished during that trip
#unit of CPUE is kilograms per hour
#the median for every year is then calculated in order to remove outliers

#starting with the landings data frame
cpue_data <- landings_data %>% 
  #adding column for kilograms by dividing gram column by 1000
  mutate(Weight_kg = Weight_g / 1000) %>%
  #grouping by year and Trip ID to calculate CPUE for every trip in every year
  group_by(Year,Trip_ID) %>% 
  #for each year and trip ID, calculating the CPUE for each trip by 
  #dividing the sum of the catch, converted from grams to kilograms, 
  #by the trip by the number of fishing hours
  summarize(Trip_CPUE = sum(Weight_kg) / mean(Effort_Hours)) %>% 
  #group by year so we can calculate median CPUE for each year across all trips 
  #in the year
  group_by(Year) %>% 
  #calculating median CPUE for each year
  summarize(Median_CPUE_kg_hour = median(Trip_CPUE))

#displaying a table of the CPUE data
cpue_data

#determining the percentage of mature fish in the catch in every year of the 
#data frame

#defining m95, the length at which 95% of fish are mature
m95 = 15.9

#starting with the landings data frame
landings_data %>% 
  #adding a column to the data that indicates whether each length measurement 
  #is from a mature or immature fish. 
  #If it's mature, this value should be TRUE; if immature, FALSE.
  mutate(Mature = Length_cm > m95) %>% 
  #grouping by year so we can see the percent mature for every year
  group_by(Year) %>% 
  #the percentage mature is equal to the  number of mature fish divided by 
  #the total number of fish and multiplied by 100
  summarize(Percent_Mature = sum(Mature) / n() * 100) 

### PLOTTING FISHERIES DATA

#starting with the annual_landings data frame you created in the last step
annual_landings %>%
  #initialising a ggplot of annual landings versus year
  ggplot(aes(x=Year,y=Annual_Landings_kg)) +
  #telling ggplot that the plot type should be a scatter plot
  geom_point() +
  #adding a line connecting the points
  geom_line() + 
  #changing the y-axis title
  ylab("Annual Landings [kg/year]") + 
  #adding figure title
  ggtitle("Annual landings of Caesio cuning") 

#starting with the landings data frame
annual_gear_landings %>% 
  #grouping the data by year
  group_by(Year,Gear) %>% 
  #initialising a ggplot of annual landings versus year
  ggplot(aes(x=Year,y=Annual_Landings_kg)) +
  #telling ggplot that the plot type should be a scatter plot
  geom_point() +
  #adding a line connecting the points
  geom_line() + 
  #changing the y-axis title
  ylab("Normalized annual Landings [kg/year]") + 
  #adding figure title
  ggtitle("Normalized annual landings of Caesio cuning") +
  #telling the figure to plot by all different gear types
  facet_wrap(~Gear) 

#starting with the CPUE data frame
cpue_data %>% 
  #initialising a ggplot of median CPUE versus year
  ggplot(aes(x=Year,y=Median_CPUE_kg_hour)) +
  #telling ggplot that the plot type should be a scatter plot
  geom_point() +
  #adding a line connecting the points
  geom_line() + 
  #changing the y-axis title
  ylab("Median CPUE [kg/hour]") + 
  #adding a figure title
  ggtitle("Median CPUE for Caesio cuning") 

#starting with the landings data frame
landings_data %>% 
  #filtering data to only look at length measurements from 2014
  filter(Year == 2014) %>% 
  #initialising ggplot of data using the length column
  ggplot(aes(Length_cm)) + 
  #telling ggplot that the plot type should be a histogram
  geom_histogram() + 
  #changing x-axis label
  xlab("Length [cm]") + 
  #adding figure title
  ggtitle("Length histogram of Caesio cuning in the catch\nLength at 95% maturity shown as a red line.") + 
  #adding a red vertical line for m95, 
  #the length at which 95% of fish are mature. 
  #Any fish below this length may be immature. 
  #Use the m95 value defined in the previous section
  geom_vline(aes(xintercept=m95),color="red") 

#starting with the landings data frame
landings_data %>% 
  #filtering data to only look at length measurements from 2014
  filter(Year == 2014) %>% 
  #initalising ggplot of data using the length column
  ggplot(aes(Length_cm)) + 
  #telling ggplot that the plot type should be a histogram
  geom_histogram() + 
  #chaning x-axis label
  xlab("Length [cm]") + 
  #adding figure title
  ggtitle("Length histogram of Caesio cuning in the catch by gear type\nLength at 95% maturity shown as a red line.") + 
  #adding a red line for m95
  geom_vline(aes(xintercept=m95),color="red") + 
  #telling the figure to plot by all different gear types, known as facetting
  facet_wrap(~Gear) 

