
library(dslabs)

data("gapminder")

head(gapminder)

library(ggplot2())

ds_theme_set()

library(tidyverse)

# Plot the fertility vs. life expectency in 1962
filter(gapminder, year== 1962) %>% ggplot(aes(fertility, life_expectancy, color = continent)) + geom_point()


# Use facet to generate this for 1962 and 2012
filter(gapminder, year %in% c(1962, 2012) ) %>% ggplot(aes(fertility, life_expectancy, color = continent)) + geom_point() + 
  facet_grid(continent~year)

# Grid
filter(gapminder, year %in% c(1962, 2012) ) %>% ggplot(aes(fertility, life_expectancy, color = continent)) + geom_point() + 
  facet_grid(.~year)

# Facet Wrap
filter(gapminder, year %in% c(1962, 1970, 1980, 1990, 2000, 2012) ) %>% 
  ggplot(aes(fertility, life_expectancy, color = continent)) + 
  geom_point() + 
  facet_wrap(~year)


############# TIME SERIES PLOTS ####################


view(gapminder)


filter(gapminder, country %in% c("United States") ) %>% ggplot(aes(year, fertility, label = fertility)) + 
  geom_point() + 
  geom_text(nudge_y = 0.1 )

countries <- c("Germany", "South Korea")
filter(gapminder, country %in% countries) %>% ggplot(aes(year, fertility, label = fertility, group = country)) + 
  geom_line()

filter(gapminder, country %in% countries) %>% ggplot(aes(year, fertility, label = fertility, col = country)) + 
  geom_line()

# Create labels for series
labels <- data.frame(country = countries, x = c(1975, 1975), y = c(2, 4) )

filter(gapminder, country %in% countries) %>% ggplot(aes(year, fertility, label = fertility, col = country)) + 
  geom_line() + 
  geom_text( data = labels, aes(x, y, label = country), size = 5 ) + 
  theme( legend.position = "none")


labels <- data.frame(country = countries, x = c(1965, 1975), y = c(72, 60) )

filter(gapminder, country %in% countries) %>% ggplot(aes(year, life_expectancy, col = country, group = country)) + 
  geom_line() +
  geom_text( data = labels, aes(x, y, label = country), size = 5 ) +
  theme( legend.position = "none")


############# TRANSFORMATIONS ####################

gapminder <- gapminder %>% mutate(dollars_per_day = gdp / population / 365)

past_year <- 1970

gapminder %>% filter(year == past_year & !is.na(gdp))  %>%
  ggplot(aes(dollars_per_day)) + 
  geom_histogram(binwidth = 1, col = "black")

# Convert the dollars per day into a log2 for plotting on a log scale
gapminder %>% filter(year == past_year & !is.na(gdp))  %>%
  ggplot(aes( log2(dollars_per_day) ) ) + 
  geom_histogram(binwidth = 1, col = "black")

# Dont convert the value but plot it on a log scale
gapminder %>% filter(year == past_year & !is.na(gdp))  %>%
  ggplot(aes( dollars_per_day ) ) + 
  geom_histogram(binwidth = 1, col = "black") + 
  scale_x_continuous(trans = "log2")


############# BOX PLOTS ####################

gapminder %>% filter(year == past_year & !is.na(gdp))  %>%
  ggplot(aes(region, dollars_per_day ) ) + 
  geom_boxplot()

# Fix the horizontal axis lables
gapminder %>% filter(year == past_year & !is.na(gdp))  %>%
  ggplot(aes(region, dollars_per_day ) ) + 
  geom_boxplot() + 
  theme (axis.text.x = element_text(angle = 90, hjust = 1))


# Order by regions
gapminder %>% filter(year == past_year & !is.na(gdp))  %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>% 
  ggplot(aes(region, dollars_per_day, fill = continent ) ) + 
  geom_boxplot() + 
  theme (axis.text.x = element_text(angle = 90, hjust = 1))


# Change to log scale
# Order by regions
gapminder %>% filter(year == past_year & !is.na(gdp))  %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>% 
  ggplot(aes(region, dollars_per_day, fill = continent ) ) + 
  geom_boxplot() + 
  theme (axis.text.x = element_text(angle = 90, hjust = 1))

# change to log scale
gapminder %>% filter(year == past_year & !is.na(gdp))  %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>% 
  ggplot(aes(region, dollars_per_day, fill = continent ) ) + 
  geom_boxplot() + 
  theme (axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_y_continuous(trans = "log2")

# change to log scale
gapminder %>% filter(year == past_year & !is.na(gdp))  %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>% 
  ggplot(aes(region, dollars_per_day, fill = continent ) ) + 
  geom_boxplot() + 
  theme (axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_y_continuous(trans = "log2") + 
  geom_point(show.legend = FALSE)
  
  
  
############# COMPARING DISTRIBUTIONS ####################

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

gapminder %>% filter(year == past_year & !is.na(gdp))  %>%
  mutate (group = ifelse(region %in% west, "West", "Developing") ) %>%
  ggplot(aes( dollars_per_day ) ) + 
  geom_histogram(binwidth = 1, col = "black") + 
  scale_x_continuous(trans = "log2") + 
  facet_grid(. ~ group)

# Compare aginst past and present

past_year <- 1970
present_year <- 2010

gapminder %>% filter(year %in% c(past_year, present_year) )  %>%
  mutate (group = ifelse(region %in% west, "West", "Developing") ) %>%
  ggplot(aes( dollars_per_day ) ) + 
  geom_histogram(binwidth = 1, col = "black") + 
  scale_x_continuous(trans = "log2") + 
  facet_grid(year ~ group)

# Compare onlt countries that exist in both years
countries1970 <- gapminder %>% filter(year %in% c(past_year) )  %>% .$country
countries2010 <- gapminder %>% filter(year %in% c(present_year) )  %>% .$country

country_list <- intersect(countries1970, countries2010)

gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list)  %>%
  mutate (group = ifelse(region %in% west, "West", "Developing") ) %>%
  ggplot(aes( dollars_per_day ) ) + 
  geom_histogram(binwidth = 1, col = "black") + 
  scale_x_continuous(trans = "log2") + 
  facet_grid(year ~ group)

# Plot two box plots
gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list)  %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>% 
  ggplot(aes(region, dollars_per_day, fill = continent ) ) + 
  geom_boxplot() + 
  theme (axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_y_continuous(trans = "log2") + 
  geom_point(show.legend = FALSE) + 
  facet_grid(year ~ .)

# Instead of 2 plots, plot two points in the same chart
gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list)  %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>% 
  ggplot(aes(region, dollars_per_day, fill = factor(year) ) ) + 
  geom_boxplot() + 
  theme (axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_y_continuous(trans = "log2")  +
  geom_point(show.legend = FALSE)

############# DENSITY PLOTS ####################

# Plot Developing vs. West on a density plot
gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list)  %>%
  mutate (group = ifelse(region %in% west, "West", "Developing") ) %>%
  ggplot(aes(x = dollars_per_day, y = ..count.. , fill = group)) + 
  scale_x_continuous(trans = "log2")  +
  geom_density(alpha = 0.2) + 
  facet_grid(year ~ .)

# Smoothen the plot with BW argument
gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list)  %>%
  mutate (group = ifelse(region %in% west, "West", "Developing") ) %>%
  ggplot(aes(x = dollars_per_day, y = ..count.. , fill = group)) + 
  scale_x_continuous(trans = "log2")  +
  geom_density(alpha = 0.2, bw = 0.75) + 
  facet_grid(year ~ .)

# gROUP data to confirm its asian countries that moving

gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list)  %>%
  mutate (group = factor(case_when (
      .$region %in% west ~ "West", 
      .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia", 
      .$region %in% c("Carrebean", "Central America", "South America") ~ "Latin America", 
      .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub Saharan Africa", 
      TRUE ~ "Others"
    )
  )) %>%
  ggplot(aes(x = dollars_per_day, y = ..count.. , fill = group)) + 
  scale_x_continuous(trans = "log2")  +
  geom_density(alpha = 0.2, bw = 0.75) + 
  facet_grid(year ~ .)

# Stack the above plot

gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list)  %>%
  mutate (group = factor(case_when (
    .$region %in% west ~ "West", 
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia", 
    .$region %in% c("Carrebean", "Central America", "South America") ~ "Latin America", 
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub Saharan Africa", 
    TRUE ~ "Others"
  )
  )) %>%
  ggplot(aes(x = dollars_per_day, y = ..count.. , fill = group)) + 
  scale_x_continuous(trans = "log2")  +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + 
  facet_grid(year ~ .)

############# ECOLOGICAL FALLACY ####################


############# ASSIGNMENTS ####################

# Using ggplot and the points layer, 
## create a scatter plot of life expectancy versus fertility for the African continent in 2012.
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
## fill out the missing parts in filter and aes
gapminder %>% filter( continent == "Africa" & year == 2012) %>%
  ggplot(aes( fertility, life_expectancy)) +
  geom_point()

# Remake the plot from the previous exercises but this time use color to dinstinguish the different regions of Africa to see if this explains the clusters. Remember that you can explore the gapminder data to see how the regions of Africa are labeled in the dataframe!
  
  
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder %>% filter( continent == "Africa" & year == 2012) %>%
  ggplot(aes( fertility, life_expectancy, color = region)) +
  geom_point()

# Create a table showing the country and region for the African countries (use select) 
## that in 2012 had fertility rates of 3 or less and life expectancies of at least 70.

df <- gapminder %>% filter( continent == "Africa" & year == 2012 & fertility <= 3 & life_expectancy >= 70) %>% 
  select(country, region)

# Exercise 4. Life expectancy and the Vietnam War - part 1
# Use filter to create a table with data for the years from 1960 to 2010 in Vietnam and the United States.
tab <- gapminder %>% filter( year %in% c(1960:2010), country %in% c("Vietnam", "United States"))

# Use geom_line to plot life expectancy vs year for Vietnam and the United States.
## Use color to distinguish the two countries.

tab %>% ggplot(aes(year, life_expectancy, color = country)) + 
  geom_line()

# Exercise 6. Life expectancy in Cambodia
## Use a single line of code to create a time series plot from 1960 to 2010 of life expectancy vs year for Cambodia.
gapminder %>% filter( year %in% c(1960:2010), country %in% c("Cambodia")) %>% 
  ggplot(aes(year, life_expectancy, color = country)) + 
  geom_line()

# Exercise 7. Dollars per day - part 1
# Now we are going to calculate and plot dollars per day for African countries in 2010 using GDP data.

# Use mutate to create a dollars_per_day variable, which is defined as gdp/population/365.
# Create the dollars_per_day variable for African countries for the year 2010.
# Remove any NA values.

daydollars <- gapminder %>% filter( year == 2010 & continent == "Africa") %>% 
  mutate(dollars_per_day = gdp / population / 365) %>% filter(! is.na(dollars_per_day))

# Create a smooth density plot of dollars per day from daydollars.
# Use a log (base 2) scale for the x axis.

daydollars %>% ggplot(aes(dollars_per_day)) +
  scale_x_continuous(trans = "log2")  +
  geom_density()
  

# Create the dollars_per_day variable as in Exercise 7, 
## but for African countries in the years 1970 and 2010 this time.
### Make sure you remove any NA values.

# Create a smooth density plot of dollars per day for 1970 and 2010 using a log (base 2) scale for the x axis.
# Use facet_grid to show a different density plot for 1970 and 2010.

gapminder %>% filter( year %in% c(2010, 1970) & continent == "Africa") %>% 
  mutate(dollars_per_day = gdp / population / 365) %>% filter(! is.na(dollars_per_day)) %>%
  ggplot(aes(dollars_per_day)) +
  scale_x_continuous(trans = "log2")  +
  geom_density() + 
  facet_grid(. ~ year)

# Make sure the densities are smooth by using bw = 0.5.
# Use the fill and position arguments where appropriate to create the stacked histograms of each region.

gapminder %>% filter( year %in% c(2010, 1970) & continent == "Africa") %>% 
  mutate(dollars_per_day = gdp / population / 365) %>% filter(! is.na(dollars_per_day)) %>%
  ggplot(aes(dollars_per_day, fill = region)) +
  scale_x_continuous(trans = "log2")  +
  geom_density(bw = 0.5, position = "stack") + 
  facet_grid(. ~ year)

# Exercise 11. Infant mortality scatter plot - part 1
# Generate dollars_per_day using mutate and filter for the year 2010 for African countries.
# Remember to remove NA values.
# Store the mutated dataset in gapminder_Africa_2010.
# Make a scatter plot of infant_mortaility versus dollars_per_day for countries in the African continent.
# Use color to denote the different regions of Africa.

gapminder_Africa_2010 <- gapminder %>% filter( year == 2010 & continent == "Africa") %>% 
  mutate(dollars_per_day = gdp / population / 365) %>% filter(! is.na(dollars_per_day))

gapminder_Africa_2010 %>% ggplot(aes(x = dollars_per_day, y = infant_mortality, color = region)) + 
  geom_point()

# Transform the x axis to be in the log (base 2) scale.
gapminder_Africa_2010 %>% ggplot(aes(x = dollars_per_day, y = infant_mortality, color = region)) + 
  geom_point() + 
  scale_x_continuous(trans = "log2")

# Add a layer to display country names instead of points.
gapminder_Africa_2010 %>% ggplot(aes(x = dollars_per_day, y = infant_mortality, color = region, label = country)) + 
  scale_x_continuous(trans = "log2") + 
  geom_text()

# Generate dollars_per_day using mutate and filter for the years 1970 and 2010 for African countries.
# Remember to remove NA values.
# As in the previous exercise, make a scatter plot of infant_mortaility versus dollars_per_day for countries in the African continent.
# As in the previous exercise, use color to denote the different regions of Africa.
# As in the previous exercise, transform the x axis to be in the log (base 2) scale.
# As in the previous exercise, add a layer to display country names instead of points.
# Use facet_grid to show different plots for 1970 and 2010.

gapminder_Africa_1970_2010 <- gapminder %>% filter( year %in% c(1970, 2010) & continent == "Africa") %>% 
  mutate(dollars_per_day = gdp / population / 365) %>% 
  filter(! is.na(dollars_per_day) & ! is.na(infant_mortality) )

gapminder_Africa_1970_2010 %>% ggplot(aes(x = dollars_per_day, y = infant_mortality, color = region, label = country)) + 
  scale_x_continuous(trans = "log2") + 
  geom_text() + 
  facet_grid(. ~ year)
