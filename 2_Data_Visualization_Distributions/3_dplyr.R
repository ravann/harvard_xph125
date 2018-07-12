library(dslabs)
library(tidyverse)


heights <- dslabs::heights

str(heights)

# Create summary data frame
s <- heights %>% filter(sex == "Male") %>%
  summarise(average = mean(height), standard_deviation = sd(height))

s

# Extract summary values

murders <- dslabs::murders

str(murders)

us_avg <- murders %>% summarise(rate = sum(total) / sum(population) * 100000) %>% .$rate

# Group and summarise

heights %>% group_by(sex) %>% 
  summarise(average = mean(height), standard_deviation = sd(height))

murders$murder_rate = murders$total / murders$population * 100000

murders %>% group_by(region) %>% summarise(median_rate = median(murder_rate) ) 
str(murders)

# Sort

heights %>% arrange(height) %>% head()

heights %>% arrange(desc(height)) %>% head()

murders %>% arrange(murder_rate) %>% head()

murders %>% arrange(desc(murder_rate)) %>% head()



### ASSIGNMENT

# install.packages('NHANES', dependencies=TRUE, repos='http://cran.rstudio.com/')

library(NHANES)
data(NHANES)

library(dslabs)
data(na_example)
mean(na_example)
sd(na_example)

mean(na_example, na.rm = TRUE)
sd(na_example, na.rm = TRUE)

# Question 1 : Filter the NHANES dataset so that only 20-29 year old females are included and assign this new data frame to the object tab.

head(NHANES)
tab <- NHANES %>% filter(Gender == "female" & AgeDecade == " 20-29")

# Question 2: Complete the line of code to save the average and standard deviation of systolic blood pressure as average and standard_deviation to a variable called ref.
ref1 <- NHANES %>% filter(Gender == "female" & AgeDecade == " 20-29") %>% filter(! is.na(BPSysAve))  %>% summarise(average = mean(BPSysAve), standard_deviation = sd(BPSysAve))
ref2 <- NHANES %>% filter(Gender == "female" & AgeDecade == " 20-29") %>% summarise(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))

# Question 4: Now we will repeat the exercise and generate only the average blood pressure for 20-29 year old females. For this exercise, you should review how to use the place holder . in dplyr.
ref_avg <- NHANES %>%
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>% .$average

# Question 5: Let's continue practicing by calculating two other data summaries: the minimum and the maximum.
NHANES %>%
  filter(AgeDecade == " 20-29"  & Gender == "female") %>% summarize(max = max(BPSysAve, na.rm = TRUE), min = min(BPSysAve, na.rm = TRUE))
  
# Question 6: We will compute the average and standard deviation of systolic blood pressure for females for each age group separately. Remember that the age groups are contained in AgeDecade.

NHANES %>% filter(Gender == "female") %>% group_by(AgeDecade) %>% 
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm=TRUE))
  

# Question 7: We are going to repeat the previous exercise of calculating the average and standard deviation of systolic blood pressure, but for males instead of females.

NHANES %>% filter(Gender == "male") %>% group_by(AgeDecade) %>% 
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm=TRUE))

# Question 8: Compute the average and standard deviation for each value of Race1 for males in the age decade 40-49.
# Order the resulting table from lowest to highest average systolic blood pressure.
# Use the functions filter, group_by, summarize, arrange, and the pipe %>% to do this in one line of code.
# Within summarize, save the average and standard deviation of systolic blood pressure as average and standard_deviation.

NHANES %>% filter(Gender == "male" & AgeDecade == " 40-49") %>% group_by(Race1) %>% 
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>%  arrange(average)

