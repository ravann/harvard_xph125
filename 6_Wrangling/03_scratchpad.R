day <- c("Monday", "Tuesday")
staff <- c("Mandy, Chris and Laura", "Steve, Ruth and Frank")

library(tidyr)
schedule <- data.frame(day = day, staff = staff)

schedule

str_split(schedule$staff, ",|and")

str_split(schedule$staff, ", | and ")

str_split(schedule$staff, ",\\s|\\sand\\s")

str_split(schedule$staff, "\\s?(,|and)\\s?")

tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ")) %>% unnest()
tidy

tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ", simplify = TRUE)) %>% 
  unnest()

library(gapminder)

gapminder

dat <- gapminder %>% filter(region == "Middle Africa") %>% 
  mutate(country_short = recode(country, 
                                c("Central African Republic", "Congo, Dem. Rep.", "Equatorial Guinea"),
                                c("CAR", "DRC", "Eq. Guinea")))

