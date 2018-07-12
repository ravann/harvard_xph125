install.packages('dslabs', dependencies=TRUE, repos='http://cran.rstudio.com/')

library(ggplot2)
library(dslabs)
library(dplyr)
library(tidyverse)


data("murders")
# murders %>% mutate(rate = total/population*100000)

# To further investigate whether moving to the western region is a wise decision, 
## let's make a box plot of murder rates by region, showing all points.

# Make a box plot of the murder rates by region.
# Order the regions by their median murder rate.
# Show all of the points on the box plot.

murders %>% mutate(rate = total/population*100000 ) %>%
  mutate(region = reorder(region, total, fun = median) )

levels (murders$region)
print(murders$region)

murders %>% mutate(rate = total/population*100000 ) %>%
  mutate(region = reorder(region, total, fun = median) ) %>%
  ggplot(aes(region, rate)) + 
  geom_boxplot() + 
  geom_point(show.legend = FALSE)


