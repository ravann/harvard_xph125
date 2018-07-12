install.packages('tidyverse', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(tidyverse)
version

library(ggplot2)

library(dslabs)

murders <- dslabs::murders

p <- ggplot(data = murders)
class(p)
p

install.packages('HistData', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(HistData)
data(Galton)
x <- Galton$child

mad(x)

p <- ggplot(data = murders, aes(x = population / 10^6 , y = total, label = abb))
p <- p + geom_point(aes(x = population / 10^6 , y = total), size = 3)
p <- p + geom_text(aes(population / 10^6, total, label = abb), nudge_x = 1)
p


p <- murders %>% ggplot(aes(x = population, y = total, label = abb))
p + geom_point( size = 3)
p + geom_point( size = 3) + geom_text(nudge_y = 2)
p + geom_text(nudge_y = 2) + geom_point( size = 3)

# Use log scale
p + geom_text(nudge_y = 0.1) + geom_point( size = 3) + scale_x_log10() + scale_y_log10() + 
  xlab("Population in Millions (log scale)") + ylab("Total number of Murders (log scale)") + ggtitle("Gun Murders in 2010 in US")

# Color the regions
p + geom_text(nudge_y = 0.1) + geom_point( aes(col = region), size = 3) + scale_x_log10() + scale_y_log10() + 
  xlab("Population in Millions (log scale)") + ylab("Total number of Murders (log scale)") + ggtitle("Gun Murders in 2010 in US")

# GG Themes and GG Repel
# install.packages('ggthemes', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(ggthemes)

install.packages('ggrepel', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(ggrepel)

## Define the slope of the line
r <- murders %>% summarise(rate = sum(total) / sum(population) * 10^6 ) %>% .$rate

p <- murders %>% ggplot(aes(x = population, y = total, label = abb)) +
  geom_point( aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_discrete(name = "Region") +
  xlab("Population in Millions (log scale)") +
  ylab("Total number of Murders (log scale)") +
  ggtitle("Gun Murders in 2010 in US") +
  theme_economist() +
  geom_abline()
p
