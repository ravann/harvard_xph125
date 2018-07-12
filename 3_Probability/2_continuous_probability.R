library(tidyverse)
library(dslabs)
data(heights)


x <- heights %>% filter(sex == "Male") %>% .$height

F <- function(a) mean(x <= a)

1 - F(82)

# Normal distribution functions

# dnorm

x <- seq(-4, 4, length.out = 100)

dn <- data.frame(x, f = dnorm(x))

data.frame(x, f = dnorm(x)) %>% ggplot(aes(x, f)) + geom_line()


# rnorm
x <- heights %>% filter(sex == "Male") %>% .$height

n <- length(x)
avg <- mean(x)
s <- sd(x)

simulated_heights <- rnorm(n, avg, s)

rnorm(10, avg, s)

# Monte carlo simulations

B <- 10000
tallest <- replicate(B, rnorm(1, avg, s))
max(tallest)


tallest <- replicate(B, { 
  simulated_data <- rnorm(800, avg, s)
  max(simulated_data)
  }
)
max(tallest)
mean(tallest >= 7 * 12)

# Other continuous distributions 


## Exercise 1. Distribution of female heights - 1

# Assume the distribution of female heights is approximated by a normal distribution with 
 # a mean of 64 inches and a standard deviation of 3 inches. 
 # If we pick a female at random, what is the probability that she is 5 feet or shorter?

female_avg <- 64
female_sd <- 3

pnorm(12 * 5, female_avg, female_sd)

  

