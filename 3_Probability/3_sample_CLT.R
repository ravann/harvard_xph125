
# Roulette winnings

color <- rep(c("Black", "Red", "Green"), c(18, 18, 2))

n <- 1000
x <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)

# Earnings bell curve
n <- 1000
B <- 10000
earnings <- replicate(B, {
    x <- sample( c(-1, 1), n, replace = TRUE, prob = c(18/38, 20/38) )
    sum(x)
  }
)

avg <- mean(earnings)
sdev <- sd(earnings)

pnorm(0, avg, sdev)
mean(earnings <= 0)

hist(earnings)

s <- seq(min(earnings), max(earnings), length = 100)
normal_density <- data.frame(s = s, f = dnorm(s, avg, sdev) )

hist(earnings)

library(tidyverse)
data.frame(earnings) %>% ggplot(aes(earnings)) + 
  geom_histogram(color = "black", bandwidth = 10) 

# Expected Value

P_red <- 18/38

EV_c <- 1 * (1 - P_red) + -1 * (P_red) # for casino

EV_p <- -1 * (1 - P_red) + 1 * (P_red) # for player

# EV using monte carlo simulation
n <- 1000000
sims <- sample(c(1, -1), n, replace = TRUE, prob = c(20/38, 18/38))
mean(sims)

# Standard Error

# SE from Monte Carlo
SE_mc <- sqrt(n) * sd(sims)

# SE from math

abs(1 - -1) * sqrt(P_red * (1 - P_red))
