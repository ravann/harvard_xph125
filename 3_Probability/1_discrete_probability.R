####### PROBABILITY #######

beads <- rep( c("red", "blue"), times = c(2,3) )
beads

sample(beads, 1)

replicate(5, sample(beads, 1))

I <- 10000
events <- replicate(I, sample(beads, 1))

sum(events == "blue")
table(events)


####### PERMUTATIONS And COBINATIONS #######

# Create a deck of cards

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

kings <- paste("King", suits)
mean(deck %in% kings)

# install.packages('gtools', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(gtools)

permutations(5, 2)
combinations(5, 2)

## BIRTHDAY PROBLEM ##

n <- 80
bdays <- sample(1:365, n, replace = TRUE)
table(bdays)
any(duplicated(bdays))

i <- 10000
results <- replicate(i, { 
  bdays <- sample(1:365, n, replace = TRUE) 
  + any(duplicated(bdays))
})
mean(results)

## SAPPLY ##

sapply(1:10, sqrt)

compute_bday_prob <- function(n, i = 10000) {
  results <- replicate(i, { 
    bdays <- sample(1:365, n, replace = TRUE) 
    any(duplicated(bdays))
  })
  mean(results)
}

compute_bday_prob(50)

n <- 1:60
probs <- sapply(n, compute_bday_prob)
probs
plot(probs)

## Removing simulations and applying probability theory

prob_unique <- seq(365, 365 - 5 + 1) / 365

exact_bday_probs <- function(n) {
  prob_unique <- seq(365, 365 - n + 1) / 365
  1 - prod(prob_unique)
}
n <- 1:60
eprobs <- sapply(n, exact_bday_probs)
eprobs
plot(eprobs)

## Monte Carlo Simulation Stability

# Keep N stable and iterate on i
compute_bdays_mc_iters <- function(i, n = 22) {
  results <- replicate(i, { 
    bdays <- sample(1:365, n, replace = TRUE) 
    any(duplicated(bdays))
  })
  mean(results)
}

iters <- as.integer(10^(seq(1, 5, len = 200)))
iters
mcprobs <- sapply(iters, compute_bdays_mc_iters)
plot(log10(iters), mcprobs, type = "l")


##### Exercise 5. Monte Carlo simulation for Celtics winning a game


# This line of sample code simulates four random games where the Celtics either lose or win. Each game is independent of other games.
simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))

# The variable 'B' specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `celtic_wins` that first replicates the sample code generating the variable called `simulated_games` for `B` iterations and then tallies the number of simulated series that contain at least one win for the Celtics.

any(simulated_games == "win")

compute_simulated_games <- function(i) {
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(simulated_games == "win")
}
compute_simulated_games()

celtic_wins <- sapply(1:B, compute_simulated_games)
mean(celtic_wins)

### Monty Hall ###

i <- 10000

stick <- replicate(i, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  stick <- my_pick
  stick == prize_door
})
mean(stick)


switch <- replicate(i, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  switch <- doors[!doors %in% c(my_pick, show)]
  switch == prize_door
})
mean(switch)

## Exercise 1. The Cavs and the Warriors

n <- 6
l <- list(c(1, 0))
t <- expand.grid(rep(l, n))
possibilities <- data.frame(t)
results <- rowSums(possibilities) >= 4

sum(results) / length(results)

## Exercise 2. The Cavs and the Warriors - Monte Carlo

B <- 10000

set.seed(1)

n <- 6
l <- list(c(1, 0))
possibilities <- expand.grid(rep(l, n))
ind <- seq(1:nrow(possibilities))

scenarios <- replicate(B, {
  s <- sample(ind, 1, replace = TRUE)
  sum(possibilities[s, ]) >= 4
}
)

sum(scenarios)


results <- replicate(B, {cavs_wins <- sample(c(0,1),6, replace = TRUE); sum(cavs_wins) >= 4})  
sum(results)
