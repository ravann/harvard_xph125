codes <- c(380, 124, 818)

country <- c("italy", "canada", "egypt")

codes

names(codes) <- country

codes <- c(italy = 380, canada = 124, egypt = 818)
codes

class(codes)

 
seq(1, 10)
s <- 1:10
s

codes[1]
codes[c(1,3)]
codes[1:2]


x <- 1:5
x
y <- as.character(x)
y

x <- c(1, "b", 2)
x
as.numeric(x)

library(dslabs)

data(murders)
str(murders)

index <- order(murders$total)

murders$state[index]

i_max = which.max(murders$total)
murders$state[i_max]
