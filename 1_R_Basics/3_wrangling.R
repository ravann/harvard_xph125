murder_rate <- murders$total * 100000 / murders$population

index <- murder_rate < 0.71

murders$state[index]


