library(deSolve)

Worm_dev = function(worm_state, parameters){
  juveniles = worm_state[1:(parameters["k"] - 1)]
  Adults = worm_state[parameters["k"]]
  # Which juveniles 
  juv.develop = rbinom(n = length(juveniles), size = juveniles, prob = parameters["r"])
  new.juveniles = juveniles - juv.develop
  new.juveniles[2:(parameters["k"] - 1)] = new.juveniles[2:(parameters["k"] - 1)] + juv.develop[1:(parameters["k"] - 2)]
  new.Adults = Adults + juv.develop[parameters["k"] - 1]
  return(c(new.juveniles, new.Adults))
}

parameters = c("k" = 28, "r" = 0.67)
worms = c(100, rep(0, times=parameters["k"] - 1))

day = 1
adults = 0

for(i in 1:100){
  worms = Worm_dev(worm_state = worms, parameters)
  adults[i] = worms[parameters["k"]]
  worms[parameters["k"]] = 0
  day[i] = i
}

plot(day, adults)
weighted.mean(x = day, w = adults)



parameters = c("k" = 146, "r" = 0.4)
worms = c(100, rep(0, times=parameters["k"] - 1))

day = 1
adults = 0

for(i in 1:730){
  worms = Worm_dev(worm_state = worms, parameters)
  adults[i] = worms[parameters["k"]]
  worms[parameters["k"]] = 0
  day[i] = i
}

plot(day, adults)
weighted.mean(x = day, w = adults)
abline(v = 335); abline(v=395)


