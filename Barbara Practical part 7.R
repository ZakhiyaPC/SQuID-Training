library(squidSim)
squid_data <- simulate_population(
  data_structure = make_structure(structure = "location(10)/individual(5)", repeat_obs=5),
    parameters = list(
      intercept=10,
    individual = list(
      names = "size",
      beta = -0.4
    ),
    observation = list(
      names="Oxygenlevel",
      beta = 0.7
    ),
    residual = list(
      vcov = 0.5
    )
  ),
  response_names="swimspeed"
  )

data <- get_population_data(squid_data)

mod <- lm(swimspeed~size+Oxygenlevel, data = data)
summary(mod)
mod$coef
mod$residuals
var(mod$residuals)


        