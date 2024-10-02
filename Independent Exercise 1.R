# here I will try to do a simple simulation which resembles the basic structure of my original data,
# I have one population, 5 groups within it and each group has 9 individuals each. 
# We have multiple (5) instances/observations of the behaviour for each individual (ignoring the partner identity for now) 
# the variations are possible at the group level and at the individual level


library(squidSim)

squid_data <- simulate_population(
  data_structure = make_structure(structure="group(5)/individual(9)", repeat_obs=10),
  parameters = list(
    individual = list(
      vcov = 0.5
    ),
    group = list(
      vcov = 0.5
      ),
    residual = list(
      vcov = 0.5
    )
  ),
  response_names = "GroomingTendancy",
#  n_pop = 100
 )

data <- squid_data
head(data)
str(data)
mod <- lmer(GroomingTendancy ~ 1 + (1 | group) + (1 | individual), data = data) # to account for amongst group variation and amongst individual variation without any fixed effects
summary(mod)
mod$coef
var(mod$residuals)   #residual variance doesnt automatically show up in the models ummary, so have to extract it.
