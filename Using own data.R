#INCORPORATING EXISTING DATA
library(squidSim)
data<-data_structure_csv
head(data)
squid_data <- simulate_population(
  data_structure = data[,c("ACTOR","AGFRACTS")],
  parameters = list(
    ACTOR = list(
      vcov = 0.2
    ),
    AGFRACTS = list(
      vcov = 0.3
    ),
    residual = list(
      vcov = 0.5
    )
  )
)

data <- get_population_data(squid_data)
head(data)
