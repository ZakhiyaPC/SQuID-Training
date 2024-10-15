library(squidSim)
squid_data <- simulate_population(
  data_structure = make_structure(structure = "individual(10)+treatment(8)", repeat_obs=2),
  parameters = list(
    intercept=0,
    individual = list(
     vcov=0.5
    ),
    treatment = list(
      vcov=0.3
    ),
    
    residual = list(
      vcov = 0.75
    )
  ),
  response_names="swimspeed"
)

data <- get_population_data(squid_data)
library(lme4)
mod <- lmer(swimspeed~1+(1|individual)+(1|treatment), data = data)
summary(mod)

vc <- VarCorr(mod)
# Extract random effect variance for the "group" variable
individual_variance <- as.numeric(vc$individual[1, 1])
print(individual_variance)

treatment_variance <- as.numeric(vc$treatment[1, 1])
print(treatment_variance)

# Extract residual variance (within-group variance)
residual_variance <- sigma(mod)^2
print(residual_variance)



#page 48

squid_data <- simulate_population( 
  data_structure = make_structure(structure = "site(10)/individual(5)", repeat_obs = 5), 
   response_names = "speed", 
   parameters = list( 
	intercept = 10, 
	individual = list( 
		names = c("size","ind_effect"), 
		beta = c(-0.5, 1), 
		mean = c(0,0), 
		vcov = matrix(c(1,0,0,0.2), 2,2)
		), 
 	site = list( 
		names = c("oxygen", "site_effect"), 
		beta = c(0.7,1), 
		mean = c(0,0),
		vcov = matrix(c(1,0,0,0.3),2,2)
		), 
   	residual = list( 
		vcov = 0.5 
		) 
   	)
)
data<-get_population_data(squid_data)
head(data)

#practical 5

squid_data <- simulate_population( 
  data_structure = make_structure(structure = "individual(25)", repeat_obs = 1), 
  response_names = "DivingDuration", 
  parameters = list( 
    intercept = 5, 
    observation = list( 
      names = c("preydensity"), 
      beta = c(0.2), 
      mean = c(0), 
      vcov = c(1)
    ), 
    
    residual = list( 
      vcov = 0.1 
    ) 
  ),
  n_pop = 100
)
 data<-get_population_data(squid_data, list = T)
#head(data)
str(data)
mod<-lm(DivingDuration~preydensity, data = data[[1]])
summary(mod)
mod$coef
#mod$residuals
var(mod$residuals)   #residual variance doesnt automatically show up in the models ummary, so have to extract it.

res_list <- lapply(data,function(dat){  
  m <- lm(DivingDuration~preydensity, data=dat)
  data.frame("squid_pop"=dat$squid_pop[1],
             "intercept"=as.numeric(coef(m)[1]),
             "beta1"=as.numeric(coef(m)[2]),
             "var_e"=var(m$residuals),
             "pval_beta1"=summary(m)$coefficients[2,4])
})

results=do.call(rbind,res_list)
head(results)
hist(results$beta1)

#Calculating power for beta1
sum(results$pval_beta1<0.05)/ nrow(results)  # the output gives power
sum(results$beta1>=0.2)/ nrow(results)       # the output gives p value, for beta estimate accounting for model uncertainity

#practical 5Q3
squid_data <- simulate_population( 
  data_structure = make_structure(structure = "individual(25)", repeat_obs = 1), 
  response_names = "DivingDuration", 
  parameters = list( 
    intercept = 5, 
    observation = list( 
      names = c("preydensity"), 
      beta = c(0), 
      mean = c(0), 
      vcov = c(1)
    ), 
    
    residual = list( 
      vcov = 0.1 
    ) 
  ),
  n_pop = 100
)
data<-get_population_data(squid_data, list = T)
#head(data)
#str(data)
#mod<-lm(DivingDuration~preydensity, data = data[[1]])
#summary(mod)
#mod$coef
#mod$residuals
#var(mod$residuals)   #residual variance doesnt automatically show up in the models ummary, so have to extract it.

res_list <- lapply(data,function(dat){  
  m <- lm(DivingDuration~preydensity, data=dat)
  data.frame("squid_pop"=dat$squid_pop[1],
             "intercept"=as.numeric(coef(m)[1]),
             "beta1"=as.numeric(coef(m)[2]),
             "var_e"=var(m$residuals),
             "pval_beta1"=summary(m)$coefficients[2,4])
})

results=do.call(rbind,res_list)
head(results)
hist(results$beta1)

#Calculating power for beta1
sum(results$pval_beta1<0.05)/ nrow(results)
sum(results$beta1>=0.2)/ nrow(results)

#practical 6- test the significance of a variance component

squid_data <- simulate_population( 
  data_structure = make_structure(structure = "individual(50)", repeat_obs = 10), 
  response_names = "DivingDuration", 
  parameters = list( 
    intercept = 5, 
    individual = list( 
      vcov = c(0.3)
    ), 
    observation = list( 
      names = c("preydensity"), 
      beta = c(0), 
      mean = c(0), 
      vcov = c(1)
    ), 
    
    residual = list( 
      vcov = 0.7 
    ) 
  ),
  n_pop = 100
)
data<-get_population_data(squid_data, list = T)
#head(data)
#str(data)
#mod<-lm(DivingDuration~preydensity, data = data[[1]])
#summary(mod)
#mod$coef
#mod$residuals
#var(mod$residuals)   #residual variance doesnt automatically show up in the models ummary, so have to extract it.


library(lme4)
m1 <- lm(DivingDuration~ 1+preydensity, data=data[[1]])
m2 <- lmer(DivingDuration~1+preydensity+(1|individual), data=data[[1]], REML = FALSE)
logLik(m1)
logLik(m2)
1-pchisq(2*( logLik(m1)-logLik(m2)),1) # equivalent to doing annova

anova(m1,m2)

res_list <- lapply(data,function(dat){  
  m1 <- lm(DivingDuration~ 1+preydensity, data=dat)
  m2 <- lme4::lmer(DivingDuration~1+preydensity+(1|individual), data=dat)
  data.frame("squid_pop"=dat$squid_pop[1],
             
       "pvalue"= as.numeric(1-pchisq(2*( logLik(m2)-logLik(m1)),1) # equivalent to doing annova
))
})

results=do.call(rbind,res_list)
head(results)
#hist(results$beta1)

#Calculating power for beta1
sum(results$pvalue<0.05)/ nrow(results)

head(results)

