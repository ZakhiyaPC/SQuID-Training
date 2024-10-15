# here I will try to do a simple simulation which resembles the basic structure of my original data,
# I have one population, 5 groups within it and each group has 9 individuals each. 
# We have multiple (5) instances/observations of the behaviour for each individual (ignoring the partner identity for now) 
# the variations are possible at the group level and at the individual level

# 1) Fit the model with group as a fixed effect and individual as a random effect
# Simulating the data
library(squidSim)

squid_data <- simulate_population(
  data_structure = make_structure(structure="group(5)/individual(9)", repeat_obs=10),
  parameters = list(
    individual = list(
      vcov = 0.1
    ),
    group = list(
      vcov = 0.4
      ),
    residual = list(
      vcov = 0.5
    )
  ),
  response_names = "GroomingTendancy",
#n_pop = 100
)

data <- get_population_data(squid_data, list=TRUE)
length(data)
head(data)
str(data) #displaying the structure of an R object

# Testing the model

#install.packages("lme4")
library(lme4)


# to account for amongst group variation and amongst individual variation without any fixed effects
#both as random effects

#running the model for just one population, since we have done n_pop, we have to index in order to take just one population out.
mod1 <- lmer(GroomingTendancy ~ 1 + (1 | group) + (1 | individual), data = data[[1]]) 

# View the model summary
summary(mod1)

data.frame(VarCorr(mod1))   #viewing to extract random effects  

#to extract random effect for a particular part, here group
data.frame(VarCorr(mod1))[2,4] #indexing done by referring to the data frame after printing to view it

#to extract random effect for a particular part, here individual
data.frame(VarCorr(mod1))[1,4] #indexing done by referring to the data frame after printing to view it

# to extract fixed effect, if it were  there/ multiple ways of doing the same
summary(mod1)$coefficient
intercept=as.numeric(fixef(mod1))
as.numeric(coef(summary(mod1)))


#var(mod$residuals)   #residual variance doesn't automatically show up in the model summary, so have to extract it.

#using lapply to get a distribution for your individual variance. This applies the model to every simulated population, but this works only on lists.


res_list <- lapply(data,function(dat){ 
  
  # Fit the mixed model for each dataset (dat)
    mod1 <- lmer(GroomingTendancy ~ 1 + (1 | group) + (1 | individual), data = dat)
    mod2 <- lmer(GroomingTendancy ~ 1 + (1 | group), data = dat)
    
    #intercept <- as.numeric(fixef(mod1))  # Intercept for the fixed effect (overall)
   # beta1 <- coef(mod1)[2]      # Coefficient for 'group'/ removed bcz there is no fixed effect
    
    
    # Extract the residual variance
    #var_e <- data.frame(VarCorr(mod1))[3,4] # Residual variance
    
   # Extract the p-value for the group effect (second row, 4th column)
   # pval_beta1 <- summary(mod1)$coefficients[1, 1] / removed because no fixed effects, hence no beta1 and no pvalue
    
  # Return the results as a data frame
    data.frame(
      "squid_pop" = dat$squid_pop[1], # Assuming 'squid_pop' exists in 'data'
      "intercept" = as.numeric(fixef(mod1)),
      "Group_variance" = data.frame(VarCorr(mod1))[2,4],
      "Individual_variance" = data.frame(VarCorr(mod1))[1,4],
      "pchisq"=1-pchisq(2*( logLik(mod1)-logLik(mod2)),1), # equivalent to doing annova
      "var_e" = data.frame(VarCorr(mod1))[3,4]
    )
})


results=do.call('rbind',res_list)
head(results)
#Power analysis

sum(results$pchisq<0.05)/ nrow(results) 


# 2) Fit the model with group as a fixed effect and individual as a random effect
#facing some issues in adding levels for fixed effect, couldn't resolve
library(squidSim)

squid_data <- simulate_population(
  data_structure <- make_structure(structure="group(5)/individuals(9)", repeat_obs=10,  level_names=list(group=c("1","2","3","4","5"))),
    parameters = list(
    individual = list(
      vcov = 0.3
    ),
    group = list(
      
            fixed = TRUE,  # Fixed effect for each group (5 levels)
            beta = c(1, 2, 3, 4, 5),
    ),
    residual = list(
      vcov = 0.3
    )
  ),
  response_names = "GroomingTendancy",
  n_pop = 100 #simulationg 100 populations
)

data <- get_population_data(squid_data, list=TRUE), #to get the data outside from the squid_data object


#viewing
head(data)
str(data) #displaying the structure of an R object

#To run the model

install.packages("lme4")
library(lme4)

# to account for amongst group variation and amongst individual variation with group as fixed effect an individual as a random effect
mod3 <- lmer(GroomingTendancy ~ group + (1 | individual), data = data)
summary(mod3)

#running the model for just one population, since we have done n_pop, we have to index in order to take just one population out.
mod3 <- lmer(GroomingTendancy ~ group + (1 | individual), data = data[[1]]) 

# View the model summary
summary(mod3)

data.frame(VarCorr(mod3))   #viewing to extract random effects  

#to extract random effect for a particular part, here group
data.frame(VarCorr(mod3))[2,4] #indexing done by referring to the data frame after printing to view it

#to extract random effect for a particular part, here individual
data.frame(VarCorr(mod3))[1,4] #indexing done by referring to the data frame after printing to view it

# to extract fixed effect, if it were  there/ multiple ways of doing the same
summary(mod3)$coefficient
intercept=as.numeric(fixef(mod3))
as.numeric(coef(summary(mod3)))


#mod$coef
#var(mod$residuals)   #residual variance doesn't automatically show up in the model summary, so have to extract it.

# View the model summary
summary(mod3)

#using lapply to get a distribution for your individual variance.

res_list <- lapply(data,function(dat){ 
  
  # Fit the mixed model for each dataset (dat)
  mod3 <- lmer(GroomingTendancy ~ group + (1 | individual), data = dat)
  
  intercept <- fixef(mod3)[1]  # Intercept for the fixed effect (overall)
  beta1 <- fixef(mod3)[2]      # Coefficient for 'group'
  
  # Extract the residual variance
  var_e <- var(residuals(mod2)) # Residual variance
  
  # Extract the p-value for the group effect (second row, 4th column)
  pval_beta1 <- summary(mod2)$coefficients[2, 1]
  
  # Return the results as a data frame
  data.frame(
    "squid_pop" = data$squid_pop[1], # Assuming 'squid_pop' exists in 'dat'
    "intercept" = intercept,
    "beta1" = beta1,
    "var_e" = var_e,
    "pval_beta1" = pval_beta1
  )
})

data.frame(VarCorr(mod2))   
summary(mod2)
results=do.call(rbind,res_list)
head(results)
hist(results$beta1)

