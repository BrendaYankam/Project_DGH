install.packages("deSolve")
install.packages("ggplot2")
install.packages ("reshape2")
library(deSolve)
library(ggplot2)
library(reshape2)
?ode #?help function#
# INPUT
initial_state_values <- c(S = 1000000,  
                          I = 1,      
                          R = 0)

parameters <- c(beta = 0.3,
                gamma = 0.4)

times <- seq(from = 0, to = 100, by = 1)

# SIR MODEL FUNCTION
sir_model <- function(time, state, parameters) {  
  
  with(as.list(c(state, parameters)), {
    
    N <- S+I+R
    
    lambda <- beta * I/N
    
    # The differential equations
    dS <- -lambda * S              
    dI <- lambda * S - gamma * I
    dR <- gamma * I            
    
    # Output
    return(list(c(dS, dI, dR)))
  })
}

output <- as.data.frame(ode(y = initial_state_values,
                            times = times,
                            func = sir_model,
                            parms = parameters))

# PLOT OF THE MODEL FIT

ggplot() +
  geom_line(data = output, aes(x = time, y = I)) +                              
  xlab("Time (days)")+                                              
  ylab("Prevalence of infection") +                                
  labs(title = paste("Deterministic model output for R0 =",parameters["beta"]/parameters["gamma"])) +
  ylim(c(0,150))
