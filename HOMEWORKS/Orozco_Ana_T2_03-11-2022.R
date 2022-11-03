##################################
## ANA PALOMA OROZCO CARSTENSEN ##
## INFECTIOUS DISEASE MODELING ###
########## HOMEWORK 2 ############ 
######### 03 - 11 - 2022 #########
##################################

library (deSolve) # Load library

## SIR model with demography

SIR <- function (time, state, parameters) { # Start the function 
  with (as.list (c (state, parameters)), { # Create the list of the equations
    dS <- - beta * S * I / (S + I + R) + mN - mS # dS equation 
    dI <- beta * S * I / (S + I + R) - gama * I - mI # dI equation 
    dR <- gama * I - mR # dR equation 
    list (c (dS, dI, dR)) # Make a list with the equations 
  })
} 

parameters <- c (beta = 5, gama = 2, mN = 1, mS = 0.5, mI = 0.8, mR = 0.1) # Set parameters
initial_conditions <- c (S = 900, I = 98, R = 2) # Set the initial conditions 
time <- seq (0, 10, by = 0.001) # Set the time conditions, from 0 to 10 
out <- ode (initial_conditions, time, SIR, parameters) # Solve the system of the equations

matplot (out [ , 1], out [ , 2 : 4], type = "l", xlab = "Time", ylab = "Population", main = "SIR MODEL WITH DEMOGRAPHY", lwd = 3) # Create the plot
legend ("topright", c ("Susceptible", "Infected", "Recovered"), col = 1 : 3, lty = 1 : 3, cex = 0.5) # Add the legend


## SEIR model with demography

SEIR <- function (time, state, parameters) { # Start the function 
  with (as.list (c (state, parameters)), { # Create the list of the equations
    dS <- - beta * S * I / (S + E + I + R) + mN - mS # dS equation 
    dE <- beta * S * I / (S + E + I + R) - gama * E - mE # dE equation 
    dI <- gama * E - lambda * I - mI # dI equation 
    dR <- lambda * I - mR # dR equation 
    list (c (dS, dE, dI, dR)) # Make a list with the equations 
  })
} 

parameters <- c (beta = 10, gama = 7, lambda = 1, mN = 1, mS = 0.5, mE = 0.5, mI = 1, mR = 0.1) # Set parameters
initial_conditions <- c (S = 500, E = 400, I = 98, R = 2) # Set the initial conditions 
time <- seq (0, 10, by = 0.001) # Set the time conditions, from 0 to 10 
out <- ode (initial_conditions, time, SEIR, parameters) # Solve the system of the equations

matplot (out [ , 1], out [ , 2 : 5], type = "l", xlab = "Time", ylab = "Population", main = "SEIR MODEL WITH DEMOGRAPHY", lwd = 3) # Create the plot
legend ("topright", c ("Susceptible", "Exposed", "Infected", "Recovered"), col = 1 : 4, lty = 1 : 4, cex = 0.5) # Add the legend


## SEIRS model with demography

SEIRS <- function (time, state, parameters) { # Start the function 
  with (as.list (c (state, parameters)), { # Create the list of the equations
    dS <- - beta * S * I / (S + E + I + R) + epsilon * R  + mN - mS # dS equation 
    dE <- beta * S * I / (S + E + I + R) - gama * E - mE # dE equation 
    dI <- gama * E - lambda * I - mI # dI equation 
    dR <- lambda * I + epsilon * R  - mR  # dR equation 
    list (c (dS, dE, dI, dR)) # Make a list with the equations 
  })
} 

parameters <- c (beta = 3, gama = 5, lambda = 2, epsilon = 0.2, mN = 1, mS = 0.5, mE = 0.5, mI = 1, mR = 0.1) # Set parameters
initial_conditions <- c (S = 500, E = 400, I = 98, R = 2) # Set the initial conditions 
time <- seq (0, 10, by = 0.001) # Set the time conditions, from 0 to 10 
out <- ode (initial_conditions, time, SEIRS, parameters) # Solve the system of the equations

matplot (out [ , 1], out [ , 2 : 5], type = "l", xlab = "Time", ylab = "Population", main = "SEIRS MODEL WITH DEMOGRAPHY", lwd = 3) # Create the plot
legend ("topright", c ("Susceptible", "Exposed", "Infected", "Recovered"), col = 1 : 4, lty = 1 : 4, cex = 0.5) # Add the legend
