# Library for ordinary differential equation solvers
library(deSolve)

# Definition of the SIR model
SIR <- function(t, x, parms) {
	with(as.list(c(parms, x)), {
		N <- S + I + R
		beta <- R_0*gamma/N
		dS <- - beta*S*I
		dI <- beta*S*I - gamma*I
		dR <- gamma*I
		der <- c(dS, dI, dR)
		list(der)
	})
}

# Set the parameters of the model
parms <- c(R_0 = 1.5, gamma = 1/4)

# Set the initial values		
inits <- c(S = 1e4 - 1, I = 1, R = 0)

# Set the time points for evaluation
times <- seq(0, 150, 1)					

# Simulate the model using the function ode()
sim <- as.data.frame(ode(inits, times, SIR, parms))

# Plot the results
plot(sim$time, sim$S,
     type = "l", lty = 3, col = "blue", ylim = c(0, sum(inits)),
     xlab = "Time (days)", ylab = "Number of individuals", frame=FALSE)
lines(sim$time, sim$I,
      lty = 1, col = "red")
lines(sim$time, sim$R,
      lty = 2, col = "darkgreen")
legend("topright",
       legend = c("Susceptible, S", "Infected, I", "Recovered, R"),
       col = c("blue", "red", "darkgreen"), lty = c(3,1,2))

  
