# Library for ordinary differential equation solvers
library(deSolve)

R_0 <- 1.5
gamma <- 1/4
S <-  1e4 - 1
I <- 1
R <- 0.2*I


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
parms <- c(R_0 = R_0, gamma = gamma)

# Set the initial values		
inits <- c(S = S, I = I, R = R)

# Set the time points for evaluation
times <- seq(0, 150, 1)					

# Simulate the model using the function ode()
sim <- as.data.frame(ode(inits, times, SIR, parms))

# Plot the results
plot(sim$time, sim$S,
     main=paste0("R=",R_0,", Gamma=", gamma, ",R_vac=", R),
     type = "l", lty = 3, col = "blue", ylim = c(0, sum(inits)),
     xlab = "Time (days)", ylab = "Number of individuals", frame=FALSE)
lines(sim$time, sim$I,
      lty = 1, col = "red")
lines(sim$time, sim$R,
      lty = 2, col = "darkgreen")
legend("topright",
       legend = c("Susceptible, S", "Infected, I", "Recovered, R"),
       col = c("blue", "red", "darkgreen"), lty = c(3,1,2))

  

# 1) Increasing R leads to faster infection -> peak higher and earlier time, more recoveries
# 2) Increasing gamma leads to same infections, but earlier in time, increasing infection duration
# Gamma moves the time of the peak
# Higher R leads to more infected people and higher peak
