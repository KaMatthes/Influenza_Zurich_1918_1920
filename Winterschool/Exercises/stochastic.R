# Set random number generator
set.seed(4386)

# Set the parameters of the model
N = 500; seed = 1; R_0 = 1.5; sigma = 1/9; gamma = 1/7; f = 0.7
beta <- R_0*gamma/N

# Set the maximal simulation time
maxtime = 1000

# Initialize plot
plot(NA, NA,
     xlim = c(0, 400), ylim = c(0, N),
     xlab = "Time (days)", ylab = "Cumulative number of cases", frame = FALSE)

# Perform 100 simulations
runs <- 100
outbreak <- as.data.frame(matrix(data = NA, nrow = runs, ncol = 2))
names(outbreak) <- c("duration", "size")
cols <- sample(terrain.colors(runs))
for(i in 1:runs) {
	# Algorithm for Gillespie's direct method
	# Set initial conditions
	S = N - seed; E = 0; I = seed; R = 0; D = 0; C = 0; simtime = 0; event = 0	
	simulation <- matrix(data = NA, nrow = 3*N, ncol = 7)
	simulation[1, ] <- c(simtime, S, E, I, R, D, C)
	while(simtime < maxtime) {
		# Calculate the event rates
		rates <- c(beta*S*I, sigma*E, (1-f)*gamma*I, f*gamma*I)
		sum_rates <- sum(rates)
		cum_rates <- cumsum(rates)
		# Interrupt the loop if infection goes extinct
		if(sum_rates == 0) break
		# Time until next event
		tau <- 1/sum_rates*log(1/runif(1))
		simtime <- simtime + tau
		# Choose which event and update populations
		rand <- runif(1, 0, sum_rates)
		if(rand < cum_rates[1]) {
			S <- S - 1
			E <- E + 1
		} else { 
			if(rand < cum_rates[2]) {
				E <- E - 1
				I <- I + 1
				C <- C + 1
			} else {
				if(rand < cum_rates[3]) {
					I <- I - 1
					R <- R + 1
				} else {
					I <- I - 1
					D <- D + 1
				} 
			}
		}	
		event <- event + 1
		simulation[event + 1, ] <- c(simtime, S, E, I, R, D, C)
	}
	# Plot the cumulative number of cases for each simulation with a different color
	lines(simulation[, 1], simulation[, 7], col = cols[i])
	# Store the duration and size of an outbreak
	outbreak[i, ] <- c(simtime, C)
}
