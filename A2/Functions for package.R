library(tidyverse)
library(reshape2)

# Convert Parameters to the new values
convert_param <- function(alpha) {
	new_alpha <- (-1)/(alpha) + (alpha)^(3)
	return(new_alpha)
}

# Estimate the beta parameters from data
estBetaParams <- function(x) {
	mu = mean(x)
	var = var(x)
	alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
	beta <- alpha * (1 / mu - 1)
	return(params = list(alpha = alpha, beta = beta))
}

# Create Ouput
monkey_hammer <- function(inla_result){
	# Checks that the PIT's were computed:
	if (length(inla_result$cpo$pit) == 0) {
		stop("You need to enable computing the CPO's in your INLA computation. \n
			 Add control.compute = list(cpo = TRUE) to the end.")
	}
	## Compute the number summary:
	# Estimate the parameters
	x <- inla_result$cpo$pit
	mu <- mean(x)
	var <- var(x)
	alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
	beta <- alpha * (1 / mu - 1)
	# Transform the parameters
	new_alpha <- convert_param(alpha)
	new_beta <- convert_param(beta)
	# Return the summary:
	param_diff <- new_alpha - new_beta
	param_sum <- new_alpha + new_beta
	
	plotting_data <- data.frame(observed = x,
								predicted = rbeta(length(x), alpha, beta))
	
	plotting_data <- melt(plotting_data)
	
	beta_diagn <- ggplot(data = plotting_data, mapping = aes(x = value, fill = variable)) +
		geom_density(alpha = 0.25) +
		theme_minimal() +
		scale_x_continuous(limits = c(0,1)) +
		labs(title = "Beta fit diagnostics")
	
	PIT_hist <- ggplot(data = tibble(observed = x)) +
		aes(x = observed) +
		geom_histogram(alpha = 0.75, bins = ceiling(1 + 3.322 * log(length(x)))) +
		theme_minimal() +
		scale_x_continuous(limits = c(0,1)) +
		labs(title = "PIT Histogram")
	
	if (param_diff < 0) {
		print("There might be some right bias")
	}
	else if (param_diff > 0) {
		print("There might be some left bias")
	}
	
	if (param_sum < 0) {
		print("There might be some under-dispersion")
	}
	else if (param_sum > 0) {
		print("There might be some over-dispersion")
	}
	
	return(list(difference = param_diff, sum = param_sum, PIT = PIT_hist, beta_diagnostic_plot = beta_diagn))
}