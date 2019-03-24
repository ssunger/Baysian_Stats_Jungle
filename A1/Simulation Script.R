library(tidyverse) # obvious reasons
library(rstan) # same
library(tidyposterior) # visuals for bayesian
library(tidybayes) # same 
library(bayesplot) # same
library(coda) # bayesian diagnostics
library(here) # working on the project together
set.seed(314159) # For Pi-Day

### Defining All the Helper Functions
Generate_Dataset_BL <- function(num_predictors, num_noise, dataset_size, distribution = "Normal", fixed = 1) {
	# Generates a dataset of length dataset_size from num_predictors predictors sampled
	# from one of the specified distributions with preset params. Adds num_noise of 
	# redundant dimensions to data. Returns a dataset, betas.
	
	### Generate the Betas:
	if (distribution == "Normal") {
		betas <- rnorm(num_predictors)
	}
	else if (distribution == "Exponential") {
		betas <- rexp(num_predictors)
	}
	else if (distribution == "t") {
		betas <- rt(num_predictors, 2)
	}
	else if (distribution == "Cauchy") {
		betas <- rcauchy(num_predictors)
	}
	else if (distribution == "Uniform") {
		betas <- runif(num_predictors)
	}
	else if (distribution == "Fixed"){
		betas <- rep(fixed, num_predictors)
	}
	else {
		stop("Invalid Distribution")
	}
	
	### Generate the sample x's 
	
	# Total number of real x's to sample
	num_real_x <- num_predictors * dataset_size
	real_x <- rnorm(num_real_x)
	
	# Convert to a matrix and then save as a tibble:
	real_x_mat <- matrix(real_x, ncol = num_predictors)
	
	# Compute real y's
	real_y <- real_x_mat %*% betas
	noisy_y <- real_y + rnorm(dataset_size, sd = 0.1)
	
	# Sample Noise x's 
	num_noise_x <- num_noise * dataset_size
	noise_x <- rnorm(num_noise_x)
	noise_x_mat <- matrix(noise_x, ncol = num_noise)
	
	# Combine into data:
	real_x_df <- as_tibble(real_x_mat)
	noise_x_df <- as_tibble(noise_x_mat)
	noisy_y_df <- as_tibble(noisy_y)
	combined_x <- bind_cols(real_x_df, noise_x_df)
	names(combined_x) <- paste0('x', 1:(ncol(combined_x)))
	
	# Return
	return(list(combined_x, noisy_y_df, betas))
}

### Set Params of the sim:

# Real Parameter Value:
b <- c(0.1,0.5,1,5)
# Real Parameter Numbers:
m <- c(1,5,10,50)
# Noise Parameter Numbers:
k <- c(1,10,100,1000)
# Dataset Size:
n <- c(10,100)#,1000,10000)

# All Ran through different Lasso Models with
# tau = 0.1, 1, 5, 10

# tau = 10, Lasso1.stan
# tau = 5, Lasso2.stan (not made yet)
# tau = 1, Lasso3.stan (not made yet)
# tau = 0.1, Lasso4.stan (not made yet)

### Script Loop:

results_list <- list() 
i <- 1
for (beta in b) {
	for (num_params_real in m) {
		for (num_params_fake in k) {
			for (dataset_size in n) {
				# Create The Dataset
				
				df <- Generate_Dataset_BL(num_params_real, num_params_fake, dataset_size, distribution = "Fixed", fixed = beta)

				## Prepare data:
				x <- df[[1]]
				y <- df[[2]]$V1
				betas <- df[[3]]
				
				## Create data list for stan:
				stan_data <- list(N = dataset_size,
								  K = ncol(x), y = y, X = x)
				
				# Run Stan
				fit <- stan(file = "Lasso1.stan",
							data = stan_data,
							chains = 5,
							iter = 10000,
							warmup = 2000)
				
				check_hmc_diagnostics(fit)
				
				fit_summary <- summary(fit)
				
				# Extract Betas Estimates and some values
				
				results = list(beta = beta,
							   num_params_real = num_params_real,
							   num_params_fake = num_params_fake,
							   dataset_size = dataset_size,
							   fit_summary = fit_summary)
				
				results_list[[paste0("run_",i)]] = results
				
				print(paste0("run_",i))
				i <- i + 1
				
			}
		}
	}
}

save(results_list, file = "results_list.Rdata")
