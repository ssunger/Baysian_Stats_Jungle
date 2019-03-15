data {
  int<lower=1> N;								// Number of datapoints
  int<lower=1> K;								// Number of predictors
  vector[N] y;     								// Vector of Responses
  matrix[N, K] X;								// Model Matrix
}

parameters {
  vector[K] beta;								// Vector of Parameters
  real<lower=0> sigma;							// Error Scale
}

model {
	vector[N] mu;								// Linear Predictor
	mu = X * beta;								// Linear Predictor ctd.
	// Priors:
	beta ~ double_exponential(0, 10);				
	// Prior on Sigma - this is half cauchy since sigma is bounded 
	sigma ~ cauchy(0,5); 
	
	// Likelihood:
  y ~ normal(mu, sigma);
}
