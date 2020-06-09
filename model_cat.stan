data {
  int<lower=1> K;                //Number of covariates
  int<lower=0> N;                //Number of observations
  int<lower=1> L;                //Number of molecule-route-strength units/index
  real y[N];                     //Outcome (log price)   
  int<lower=1,upper=L> ll[N];    //Index 
  row_vector[K] x[N];            //Predictors
}

parameters {
  real mu[K];
  real<lower=0> omega[K];
  real<lower=0> sigma;
  vector[K] beta[L];
}

model {
  mu[1] ~ normal(0, 100);
  mu[2] ~ normal(-0.09, 0.01);
  mu[3] ~ normal(-0.08, 0.02);
  mu[4] ~ normal(-0.02, 0.01);
  //mu[2] ~ normal(0, 100);
  //mu[3] ~ normal(0, 100);
  //mu[4] ~ normal(0, 100);
  mu[5] ~ normal(0, 100);
  mu[6] ~ normal(0, 100);


  omega ~ inv_gamma(0.01, 0.01);
  sigma ~ inv_gamma(0.01, 0.01);
  
  for (l in 1:L)
  beta[l] ~ normal(mu, omega);
  
  for (n in 1:N)
    y[n] ~ normal(x[n] * beta[ll[n]], sigma);
}
