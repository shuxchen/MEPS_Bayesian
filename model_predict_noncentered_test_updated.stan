data {
  int<lower=1> K;                //Number of covariates
  int<lower=0> N;                //Number of observations
  int<lower=1> L;                //Number of molecule-route-strength units/index
  real y[N];                     //Outcome (log price)   
  int<lower=1,upper=L> ll[N];    //Index 
  row_vector[K-1] x[N];            //Predictors
  real x_N[N];
  real informative_coefficient;
  
  int<lower=0> N_test;                //Number of observations
  int<lower=1> L_test;                //Number of molecule-route-strength units/index
  int<lower=1,upper=L_test> ll_test[N_test];    //Index 
  row_vector[K-1] x_test[N_test];            //Predictors
  real x_N_test[N_test];
  //real y_test[N_test];                     //Outcome (log price)   
}

parameters {
  vector[K-1] mu;
  real mu_N;
  vector<lower=0>[K-1] omega;
  real<lower=0> omega_N;
  real<lower=0> sigma;
  row_vector[K-1] alpha;
  row_vector[K-1] alpha_pred;
  real alpha_N[L];
  real alpha_N_pred[L_test];
  
}

transformed parameters {
  vector[K-1] beta;
  vector[K-1] beta_pred;
  real beta_N[L];
  real beta_N_pred[L_test];
  
  beta = mu + alpha * omega;
  beta_pred = mu + alpha_pred * omega;
  
  for (l in 1:L)
    beta_N[l] = mu_N + alpha_N[l]*omega_N;
  
  for (l in 1:L_test)
    beta_N_pred[l] = mu_N + alpha_N_pred[l]*omega_N;
  
}

model {
  mu[1] ~ normal(0, 100);
  mu[2] ~ normal(0, 100);
  mu[3] ~ normal(0, 100);
  
  if (informative_coefficient == 1){
    mu_N ~ normal(-0.075, 0.051);
  } else {
    mu_N ~ normal(0, 100);
  }
  
  omega[1] ~ cauchy(0, 10);
  omega[2] ~ cauchy(0, 2.5);
  omega[3] ~ cauchy(0, 2.5);
  
  omega_N ~ cauchy(0, 2.5);
  
  sigma ~ inv_gamma(0.01, 0.01);
  
  alpha ~ normal(0, 1);
  alpha_pred ~ normal(0, 1);
  
  for (l in 1:L)
    alpha_N ~ normal(0, 1);
  
  for (l in 1:L_test)
    alpha_N_pred[l] ~ normal(0, 1);
  
  
  for (n in 1:N)
    y[n] ~ normal(x[n] * beta + x_N[n] * beta_N[ll[n]], sigma);
  
  
}

generated quantities {
  real y_pred_test[N_test];
  
  for(n in 1:N_test) {
    y_pred_test[n] = normal_rng(x_test[n] * beta_pred + x_N_test[n] * beta_N_pred[ll_test[n]], sigma);
    #y_pred_test[n] = normal_rng(x_test[n] * mu + x_N_test[n] * mu_N, sigma);
    
  }
}