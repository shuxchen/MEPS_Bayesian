data {
  int<lower=1> K;                //Number of covariates
  int<lower=0> N;                //Number of observations
  int<lower=1> L;                //Number of molecule-route-strength units/index
  real y[N];                     //Outcome (log price)   
  int<lower=1,upper=L> ll[N];    //Index 
  row_vector[K] x[N];            //Predictors
  
  //int<lower=0> N_test;                //Number of observations
  //int<lower=1> L_test;                //Number of molecule-route-strength units/index
  //int<lower=1,upper=L_test> ll_test[N_test];    //Index 
  //row_vector[K] x_test[N_test];            //Predictors
  //real y_test[N_test];                     //Outcome (log price)   
}

parameters {
  vector[K] mu;
  vector<lower=0>[K] omega;
  real<lower=0> sigma;
  vector[K] alpha[L];
  //real alpha_pred;

}

transformed parameters {
  vector[K] beta[L];
  //vector[K] beta_pred[L_test];

  for (l in 1:L)
    beta[l] = mu + alpha[l].*omega;
    
  //for (l in 1:L_test)
  //  beta_pred[l] = mu;

}

model {
  mu[1] ~ normal(0, 100);
  mu[2] ~ normal(-0.09, 0.01);
  mu[3] ~ normal(-0.08, 0.02);
  mu[4] ~ normal(-0.02, 0.01);
  mu[5] ~ normal(0, 100);
  mu[6] ~ normal(0, 100);

  omega[1] ~ cauchy(0, 10);
  omega[2] ~ cauchy(0, 2.5);
  omega[3] ~ cauchy(0, 2.5);
  omega[4] ~ cauchy(0, 2.5);
  omega[5] ~ cauchy(0, 2.5);
  omega[6] ~ cauchy(0, 2.5);
  //omega ~ cauchy(0, 2.5);

  sigma ~ inv_gamma(0.0001, 0.0001);
  
  for (l in 1:L)
    alpha[l] ~ normal(0, 1);
  //alpha_pred ~ normal(0, 1);

  
  for (n in 1:N)
    y[n] ~ normal(x[n] * beta[ll[n]], sigma);
    
    
}

generated quantities {
  vector[N] log_lik;
  vector[N] y_pred;

  for(n in 1:N) {
    real y_hat = x[n]*beta[ll[n]];
    log_lik[n] = normal_lpdf(y[n] | y_hat , sigma);
    y_pred[n] = normal_rng(y_hat, sigma);
  }
}