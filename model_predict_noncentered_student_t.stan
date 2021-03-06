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
  real alpha;
  //real alpha_pred;
  real nu;

}

transformed parameters {
  vector[K] beta[L];
  //vector[K] beta_pred[L_test];

  for (l in 1:L)
    beta[l] = mu + omega*alpha;
    
  //for (l in 1:L_test)
  //  beta_pred[l] = mu;

}

model {
  mu[1] ~ normal(0, 100);
  mu[2] ~ normal(-0.08, 0.08);
  //mu[2] ~ normal(0, 100);
  mu[3] ~ normal(0, 100);
  mu[4] ~ normal(0, 100);

  omega ~ inv_gamma(0.01, 0.01);
  sigma ~ inv_gamma(0.01, 0.01);
  
  alpha ~ normal(0, 1);
  //alpha_pred ~ normal(0, 1);
  
  // see Stan prior distribution suggestions
  nu ~ gamma(1, 0.1);

  
  for (n in 1:N)
    y[n] ~ student_t(nu, x[n] * beta[ll[n]], sigma);
    
    
}

generated quantities {
  vector[N] log_lik;
  vector[N] y_pred;

  for(n in 1:N) {
    real y_hat = x[n]*beta[ll[n]];
    log_lik[n] = student_t_lpdf(y[n] | nu, y_hat , sigma);
    y_pred[n] = student_t_rng(nu, y_hat, sigma);
  }
}