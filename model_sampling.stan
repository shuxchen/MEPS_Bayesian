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
  target += normal_lpdf(mu[1] | 0, sqrt(100));
  target += normal_lpdf(mu[2] | -0.08, 0.08);
  target += inv_gamma_lpdf(omega | 0.01, 0.01);
  target += inv_gamma_lpdf(sigma | 0.01, 0.01);

  for (l in 1:L)
    target += normal_lpdf(beta[l] | mu, sqrt(omega));

  for (n in 1:N)
    target += normal_lpdf(y[n] | x[n] * beta[ll[n]], sqrt(sigma));
}
