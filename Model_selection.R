load("~/Dropbox/Advanced Method Project/Data/Aim1/MEPS_Bayesian/")

stan_code_M0 <- 'data {
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
'

stan_code_M1 <- 'data {
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
target += normal_lpdf(mu[3] | 0, sqrt(100));
target += inv_gamma_lpdf(omega | 0.01, 0.01);
target += inv_gamma_lpdf(sigma | 0.01, 0.01);

for (l in 1:L)
target += normal_lpdf(beta[l] | mu, sqrt(omega));

for (n in 1:N)
target += normal_lpdf(y[n] | x[n] * beta[ll[n]], sqrt(sigma));
}
'
stan_model_M0 <- stan_model(model_code = stan_code_M0, model_name="stan_model_M0")

fit_M0 <- sampling(stan_model_M0, 
                   data = stan.dat_generic_price_train,
                   chains = 4,             # number of Markov chains
                   warmup = 5000,          # number of warmup iterations per chain
                   iter = 15000,           # total number of iterations per chain
                   cores = 4,              # number of cores (could use one per chain)
                   refresh = 0,            # no progress shown
                   control = list(adapt_delta = 0.9999, max_treedepth = 15)
)

stan_model_M1 <- stan_model(model_code = stan_code_M1, model_name="stan_model_M1")

fit_M1 <- sampling(stan_model_M1, 
                   data = stan.dat_generic_price_train,
                   chains = 4,             # number of Markov chains
                   warmup = 5000,          # number of warmup iterations per chain
                   iter = 15000,           # total number of iterations per chain
                   cores = 4,              # number of cores (could use one per chain)
                   refresh = 0,            # no progress shown
                   control = list(adapt_delta = 0.9999, max_treedepth = 15)
)                      

# compute log marginal likelihood via bridge sampling for H0
M0_bridge <- bridge_sampler(fit_M0, silent = TRUE)
print(M0_bridge)

# compute log marginal likelihood via bridge sampling for H1
M1_bridge <- bridge_sampler(fit_M1, silent = TRUE)
print(M1_bridge)

BF_01 <- bf(M0_bridge, M1_bridge)
print(BF_01)

post1 <- post_prob(M0_bridge, M1_bridge)
print(post1)
