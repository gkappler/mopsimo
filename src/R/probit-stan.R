
source("lib_gregor.R")
##  Please make sure to load data from MQ_Felix.R

library(rstan)
set_cppo("fast")  # for best running speed

probit_dat <- list(J = length(unique(dat$pid)), 
K = length(unique(dat$pic)), 
N = nrow(dat),
pid = as.numeric(as.factor(dat$pid)),
pic = as.numeric(dat$pic),
y = dat$ach)

probit_code_main_effects_pow <-
probit_code_main_effects_aff <-
  probit_code_main_effects_ach <- '
  data {
    int<lower=1> J;                // number of students
    int<lower=1> K;                // number of questions
    int<lower=1> N;                // number of observations
    int<lower=1,upper=J> pid[N];    // student for observation n
    int<lower=1,upper=K> pic[N];    // question for observation n
    int<lower=0,upper=1> y[N];     // correctness of observation n
  }
  parameters {    
    real mu;                       // mean student ability
    real theta[J];                 // ability for student j - mean ability
    real beta[K];                  // difficulty for picture k
    real<lower=0> sigma_theta;     // sd of student abilities  
    real<lower=0> sigma_beta;      // sd of question difficulties 
  }
  model {
    theta ~ normal(0,sigma_theta); // informative priors for identification?
    beta ~ normal(0,sigma_beta);   // informative priors for identification?
    mu ~ normal(-1,10);
    for (n in 1:N) 
      y[n] ~ bernoulli(Phi_approx(theta[pid[n]] + beta[pic[n]] + mu));
  }
  '

probit_dat <- list(J = length(unique(dat$pid)), 
K = length(unique(dat$pic)), 
N = nrow(dat),
pid = as.numeric(as.factor(dat$pid)),
pic = as.numeric(dat$pic),
y = dat$ach)
system.time(fit.stan.probit.ach <- stan(model_code = probit_code_main_effects_ach, data = probit_dat, 
            iter = 1000, chains = 4))

probit_dat <- list(J = length(unique(dat$pid)), 
K = length(unique(dat$pic)), 
N = nrow(dat),
pid = as.numeric(as.factor(dat$pid)),
pic = as.numeric(dat$pic),
y = dat$aff)
system.time(fit.stan.probit.aff <- stan(model_code = probit_code_main_effects_aff, data = probit_dat, 
            iter = 1000, chains = 4))

probit_dat <- list(J = length(unique(dat$pid)), 
K = length(unique(dat$pic)), 
N = nrow(dat),
pid = as.numeric(as.factor(dat$pid)),
pic = as.numeric(dat$pic),
y = dat$pow)
system.time(fit.stan.probit.pow <- stan(model_code = probit_code_main_effects_pow, data = probit_dat, 
            iter = 1000, chains = 4))

pdf(file="stan-probit-pow.pdf")
plot(fit.stan.probit.pow)
dev.off()
pdf(file="stan-probit-ach.pdf")
plot(fit.stan.probit.ach)
dev.off()
pdf(file="stan-probit-aff.pdf")
plot(fit.stan.probit.aff)
dev.off()

rel <- matrix(c(getRel1.stan(fit.stan.probit.pow),
getRel1.stan(fit.stan.probit.ach),
getRel1.stan(fit.stan.probit.aff)),
ncol=1)
colnames(rel)<-c("rel probit")
rownames(rel)<-motives
round(rel,3)

probit_code_iact_effects <- '
/**
 * HIERARCHICAL PL2
 */
data {
  int<lower=1> J;                // number of students
  int<lower=1> K;                // number of questions
  int<lower=1> N;                // number of observations
  int<lower=1,upper=J> pid[N];    // student for observation n
  int<lower=1,upper=K> pic[N];    // question for observation n
  int<lower=0,upper=1> y[N];     // correctness of observation n
}
parameters {    
  real delta;                    // mean student ability
  real theta[J];                 // ability for student j - mean ability
  real beta[K];                  // difficulty for picture k
  real gamma[K,J];               // difficulty for picture k, student j
  real<lower=0> sigma_theta;     // sd of student abilities  
  real<lower=0> sigma_beta;      // sd of question difficulties 
  real<lower=0> sigma_gamma;      // sd of question difficulties 
}
model {
  theta ~ normal(0,sigma_theta); // informative priors for identification
  beta ~ normal(0,sigma_beta);   // informative priors for identification
  gamma ~ normal(0,sigma_gamma);   // informative priors for identification
  delta ~ normal(0,sigma);       // informative priors for identification?
  delta ~ cauchy(0,5);
  sigma_theta ~ cauchy(0,5);
  sigma_beta ~ cauchy(0,5);
  sigma_gamma ~ cauchy(0,5);
  for (n in 1:N)
    y[n] ~ bernoulli_logit( theta[pid[n]] - beta[pic[n]] + gamma[pic[n],pid[n]] + delta );
}
'
