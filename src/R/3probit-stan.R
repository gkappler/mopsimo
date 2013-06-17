
source("lib_gregor.R")
##  Please make sure to load data from MQ_Felix.R

require(rstan)  
motives <- c("pow","ach","aff")
probit_dat3 <- list(J    = length(unique(dat$pid)), 
                 K    = length(unique(dat$pic)), 
                 N    = nrow(dat),
                 pid  = as.numeric(as.factor(dat$pid)),
                 pic  = as.numeric(dat$pic),
                 y    = as.matrix(dat[,motives]),
                 zero = rep(0,3))

nrow(dat)

stan_code_probits3_main <- '
  /**
   * main effects, all motives
   */
  data {
    int<lower=1> J;                // number of students
    int<lower=1> K;                // number of pictures
    int<lower=1> N;                // number of observations
    int<lower=1,upper=J> pid[N];    // student for observation n
    int<lower=1,upper=K> pic[N];    // question for observation n
    int<lower=0,upper=1> y[N,3];     // rating of observation n
    vector[3] zero;
  }
  parameters {    
    real mu[3];                         // Mittlere Motivausprägung
    vector[3] theta[J];                 // Motivausprägung for student j - Mittlere Motivausprägung
    vector[3] beta[K];                  // Anregungspotential Bild k
//    vector[3] gamma[K,J];               // 
    cov_matrix[3] sigma_theta;         // Cov der Motivausprägungen
    cov_matrix[3] sigma_beta;          // Cov der Anregungspotentiale
//    cov_matrix[3] sigma_gamma;      // 
  }
  model {
    for (j in 1:J)
      theta[j] ~ multi_normal(zero,sigma_theta); 
    for (k in 1:K) 
      beta[k] ~ multi_normal(zero,sigma_beta);   

//    for (j in 1:J)
//      for (k in 1:K) 
//      gamma[k,j] ~ multi_normal(zero,sigma_gamma);   
    for (n in 1:N) {
      for (m in 1:3) 
        y[n,m] ~ bernoulli(Phi_approx(theta[pid[n],m] + beta[pic[n],m] + mu[m]));
                       // gamma[pic[n],pid[n],m] +
    }
  }  
  '
  
  fit.probits3.main <- stan(model_code = stan_code_probits3_main, data = probit_dat3, 
              iter = 1000, chains = 4)

pdf(file="stan-probits3.pdf")
plot(fit.probits3.main)
dev.off()
round(getRel3.stan(fit.probits3.main,2))

#######################################################################

  stan_code_probits3_iact <- '
  /**
   * interaction effects all motives
   */
  data {
    int<lower=1> J;                // number of students
    int<lower=1> K;                // number of pictures
    int<lower=1> N;                // number of observations
    int<lower=1,upper=J> pid[N];    // student for observation n
    int<lower=1,upper=K> pic[N];    // question for observation n
    int<lower=0,upper=1> y[N,3];     // rating of observation n
    vector[3] zero;
  }
  parameters {    
    real mu[3];                         // Mittlere Motivausprägung
    vector[3] theta[J];                 // Motivausprägung for student j - Mittlere Motivausprägung
    vector[3] beta[K];                  // Anregungspotential Bild k
    vector[3] gamma[K,J];               // 
    cov_matrix[3] sigma_theta;         // Cov der Motivausprägungen
    cov_matrix[3] sigma_beta;          // Cov der Anregungspotentiale
    cov_matrix[3] sigma_gamma;         // 
  }
  model {
    for (j in 1:J)
      theta[j] ~ multi_normal(zero,sigma_theta); 
    for (k in 1:K) 
      beta[k] ~ multi_normal(zero,sigma_beta);   

    for (j in 1:J)
      for (k in 1:K) 
      gamma[k,j] ~ multi_normal(zero,sigma_gamma);   
    for (n in 1:N) {
      for (m in 1:3) 
        y[n,m] ~ bernoulli(Phi_approx(theta[pid[n],m] + beta[pic[n],m] + mu[m]));
                       // gamma[pic[n],pid[n],m] +
    }
  }  
  '
  
  fit.probit3.iact <- stan(model_code = stan_code_probits3_iact, data = probit_dat3, 
              iter = 100, chains = 4)

pdf(file="stan-probits3-iact.pdf")
plot(fit.probit3.iact)
dev.off()
round(getRel3.stan(fit.probit3.iact,2))
