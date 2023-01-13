// Reanalyse meta-analysis with simulated new study of binary data under random-effects model //
// A basic meta-analysis model, but in matrix form to account for each simulation with a different new study //

// Input data for model
data {
  int<lower=1> Num;                   // # of studies in current meta-analysis
  int<lower=1> Rep;                   // # of simulations to estimate power
  int<lower=0> rc[Rep, Num+1];        // # of events in control group of each study, repeated Rep times
  int<lower=0> nc[Rep, Num+1];        // # of people in control group of each study, repeated Rep times
  int<lower=0> rt[Rep, Num+1];        // # of events in treatment group of each study, repeated Rep times
  int<lower=0> nt[Rep, Num+1];        // # of people in treatment group of each study, repeated Rep times
}

// The (incl. transformed) parameters accepted by the model. 
parameters {
  real mu[Rep, Num+1];                // transformed baseline effect for each study
  real delta[Rep, Num+1];             // transformed effect for each study
  real d[Rep];                        // common-effect on transformed scale
  real<lower=0> sigma[Rep];           // between-study SD                       NEED TO CHECK WHETHER WANT THIS (means I can't have exactly 0)
}

transformed parameters {
  // common effect
  real OR[Rep] = exp(d);  
  // event rates transformed to be on logit scale
  real<lower=0,upper=1> pc[Rep, Num+1];
  real<lower=0,upper=1> pt[Rep, Num+1];
  for (i in 1:Num+1) {
    pc[,i] = inv_logit(mu[,i]);
    for (r in 1:Rep) {
      pt[r,i] = inv_logit(mu[r,i] + delta[r,i]);
    }
  }
}

model {
  for (i in 1:Num+1) {
    // Likelihood
    rc[,i] ~ binomial(nc[,i], pc[,i]);
    rt[,i] ~ binomial(nt[,i], pt[,i]);
    // Priors
    mu[,i] ~ normal(0,100);                // Vague priors
    delta[,i] ~ normal(d, sigma);    // random-effects model with common effect
  }
  d ~ normal(0, 1000);
  sigma ~ cauchy(0,0.5);
}

