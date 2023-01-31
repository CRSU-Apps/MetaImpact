### File to run Stan Models ###
library(rstan)

## Test Data ##
test_data_stage1 = list(Num = 6,
                 rc = c(10,51,94,4,8,3),
                 nc = c(22,155,142,87,18,59),
                 rt = c(7,39,97,8,5,12),
                 nt = c(46,154,146,174,15,129),
                 sizeNew = 1000,
                 pcNew = 0.2)

## Compile models as this takes most of the time (will need to think carefully about how to optimise this in app)

GenBinaryRandomFit = stan("GenBinaryRandom.stan", data = test_data_stage1, chains = 1, iter = 2)

## Generate new study

BinaryRandomNewStudy = stan(fit=GenBinaryRandomFit,
                            data = test_data_stage1,
                            chains = 2,
                            iter = 1000,                    # The number of samples here should equal the number of samples I want my power estimate to be based off
                            warmup = 500,
                            thin = 10,                      # Don't want lots of simulations for new study, but want a non-correlated sample
                            init = list(chain1=list(d = 0, deltaNew = 0, sigma=0.1, mu = c(0, 0,0,0,0,0),
                                                    delta = c(0, 0,0,0,0,0)),
                                        chain2=list(d = 1, deltaNew = 1, sigma=0.8, mu = c(1, 1,1,1,1,1),
                                                    delta = c(1,1,1,1,1,1))))
print(BinaryRandomNewStudy)  # check diagnostics
traceplot(BinaryRandomNewStudy, pars="sigma", include=TRUE)
# 0.255 seconds

## Extract values of rtNew and rcNew from each 'simulation'
rtNew = as.data.frame(BinaryRandomNewStudy)$rtNew
rcNew = as.data.frame(BinaryRandomNewStudy)$rcNew

## Data for Stage 2 ##
test_data_stage2 = list(Num = 6, Rep = 100,     # Rep is number of simulations to calculate power and has to match iterations of step 1
                        rc = cbind(matrix(rep(c(10,51,94,4,8,3), 100), nrow=100, byrow=TRUE), rcNew),     # duplicated original data with new simulated studies
                        nc = cbind(matrix(rep(c(22,155,142,87,18,59), 100), nrow=100, byrow=TRUE), rep(1000, 100)),
                        rt = cbind(matrix(rep(c(7,39,97,8,5,12), 100), nrow=100, byrow=TRUE), rtNew),
                        nt = cbind(matrix(rep(c(46,154,146,174,15,129), 100), nrow=100, byrow=TRUE), rep(1000, 100)))

## Compile stage 2 model ##

SimBinaryRandomFit = stan("SimBinaryRandom.stan", data = test_data_stage2, chains=1, iter=2)

# Run stage 2 #

BinaryRandomResults = stan(fit=SimBinaryRandomFit,
                           data = test_data_stage2,
                           chains=1,
                           iter=1200,                          # here the iterations just need to be enough to have proper convergence
                           #warmup=400,
                           thin=1,
                           init = list(chain1=list(d = rep(-0.29,100), sigma=rep(0.39,100), mu = matrix(rep(c(-0.7,-0.76,0.72,-2.99,-0.29,-2.46,0), 100), nrow=100, byrow=TRUE),
                                                   delta = matrix(rep(c(-0.63,-0.31,-0.09,-0.22,-0.33,-0.06,-0.35), 100), nrow=100, byrow=TRUE))))    # added initial values from stage 1
#457.05 seconds
print(BinaryRandomResults, pars="sigma", include=TRUE) # checking sigma Rhat <1.1

# Extract ORs #

ORData = as.data.frame(BinaryRandomResults, pars="OR", include=TRUE)
ORSummary = as.data.frame(summary(BinaryRandomResults, pars="OR", include=TRUE)$summary)  # over all chains

# Calculate power #
ORSummary$Significant <- (ORSummary$`2.5%`<1 & ORSummary$`97.5%`<1) | (ORSummary$`2.5%`>1 & ORSummary$`97.5%`>1)
Power <- sum(as.numeric(ORSummary$Significant))/length(ORSummary$Significant)
# Compare results
prop.test(sum(as.numeric(ORSummary$Significant)),length(ORSummary$Significant))  # 2% (95% CI: 0.3%, 7.7%)
prop.test(3,100) # WinBUGS 3% (95% CI: 0.8%, 9.2%)    happy with that!
# Crowther's Stata functions give: 32.% (95% CI: 23.02%, 42.08%) -> not so happy :(
# Frequentist on app gives: 0% power (95% CI: 0% to 4.6%) -> which is better, but what does this say about how my frequentist is so different to Michael's?

# See if Alex's WinBUGS code gives same answer as figure 6 in Crowther's paper.