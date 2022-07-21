# Sourcing minimal sample size Functions #
#----------------------------------------#

# Other functions required #
source("MAFunctions.R",local = TRUE) 
source("SampleSizeFunctions.R", local=TRUE)
source("ForestFunctions.R", local=TRUE)


# Objects needed to play with whilst developing #
#-----------------------------------------------#

# Data and extras #
data <- read.csv("./AntiVEGF_Binary_Pairwise.csv")
levels <- levels(as_vector(lapply(data[grep("^T", names(data), value=TRUE)], factor)))  #extract treatment names
ref <- "RANI"
trt <- "BEVA"
contbin <- "binary"
outcome <- "OR"
wide_data <- Long2Wide(data)

# Frequentist Pairwise MA #
freqMA <- FreqPair(data=wide_data, outcome=outcome, model='both', CONBI=contbin, trt=trt)

# Focusing on finding just one result (rather than coverage for a range) #
# Need to decide what value of nit gives me a 'perfect' power estimate...otherwise it'll probably fluctuate around...or a threshold interval like the paper does...
# Crowther paper suggests 1,000. 
# So 5 iterations? nit=50, 100, 250, 500, 1,000?


# Step One #
#----------#
# Find a ball-park range #
# Set of 10 sample sizes, where, after a small number of iterations, the desired power range is included in the CI intervals #

# function for ensuring even sample sizes
make_even <- function(nums) {
  for (i in 1:length(nums)) {
    nums[i] <- floor(nums[i]/2)*2
  }
  return(nums)
}

get_ballpark_range <- function(n_range, longdata=data, widedata=wide_data, MA=freqMA, nit=50, inference='pvalue', pow=0.35, measure=outcome, first=TRUE, sample.range) {   # n_range = number of sample sizes; longdata = MA data in long format; widedata = MA data in wide format; MA = meta-analysis (fixed & random), nit = number of iterations; inference = type of impact; pow = inference cut-off; measure = type of outcome measure (e.g. OR); first = whether its the first iteration; sample.range = range of sample sizes when 'first' is false
  # To speed things up, consider having a fixed/random/both option - need to implement it into metapow command first
  
  # 10 sample sizes with same range as current MA, evenly spaced #
  if (first==TRUE) {
    ballpark_samples <- make_even(seq(from = min(longdata$N)*2, to = max(longdata$N)*2, length.out = n_range))  # x2 to get total sample size
  } else {
    ballpark_samples <- make_even(sample.range)
  }
  
  # Calculate power for initial ball-park range #
  pow_results <- metapow_multiple(SampleSizes=ballpark_samples, NMA=MA, data=widedata, nit=nit, inference=inference, pow=pow, measure=measure)
  pow_results_F <- pow_results[pow_results$Model=='Fixed-effects',]   # just to ensure we're sticking with fixed for now.
  
  # Change ball-park range if needed #
  # Trialled +/- 10% rather than +/- 15%, but it wasn't going high enough quick enough.
  while (min(pow_results_F$CI_lower) > 65 | max(pow_results_F$CI_upper) < 95) {    # ballpark sample needs changing
    if (max(pow_results_F$CI_upper) < 65) {
      ballpark_min <- ballpark_samples[n_range]    # new set of numbers all bigger #
      ballpark_max <- ballpark_samples[n_range]*2
    } else if (min(pow_results_F$CI_lower) > 95) {
      ballpark_max <- ballpark_samples[1]     # new set of numbers all smaller #
      ballpark_min <- ballpark_samples[1]/2
    } else if ((65 < min(pow_results_F$CI_lower) | min(pow_results_F$CI_lower) < 95) & max(pow_results_F$CI_upper) > 95) {
      ballpark_min <- ballpark_samples[1]/2   # include smaller sample sizes
      ballpark_max <- ballpark_samples[n_range]
    } else if ((65 < max(pow_results_F$CI_upper) | max(pow_results_F$CI_upper) < 95) & min(pow_results_F$CI_lower) < 65) {
      ballpark_max <- ballpark_samples[n_range]*2  # include larger sample sizes
      ballpark_min <- ballpark_samples[1]
    } else if ((65 < min(pow_results_F$CI_lower) | min(pow_results_F$CI_lower) < 95) & (65 < max(pow_results_F$CI_upper) | max(pow_results_F$CI_upper) < 95)) {
      ballpark_min <- ballpark_samples[1]/2   # include smaller and larger sample sizes
      ballpark_max <- ballpark_samples[n_range]*2
    }
    ballpark_samples <- make_even(seq(from = ballpark_min, to = ballpark_max, length.out = n_range))
    pow_results <- metapow_multiple(SampleSizes=ballpark_samples, NMA=MA, data=widedata, nit=nit, inference=inference, pow=pow, measure=measure) # redo power analysis with new range
    pow_results_F <- pow_results[pow_results$Model=='Fixed-effects',]
  }
  
  return(pow_results)   # contains ball-park range and power results
  ## Consider having a 'too large' option that doesn't go any further if reached (i.e. if the user is asking for something ridiculous)
  
}


results <- get_ballpark_range(n_range=10, nit=50)  # defaults are set for this example


# Step Two #
#----------#
# Create a model of power and sample size using the latest set of sample sizes #
# need a monotone non-decreasing spline because of the assumption that the bigger the sample size, the larger the power #
# Using code from stackexchange (https://stats.stackexchange.com/questions/519465/how-to-correctly-use-i-splines-for-monotone-non-decreasing-increasing-regressio)
# NEED TO: put in requirement that yvar cannot go beyond 100

create_mono_spline <- function(xvar, yvar, knots=median(xvar), origin=TRUE) {  #xvar = independent data; yvar = dependent data; knots = internal knots; origin = whether to include origin in model
  # Create basis #
  ispline <- splines2::iSpline(xvar, knots = knots, degree = 2, intercept = TRUE)  # inner knot at median gives point of inflection
  if (origin == FALSE) {
    ispline <- cbind(1, ispline) # Allows an intercept term to be non-zero
  }
  # Ensure coefficients are non-negative using a 'solver'
  d_mat <- crossprod(ispline, ispline)
  d_vec <- crossprod(ispline, yvar)
  a_mat <- diag(1, ncol(ispline))
  b_vec <- rep(0, ncol(ispline))
  a_mat[1,1] <- 0  # freeing the first parameter
  alpha_non_negative <- quadprog::solve.QP(Dmat = d_mat, dvec = d_vec, Amat = t(a_mat), bvec = b_vec)$solution
  # Return spline and coefficients 
  return(list(spline = ispline %*% alpha_non_negative, coeffs = alpha_non_negative))
}
create_spline_model <- function(data, knots, xvar, yvar, origin=TRUE) {  # data = data; knots = internal knots; xvar = independent data; yvar = dependent data; origin = whether to include origin in model
  # I-spline model
  mylm = lm(Estimate ~ splines2::iSpline(SampleSize, 
                                         knots=knots,
                                         degree=2, intercept=TRUE), 
            data=data)
  # Identical spline with non-negative coefficients
  spline_results <- create_mono_spline(xvar = xvar, 
                                       yvar = yvar,
                                       knots = knots,
                                       origin = origin)
  # Add in fitted values and coefficients into model
  if (origin == TRUE) {
    mylm$coefficients <- c(0, spline_results$coeffs) # give the non-negative coefficients (and zero intercept) 
  } else {
    mylm$coefficients <- spline_results$coeffs
  }
  mylm$fitted.values <- array(spline_results$spline) # and responding fitted values
  return(mylm)
}

## This worked, and uses commands from splines2 package! #
#require(graphics); require(stats)
#plot(women$height, women$weight, xlab = "Height (in)", ylab = "Weight (lb)")
#knots = median(women$height)
#mylm = lm(weight ~ mSpline(height, knots=knots, degree=3, intercept=TRUE), data=women)
#pr = predict(mylm, newdata=data.frame(height = seq(58,72,0.5)))
#lines(seq(58,72,0.5),pr,col = "red")

# Try with my dataset... #
# As this example has the smallest sample size as the smallest MA study, add (0,0) to data (no sample, no power) - current function gives a fitted value of 0 for the first data point - ensures this data point is at sample size 0
results_origin <- rbind(c(0, "Fixed-effects", 0, 0, 0), c(0, "Random-effects", 0, 0, 0), results)
results_origin[,c('SampleSize','Estimate','CI_lower','CI_upper')] <- as.data.frame(sapply(results_origin[,c('SampleSize','Estimate','CI_lower','CI_upper')], as.numeric))
# Create initial plot of data
#plot(results_origin$SampleSize[results_origin$Model=="Fixed-effects"], results_origin$Estimate[results_origin$Model=="Fixed-effects"], 
#     xlab = "Sample Size", ylab = "Power", xlim=c(0, 76700), ylim=c(0,100))
# Create spline model
#mylm <- create_spline_model(data = results_origin[results_origin$Model=="Fixed-effects",],
#                            knots = quantile(results$SampleSize[results$Model=="Fixed-effects"], probs=c(1/3, 2/3)),
#                            xvar = results_origin$SampleSize[results_origin$Model=="Fixed-effects"],
#                            yvar = results_origin$Estimate[results_origin$Model=="Fixed-effects"])
# Interpolate to get a smooth function and granular results 
#interp_range <- seq(ballpark_samples[1],ballpark_samples[10],length.out=200)
#interp_range <- seq(0, 76700, length.out=200)
#pr = predict(mylm, newdata=data.frame(SampleSize = interp_range))
#lines(interp_range,pr,col = "red")
# It works - I just got to remember that the other elements of mylm are not true #


# I just realised that if a 'new study' of size zero is added, the power won't be zero, as I am currently estimated the power of the new MA.
# With a non-new study, it should just be the same power as the current MA.
# Estimate power of current MA - and set that as the intercept (do this by shifting everything down to zero, then back up afterwards)


# Calculate power of current MA - hopefully its near 15-20% (sample size of 14 gave power of 18% (9-32%))
# Function to calculate power of current MA -> can't be done as with other function as nothing is changing between each simulation (before it was the new study that was the difference)
# Intercept will be zero ... you can't have a power of an analysis that has already been complete ... its at least not advised? 
# I'm assuming that users are trying to encourage the MA to have an impact of a certain type that it currently doesnt' have - i.e. the power of that impact is 0?
# The Bayesian MA may give a power result through simulation as that does change between iterations? Haven't developed that yet :(
# Leave it at assumes 0% power as 0 and get back to this the option of Bayesian once developed Bayesian side - shouldn't affect the model too much.


# Step Three #
#------------#
# Estimate the uncertainty around the spline model, and then find the range of sample sizes where 80% is plausible #

# Use bootstrapping to obtain confidence intervals #
# resampler of the data points that we have #
sampler_data <- results[results$Model=="Fixed-effects", c('SampleSize', 'Estimate')]
spline.resampler <- function(data, origin=TRUE) {   # data = population data to resample from; origin = whether to include origin in model
    n <- nrow(data)
    resample.rows <- sample(1:n,size=n,replace=TRUE)  # take a sample of the same size, but allow replacement
    if (origin == TRUE) {
      return(rbind(c(0,0), data[resample.rows,])) # ensure a 0,0 origin each time
    } else {
      return(data[resample.rows,])
    }
}
# estimator to create spline and obtain predicted values #
spline.estimator <- function(data, interp_range, origin=TRUE) {   # data = bootstrapped data; interp_range = range of values to interpolate; origin = whether to include origin in model
  quants <- quantile(data$SampleSize, probs=c(1/3, 2/3))
  if (quants[1]==min(data$SampleSize) & quants[2]==max(data$SampleSize)) {knots=NULL}
  else if (quants[1]==min(data$SampleSize)) {knots=quants[2]}
  else if (quants[2]==max(data$SampleSize)) {knots=quants[1]}
  else {knots = quants}   # knots need to be internal (i.e. not on boundary), which can happen if bootstrap sample has lots of replications
  mylm <- create_spline_model(data=data,
                              knots=knots,
                              xvar=data$SampleSize,
                              yvar=data$Estimate,
                              origin = origin)
  return(predict(mylm, newdata=data.frame(SampleSize = interp_range)))
}
# run the bootstraps and estimate confidence interval bands
spline.cis <- function(B,alpha=0.05, interp_range, og_data, sp_data, origin=TRUE) {   # B = number of bootstrap samples; alpha = alpha level for confidence bands; interp_range = range of values to interpolate; og_data = original dataset of results with origin; sp_data = reduced dataset; origin = whether to keep origin in model 
  spline.main <- spline.estimator(data=og_data,
                                  interp_range=interp_range,
                                  origin = origin)   # get line for main spline
  # Draw B bootstrap samples, fit the spline to each
  spline.boots <- replicate(B,spline.estimator(data=spline.resampler(sp_data, origin = origin),
                                               interp_range=interp_range,
                                               origin=origin))
  # Result has m (length of interp_range) rows and B columns
  cis.lower <- 2*spline.main - apply(spline.boots,1,quantile,probs=1-alpha/2)
  cis.upper <- 2*spline.main - apply(spline.boots,1,quantile,probs=alpha/2)
  return(list(main.curve=spline.main, lower.ci=cis.lower, upper.ci=cis.upper, x.range=interp_range))
}
first.cis <- spline.cis(B=100, alpha=0.05, interp_range=seq(0, 76700, length.out=200), 
                       og_data = results_origin[results_origin$Model=="Fixed-effects",],
                       sp_data = sampler_data)
### ISSUE THAT NEEDS FIXING ####
# For some bootstrap samples, the spline model function fails (matrix D in quadprog::solve.QP stops being positive definite) #

plot(results_origin$SampleSize[results_origin$Model=="Fixed-effects"], results_origin$Estimate[results_origin$Model=="Fixed-effects"], 
     xlab = "Sample Size", ylab = "Power", xlim=c(0,76700), ylim=c(0, 100))
lines(c(0,76700), c(80,80), lty='dashed')
lines(first.cis$x.range,first.cis$main.curve,col = "red")
lines(first.cis$x.range,first.cis$lower.ci,col = "purple")
lines(first.cis$x.range,first.cis$upper.ci,col = "purple")


# Go Through Algorithm #
#----------------------#

# SECOND ROUND with nit=100 #
# Find range of plausible values from the spline with confidence intervals #
new_min <- make_even(min(first.cis$x.range[round(first.cis$upper.ci)>=80]))   # gives point that upper CI crosses power of 80% (8,104)
new_max <- make_even(min(first.cis$x.range[round(first.cis$lower.ci)>=80]))   # gives point that lower CI crosses power of 80% (8,924)
#new_max <- max(ballpark_results$SampleSize)   # if the lower CI never crossed 80%
## GOT TO CONSIDER WIDE TAILS (that might cross the line multiple times ##

# If the lower CI never crossed 80% suggest re-running ballpark range algorithm #
#ballpark_results <- get_ballpark_range(n_range=10, nit=50, first=FALSE, sample.range=seq(from=new_min, to=new_max, length.out=10))

# If there is a band for 80%, suggest just running power functions on that range (i.e. no ballpark range algorithm)
results <- metapow_multiple(SampleSizes=make_even(seq(from=new_min, to=new_max, length.out=10)), NMA=freqMA, data=wide_data, nit=100, inference='pvalue', pow=0.35, measure=outcome)

# fit spline, bootstrap, and plot (don't need to include zero any more #
sampler_data <- results[results$Model=="Fixed-effects", c('SampleSize', 'Estimate')]
second.cis <- spline.cis(B=25, alpha=0.05, interp_range=seq(5700, 12400, length.out=200), 
                      og_data = results[results$Model=="Fixed-effects",],
                      sp_data = sampler_data,
                      origin = FALSE)
plot(results[results$Model=="Fixed-effects",]$SampleSize, results[results$Model=="Fixed-effects",]$Estimate,
     xlab = "Sample Size", ylab = "Power",
     xlim = c(5700, 12400), ylim = c(0, 100))
lines(c(5700,12400), c(80,80), lty='dashed')
lines(second.cis$x.range,second.cis$main.curve,col = "red")
lines(second.cis$x.range,second.cis$lower.ci,col = "purple")
lines(second.cis$x.range,second.cis$upper.ci,col = "purple")


# THIRD ROUND with nit=250 #
# Find range of plausible values from the spline with confidence intervals #
new_min <- make_even(min(second.cis$x.range[round(second.cis$upper.ci)>=80 & second.cis$x.range>8000]))   # bodge for tail
#new_max <- make_even(min(first.cis$x.range[round(first.cis$lower.ci)>=80]))
new_max <- max(results$SampleSize)   # if the lower CI never crossed 80%

# As the lower CI never crossed 80% suggest re-running ballpark range algorithm #
results <- get_ballpark_range(n_range=10, nit=250, first=FALSE, sample.range=seq(from=new_min, to=new_max, length.out=10))

# fit spline, bootstrap, and plot (don't need to include zero any more #
sampler_data <- results[results$Model=="Fixed-effects", c('SampleSize', 'Estimate')]
third.cis <- spline.cis(B=20, alpha=0.05, interp_range=seq(2900, 197400, length.out=200), 
                         og_data = results[results$Model=="Fixed-effects",],
                         sp_data = sampler_data,
                         origin = FALSE)
plot(results[results$Model=="Fixed-effects",]$SampleSize, results[results$Model=="Fixed-effects",]$Estimate,
     xlab = "Sample Size", ylab = "Power",
     xlim = c(2900, 197400), ylim = c(0, 100))
lines(c(2900,197400), c(80,80), lty='dashed')
lines(third.cis$x.range,third.cis$main.curve,col = "red")
lines(third.cis$x.range,third.cis$lower.ci,col = "purple")
lines(third.cis$x.range,third.cis$upper.ci,col = "purple")


# FOURTH ROUND with nit=500 #
# Find range of plausible values from the spline with confidence intervals #
new_min <- make_even(min(third.cis$x.range[round(third.cis$upper.ci)>=80])) 
new_max <- make_even(min(third.cis$x.range[round(third.cis$lower.ci)>=80]))
#new_max <- max(ballpark_results$SampleSize)   # if the lower CI never crossed 80%

# As the lower CI never crossed 80% suggest re-running ballpark range algorithm #
results <- metapow_multiple(SampleSizes=make_even(seq(from=new_min, to=new_max, length.out=10)), NMA=freqMA, data=wide_data, nit=500, inference='pvalue', pow=0.35, measure=outcome)

# fit spline, bootstrap, and plot (don't need to include zero any more #
sampler_data <- results[results$Model=="Fixed-effects", c('SampleSize', 'Estimate')]
fourth.cis <- spline.cis(B=20, alpha=0.05, interp_range=seq(14600, 27400, length.out=200), 
                        og_data = results[results$Model=="Fixed-effects",],
                        sp_data = sampler_data,
                        origin = FALSE)
plot(results[results$Model=="Fixed-effects",]$SampleSize, results[results$Model=="Fixed-effects",]$Estimate,
     xlab = "Sample Size", ylab = "Power",
     xlim = c(14600, 27400), ylim = c(0, 100))
lines(c(14600,27400), c(80,80), lty='dashed')
lines(fourth.cis$x.range,fourth.cis$main.curve,col = "red")
lines(fourth.cis$x.range,fourth.cis$lower.ci,col = "purple")
lines(fourth.cis$x.range,fourth.cis$upper.ci,col = "purple")

# I feel like if I go another round, it'll say do ballpark range function, which'll just go on forever...
# Whereas the plot shows pretty good estimations.
make_even(min(fourth.cis$x.range[round(fourth.cis$main.curve)>=80]))
make_even(min(third.cis$x.range[round(third.cis$upper.ci)>=80])) 
make_even(min(third.cis$x.range[round(third.cis$lower.ci)>=80]))

# Maybe for final round, just take estimate from spline, and check it with 1,000...
est <- make_even(median(fourth.cis$x.range[round(fourth.cis$main.curve)==80]))  # (20,678)
est_power <- metapow(NMA=freqMA, data=wide_data, n=est, nit=1000, inference='pvalue', pow=0.35, measure=outcome)  # gave 84% ... where do we draw the line...
# For a 1,000 iterations, you can have between 795 and 805 'correct' simulations to give a power of 80%... 

# Consider different sample sizes, i.e. sample sizes of x significant figures...
# Or consider Janion's idea to get more consistent results...





