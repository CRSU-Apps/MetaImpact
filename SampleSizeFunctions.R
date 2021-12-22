# Sample size calculator functions #
#----------------------------------#

# Functions have not yet been tested for optimal run times

# Frequentist #
#-------------#
# Follow same set-up as Stata where its made up of three functions
library(metafor)
library(ggplot2)
library(tidyr)

# Test datasets #
# Binary #
data <- data.frame(Study=c("Herne","Hoaglund","Kaiser","Lexomboon","McKerrow","Taylor"),
                   Year=c(1980, 1950, 1996, 1971, 1961, 1977),
                   R.1=c(7,39,97,8,5,12), N.1=c(7+39,39+115,97+49,8+166,5+10,12+117), T.1=rep("Treatment",6),
                   R.2=c(10,51,94,4,8,3), N.2=c(10+12,51+104,94+48,4+83,8+10,3+56), T.2=rep("Control",6))
# Continuous test data
data <- data.frame(Study=c("DRCRnet","Ekinci","Nepomuceno","Wiley"),
                   Year=c(2015, 2014, 2013, 2016),
                   Mean.1=c(-0.194, -0.237, -0.23, -0.106), SD.1=c(0.202, 0.16, 0.113, 0.169), N.1=c(206, 50, 32, 62), T.1=rep("Treatment",4),
                   Mean.2=c(-0.194, -0.211, -0.29, -0.132), SD.2=c(0.202, 0.16, 0.212, 0.173), N.2=c(206, 50, 28, 62), T.2=rep("Control",4))


# Conduct base MA (will be within app, so will be an input to stop uneccesary recalculations)
MAdata <- escalc(measure="OR", ai=R.1, bi=N.1-R.1, ci=R.2, di=N.2-R.2, data=data)
MAdata <- escalc(measure="SMD", m1i=Mean.1, m2i=Mean.2, sd1i=SD.1, sd2i=SD.2, n1i=N.1, n2i=N.2, data=data)
MA.Fixed <- rma(yi, vi, slab=Study, data=MAdata, method="FE", measure="SMD") #fixed effects#
MA.Random <- rma(yi, vi, slab=Study, data=MAdata, method="DL", measure="SMD") #random effects #
forest(MA.Random)
forest(MA.Random, atransf=exp)
summary(MA.Fixed, atransf=exp)
MA <- list(MA.Fixed=MA.Fixed, MA.Random=MA.Random)

# Function for creating new trial
metasim <- function(es, tausq, var, model, n, data, measure) {  # es - mean estimate from MA; tausq - estimated tau-squared from MA; var = estimated variance of estimate from MA; model - fixed or random; n - total sample size; data - original MA data; measure - type of outcome (or/rr/rd/md);
  # calculate control group stats 
  if (measure=='OR' | measure=='RR' | measure=='RD') {
    for (i in 1:nrow(data)) {
      if ((measure=='OR' | measure=='RR') & data$R.2[i]==0) {  # zero adjustment if needed
        data$R.2[i] <- data$R.2[i]+0.5
        data$N.2[i] <- data$N.2[i]+1
      }
    }
    prob.c = mean(data$R.2/data$N.2) 
  } else {
    mean.c = mean(data$Mean.2) #average mean outcome
    stdev.c = mean(data$SD.2) #standard deviation
  }
  # establish standard error for predictive distribution
  if (model=='random') {
    std_err <- sqrt(tausq + var)
  } else {
    std_err <- sqrt(var)
  }
  # draw new effect measure for new trial from predictive distribution of current MA (in terms of outcome)
  if (measure=='OR' | measure=='RR') {  # for OR/RR, the MA is conducted in log terms
    new.effect <- exp(rnorm(n=1, mean=es, sd=std_err))
  } else {
    new.effect <- rnorm(n=1, mean=es, sd=std_err)
  }
  # calculate number of events/mean/sd in the control group
  if (measure=='OR' | measure=='RR' | measure=='RD') {
    new.events.c <- rbinom(n=1, size=n/2, prob=prob.c)
  } else {
    sample <- rnorm(n=n/2, mean=mean.c, sd=stdev.c)
    new.mean.c <- mean(sample)
    new.stdev.c <-sd(sample)
  }
  # calculate number of events/mean/sd in the treatment group
  if (measure=='OR' | measure=='RR' | measure=='RD') {
    if (measure=='OR') {
      new.prob.t <- (new.effect*prob.c/(1-prob.c))/(1+(new.effect*prob.c/(1-prob.c)))
    } else if (measure=='RR') {
      new.prob.t <- new.effect*prob.c
    } else if (measure=='RD') {
      new.prob.t <- new.effect+prob.c  # risk difference = prob.t - prob.c
      if (new.prob.t < 0) { new.prob.t <- 0}  # the new RD estimate may be so negative that it creates a negative probability of event in treatment group, which needs to be capped
    } 
    new.events.t <- rbinom(n=1, size=n/2, prob=new.prob.t)
    new <- list(new.events.t=new.events.t, new.events.c=new.events.c, new.effect=new.effect)
  } else {
    if (measure=='MD') {
      sample <- rnorm(n=n/2, mean=mean.c+new.effect, sd=stdev.c)  #assume standard deviation is the same in both groups
    } else {
      sample <- rnorm(n=n/2, mean=(new.effect*stdev.c)+mean.c, sd=stdev.c) # standardised mean difference
    }
    new.mean.t <- mean(sample)
    new.stdev.t <- sd(sample)
    new <- list(new.mean.t=new.mean.t, new.stdev.t=new.stdev.t, new.mean.c=new.mean.c, new.stdev.c=new.stdev.c)
  }
  return(new)
}


# function to conduct bulk of power function below
rerunMA <- function(new, data, model, n, measure) {   # new - newly simulated trial using metasim; data - original dataset; model - fixed or random; n - total sample size; measure - type of outcome (or/rr/rd);
  # put together dataframe for new trial
  if (measure=='OR' | measure=='RR' | measure=='RD') {
    newtrial <- data.frame(Study="New", Year=2021, T.1="Treatment", T.2="Control", R.1=new$new.events.t, R.2=new$new.events.c, N.1=n/2, N.2=n/2)
  } else {
    newtrial <- data.frame(Study="New", Year=2021, T.1="Treatment", T.2="Control", Mean.1=new$new.mean.t, Mean.2=new$new.mean.c, 
                           SD.1=new$new.stdev.t, SD.2=new$new.stdev.c, N.1=n/2, N.2=n/2)
  }
  newdata <- rbind(data, newtrial)
  # re-meta-analyse
  if (measure=='OR' | measure=='RR' | measure=='RD') {
    if ((measure=='OR' | measure=='RR') & data$R.2[nrow(data)]==0) { # Add a continuity correction for new trial (rest of data would have been done in metasim command)
        data$R.2[nrow(data)] <- data$R.2[nrow(data)]+0.5
        data$N.2[nrow(data)] <- data$N.2[nrow(data)]+1
    }
    newMAdata <- escalc(measure=measure, ai=R.1, bi=N.1-R.1, ci=R.2, di=N.2-R.2, data=newdata)
  } else {
    newMAdata <- escalc(measure=measure, m1i=Mean.1, m2i=Mean.2, sd1i=SD.1, sd2i=SD.2, n1i=N.1, n2i=N.2, data=newdata)
  }
  if (model=='fixed') {newMA <- rma(yi, vi, slab=Study, data=newMAdata, method="FE", measure=measure)} #fixed effects#
  if (model=='random') {newMA <- rma(yi, vi, slab=Study, data=newMAdata, method="DL", measure=measure)} #random effects #
  return(newMA)
}
# function for calculating power
metapow <- function(NMA, data, n, nit, inference, pow, measure, recalc=FALSE, plot_ext=NA) {  # NMA - an NMA object from inbuilt function FreqMA; data - original data; n - total sample size; nit - number of iterations; inference - type of stat to calculate power; pow - power cut-off; measure - type of outcome (or/rr/rd); recalc - re-calculate power based on difference inference; plot - whether it is being used within the metapowplot function
  # create empty list elements
  power <- data.frame(Fixed=NA, Random=NA)
  CI_lower <- data.frame(Fixed=NA, Random=NA)
  CI_upper <- data.frame(Fixed=NA, Random=NA)
  sim.inference <- data.frame(Fixed=rep(x=NA, times=nit), Random=rep(x=NA, times=nit))
  if (recalc=='FALSE') {
  sims <- data.frame(Fixed.p=rep(x=NA, times=nit), Fixed.lci=rep(x=NA, times=nit), Fixed.uci=rep(x=NA, times=nit), 
                     Random.p=rep(x=NA, times=nit), Random.lci=rep(x=NA, times=nit), Random.uci=rep(x=NA, times=nit))
  # fixed-effects
  print("Running fixed-effects iterations:")
  progress_bar = txtProgressBar(min=0, max=nit, style = 1, char="=")
  for (i in 1:nit) {
    # obtain new data, add to existing data, and re-run MA
    new <- metasim(es=NMA$MA.Fixed$beta, tausq=NMA$MA.Fixed$tau2, var=(NMA$MA.Fixed$se)^2, model='fixed', n=n, data=data, measure=measure)
    newMA <- rerunMA(new=new, data=data, model='fixed', n=n, measure=measure)
    # Collect inference information
    sims$Fixed.p[i] <- newMA$pval
    if (measure=='OR' | measure=='RR') {
      sims$Fixed.lci[i] <- exp(newMA$ci.lb)
      sims$Fixed.uci[i] <- exp(newMA$ci.ub)
    } else {
      sims$Fixed.lci[i] <- newMA$ci.lb
      sims$Fixed.uci[i] <- newMA$ci.ub
    }
    setTxtProgressBar(progress_bar, value = i)
  }
  close(progress_bar)
  # random-effects
  print("Running random-effects iterations:")
  progress_bar = txtProgressBar(min=0, max=nit, style = 1, char="=")
    for (i in 1:nit) {
      # obtain new data, add to existing data, and re-run MA
      new <- metasim(es=NMA$MA.Random$beta, tausq=NMA$MA.Random$tau2, var=(NMA$MA.Random$se)^2, model='random', n=n, data=data, measure=measure)
      newMA <- rerunMA(new=new, data=data, model='random', n=n, measure=measure)
      # Collect inference information
      sims$Random.p[i] <- newMA$pval
      if (measure=='OR' | measure=='RR') {
        sims$Random.lci[i] <- exp(newMA$ci.lb)
        sims$Random.uci[i] <- exp(newMA$ci.ub)
      } else {
        sims$Random.lci[i] <- newMA$ci.lb
        sims$Random.uci[i] <- newMA$ci.ub
      }
      setTxtProgressBar(progress_bar, value = i)
    }
  close(progress_bar)
  if (is.na(plot_ext)==TRUE) {
    write.table(sims, file='sims.txt', sep = "\t", row.names=FALSE)  # save this data if model option is changed
  } else {
    write.table(sims, file=paste('sims',plot_ext,'.txt', sep=""), sep = "\t", row.names=FALSE)  # extra option needed for metapowplot
  }
  }
  if (is.na(plot_ext)==TRUE) {
    sims <- read.table('sims.txt', sep = "\t", header = TRUE) # read in simulated data
  } else {
    sims <- read.table(paste('sims',plot_ext,'.txt', sep=""), sep="\t", header=TRUE)
  }
  # Calculate power with 95% CI = proportion of 'TRUE' - true statements depend on inference
  for (i in 1:nit) {
    if (inference=='pvalue') {
      sim.inference$Fixed[i] <- sims$Fixed.p[i] < pow
      sim.inference$Random[i] <- sims$Random.p[i] < pow
    } else if (inference=='ciwidth') {
      sim.inference$Fixed[i] <- sims$Fixed.uci[i]-sims$Fixed.lci[i] < pow
      sim.inference$Random[i] <- sims$Random.uci[i]-sims$Random.lci[i] < pow
    } else if (inference=='lci') {
      sim.inference$Fixed[i] <- sims$Fixed.lci[i] > pow
      sim.inference$Random[i] <- sims$Random.lci[i] > pow
    } else if (inference=='uci') {
      sim.inference$Fixed[i] <- sims$Fixed.uci[i] < pow
      sim.inference$Random[i] <- sims$Random.uci[i] < pow
    }
  }
  power_results <- prop.test(sum(sim.inference$Fixed),nit) #sum(sims) counts number TRUE
  power$Fixed <- power_results$estimate
  CI_lower$Fixed <- power_results$conf.int[1]
  CI_upper$Fixed <- power_results$conf.int[2]
  power_results <- prop.test(sum(sim.inference$Random),nit) #sum(sims) counts number TRUE
  power$Random <- power_results$estimate
  CI_lower$Random <- power_results$conf.int[1]
  CI_upper$Random <- power_results$conf.int[2]
  return(list(simdata=sims, power=power, CI_lower=CI_lower, CI_upper=CI_upper))
}

test <- metapow(NMA=MA, data=data, n=500, nit=100, inference='pvalue', pow=0.05, measure='SMD')
test <- metapow(nit=100, inference='ciwidth', pow=0.25, recalc='TRUE')


# function for plotting the power curve
metapowplot <- function(SampleSizes, NMA, data, nit, inference, pow, measure, ModelOpt, recalc=FALSE, regraph=FALSE) { # SampleSizes - a vector of (total) sample sizes; NMA - an NMA object from inbuilt function FreqMA; data - original dataset; nit - number of iterations; inference - type of stat to calculate power; pow - power cut-off; measure - type of outcome (or/rr/rd); ModelOpt - either show fixed or random results, or both; recalc - re-calculate power based on difference inference; regraph - change plot settings without re-running analysis
  if (regraph==FALSE) {   # obtain power data if its the first run
  PowerData <- data.frame(SampleSize = rep(SampleSizes,2), Model = c(rep("Fixed-effects",length(SampleSizes)),rep("Random-effects",length(SampleSizes))), Estimate = NA, CI_lower = NA, CI_upper = NA)
  for (i in 1:length(SampleSizes)) {
    results <- metapow(NMA=NMA, data=data, n=SampleSizes[i], nit=nit, inference=inference, pow=pow, measure=measure, recalc=recalc, plot_ext=i)
    PowerData$Estimate[i] <- results$power$Fixed*100
    PowerData$Estimate[i+length(SampleSizes)] <- results$power$Random*100
    PowerData$CI_lower[i] <- results$CI_lower$Fixed*100
    PowerData$CI_lower[i+length(SampleSizes)] <- results$CI_lower$Random*100
    PowerData$CI_upper[i] <- results$CI_upper$Fixed*100
    PowerData$CI_upper[i+length(SampleSizes)] <- results$CI_upper$Random*100
    print(paste("Simulation",i,"of", length(SampleSizes), "complete"))
  }
  write.table(PowerData, file='PowerData.txt', sep = "\t", row.names=FALSE)  # save this data if model option is changed
  }
  PowerData <- read.table('PowerData.txt', sep = "\t", header = TRUE) # read in power data
  if (ModelOpt == 'fixed') { # only fixed
    g <- ggplot(PowerData[PowerData$Model=='Fixed-effects',], aes(x=SampleSize, y=Estimate)) +
      geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper),alpha=0.3, colour='gray70', linetype='blank') +
      labs(x = "Total Sample Size", y = "Power (%)", title = "Power curves (fixed-effects model)", subtitle = "with 95% confidence intervals") 
  }
  if (ModelOpt == 'random') { # only random
    g <- ggplot(PowerData[PowerData$Model=='Random-effects',], aes(x=SampleSize, y=Estimate)) +
      geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper),alpha=0.3, colour='gray70', linetype='blank') +
      labs(x = "Total Sample Size", y = "Power (%)", title = "Power curves (random-effects model)", subtitle = "with 95% confidence intervals") 
  }
  if (ModelOpt == 'both') { #present both models
    g <- ggplot(PowerData, aes(x=SampleSize, y=Estimate, group=Model, colour=Model, fill=Model)) +
      geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper),alpha=0.3, linetype='blank') +
      scale_color_manual(values=c('black','navy')) +
      scale_fill_manual(values=c('gray','skyblue')) +
      labs(x = "Total Sample Size", y = "Power (%)", title = "Power curves", subtitle = "with 95% confidence intervals")
  }
  g <- g +
    geom_line(size=1) + geom_point(shape=21) +
    theme_classic() +
    scale_x_continuous(limits=c(0,max(SampleSizes)+5), expand=c(0,0)) +
    scale_y_continuous(limits=c(0,NA), expand=c(0,0)) +
    theme(legend.position="bottom", legend.title=element_blank(), legend.background = element_rect(linetype="solid",colour ="black"), 
          panel.grid.major.y = element_line(colour="gray80", linetype='dashed', size=0.5), plot.margin = unit(c(10,15,10,10), "points"), 
          plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  return(list(plot=g, data=PowerData))
}   

test<-metapowplot(SampleSizes=c(1000, 2000, 3000, 4000, 5000, 6000), NMA=MA, data=data, nit=50, inference='pvalue', pow=0.05, measure='SMD', ModelOpt='both')
test$plot
test <- metapowplot(SampleSizes=c(1000,2000,3000,4000,5000,6000), nit=50, inference='ciwidth', pow=0.2, ModelOpt='both', recalc=TRUE) #testing re-calculation option
test$plot
test <- metapowplot(SampleSizes=c(1000,2000,3000,4000,5000,6000), regraph=TRUE, ModelOpt='random') #testing regraph option
test$plot

# need to make base-app have separate part for conducting pairwise MA - i.e. change the NMA to be an additional feature for now (want options to go 'above', not 'below')