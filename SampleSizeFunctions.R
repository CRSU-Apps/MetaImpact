# Sample size calculator functions #
#----------------------------------#

# Functions have not yet been tested for optimal run times

# Frequentist #
#-------------#
# Follow same set-up as Stata where its made up of three functions
library(netmeta)
library(ggplot2)
library(tidyr)

# Test data from Stata documents to check functioning correctly#
data <- data.frame(Study=c("Herne","Hoaglund","Kaiser","Lexomboon","McKerrow","Taylor"),
                   Year=c(1980, 1950, 1996, 1971, 1961, 1977),
                   R.1=c(7,39,97,8,5,12), N.1=c(7+39,39+115,97+49,8+166,5+10,12+117), T.1=rep("Treatment",6),
                   R.2=c(10,51,94,4,8,3), N.2=c(10+12,51+104,94+48,4+83,8+10,3+56), T.2=rep("Control",6))

# conduct base MA from function created 
source("MAFunctions.R",local = TRUE) 
MA <- FreqMA(data=data, outcome="OR",CONBI="binary",model="both",ref="Control") # will make it the default that it calculates the fixed and random effects, but switch in UI will be which to present (rather than calculate)
summary(MA$MAObject) # matches Stata example

# Calculate the average mean and SD in the control group #
prob.c = mean(data$R.2/data$N.2) # average proportion
#stdevC = sd(data$P.2) #not needed for binary

# function for creating new trial
metasim <- function(es, tausq, var, model, n, prob.c) {  # es - mean estimate; tausq - estimates tau-squared; var = estimated within-study variance; model - fixed or random; n - total sample size; prob.c - probability of event in control arm
  # establish standard error for predictive distribution
  if (model=='random') {
    std_err <- sqrt(tausq + var)
  } else {
    std_err <- sqrt(var)
  }
  # draw new OR for new trial from predictive distribution
  newOR <- exp(rnorm(n=1, mean=es, sd=std_err))
  # calculate number of events in the control group
  new.events.c <- rbinom(n=1, size=n/2, prob=prob.c)
  # calculate number of events in the treatment group
  new.prob.t <- (newOR*prob.c/(1-prob.c))/(1+(newOR*prob.c/(1-prob.c))) #calculation specific to OR
  new.events.t <- rbinom(n=1, size=n/2, prob=new.prob.t)
  list(new.events.t=new.events.t, new.events.c=new.events.c)
}

# function to conduct bulk of power function below
rerunMA <- function(new, model, NMA, n) {   # new - newly simulated trial using metasim; model - fixed or random; NMA - an NMA object from inbuilt function freqMA; n - total sample size
  # Add a continuity correction to simulated data set if any values are 0	
  if (new$new.events.t==0) {new$new.events.t=new$new.events.t+0.5}
  if (new$new.events.c==0) {new$new.events.c=new$new.events.c+0.5}
  # put together dataframe for new trial
  newtrial <- data.frame(studlab="New", treat1="Treatment", treat2="Control",
                         TE=log((new$new.events.t*((n/2)-new$new.events.c))/(new$new.events.c*((n/2)-new$new.events.t))), 
                         seTE=sqrt(1/new$new.events.t + 1/(n-new$new.events.t) + 1/new$new.events.c + 1/(n-new$new.events.c)))
  newdata <- rbind(NMA$MAData[,1:5],newtrial) # using pairwise data from FreqMA function
  # re-run meta-analysis
  newMA <- netmeta(TE, seTE, treat1, treat2, studlab, data=newdata,
                   sm='OR', level=0.95, level.comb=0.95,
                   comb.random=(model=='random'), comb.fixed=(model=='fixed'), reference.group="Control",
                   all.treatments=NULL, seq=NULL, tau.preset=NULL,
                   tol.multiarm=0.05, tol.multiarm.se=0.2, warn=FALSE)
  return(newMA)
  }
# function for calculating power
metapow <- function(NMA, n, nit, p) {  # NMA - an NMA object from inbuilt function FreqMA; n - total sample size; nit - number of iterations; p - p-value cut off
  # create empty list elements
  power <- data.frame(Fixed=NA, Random=NA)
  CI_lower <- data.frame(Fixed=NA, Random=NA)
  CI_upper <- data.frame(Fixed=NA, Random=NA)
  sims <- data.frame(Fixed=rep(x=NA, times=nit), Random=rep(x=NA, times=nit))
  # fixed-effects
  for (i in 1:nit) {
    # obtain new data, add to existing data, and re-run MA
      new <- metasim(es=NMA$MAObject$TE.direct.fixed[2,1], tausq=NMA$MAObject$tau2, var=NMA$MAObject$seTE.nma.fixed[1], model='fixed', n=n, prob.c=prob.c)
      newMA <- rerunMA(new=new, model='fixed', NMA=NMA, n=n)
    # Ascertain whether new MA had a significant result
    sims$Fixed[i] <- newMA$pval.nma.fixed[1]<p
  }
    # Calculate power with 95% CI = proportion of 'TRUE'
  power_results <- prop.test(sum(sims$Fixed),nit) #sum(sims) counts number TRUE
  power$Fixed <- power_results$estimate
  CI_lower$Fixed <- power_results$conf.int[1]
  CI_upper$Fixed <- power_results$conf.int[2]
  # random-effects
    for (i in 1:nit) {
      # obtain new data, add to existing data, and re-run MA
      new <- metasim(es=NMA$MAObject$TE.direct.random[2,1], tausq=NMA$MAObject$tau2, var=NMA$MAObject$seTE.nma.random[1], model='random', n=n, prob.c=prob.c)
      newMA <- rerunMA(new=new, model='random', NMA=NMA, n=n)
      # Ascertain whether new MA had a significant result
      sims$Random[i] <- newMA$pval.nma.random[1]<p
    }
    # Calculate power with 95% CI = proportion of 'TRUE'
    power_results <- prop.test(sum(sims$Random),nit) #sum(sims) counts number TRUE
    power$Random <- power_results$estimate
    CI_lower$Random <- power_results$conf.int[1]
    CI_upper$Random <- power_results$conf.int[2]
  return(list(simdata=sims, power=power, CI_lower=CI_lower, CI_upper=CI_upper))
}


test<- metapow(NMA=MA, n=200, nit=50, p=0.05)


# function for plotting the power curve
metapowplot <- function(SampleSizes, NMA, nit, p, ModelOpt) { # SampleSizes - a vector of (total) sample sizes; NMA - an NMA object from inbuilt function FreqMA; nit - number of iterations; p - p-value cut off; ModelOpt - either show fixed or random results, or both
  PowerData <- data.frame(SampleSize = SampleSizes, Estimate.Fixed = NA, Estimate.Random = NA, CI_lower.Fixed = NA, CI_lower.Random = NA, CI_upper.Fixed = NA, CI_upper.Random = NA)
  for (i in 1:length(SampleSizes)) {
    results <- metapow(NMA=NMA, n=SampleSizes[i], nit=nit, p=p)
    PowerData$Estimate.Fixed[i] <- results$power$Fixed*100
    PowerData$Estimate.Random[i] <- results$power$Random*100
    PowerData$CI_lower.Fixed[i] <- results$CI_lower$Fixed*100
    PowerData$CI_lower.Random[i] <- results$CI_lower$Random*100
    PowerData$CI_upper.Fixed[i] <- results$CI_upper$Fixed*100
    PowerData$CI_upper.Random[i] <- results$CI_upper$Random*100
    print(paste("Simulation",i,"of", length(SampleSizes), "complete"))   # try and get it to save this data if model option is changed
  }
  PowerData <- gather(PowerData, key = Measure, value = Value, 
               c("Estimate.Fixed","Estimate.Random","CI_lower.Fixed","CI_lower.Random","CI_upper.Fixed","CI_upper.Random")) #rearrange to three columns
  if (ModelOpt == 'fixed') { # only fixed
    g <- ggplot(PowerData[PowerData$Measure=='Estimate.Fixed' | PowerData$Measure=='CI_lower.Fixed' | PowerData$Measure=='CI_upper.Fixed',], 
                aes(x=SampleSize, y=Value, group=Measure, colour=Measure)) +
      scale_color_manual(values=c('gray','gray','black'), breaks=c("Estimate.Fixed","CI_lower.Fixed"), labels=c("Power estimate","95% confidence interval"))
  }
  if (ModelOpt == 'random') { # only random
    g <- ggplot(PowerData[PowerData$Measure=='Estimate.Random' | PowerData$Measure=='CI_lower.Random' | PowerData$Measure=='CI_upper.Random',], 
                aes(x=SampleSize, y=Value, group=Measure, colour=Measure)) +
      scale_color_manual(values=c('gray','gray','black'), breaks=c("Estimate.Random","CI_lower.Random"), labels=c("Power estimate","95% confidence interval"))
  }
  if (ModelOpt == 'both') { #present both models
    g <- ggplot(PowerData, aes(x=SampleSize, y=Value, group=Measure, colour=Measure)) +
      scale_color_manual(values=c('gray','skyblue','gray','skyblue','black','navy'), breaks=c("Estimate.Fixed","CI_lower.Fixed","Estimate.Random","CI_lower.Random"), labels=c("Fixed-effects power estimate","95% confidence interval","Random-effects power estimate", "95% confidence interval"))
  }
  g +
    geom_line() + geom_point(shape=21) +
    theme_classic() +
    scale_x_continuous(limits=c(0,max(SampleSizes)+5), expand=c(0,0)) +
    scale_y_continuous(limits=c(0,NA), expand=c(0,0)) +
    labs(x = "Total Sample Size", y = "Power (%)", title = "Power curves", subtitle = "with 95% confidence intervals") +
    theme(legend.position="bottom", legend.title=element_blank(), legend.background = element_rect(linetype="solid",colour ="black"), 
          panel.grid.major.y = element_line(colour="gray80", linetype='dashed', size=0.5), plot.margin = unit(c(10,15,10,10), "points"), 
          plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
    guides(color=guide_legend(ncol=2))
}

metapowplot(SampleSizes=c(200,400,600,800,1000), NMA=MA, nit=25, p=0.05, ModelOpt='both')
