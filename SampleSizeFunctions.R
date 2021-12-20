# Sample size calculator functions #
#----------------------------------#

# Functions have not yet been tested for optimal run times

# Frequentist #
#-------------#
# Follow same set-up as Stata where its made up of three functions
library(metafor)
library(ggplot2)
library(tidyr)

# Stata results (n=500, nit=100, fixed) OR=53.00 (95% CI: 42.76, 63.06); RR=30.00 (95% CI: 21.24, 39.98) - my command gave 62%...; RD=6.00 (95% CI: 2.23, 12.60) - my command gave 81%.....
data <- data.frame(Study=c("Herne","Hoaglund","Kaiser","Lexomboon","McKerrow","Taylor"),
                   Year=c(1980, 1950, 1996, 1971, 1961, 1977),
                   R.1=c(7,39,97,8,5,12), N.1=c(7+39,39+115,97+49,8+166,5+10,12+117), T.1=rep("Treatment",6),
                   R.2=c(10,51,94,4,8,3), N.2=c(10+12,51+104,94+48,4+83,8+10,3+56), T.2=rep("Control",6))
# Continuous test data
#data <- data.frame(studlab=c("Barnes_2004","Engleman_2002","Ferguson_1997","Tan_2002"),
#                   treat1=rep("OA",4), treat2=rep("CPAP",4),
#                   TE=c(0, 4, -0.4, 0.9), seTE=c(0.566, 1.16, 0.94, 1.33), 
#                   n1=c(80, 48, 19, 21),  n2=c(80, 48, 19, 21))
# dataset from Stata example code: random power 98.70 (95% CI: 97.79, 99.31)
data <- data.frame(Study=c("Albaladejo","Andrade","Kell","Saper","Djavid","Bakhtirary","Kofotolis","Smeets"),
                   Mean.1=c(57.5,32.2,33,-23,46,-32,14,0), SD.1=c(24.3,28.3,5,21,17,14.7,4,21.32), N.1=c(100,34,9,15,20,30,28,52), T.1=rep("Treatment",8),
                   Mean.2=c(67.5,48.6,48,-4,60,-5,16,8.68), SD.2=c(24.3,19,7,18,16,11.7,4,20.9), N.2=c(109,36,9,15,19,30,30,50), T.2=rep("Control",8))
# Stata gave random power of 40.70 (95% CI: 37.64, 43.82), with total sample size 500
data <- data.frame(Study=c("Fleetham_1998","Hoekema_2006","Lam_2007"), 
                   Mean.1=c(9.3, 6.9, 9), SD.1=c(4.7, 5.5, 5.83), N.1=c(50, 49, 34), T.1=rep("Treatment",3),
                   Mean.2=c(9.5, 5.9, 7), SD.2=c(4.5, 4.8, 5.83), N.2=c(51, 50, 34), T.2=rep("Control", 3))

# conduct base MA from function created 
source("MAFunctions.R",local = TRUE) 
#MA <- FreqMA(data=data, outcome="OR",CONBI="binary",model="both",ref="Control") # will make it the default that it calculates the fixed and random effects, but switch in UI will be which to present (rather than calculate)
#summary(MA$MAObject) # matches Stata example
#forest(MA$MAObject)
MAdata <- escalc(measure="OR", ai=R.1, bi=N.1-R.1, ci=R.2, di=N.2-R.2, data=data)
MA <- rma(yi, vi, slab=Study, data=MAdata, method="FE", measure="OR") #fixed effects#
forest(MA, atransf=exp)
summary(MA)


# Calculate the average mean and SD in the control group #
prob.c = mean(data$R.2/data$N.2) # average proportion
mean.c = mean(data$Mean.2) #average mean outcome
stdev.c = mean(data$SD.2) #standard deviation
#mean.TE = mean(data$TE)
#mean.seTE = mean(data$seTE)

# function for creating new trial
metasim <- function(es, tausq, var, model, n, prob.c, mean.c, stdev.c, measure, outcome) {  # es - mean estimate; tausq - estimates tau-squared; var = estimated within-study variance; model - fixed or random; n - total sample size; prob.c/mean.c/stdev.c - probability of event/mean/sd in control arm; measure - type of outcome (or/rr/rd/md); outcome - binary or continuous;
  # establish standard error for predictive distribution
  if (model=='random') {
    std_err <- sqrt(tausq + var)
  } else {
    std_err <- sqrt(var)
  }
  # draw new effect measure for new trial from predictive distribution
  if (measure=='OR' | measure=='RR') {  # for OR/RR, the MA is conducted in log terms
    new.effect <- exp(rnorm(n=1, mean=es, sd=std_err))
  } else {
    new.effect <- rnorm(n=1, mean=es, sd=std_err)   # in contrast form, this is new.TE
  }
  # calculate number of events/mean/sd in the control group
  if (outcome=='binary') {new.events.c <- rbinom(n=1, size=n/2, prob=prob.c)}
  if (outcome=='continuous') {
    sample <- rnorm(n=n/2, mean=mean.c, sd=stdev.c)
    new.mean.c <- mean(sample)
    new.stdev.c <-sd(sample)   # in contrast form, this is new.seTE
  }
  # calculate number of events/mean/sd in the treatment group
  if (outcome=='binary') {
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
  }
  if (outcome=='continuous') {
    sample <- rnorm(n=n/2, mean=mean.c+new.effect, sd=stdev.c)  #assume standard deviation is the same in both groups
    new.mean.t <- mean(sample)
    new.stdev.t <- sd(sample)
    #if (contrastform==FALSE) {
      new <- list(new.mean.t=new.mean.t, new.stdev.t=new.stdev.t, new.mean.c=new.mean.c, new.stdev.c=new.stdev.c)
    #  }
    #if (contrastform==TRUE) {new <- list(new.TE=new.effect, new.seTE=new.stdev.c)}
  }
  return(new)
}

#unittest <- data.frame(Run=rep(NA,1000), new.events.t=rep(NA,1000), new.events.c=rep(NA,1000), new.effect=rep(NA,1000))
#for (i in 1:1000) {
#  test <- metasim(es=MA$MAObject$TE.direct.fixed[2,1], tausq=MA$MAObject$tau2, var=MA$MAObject$seTE.nma.fixed[1], model='random', n=300, prob.c=prob.c, measure='RD')
#  unittest$Run[i] <- i
#  unittest$new.events.t[i] <- test$new.events.t
#  unittest$new.events.c[i] <- test$new.events.c
#  unittest$new.effect[i] <- test$new.effect
  #if (unittest$new.events.t[i]==0) {unittest$new.events.t[i]=unittest$new.events.t[i]+0.5}
#}

#new <- metasim(es=MA$MAObject$TE.direct.fixed[2,1], tausq=MA$MAObject$tau2, var=MA$MAObject$seTE.nma.fixed[1], model='fixed', n=300, prob.c=prob.c, measure='RD')
new <- metasim(es=MA$MAObject$TE.direct.fixed[2,1], tausq=MA$MAObject$tau2, var=MA$MAObject$seTE.nma.fixed[1], model='fixed', n=300, mean.c=mean.c, stdev.c=stdev.c, measure='MD', outcome='continuous')
#new <- metasim(es=MA$MAObject$TE.direct.fixed[2,1], tausq=MA$MAObject$tau2, var=MA$MAObject$seTE.nma.fixed[1], model='fixed', n=300, mean.c=mean.TE, stdev.c=mean.seTE, measure='MD', outcome='continuous', contrastform=TRUE)

# function to conduct bulk of power function below
rerunMA <- function(new, model, NMA, n, measure, outcome) {   # new - newly simulated trial using metasim; model - fixed or random; NMA - an NMA object from inbuilt function freqMA; n - total sample size; measure - type of outcome (or/rr/rd); outcome - binary or continuous; 
  # Add a continuity correction to simulated data set if any values are 0	for outcome measures that are based on ratios
  if (measure=='OR' | measure=='RR') {
    if (new$new.events.t==0) {new$new.events.t=new$new.events.t+0.5} # double check whether I need to add 0.5 to denominator as well? I think the Stata code does (i.e. adds 0.5 to 0 events, and 1 to total n for that group (adding 0.5 to corresponding non-event))
    if (new$new.events.c==0) {new$new.events.c=new$new.events.c+0.5}
  }
  # put together dataframe for new trial
  if (outcome=='binary') {
    newtrial <- data.frame(Study="New", Year=2021, T.1="Treatment", T.2="Control", R.1=new$new.events.t, R.2=new$new.events.c, N.1=n/2, N.2=n/2)
    #newtrial <- pairwise(treat=c("Treatment","Control"), n=c(n/2, n/2), event=c(new$new.events.t, new$new.events.c), 
    #                     studlab=c("New","New"), sm=measure)
  }
  #if (measure=='OR') {
  #  newtrial <- data.frame(studlab="New", treat1="Treatment", treat2="Control",
  #                       TE=log((new$new.events.t*((n/2)-new$new.events.c))/(new$new.events.c*((n/2)-new$new.events.t))), 
  #                       seTE=sqrt(1/new$new.events.t + 1/((n/2)-new$new.events.t) + 1/new$new.events.c + 1/((n/2)-new$new.events.c)))
  #} else if (measure=='RR') {
  #  newtrial <- data.frame(studlab="New", treat1="Treatment", treat2="Control",
  #                         TE=log(new$new.events.t/new$new.events.c),
  #                         seTE=sqrt(1/new$new.events.t + 1/new$new.events.c - 4/n))
  #} else if (measure=='RD') {
  #  newtrial <- data.frame(studlab="New", treat1="Treatment", treat2="Control",
  #                         TE=((new$new.events.t/(n/2)) - (new$new.events.c/(n/2))),
  #                         seTE=sqrt(((new$new.events.t*((n/2)-new$new.events.t))/((n/2)^3)) + ((new$new.events.c*((n/2)-new$new.events.c))/((n/2)^3))))
  #} else if (measure=='MD') {
    #if (contrastform==FALSE) {
  if (outcome=='continuous') {
      newtrial <- pairwise(treat=c("Treatment","Control"), n=c(n/2, n/2), mean=c(new$new.mean.t, new$new.mean.c),
                           sd=c(new$new.stdev.t, new$new.stdev.c), studlab=c("New","New"), sm=measure)
  }
    #} else {
    #  newtrial <- data.frame(studlab="New", treat1="OA", treat2="CPAP", TE=new$new.TE, seTE=new$new.seTE)
    #}
  #}
  newdata <- rbind(data, newtrial)
  #newdata <- rbind(NMA$MAData[,1:5],newtrial[,1:5]) # using pairwise data from FreqMA function
  # re-run meta-analysis
    #newMA <- netmeta(TE, seTE, treat1, treat2, studlab, data=newdata,
    #               sm=measure, level=0.95, level.comb=0.95,
    #               comb.random=(model=='random'), comb.fixed=(model=='fixed'), reference.group="Control",
    #               all.treatments=NULL, seq=NULL, tau.preset=NULL,
    #               tol.multiarm=0.05, tol.multiarm.se=0.2, warn=FALSE)
    newMAdata <- escalc(measure="OR", ai=R.1, bi=N.1-R.1, ci=R.2, di=N.2-R.2, data=newdata)
    newMA <- rma(yi, vi, slab=Study, data=newMAdata, method="FE", measure=measure) #fixed effects#
  return(newMA)
  }
# function for calculating power
metapow <- function(NMA, n, nit, p, measure, outcome, model) {  # NMA - an NMA object from inbuilt function FreqMA; n - total sample size; nit - number of iterations; p - p-value cut offmeasure - type of outcome (or/rr/rd); outcome - binary or continuous;
  # create empty list elements
  power <- data.frame(Fixed=NA, Random=NA)
  CI_lower <- data.frame(Fixed=NA, Random=NA)
  CI_upper <- data.frame(Fixed=NA, Random=NA)
  sims <- data.frame(Fixed=rep(x=NA, times=nit), Random=rep(x=NA, times=nit))
  # fixed-effects
  if (model=='fixed') {
  print("Running fixed-effects iterations:")
  progress_bar = txtProgressBar(min=0, max=nit, style = 1, char="=")
  for (i in 1:nit) {
    # obtain new data, add to existing data, and re-run MA
    #if (outcome=='binary') {new <- metasim(es=NMA$MAObject$TE.direct.fixed[2,1], tausq=NMA$MAObject$tau2, var=NMA$MAObject$seTE.nma.fixed[1], model='fixed', n=n, prob.c=prob.c, measure=measure, outcome=outcome)}
    if (outcome=='binary') {new<- metasim(es=NMA$beta, tausq=NMA$tau2, var=(NMA$se)^2, model='fixed', n=n, prob.c=prob.c, measure=measure, outcome=outcome)}
    if (outcome=='continuous') {
      #if (contrastform==FALSE) {
        new <- metasim(es=MA$MAObject$TE.direct.fixed[2,1], tausq=MA$MAObject$tau2, var=MA$MAObject$seTE.nma.fixed[1], model='fixed', n=n, mean.c=mean.c, stdev.c=stdev.c, measure=measure, outcome=outcome)
      #  }
      #if (contrastform==TRUE) {new <- metasim(es=MA$MAObject$TE.direct.fixed[2,1], tausq=MA$MAObject$tau2, var=MA$MAObject$seTE.nma.fixed[1], model='fixed', n=n, mean.c=mean.TE, stdev.c=mean.seTE, measure=measure, outcome=outcome, contrastform=TRUE)}
    }
      newMA <- rerunMA(new=new, model='fixed', NMA=NMA, n=n, measure=measure, outcome=outcome)
    # Ascertain whether new MA had a significant result
    sims$Fixed[i] <- newMA$pval<p
    setTxtProgressBar(progress_bar, value = i)
  }
  close(progress_bar)
    # Calculate power with 95% CI = proportion of 'TRUE'
  power_results <- prop.test(sum(sims$Fixed),nit) #sum(sims) counts number TRUE
  power$Fixed <- power_results$estimate
  CI_lower$Fixed <- power_results$conf.int[1]
  CI_upper$Fixed <- power_results$conf.int[2]
  }
  # random-effects
  if (model=='random') {
  print("Running random-effects iterations:")
  progress_bar = txtProgressBar(min=0, max=nit, style = 1, char="=")
    for (i in 1:nit) {
      # obtain new data, add to existing data, and re-run MA
      if (outcome=='binary') {new <- metasim(es=NMA$MAObject$TE.direct.random[2,1], tausq=NMA$MAObject$tau2, var=NMA$MAObject$seTE.nma.random[1], model='random', n=n, prob.c=prob.c, measure=measure, outcome=outcome)}
      if (outcome=='continuous') {
        #if (contrastform==FALSE) {
          new <- metasim(es=MA$MAObject$TE.direct.random[2,1], tausq=MA$MAObject$tau2, var=MA$MAObject$seTE.nma.random[1], model='random', n=n, mean.c=mean.c, stdev.c=stdev.c, measure=measure, outcome=outcome)
        #  }
        #if (contrastform==TRUE) {new <- metasim(es=MA$MAObject$TE.direct.random[2,1], tausq=MA$MAObject$tau2, var=MA$MAObject$seTE.nma.random[1], model='random', n=n, mean.c=mean.TE, stdev.c=mean.seTE, measure=measure, outcome=outcome, contrastform=TRUE)}
      }
      newMA <- rerunMA(new=new, model='random', NMA=NMA, n=n, measure=measure, outcome=outcome)
      # Ascertain whether new MA had a significant result
      sims$Random[i] <- newMA$pval.nma.random[1]<p
      setTxtProgressBar(progress_bar, value = i)
    }
  close(progress_bar)
    # Calculate power with 95% CI = proportion of 'TRUE'
    power_results <- prop.test(sum(sims$Random),nit) #sum(sims) counts number TRUE
    power$Random <- power_results$estimate
    CI_lower$Random <- power_results$conf.int[1]
    CI_upper$Random <- power_results$conf.int[2]
  }
  return(list(simdata=sims, power=power, CI_lower=CI_lower, CI_upper=CI_upper))
}


test<- metapow(NMA=MA, n=200, nit=100, p=0.05, measure='OR', outcome='binary', model='fixed')
#test <- metapow(NMA=MA, n=500, nit=100, p=0.05, measure='MD', outcome='continuous')


# function for plotting the power curve
metapowplot <- function(SampleSizes, NMA, nit, p, measure, outcome, ModelOpt, regraph=FALSE) { # SampleSizes - a vector of (total) sample sizes; NMA - an NMA object from inbuilt function FreqMA; nit - number of iterations; p - p-value cut off; measure - type of outcome (or/rr/rd); outcome - binary or continuous; ModelOpt - either show fixed or random results, or both; regraph - change plot settings without re-running analysis
  if (regraph==FALSE) {   # obtain power data if its the first run
  PowerData <- data.frame(SampleSize = rep(SampleSizes,2), Model = c(rep("Fixed-effects",length(SampleSizes)),rep("Random-effects",length(SampleSizes))), Estimate = NA, CI_lower = NA, CI_upper = NA)
  for (i in 1:length(SampleSizes)) {
    results <- metapow(NMA=NMA, n=SampleSizes[i], nit=nit, p=p, measure=measure, outcome=outcome)
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
    g <- ggplot(PowerData[PowerData$Model=='Random-effects'], aes(x=SampleSize, y=Estimate)) +
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

test<-metapowplot(SampleSizes=c(100,200,300,400,500,600,700,800,900,1000), NMA=MA, nit=25, p=0.05, measure='MD', outcome='continuous', ModelOpt='both', regraph=FALSE)
test$plot
