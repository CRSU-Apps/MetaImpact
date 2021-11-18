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
MA <- FreqMA(data=data, outcome="OR",CONBI="binary",model="fixed",ref="Control")
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

# function for calculating power
metapow <- function(NMA, n, nit, p) {  # NMA - an NMA object from inbuilt function FreqMA; n - total sample size; nit - number of iterations; p - p-value cut off
  # create empty simulation data frame
  sims <- rep(x=NA, times=nit)
  for (i in 1:nit) {
    # obtain new data & add to existing data
    new <- metasim(es=NMA$MAObject$TE.direct.fixed[2,1], tausq=NMA$MAObject$tau2, var=NMA$MAObject$seTE.nma.fixed[1], model='fixed', n=n, prob.c=prob.c)
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
                     comb.random=FALSE, comb.fixed=TRUE, reference.group="Control",
                     all.treatments=NULL, seq=NULL, tau.preset=NULL,
                     tol.multiarm=0.05, tol.multiarm.se=0.2, warn=FALSE)
    # Ascertain whether new MA had a significant result
    sims[i] <- newMA$pval.nma.fixed[1]<p
  }
    # Calculate power with 95% CI = proportion of 'TRUE'
  power_results <- prop.test(sum(sims),nit) #sum(sims) counts number TRUE
  return(list(simdata=sims, power=power_results$estimate, CI_lower=power_results$conf.int[1], CI_upper=power_results$conf.int[2]))
}


#test<- metapow(NMA=MA, n=50, nit=25, p=0.05)


# function for plotting the power curve
metapowplot <- function(SampleSizes, NMA, nit, p) { # SampleSizes - a vector of (total) sample sizes; NMA - an NMA object from inbuilt function FreqMA; nit - number of iterations; p - p-value cut off
  PowerData <- data.frame(SampleSize = SampleSizes, Estimate = NA, CI_lower = NA, CI_upper = NA)
  for (i in 1:length(SampleSizes)) {
    results <- metapow(NMA=NMA, n=SampleSizes[i], nit=nit, p=p)
    PowerData$Estimate[i] <- results$power*100
    PowerData$CI_lower[i] <- results$CI_lower*100
    PowerData$CI_upper[i] <- results$CI_upper*100
    print(paste("Simulation",i,"of", length(SampleSizes), "complete"))
  }
  PowerData <- gather(PowerData, key = Measure, value = Value, 
               c("Estimate","CI_lower","CI_upper")) #rearrange to three columns
  ggplot(PowerData, aes(x=SampleSize, y=Value, group=Measure, colour=Measure)) +
    geom_line() + geom_point(shape=21) +
    theme_classic() +
    scale_x_continuous(limits=c(0,max(SampleSizes)+5), expand=c(0,0)) +
    scale_y_continuous(limits=c(0,NA), expand=c(0,0)) +
    labs(x = "Total Sample Size", y = "Power (%)", title = "Power curves", subtitle = "with 95% confidence intervals") +
    theme(legend.position="bottom", legend.title=element_blank(), legend.background = element_rect(linetype="solid",colour ="black"), 
          panel.grid.major.y = element_line(colour="gray80", linetype='dashed', size=0.5), plot.margin = unit(c(10,15,10,10), "points"), 
          plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
    scale_color_manual(values=c('gray','gray','black'), breaks=c("Estimate","CI_lower"), labels=c("Power estimate","95% confidence interval"))

}

metapowplot(SampleSizes=c(100,200,300,400,500,600,700,800,900,1000), NMA=MA, nit=25, p=0.05)
