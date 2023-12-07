# Running Meta-Analysis Functions #
#---------------------------------#


# Reshaping #
#-----------#

### Convert long to wide ###
Long2Wide <- function(data) { #inputs: data frame
  TempData <- as.data.frame(data)
  if (ncol(TempData)==6 | ncol(TempData)==5){ #long format
    TempData<-TempData[order(TempData$StudyID, TempData$T), ]
    TempData$Arms<- ave(as.numeric(TempData$StudyID),TempData$StudyID,FUN=seq_along)  # create counting variable for number of arms within each study.
    data_wide <- reshape(TempData, timevar = "Arms",idvar = c("Study", "StudyID"), direction = "wide") # reshape
  }
  else {
    data_wide<- TempData
  }
}

### Convert wide to long ###
Wide2Long <- function(data) { #inputs: data frame
  TempData <- as.data.frame(data)
  if (ncol(TempData)==6 | ncol(TempData)==5) {
    data_long <- TempData
  } else {                # wide format
    TempData <- TempData[order(TempData$StudyID), ]
    data_long <- reshape(TempData, idvar = c("Study", "StudyID"), direction = "long", varying = 3:ncol(TempData))
    data_long <- data_long[!is.na(data_long$T), ]
    data_long <- data_long[order(data_long$StudyID, data_long$T), ]
  }
}

### Swapping treatment and control as neccessary when in wide format ###
SwapTrt <- function(CONBI, data, trt) { # inputs: continuous/binary; data frame; intended primary treatment 
  if (CONBI=='continuous') {  # different variables need swapping
    list_vars <- c("T", "N", "Mean", "SD")
  } else {
    list_vars <- c("T", "N", "R")
  }
  for (i in 1:nrow(data)) {   # need to check for each study
    if (data$T.1[i]!=trt) {   # if the study data needs swapping
      for(j in 1:length(list_vars)) {     # complete the swaps for each variable
        vars <- list(varname = as.name(list_vars[[j]]))
        eval(parse(text = paste0("data$",vars,".3 <- NA"))) # initialise
        eval(parse(text = paste0("data$",vars,".3[i] <- data$",vars,".2[i]")))
        eval(parse(text = paste0("data$",vars,".2[i] <- data$",vars,".1[i]")))
        eval(parse(text = paste0("data$",vars,".1[i] <- data$",vars,".3[i]")))
        eval(parse(text = paste0("data <- subset(data, select=-",vars,".3)")))
      }
    }
  }
  return(data)   # output is corrected data frame
}

## Ensure have StudyIDs and sequential ##


# Frequentist #
#-------------#

### Frequentist Pairwise ###

FreqPair <- function(data, outcome, CONBI, model) { #inputs: data frame in wide format; outcome type; continuous or binary' fixed or random (or both);
  if (CONBI=='continuous') {
    MAdata <- escalc(measure=outcome, m1i=Mean.1, m2i=Mean.2, sd1i=SD.1, sd2i=SD.2, n1i=N.1, n2i=N.2, data=data)
  } else {
    MAdata <- escalc(measure=outcome, ai=R.1, bi=N.1-R.1, ci=R.2, di=N.2-R.2, data=data)
  }
  MAdata$sei <- sqrt(MAdata$vi)  #standard error
  if (model=='fixed' | model=='both') {MA.Fixed <- rma(yi, vi, slab=Study, data=MAdata, method="FE", measure=outcome)} #fixed effects#
  if (model=='random' | model=='both') {MA.Random <- rma(yi, vi, slab=Study, data=MAdata, method="PM", measure=outcome)} #random effects #
  list(MAdata=MAdata, MA.Random=MA.Random, MA.Fixed=MA.Fixed)
} 
## DOESN'T WORK UNLESS MODEL=='BOTH'

### Frequentist NMA ###

FreqNMA <- function(data, outcome, CONBI, model, ref) { #inputs: data frame; outcome type; continuous or binary; fixed or random (or both); reference group
  treat <- data[,grep(pattern="^T", colnames(data))]
  n <- data[,grep(pattern="^N", colnames(data))]
  if (CONBI=='continuous') { 
    mean <- data[,grep(pattern="^Mean", colnames(data))]
    sd <- data[,grep(pattern="^SD", colnames(data))]
    d1 <- pairwise(treat=treat,n=n,mean=mean,sd=sd, data=data, studlab=Study, sm=outcome) #convert to contrast form
  } else {
    event <- data[,grep(pattern="^R", colnames(data))]
    d1 <- pairwise(treat=treat,event=event,n=n, data=data, studlab=Study, sm=outcome)
  }
  net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data=d1,
                  sm=outcome, level=0.95, level.comb=0.95,
                  comb.random=(model=='random' | model=='both'), comb.fixed=(model=='fixed' | model=='both'), reference.group=ref,
                  all.treatments=NULL, seq=NULL, tau.preset=NULL,
                  tol.multiarm=0.05, tol.multiarm.se=0.2, warn=TRUE)
  list(MAObject=net1, MAData=d1)
}

### Forest Plot ###

FreqNMAForest <- function(NMA, model, ref) { #inputs: NMA object; fixed or random; reference treatment
  metafor::forest(NMA,reference.group=ref, pooled=model)
}


# Bayesian #
#----------#

### Bayesian Pairwise ###

#rstan_options(auto_write = TRUE)
#options(mc.cores = max(parallel::detectCores() - 1, 1))   #rstan options to help with computational speed
# data and inputs for example (wide format required)
#data <- data.frame(StudyID=c(1,2,3,4,5,6), Study=c("Herne_1980","Hoaglund_1950","Kaiser_1996","Lexomboon_1971","McKerrow_1961","Taylor_1977"),
#                   R.1=c(7,39,97,8,5,12), N.1=c(7+39,39+115,97+49,8+166,5+10,12+117), T.1=rep("Treatment",6),
#                   R.2=c(10,51,94,4,8,3), N.2=c(10+12,51+104,94+48,4+83,8+10,3+56), T.2=rep("Control",6))
#CONBI <- 'binary'
#trt <- 'Treatment'
#ctrl <- 'Control'
#outcome <- 'OR'

BayesPair <- function(CONBI, data, trt, ctrl, outcome, chains=2, iter=4000, warmup=400, model, prior) {         # inputs: continuous/binary; data frame (wide); treatment and control interventions; OR/RR/MD etc; number of chains; number of iterations, number of iterations to burn; fixed/random/both; type of prior for tau
  # prep data #
  if (CONBI=='binary') {armVars=c(responders="R.", sampleSize="N.")
  } else {armVars=c(mean="Mean.", std.err="SD.")}
  StanData <- create_MetaStan_dat(dat = SwapTrt(CONBI=CONBI, data=data, trt=ctrl),      # arrange data such that 'control' columns are first (metastan backwards) and suitable for meta-stan
                                  armVars=armVars)
  if (outcome=='OR' | outcome=='RR' | outcome=='RD') {
    like <- 'binomial'
  } else {
    like <- 'normal'
  }
  if (prior=='uniform') {
    prior.value <- 2                      # uniform(0,2)
  } else if (prior=='half-cauchy') {
    prior.value <- 0.5                    # hCauchy(0,0.5)
  } else if (prior=='half-normal') {
    prior.value <- 1                      # hNormal(0,1)
  }
  # Run analysis #
  if (model=='random' | model=='both') {
    MA.Random <- meta_stan(data = StanData,
                         likelihood = like,
                         re = TRUE,
                         ncp = TRUE,                     # suggested for small datasets
                         mu_prior = c(0, 100),           # vague normal prior for all study baseline/control effects
                         theta_prior = c(0, 10),         # vague normal prior for treatment effect estimate (which is mean of distribution from which all study treatment effects come from)
                         tau_prior_dist = prior,         # type of prior distribution for tau (between study SD)
                         tau_prior = prior.value,        # details of prior distribution
                         chains = chains,
                         iter = iter,
                         warmup = warmup,
                         adapt_delta = 0.99)
  }
  if (model=='fixed' | model=='both') {
    MA.Fixed <- meta_stan(data = StanData,
                         likelihood = like,
                         re = FALSE,
                         ncp = TRUE,                     # suggested for small datasets
                         mu_prior = c(0, 100),           # vague normal prior for all study baseline/control effects
                         theta_prior = c(0, 10),         # vague normal prior for treatment effect estimate (which is same for all studies)
                         chains = chains,
                         iter = iter,
                         warmup = warmup,
                         adapt_delta = 0.99)
  }
  # prep output data
  MAdata <- SwapTrt(CONBI=CONBI, data=data, trt=trt)   # base data
  if (CONBI=='continuous') {
    MAdata <- escalc(measure=outcome, m1i=Mean.1, m2i=Mean.2, sd1i=SD.1, sd2i=SD.2, n1i=N.1, n2i=N.2, data=MAdata) # obtain treatment effects
  } else {
    MAdata <- escalc(measure=outcome, ai=R.1, bi=N.1-R.1, ci=R.2, di=N.2-R.2, data=MAdata)
  }
  if (outcome %in% c('OR','RR')) {
    MAdata$est <- exp(MAdata$yi)
    MAdata$lci <- exp(MAdata$yi - 1.96 * sqrt(MAdata$vi))
    MAdata$uci <- exp(MAdata$yi + 1.96 * sqrt(MAdata$vi))
    results.fixed <- exp(MA.Fixed$fit_sum['theta', c(1,4,8)])
    results.random <- exp(MA.Random$fit_sum['theta', c(1,4,8)])
  } else {
    MAdata$est <- MAdata$yi
    MAdata$lci <- MAdata$yi - 1.96 * sqrt(MAdata$vi)
    MAdata$uci <- MAdata$yi + 1.96 * sqrt(MAdata$vi)
    results.fixed <- MA.Fixed$fit_sum['theta', c(1,4,8)]
    results.random <- MA.Random$fit_sum['theta', c(1,4,8)]
  }
  
  MAdata <- bind_cols(data.frame(StudyID.new = seq(from=1, by=1, length=nrow(data))),
                      MAdata[order(-MAdata$StudyID),])    # reverse ID so that forest plot matched frequentist
  if (CONBI=='binary') {cols_n <- 9} else {cols_n <- 11}  # number of columns in MAdata differs between binary and continuous
  if (model=='fixed') {
    MAdata[nrow(data)+1, -c(3)] <- c(-1, rep(NA,cols_n), results.fixed[1], results.fixed[2], results.fixed[3])
    MAdata[nrow(data)+1, 3] <- "FE Model"
  } else if (model=='random') {
    MAdata[nrow(data)+1, -c(3)] <- c(-1, rep(NA,cols_n), results.random[1], results.random[2], results.random[3])
    MAdata[nrow(data)+1, 3] <- "RE Model"
  } else {
    MAdata[nrow(data)+1, -c(3)] <- c(-1, rep(NA,cols_n), results.fixed[1], results.fixed[2], results.fixed[3])
    MAdata[nrow(data)+2, -c(3)] <- c(-2, rep(NA,cols_n), results.random[1], results.random[2], results.random[3])
    MAdata[(nrow(data)+1):(nrow(data)+2), 3] <- c("FE Model", "RE Model")
  }
  list(MAdata=MAdata, MA.Fixed=MA.Fixed, MA.Random=MA.Random)
}

#test <- BayesPair(CONBI=CONBI, data=data, trt=trt, ctrl=ctrl, outcome=outcome, model='both', prior='half-cauchy')
#test$MAdata

#stan_trace(test$MA.Random$fit, pars="theta")   # trace plot

## Forest plot ##
BayesPairForest <- function(MAdata, model, outcome) {    # inputs: summary MA data, random/fixed/both, OR/RR etc.
  #prep data#
  MAdata$Study = str_replace_all(MAdata$Study, "_", " ")
  if (model=='fixed') {
    MAdata <- MAdata[!(MAdata$Study=='RE Model'),]
    MAdata[(MAdata$Study=='FE Model'),1] <- -1
  } else if (model=='random') {
    MAdata <- MAdata[!(MAdata$Study=='FE Model'),]
    MAdata[(MAdata$Study=='RE Model'),1] <- -1
  }
  scaleFUN <- function(x) sprintf("%.2f", x)   # function to reduce axis labels to 2dp
  # create plot #
  g <- ggplot(aes(est,
                  StudyID.new),
              data = MAdata) +
    # Add vertical lines 
    geom_vline(xintercept = ifelse(outcome %in% c('OR','RR'),1,0), color = "black",
               size = 0.5, linetype = "dashed") +
    geom_hline(yintercept = 0, color = "black", size = 0.5) +
    # Add horizontal lines
    geom_pointinterval(size = 0.5, point_size = 2,
                       aes(xmin = lci, xmax = uci)) +
    # Add weights squares
    #geom_point(aes(size = MAdata$weight)) +       # don't have weighted squares as weighting is more complex with GLMs rather than inverse variance?
    # Add text and labels
    geom_text(data = mutate_if(MAdata,
                               is.numeric, round, 2),
              aes(label = glue("{est} [{lci}, {uci}]"),
                  x = max(uci)+0.2), hjust = "outward") +
    theme_classic() + theme(axis.line.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.text.x=element_text(size=11), axis.title.x=element_text(size=13))
  if (outcome=='OR') {
    g + labs(x = "Odds Ratio (log scale)", y = element_blank()) +
      scale_x_continuous(trans='log2', expand = expansion(mult = 0.4), labels=scaleFUN) +
      geom_text(aes(label = Study, x = min(lci)-0.02), hjust=1)
  } else if (outcome=='RR') {
    g + labs(x = "Risk Ratio (log scale)", y = element_blank()) +
      scale_x_continuous(trans='log2', expand = expansion(mult = 0.4), labels=scaleFUN) +
      geom_text(aes(label = Study, x = min(lci)-0.02), hjust=1)
  } else if (outcome=='RD') {
    g + labs(x = "Risk Difference", y = element_blank()) +
      scale_x_continuous(expand = expansion(mult = 0.4), labels=scaleFUN) +
      geom_text(aes(label = Study, x = min(lci)-0.2), hjust=1)
  } else if (outcome=='MD') {
    g + labs(x = "Mean Difference", y = element_blank()) +
      scale_x_continuous(expand = expansion(mult = 0.4), labels=scaleFUN) +
      geom_text(aes(label = Study, x = min(lci)-0.2), hjust=1)
  } else {
    g + labs(x = "Standardised Mean Difference", y = element_blank()) +
      scale_x_continuous(expand = expansion(mult = 0.4), labels=scaleFUN) +
      geom_text(aes(label = Study, x = min(lci)-0.2), hjust=1)
  }
}

#g<- BayesPairForest(test$MAdata, outcome, model='both')
#g + ggtitle("Forest plot of studies with overall estimate from fixed-effects model")


### Bayesian NMA ###

BayesMA <- function(data, CONBI, outcome, model, ref, prior) { #inputs: data; continuous or binary; outcome type; fixed or random; reference treatment; prior
if (CONBI=='continuous') {                                         # set up data frame
  armData <- data.frame(study=data$Study,
                       treatment=data$T,
                       mean=data$Mean,
                       std.dev=data$SD,
                       sampleSize=data$N)
} else {
  armData <- data.frame(study=data$Study,
                        treatment=data$T,
                        responders=data$R,
                        sampleSize=data$N)
}
mtcNetwork <- mtc.network(data.ab=armData,description="Network")   # Gemtc network object
if (outcome == "MD") {                                             # set up likelihood and links
  like <- "normal"
  link <- "identity"
} else  {
  like <- "binom"
  link <- ifelse (outcome == "OR", "logit", "log")
}
if (prior=='uniform') {
  Prior <- mtc.hy.prior("std.dev", "dunif", 0, 2)
} else if (prior=='gamma') {
  Prior <- mtc.hy.prior("prec", "dgamma", 0.1, 0.1)
} else {
  Prior <- mtc.hy.prior("std.dev", "dhnorm", 0, 1)
}
mtcModel <- mtc.model(network=mtcNetwork,                          # Formulate model
                      type = "consistency",
                      linearModel=model,
                      likelihood=like,
                      link = link,
                      dic=TRUE,
                      hy.prior=eval(Prior))
mtcResults <- mtc.run(mtcModel)                                    # Run model
mtcRelEffects <- relative.effect(mtcResults,t1=ref)                # Obtain relative effects
sumresults <- summary(mtcRelEffects)                               # Summary of relative effects
sumoverall <- summary(mtcResults)                                  # Overall summary of analysis
DIC <- as.data.frame(sumoverall$DIC)                               # DIC
list(Network=mtcNetwork, RelEffects=mtcRelEffects, ResultsSum=sumresults, DIC=DIC)
}

TauDesc <- function(ResultsSum,outcome,model) {                    # Between-study standard deviation
  if (model=="random") {
    ntx <- nrow(ResultsSum$summaries$statistics)
    sd_mean<- round(ResultsSum$summaries$statistics[ntx,1], digits = 2)
    sd_lowCI<-round(ResultsSum$summaries$quantiles[ntx,1], digits = 2)
    sd_highCI<-round(ResultsSum$summaries$quantiles[ntx,5], digits=2)
  }
  if (model=="random") {
    if (outcome=="OR") {
      paste("Between-study standard deviation (log-odds scale):", sd_mean, ". 95% credible interval:",sd_lowCI,", ", sd_highCI, ".")}
    else if (outcome=="RR") {
      paste ("Between-study standard deviation (log probability scale):", sd_mean, ". 95% credible interval:",sd_lowCI,", ", sd_highCI, ".")}
    else {
      paste ("Between-study standard deviation:", sd_mean, ". 95% credible interval:",sd_lowCI,", ", sd_highCI, ".")}}
  else{
    if (outcome=="OR") {
      paste("Between-study standard deviation (log-odds scale) set at 0")}
    else if (outcome=="RR") {
      paste("Between-study standard deviation (log probability scale) set at 0")}
    else {
      paste("Between-study standard deviation set at 0")}
  }
}




