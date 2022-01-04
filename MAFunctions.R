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


# Frequentist #
#-------------#

### Frequentist MA ###
# reference treatment no longer works - guessing its from netmeta update 22.12.21

FreqMA <- function(data, outcome, CONBI, model, ref) { #inputs: data frame; outcome type; continuous or binary; fixed or random (or both); reference group
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

FreqForest <- function(NMA, model, ref) { #inputs: NMA object; fixed or random; reference treatment
  metafor::forest(NMA,reference.group=ref, pooled=model)
}


# Bayesian #
#----------#

### Bayesian MA ###

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




