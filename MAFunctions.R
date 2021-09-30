# Running Meta-Analysis Functions #
#---------------------------------#


# Reshaping #
#-----------#

### Convert long to wide ###
Long2Wide <- function(data, CONBI) { #inputs: data frame; whether continuous or binary
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
Wide2Long <- function(data, CONBI) { #inputs: data frame; continuous or binary
  
}


# Frequentist #
#-------------#

### Frequentist MA ###

FreqMA <- function(data, outcome, CONBI, model, ref) { #inputs: data frame; outcome type; continuous or binary; fixed or random; reference group
  treat <- data[,grep(pattern="^T", colnames(data))]
  n <- data[,grep(pattern="^N", colnames(data))]
  if (CONBI=='continuous') { #convert to contrast form
    mean <- data[,grep(pattern="^Mean", colnames(data))]
    sd <- data[,grep(pattern="^SD", colnames(data))]
    d1 <- pairwise(treat=treat,n=n,mean=mean,sd=sd, data=data, studlab=Study, sm=outcome)
  } else {
    event <- data[,grep(pattern="^R", colnames(data))]
    d1 <- pairwise(treat=treat,event=event,n=n, data=data, studlab=Study, sm=outcome)
  }
  net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data=d1,
                  sm=outcome, level=0.95, level.comb=0.95,
                  comb.random=(model=='random'), comb.fixed=(model=='fixed'), reference.group=ref,
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

BayesMA <- function(data, CONBI, outcome, model, ref) { #inputs: data; continuous or binary; outcome type; fixed or random; reference treatment
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
mtcModel <- mtc.model(network=mtcNetwork,                          # Formulate model
                      type = "consistency",
                      linearModel=model,
                      likelihood=like,
                      link = link,
                      dic=TRUE)
mtcResults <- mtc.run(mtcModel)                                    # Run model
mtcRelEffects <- relative.effect(mtcResults,t1=ref)                # Obtain relative effects
sumresults <- summary(mtcRelEffects)                               # Summary of relative effects
sumoverall <- summary(mtcResults)                                  # Overall summary of analysis
DIC <- as.data.frame(sumoverall$DIC)                               # DIC
DIC
}




