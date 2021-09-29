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


# Frequentist #
#-------------#

### Frequentist MA ###

FreqMA <- function(data, outcome, CONBI, model, ref) { #inputs: data frame; outcome type; continuous or binary; fixed or random; reference group
  treat <- data[,grep(pattern="^T", colnames(data))]
  n <- data[,grep(pattern="^N", colnames(data))]
  if (CONBI=='continuous') { #convert to contrast form
    mean <- data[,grep(pattern="^Mean", colnames(data))]
    sd <- data[,grep(pattern="^SD", colnames(data))]
    d1 <- pairwise(treat=treat,n=n,mean=mean,sd=sd, data=data, studlab=Study, sm='MD')
  } else {
    event <- data[,grep(pattern="^R", colnames(data))]
    d1 <- pairwise(treat=treat,event=event,n=n, data=data, studlab=Study, sm=outcome)
  }
  net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data=d1,
                  sm=outcome, level=0.95, level.comb=0.95,
                  comb.random=(model=='random'), comb.fixed=(model=='fixed'), reference.group=ref,
                  all.treatments=NULL, seq=NULL, tau.preset=NULL,
                  tol.multiarm=0.05, tol.multiarm.se=0.2, warn=TRUE)
  return(list(MAObject=net1, MAdata=d1))
}



# Bayesian #
#----------#