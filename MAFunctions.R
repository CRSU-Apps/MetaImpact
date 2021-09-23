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
  a <- ifelse(CONBI=='continuous', 4, 3)
  numbertreat=(ncol(data_wide)-2)/a
  if (numbertreat < 6) {  # generate additional columns if less than 6 arms.
    for (k in (numbertreat+1):6) {
      if (CONBI=='continuous') {
        data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
      } else {
        data_wide[c(paste0("T.",k),paste0("R.",k),paste0("N.",k))]<-NA
      }
    }
  }
  return(data_wide)
}


# Frequentist #
#-------------#

### Frequentist MA ###

FreqMA <- function(data, outcome, CONBI, model, ref) { #inputs: data frame; outcome type; continuous or binary; fixed or random; reference group
  if (CONBI=='continuous') { #convert to contrast form
    d1 <- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6), data=data, studlab=Study, sm='MD')
  } else {
    d1 <- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),event=list(R.1,R.2,R.3,R.4,R.5,R.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6), data=data, studlab=Study, sm=outcome)
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