#### Langan Plots ####
library(metafor)
library(ggplot2)
library(tibble)
library(magrittr)

### Test Data antibiotics vs. control for the common cold to alleviate symptoms by 7 days ###
raw_data <- data.frame(StudyID=c(1,2,3,4,5,6), Study=c("Herne_1980","Hoaglund_1950","Kaiser_1996","Lexomboon_1971","McKerrow_1961","Taylor_1977"),
                   R.1=c(7,39,97,8,5,12), N.1=c(7+39,39+115,97+49,8+166,5+10,12+117), T.1=rep("Treatment",6),
                   R.2=c(10,51,94,4,8,3), N.2=c(10+12,51+104,94+48,4+83,8+10,3+56), T.2=rep("Control",6))
# Obtain study effects and standard errors #
MAdata <- escalc(measure='OR', ai=R.1, bi=N.1-R.1, ci=R.2, di=N.2-R.2, data=raw_data)   # gives ES (effect estimate) and seES (sampling variances) on logOR scale for binary data
MAdata$sei <- sqrt((1/raw_data$R.1) + (1/(raw_data$N.1-raw_data$R.1)) + (1/raw_data$R.2) + (1/(raw_data$N.2-raw_data$R.2)))  # Calculate standard errors
outcome <- 'OR'
method='random'
rand.load <- 10
if (outcome %in% c("OR","RR")) {
  MAdata$yi_trans <- exp(MAdata$yi)
}
# pairwise meta-analysis #
random_model <- rma(yi, vi, slab=Study, data=MAdata, method="PM", measure=outcome)
MAresults <- summary(random_model)
# data frame for summary diamond #
sediff <- max(MAdata$sei) - min(MAdata$sei)
ylim <- c(0,1)   # pre-set for plot
if (!is.null(ylim)) if (ylim[1]<ylim[2]) ylim <- rev(ylim)   # if there is a pre-set
if (is.null(ylim)) {
  ylim <- c(max(MAdata$sei) + 0.2*sediff, min(MAdata$sei) - 0.25*sediff)   # from code
  if (ylim[2]<0) ylim[2] <- 0
}
axisdiff <- ylim[2] - ylim[1]
summ.pos=0 # by default - Langan code has this is an option
summary_diamond <- data.frame(
  xsumm = c(MAresults$ci.lb, MAresults$b, MAresults$ci.ub, MAresults$b),
  ysumm = c(ylim[2]-0.10*axisdiff+summ.pos,ylim[2]-0.06*axisdiff+summ.pos,ylim[2]-0.10*axisdiff+summ.pos,ylim[2]-0.14*axisdiff+summ.pos)
  )


### Creating the significance contours (code from Langan) ###
# plot parameters/sizes/weights
SSdiff <- max(MAdata$yi) - min(MAdata$yi)  #difference between effect sizes
xlim <- c(min(MAdata$yi) - 0.2*SSdiff, max(MAdata$yi) + 0.2*SSdiff) # limits in effect size with a buffer
contour.points=200 # smoother option - set at default
cSS <- seq(xlim[1], xlim[2], length.out=contour.points)  # all estimate effects on plot
csize <- seq(ylim[1], ylim[2], length.out=contour.points)  # all standard errors on plot
csize[csize<=0] <- 0.0000001*min(MAdata$sei)
for (k in 2:length(csize)) if (csize[k]==0 & csize[k-1]==0) csize[k] <- NA
csize <- csize[!is.na(csize)]   # removing zero entries?
# weightings of studies
if (method=="random") {
  df <- NROW(MAdata$yi) - 1
  #df2 = the degrees of freedom given a prospective trial is also included
  df2= df+1
  size <- 1/((MAdata$sei^2)+MAresults$tau2)
} else {
  size <- 1/(MAdata$sei^2)
}
ci <- qnorm(1-((0.05)/2))  #converts significance level (e.g. 0.05 -> 1.96)
zero <- 0    # option of what is 'zero-effect'
# fixed-effect model #
if (method=="fixed")  {
  vwt <- 1/(csize^2)
  c1SS <- (1/vwt)*(zero*(sum(size) + vwt) - sum(size*MAdata$yi) +  ci * (sum(size)+vwt)^0.5)
  c2SS <- (1/vwt)*(zero*(sum(size) + vwt) - sum(size*MAdata$yi) -  ci * (sum(size)+vwt)^0.5)
}
# random-effects model #  
if (method=="random")  {
  matcont <- matrix(rep(NA,times=length(cSS)*length(csize)),nrow=length(csize))	 # empty matrix of points
  overmax <- 0
  for (i in 1: length(csize))  {   # for every standard error on the plot
    #display percentage of computation complete
    if (rand.load>0) {                                    
      roundi<-i/rand.load
      flush.console()
      if (roundi==round(roundi,0)) {
        perc_complete <- (i/contour.points)*100
        cat(perc_complete, "%")
      }
      else cat(".")
    }
    # where the standard errors exist in the plot
    if ( !is.na(csize[i]) ) {
      # for every effect size on the plot 
      for (j in 1:length(cSS))  {
        # calculate the MA for the existing studies alongside a new study of effect size and standard error as per plot position  
        MAupdate <- MAdata %>% add_row(yi = cSS[j], sei = csize[i], Study = 'New')   
        newMA <- rma(yi, sei, slab=Study, data=MAupdate, method="PM", measure=outcome)
        newMA_summ <- summary(newMA)
        lc <- newMA_summ$ci.lb
        uc <- newMA_summ$ci.ub
        # code according to significance  
        if (lc < zero & uc < zero) matcont[i,j] <- 1   # sig < 0
        if (lc < zero & uc > zero) matcont[i,j] <- 0   # not sig
        if (lc > zero & uc > zero) matcont[i,j] <- 2   # sig > 0
      }	   
    }
    # for when standard error is NA  
    else matcont[i,] <- 3
  }
}





## Piece together first plot (studies, summary diamond, contours) ###

Langan <- ggplot(MAdata, aes(x=yi, y=sei)) +
  # line of no effect
  geom_vline(xintercept=0, color='lightgrey') +
  # summary results diamond
  geom_polygon(data=summary_diamond, aes(x=xsumm, y=ysumm),
               color="black", fill = "lavenderblush4") +
  # plot individual studies in current MA
  geom_point() +
  # formatting of plot
  labs(x = "Odds Ratio", y = "Standard Error", title = "Meta-analysis results with Significance Contours") +
  theme_classic() + theme(aspect.ratio=1, panel.background = element_rect(colour = "black")) +
  scale_x_continuous(breaks=log(c(0.25, 0.5, 1, 2, 4)), labels=c(0.25, 0.5, 1, 2, 4), limits=xlim, expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0))

#The significance contours (creates squares of colour)
contour_data <- data.frame(cSS = cSS, csize=csize)
for (j in 1:(length(cSS)-1)) {
  for (i in 1:(length(csize)-1)) {
    if (matcont[i,j]==0 & matcont[i+1,j]==0 & matcont[i,j+1]==0 & matcont[i+1,j+1]==0)
      Langan <- Langan +
        geom_polygon(data=contour_data,
                     aes(x=c(cSS[j],cSS[j],cSS[j+1],cSS[j+1]),y=c(csize[i],csize[i+1],csize[i+1],csize[i])), 
                     color="white", fill = "white", alpha=0.5)
    if (matcont[i,j]==1 & matcont[i+1,j]==1 & matcont[i,j+1]==1 & matcont[i+1,j+1]==1)
      Langan <- Langan +
        geom_polygon(data=contour_data,
                     aes(x=c(cSS[j],cSS[j],cSS[j+1],cSS[j+1]),y=c(csize[i],csize[i+1],csize[i+1],csize[i])), 
                     color="gray91", fill = "gray91", alpha=0.5)
    if (matcont[i,j]==2 & matcont[i+1,j]==2 & matcont[i,j+1]==2 & matcont[i+1,j+1]==2)
      Langan <- Langan +
        geom_polygon(data=contour_data,
                     aes(x=c(cSS[j],cSS[j],cSS[j+1],cSS[j+1]),y=c(csize[i],csize[i+1],csize[i+1],csize[i])), 
                     color="gray72", fill = "gray72", alpha=0.5)
  }
}


Langan
# Maxed out ggplot....

