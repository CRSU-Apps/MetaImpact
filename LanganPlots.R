#-----------------------------------------------------------------------------------#
# Function to create funnel plots with MA results, contours, and simulation results #
#-----------------------------------------------------------------------------------#
#----------------------------------------#
# Developed using code from Langan et al #
#----------------------------------------#
#--------------------------#  
# Clareece Nevill May 2023 #
#--------------------------#

# Packages required #
library(metafor)
library(ggplot2)
#library(tibble)
#library(magrittr)

############################### Description of possible arguments ##########################################################
#SS - effect estimates of the current studies													#
#seSS - standard errors of the current studies													#
#sig.level - significance level															#
#method - "fixed" or "random"																#
#outcome - "OR", "RR", "RD", "MD", "SMD"
#ylim - c(y1,y2) limits of the y axis														#
#xlim - c(x1,x2) limits of the x axis														#
#contour.points - number of points for creating contours with a random-effects model - more means a smoother contour but takes longer to compute	#
#contour - TRUE/FALSE for displaying the significance contour											#
#legend - TRUE/FALSE for displaying key/legend													#
#expxticks - custom ticks for the x axis on a exponential scale (assumes data is already on log scale)		#
#xticks - custom ticks for the x axis														#
#yticks - custom ticks for the y axis														#
#zero - value for the null effect (usually 0, even when expxticks is used for odds ratios)						#
#xlab - label for the x axis																#
#ylab - label for the y axis																#
#plot.zero - TRUE/FALSE plot the null effect vertical line as defined by the arguament 'zero'					#
#plot.summ - TRUE/FALSE plot the pooled effect vertical line of current studies								#
#legendpos - position of the upper left hand corner on the x/y axis scale									#
#summ - TRUE/FALSE plot summary diamond including pooled effect and confidence interval (significance level as defined by sig.level		#
#summ.pos - adjustment of position of summary diamond
#xpoints/ypoints - add an extra point(s) in the plot just to show as an example								#
#points - whether the study points should be displayed at all (TRUE default)								#
#pred.interval - TRUE/FALSE display predictive interval along with the summary diamond.								#
#rand.load - show percentage of computations complete when the random effects contours are calculated				#
#sim.points - add simulated trials to plot (data frame of 'estimate' and 'st_err')                  #
############################################################################################################################


### Test Data antibiotics vs. control for the common cold to alleviate symptoms by 7 days ###
raw_data <- data.frame(StudyID=c(1,2,3,4,5,6), Study=c("Herne_1980","Hoaglund_1950","Kaiser_1996","Lexomboon_1971","McKerrow_1961","Taylor_1977"),
                   R.1=c(7,39,97,8,5,12), N.1=c(7+39,39+115,97+49,8+166,5+10,12+117), T.1=rep("Treatment",6),
                   R.2=c(10,51,94,4,8,3), N.2=c(10+12,51+104,94+48,4+83,8+10,3+56), T.2=rep("Control",6))
### Obtain study effects and standard errors #
MAdata <- escalc(measure='OR', ai=R.1, bi=N.1-R.1, ci=R.2, di=N.2-R.2, data=raw_data)   # gives ES (effect estimate) and seES (sampling variances) on logOR scale for binary data
MAdata$sei <- sqrt(MAdata$vi)  # Calculate standard errors
#### Add simulated trials ##
source("SampleSizeFunctions.R")
source("MAFunctions.R")
app_MA <- FreqPair(data=MAdata, outcome='OR', CONBI='binary', model='both')
sims <- metapow(NMA=app_MA, data=raw_data, n=2000, nit=300, inference='pvalue', pow=0.05, measure='OR', recalc=FALSE, plot_ext=NA)
# result (fixed-effect) was 72% power (95% CI: 66.1% to 76.6%)



#------------------#
# Specify function #
#------------------#

extfunnel <- function(SS, seSS, method, outcome,
                      sig.level=0.05, contour=FALSE, contour.points=200, summ=TRUE,
                      summ.pos=0, pred.interval=FALSE, plot.zero=TRUE, plot.summ=FALSE, ylim=NULL, xlim=NULL, legend=FALSE,
                      expxticks=NULL, xticks=NULL, yticks=NULL, zero=0, xlab=NULL, ylab=NULL, rand.load=10,
                      legendpos=c(xlim[2]+0.05*(xlim[2]-xlim[1]),ylim[2]), sim.points=NULL, points=TRUE) {


#----------------------------------------#  
# Calculate initial arguments/parameters #
#----------------------------------------#
  
  #Converts the significance level into 'z' from the normal distribution (i.e. 0.05 -> 1.96)
  ci <- qnorm(1-((sig.level)/2)) 
  
  #Number of studies in meta-analysis for use later
  length <- length(SS)
  length.vector <- c(rep(1,length))
  
  #Calculates the summary effect estimate from which we can get tau-squared and CI (using rma from {metafor})
  meta <- rma(yi=SS, sei=seSS, method=ifelse(method=='random',"PM","FE"), level=(1-sig.level), measure=outcome)
  tau2 <- meta$tau2
  
#-------------------------------#  
# CURRENT WEIGHTINGS of studies #
#-------------------------------#
  
  if (method=="random") {
    size <- 1/((seSS^2)+tau2)  # standard inverse-variance weighting
  } else {
    size <- 1/(seSS^2)
  }
  
#-----------------------------------#  
# Intelligent y-axis default limits #
#-----------------------------------#  
  
  sediff <- max(seSS) - min(seSS)
  
  if (!is.null(ylim)) if (ylim[1]<ylim[2]) ylim <- rev(ylim)  # for when the user has already defined y limits
  
  if (is.null(ylim)) {
    ylim <- c(max(seSS) + 0.20*sediff, min(seSS) - 0.25*sediff)
    if (ylim[2]<0) ylim[2] <- 0
  }
  
  axisdiff <- ylim[2] - ylim[1] 
  
#-----------------------------------#  
# Intelligent x-axis default limits #
#-----------------------------------#  
  
  SSdiff <- max(SS) - min(SS)
  
  if (is.null(xlim)) {
    xlim <- c(min(SS) - 0.2*SSdiff, max(SS) + 0.2*SSdiff)
  }  
  
#-------------------------------------------------#
# Vector of weights/sizes to define contours upon #
#-------------------------------------------------#  
  
  if (contour) {
    cSS <- seq(xlim[1], xlim[2], length.out=contour.points)  # granulated vector for effect size (x-axis)
    csize <- seq(ylim[1], ylim[2], length.out=contour.points)  # granulated vector for standard error (y-axis)
    csize[csize<=0] <- 0.0000001*min(seSS)
    for (k in 2:length(csize)) if (csize[k]==0 & csize[k-1]==0) csize[k] <- NA
    csize <- csize[!is.na(csize)]   # remove unnecessary data points
  }

#------------------------------#
# Create significance contours #
#------------------------------#

  if (contour) {

    # fixed-effect model #
    if (method=="fixed")  {
      vwt <- 1/(csize^2)     # weight for each point on plot (inverse of variance) (i.e. weight of new study)
      c1SS <- (1/vwt)*(zero*(sum(size) + vwt) - sum(size*SS) +  ci * (sum(size)+vwt)^0.5)   # formulae based on CI boundaries of new MA meeting no effect (see Langan et al for details)
      c2SS <- (1/vwt)*(zero*(sum(size) + vwt) - sum(size*SS) -  ci * (sum(size)+vwt)^0.5)   
    }

    # random-effects model #  
    if (method=="random")  {
      print("{ggplot2} cannot cope with the current method as defined by Langan et al. A new, less computational method, is to be developed.")
      #matcont <- matrix(rep(NA,times=length(cSS)*length(csize)),nrow=length(csize))	 # empty matrix of points
      #overmax <- 0
      #for (i in 1: length(csize))  {   # for every standard error on the plot
        #display percentage of computation complete
        #if (rand.load>0) {                                    
          #roundi<-i/rand.load
          #flush.console()
          #if (roundi==round(roundi,0)) {
            #perc_complete <- (i/contour.points)*100
            #cat(perc_complete, "%")
          #}
          #else cat(".")
        #}
        # where the standard errors exist in the plot
        #if ( !is.na(csize[i]) ) {
          # for every effect size on the plot 
          #for (j in 1:length(cSS))  {
            # calculate the MA for the existing studies alongside a new study of effect size and standard error as per plot position  
            #metacont <- rma(yi=c(SS,cSS[j]), sei=c(seSS,csize[i]), method=ifelse(method=='random',"PM","FE"), level=(1-sig.level))
            #lc <- metacont$ci.lb
            #uc <- metacont$ci.ub
            # code according to significance  
            #if (lc < zero & uc < zero) matcont[i,j] <- 1   # sig < 0
            #if (lc < zero & uc > zero) matcont[i,j] <- 0   # not sig
            #if (lc > zero & uc > zero) matcont[i,j] <- 2   # sig > 0
          #}	   
        #}
        # for when standard error is NA  
        #else matcont[i,] <- 3
      #}
    }
  }
  
#----------------------#  
# Defining Axis labels #
#----------------------#
  
  if (is.null(xlab)) xlab <- "Effect"
  if (is.null(ylab)) ylab <- "Standard Error" 
  
#-----------------#  
# Summary diamond #
#-----------------#
  
  if (summ) {
    summary_diamond <- data.frame(
      xsumm = c(meta$ci.lb, meta$b, meta$ci.ub, meta$b),
      ysumm = c(ylim[2]-0.10*axisdiff+summ.pos,ylim[2]-0.07*axisdiff+summ.pos,ylim[2]-0.10*axisdiff+summ.pos,ylim[2]-0.13*axisdiff+summ.pos)
    )
    
    if (pred.interval & method=='random') {	# for fixed-effects models, tau2 is equal to 0 and the PI becomes equivalent to the CI
      predint1 <- predict(meta)$pi.lb
      predint2 <- predict(meta)$pi.ub
    }
  }
  
#---------------#  
# Design legend #
#---------------#
  
  #TBD
  
#-------------------#  
# Put together plot #
#-------------------#  
  
  # empty frame
  plot <- ggplot(data=data.frame(x=SS, y=seSS), aes(x=x, y=y)) +
    labs(x = xlab, y = ylab) +
    theme_classic() + theme(aspect.ratio=1, panel.background = element_rect(colour = "black")) +
    scale_x_continuous(limits=xlim, expand=c(0,0)) +
    scale_y_reverse(limits=ylim, expand=c(0,0))
  
  # Specify axis ticks if specified or exponential
  # x axis ticks for exponential effects
  if (!is.null(expxticks)) {
    plot <- plot +
      scale_x_continuous(breaks=log(expxticks), labels=expxticks, limits=xlim, expand=c(0,0))
  }
  # x axis ticks (non exp)
  if (!is.null(xticks)) {
    plot <- plot +
      scale_x_continuous(breaks=xticks, labels=xticks, limits=xlim, expand=c(0,0))
  }
  # y axis ticks
  if (!is.null(yticks)) {
    plot <- plot +
      scale_y_reverse(breaks=yticks, labels=yticks, limits=ylim, expand=c(0,0))
  }
  
  # contours
  if (contour & method=='fixed') {
    plot <- plot +
      geom_polygon(data=data.frame(x=c(c1SS, rev(c2SS)), y=c(csize, rev(csize))), aes(x=x, y=y), color="white", fill="white") +
      geom_polygon(data=data.frame(x=c(c2SS,xlim[1], xlim[1]), y=c(csize, ylim[2], ylim[1])), aes(x=x, y=y), color="gray91", fill="gray91") +
      geom_polygon(data=data.frame(x=c(c1SS,xlim[2], xlim[2]), y=c(csize, ylim[2], ylim[1])), aes(x=x, y=y), color="gray72", fill="gray72")
  }
  # random TBD
  
  # Pooled effect line
  if (plot.summ) {
    plot <- plot +
      geom_vline(xintercept = meta$b, color="slategrey")
  }
  
  # summary diamond
  if (summ) {
    if (pred.interval) {
      plot <- plot +
        geom_segment(aes(x=predint1, y=ylim[2]-0.10*axisdiff+summ.pos, xend=predint2, yend=ylim[2]-0.10*axisdiff+summ.pos))
    }
    plot <- plot +
      geom_polygon(data=summary_diamond, aes(x=xsumm, y=ysumm),
                   color="black", fill = "lavenderblush4")
  }
  
  # Null vertical line
  if (plot.zero) {
    plot <- plot +
      geom_vline(xintercept = zero, color="lightgrey")
  }
  
  # Simulated trials
  if (!is.null(sim.points)) {
    plot <- plot +
      geom_point(data=sim.points, aes(x=estimate, y=st_err),
                 size=0.5)
  }
  
  # study points
  if (points) {
    plot <- plot +
      geom_point()
  }
  
  # return final plot
  return(plot)
  
}
  
#-----------------------------------------#
#             End of function             #
#-----------------------------------------#

# Tests #  
  
# Study points & summary diamond  PASS
extfunnel(SS=MAdata$yi, seSS=MAdata$sei, method='fixed', outcome='OR',
          ylim=c(0,1), expxticks=c(0.25,0.5,1,2,4), xlab="Odds Ratio")

# Study points & summary diamond with predictive interval  PASS
extfunnel(SS=MAdata$yi, seSS=MAdata$sei, method='random', outcome='OR',
          ylim=c(0,1), expxticks=c(0.25,0.5,1,2,4), xlab="Odds Ratio", pred.interval=TRUE)

# Study points & summary diamond & effect line PASS
extfunnel(SS=MAdata$yi, seSS=MAdata$sei, method='fixed', outcome='OR',
          ylim=c(0,1), expxticks=c(0.25,0.5,1,2,4), xlab="Odds Ratio", plot.summ=TRUE)

# Study points & summary diamond & significance contours  PASS
extfunnel(SS=MAdata$yi, seSS=MAdata$sei, method='fixed', outcome='OR',
          ylim=c(0,1), expxticks=c(0.25,0.5,1,2,4), xlab="Odds Ratio", contour=TRUE)

# Study points, summary diamond, significance contours & simulated trials  PASS
extfunnel(SS=MAdata$yi, seSS=MAdata$sei, method='fixed', outcome='OR',
          ylim=c(0,1), expxticks=c(0.25,0.5,1,2,4), xlab="Odds Ratio", contour=TRUE, sim.points=sims$sim_study)

# Above but zoomed in on simulated studies  PASS
extfunnel(SS=MAdata$yi, seSS=MAdata$sei, method='fixed', outcome='OR',
          ylim=c(0.05,0.15), xlim=log(c(0.4, 2.1)), expxticks=c(0.25,0.5,1,2,4), xlab="Odds Ratio", 
          contour=TRUE, sim.points=sims$sim_study)

  
  




