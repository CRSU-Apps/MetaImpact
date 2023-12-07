#-----------------------------------------------------------------------------------#
# Function to create funnel plots with MA results, contours, and simulation results #
#-----------------------------------------------------------------------------------#
#----------------------------------------#
# Developed using code from Langan et al #
#----------------------------------------#
#--------------------------#  
# Clareece Nevill May 2023 #
#--------------------------#

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
#legendpos - position as per ggplot styling									#
#summ - TRUE/FALSE plot summary diamond including pooled effect and confidence interval (significance level as defined by sig.level		#
#summ.pos - adjustment of position of summary diamond
#xpoints/ypoints - add an extra point(s) in the plot just to show as an example								#
#points - whether the study points should be displayed at all (TRUE default)								#
#pred.interval - TRUE/FALSE display predictive interval along with the summary diamond.								#
#rand.load - show percentage of computations complete when the random effects contours are calculated				#
#sim.points - add simulated trials to plot (data frame of 'estimate' and 'st_err')                  #
############################################################################################################################


# ### Test Data antibiotics vs. control for the common cold to alleviate symptoms by 7 days ###
# raw_data_bin <- data.frame(StudyID=c(1,2,3,4,5,6), Study=c("Herne_1980","Hoaglund_1950","Kaiser_1996","Lexomboon_1971","McKerrow_1961","Taylor_1977"),
#                    R.1=c(7,39,97,8,5,12), N.1=c(7+39,39+115,97+49,8+166,5+10,12+117), T.1=rep("Treatment",6),
#                    R.2=c(10,51,94,4,8,3), N.2=c(10+12,51+104,94+48,4+83,8+10,3+56), T.2=rep("Control",6))
# raw_data_con <- data.frame(StudyID=c(1,2,3,4,5,6,7), Study=c("Connor_2002","Geier_2004","Kinzler_1991","Lehri_2004","Halsch_2001","Volz_1997","Warnecke_1991"),
#                        Mean.1=c(5.7,12.7,12.3,10.6,3,21,25.61), Mean.2=c(8.5,12.3,3.6,9.2,0.6,16.2,7.65), 
#                        SD.1=c(7.6,6.7,8.7,7.3,7.5,13,12.8), SD.2=c(4.2,7.3,8.4,10,4.6,14.3,15.9),
#                        N.1=c(17,25,29,34,20,52,20), N.2=c(18,25,29,23,30,48,20),
#                        T.1=rep("Treatment",7), T.2=rep("Control",7))
# ### Obtain study effects and standard errors #
# MAdata_bin <- escalc(measure='OR', ai=R.1, bi=N.1-R.1, ci=R.2, di=N.2-R.2, data=raw_data_bin)   # gives ES (effect estimate) and seES (sampling variances) on logOR scale for binary data
# MAdata_con <- escalc(measure="MD", m1i=Mean.1, m2i=Mean.2, sd1i=SD.1, sd2i=SD.2, n1i=N.1, n2i=N.2, data=raw_data_con)
# MAdata_bin$sei <- sqrt(MAdata_bin$vi)  # Calculate standard errors
# MAdata_con$sei <- sqrt(MAdata_con$vi)
# #### Add simulated trials ##
# source("SampleSizeFunctions.R")
# source("MAFunctions.R")
# app_MA_bin <- FreqPair(data=MAdata_bin, outcome='OR', CONBI='binary', model='both')
# app_MA_con <- FreqPair(data=MAdata_con, outcome='MD', CONBI='continuous', model='both')
# sims_bin <- metapow(NMA=app_MA_bin, data=raw_data_bin, n=2000, nit=300, inference='pvalue', pow=0.05, measure='OR', recalc=FALSE, plot_ext=NA)
# # result (fixed-effect) was 72% power (95% CI: 66.1% to 76.6%)
# sims_con <- metapow(NMA=app_MA_con, data=raw_data_con, n=300, nit=300, inference='pvalue', pow=0.05, measure='MD', recalc=FALSE, plot_ext=NA)



#------------------#
# Specify function #
#------------------#

extfunnel <- function(SS, seSS, method, outcome,
                      sig.level=0.05, contour=FALSE, contour.points=200, summ=TRUE,
                      summ.pos=0, pred.interval=FALSE, plot.zero=TRUE, plot.summ=FALSE, ylim=NULL, xlim=NULL, legend=FALSE,
                      expxticks=NULL, xticks=NULL, yticks=NULL, zero=0, xlab=NULL, ylab=NULL, rand.load=100,
                      legendpos=NULL, sim.points=NULL, points=TRUE) {


#----------------------------------------#  
# Calculate initial arguments/parameters #
#----------------------------------------#
  
  #Converts the significance level into 'z' from the normal distribution (i.e. 0.05 -> 1.96)
  ci <- qnorm(1-((sig.level)/2)) 
  
  #Number of studies in meta-analysis for use later
  #length <- length(SS)
  #length.vector <- c(rep(1,length))
  
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
    if (method=="random")  {     # code had been developed from Langan et al & Florian Teichert
      print("A less computationally expensive method is required.")
      # tibble of every point on plot (i.e. every possible combo)
      #contour_tiles <- expand_grid(cSS = cSS, csize = csize) %>%   
      #        mutate(id = row_number(), code=NA) %>%
      #        select(id, everything())
      #for (i in 1:nrow(contour_tiles)) {        # need to increase speed....
      #  if (rand.load>0) {                                    
      #    roundi<-i/rand.load
      #    flush.console()
      #    if (roundi==round(roundi,0)) {
      #      perc_complete <- (i/(contour.points*contour.points))*100
      #      cat(perc_complete, "%")
      #    } else cat(".")
      #  }
      #  metacont <- rma(yi=c(SS,contour_tiles$cSS[i]), sei=c(seSS,contour_tiles$csize[i]), method=ifelse(method=='random',"PM","FE"), level=(1-sig.level))
      #  lc <- metacont$ci.lb
      #  uc <- metacont$ci.ub
      #  # code according to significance  
      #  if (lc < zero & uc < zero) contour_tiles$code[i] <- "sigless_col"   # sig < 0
      #  if (lc < zero & uc > zero) contour_tiles$code[i] <- "nosig_col"   # not sig
      #  if (lc > zero & uc > zero) contour_tiles$code[i] <- "sigmore_col"   # sig > 0
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
    
    if (pred.interval) {	
      if (method=='random') { 
        predint1 <- predict(meta)$pi.lb
        predint2 <- predict(meta)$pi.ub
        # update x-axis limits if predictive interval is wider
        xlim <- c(min(predint1-0.5, xlim[1]), max(predint2+0.5, xlim[2]))
      } else {
        print("For fixed-effects models, tau-squared is equal to 0 and therefore the PI becomes equivalent to the CI")
      }
    }
  }
  
#---------------#  
# Design legend #
#---------------#
  
  # empty data frame ready for filling (one for each type of legend)
  legendmat.col.values <- NULL
  legendmat.col <- data.frame(labels=rep(NA,5), linetype=rep(NA,5), shape=rep(NA,5), color=rep(NA,5))
  legendmat.fill.values <- NULL
  legendmat.fill.labels <- NULL
  
  if (points) {
    legendmat.col.values <- c(legendmat.col.values,"point_col"="black")
    legendmat.col$labels[1] <- "Current studies"
    legendmat.col$linetype[1] <- "blank"
    legendmat.col$shape[1] <- 19
    legendmat.col$color[1] <- "black"
  }
  
  if (!is.null(sim.points)) {
    legendmat.col.values <- c(legendmat.col.values,"sim_col"="black")
    legendmat.col$labels[2] <- "Simulated studies"
    legendmat.col$linetype[2] <- "blank"
    legendmat.col$shape[2] <- 20
    legendmat.col$color[2] <- "black"
  }
  
  if (pred.interval) {
    legendmat.col.values <- c(legendmat.col.values,"pred_col"="black")
    legendmat.col$labels[3] <- "95% Predictive Interval"
    legendmat.col$linetype[3] <- "solid"
    legendmat.col$color[3] <- "black"
  }
  
  if (plot.summ) {
    legendmat.col.values <- c(legendmat.col.values,"summ_col"="slategrey")
    legendmat.col$labels[4] <- "Pooled Effect"
    legendmat.col$linetype[4] <- "solid"
    legendmat.col$color[4] <- "slategrey"
  }
  
  if (plot.zero) {
    legendmat.col.values <- c(legendmat.col.values,"zero_col"="lightgrey")
    legendmat.col$labels[5] <- "Null Effect"
    legendmat.col$linetype[5] <- "solid"
    legendmat.col$color[5] <- "lightgray"
  }
  
  if (summ) {
    legendmat.fill.values <- c(legendmat.fill.values,"diamond_fill"="lavenderblush4")
    legendmat.fill.labels <- c(legendmat.fill.labels,"Pooled Result (diamond)")
  }
  
  if (contour) {
    legendmat.fill.values <- c(legendmat.fill.values,"nosig_col"="white","sigless_col"="gray91","sigmore_col"="gray72")
    legendmat.fill.labels <- c(legendmat.fill.labels, paste("Non Sig Effect (",sig.level*100,"% level)", sep=""), paste("Sig Effect < NULL (",sig.level*100,"% level)", sep=""), paste("Sig Effect > NULL (",sig.level*100,"% level)", sep=""))
  }
  
  # drop rows that are not included (based on inputs)
  legendmat.col <- legendmat.col[!is.na(legendmat.col$labels),]
  
#-------------------#  
# Put together plot #
#-------------------#  
  
  # empty frame
  plot <- ggplot(data=data.frame(x=SS, y=seSS), aes(x=x, y=y)) +
    labs(x = xlab, y = ylab) +
    theme_classic() + theme(aspect.ratio=1, panel.background = element_rect(colour = "black")) +
    scale_x_continuous(limits=xlim, expand=c(0,0)) +
    scale_y_reverse(limits=ylim, expand=c(0,0))
  
  # Specify axis ticks if specified or exponential (and extend if predictive interval)
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
      geom_polygon(data=data.frame(x=c(c1SS, rev(c2SS)), y=c(csize, rev(csize))), aes(x=x, y=y, fill="nosig_col"), color="white") +
      geom_polygon(data=data.frame(x=c(c2SS,xlim[1], xlim[1]), y=c(csize, ylim[2], ylim[1])), aes(x=x, y=y, fill="sigless_col"), color="gray91") +
      geom_polygon(data=data.frame(x=c(c1SS,xlim[2], xlim[2]), y=c(csize, ylim[2], ylim[1])), aes(x=x, y=y, fill="sigmore_col"), color="gray72") 
  }
  if (contour & method=='random') {
    plot <- plot +
      geom_tile(data=contour_tiles, aes(x = cSS, y = csize, fill = code))  # haven't tested this yet or its affect on the legend
  }
  
  # Pooled effect line
  if (plot.summ) {
    plot <- plot +
      geom_vline(aes(xintercept = meta$b, color="summ_col"))  
  }
  
  # summary diamond
  if (summ) {
    if (pred.interval) {
      plot <- plot +
        geom_segment(aes(x=predint1, y=ylim[2]-0.10*axisdiff+summ.pos, xend=predint2, yend=ylim[2]-0.10*axisdiff+summ.pos, color="pred_col"),
                     show.legend=ifelse(plot.summ | plot.zero, FALSE, TRUE))   # needed to avoid crosshairs in legend for when there are vlines also present
    }
    plot <- plot +
      geom_polygon(data=summary_diamond, aes(x=xsumm, y=ysumm,
                   fill = "diamond_fill"), color="black")
  }
  
  # Null vertical line
  if (plot.zero) {
    plot <- plot +
      geom_vline(aes(xintercept = zero, color="zero_col"))
  }
  
  # Simulated trials
  if (!is.null(sim.points)) {
    if (method=='fixed') {
      plot <- plot +
        geom_point(data=sim.points, aes(x=estimate.fixed, y=st_err.fixed, color="sim_col"),
                  shape=20)
    } else {
      plot <- plot +
        geom_point(data=sim.points, aes(x=estimate.rand, y=st_err.rand, color="sim_col"),
                   shape=20)
    }
  }
  
  # Study points
  if (points) {
    plot <- plot +
      geom_point(aes(color="point_col"), shape=19)
  }
  
  # Add legend
  if (!is.null(legendmat.col.values)) {
    plot <- plot +
      scale_colour_manual(name="",
                          values=legendmat.col.values,
                          labels=legendmat.col$labels,
                          guide=guide_legend(override.aes = list(linetype = legendmat.col$linetype,
                                                                shape = legendmat.col$shape,
                                                                color = legendmat.col$color)))
  }
  if (!is.null(legendmat.fill.values)) {
      plot <- plot + 
        scale_fill_manual(name="",
                          values=legendmat.fill.values,
                          labels=legendmat.fill.labels)
  }
  if (!legend) {
    plot <- plot +
      theme(legend.position = "none")   # turns off legend
  }
  if (legend & !is.null(legendpos)) {
    plot <- plot +
      theme(legend.position = legendpos)   # if user wants legend and specified a position
  }
  
  # return final plot
  return(plot)
  
}
  
#-----------------------------------------#
#             End of function             #
#-----------------------------------------#

# Tests #  
  
# Study points & summary diamond  PASS
#extfunnel(SS=MAdata_bin$yi, seSS=MAdata_bin$sei, method='fixed', outcome='OR',
#          ylim=c(0,1), expxticks=c(0.25,0.5,1,2,4), xlab="Odds Ratio", legend=TRUE)

# Study points & summary diamond with predictive interval  PASS
#extfunnel(SS=MAdata_bin$yi, seSS=MAdata_bin$sei, method='random', outcome='OR',
#          ylim=c(0,1), expxticks=c(0.25,0.5,1,2,4), xlab="Odds Ratio", pred.interval=TRUE, legend=TRUE)

# Study points & summary diamond & effect line PASS
#extfunnel(SS=MAdata_bin$yi, seSS=MAdata_bin$sei, method='fixed', outcome='OR',
#          ylim=c(0,1), expxticks=c(0.25,0.5,1,2,4), xlab="Odds Ratio", plot.summ=TRUE, legend=TRUE)

# Study points & summary diamond & significance contours  PASS
#extfunnel(SS=MAdata_bin$yi, seSS=MAdata_bin$sei, method='fixed', outcome='OR',
#          ylim=c(0,1), xlim=(log(c(0.1, 4))), expxticks=c(0.25,0.5,1,2,4), xlab="Odds Ratio", contour=TRUE, legend=TRUE, legendpos='left')

# Study points, summary diamond, significance contours & simulated trials  PASS
#extfunnel(SS=MAdata_bin$yi, seSS=MAdata_bin$sei, method='fixed', outcome='OR',
#          ylim=c(0,1), expxticks=c(0.25,0.5,1,2,4), xlab="Odds Ratio", contour=TRUE, sim.points=sims_bin$sim_study, legend=TRUE)

# Above but zoomed in on simulated studies  PASS
#extfunnel(SS=MAdata_bin$yi, seSS=MAdata_bin$sei, method='fixed', outcome='OR',
#          ylim=c(0.05,0.15), xlim=log(c(0.4, 2.1)), expxticks=c(0.25,0.5,1,2,4), xlab="Odds Ratio", 
#          contour=TRUE, sim.points=sims_bin$sim_study, legend=TRUE)   # would be ideal to remove 'current studies' from legend if they are forced off from the plot

# # Mirror figure 2C in Langan et al  FAIL -> yes but big old triangle of colour missing at the bottom (I think it's going to have to stay as a bug for now...its only when the 'curtain' moves across to the opposite side to standard...)
# extfunnel(SS=MAdata_con$yi, seSS=MAdata_con$sei, method='fixed', outcome='MD',
#           ylim=c(0,5.5), xlab="Difference in means", contour=TRUE, plot.summ=TRUE, legend=TRUE)
# 
# # Finding error of continuous not showing predictive interval (FIXED)
# extfunnel(SS=MAdata_con$yi, seSS=MAdata_con$sei, method='random', outcome='MD',
#           ylim=c(0,5.5), xlab="Difference in means", contour=FALSE, plot.summ=TRUE, legend=TRUE, pred.interval = TRUE)

