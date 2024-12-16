# Sample size calculator functions #
#----------------------------------#

# Functions metasim, metapow, and metapowplot have been transformed from the Stata commands developed by Crowther et al https://doi.org/10.1177/1536867X1301300302

#' Function for creating new trial simulated from the current meta-analysis
#' 
#' @param es pooled estimate from an existing meta-analysis
#' @param tausq estimated tau-squared (between study heterogeneity) from an existing meta-analysis
#' @param var estimated variance from an existing meta-analysis
#' @param model type of meta-analysis model, one of ['fixed', 'random']
#' @param n total sample size of new trial
#' @param data original dataset for the existing meta-analysis
#' @param measure type of meta-analytic outcome, one of ['OR', 'RR', 'RD', 'MD', 'SMD']
#' @return A list containing the key data for the newly simulated trial
#'  The following when measure is 'OR', 'RR', or 'RD'
#'  - 'new.events.t': For binary outcome, number of events in the treatment arm of the new trial
#'  - 'new.events.c': For binary outcome, number of events in the control arm of the new trial
#'  - 'new.effect': For binary outcome, new effect result for new trial (e.g. OR)
#'  The following when measure is 'MD' or 'SMD'
#'  - 'new.mean.t': For continuous outcome, new mean in treatment arm of new trial
#'  - 'new.stdev.t': For continuous outcome, new standard deviation in treatment arm of new trial
#'  - 'new.mean.c': For continuous outcome, new mean in control arm of new trial
#'  - 'new.stdev.c': For continuous outcome, new standard deviation in control arm of new trial
metasim <- function(es, tausq, var, model, n, data, measure) {
  
  # calculate average control group data from current meta-analysis dataset
  if (measure == 'OR' | measure == 'RR' | measure == 'RD') {
    # add zero adjustment if needed
    for (i in 1:nrow(data)) {
      if ((measure == 'OR' | measure == 'RR') & data$R.2[i] == 0) {  
        data$R.2[i] <- data$R.2[i]+0.5
        data$N.2[i] <- data$N.2[i]+1
      }
    }
    prob.c = mean(data$R.2/data$N.2)
  } else {
    mean.c = mean(data$Mean.2) #average mean outcome
    stdev.c = mean(data$SD.2) #standard deviation
  }
  
  # establish standard error for predictive distribution
  if (model == 'random') {
    std_err <- sqrt(tausq + var)
  } else {
    std_err <- sqrt(var)
  }
  
  # draw a new effect measure for new trial from predictive distribution of current MA (in terms of outcome) (note that this is the 'true' effect, and so will naturally vary from the trial effect)
  if (measure == 'OR' | measure == 'RR') {  # for OR/RR, the MA is conducted in log terms
    new.effect <- exp(rnorm(n = 1, mean = es, sd = std_err))
  } else {
    new.effect <- rnorm(n = 1, mean = es, sd = std_err)
  }
  
  # calculate number of events/mean/sd in the control group
  if (measure == 'OR' | measure == 'RR' | measure == 'RD') {
    new.events.c <- rbinom(n = 1, size = n/2, prob = prob.c)
  } else {
    sample <- rnorm(n = n/2, mean = mean.c, sd = stdev.c)  # recreate the new study
    new.mean.c <- mean(sample)   # take mean and SD of new study results
    new.stdev.c <- sd(sample)
  }
  
  # calculate number of events/mean/sd in the treatment group
  if (measure == 'OR' | measure == 'RR' | measure == 'RD') {
    if (measure == 'OR') {
      new.prob.t <- (new.effect*prob.c/(1-prob.c))/(1+(new.effect*prob.c/(1-prob.c)))
    } else if (measure == 'RR') {
      new.prob.t <- new.effect*prob.c
    } else if (measure == 'RD') {
      new.prob.t <- new.effect+prob.c  # risk difference = prob.t - prob.c
      if (new.prob.t < 0) { new.prob.t <- 0}  # the new RD estimate may be so negative that it creates a negative probability of event in treatment group, which needs to be capped
    }
    new.events.t <- rbinom(n = 1, size = n/2, prob = new.prob.t)
    new <- list(new.events.t = new.events.t, new.events.c = new.events.c, new.effect = new.effect)
  } else {
    if (measure == 'MD') {
      sample <- rnorm(n = n/2, mean = mean.c+new.effect, sd = stdev.c)  #assume standard deviation is the same in both groups
    } else {
      sample <- rnorm(n = n/2, mean = (new.effect*stdev.c)+mean.c, sd = stdev.c) # standardised mean difference
    }
    new.mean.t <- mean(sample)
    new.stdev.t <- sd(sample)
    new <- list(new.mean.t = new.mean.t, new.stdev.t = new.stdev.t, new.mean.c = new.mean.c, new.stdev.c = new.stdev.c)
  }
  return(new)
}


#' Function for conducting bulk of power function where the meta-analysis is updated
#' @param new the newly simulated trial data created by the command metasim
#' @param data original meta-analysis dataset
#' @param model type of meta-analysis model, one of ['fixed', 'random']
#' @param n total sample size of new study
#' @param measure type of meta-analytic outcome, one of ['OR', 'RR', 'RD', 'MD', 'SMD']
#' @return meta-analysis object in 'rma' format
rerunMA <- function(new, data, model, n, measure) { 
  
  # combine new trial with original data to create new dataset
  if (measure == 'OR' | measure == 'RR' | measure == 'RD') {
    newtrial <- data.frame(StudyID = max(data$StudyID)+1, Study = paste("New_", format(Sys.Date(), "%Y")), T.1 = data$T.1[1], T.2 = data$T.2[1], R.1 = new$new.events.t, R.2 = new$new.events.c, N.1 = n/2, N.2 = n/2)
    if (measure %in% c('OR', 'RR') & newtrial$R.2 == 0) {   # Add a continuity correction for new trial (rest of data would have been done in metasim command)
      newtrial$R.2 <- newtrial$R.2+0.5
      newtrial$N.2 <- newtrial$N.2+1
    }
  } else {
    newtrial <- data.frame(StudyID = max(data$StudyID)+1, Study = paste("New_", format(Sys.Date(), "%Y")), T.1 = data$T.1[1], T.2 = data$T.2[1], Mean.1 = new$new.mean.t, Mean.2 = new$new.mean.c,
                           SD.1 = new$new.stdev.t, SD.2 = new$new.stdev.c, N.1 = n/2, N.2 = n/2)
  }
  newdata <- rbind(data, newtrial)
  
  # re-meta-analyse the results with the new trial added
  newMA <- FreqPair(data = newdata, outcome = measure, CONBI = ifelse(measure %in% c('OR', 'RR', 'RD'), 'binary', 'continuous'), model = 'both')  # once fixed FreqPair, can change model = 'fixed' etc. to reduce computation (MIMP-19)
  if (model == 'fixed') {
    return(newMA$MA.Fixed)
  } else {
    return(newMA$MA.Random)
  }
}


#' Function for calculating power of a new study impacting the current evidence base
#' @param NMA a meta-analysis object
#' @param data dataset of evidence from original existing meta-analysis
#' @param n total sample size of new study
#' @param nit number of iterations/simulation to calculate the power from
#' @param inference the type of inference/impact to calculate, one of ['pvalue', 'ciwidth', 'lci', 'uci']
#' @param pow the cut-off level of the desired impact (e.g. 0.05 if a p-value of <=0.05 is desired from the updated meta-analysis)
#' @param measure type of meta-analytic outcome, one of ['OR', 'RR', 'RD', 'MD', 'SMD']
#' @param recalc whether or not new trials need to be simulated again. If FALSE, then trials need to be simulated again, if TRUE, then the power can be recalculated based on the same trials, but using different cutoffs 
#' @param plot_ext whether the results are going to be used within the metapowplot function
#' @return list containing the following
#'  - 'simdata': Data from updated meta-analyses from each simulation
#'  - 'power': Estimated power for each sample size
#'  - 'CI_lower': lower 95% confidence interval for estimated power for each sample size
#'  - 'CI_upper': upper 95% confidence interval for estimated power for each sample size
#'  - 'sim_study': Data from each simulated trial
metapow <- function(NMA, data, n, nit, inference, pow, measure, recalc = FALSE, plot_ext = NA) {  
  
  # create empty list elements
  power <- data.frame(Fixed = NA, Random = NA)
  CI_lower <- data.frame(Fixed = NA, Random = NA)
  CI_upper <- data.frame(Fixed = NA, Random = NA)
  sim.inference <- data.frame(Fixed = rep(x = NA, times = nit), Random = rep(x = NA, times = nit))
  
  # simulate new trials and conduct an updated meta-analysis for each trial
  if (recalc == 'FALSE') {
    # initialise objects
    sim_study <- data.frame(estimate.fixed = rep(NA, nit), st_err.fixed = rep(NA, nit), estimate.rand = rep(NA, nit), st_err.rand = rep(NA, nit)) 
    sims <- data.frame(Fixed.p = rep(x = NA, times = nit), Fixed.lci = rep(x = NA, times = nit), Fixed.uci = rep(x = NA, times = nit),
                       Random.p = rep(x = NA, times = nit), Random.lci = rep(x = NA, times = nit), Random.uci = rep(x = NA, times = nit))
    # run for fixed-effects
    print("Running fixed-effects iterations:")
    progress_bar = txtProgressBar(min = 0, max = nit, style = 1, char = " = ")
    for (i in 1:nit) {
      # obtain new data, add to existing data, and re-run MA
      new <- metasim(es = NMA$MA.Fixed$beta, tausq = 0, var = (NMA$MA.Fixed$se)^2, model = 'fixed', n = n, data = data, measure = measure)
      newMA <- rerunMA(new = new, data = data, model = 'fixed', n = n, measure = measure)
      # Collect data about simulated data (for OR, RR, on log scale)
      sim_study$estimate.fixed[i] <- newMA$data$yi[nrow(data)+1]
      sim_study$st_err.fixed[i] <- sqrt(newMA$data$vi[nrow(data)+1])
      # Collect inference information
      sims$Fixed.p[i] <- newMA$pval
      if (measure == 'OR' | measure == 'RR') {
        sims$Fixed.lci[i] <- exp(newMA$ci.lb)
        sims$Fixed.uci[i] <- exp(newMA$ci.ub)
      } else {
        sims$Fixed.lci[i] <- newMA$ci.lb
        sims$Fixed.uci[i] <- newMA$ci.ub
      }
      setTxtProgressBar(progress_bar, value = i)
    }
    close(progress_bar)
    # run for random-effects
    print("Running random-effects iterations:")
    progress_bar = txtProgressBar(min = 0, max = nit, style = 1, char = " = ")
      for (i in 1:nit) {
        # obtain new data, add to existing data, and re-run MA
        new <- metasim(es = NMA$MA.Random$beta, tausq = NMA$MA.Random$tau2, var = (NMA$MA.Random$se)^2, model = 'random', n = n, data = data, measure = measure)
        newMA <- rerunMA(new = new, data = data, model = 'random', n = n, measure = measure)
        # Collect data about simulated data (for OR, RR, on log scale)
        sim_study$estimate.rand[i] <- newMA$data$yi[nrow(data)+1]
        sim_study$st_err.rand[i] <- sqrt(newMA$data$vi[nrow(data)+1])
        # Collect inference information
        sims$Random.p[i] <- newMA$pval
        if (measure == 'OR' | measure == 'RR') {
          sims$Random.lci[i] <- exp(newMA$ci.lb)
          sims$Random.uci[i] <- exp(newMA$ci.ub)
        } else {
          sims$Random.lci[i] <- newMA$ci.lb
          sims$Random.uci[i] <- newMA$ci.ub
        }
        setTxtProgressBar(progress_bar, value = i)
      }
    close(progress_bar)
    
    # save data to be used if the impact options change and simulations are not needed to be redone
    if (is.na(plot_ext)) {
      write.table(sims, file = 'sims.txt', sep = "\t", row.names = FALSE) 
      write.table(sim_study, file = 'sim_study.txt', sep = "\t", row.names = FALSE)
    } else {
      write.table(sims, file = paste('sims', plot_ext, '.txt', sep = ""), sep = "\t", row.names = FALSE)  # extra option needed for metapowplot
      write.table(sim_study, file = paste('sim_study', plot_ext, '.txt', sep = ""), sep = "\t", row.names = FALSE)
    }
  }
  
  # read in simulated data (needed for when recalc is TRUE and the previous data is needed)
  if (is.na(plot_ext)) {
    sims <- read.table('sims.txt', sep = "\t", header = TRUE)
    sim_study <- read.table('sim_study.txt', sep = "\t", header = TRUE)
  } else {
    sims <- read.table(paste('sims', plot_ext, '.txt', sep = ""), sep = "\t", header = TRUE)
    sim_study <- read.table(paste('sim_study', plot_ext, '.txt', sep = ""), sep = "\t", header = TRUE)
  }
  
  # For each simulation, ascertain whether the updated meta-analysis met the desired impact
  for (i in 1:nit) {
    if (inference == 'pvalue') {
      sim.inference$Fixed[i] <- sims$Fixed.p[i] < pow
      sim.inference$Random[i] <- sims$Random.p[i] < pow
    } else if (inference == 'ciwidth') {
      sim.inference$Fixed[i] <- sims$Fixed.uci[i]-sims$Fixed.lci[i] < pow
      sim.inference$Random[i] <- sims$Random.uci[i]-sims$Random.lci[i] < pow
    } else if (inference == 'lci') {
      sim.inference$Fixed[i] <- sims$Fixed.lci[i] > pow
      sim.inference$Random[i] <- sims$Random.lci[i] > pow
    } else if (inference == 'uci') {
      sim.inference$Fixed[i] <- sims$Fixed.uci[i] < pow
      sim.inference$Random[i] <- sims$Random.uci[i] < pow
    }
  }
  
  # Calculate the power based on the proportion of simulations that met the desired impact (i.e. proportion that say 'TRUE')
  power_results <- prop.test(sum(sim.inference$Fixed), nit) 
  power$Fixed <- power_results$estimate
  CI_lower$Fixed <- power_results$conf.int[1]
  CI_upper$Fixed <- power_results$conf.int[2]
  power_results <- prop.test(sum(sim.inference$Random), nit)
  power$Random <- power_results$estimate
  CI_lower$Random <- power_results$conf.int[1]
  CI_upper$Random <- power_results$conf.int[2]
  return(list(simdata = sims, power = power, CI_lower = CI_lower, CI_upper = CI_upper, sim_study = sim_study))
}


#' Function for calculating power results for multiple sample sizes
#' @param SampleSizes a vector of the (total) sample sizes for which power was calculated
#' @param NMA a meta-analysis object
#' @param data dataset of evidence from original existing meta-analysis
#' @param nit number of iterations/simulation to calculate the power from
#' @param inference the type of inference/impact to calculate, one of ['pvalue', 'ciwidth', 'lci', 'uci']
#' @param pow the cut-off level of the desired impact (e.g. 0.05 if a p-value of <=0.05 is desired from the updated meta-analysis)
#' @param measure type of meta-analytic outcome, one of ['OR', 'RR', 'RD', 'MD', 'SMD']
#' @param recalc whether or not new trials need to be simulated again. If FALSE, then trials need to be simulated again, if TRUE, then the power can be recalculated based on the same trials, but using different cutoffs 
#' @param updateProgress needed for updating the user on the progress of the simulations
#' @return dataset of power results (power estimate plus confidence interval) for each sample size
metapow_multiple <- function(SampleSizes, NMA, data, nit, inference, pow, measure, recalc = FALSE, updateProgress = NULL) { 
  
  # Initialise
  PowerData <- data.frame(SampleSize = rep(SampleSizes, 2), Model = c(rep("Fixed-effects", length(SampleSizes)), rep("Random-effects", length(SampleSizes))), Estimate = NA, CI_lower = NA, CI_upper = NA)
  
  # Calculate power for each sample size
  for (i in 1:length(SampleSizes)) {
    results <- metapow(NMA = NMA, data = data, n = SampleSizes[i], nit = nit, inference = inference, pow = pow, measure = measure, recalc = recalc, plot_ext = i)
    PowerData$Estimate[i] <- results$power$Fixed*100
    PowerData$Estimate[i+length(SampleSizes)] <- results$power$Random*100
    PowerData$CI_lower[i] <- results$CI_lower$Fixed*100
    PowerData$CI_lower[i+length(SampleSizes)] <- results$CI_lower$Random*100
    PowerData$CI_upper[i] <- results$CI_upper$Fixed*100
    PowerData$CI_upper[i+length(SampleSizes)] <- results$CI_upper$Random*100
    print(paste("Simulation", i, "of", length(SampleSizes), "complete"))
    if (is.function(updateProgress)) {
      text <- paste0("Simulation ", i, " of ", length(SampleSizes), " complete")
      updateProgress(detail = text)
    }
  }
  
  # Order dataset
  PowerData <- PowerData[order(PowerData$SampleSize), ]
  return(PowerData)
}


#' Function to plot the power results when multiple sample sizes were given
#' @param PowerData dataset of power results (estimate plus 95% CI)
#' @param ModelOpt whether to display the fixed effect, random effects results, or both
#' @param SampleSizes a vector of the sample sizes for which power was calculated
#' @return a ggplot object
metapowplot <- function(PowerData, ModelOpt = 'both', SampleSizes) {
  PowerData <- PowerData
  # Fixed effects only
  if (ModelOpt == 'fixed') { 
    g <- ggplot(PowerData[PowerData$Model == 'Fixed-effects', ], aes(x = SampleSize, y = Estimate)) +
      geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3, colour = 'gray70', linetype = 'blank') +
      labs(x = "Total Sample Size", y = "Power (%)", title = "Power curves (fixed-effects model)", subtitle = "with 95% confidence intervals")
  }
  # Random effects only
  if (ModelOpt == 'random') { 
    g <- ggplot(PowerData[PowerData$Model == 'Random-effects', ], aes(x = SampleSize, y = Estimate)) +
      geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3, colour = 'gray70', linetype = 'blank') +
      labs(x = "Total Sample Size", y = "Power (%)", title = "Power curves (random-effects model)", subtitle = "with 95% confidence intervals")
  }
  # Present both models
  if (ModelOpt == 'both') { 
    g <- ggplot(PowerData, aes(x = SampleSize, y = Estimate, group = Model, colour = Model, fill = Model)) +
      geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3, linetype = 'blank') +
      scale_color_manual(values = c('black', 'navy')) +
      scale_fill_manual(values = c('gray', 'skyblue')) +
      labs(x = "Total Sample Size", y = "Power (%)", title = "Power curves", subtitle = "with 95% confidence intervals")
  }
  
  # Formatting
  g <- g +
    geom_line(linewidth = 1) + geom_point(shape = 21) +
    theme_classic() +
    scale_x_continuous(limits = c(min(SampleSizes)-5, max(SampleSizes)+5), breaks = SampleSizes, expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(linetype = "solid", colour = "black"), legend.key.size = unit(1, 'cm'), legend.text = element_text(size = 10),
          panel.grid.major.y = element_line(colour = "gray80", linetype = 'dashed', size = 0.5), plot.margin = unit(c(10, 15, 10, 10), "points"), axis.text = element_text(size = 10),
          plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  return(g)
}


#' Function to obtain information needed to for the section of UI where users select their cut-off values for power calculation
#' @param type Type of impact (pvalue/ciwidth/uci/lci)
#' @param outcome Type of outcome that is being used for the meta-analysis (or/rr/rd/md)
#' @param MAFix Fixed-effects meta-analysis object
#' @param MARan Random-effects meta-analysis object
#' @return list containing the following:
#'  - 'label': The label for the UI dropdown input
#'  - 'initial': Default value for the UI numeric input
#'  - 'current': Sentence to display stating the current value of the impact type (to help users guage what's possible)
CutOffSettings <- function(type, outcome, MAFix, MARan) {
  
  # Obtain summary information from meta-analysis
  sumFix <- summary(MAFix)
  sumRan <- summary(MARan)
  
  # For each type of impact create the required elements for UI  
  if (type == 'pvalue') {
    label <- paste("P-value less than ...")
    initial <- 0.05
    current <- paste("<i>Current p-values are ", strong(round(sumFix$pval, 3)), " (FE) and ", strong(round(sumRan$pval, 3)), " (RE)</i><br>")
  } else if (type == 'ciwidth') {
    label <- paste("Width less than ...")
    initial <- 0.5
    if (outcome %in% c("OR", "RR")) {
      current <- paste("<i>Current width of 95% confidence intervals are ", strong(round(exp(sumFix$ci.ub) - exp(sumFix$ci.lb), 2)), " (FE) and ", strong(round(exp(sumRan$ci.ub) - exp(sumRan$ci.lb), 2)), " (RE)</i><br>")
    } else {
      current <- paste("<i>Current width of 95% confidence intervals are ", strong(round(sumFix$ci.ub - sumFix$ci.lb, 2)), " (FE) and ", strong(round(sumRan$ci.ub - sumRan$ci.lb, 2)), " (RE)</i><br>")
    }
  } else if (type == 'lci') {
    label <- paste("Lower bound greater than ...")
    initial <- 1.1
    if (outcome %in% c("OR", "RR")) {
      current <- paste("<i>Current lower bounds are ", strong(round(exp(sumFix$ci.lb), 2)), " (FE) and ", strong(round(exp(sumRan$ci.lb), 2)), " (RE)</i><br>")
    } else {
      current <- paste("<i>Current lower bounds are ", strong(round(sumFix$ci.lb, 2)), " (FE) and ", strong(round(sumRan$ci.lb, 2)), " (RE)</i><br>")
    }
  } else {
    label <- paste("Upper bound less than ...")
    initial <- 0.9
    if (outcome %in% c("OR", "RR")) {
      current <- paste("<i> Current upper bounds are ", strong(round(exp(sumFix$ci.ub), 2)), " (FE) and ", strong(round(exp(sumRan$ci.ub), 2)), " (RE)</i><br>")
    } else {
      current <- paste("<i> Current upper bounds are ", strong(round(sumFix$ci.ub, 2)), " (FE) and ", strong(round(sumRan$ci.ub, 2)), " (RE)</i><br>")
    }
  }
  list(label = label, initial = initial, current = current)
}