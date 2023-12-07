# MetaImpact Server


#----------------#
# Server Content #
#----------------#
function(input, output, session) {
  source("DownloadButtons.R", local=TRUE)  # needs to be within server
  
#------------------#
# Warning messages #
#------------------#
BadSampleSizes <- function(){
  showModal(modalDialog(
    title = "Unsuitable Sample Sizes",
    easyClose = FALSE,
    p("The total sample size is assuming two arms of equal size. Therefore, please enter ", tags$strong("even integers.")),
    br(),
    modalButton("Close warning"),
    footer = NULL
  ))
}
NoBayesian <- function(){
  showModal(modalDialog(
    title = "Feature not yet available",
    easyClose = FALSE,
    p("Calculating the power of new studies with set sample size(s) is not ready yet within the Bayesian framework. Please ", tags$strong("choose frequentist.")),
    br(),
    modalButton("Close warning"),
    footer = NULL
  ))
}
NoNMA <- function(){
  showModal(modalDialog(
    title = "Feature not yet available",
    easyClose = FALSE,
    p("Synthesising evidence with an NMA is not quite ready yet. Please ", tags$strong("choose pairwise.")),
    br(),
    modalButton("Close warning"),
    footer = NULL
  ))
}
NoRandomContours <- function(){
  showModal(modalDialog(
    title = "Feature not yet available",
    easyClose = FALSE,
    p("Drawing the significance contours for a random-effects meta-analysis is not quite ready yet. Please either ", tags$strong("choose fixed-effects"), " or ", tags$strong("uncheck contours option.")),
    br(),
    modalButton("Close warning"),
    footer = NULL
  ))
}
FixedPredInt <- function(){
  showModal(modalDialog(
    title = "Option combination not applicable",
    easyClose = FALSE,
    p("Within a fixed-effects model, the between-study heterogeneity is set to zero, therefore a 95% predictive interval would be equivalent to the 95% confidence interval (represented by the width of the diamond) and is not a plottable option."),
    br(),
    modalButton("Close warning"),
    footer = NULL
  ))
}
SigContourOnly <- function(){
  showModal(modalDialog(
    title = "Feature not yet available",
    easyClose = FALSE,
    p("Currently, the contours on the extended funnel plot are only for when the desired impact of the new evidence base is related to levels of p-values."),
    p("Therefore, if you wish to plot the simulated 'new trials' from the power calculations, please either: ", tags$ol(tags$li("ensure that the ", strong("type of impact"), " is set to ", strong("Significant p-value"), ", or"), tags$li("uncheck the ", strong("contours"), " option"))),
    p("If you do not wish to plot the simulated 'new trials', please ", strong("uncheck"), " the plot simulated trials option."),
    br(),
    modalButton("Close warning"),
    footer = NULL
  ))
}
DiffSigValues <- function(){
  showModal(modalDialog(
    title = "Significance values don't match",
    easyClose = FALSE,
    p("If you wish to plot the simulated 'new trials' on top of the extended funnel plot with contours, then the significance level/cut-off value needs to match."),
    p("Please ensure that the ", strong("Sig. level for contours"), " plot option is set to the same value as the ", strong("cut-off calculator"), " option."),
    p("If you do not wish to plot the simulated 'new trials' with the contour option of the extended funnel plot, please ", strong("uncheck"), " the plot simulated trials and/or contour option."),
    br(),
    modalButton("Close warning"),
    footer = "If this error appears after changing the impact type to 'significant p-value' due to a previous warning message, and your significance levels/cut-off values match, please ignore."
  ))
}
NoPlotMultipleSampleSizes <- function(){
  showModal(modalDialog(
    title = "Feature not yet available",
    easyClose = FALSE,
    p("Plotting the simulated 'new trials' of multiple sample sizes is not yet available. Please either ", tags$strong("uncheck 'plot simulated trials'"), " option or ", tags$strong("specify one sample size.")),
    br(),
    modalButton("Close warning"),
    footer = NULL
  ))
}



### Walk-through demonstration ###
  #---------------------------#

## Code for stepping through the multiple pages ##
rv <- reactiveValues(page = 1)

observe({
  shinyjs::toggleState(id = "prevBtn", condition = rv$page > 1)
  shinyjs::toggleState(id = "nextBtn", condition = rv$page < 8)
  shinyjs::hide(selector = ".page")     # hides previously loaded page
  shinyjs::show(paste0("page", rv$page))
})

navPage <- function(direction) {
  rv$page <- rv$page + direction
}

observeEvent(input$prevBtn, navPage(-1))
observeEvent(input$nextBtn, navPage(1))

## Dataset ##

WalkData <- read.csv("./AntiVEGF_Binary_Pairwise.csv")

## Page 2 content ##

WalkData <- SwapTrt(CONBI='binary', data=Long2Wide(WalkData), trt='RANI')
WalkFreq <- FreqPair(data=WalkData, outcome='OR', model='both', CONBI='binary')  #conduct frequentist MA

output$page2Forest <- renderPlot({
  metafor::forest(WalkFreq$MA.Fixed, atransf=exp, at=log(c(0.05, 0.25, 1, 4, 16)))
  title("Forest plot of studies with overall estimate from fixed-effects model")
})

## Page 3 content ##

WalkCalcResultsData <- read.csv("WalkThroughResults.csv")

output$page3powplot <- renderPlot({    
  metapowplot(PowerData=WalkCalcResultsData, ModelOpt='fixed', SampleSizes=c(250, 500, 750, 1000))
})

output$page3powtable <- renderTable({
  powdata <- WalkCalcResultsData[WalkCalcResultsData$Model=='Fixed-effects',c(1,3:5)]
  names(powdata) <- c("Total Sample Size", "Power estimate (%)", "Lower 95% CI bound", "Upper 95% CI bound")
  powdata
}, digits=1)

## Page 4 content ##

WalkSims <- reactive({
  data <- read.csv(paste("WalkThroughSims", input$WalkSizeChoice, ".csv", sep=""))
  return(data)
})

WalkZoomValues <- data.frame(xlim = list(S250 = log(c(0.4,3)), S500 = log(c(0.5,2.2)), S750 = log(c(0.5,2.2)), S1000 = log(c(0.6,2))),
                             ylim = list(S250 = c(0.2, 0.4), S500 = c(0.1,0.3), S750 = c(0,0.3), S1000 = c(0,0.25)))

output$page4Langan <- renderPlot({
  extfunnel(SS=WalkFreq$MAdata$yi, seSS=WalkFreq$MAdata$sei, method='fixed', outcome='OR',
           expxticks=c(0.25,0.5,1,2,4), xlab="Odds Ratio", legend=TRUE,
           contour = ifelse(input$WalkAddContours != 0, TRUE, FALSE), sig.level = 0.15,
           sim.points = { if (input$WalkAddSims != 0) {WalkSims()}},
           xlim = { if (input$WalkZoomSims==TRUE) {eval(parse(text=paste('WalkZoomValues$xlim.S',input$WalkSizeChoice, sep='')))}},
           ylim = { if (input$WalkZoomSims==TRUE) {eval(parse(text=paste('WalkZoomValues$ylim.S',input$WalkSizeChoice, sep='')))}})
})

## Page 5 content ##

#summary(WalkFreq$MA.Random)$tau2

output$page5Forest <- renderPlot({
  forest.rma.CN(WalkFreq$MA.Fixed, WalkFreq$MA.Random, atransf=exp, at=log(c(0.05, 0.25, 1, 4, 16)))
  title("Forest plot of studies and overall pooled estimates")
})

## Page 6 content ##


output$page6fixpowtable <- renderTable({
  powdata <- WalkCalcResultsData[WalkCalcResultsData$Model=='Fixed-effects',c(1,3:5)]
  names(powdata) <- c("Total Sample Size", "Power estimate (%)", "Lower 95% CI bound", "Upper 95% CI bound")
  powdata
}, digits=1)

output$page6fixpowplot <- renderPlot({
  metapowplot(PowerData=WalkCalcResultsData, ModelOpt='fixed', SampleSizes=c(250, 500, 750, 1000))
})

output$page6ranpowtable <- renderTable({
  powdata <- WalkCalcResultsData[WalkCalcResultsData$Model=='Random-effects',c(1,3:5)]
  names(powdata) <- c("Total Sample Size", "Power estimate (%)", "Lower 95% CI bound", "Upper 95% CI bound")
  powdata
}, digits=1)

output$page6ranpowplot <- renderPlot({
  metapowplot(PowerData=WalkCalcResultsData, ModelOpt='random', SampleSizes=c(250, 500, 750, 1000))
})

WalkSims1000 <- read.csv("WalkThroughSims1000.csv")

output$page6LanganFix <- renderPlot({
  extfunnel(SS=WalkFreq$MAdata$yi, seSS=WalkFreq$MAdata$sei, method='fixed', outcome='OR',
                       expxticks=c(0.25,0.5,1,2,4), xlab="Odds Ratio", legend=TRUE,
                       sim.points=WalkSims1000,
                       xlim = log(c(0.5, 2.5)), ylim=c(0, 0.25))
})

output$page6LanganRan <- renderPlot({
  extfunnel(SS=WalkFreq$MAdata$yi, seSS=WalkFreq$MAdata$sei, method='random', outcome='OR',
                       expxticks=c(0.25,0.5,1,2,4), xlab="Odds Ratio", legend=TRUE,
                       sim.points=WalkSims1000, pred.interval=TRUE,
                       xlim = log(c(0.5, 2.5)), ylim=c(0, 0.25))
})

## page 7 content ##

output$page7ForestFreq <- renderPlot({
  metafor::forest(WalkFreq$MA.Random, atransf=exp, at=log(c(0.05, 0.25, 1, 4, 16)))
  title("Forest plot of studies with overall estimate from fixed-effects model")
})


load("BayesForestRand.rdata")

output$page7ForestBayes <- renderPlot({
  BayesRandomForest
})



### Load and present Data ###
  #-----------------------#

  data <- reactive({                     # Read in user or default data
    file <- input$data
    if (is.null(file)) {
      if (input$ChooseExample=='continuousEx') {
        data <- read.csv("./AntiVEGF_Continuous_Pairwise.csv")
      } else {
        data <- read.csv("./AntiVEGF_Binary_Pairwise.csv")
      }
    } else {
    data <- read.table(file = file$datapath, sep =",", header=TRUE, stringsAsFactors = FALSE, quote="\"")
    }
    levels <- levels(as_vector(lapply(data[grep("^T", names(data), value=TRUE)], factor)))  # extract treatment names/levels
    return(list(data=data, levels=levels))
  })

  pairwise_ref <- function(trt_ctrl) {   # pairwise options
    if (trt_ctrl=='trt') {
      ref <- reactive({
        file <- input$data
        if (is.null(file)) {
          return("BEVA")
        } else {
          return(data()$levels[1])
        }
      })
    } else {
      ref <- reactive({
        file <- input$data
        if (is.null(file)) {
          return("RANI")
        } else {
          return(data()$levels[2])
        }
      })
    }
    return(ref())
  }

  observe({
    updateSelectInput(session=session, inputId = "Pair_Trt", choices = data()$levels, selected = pairwise_ref(trt_ctrl='trt'))
  })
  observe({
    updateSelectInput(session=session, inputId = "Pair_Ctrl", choices = data()$levels, selected = pairwise_ref(trt_ctrl='ctrl'))
  })

  output$data <- renderTable({           # Create a table which displays the raw data just uploaded by the user
    data()$data
  })


  ContBin <- reactive({           # automatically detect if continuous or binary
    if (max(grepl("^Mean", names(data()$data)))==TRUE) {
      return('continuous')
    } else if (max(grepl("^R", names(data()$data)))==TRUE) {
      return ('binary')
    }
  })
  output$ContBin <- renderText({
    ContBin()
  })
  outputOptions(output, "ContBin", suspendWhenHidden=FALSE) #needed for UI options, but doesn't need displaying itself

  outcome <- reactive({                  # different outcome variables if continuous or binary
    if (ContBin()=='continuous') {
      input$OutcomeCont
    } else {
      input$OutcomeBina
    }
  })




### Summary sentence of meta-analysis ###
  #-----------------------------------#

FreqSummaryText <- eventReactive( input$FreqRun, {
  paste("Results for ", strong(input$FixRand), "-effects ", strong("Pairwise"), " meta-analysis of ", strong(outcome()), "s using ", strong("frequentist"), " methodology,
    with reference treatment ", strong(input$Pair_Ctrl), ".", sep="")
})
output$SynthesisSummaryFreq <- renderText({FreqSummaryText()})
BayesSummaryText <- eventReactive( input$BayesRun, {
  paste("Results for ", strong(input$FixRand), "-effects ", strong("Pairwise"), " meta-analysis of ", strong(outcome()), "s using ", strong("Bayesian"), " methodology, with vague prior ", strong(input$prior), " and
    reference treatment ", strong(input$Pair_Ctrl), ".", sep="")
})
output$SynthesisSummaryBayes <- renderText({BayesSummaryText()})


### Run frequentist Pairwise MA ###
  #-----------------------------#

WideData <- reactive({               # convert long format to wide if need be (and ensure trt and ctrl are the right way round)
  SwapTrt(CONBI=ContBin(), data=Long2Wide(data=data()$data), trt=input$Pair_Trt)
})

observeEvent( input$FreqRun, {      # reopen panel when a user re-runs analysis
  updateCollapse(session=session, id="FreqID", open="Frequentist Analysis")
})

PairwiseSummary_functionF <- function(outcome, MA.Model) {
  sum <- summary(MA.Model)
  line0<-paste(strong("Results"))
  line1<-paste("Number of studies: ", sum$k, sep="")
  if (outcome=="OR") {
    line2<-paste("Pooled estimate: ", round(exp(sum$b),2), " (95% CI: ", round(exp(sum$ci.lb),2), " to ", round(exp(sum$ci.ub),2), "); p-value: ", round(sum$pval, 3), sep="")
    line4<-paste("Between study standard-deviation (log-odds scale): ")
  } else if (outcome=="RR") {
    line2<-paste("Pooled estimate: ", round(exp(sum$b),2), " (95% CI: ", round(exp(sum$ci.lb),2), " to ", round(exp(sum$ci.ub),2), "); p-value: ", round(sum$pval, 3), sep="")
    line4<-paste("Between study standard-deviation (log-probability scale): ")
  } else {
    line2<-paste("Pooled estimate: ", round(sum$b,2), " (95% CI: ", round(sum$ci.lb,2), " to ", round(sum$ci.ub,2), "); p-value: ", round(sum$pval, 3), sep="")
    line4<-paste("Between study standard-deviation: ")
  }
  line3<-paste(strong("Heterogeneity results"))
  line4<-paste(line4, round(sqrt(sum$tau2),3), "; I-squared: ", round(sum$I2,1), "%; P-value for testing heterogeneity: ", round(sum$QEp,3), sep="")
  HTML(paste(line0,line1, line2, line3, line4, sep = '<br/>'))
}
PairwiseModelFit_functionF <- function(MA.Model) {
  sum <- summary(MA.Model)
  HTML(paste("AIC: ", round(sum$fit.stats[3,1],2), "; BIC: ", round(sum$fit.stats[4,1],2), sep=""))
}

freqpair <- eventReactive( input$FreqRun, {         # run frequentist pairwise MA and obtain plots etc.
  information <- list()
  information$MA <- FreqPair(data=WideData(), outcome=outcome(), model='both', CONBI=ContBin())
  if (input$FixRand=='fixed') {                   # Forest plot
    if (outcome()=='OR' | outcome()=='RR') {
      information$Forest <- {
        metafor::forest(information$MA$MA.Fixed, atransf=exp)
        title("Forest plot of studies with overall estimate from fixed-effects model")}
    } else {
      information$Forest <- {
        metafor::forest(information$MA$MA.Fixed)
        title("Forest plot of studies with overall estimate from fixed-effects model")}
    }
    information$Summary <- PairwiseSummary_functionF(outcome(),information$MA$MA.Fixed)
    information$ModelFit <- PairwiseModelFit_functionF(information$MA$MA.Fixed)
  } else if (input$FixRand=='random') {
    if (outcome()=='OR' | outcome()=='RR') {
      information$Forest <- {
        metafor::forest(information$MA$MA.Random, atransf=exp)
        title("Forest plot of studies with overall estimate from random-effects model")}
    } else {
      information$Forest <- {
        metafor::forest(information$MA$MA.Random)
        title("Forest plot of studies with overall estimate from random-effects model")}
    }
    information$Summary <- PairwiseSummary_functionF(outcome(),information$MA$MA.Random)
    information$ModelFit <- PairwiseModelFit_functionF(information$MA$MA.Random)
  }
  information
})

output$ForestPlotPairF <- renderPlot({      # Forest plot
  freqpair()$Forest
})

output$SummaryTableF <- renderUI({          # Summary table
  freqpair()$Summary
})

output$ModelFitF <- renderUI({              # Model fit statistics
  freqpair()$ModelFit
})






LongData <- reactive({               # convert wide format to long if need be
  Wide2Long(data=data()$data)
})

observeEvent( input$BayesRun, {                           # reopen panel when a user re-runs analysis
  #NoBayesian()
  updateCollapse(session=session, id="BayesID", open="Bayesian Analysis")
})


PairwiseSummary_functionB <- function(outcome, MA.Model, model) {   # MA.Model has to have MAData, MA.Fixed and MA.Random
  line0<-paste(strong("Results"))
  line1<-paste("Number of studies: ", nrow(MA.Model$MA.Fixed$data_wide), sep="") # same for fixed or random
  if (model=='random') {
    line2<-paste("Pooled estimate: ", round(MA.Model$MAdata[MA.Model$MAdata$Study=='RE Model','est'],2), " (95% CI: ", round(MA.Model$MAdata[MA.Model$MAdata$Study=='RE Model','lci'],2), " to ", round(MA.Model$MAdata[MA.Model$MAdata$Study=='RE Model','uci'],2), ")", sep="") # already exponentiated where needed within BayesPair function
    if (outcome=='OR') {
      line3<-paste("Between study standard-deviation (log-odds scale): ")
    } else if (outcome=='RR') {
      line3<-paste("Between study standard-deviation (log-probability scale): ")
    } else {
      line3<-paste("Between study standard-deviation: ")
    }
    line3<-paste(line3, round(MA.Model$MA.Random$fit_sum['tau[1]',1],3), " (95% CI: ", round(MA.Model$MA.Random$fit_sum['tau[1]',4],3), " to ", round(MA.Model$MA.Random$fit_sum['tau[1]',8],3), ")", sep="")
  } else {
    line2<-paste("Pooled estimate: ", round(MA.Model$MAdata[MA.Model$MAdata$Study=='FE Model','est'],2), " (95% CI: ", round(MA.Model$MAdata[MA.Model$MAdata$Study=='FE Model','lci'],2), " to ", round(MA.Model$MAdata[MA.Model$MAdata$Study=='FE Model','uci'],2), ")", sep="") # already exponentiated where needed within BayesPair function
    line3<-paste("For fixed models, between study standard-deviation is set to 0.")
  }
  HTML(paste(line0,line1, line2, line3, sep = '<br/>'))
}
PairwiseModelFit_functionB <- function(MA.Model) {
  HTML(paste("Rhat: ", round(MA.Model$Rhat.max,2), sep=""))
}

bayespair <- eventReactive( input$BayesRun, {         # run Bayesian pairwise MA and obtain plots etc.
  #NoBayesian()
  information <- list()
  information$MA <- BayesPair(CONBI=ContBin(), data=WideData(), trt=input$Pair_Trt, ctrl=input$Pair_Ctrl, outcome=outcome(), chains=input$chains, iter=input$iter, warmup=input$burn, model='both', prior=input$prior)
  if (input$FixRand=='fixed') {
    information$Forest <- {
      g <- BayesPairForest(information$MA$MAdata, outcome=outcome(), model='fixed')
      g + ggtitle("Forest plot of studies with overall estimate from fixed-effects model") +
        theme(plot.title = element_text(hjust = 0.5, size=13, face='bold'))
    }
    information$Summary <- PairwiseSummary_functionB(outcome(),information$MA,'fixed')
    information$ModelFit <- PairwiseModelFit_functionB(information$MA$MA.Fixed)
    information$Trace <- {
      g <- stan_trace(information$MA$MA.Fixed$fit, pars="theta")
      g + theme(legend.position='none', aspect.ratio = 0.45, axis.title=element_text(size=10,face="bold")) +
        labs(y="Pooled estimate", x="Iteration")
    }
  } else if (input$FixRand=='random') {
    information$Forest <- {
      g <- BayesPairForest(information$MA$MAdata, outcome=outcome(), model='random')
      g + ggtitle("Forest plot of studies with overall estimate from random-effects model") +
        theme(plot.title = element_text(hjust = 0.5, size=13, face='bold'))
    }
    information$Summary <- PairwiseSummary_functionB(outcome(),information$MA,'random')
    information$ModelFit <- PairwiseModelFit_functionB(information$MA$MA.Random)
    information$Trace <- {
      g <- stan_trace(information$MA$MA.Random$fit, pars=c("theta","tau"))
      g + theme(legend.position='none', strip.placement = "outside", aspect.ratio=0.3, axis.title=element_text(size=10,face="bold")) +
        labs(x="Iteration") +
        facet_wrap(~parameter, strip.position='left', nrow=2, scales='free', labeller=as_labeller(c(theta = "Pooled estimate", 'tau[1]' = "Between-study SD") ) )
    }
  }
  information
})


output$ForestPlotPairB <- renderPlot({      # Forest plot
  bayespair()$Forest
})

output$SummaryTableB <- renderUI({          # Summary table
  bayespair()$Summary
})

output$ModelFitB <- renderUI({              # Model fit statistic
  bayespair()$ModelFit
})

output$TracePlot <- renderPlot({            # Trace plot
  bayespair()$Trace
})




### Pairwise Sample Size Calculations ###
  #-----------------------------------#

# Forest plot of current evidence base
output$EvBase <- renderPlot({
  if (input$EvBase_choice!='freq') {
    NoBayesian()
    #g <- BayesPairForest(bayespair()$MA$MAdata, outcome=outcome(), model='both')
    #g + ggtitle("Forest plot of studies and overall pooled estimates") +
    #  theme(plot.title = element_text(hjust = 0.5, size=13, face='bold'))
  } else {
    if (freqpair()$MA$MA.Fixed$measure %in% c('OR','RR')) {
      forest.rma.CN(freqpair()$MA$MA.Fixed, freqpair()$MA$MA.Random, atransf=exp)
    } else {
      forest.rma.CN(freqpair()$MA$MA.Fixed, freqpair()$MA$MA.Random)
    }
    title("Forest plot of studies and overall pooled estimates")
  }
})

# Settings for UI
OneOrMultiple <- eventReactive( input$CalcRun, {         # function to be used in update Collapse below
  if (grepl(';', input$samplesizes)==TRUE) {return('Power Plot of Results')}
  if (grepl(';', input$samplesizes)==FALSE) {return('Table of power results')}
})

observeEvent( input$CalcRun, {                           # reopen panel when a user re-runs calculator
  updateCollapse(session=session, id="Calculator", open=OneOrMultiple())
})

# Function for checking if recalc option needs to be TRUE or FALSE (TRUE if only the impact type and/or cut-off have changed)
Recalc <- reactiveVal(FALSE)  # initialise
### Create set of constantly updated reactive values of cached inputs
tmpInputs <- reactiveVal()  # initialised
tmp_pairwise <- reactive({
  if (input$EvBase_choice=='freq') {
    FreqPair(data=WideData(), outcome=outcome(), model='both', CONBI=ContBin()) # if I use freqpair(), then the forest plot on evidence synthesis tab doesn't load - minor bug for now (extra run-time and hope no-one changes frequentist settings without re-running)
  } else {
    bayespair()$MA
  }
})
inputCache <- reactive({
  list(
    sample=input$samplesizes,
    NMA=tmp_pairwise(),
    nit=input$its,
    FreqBayes=input$EvBase_choice
  )
})

# compare previous input settings to decide on recalc option
observeEvent(input$CalcRun, {
  #compare previous two sets of inputs
  if (!is.null(tmpInputs()) &&
      setequal(inputCache()$sample, tmpInputs()$sample) &&
      setequal(inputCache()$NMA, tmpInputs()$NMA) &&
      setequal(inputCache()$nit, tmpInputs()$nit) &&
      setequal(inputCache()$FreqBayes, tmpInputs()$FreqBayes)) {
    Recalc(TRUE)
  } else {
    Recalc(FALSE)
  }
  tmpInputs(inputCache())
})

pairwise_MA <- reactive({
  if (input$EvBase_choice=='freq') {
    freqpair()$MA
  } else {
    bayespair()$MA
  }
})

# Calculate
CalcResults <- eventReactive( input$CalcRun, {
  list1 <- list()
  list1$sample_sizes <- as.integer(unlist(str_split(input$samplesizes, ";"), use.names=FALSE)) # convert to vector of integers
  if (grepl(".", input$samplesizes, fixed=TRUE) | grepl(".", toString(list1$sample_sizes/2), fixed=TRUE)) {     # If any decimals or odds have been entered
    BadSampleSizes()
  } else {
  progress <- shiny::Progress$new() # Create a Progress object
  progress$set(message = "Running simulations", value = 0)
  on.exit(progress$close()) # Close the progress when this reactive exits (even if there's an error)
  if (length(list1$sample_sizes)>1) {  # only plot if input multiple sample sizes
    updateProgress <- function(value = NULL, detail = NULL) { #callback function to update progress where there are multiple simulations.
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() / length(list1$sample_sizes))
      }
      progress$set(value = value, detail = detail)
    }
    list1$data <- metapow_multiple(SampleSizes=list1$sample_sizes, NMA=pairwise_MA(), data=WideData(), nit=input$its, inference=input$impact_type, pow=input$cutoff, measure=outcome(), recalc=Recalc(), updateProgress=updateProgress, chains=input$chains, iter=input$iter, warmup=input$burn, prior=input$prior)
  } else if (length(list1$sample_sizes)==1) {
    list1$singleresult <- metapow(NMA=pairwise_MA(), data=WideData(), n=list1$sample_sizes, nit=input$its, inference=input$impact_type, pow=input$cutoff, measure=outcome(), recalc=Recalc(), chains=input$chains, iter=input$iter, warmup=input$burn, prior=input$prior)
  }
  list1
}})
# Bayesian takes a while doing 'something' before starting to run simulations (which also means when using recalc option, it takes longer than I think it should?)

# Results
output$powplot <- renderPlot({    # only if multiple sample sizes entered
  metapowplot(PowerData=CalcResults()$data, ModelOpt=input$powplot_options, SampleSizes=CalcResults()$sample_sizes)
})

output$powtable <- renderTable({
  powdata <- CalcResults()$data
  names(powdata) <- c("Total Sample Size", "Model", "Power estimate (%)", "Lower 95% CI bound", "Upper 95% CI bound")
  powdata
}, digits=1)

output$singleresult <- renderUI({
  HTML(paste0("<b>Fixed-effects</b>: ", CalcResults()$singleresult$power$Fixed*100, "% power (95% CI: ", round(CalcResults()$singleresult$CI_lower$Fixed*100, 1), "% to ", round(CalcResults()$singleresult$CI_upper$Fixed*100, 1), "%)<br>",
              "<b>Random-effects</b>: ", CalcResults()$singleresult$power$Random*100, "% power (95% CI: ", round(CalcResults()$singleresult$CI_lower$Random*100, 1), "% to ", round(CalcResults()$singleresult$CI_upper$Random*100, 1), "%)"))
})

# Langan Plot #

output$Langan <- renderPlot({
  if (input$Lang_method == 'random' && 'contour' %in% input$LanganOptions) {   # significance contours not available for random-effects
    NoRandomContours()
  } else if (input$Lang_method == 'fixed' && 'pred.interval' %in% input$LanganOptions) {  # Warning how predictive intervals are not of use within fixed-effects models
    FixedPredInt()
  } else if (input$plot_sims && input$impact_type != 'pvalue' &&'contour' %in% input$LanganOptions) { # the significance contours only relate to p-value impact, whereas the power calculator has other options
    SigContourOnly()
  } else if (input$plot_sims && input$impact_type == 'pvalue' && input$Lang_pvalue != input$cutoff && 'contour' %in% input$LanganOptions) { # the contour cut-offs/sig levels need to be the same
    DiffSigValues()
  } else if (input$plot_sims && length(as.integer(unlist(str_split(input$samplesizes, ";"), use.names=FALSE)))>1) {   # haven't added functionlity to Langan plot yet so plot multiple sets of sample sizes
    NoPlotMultipleSampleSizes()
  } else {
    extfunnel(SS = freqpair()$MA$MAdata$yi, seSS = freqpair()$MA$MAdata$sei, method = input$Lang_method, outcome = outcome(),
              sig.level = input$Lang_pvalue, legend = TRUE, points = TRUE,
              contour = {'contour' %in% input$LanganOptions}, summ = {'summ' %in% input$LanganOptions}, pred.interval = {'pred.interval' %in% input$LanganOptions}, plot.zero = {'plot.zero' %in% input$LanganOptions}, plot.summ = {'plot.summ' %in% input$LanganOptions},
              expxticks = {if (outcome() %in% c('OR','RR')) {c(0.25,0.5,1,2,4)}},
              sim.points = {if (input$plot_sims) {CalcResults()$singleresult$sim_study}})
    # remaining settings not in UI: contour.points, summ.pos, ylim, xlim, xticks, yticks, zero, xlab, ylab, legendpos
  }
})


### Interactive UI ###
  #----------------#


# Interactive help boxes #

steps <- reactive(data.frame(
  category=c(rep("CalcSettings",5), rep("BayesSettings",4)),
  element=c("#samplesizes", "#its", "#impact_type", "#cutoff", "#plot_sims",  "#prior", "#chains", "#iter", "#burn"),
  intro=c("This is where you specify sample sizes for which you wish to estimate power. You can enter one sample size, or multiple by separating them with a semi-colon (;). Currently, it is assumed that future designed trials have two arms of equal size.",
          "Choose how many iterations (i.e. times the algorithm is run) you wish to have per simulation (sample size). If you choose a higher number of iterations, the simulations will take longer but give more precise estimates (narrower confidence intervals), and vice versa.",
          "Making an 'impact' on the current evidence base can be done in multiple ways - choose here which method you wish to focus on (1. Having a significant p-value; 2. Having a 95% confidence interval of a certain width; 3. Having the lower bound of the 95% CI above a certain value; 4. Having the upper bound of the 95% CI below a certain value).",
          "Depending on which type of impact has been chosen, please choose a specific cut-off value for which you define as 'impactful' (e.g. a p-value of less than 0.05).",
          "Choose to plot the results from every simulated 'new study' into the extended funnel plot to visually see how the power is calculated (when viewed alongside the significance contours",
          "Choose which vague prior to use to initially model the between-study standard deviation (used for random-effects models)",
          "Choose the number of chains. A chain represents a run-through of the analysis, with each chain starting with different values to aid robustness. The results then incorporate all chains.",
          "The number of iterations to run through. A higher number of iterations is likely to lead to more robust results but does take longer.",
          "The number of iterations to 'burn' (i.e. not include in the results) at the start. In early iterations, estimated parameters are unlikely to have converged and thus are likely to give spurious results.")
))
# Calculator settings #
observeEvent(input$calc_help,
             introjs(session, options = list(steps=steps() %>% filter(category=="CalcSettings"), "showBullets"="false", "showProgress"="true",
                                             "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip"))   # IMPACT_TYPE NOT WORKING and don't know why...
)
# Bayesian settings #
observeEvent(input$bayes_help,
             introjs(session, options = list(steps=steps() %>% filter(category=="BayesSettings"), "showBullets"="false", "showProgress"="true",
                                             "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip"))
)



# Cut-off information #

CutOffSettings <- function(type, outcome, MAFix, MARan) {
  sumFix <- summary(MAFix)
  sumRan <- summary(MARan)
  if (type=='pvalue') {
    label <- paste("P-value less than ...")
    initial <- 0.05
    current <- paste("<i>Current p-values are ", strong(round(sumFix$pval,3)), " (FE) and ", strong(round(sumRan$pval,3)), " (RE)</i><br>")}
  else if (type=='ciwidth') {
    label <- paste("Width less than ...")
    initial <- 0.5
    if (outcome %in% c("OR","RR")) {
      current <- paste("<i>Current width of 95% confidence intervals are ", strong(round(exp(sumFix$ci.ub) - exp(sumFix$ci.lb), 2)), " (FE) and ", strong(round(exp(sumRan$ci.ub) - exp(sumRan$ci.lb), 2)), " (RE)</i><br>")
      } else {current <- paste("<i>Current width of 95% confidence intervals are ", strong(round(sumFix$ci.ub - sumFix$ci.lb, 2)), " (FE) and ", strong(round(sumRan$ci.ub - sumRan$ci.lb, 2)), " (RE)</i><br>")}}
  else if (type=='lci') {
    label <- paste("Lower bound greater than ...")
    initial <- 1.1
    if (outcome %in% c("OR","RR")) {
      current <- paste("<i>Current lower bounds are ", strong(round(exp(sumFix$ci.lb), 2)), " (FE) and ", strong(round(exp(sumRan$ci.lb), 2)), " (RE)</i><br>")
      } else {current <- paste("<i>Current lower bounds are ", strong(round(sumFix$ci.lb, 2)), " (FE) and ", strong(round(sumRan$ci.lb, 2)), " (RE)</i><br>")}}
  else {
    label <- paste("Upper bound less than ...")
    initial <- 0.9
    if (outcome %in% c("OR","RR")) {
      current <- paste("<i> Current upper bounds are ", strong(round(exp(sumFix$ci.ub), 2)), " (FE) and ", strong(round(exp(sumRan$ci.ub), 2)), " (RE)</i><br>")
      } else {current <- paste("<i> Current upper bounds are ", strong(round(sumFix$ci.ub, 2)), " (FE) and ", strong(round(sumRan$ci.ub, 2)), " (RE)</i><br>")}}
  list(label=label, initial=initial, current=current)
}

output$CutOff <- renderUI({
  cutsettings <- CutOffSettings(input$impact_type, outcome(), freqpair()$MA$MA.Fixed, freqpair()$MA$MA.Random)
  tagList(
    numericInput('cutoff', label = paste(cutsettings$label), value=cutsettings$initial),
    HTML(cutsettings$current)
  )
})

# Calculator Results #

SingMult <- eventReactive( input$CalcRun, {           # single or multiple sample sizes
  if (grepl(';', input$samplesizes)==TRUE) {
    return('multiple')
  } else {
    return ('single')
  }
})
output$SingMult <- renderText({
  SingMult()
})
outputOptions(output, "SingMult", suspendWhenHidden=FALSE) #needed for UI options, but doesn't need displaying itself

output$CalculatorResults <- renderUI({
  panel <- OneOrMultiple()   # ascertain which panel should be open based on whether one or multple sample sizes have been inputted
  conditionalPanel(condition = "input.CalcRun!=0", bsCollapse(id="Calculator", open=panel, multiple=TRUE,
                                                            bsCollapsePanel(title="Power Plot of Results", style='success',
                                                                            conditionalPanel(condition = "output.SingMult=='multiple'", withSpinner(plotOutput('powplot'), type=6), radioButtons('powplot_options', "", c("Fixed-effects only"='fixed', "Random-effects only"='random', "Both fixed- and random-effects"='both'), selected='both', inline=TRUE),
                                                                                             fluidRow(div(style="display: inline-block;", downloadButton('powplot_download', "Download power plot")),
                                                                                                      div(style="display:inline-block; width: 10px;", HTML("<br>")),
                                                                                                      div(style="display: inline-block;", radioButtons('powplot_choice', "", c('pdf', 'png'), inline=TRUE)))),
                                                                            conditionalPanel(condition = "output.SingMult=='single'", p("Only one sample size has been entered."))),
                                                            bsCollapsePanel(title="Table of power results", style='success',
                                                                            conditionalPanel(condition = "output.SingMult=='multiple'", withSpinner(tableOutput("powtable"), type=6), downloadButton('powtable_download', "Download (CSV)")),
                                                                            conditionalPanel(condition = "output.SingMult=='single'", withSpinner(htmlOutput("singleresult"), type=6))
                                                            )))
})
observeEvent(input$CalcRun, {
  updateRadioButtons(session, "powplot_options", selected = input$powplot_options)  # remember plot settings from before re-running calculator
})

# evidence base #
output$evbase_download <- downloadHandler(
  filename = function() {
    paste0("EvidenceBase.", input$evbase_choice)
  },
  content = function(file) {
    if (input$evbase_choice=='pdf') {pdf(file=file)}
    else {png(file=file)}
    if (input$EvBase_choice=='freq') {
      if (freqpair()$MA$MA.Fixed$measure %in% c('OR','RR')) {
        forest.rma.CN(freqpair()$MA$MA.Fixed, freqpair()$MA$MA.Random, atransf=exp)
      } else {
        forest.rma.CN(freqpair()$MA$MA.Fixed, freqpair()$MA$MA.Random)
      }
      title("Forest plot of studies and overal pooled estimates")
    }
    dev.off()
  }
)

# extended funnel plot #
output$Langan_download <- downloadHandler(
  filename = function() {
    paste0('ExtFunnelPlot.', input$langan_choice)
  }, 
  content = function(file) {
    plot <- extfunnel(SS = freqpair()$MA$MAdata$yi, seSS = freqpair()$MA$MAdata$sei, method = input$Lang_method, outcome = outcome(),
                      sig.level = input$Lang_pvalue, legend = TRUE, points = TRUE,
                      contour = {'contour' %in% input$LanganOptions}, summ = {'summ' %in% input$LanganOptions}, pred.interval = {'pred.interval' %in% input$LanganOptions}, plot.zero = {'plot.zero' %in% input$LanganOptions}, plot.summ = {'plot.summ' %in% input$LanganOptions},
                      expxticks = {if (outcome() %in% c('OR','RR')) {c(0.25,0.5,1,2,4)}},
                      sim.points = {if (input$plot_sims) {CalcResults()$singleresult$sim_study}})
    if (input$langan_choice=='png') {
      ggsave(file,plot, height=7, width=12, units="in", device="png")
    } else {
      ggsave(file,plot, height=7, width=12, units="in", device="pdf")
    }
  }
)


### Pairwise Meta-Analysis ###
#------------------------#


output$forestpairF_download <- downloadHandler(
  filename = function() {
    paste0("PairwiseAnalysis.", input$forestpairF_choice)
  },
  content = function(file) {
    if (input$forestpairF_choice=='pdf') {pdf(file=file)}
    else {png(file=file)}
    if (input$FixRand=='fixed') { 
      if (outcome()=='OR' | outcome()=='RR') {
        forest(freqpair()$MA$MA.Fixed, atransf=exp)
        title("Forest plot of studies with overall estimate from fixed-effects model")
      } else {
        forest(freqpair()$MA$MA.Fixed)
        title("Forest plot of studies with overall estimate from fixed-effects model")
      }
    } else {
      if (outcome()=='OR' | outcome()=='RR') {
        forest(freqpair()$MA$MA.Random, atransf=exp)
        title("Forest plot of studies with overall estimate from random-effects model")
      } else {
        forest(freqpair()$MA$MA.Random)
        title("Forest plot of studies with overall estimate from random-effects model")
      }
    }
    dev.off()
  }
)

output$forestpairB_download <- downloadHandler(
  filename = function() {
    paste0("PairwiseAnalysis.", input$forestpairB_choice)
  },
  content = function(file) {
    plot <- bayespair()$Forest
    if (input$forestpairB_choice=='png') {
      ggsave(file, plot, height=7, width=12, units="in", device="png")
    } else {
      ggsave(file, plot, height=7, width=12, units="in", device="pdf")
    }
  }
)

output$tracepair_download <- downloadHandler(
  filename = function() {
    paste0("PairwiseTrace.", input$tracepair_choice)
  },
  content = function(file) {
    plot <- bayespair()$Trace
    if (input$forestpairB_choice=='png') {
      ggsave(file, plot, height=7, width=12, units="in", device="png")
    } else {
      ggsave(file, plot, height=7, width=12, units="in", device="pdf")
    }
  }
)

  
}
