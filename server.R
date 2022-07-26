# MetaImpact Server #
#-------------------#

#----------------#
# load libraries #
#----------------#
library(netmeta)
library(sjlabelled)
library(gemtc)
library(tidyverse)
library(metafor)
library(ggplot2)
library(tidyr)
library(stringr)
library(tidybayes)
library(dplyr)
library(ggridges)
library(glue)
library(forcats)
library(rstan)
library(MetaStan)
library(purrr)

#-----------------------------#
# load user-written functions #
#-----------------------------#

source("MAFunctions.R",local = TRUE) 
source("SampleSizeFunctions.R", local=TRUE)
source("ForestFunctions.R", local=TRUE)


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
    p("This feature is not ready yet within the Bayesian framework. Please ", tags$strong("choose frequentist.")),
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

  
### Load and present Data ###
  #-----------------------#
  
  data <- reactive({                     # Read in user or default data
    file <- input$data             
    if (is.null(file)) {
      if (input$Pairwise_NMA=='FALSE') {
        if (input$ChooseExample=='continuousEx') {
          data <- read.csv("./AntiVEGF_Continuous.csv")
        } else {
          data <- read.csv("./AntiVEGF_Binary.csv")
        }
      } else {
        if (input$ChooseExample=='continuousEx') {
          data <- read.csv("./AntiVEGF_Continuous_Pairwise.csv")
        } else {
          data <- read.csv("./AntiVEGF_Binary_Pairwise.csv")
        }
      }
    } else {
    data <- read.table(file = file$datapath, sep =",", header=TRUE, stringsAsFactors = FALSE, quote="\"")
    }
    levels <- levels(as_vector(lapply(data[grep("^T", names(data), value=TRUE)], factor)))  # extract treatment names/levels
    return(list(data=data, levels=levels))
  })
  
  reference <- reactive({               # Select default reference treatment
    file <- input$data
    if (is.null(file)) {
      return("laser")
    } else {
      return(data()$levels[1])
    }
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
  
  observe({                              # populating reference treatment options
    updateSelectInput(session = session, inputId = "Reference", choices = data()$levels, selected = reference())
  })
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

  
### NEED TO SORT OUT/DECIDE WHAT THE 'RUN' BUTTONS DO (pairwise complete) ###  
    
  
### Summary sentence of meta-analysis ###
  #-----------------------------------#
  
MAText <- reactive({ #different phrases
  if (input$Pairwise_NMA=='TRUE') {
    paste(strong("Pairwise"), "meta-analysis")
  } else {
    paste(strong("NMA"))
  }
}) 
FreqSummaryText <- eventReactive( input$FreqRun, {
  paste("Results for ", strong(input$FixRand), "-effects ", MAText(), " of ", strong(outcome()), "s using ", strong("frequentist"), " methodology, 
    with reference treatment ", ifelse(input$Pairwise_NMA=='TRUE',paste(strong(input$Pair_Ctrl)),paste(strong(input$Reference))), ".", sep="")
})
output$SynthesisSummaryFreq <- renderText({FreqSummaryText()})
BayesSummaryText <- eventReactive( input$BayesRun, {
  paste("Results for ", strong(input$FixRand), "-effects ", MAText(), " of ", strong(outcome()), "s using ", strong("Bayesian"), " methodology, with vague prior ", strong(input$prior), " and 
    reference treatment ", ifelse(input$Pairwise_NMA=='TRUE', paste(strong(input$Pair_Ctrl)), paste(strong(input$Reference))), ".", sep="")
})
output$SynthesisSummaryBayes <- renderText({BayesSummaryText()})


### Run frequentist Pairwise MA ###
  #-----------------------------#

WideData <- reactive({               # convert long format to wide if need be
  Long2Wide(data=data()$data)
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
    line4<-paste("Between study standard-reviation: ")
  }
  line3<-paste(strong("Heterogeneity results"))
  line4<-paste(line4, round(sqrt(sum$tau2),3), "; I-squared: ", round(sum$I2,1), "%; P-value for testing heterogeneity: ", round(sum$QEp,3), sep="")
  line5<-paste(strong("Model fit statistics"))
  line6<-paste("AIC: ", round(sum$fit.stats[3,1],2), "; BIC: ", round(sum$fit.stats[4,1],2), sep="")
  HTML(paste(line0,line1, line2, line3, line4, line5, line6, sep = '<br/>'))
}

freqpair <- eventReactive( input$FreqRun, {         # run frequentist pairwise MA and obtain plots etc.
  if (input$Pairwise_NMA==TRUE) {
    information <- list()
    information$MA <- FreqPair(data=WideData(), outcome=outcome(), model='both', CONBI=ContBin(), trt=input$Pair_Trt)
    if (input$FixRand=='fixed') {                   # Forest plot
      if (outcome()=='OR' | outcome()=='RR') {
        information$Forest <- {
          forest(information$MA$MA.Fixed, atransf=exp)
          title("Forest plot of studies with overall estimate from fixed-effects model")}
      } else {
        information$Forest <- {
          forest(information$MA$MA.Fixed)
          title("Forest plot of studies with overall estimate from fixed-effects model")}
      }
      information$Summary <- PairwiseSummary_functionF(outcome(),information$MA$MA.Fixed)
    } else if (input$FixRand=='random') {
      if (outcome()=='OR' | outcome()=='RR') {
        information$Forest <- {
          forest(information$MA$MA.Random, atransf=exp)
          title("Forest plot of studies with overall estimate from random-effects model")}
      } else {
        information$Forest <- {
          forest(information$MA$MA.Random)
          title("Forest plot of studies with overall estimate from random-effects model")}
      }
      information$Summary <- PairwiseSummary_functionF(outcome(),information$MA$MA.Random)
    }
    information
  }
})

output$ForestPlotPairF <- renderPlot({      # Forest plot
  freqpair()$Forest
})

output$SummaryTableF <- renderUI({          # Summary table
  freqpair()$Summary
})





### Run frequentist NMA ###
  #---------------------#

freqnma <- eventReactive( input$FreqRun, {                   # Run frequentist NMA 
  if (input$Pairwise_NMA==FALSE) {
    NoNMA()
    #FreqNMA(data=WideData(), outcome=outcome(), CONBI=ContBin(), model=input$FixRand, ref=input$Reference)
  }
})
output$NetworkPlotF <- renderPlot({   # Network plot
  netgraph(freqnma()$MAObject, thickness = "number.of.studies", number.of.studies = TRUE, plastic=FALSE, points=TRUE, cex=1.25, cex.points=3, col.points=1, col="gray80", pos.number.of.studies=0.43,
           col.number.of.studies = "forestgreen", col.multiarm = "white", bg.number.of.studies = "black", offset=0.03)
  title("Network plot of all studies")
})
output$ForestPlotNMAF <- renderPlot({    # Forest plot
  FreqNMAForest(NMA=freqnma()$MAObject, model=input$FixRand, ref=input$Reference)
  title("Forest plot of outcomes")
})

## Double zero arms are not included in analysis - need to add warning






### Run Bayesian Pairwise MA ###
  #--------------------------#

LongData <- reactive({               # convert wide format to long if need be
  Wide2Long(data=data()$data)
})

observeEvent( input$BayesRun, {                           # reopen panel when a user re-runs analysis
  updateCollapse(session=session, id="BayesID", open="Bayesian Analysis")
})  


PairwiseSummary_functionB <- function(outcome, MA.Model, model) {
  line0<-paste(strong("Results"))
  line1<-paste("Number of studies: ", nrow(MA.Model$data_wide), sep="") 
  line2<-paste("Pooled estimate: ", round(MA.Model$fit_sum['theta', 1],2), " (95% CI: ", round(MA.Model$fit_sum['theta', 4],2), " to ", round(MA.Model$fit_sum['theta', 8],2), ")", sep="") # already exponentiated where needed within BayesPair function
  if (model=='random') {
    if (outcome=='OR') {
      line3<-paste("Between study standard-deviation (log-odds scale): ", round(MA.Model$fit_sum['tau[1]',1],3), " (95% CI: ", round(MA.Model$fit_sum['tau[1]',4],3), " to ", round(MA.Model$fit_sum['tau[1]',8],3), ")", sep="")
    } else if (outcome=='RR') {
      line3<-paste("Between study standard-deviation (log-probability scale): ", round(MA.Model$fit_sum['tau[1]',1],3), " (95% CI: ", round(MA.Model$fit_sum['tau[1]',4],3), " to ", round(MA.Model$fit_sum['tau[1]',8],3), ")", sep="")
    } else {
      line3<-paste("Between study standard-deviation: ", round(MA.Model$fit_sum['tau[1]',1],3), " (95% CI: ", round(MA.Model$fit_sum['tau[1]',4],3), " to ", round(MA.Model$fit_sum['tau[1]',8],3), ")", sep="")
    }
  } else {
    line3<-paste("For fixed models, between study standard-deviation is set to 0.")
  }
  line4<-paste(strong("Model fit assessment"))
  line5<-paste("Rhat: ", round(MA.Model$Rhat.max,2), sep="")
  line6<-paste(strong("Trace plot"))
  HTML(paste(line0,line1, line2, line3, line4, line5, line6, sep = '<br/>'))
}

bayespair <- eventReactive( input$BayesRun, {         # run Bayesian pairwise MA and obtain plots etc.
  if (input$Pairwise_NMA==TRUE) {
    information <- list()
    information$MA <- BayesPair(CONBI=ContBin(), data=WideData(), trt=input$Pair_Trt, ctrl=input$Pair_Ctrl, outcome=outcome(), chains=input$chains, iter=input$iter, warmup=input$burn, model='both', prior=input$prior)
    if (input$FixRand=='fixed') {                   
      information$Forest <- {               
        g <- BayesPairForest(information$MA$MAdata, outcome=outcome(), model='fixed')
        g + ggtitle("Forest plot of studies with overall estimate from fixed-effects model") +
          theme(plot.title = element_text(hjust = 0.5))
      }
      information$Summary <- PairwiseSummary_functionB(outcome(),information$MA$MA.Fixed,input$FixRand)
      information$Trace <- {
        g <- stan_trace(information$MA$MA.Fixed$fit, pars="theta")
        g + ggtitle("Trace plot of the pooled estimate over iterations") +
          theme(plot.title = element_text(hjust = 0.5))
      }
    } else if (input$FixRand=='random') {
      information$Forest <- {               
        g <- BayesPairForest(information$MA$MAdata, outcome=outcome(), model='random')
        g + ggtitle("Forest plot of studies with overall estimate from random-effects model") +
          theme(plot.title = element_text(hjust = 0.5))
      }
      information$Summary <- PairwiseSummary_functionB(outcome(),information$MA$MA.Random,input$FixRand)
      information$Trace <- {
        g <- stan_trace(information$MA$MA.Random$fit, pars=c("theta","tau"))
        g + ggtitle("Trace plot of the pooled estimate and between-study SD over iterations") +
          theme(plot.title = element_text(hjust = 0.5))
      }
    }
    information
  }
})


output$ForestPlotPairB <- renderPlot({      # Forest plot
  bayespair()$Forest
})

output$SummaryTableB <- renderUI({          # Summary table
  bayespair()$Summary
})

output$TracePlot <- renderPlot({            # Trace plot
  bayespair()$Trace
})




### Run Bayesian NMA ###
#------------------#

Bayes <- eventReactive( input$BayesRun & input$Pairwise_NMA=='FALSE', {                 # Run Bayesian NMA
  NoNMA()
  #BayesMA(data=LongData(), CONBI=ContBin(), outcome=outcome(), model=input$FixRand, ref=input$Reference, prior=input$prior)
})


output$NetworkPlotB <- renderPlot({  # Network plot
  plot(Bayes()$Network)
  title("Network plot of all studies")
})

output$ForestPlotB <- renderPlot({   # Forest plot
  forest(Bayes()$RelEffects, digits=3)
  title("Forest plot of outcomes")
})

output$TauB <- renderText({          # Between-study standard deviation
  TauDesc(ResultsSum=Bayes()$ResultsSum, outcome=outcome(), model=input$FixRand)
})

output$DICB <- renderTable({         # DIC
  Bayes()$DIC
}, digits=3, rownames=TRUE, colnames=FALSE)




### Frequentist Pairwise Sample Size Calculations ###
  #-----------------------------------------------#

# Forest plot of current evidence base
output$EvBase <- renderPlot({
  if (input$EvBase_choice!='freq') {
    NoBayesian()
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

# Interactive help boxes for the calculator settings
observeEvent(input$calc_help,
             introjs(session, options = list("showBullets"="false", "showProgress"="true", 
                                             "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip"))
)

# Function for checking if recalc option needs to be TRUE or FALSE (TRUE if only the impact type and/or cut-off have changed)
Recalc <- reactiveVal('FALSE')  # initialise
### Create set of constantly updated reactive values of cached inputs
tmpInputs <- reactiveValues()  # initialised
tmp_freqpair <- eventReactive( input$FreqRun, {    # without this, the evidence synthesis tab was getting upset (forest plot)         
  if (input$Pairwise_NMA==TRUE) {
    FreqPair(data=WideData(), outcome=outcome(), model='both', CONBI=ContBin(), trt=input$Pair_Trt) }
  })
inputCache <- reactive(list(sample=input$samplesizes, NMA=tmp_freqpair(), nit=input$its))
source("InputCaches.R", local=TRUE)  # (non elegant) code for caching inputs - updating tmpInputs
# compare previous input settings to decide on recalc option
observeEvent(input$CalcRun, {
  if (input$CalcRun>1) {
    eval(parse(text = paste0("if (setequal(tmpInputs[['", input$CalcRun, "']]$sample,tmpInputs[['", input$CalcRun-1, "']]$sample) & setequal(tmpInputs[['", input$CalcRun, "']]$NMA,tmpInputs[['", input$CalcRun-1, "']]$NMA) & setequal(tmpInputs[['", input$CalcRun, "']]$nit,tmpInputs[['", input$CalcRun-1, "']]$nit)) {Recalc('TRUE')} else {Recalc('FALSE')}", sep=""))) #compare previous two sets of inputs
}})

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
    list1$data <- metapow_multiple(SampleSizes=list1$sample_sizes, NMA=freqpair()$MA, data=WideData(), nit=input$its, inference=input$impact_type, pow=input$cutoff, measure=outcome(), recalc=as.logical(Recalc()), updateProgress=updateProgress)
  } else if (length(list1$sample_sizes)==1) {
    list1$singleresult <- metapow(NMA=freqpair()$MA, data=WideData(), n=list1$sample_sizes, nit=input$its, inference=input$impact_type, pow=input$cutoff, measure=outcome(), recalc=as.logical(Recalc()))
  }
  list1
}})

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



### Links ###
  #-------#

observeEvent(input$link_to_tabpanel_evsynth, {
  updateTabsetPanel(session, "MetaImpact", "Evidence Synthesis")
})


### Interactive UI ###
  #----------------#

# Cut-off information #

CutOffSettings <- function(type, outcome, MAFix, MARan) {
  sumFix <- summary(MAFix)
  sumRan <- summary(MARan)
  if (type=='pvalue') {
    label <- paste("P-value less than ...")
    initial <- 0.05
    current <- paste("<i>Current p-values are ", strong(round(sumFix$pval,3)), " (FE) and ", strong(round(sumRan$pval,3)), " (RE)</i>")}
  else if (type=='ciwidth') {
    label <- paste("Width less than ...")
    initial <- 0.5
    if (outcome %in% c("OR","RR")) {
      current <- paste("<i>Current width of 95% confidence intervals are ", strong(round(exp(sumFix$ci.ub) - exp(sumFix$ci.lb), 2)), " (FE) and ", strong(round(exp(sumRan$ci.ub) - exp(sumRan$ci.lb), 2)), " (RE)</i>")
      } else {current <- paste("<i>Current width of 95% confidence intervals are ", strong(round(sumFix$ci.ub - sumFix$ci.lb, 2)), " (FE) and ", strong(round(sumRan$ci.ub - sumRan$ci.lb, 2)), " (RE)</i>")}}
  else if (type=='lci') {
    label <- paste("Lower bound greater than ...")
    initial <- 1.1
    if (outcome %in% c("OR","RR")) {
      current <- paste("<i>Current lower bounds are ", strong(round(exp(sumFix$ci.lb), 2)), " (FE) and ", strong(round(exp(sumRan$ci.lb), 2)), " (RE)</i>")
      } else {current <- paste("<i>Current lower bounds are ", strong(round(sumFix$ci.lb, 2)), " (FE) and ", strong(round(sumRan$ci.lb, 2)), " (RE)</i>")}}
  else {
    label <- paste("Upper bound less than ...")
    initial <- 0.9
    if (outcome %in% c("OR","RR")) {
      current <- paste("<i> Current upper bounds are ", strong(round(exp(sumFix$ci.ub), 2)), " (FE) and ", strong(round(exp(sumRan$ci.ub), 2)), " (RE)</i>")
      } else {current <- paste("<i> Current upper bounds are ", strong(round(sumFix$ci.ub, 2)), " (FE) and ", strong(round(sumRan$ci.ub, 2)), " (RE)</i>")}}
  list(label=label, initial=initial, current=current)
}

output$CutOff <- renderUI({
  cutsettings <- CutOffSettings(input$impact_type, outcome(), freqpair()$MA$MA.Fixed, freqpair()$MA$MA.Random)
  tagList(
    introBox(numericInput('cutoff', label = paste(cutsettings$label), value=cutsettings$initial),
             data.step=4, data.intro="Depending on which type of impact has been chosen, please choose a specific cut-off value for which you define as 'impactful' (e.g. a p-value of less than 0.05)."),
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
#observeEvent(input$CalcRun, {   # need to add criteria that its if one/multiple hasn't changed
#  updateCollapse(session, "Calculator", open = , close = )   # remember open/close status of tabs when re-running calculator (if within same one/multiple state)
#})




  
}
