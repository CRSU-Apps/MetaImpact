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
    cols <- grep("^T", names(data), value=TRUE)
    data[cols] <- lapply(data[cols], factor)  # factor variables for treatment columns
    levels <- levels(as_vector(data[cols])) # obtain list of treatments
    data[cols] <- lapply(data[cols], as.character) #revert back to character variables to allow analyses to function
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
    reference treatment ", ifselse(input$Pairwise_NMA=='TRUE', paste(strong(input$Pair_Ctrl)), paste(strong(input$Reference))), ".", sep="")
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

PairwiseSummary_function <- function(outcome, MA.Model) {
  sum <- summary(MA.Model)
  line0<-paste(strong("Results"))
  line1<-paste("Number of studies: ", sum$k, sep="")
  if (outcome=='OR' | outcome=='RR') {
    line2<-paste("Pooled estimate: ", round(exp(sum$b),2), " (95% CI: ", round(exp(sum$ci.lb),2), " to ", round(exp(sum$ci.ub),2), "); p-value: ", round(sum$pval, 3), sep="")
  } else {
    line2<-paste("Pooled estimate: ", round(sum$b,2), " (95% CI: ", round(sum$ci.lb,2), " to ", round(sum$ci.ub,2), "); p-value: ", round(sum$pval, 3), sep="")
  }
  line3<-paste(strong("Heterogeneity results"))
  line4<-paste("Between study standard-devation: ", round(sqrt(sum$tau2),3), "; I-squared: ", round(sum$I2,1), "%; P-value for testing heterogeneity: ", round(sum$QEp,3), sep="")
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
      information$Summary <- PairwiseSummary_function(outcome(),information$MA$MA.Fixed)
    } else {
      if (outcome()=='OR' | outcome()=='RR') {
        information$Forest <- {
          forest(information$MA$MA.Random, atransf=exp)
          title("Forest plot of studies with overall estimate from random-effects model")}
      } else {
        information$Forest <- {
          forest(information$MA$MA.Random)
          title("Forest plot of studies with overall estimate from random-effects model")}
      }
      information$Summary <- PairwiseSummary_function(outcome(),information$MA$MA.Random)
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
    FreqNMA(data=WideData(), outcome=outcome(), CONBI=ContBin(), model=input$FixRand, ref=input$Reference)
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


### Run Bayesian NMA ###
  #------------------#

LongData <- reactive({               # convert wide format to long if need be
  Wide2Long(data=data()$data)
})

observeEvent( input$BayesRun, {                           # reopen panel when a user re-runs analysis
  updateCollapse(session=session, id="BayesID", open="Bayesian Analysis")
})                                                        

Bayes <- eventReactive( input$BayesRun & input$Pairwise_NMA=='FALSE', {                 # Run Bayesian NMA
  BayesMA(data=LongData(), CONBI=ContBin(), outcome=outcome(), model=input$FixRand, ref=input$Reference, prior=input$prior)
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
  if (input$EvBase_choice=='freq') {
    if (freqpair()$MA$MA.Fixed$measure %in% c('OR','RR')) {
      forest.rma.CN(freqpair()$MA$MA.Fixed, freqpair()$MA$MA.Random, atransf=exp)
    } else {
      forest.rma.CN(freqpair()$MA$MA.Fixed, freqpair()$MA$MA.Random)
    }
    title("Forest plot of studies and overal pooled estimates")
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

# Calulate
CalcResults <- eventReactive( input$CalcRun, {
  list1 <- list()
  list1$sample_sizes <- as_numeric(unlist(str_split(input$samplesizes, ";"), use.names=FALSE)) # convert to numeric vector
  if (length(list1$sample_sizes)>1) {  # only plot if input multiple sample sizes
    list1$plot <- metapowplot(SampleSizes=list1$sample_sizes, NMA=freqpair()$MA, data=WideData(), nit=input$its, inference=input$impact_type, pow=input$cutoff, measure=outcome(), ModelOpt='both', recalc=FALSE, regraph=FALSE)
  } else if (length(list1$sample_sizes)==1) {
    list1$singleresult <- metapow(NMA=freqpair()$MA, data=WideData(), n=list1$sample_sizes, nit=input$its, inference=input$impact_type, pow=input$cutoff, measure=outcome(), recalc=FALSE)
  }
  list1
})

# Results
output$powplot <- renderPlot({    # only if multiple sample sizes entered
  CalcResults()$plot$plot
})

output$powtable <- renderTable({
  powdata <- CalcResults()$plot$data
  names(powdata) <- c("Total Sample Size", "Model", "Power estimate (%)", "Lower 95% CI bound", "Upper 95% CI bound")
  powdata
}, digits=1)

output$singleresult <- renderUI({
  HTML(paste0("<b>Fixed-effects</b>: ", CalcResults()$singleresult$power$Fixed*100, "% power (95% CI: ", round(CalcResults()$singleresult$CI_lower$Fixed*100, 1), " to ", round(CalcResults()$singleresult$CI_upper$Fixed*100, 1), ")<br>",
              "<b>Random-effects</b>: ", CalcResults()$singleresult$power$Random*100, "% power (95% CI: ", round(CalcResults()$singleresult$CI_lower$Random*100, 1), " to ", round(CalcResults()$singleresult$CI_upper$Random*100, 1), ")"))
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
  panel <- OneOrMultiple()   # ascertain which panel should be open
  conditionalPanel(condition = "input.CalcRun!=0", bsCollapse(id="Calculator", open=panel, multiple=TRUE,  
                                                            bsCollapsePanel(title="Power Plot of Results", style='success',
                                                                            conditionalPanel(condition = "output.SingMult=='multiple'", plotOutput('powplot'), downloadButton('powplot_download', "Download (PNG)")),
                                                                            conditionalPanel(condition = "output.SingMult=='single'", p("Only one sample size has been entered."))),
                                                            bsCollapsePanel(title="Table of power results", style='success',
                                                                            conditionalPanel(condition = "output.SingMult=='multiple'", tableOutput("powtable"), downloadButton('powtable_download', "Download (CSV)")),
                                                                            conditionalPanel(condition = "output.SingMult=='single'", htmlOutput("singleresult"))
                                                            )))
})




  
}
