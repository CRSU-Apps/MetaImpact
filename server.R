# MetaImpact Server #
#-------------------#

#----------------#
# load libraries #
#----------------#
library(shiny)
library(netmeta)
library(sjlabelled)
library(gemtc)
library(tidyverse)
library(metafor)

#-----------------------------#
# load user-written functions #
#-----------------------------#

source("MAFunctions.R",local = TRUE) 

#----------------#
# Server Content #
#----------------#
function(input, output, session) {

  
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

  
### NEED TO SORT OUT/DECIDE WHAT THE 'RUN' BUTTONS DO ###  
    
  
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

freqpair <- eventReactive( input$FreqRun, {         # run frequentist pairwise MA
  if (input$Pairwise_NMA==TRUE) {
    FreqPair(data=WideData(), outcome=outcome(), model='both', CONBI=ContBin(), trt=input$Pair_Trt)
  }
})
MA.Model <- reactive({
  if (input$FixRand=='fixed') {freqpair()$MA.Fixed} else {freqpair()$MA.Random}
})

output$ForestPlotPairF <- renderPlot({      # Forest plot
  if (outcome()=='OR' | outcome()=='RR') {forest(MA.Model(), atransf=exp)} else {forest(MA.Model())}
  title("Forest plot of studies with overall pooled estimate")
})

output$SummaryTableF <- renderUI({
  if (outcome()=='OR' | outcome()=='RR') {sum <- summary(MA.Model(), atransf=exp)} else {sum <- summary(MA.Model())}
  line0<-paste(strong("Results"))
  line1<-paste("Number of studies: ", sum$k, sep="")
  if (outcome()=='OR' | outcome()=='RR') {
    line2<-paste("Pooled estimate: ", round(exp(sum$b),2), " (95% CI: ", round(exp(sum$ci.lb),2), " to ", round(exp(sum$ci.ub),2), ")", sep="")
  } else {
    line2<-paste("Pooled estimate: ", round(sum$b,2), " (95% CI: ", round(sum$ci.lb,2), " to ", round(sum$ci.ub,2), ")", sep="")
  }
  line3<-paste(strong("Heterogeneity results"))
  line4<-paste("Between study standard-devation: ", round(sqrt(sum$tau2),3), "; I-squared: ", round(sum$I2,1), "%; P-value for testing heterogeneity: ", round(sum$QEp,3), sep="")
  line5<-paste(strong("Model fit statistics"))
  line6<-paste("AIC: ", round(sum$fit.stats[3,1],2), "; BIC: ", round(sum$fit.stats[4,1],2), sep="")
  HTML(paste(line0,line1, line2, line3, line4, line5, line6, sep = '<br/>'))
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



  
}
