# MetaImpact Server #
#-------------------#

# load libraries #
#----------------#
library(shiny)
library(netmeta)
library(sjlabelled)
library(gemtc)
library(tidyverse)

# load user-written functions #
#-----------------------------#

source("MAFunctions.R",local = TRUE) 

# Server Content #
#----------------#
function(input, output, session) {

  
### Load and present Data ###
  
  data <- reactive({                     # Read in user or default data
    file <- input$data             
    if (is.null(file)) {
      if (input$ChooseExample=='continuousEx') {
        data <- read.csv("./AntiVEGF_Continuous.csv")
      } else {
        data <- read.csv("./AntiVEGF_Binary.csv")
      }
    } else {
    data <- read.table(file = file$datapath, sep =",", header=TRUE, stringsAsFactors = FALSE, quote="\"")
    }
    cols <- grep("^T", names(data), value=TRUE)
    data[cols] <- lapply(data[cols], factor)  # factor variables for treatment columns
    levels <- levels(as_vector(data[cols]))
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
  
  observe({                              # populating reference treatment options
    updateSelectInput(session = session, inputId = "Reference", choices = data()$levels, selected = reference())
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
  outputOptions(output, "ContBin", suspendWhenHidden=FALSE)
  
  outcome <- reactive({                  # different outcome variables if continuous or binary
    if (ContBin()=='continuous') {
      input$OutcomeCont
    } else {
      input$OutcomeBina
    }
  })
  
  
### Summary sentence of meta-analysis ###  
  # Need to change to be reactive to the results rather than inputs#
  
output$SynthesisSummaryFreq <- renderText({
    paste("Results for ", strong(input$FixRand), "-effects meta-analysis of ", strong(outcome()), "s using ", strong("frequentist"), " methodology, 
    with reference treatment ", strong(input$Reference), ".")
})
output$SynthesisSummaryBayes <- renderText({
    paste("Results for ", strong(input$FixRand), "-effects meta-analysis of ", strong(outcome()), "s using ", strong("Bayesian"), " methodology, with vague prior ", strong(input$prior), " and 
    reference treatment ", strong(input$Reference), ".") 
})


### Run frequentist NMA ###

WideData <- reactive({               # convert long format to wide if need be
  Long2Wide(data=data()$data)
})

observeEvent( input$FreqRun, {      # reopen panel when a user re-runs analysis
  updateCollapse(session=session, id="FreqID", open="Frequentist Analysis")
}) 

Freq <- eventReactive( input$FreqRun, {                   # Run frequentist NMA 
  FreqMA(data=WideData(), outcome=outcome(), CONBI=ContBin(), model=input$FixRand, ref=input$Reference)
})
output$NetworkPlotF <- renderPlot({   # Network plot
  netgraph(Freq()$MAObject, thickness = "number.of.studies", number.of.studies = TRUE, plastic=FALSE, points=TRUE, cex=1.25, cex.points=3, col.points=1, col="gray80", pos.number.of.studies=0.43,
           col.number.of.studies = "forestgreen", col.multiarm = "white", bg.number.of.studies = "black", offset=0.03)
  title("Network plot of all studies")
})
output$ForestPlotF <- renderPlot({    # Forest plot
  FreqForest(NMA=Freq()$MAObject, model=input$FixRand, ref=input$Reference)
  title("Forest plot of outcomes")
})

## Double zero arms are not included in analysis - need to add warning

### Run Bayesian NMA ###

LongData <- reactive({               # convert wide format to long if need be
  Wide2Long(data=data()$data)
})

observeEvent( input$BayesRun, {                           # reopen panel when a user re-runs analysis
  updateCollapse(session=session, id="BayesID", open="Bayesian Analysis")
})                                                        

Bayes <- eventReactive( input$BayesRun, {                 # Run Bayesian MA
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
