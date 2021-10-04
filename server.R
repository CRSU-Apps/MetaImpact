# MetaImpact Server #
#-------------------#

# load libraries #
#----------------#
library(shiny)
library(netmeta)
library(sjlabelled)
library(gemtc)

# load user-written functions #
#-----------------------------#

source("MAFunctions.R",local = TRUE) 

# Server Content #
#----------------#
function(input, output, session) {

  
### Load and present default Data ###
  
  defaultD <- reactive({                 # Read in default data
    if (input$ContBin=='continuous') {
      defaultD <- read.csv("./AntiVEGF_Continuous.csv")
    } else {
      defaultD <- read.csv("./AntiVEGF_Binary.csv")
    }
    defaultD$T <- as_factor(defaultD$T)  # Factor variable to remove need for separate file of labels
    return(defaultD)
  })
  defaultRef <- "laser"                  # Default reference treatment
  
  observe({                              # populating reference treatment options
    updateSelectInput(session = session, inputId = "Reference", choices = levels(defaultD()$T), selected = defaultRef)
  })
  
  outcome <- reactive({                  # different outcome variables if continuous or binary
    if (input$ContBin=='continuous') {
      input$OutcomeCont
    } else {
      input$OutcomeBina
    }
  })
  
  output$data <- renderTable({           # Create a table which displays the raw data just uploaded by the user
    #if(is.null(data())){return()}
    #data()
    defaultD()
  })
  
  
### Summary sentence of meta-analysis ###  
  
output$SynthesisSummaryFreq <- renderText({
    paste("Expand for ", strong(input$FixRand), "-effects meta-analysis of ", strong(outcome()), "s using ", strong("frequentist"), " methodology, 
    with reference treatment ", strong(input$Reference), ".")
})
output$SynthesisSummaryBayes <- renderText({
    paste("Expand for ", strong(input$FixRand), "-effects meta-analysis of ", strong(outcome()), "s using ", strong("Bayesian"), " methodology, with vague prior ", strong(input$prior), " and 
    reference treatment ", strong(input$Reference), ".") 
})


### Run frequentist NMA ###

WideData <- reactive({               # convert long format to wide if need be
  Long2Wide(data=defaultD(),CONBI=input$ContBin)
})

Freq <- reactive({                   # Run frequentist NMA 
  FreqMA(data=WideData(), outcome=outcome(), CONBI=input$ContBin, model=input$FixRand, ref=input$Reference)
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

Bayes <- reactive({                  # Run Bayesian MA
  BayesMA(data=defaultD(), CONBI=input$ContBin, outcome=outcome(), model=input$FixRand, ref=input$Reference)
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
