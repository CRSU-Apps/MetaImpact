# MetaImpact Server #
#-------------------#

# load libraries #
#----------------#
library(shiny)
library(netmeta)

# load user-written functions #
#-----------------------------#

source("MAFunctions.R",local = TRUE) 

# Server Content #
#----------------#
function(input, output, session) {

  
### Load and present default Data ###
  
  defaultD <- reactive({
    if (input$ContBin=='continuous') {
      defaultD <- read.csv("./AntiVEGF_Continuous.csv")
    } else {
      defaultD <- read.csv("./AntiVEGF_Binary.csv")
    }
    defaultD$T <- as_factor(defaultD$T)
    return(defaultD)
  })
  defaultRef <- "laser"
  
  observe({        # populating reference treatment options
    updateSelectInput(session = session, inputId = "Reference", choices = levels(defaultD()$T), selected = defaultRef)
  })
  
  output$data <- renderTable({        # Create a table which displays the raw data just uploaded by the user
    #if(is.null(data())){return()}
    #data()
    defaultD()
  })
  
  
### Summary sentence of meta-analysis ###  
output$SynthesisSummary <- renderText({
  if (input$FreqBaye=='frequentist') {
  if (input$ContBin=='binary') {
    paste("Below are the results from a ", strong(input$FixRand), "-effects meta-analysis of ", strong(input$OutcomeBina), "s using ", strong("frequentist"), " methodology, 
    with reference treatment ", strong(input$Reference), ". To change the model options, please see the synthesis options above.")
  }
  else if (input$ContBin=='continuous') {
    paste("Below are the results from a ", strong(input$FixRand), "-effects meta-analysis of ", strong(input$OutcomeCont), "s using ", strong("frequentist"), " methodology, 
    with reference treatment ", strong(input$Reference), ". To change the model options, please see the synthesis options above.")
  }
  }
  else if (input$FreqBaye=='Bayesian') {
    if (input$ContBin=='binary') {
      paste("Below are the results from a ", strong(input$FixRand), "-effects meta-analysis of ", strong(input$OutcomeBina), "s using ", strong("Bayesian"), " methodology, with vague prior ", strong(input$prior), " and 
      reference treatment ", strong(input$Reference), ". To change the model options, please see the synthesis options above.")
    }
    else if (input$ContBin=='continuous') {
      paste("Below are the results from a ", strong(input$FixRand), "-effects meta-analysis of ", strong(input$OutcomeCont), "s using ", strong("Bayesian"), " methodology, with vague prior ", strong(input$prior), " and 
      reference treatment ", strong(input$Reference), ". To change the model options, please see the synthesis options above.")
    }
  }
})


### Run frequentist NMA ###

WideData <- reactive({               # convert long format to wide if need be
  Long2Wide(data=defaultD(),CONBI=input$ContBin)
})
outcome <- reactive({                # different outcome variables if continuous or binary
  if (input$ContBin=='continuous') {
    input$OutcomeCont
  } else {
    input$OutcomeBina
  }
})
Freq <- reactive({                   # Run frequentist NMA 
  FreqMA(data=WideData(), outcome=outcome(), CONBI=input$ContBin, model=input$FixRand, ref=input$Reference)
})
output$NetworkPlot <- renderPlot({   # Network plot
  netgraph(Freq()$MAObject, thickness = "number.of.studies", number.of.studies = TRUE, plastic=FALSE, points=TRUE, cex=1.25, cex.points=3, col.points=1, col="gray80", pos.number.of.studies=0.43,
           col.number.of.studies = "forestgreen", col.multiarm = "white", bg.number.of.studies = "black", offset=0.03)
  title("Network plot of all studies")
})
output$ForestPlot <- renderPlot({    # Forest plot
  FreqForest(NMA=Freq()$MAObject, model=input$FixRand, ref=input$Reference)
  title("Forest plot of outcomes")
})

## Double zero arms are not included in analysis - need to add warning
  
}
