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
    defaultRef <- "laser"
    return(defaultD)
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
    paste("Below are the results from a ", strong(input$FixRand), "-effects meta-analysis of ", strong(input$OutcomeBina), "s using ", strong("frequentist"), " methodology. 
    To change the model options, please see the synthesis options above.")
  }
  else if (input$ContBin=='continuous') {
    paste("Below are the results from a ", strong(input$FixRand), "-effects meta-analysis of ", strong(input$OutcomeCont), "s using ", strong("frequentist"), " methodology. 
    To change the model options, please see the synthesis options above.")
  }
  }
  else if (input$FreqBaye=='Bayesian') {
    if (input$ContBin=='binary') {
      paste("Below are the results from a ", strong(input$FixRand), "-effects meta-analysis of ", strong(input$OutcomeBina), "s using ", strong("Bayesian"), " methodology with vague prior ", strong(input$prior), ". 
    To change the model options, please see the synthesis options above.")
    }
    else if (input$ContBin=='continuous') {
      paste("Below are the results from a ", strong(input$FixRand), "-effects meta-analysis of ", strong(input$OutcomeCont), "s using ", strong("Bayesian"), " methodology with vague prior ", strong(input$prior), ". 
    To change the model options, please see the synthesis options above.")
    }
  }
})


### Run frequentist NMA ###

WideData <- reactive({
  Long2Wide(data=defaultD(),CONBI=input$ContBin)
})
outcome <- reactive({
  if (input$ContBin=='continuous') {
    input$OutcomeCont
  } else {
    input$OutcomeBina
  }
})
Freq <- reactive({
  FreqMA(data=WideData(), outcome=outcome(), CONBI=input$ContBin, model=input$FixRand, ref=defaultRef)
})


output$test <- renderPrint({
  Freq()$MAObject
})  
  
}
