# MetaImpact Server #
#-------------------#

# load libraries #
#----------------#
library(shiny)

# load user-written functions #
#-----------------------------#

# Server Content #
#----------------#
function(input, output, session) {
  
# Summary sentence of meta-analysis #  
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
  
  
}
