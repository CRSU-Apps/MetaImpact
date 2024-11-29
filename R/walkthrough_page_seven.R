#' Module UI for page 7 of the walk-through.
#' 
#' @param id ID of the module
#' @return Div for the home page
walkthrough_page_seven_ui <- function(id) {
  ns <- NS(id)
  div(
    h2("Bayesian versus Frequentist framework"),
    h4("Up until now, this walk-through was using a frequentist framework. But would there be any difference in using a Bayesian framework?"),
    br(),
    h4("Often, researchers like to use a Bayesian framework as it is more flexible towards more complex models."),
    h4("Below you can see the forest plots for our example random-effects models under a frequentist and Bayesian framework - the estimates are very similar."),
    h4("With regards to meta-analysis, a main difference between these frameworks is how the between-study heterogeneity is modelled. Under a frequentist framework, it is modelled with a single number. However, under a Bayesian framework, it can be modelled with a distribution to represent the uncertainty in the estimation of the heterogeneity (which can be considerable)."),
    h4("This will have a knock-on effect on the sample size calculations - an effect that could be argued to be more accurate."),
    br(),
    h5("Unfortunately, MetaImpact doesn't currently have Bayesian functionality for the Calculator element, however, this is in development."),
    br(),
    fluidRow(
      align = 'center',
      column(
        width = 6,
        h4("Frequentist"),
        withSpinner(
          type = 6,
          plotOutput(outputId = ns('ForestFreq'))
        )
      ),
      column(
        width = 6,
        h4("Bayesian"),
        withSpinner(
          type = 6,
          plotOutput(outputId = ns('ForestBayes'))
        )
      )
    )
  )
}

#' Module server for page 7 of the walk-through.
#' 
#' @param id ID of the module
#' @param WalkFreq Frequentist MA results for walk-through
walkthrough_page_seven_server <- function(id, WalkFreq) {
  moduleServer(id, function(input, output, session) {
    
    # Create example frequentist and bayesian forest plots
    
    output$ForestFreq <- renderPlot({
      metafor::forest(WalkFreq$MA.Random, atransf = exp, at = log(c(0.05, 0.25, 1, 4, 16)))
      title("Forest plot of studies with overall estimate from fixed-effects model")
    })
    
    load("BayesForestRand.rdata")
    
    output$ForestBayes <- renderPlot({
      BayesRandomForest
    })
    
  })
}