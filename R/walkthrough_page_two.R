#' Module UI for page 2 of the walk-through.
#' 
#' @param id ID of the module
#' @return Div for the home page
walkthrough_page_two_ui <- function(id) {
  ns <- NS(id)
  div(
    h2("Fixed-effects meta-analysis (frequentist framework)"),
    h4("The forest plot below summarises the current evidence base, under a fixed-effects model and frequentist framework."),
    h4("Currently, the meta-analysis is showing an improvement in visual acuity when receiving RANI, but this is not statistically significant (i.e. the odds ratio 95% confidence interval (indicated by diamond at the bottom of the plot) crosses the line of no effect)."),
    br(),
    withSpinner(
      type = 6,
      plotOutput(outputId = ns('Forest'))
    )
  )
}

#' Module server for page 2 of the walk-through.
#' 
#' @param id ID of the module
#' @param WalkData Data for the walk-through
#' @return WalkFreq - the frequentist analysis for walk-through
walkthrough_page_two_server <- function(id, WalkData) {
  moduleServer(id, function(input, output, session) {
    
    # Create example forest plot
    
    WalkData <- SwapTrt(CONBI = 'binary', data = Long2Wide(WalkData), trt = 'RANI')
    WalkFreq <- FreqPair(data = WalkData, outcome = 'OR', model = 'both', CONBI = 'binary')  #conduct frequentist MA
    
    output$Forest <- renderPlot({
      metafor::forest(WalkFreq$MA.Fixed, atransf = exp, at = log(c(0.05, 0.25, 1, 4, 16)))
      title("Forest plot of studies with overall estimate from fixed-effects model")
    })
    
    return(WalkFreq)
    
  })
}