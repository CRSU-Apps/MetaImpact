#' Module UI for page 5 of the walk-through.
#' 
#' @param id ID of the module
#' @return Div for the home page
walkthrough_page_five_ui <- function(id) {
  ns <- NS(id)
  div(
    h2("Random-effects versus Fixed-effects"),
    h4("The fixed-effects model assumes that every study in the meta-analysis has the same underlying treatment effect (i.e. they only vary due to random error). However, this assumption is not always suitable."),
    h4("Instead, a random-effects model assumes that every study has it's own underlying treatment effect, but that each of these underlying effects come from a common/shared distribution (i.e. variation isn't purely due to random error, but the underlying effects are 'similar')."),
    br(),
    fluidRow(
      column(
        width = 4,
        h4("In this example, when we fit a random-effects model, the between study heterogeneity (on the log-odds ratio scale) was estimated at 0.033 (a fixed-effects model forces this to equal 0). The presence of heterogeneity can infer that a random-effects model may be a better 'fit'."),
        h4("The forest plot to the right shows both the fixed-effects and random-effects results. The point estimates are very similar, but the random-effects estimate has a slightly wider confidence interval (due to the added variance from the between-study heterogeneity).")
      ),
      column(
        width = 7,
        offset = 1,
        withSpinner(
          type = 6,
          plotOutput(outputId = ns('Forest'))
        )
      )
    )
  )
}

#' Module server for page 5 of the walk-through.
#' 
#' @param id ID of the module
#' @param WalkFreq Frequentist MA results for walk-through
walkthrough_page_five_server <- function(id, WalkFreq) {
  moduleServer(id, function(input, output, session) {
    
    output$Forest <- renderPlot({
      forest.rma(
        WalkFreq$MA.Fixed,
        atransf = exp,
        ylim = -2.5,
        at = log(c(0.05, 0.25, 1, 4, 16))
      )
      addpoly(WalkFreq$MA.Random)
      title("Forest plot of studies and overall pooled estimates")
    })
    
  })
}