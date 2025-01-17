#' Module UI for page 3 of the walk-through.
#' 
#' @param id ID of the module
#' @return Div for the home page
walkthrough_page_three_ui <- function(id) {
  ns <- NS(id)
  div(
    fluidRow(
      h2("Design new trial to have impact on current evidence base"),
      h4(
        "It can be argued that if a new trial is being designed with the same treatments and outcomes, it should be designed in such a way that is has an impact on the current evidence base. 
                For example, if a current meta-analysis has a non-statistically significant result, a new trial should be designed in such a way that when it is added to the current meta-analysis (evidence base) it results in a statistically significant result."
      ),
      h4("Lets design a new trial such that when its added to the example meta-analysis, we make an impact on the resulting evidence base. The current p-value is 0.379, lets aim to have a resulting meta-analysis with p-value of 0.15 or less."),
      h4(
        "We're going to keep things simple and assume a trial with two arms with equal numbers of participants, therefore, we just need to calculate the total sample size. 
                The calculator runs many simulations, where for each iteration: a new study is simulated (that has an (underlying) effect consistent with the current meta-analysis outcome estimate); added to the meta-analysis; and the resulting 'impact' (here we're looking at the p-value) is assessed."
      ),
      h4("Press the 'run' button to start the simulations!")
    ),
    br(),
    fluidRow(
      actionButton(inputId = ns("WalkFreqCalcRun3"), label = "Run Sample Size Calculations", class = "btn-primary btn-lg")
    ),
    br(),
    fluidRow(
      conditionalPanel(
        ns = ns,
        condition = "input.WalkFreqCalcRun3 != 0",
        h4(
          "The 'power' to have the desired impact (p-value of 0.15 or less), calculated as the % of simulations when the addition of the simulated study reduced the p-value to 0.15 or less, was calculated for total sample sizes of 250, 500, 750, and 1000.
                   The number of iterations/simulations to calculate the power was set at 100 (i.e. 100 updated meta-analyses were conducted, each with a 'new trial').
                   If we had increased the number of iterations, our power estimates would be more accurate (i.e. smaller 95% CI), but be more computationally expensive."
        ),
        br(),
        h4("The power results for each sample size are available below"),
        br(),
        column(
          width = 5,
          withSpinner(
            type = 6,
            tableOutput(outputId = ns("powtable"))
          )
        ),
        column(
          width = 7,
          withSpinner(
            type = 6,
            plotOutput(outputId = ns('powplot'))
          )
        )
      )
    )
  )
}

#' Module server for page 3 of the walk-through.
#' 
#' @param id ID of the module
#' @param WalkCalcResultsData dataset of power results from the walk-through
walkthrough_page_three_server <- function(id, WalkCalcResultsData) {
  moduleServer(id, function(input, output, session) {
    
    # Create example power plots and tables
    
    output$powplot <- renderPlot({
      metapowplot(PowerData = WalkCalcResultsData, ModelOpt = 'fixed', SampleSizes = c(250, 500, 750, 1000))
    })
    
    output$powtable <- renderTable({
      powdata <- WalkCalcResultsData[WalkCalcResultsData$Model == 'Fixed-effects', c(1, 3:5)]
      names(powdata) <- c("Total Sample Size", "Power estimate (%)", "Lower 95% CI bound", "Upper 95% CI bound")
      powdata
    }, digits = 1)
    
  })
}