#' Module UI for page 6 of the walk-through.
#' 
#' @param id ID of the module
#' @return Div for the home page
walkthrough_page_six_ui <- function(id) {
  ns <- NS(id)
  div(
    h2("How does the random-effects model affect the sample size calculations?"),
    h4("A natural follow-on question is how does the choice of moving from fixed-effects to random-effects affect the power calculations from the sample size calculator."),
    br(),
    actionButton(inputId = ns("WalkFreqCalcRun6"), label = "Run Sample Size Calculations", class = "btn-primary btn-lg"),
    br(),
    conditionalPanel(
      ns = ns,
      condition = "input.WalkFreqCalcRun6 != 0",
      fluidRow(
        column(
          width = 3,
          h4(
            "The power is much lower when using a random-effects model! A primary reason for this reduction is that when confidence intervals/p-values are calculated, they include both within- and between-study variation - this is what leads to the wider confidence intervals.
                    In turn, it is then 'harder' to move the pooled treatment effects to the desired levels of statistical significance. Even with the largest study sizes, the between-study variation is still present and driving this 'conundrum'."
          ),
          h4(
            "You can find out more statistical details about this in the paper by Sutton et al ",
            a(href = "https://onlinelibrary.wiley.com/doi/10.1002/sim.2704", "here.")
          ),
          br(),
          h4("An extra element is that the simulated 'new trials' behave slightly differently under the random-effects model."),
          h4(
            "When simulating a new trial, the calculator uses parameters from the current meta-analysis, this includes whether it was fitted under a fixed- or random-effects model. 
                    As such, the 'new trials' are allowed to 'vary' more under the random-effects model, to mirror the assumption of each trial having their own underlying study effect."
          ),
          conditionalPanel(
            ns = ns,
            condition = "input.WalkFreqRandomSims == 0",
            actionButton(inputId = ns("WalkFreqRandomSims"), label = "Let's see with our example...", class = "btn-primary btn-large")
          )
        ),
        column(
          width = 4,
          align = 'center',
          offset = 1,
          h4("Fixed-effects results"),
          withSpinner(
            type = 6,
            tableOutput(outputId = ns("fixpowtable"))
          ),
          withSpinner(
            type = 6,
            plotOutput(outputId = ns("fixpowplot")),
          )
        ),
        column(
          width = 4,
          align = 'center',
          h4("Random-effects results"),
          withSpinner(
            type = 6,
            tableOutput(outputId = ns("ranpowtable"))
          ),
          withSpinner(
            type = 6,
            plotOutput(outputId = ns("ranpowplot"))
          )
        )
      )
    ),
    conditionalPanel(
      ns = ns,
      condition = "input.WalkFreqRandomSims != 0",
      fluidRow(
        column(
          width = 3,
          h4("This can be seen in the funnel plots to the right. The simulated studies (for sample size 1,000) are more spread out (in terms of resulting odds ratio) under the random-effects model than the fixed-effects."),
          h4("In the random-effects model, the predictive interval is also plotted which represents the likely result of a new study. This is the same as the confidence interval under the fixed-effects model as any extra variablility due to between-study differences is set to 0.")
        ),
        column(
          width = 4,
          align = 'center',
          offset = 1,
          h4("Fixed-effects results"),
          withSpinner(
            type = 6,
            plotOutput(outputId = ns("LanganFix"))
          )
        ),
        column(
          width = 4,
          align = 'center',
          h4("Random-effects results"),
          withSpinner(
            type = 6,
            plotOutput(outputId = ns("LanganRan"))
          )
        )
      )
    )
  )
}

#' Module server for page 6 of the walk-through.
#' 
#' @param id ID of the module
#' @param WalkCalcResultsData dataset of power results from the walk-through
#' @param WalkFreq Frequentist MA results for walk-through
walkthrough_page_six_server <- function(id, WalkCalcResultsData, WalkFreq) {
  moduleServer(id, function(input, output, session) {
    
    # Create power results for fixed and random effects
    
    output$fixpowtable <- renderTable({
      powdata <- WalkCalcResultsData[WalkCalcResultsData$Model == 'Fixed-effects', c(1, 3:5)]
      names(powdata) <- c("Total Sample Size", "Power estimate (%)", "Lower 95% CI bound", "Upper 95% CI bound")
      powdata
    }, digits = 1)
    
    output$fixpowplot <- renderPlot({
      metapowplot(PowerData = WalkCalcResultsData, ModelOpt = 'fixed', SampleSizes = c(250, 500, 750, 1000))
    })
    
    output$ranpowtable <- renderTable({
      powdata <- WalkCalcResultsData[WalkCalcResultsData$Model == 'Random-effects', c(1, 3:5)]
      names(powdata) <- c("Total Sample Size", "Power estimate (%)", "Lower 95% CI bound", "Upper 95% CI bound")
      powdata
    }, digits = 1)
    
    output$ranpowplot <- renderPlot({
      metapowplot(PowerData = WalkCalcResultsData, ModelOpt = 'random', SampleSizes = c(250, 500, 750, 1000))
    })
    
    # Create Langan plot for illustrating differences between fixed and random
    
    WalkSims1000 <- rio::import("WalkThroughSims1000.csv")
    
    output$LanganFix <- renderPlot({
      extfunnel(
        SS = WalkFreq$MAdata$yi,
        seSS = WalkFreq$MAdata$sei,
        method = 'fixed',
        outcome = 'OR',
        expxticks = c(0.25, 0.5, 1, 2, 4),
        xlab = "Odds Ratio",
        legend = TRUE,
        sim.points = WalkSims1000,
        xlim = log(c(0.5, 2.5)),
        ylim = c(0, 0.25)
      )
    })
    
    output$LanganRan <- renderPlot({
      extfunnel(
        SS = WalkFreq$MAdata$yi,
        seSS = WalkFreq$MAdata$sei,
        method = 'random',
        outcome = 'OR',
        expxticks = c(0.25, 0.5, 1, 2, 4),
        xlab = "Odds Ratio",
        legend = TRUE,
        sim.points = WalkSims1000,
        pred.interval = TRUE,
        xlim = log(c(0.5, 2.5)),
        ylim = c(0, 0.25)
      )
    })
    
  })
}