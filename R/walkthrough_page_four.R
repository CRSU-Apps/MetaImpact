#' Module UI for page 4 of the walk-through.
#' 
#' @param id ID of the module
#' @return Div for the home page
walkthrough_page_four_ui <- function(id) {
  ns <- NS(id)
  div(
    h2("Visually understanding the power calculation"),
    h4("We saw that new trials of sample sizes 250, 500, 750, and 1,000 each gave 3%, 9%, 19%, and 22% power of giving the desired impact on the new evidence base/meta-analysis (p-value of 0.15 or less). But how were those values calculated?"),
    h4(
      "Below is a funnel plot of the original studies (outcome effect (odds ratio) versus the standard error (which is related to sample size)). 
              Shaded regions (defined by contour equations) can be added to illustrate areas of the plot that would give a desired impact (e.g. defined here as an updated meta-analysis with p-value < 0.15) from including a 'new' simulated trial with the corresponding effect size and standard error. Press the button below to add these regions."
    ),
    br(),
    fluidRow(
      column(
        width = 10,
        withSpinner(
          type = 6,
          plotOutput(outputId = ns('Langan'))
        )
      ),
      column(
        width = 2,
        conditionalPanel(
          ns = ns,
          condition = "input.WalkAddSims != 0",
          selectInput(
            inputId = ns('WalkSizeChoice'),
            label = "Choose sample size",
            choices = c('250', '500', '750', '1000'),
            selected = '1000'
          ),
          checkboxInput(inputId = ns("WalkZoomSims"), label = "Zoom in on Simulations", value = FALSE)
        )
      )
    ),
    conditionalPanel(
      ns = ns,
      condition = "input.WalkAddContours == 0",
      actionButton(inputId = ns("WalkAddContours"), label = "Add Significance Regions", class = "btn-primary btn-large")
    ),
    br(),
    conditionalPanel(
      ns = ns,
      condition = "input.WalkAddContours != 0",
      h4(
        "We can now see what trial treatment effects (i.e. effect size and standard error) would lead to certain impacts on the new evidence base. Details of how the contour equations for defining these regions are calculated can be found in the paper by Langan et al ",
        a(href = "https://www.jclinepi.com/article/S0895-4356(11)00327-1/fulltext", "here.")
      ),
      h4("Now let's add onto the plot treatment effects from all 100 simulated 'new trials' of sample size 1,000 that were used for the power calculation. Press the button below to add the simulations (once loaded you may want to click the zoom option to make this clearer)."),
      conditionalPanel(
        ns = ns,
        condition = "input.WalkAddSims == 0",
        actionButton(inputId = ns("WalkAddSims"), label = "Add Simulations", class = "btn-primary btn-large")
      )
    ),
    br(),
    conditionalPanel(
      ns = ns,
      condition = "input.WalkAddSims != 0",
      h4("We can now see that of the 100 simulated new trials (iterations of the calculator), 22 of them are within the shaded region indicating the desired impact (a significant effect greater than the null effect at the 15% level, i.e. the region of darkest shading). Therefore, the power of a new study of sample size 1,000 having the desired impact is 22%."),
      h4("Why not select one of the other sample sizes tested (from the drop down menu to the right of the plot) and see how the simulated trials fall on the different regions.")
    )
  )
}

#' Module server for page 4 of the walk-through.
#' 
#' @param id ID of the module
#' @param WalkFreq Frequentist MA results for walk-through
walkthrough_page_four_server <- function(id, WalkFreq) {
  moduleServer(id, function(input, output, session) {
    
    # Create example simulation data and add to Langan plot
    
    WalkSims <- reactive({
      data <- rio::import(paste0("WalkThroughSims", input$WalkSizeChoice, ".csv"))
      return(data)
    })
    
    WalkZoomValues <- data.frame(
      xlim = list(
        S250 = log(c(0.4, 3)),
        S500 = log(c(0.5, 2.2)),
        S750 = log(c(0.5, 2.2)),
        S1000 = log(c(0.6, 2))
      ),
      ylim = list(
        S250 = c(0.2, 0.4),
        S500 = c(0.1, 0.3),
        S750 = c(0, 0.3),
        S1000 = c(0, 0.25)
      )
    )
    
    output$Langan <- renderPlot({
      extfunnel(
        SS = WalkFreq$MAdata$yi,
        seSS = WalkFreq$MAdata$sei,
        method = 'fixed',
        outcome = 'OR',
        expxticks = c(0.25, 0.5, 1, 2, 4),
        xlab = "Odds Ratio",
        legend = TRUE,
        contour = input$WalkAddContours != 0,
        sig.level = 0.15,
        sim.points = {
          if (input$WalkAddSims != 0) {
            WalkSims()
          }
        },
        xlim = {
          if (input$WalkZoomSims) {
            WalkZoomValues[[paste0('xlim.S', input$WalkSizeChoice)]]
          }
        },
        ylim = {
          if (input$WalkZoomSims) {
            WalkZoomValues[[paste0('ylim.S', input$WalkSizeChoice)]]
          }
        }
      )
    })
    
  })
}