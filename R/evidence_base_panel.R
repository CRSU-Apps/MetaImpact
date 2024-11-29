#' Module UI for 'evidence base' panel.
#' 
#' @param id ID of the module
#' @return Div for the home page
evidence_base_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    column(
      width = 5,
      align = 'center',
      bsCollapse(
        id = ns("EvidenceBase"),
        open = "Current Evidence Base",
        bsCollapsePanel(
          title = "Current Evidence Base",
          style = 'primary',
          h6("This panel presents the current evidence base from which the sample size calculations are based on. If you wish to change this, please alter the synthesis options above accordingly."),
          withSpinner(
            type = 6,
            plotOutput(outputId = ns("EvBase"))
          ),
          div(title = "As Bayesian functionality is not yet available, only frequentist results are shown.",
              radioButtons(
              inputId = ns("EvBase_choice"),
              label = NULL,
              choices = c(
                "Frequentist MA" = "freq",
                "Bayesian MA" = "bayes"
              ),
              selected = 'freq',
              inline = TRUE
            )
          ),
          fluidRow(
            div(
              style = "display: inline-block;",
              downloadButton(outputId = ns('evbase_download'), label = "Download forest plot")
            ),
            div(
              style = "display:inline-block; width: 10px;",
              br()
            ),
            div(
              style = "display: inline-block;",
              radioButtons(inputId = ns('evbase_choice'), label = NULL, choices = c('pdf', 'png'), inline = TRUE)
            )
          )
        )
      )
    )
  )
}

#' Module server for 'evidence base' panel.
#' 
#' @param id ID of the module
#' @param freqpair Reactive object of frequentist analysis
#' @return Input for choice of evidence base
evidence_base_panel_server <- function(id, freqpair) {     
  moduleServer(id, function(input, output, session) {
    
    # Disable radio buttons whilst waiting to add Bayesian functionality
    shinyjs::disable(id = "EvBase_choice")
    
    # Forest plot of current evidence base #
    output$EvBase <- renderPlot({
      if (freqpair()$MA$MA.Fixed$measure %in% c('OR', 'RR')) {
        forest.rma(freqpair()$MA$MA.Fixed, atransf = exp, ylim = -2.5)
        addpoly(freqpair()$MA$MA.Random)
      } else {
        forest.rma(freqpair()$MA$MA.Fixed, ylim = -2.5)
        addpoly(freqpair()$MA$MA.Random)
      }
      title("Forest plot of studies and overall pooled estimates")
    })
  
    # Download #
    output$evbase_download <- downloadHandler(
      filename = function() {
        paste0("EvidenceBase.", input$evbase_choice)
      },
      content = function(file) {
        if (input$evbase_choice == 'pdf') {
          pdf(file = file)
        } else {
          png(file = file)
        }
        if (freqpair()$MA$MA.Fixed$measure %in% c('OR', 'RR')) {
          forest.rma(freqpair()$MA$MA.Fixed, atransf = exp, ylim = -2.5)
          addpoly(freqpair()$MA$MA.Random)
        } else {
          forest.rma(freqpair()$MA$MA.Fixedz, ylim = -2.5)
          addpoly(freqpair()$MA$MA.Random)
        }
        title("Forest plot of studies and overal pooled estimates")
        dev.off()
      }
    )
    
    return(reactive({ input$EvBase_choice }))
    
  })
}