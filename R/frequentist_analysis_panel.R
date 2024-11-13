#' Module UI for the frequentist analysis results panel.
#' 
#' @param id ID of the module
#' @return Div for the home page
frequentist_analysis_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    fluidRow(
      p(htmlOutput(outputId = ns("SynthesisSummary"))),
      p("To change the model options, please adjust synthesis options above and re-run analysis."),
      bsCollapse(
        id = ns("FreqID"),
        open = "Frequentist Analysis",
        bsCollapsePanel(
          title = "Frequentist Analysis",
          style = 'success',
          column(
            width = 5,
            align = 'center',
            withSpinner(
              type = 6,
              htmlOutput(outputId = ns("SummaryTable"))
            ),
            fluidRow(
              div(
                style = "display: inline-block;",
                p(strong("Model fit statistics"))
              ),
              div(
                style = "display: inline-block;",
                dropMenu(
                  dropdownButton(size = 'xs', icon = icon('info')),
                  align = 'left',
                  h6("Model fit statistics"),
                  p("Akaike information criterion (AIC) and Bayesian information criterion (BIC) measure 'model performance' whilst taking into account model complexity."),
                  p("The smaller the AIC or BIC, the 'better' the model. Values are best interpreted between models rather than alone.")
                )
              )
            ),
            htmlOutput(outputId = ns("ModelFit"))
          ),
          column(
            width = 6,
            align = 'center',
            offset = 1,
            withSpinner(
              type = 6,
              plotOutput(outputId = ns("ForestPlot"))
            ),
            downloadButton(outputId = ns('forest_download'), label = "Download forest plot"),
            radioButtons(inputId = ns('forest_choice'), label = NULL, choices = c('pdf','png'), inline = TRUE)
          )
        )
      )
    )
  )
}


#' Module server for the frequentist analysis results panel.
#' 
#' @param id ID of the module
#' @param action_button Action button from calculator module for running frequentist analysis elements
#' @param data Data loaded by the user or default data loaded from data_page module (reactive)
#' @param FixRand Whether the model is fixed- or random-effects (reactive)
#' @param outcome The outcome type of the data (reactive)
#' @param Pair_Ref The reference treatment (reactive)
#' @param ContBin Indicator of whether data is continuous or binary (reactive)
#' @param Pair_Trt The treatment of interest (reactive)
#' @return reactive object 'freqpair' which contains all the results from conducting frequentist analysis
frequentist_analysis_panel_server <- function(id, action_button, data, FixRand, outcome, Pair_Ref, ContBin, Pair_Trt) {     
  moduleServer(id, function(input, output, session) {
    
    # Summary Sentence #
    SummaryText <- eventReactive( action_button(), {
      paste0(
        "Results for ", strong(FixRand()), "-effects ",
        strong("Pairwise"), " meta-analysis of ", strong(outcome()), "s using ",
        strong("frequentist"), " methodology, with reference treatment ", strong(Pair_Ref()), "."
      )
    })
    output$SynthesisSummary <- renderText({SummaryText()})
    
    # Meta analysis prep #
    WideData <- reactive({               # convert long format to wide if need be (and ensure trt and ctrl are the right way round)
      SwapTrt(CONBI = ContBin(), data = Long2Wide(data = data()$data), trt = Pair_Trt())
    })
    
    # Conduct meta-analysis and obtain plots #
    freqpair <- eventReactive( action_button(), {
      information <- list()
      information$MA <- FreqPair(data = WideData(), outcome = outcome(), model = 'both', CONBI = ContBin())
      if (FixRand() == 'fixed') {                   # Forest plot
        if (outcome() == 'OR' | outcome() == 'RR') {
          information$Forest <- {
            metafor::forest(information$MA$MA.Fixed, atransf = exp)
            title("Forest plot of studies with overall estimate from fixed-effects model")
          }
        } else {
          information$Forest <- {
            metafor::forest(information$MA$MA.Fixed)
            title("Forest plot of studies with overall estimate from fixed-effects model")
          }
        }
        information$Summary <- PairwiseSummary_functionF(outcome(), information$MA$MA.Fixed)
        information$ModelFit <- PairwiseModelFit_functionF(information$MA$MA.Fixed)
      } else if (FixRand() == 'random') {
        if (outcome() == 'OR' | outcome() == 'RR') {
          information$Forest <- {
            metafor::forest(information$MA$MA.Random, atransf = exp)
            title("Forest plot of studies with overall estimate from random-effects model")
          }
        } else {
          information$Forest <- {
            metafor::forest(information$MA$MA.Random)
            title("Forest plot of studies with overall estimate from random-effects model")
          }
        }
        information$Summary <- PairwiseSummary_functionF(outcome(), information$MA$MA.Random)
        information$ModelFit <- PairwiseModelFit_functionF(information$MA$MA.Random)
      }
      return(information)
    })
    
    # Assign plots #
    
    output$ForestPlot <- renderPlot({      # Forest plot
      freqpair()$Forest
    })
    
    output$SummaryTable <- renderUI({          # Summary table
      freqpair()$Summary
    })
    
    output$ModelFit <- renderUI({              # Model fit statistics
      freqpair()$ModelFit
    })
    
    # Download #
    
    output$forest_download <- downloadHandler(
      filename = function() {
        paste0("PairwiseAnalysis.", input$forest_choice)
      },
      content = function(file) {
        if (input$forest_choice == 'pdf') {
          pdf(file = file)
        } else {
          png(file = file)
        }
        if (FixRand() == 'fixed') {
          if (outcome() == 'OR' | outcome() == 'RR') {
            forest(freqpair()$MA$MA.Fixed, atransf = exp)
            title("Forest plot of studies with overall estimate from fixed-effects model")
          } else {
            forest(freqpair()$MA$MA.Fixed)
            title("Forest plot of studies with overall estimate from fixed-effects model")
          }
        } else {
          if (outcome() == 'OR' | outcome() == 'RR') {
            forest(freqpair()$MA$MA.Random, atransf = exp)
            title("Forest plot of studies with overall estimate from random-effects model")
          } else {
            forest(freqpair()$MA$MA.Random)
            title("Forest plot of studies with overall estimate from random-effects model")
          }
        }
        dev.off()
      }
    )
    
    # Return freqpair to be used elsewhere in app
    return(freqpair)
    
  })
}