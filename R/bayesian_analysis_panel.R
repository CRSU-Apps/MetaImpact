#' Module UI for the Bayesian analysis results panel.
#' 
#' @param id ID of the module
#' @return Div for the home page
bayesian_analysis_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    fluidRow(
      p(htmlOutput(outputId = ns("SynthesisSummary"))),
      p("To change the model options, please adjust synthesis options above and re-run analysis."),
      bsCollapse(
        id = ns("BayesID"),
        open = "Bayesian Analysis",
        bsCollapsePanel(
          title = "Bayesian Analysis",
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
                p(strong("Model assessment"))
              ),
              div(
                style = "display: inline-block;",
                dropMenu(
                  dropdownButton(size = 'xs', icon = icon('info')),
                  align = 'left',
                  h6("Model assessment"),
                  p("For Bayesian models it is key that the model has converged (i.e. that the MCMC algorithm found the optimal solution)"),
                  p("If a model has converged, Rhat should be smaller than 1.01 and the trace plot (parameter estimates over all iterations) should be 'spiky' and show no signs of distinct pattens. Also note that for ORs and RRs, the parameter estimate has been log-transformed.")
                )
              )
            ),
            htmlOutput(outputId = ns("ModelFit")),
            plotOutput(outputId = ns("TracePlot")),
            downloadButton(outputId = ns('tracepair_download'), label = "Download trace plot"),
            radioButtons(inputId = ns('tracepair_choice'), label = "", choices = c('pdf','png'), inline = TRUE)
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


#' Module server for the Bayesian analysis results panel.
#' 
#' @param id ID of the module
#' @param action_button Action button from calculator module for running Bayesian analysis elements
#' @param data Data loaded by the user or default data loaded from data_page module (reactive)
#' @param FixRand Whether the model is fixed- or random-effects (reactive)
#' @param outcome The outcome type of the data (reactive)
#' @param Pair_Ref The reference treatment (reactive)
#' @param ContBin Indicator of whether data is continuous or binary (reactive)
#' @param Pair_Trt The treatment of interest (reactive)
#' @param prior Input for selected prior (reactive)
#' @param chains Input for selected number of chains (reactive)
#' @param iter Input for selected number of iterations (reactive)
#' @param burn Input for selected number of iterations to burn-in (reactive)
#' @return reactive object 'freqpair' which contains all the results from conducting frequentist analysis
bayesian_analysis_panel_server <- function(id, action_button, data, FixRand, outcome, Pair_Ref, ContBin, Pair_Trt, prior, chains, iter, burn) {     
  moduleServer(id, function(input, output, session) {
    
    # Summary sentence #
    SummaryText <- eventReactive( action_button(), {
      paste0(
        "Results for ", strong(FixRand()), "-effects ",
        strong("Pairwise"), " meta-analysis of ", strong(outcome()), "s using ",
        strong("Bayesian"), " methodology, with vague prior ", strong(prior()),
        " and reference treatment ", strong(Pair_Ref()), "."
      )
    })
    output$SynthesisSummary <- renderText({ SummaryText() })
    
    # Meta-analysis prep #
    WideData <- reactive({               # convert long format to wide if need be (and ensure trt and ctrl are the right way round)
      SwapTrt(CONBI = ContBin(), data = Long2Wide(data = data()$data), trt = Pair_Trt())
    })
    
    # Conduct meta-analysis and obtain plots #
    bayespair <- eventReactive( action_button(), {         # run Bayesian pairwise MA and obtain plots etc.
      information <- list()
      information$MA <- BayesPair(CONBI = ContBin(), data = WideData(), trt = Pair_Trt(),
        ctrl = Pair_Ref(), outcome = outcome(), chains = chains(), iter = iter(),
        warmup = burn(), model = 'both', prior = prior())
      if (FixRand() == 'fixed') {
        information$Forest <- {
          g <- BayesPairForest(information$MA$MAdata, outcome = outcome(), model = 'fixed')
          g + ggtitle("Forest plot of studies with overall estimate from fixed-effects model") +
            theme(plot.title = element_text(hjust = 0.5, size = 13, face = 'bold'))
        }
        information$Summary <- PairwiseSummary_functionB(outcome(), information$MA, 'fixed')
        information$ModelFit <- PairwiseModelFit_functionB(information$MA$MA.Fixed)
        information$Trace <- {
          g <- stan_trace(information$MA$MA.Fixed$fit, pars = "theta")
          g + theme(legend.position = 'none', aspect.ratio = 0.45, axis.title = element_text(size = 10, face = "bold")) +
            labs(y = "Pooled estimate", x = "Iteration")
        }
      } else if (FixRand() == 'random') {
        information$Forest <- {
          g <- BayesPairForest(information$MA$MAdata, outcome = outcome(), model = 'random')
          g + ggtitle("Forest plot of studies with overall estimate from random-effects model") +
            theme(plot.title = element_text(hjust = 0.5, size = 13, face = 'bold'))
        }
        information$Summary <- PairwiseSummary_functionB(outcome(), information$MA, 'random')
        information$ModelFit <- PairwiseModelFit_functionB(information$MA$MA.Random)
        information$Trace <- {
          g <- stan_trace(information$MA$MA.Random$fit, pars = c("theta", "tau"))
          g + theme(legend.position = 'none', strip.placement = "outside", aspect.ratio = 0.3, axis.title = element_text(size = 10, face = "bold")) +
            labs(x = "Iteration") +
            facet_wrap(~parameter, strip.position = 'left', nrow = 2, scales = 'free', labeller = as_labeller(c(theta = "Pooled estimate", 'tau[1]' = "Between-study SD") ) )
        }
      }
      information
    })
    
    # Assign plots #
    
    output$ForestPlot <- renderPlot({      # Forest plot
      bayespair()$Forest
    })
    
    output$SummaryTable <- renderUI({          # Summary table
      bayespair()$Summary
    })
    
    output$ModelFit <- renderUI({              # Model fit statistic
      bayespair()$ModelFit
    })
    
    output$TracePlot <- renderPlot({            # Trace plot
      bayespair()$Trace
    })
    
    # Download #
    
    output$forest_download <- downloadHandler(
      filename = function() {
        paste0("PairwiseAnalysis.", input$forest_choice)
      },
      content = function(file) {
        plot <- bayespair()$Forest
        if (input$forest_choice == 'png') {
          ggsave(file, plot, height = 7, width = 12, units = "in", device = "png")
        } else {
          ggsave(file, plot, height = 7, width = 12, units = "in", device = "pdf")
        }
      }
    )
    
    output$tracepair_download <- downloadHandler(
      filename = function() {
        paste0("PairwiseTrace.", input$tracepair_choice)
      },
      content = function(file) {
        plot <- bayespair()$Trace
        if (input$tracepair_choice == 'png') {
          ggsave(file, plot, height = 7, width = 12, units = "in", device = "png")
        } else {
          ggsave(file, plot, height = 7, width = 12, units = "in", device = "pdf")
        }
      }
    )
    
    # Return freqpair to be used elsewhere in app
    return(bayespair)
    
  })
}