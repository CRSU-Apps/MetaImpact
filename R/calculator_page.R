#' Module UI for the calculator page.
#' 
#' @param id ID of the module
#' @return Div for the home page
calculator_page_ui <- function(id) {
  ns <- NS(id)
  div(
    # Meta-analysis
    h3("Step 1: Create evidence base via meta-analysis"),
    br(),
    # Run analysis buttons
    fluidRow(
      align = 'center',
      column(
        width = 4,
        actionButton(inputId = ns("FreqRun"), label = "Run frequentist meta-analysis", class = "btn-primary btn-lg"),
        div(style = "height:20px"),
        div(title = "Running MetaImpact with a Bayesian meta-analysis is not yet available. Please run a frequentist model.",
            actionButton(inputId = ns("BayesRun"), label = "Run Bayesian meta-analysis", class = "btn-primary btn-lg")
        )
      ),
      # Inputs
      column(
        width = 8,
        synthesis_options_panel_ui(id = ns("synthesis_options"))
      )
    ),
    # Outputs
    # Frequentist Base
    conditionalPanel(
      ns = ns,
      condition = "input.FreqRun != 0",
      frequentist_analysis_panel_ui(id = ns("frequentist_analysis"))
    ),
    # Bayesian Base
    conditionalPanel(
      ns = ns,
      condition = "input.BayesRun != 0",
      bayesian_analysis_panel_ui(id = ns("bayesian_analysis"))
    ),
    
    # Consider impact of new study
    conditionalPanel(
      ns = ns,
      condition = "input.FreqRun != 0 || input.BayesRun != 0",
      br(),
      h3("Step 2: Consider what a new study may look like and it's potential impact"),
      br(),
      evidence_base_panel_ui(id = ns("evidence_base")),
      langan_plot_panel_ui(id = ns("langan"))
    ),
    
    # Sample Size Calculator
    conditionalPanel(
      ns = ns,
      condition = "input.FreqRun != 0 || input.BayesRun != 0",
      br(),
      br(),
      h3("Step 3: Calculate power of new study of certain sample size(s)"),
      br(),
      bsAlert(anchorId = ns("SampleSizeAlertUI")), #error warning about sample sizes
      # Calculator Settings
      column(
        width = 5,
        align = 'center',
        calculator_options_panel_ui(id = ns("calculator_options")),
        actionButton(inputId = ns("CalcRun"), label = "Run Sample Size Calculations", class = "btn-primary btn-lg")
      ),
      # Results
      calculator_results_panel_ui(id = ns("results"))
    )
  )
}


#' Module server for the calculator page.
#' 
#' @param id ID of the module
#' @param data Data loaded by the user or default data loaded from data_page module
calculator_page_server <- function(id, data) {     
  moduleServer(id, function(input, output, session) {
    
    # Disable Bayesian buttons whilst functionality is not complete #
    
    shinyjs::disable(id = "BayesRun")
    
    ## Load Synthesis Options ##
    
    synthOptionsReactives <- synthesis_options_panel_server(id = "synthesis_options", data = data)
    
    Pair_Ref <- synthOptionsReactives$Pair_ref
    Pair_Trt <- synthOptionsReactives$Pair_trt
    FixRand <- synthOptionsReactives$FixRand
    ContBin <- synthOptionsReactives$ContBin
    outcome <- synthOptionsReactives$outcome
    prior <- synthOptionsReactives$prior
    chains <- synthOptionsReactives$chains
    iter <- synthOptionsReactives$iter
    burn <- synthOptionsReactives$burn
    
    # Meta-analysis prep #
    WideData <- reactive({               # convert long format to wide (and ensure trt and ctrl are the right way round)
      SwapTrt(CONBI = ContBin(), data = Long2Wide(data = data()$data), trt = Pair_Trt())
    })
    
    ## Frequentist Meta-Analysis ##
    
    freqpair <- frequentist_analysis_panel_server(id = "frequentist_analysis", action_button=reactive({input$FreqRun}), 
                                                  data = WideData, FixRand = FixRand, 
                                                  outcome = outcome, Pair_Ref = Pair_Ref, 
                                                  ContBin = ContBin, Pair_Trt = Pair_Trt)
    
    observeEvent( input$FreqRun, {      # reopen panel when a user re-runs analysis
      updateCollapse(session = session, id = "FreqID", open = "Frequentist Analysis")
    })
    
    
    ## Bayesian Meta-Analysis ##
    
    bayespair <- bayesian_analysis_panel_server(id = "bayesian_analysis", action_button = reactive({input$BayesRun}),
                                                data = WideData, FixRand = FixRand, 
                                                outcome = outcome, Pair_Ref = Pair_Ref, 
                                                ContBin = ContBin, Pair_Trt = Pair_Trt,
                                                prior = prior, chains = chains, 
                                                iter = iter, burn = burn)
    
    observeEvent( input$BayesRun, {      # reopen panel when a user re-runs analysis
      updateCollapse(session = session, id = "BayesID", open = "Bayesian Analysis")
    })
    
    
    ## Show Evidence Base ##
    
    EvBase_choice <- evidence_base_panel_server(id = "evidence_base", freqpair = freqpair)
    
    
    ## Langan plot ##
    
    langan_plot_panel_server(id = "langan", freqpair = freqpair, plot_sims_btn = plot_sims, 
                             impact_type_btn = impact_type, cutoff_btn = cutoff, 
                             sample_sizes_btn = samplesizes, outcome = outcome, CalcResults = CalcResults)
    
    
    ## Load Calculator Options ##
    
    calcOptionsReactives <- calculator_options_panel_server(id = "calculator_options", EvBase_choice = EvBase_choice, 
                                                            data = WideData, outcome = outcome, ContBin = ContBin,
                                                            freqpair = freqpair, calc_button = reactive({ input$CalcRun }))
    
    samplesizes <-  calcOptionsReactives$samplesizes
    its <- calcOptionsReactives$its
    impact_type <- calcOptionsReactives$impact_type
    plot_sims <- calcOptionsReactives$plot_sims
    cutoff <- calcOptionsReactives$cutoff
    Recalc <- calcOptionsReactives$Recalc
    
    ## Run Calculator & obtain results ##
    
    # Load correct meta-analysis object depending on user selection
    pairwise_MA <- reactive({
      if (EvBase_choice() == 'freq') {
        return(freqpair()$MA)
      } else {
        return(bayespair()$MA)
      }
    })
    
    # Run calculator
    CalcResults <- calculator_results_panel_server(id = "results", calc_button = reactive({ input$CalcRun }), samplesizes = samplesizes, 
                                                   pairwise_MA = pairwise_MA, WideData = WideData, its = its, 
                                                   impact_type = impact_type, cutoff = cutoff, outcome = outcome, 
                                                   Recalc = Recalc, chains = chains, iter = iter, 
                                                   burn = burn, prior = prior)
    
  })
}