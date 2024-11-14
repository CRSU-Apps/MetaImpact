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
        actionButton(inputId = ns("BayesRun"), label = "Run Bayesian meta-analysis", class = "btn-primary btn-lg")
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
      ns = NS(id),
      condition = "input.FreqRun != 0",
      frequentist_analysis_panel_ui(id = ns("frequentist_analysis"))
    ),
    # Bayesian Base
    conditionalPanel(
      ns = NS(id),
      condition = "input.BayesRun != 0",
      bayesian_analysis_panel_ui(id = ns("bayesian_analysis"))
    ),
    
    # Consider impact of new study
    conditionalPanel(
      ns = NS(id),
      condition = "input.FreqRun != 0 || input.BayesRun != 0",
      br(),
      h3("Step 2: Consider what a new study may look like and it's potential impact"),
      br(),
      evidence_base_panel_ui(id = ns("evidence_base")),
      langan_plot_panel_ui(id = ns("langan"))
    ),
    
    # Sample Size Calculator
    conditionalPanel(
      ns = NS(id),
      condition = "input.FreqRun != 0 || input.BayesRun != 0",
      br(),
      br(),
      h3("Step 3: Calculate power of new study of certain sample size(s)"),
      br(),
      bsAlert(anchorId = ns("SampleSizeAlertUI")), #error warning about sample sizes
      # Calculator Settings
      calculator_options_panel_ui(id = ns("calculator_options")),
      # Results
      column(
        width = 7,
        align = 'center',
        uiOutput(outputId = ns("CalculatorResults"))
      )
    )
  )
}


#' Module server for the calculator page.
#' 
#' @param id ID of the module
#' @param data Data loaded by the user or default data loaded from data_page module
calculator_page_server <- function(id, data) {     
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## Load Synthesis Options ##
    
    synthOptionsReactives <- synthesis_options_panel_server("synthesis_options", data = data)
    
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
    
    freqpair <- frequentist_analysis_panel_server("frequentist_analysis", action_button=reactive({input$FreqRun}), 
                                                  data = WideData, FixRand = FixRand, 
                                                  outcome = outcome, Pair_Ref = Pair_Ref, 
                                                  ContBin = ContBin, Pair_Trt = Pair_Trt)
    
    observeEvent( input$FreqRun, {      # reopen panel when a user re-runs analysis
      updateCollapse(session = session, id = "FreqID", open = "Frequentist Analysis")
    })
    
    
    ## Bayesian Meta-Analysis ##
    
    bayespair <- bayesian_analysis_panel_server("bayesian_analysis", action_button = reactive({input$BayesRun}),
                                                data = WideData, FixRand = FixRand, 
                                                outcome = outcome, Pair_Ref = Pair_Ref, 
                                                ContBin = ContBin, Pair_Trt = Pair_Trt,
                                                prior = prior, chains = chains, 
                                                iter = iter, burn = burn)
    
    observeEvent( input$BayesRun, {      # reopen panel when a user re-runs analysis
      updateCollapse(session = session, id = "BayesID", open = "Bayesian Analysis")
    })
    
    
    ## Show Evidence Base ##
    
    EvBase_choice <- evidence_base_panel_server("evidence_base", freqpair = freqpair)
    
    
    ## Langan plot ##
    
    langan_plot_panel_server("langan", freqpair = freqpair, plot_sims_btn = plot_sims, 
                             impact_type_btn = impact_type, cutoff_btn = cutoff, 
                             sample_sizes_btn = samplesizes, outcome = outcome, CalcResults = CalcResults)
    
    
    ## Load Calculator Options ##
    
    calcOptionsReactives <- calculator_options_panel_server("calculator_options", EvBase_choice = EvBase_choice, 
                                                            data = WideData, outcome = outcome, ContBin = ContBin,
                                                            freqpair = freqpair)
    
    samplesizes <-  calcOptionsReactives$samplesizes
    its <- calcOptionsReactives$its
    impact_type <- calcOptionsReactives$impact_type
    plot_sims <- calcOptionsReactives$plot_sims
    CalcRun <- calcOptionsReactives$CalcRun
    cutoff <- calcOptionsReactives$cutoff
    Recalc <- calcOptionsReactives$Recalc
    
  
    
    
    # Settings for UI
    OneOrMultiple <- eventReactive( CalcRun(), {         # function to be used in update Collapse below
      if (grepl(';', samplesizes())) {
        return('Power Plot of Results')
      }
      if (!grepl(';', samplesizes())) {
        return('Table of power results')
      }
    })
    
    observeEvent( CalcRun(), {                           # reopen panel when a user re-runs calculator
      updateCollapse(session = session, id = "Calculator", open = OneOrMultiple())
    })
    

    
    pairwise_MA <- reactive({
      if (EvBase_choice() == 'freq') {
        freqpair()$MA
      } else {
        bayespair()$MA
      }
    })
    
    # Calculate
    CalcResults <- eventReactive( CalcRun(), {
      list1 <- list()
      list1$sample_sizes <- as.integer(unlist(str_split(samplesizes(), ";"), use.names = FALSE)) # convert to vector of integers
      if (grepl(".", samplesizes(), fixed = TRUE) | grepl(".", toString(list1$sample_sizes/2), fixed = TRUE)) {     # If any decimals or odds have been entered
        BadSampleSizes()
      } else {
        progress <- shiny::Progress$new() # Create a Progress object
        progress$set(message = "Running simulations", value = 0)
        on.exit(progress$close()) # Close the progress when this reactive exits (even if there's an error)
        if (length(list1$sample_sizes)>1) {  # only plot if input multiple sample sizes
          updateProgress <- function(value = NULL, detail = NULL) { #callback function to update progress where there are multiple simulations.
            if (is.null(value)) {
              value <- progress$getValue()
              value <- value + (progress$getMax() / length(list1$sample_sizes))
            }
            progress$set(value = value, detail = detail)
          }
          list1$data <- metapow_multiple(SampleSizes = list1$sample_sizes, NMA = pairwise_MA(), data = WideData(), nit = its(), inference = impact_type(), pow = cutoff(), measure = outcome(), recalc = Recalc(), updateProgress = updateProgress, chains = chains(), iter = iter(), warmup = burn(), prior = prior())
        } else if (length(list1$sample_sizes) == 1) {
          list1$singleresult <- metapow(NMA = pairwise_MA(), data = WideData(), n = list1$sample_sizes, nit = its(), inference = impact_type(), pow = cutoff(), measure = outcome(), recalc = Recalc(), chains = chains(), iter = iter(), warmup = burn(), prior = prior())
        }
        list1
      }
    })
    # Bayesian takes a while doing 'something' before starting to run simulations (which also means when using recalc option, it takes longer than I think it should?)
    
    # Results
    output$powplot <- renderPlot({    # only if multiple sample sizes entered
      metapowplot(PowerData = CalcResults()$data, ModelOpt = input$powplot_options, SampleSizes = CalcResults()$sample_sizes)
    })
    
    output$powtable <- renderTable({
      powdata <- CalcResults()$data
      names(powdata) <- c("Total Sample Size", "Model", "Power estimate (%)", "Lower 95% CI bound", "Upper 95% CI bound")
      powdata
    }, digits = 1)
    
    output$singleresult <- renderUI({
      HTML(paste0("<b>Fixed-effects</b>: ", CalcResults()$singleresult$power$Fixed*100, "% power (95% CI: ", round(CalcResults()$singleresult$CI_lower$Fixed*100, 1), "% to ", round(CalcResults()$singleresult$CI_upper$Fixed*100, 1), "%)<br>",
                  "<b>Random-effects</b>: ", CalcResults()$singleresult$power$Random*100, "% power (95% CI: ", round(CalcResults()$singleresult$CI_lower$Random*100, 1), "% to ", round(CalcResults()$singleresult$CI_upper$Random*100, 1), "%)"))
    })
    
    
    
    ### Interactive UI ###
    #----------------#
    
  
    
    # Calculator Results #
    
    SingMult <- eventReactive( CalcRun(), {           # single or multiple sample sizes
      if (grepl(';', samplesizes())) {
        return('multiple')
      } else {
        return ('single')
      }
    })
    output$SingMult <- renderText({
      SingMult()
    })
    outputOptions(output, "SingMult", suspendWhenHidden = FALSE) #needed for UI options, but doesn't need displaying itself
    
    output$CalculatorResults <- renderUI({
      panel <- OneOrMultiple()   # ascertain which panel should be open based on whether one or multple sample sizes have been inputted
      conditionalPanel(
        ns = NS(id),
        condition = "input.CalcRun!= 0",
        bsCollapse(
          id = ns("Calculator"),
          open = panel,
          multiple = TRUE,
          bsCollapsePanel(
            title = "Power Plot of Results",
            style = 'success',
            conditionalPanel(
              ns = NS(id),
              condition = "output.SingMult == 'multiple'",
              withSpinner(plotOutput(outputId = ns('powplot')), type = 6),
              radioButtons(
                inputId = ns('powplot_options'),
                label = NULL,
                c(
                  "Fixed-effects only" = 'fixed',
                  "Random-effects only" = 'random',
                  "Both fixed- and random-effects" = 'both'
                ),
                selected = 'both',
                inline = TRUE
              ),
              fluidRow(
                div(
                  style = "display: inline-block;",
                  downloadButton(outputId = ns('powplot_download'), label = "Download power plot")
                ),
                div(style = "display:inline-block; width: 10px;", br()),
                div(
                  style = "display: inline-block;",
                  radioButtons(inputId = ns('powplot_choice'), label = NULL, c('pdf', 'png'), inline = TRUE)
                )
              )
            ),
            conditionalPanel(
              ns = NS(id),
              condition = "output.SingMult == 'single'",
              p("Only one sample size has been entered.")
            )
          ),
          bsCollapsePanel(
            title = "Table of power results",
            style = 'success',
            conditionalPanel(
              ns = NS(id),
              condition = "output.SingMult == 'multiple'",
              withSpinner(tableOutput(outputId = ns("powtable")), type = 6),
              downloadButton(outputId = ns('powtable_download'), label = "Download (CSV)")
            ),
            conditionalPanel(
              ns = NS(id),
              condition = "output.SingMult == 'single'",
              withSpinner(htmlOutput(outputId = ns("singleresult")), type = 6)
            )
          )
        )
      )
    })
    observeEvent(CalcRun(), {
      updateRadioButtons(session, "powplot_options", selected = input$powplot_options)  # remember plot settings from before re-running calculator
    })
    
  })
}