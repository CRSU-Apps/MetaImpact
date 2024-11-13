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
    # Frequentist
    conditionalPanel(
      ns = NS(id),
      condition = "input.FreqRun != 0",
      frequentist_analysis_panel_ui(id = ns("frequentist_analysis"))
    ),
    # Bayesian
    conditionalPanel(
      ns = NS(id),
      condition = "input.BayesRun != 0",
      fluidRow(
        p(htmlOutput(outputId = ns("SynthesisSummaryBayes"))),
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
                htmlOutput(outputId = ns("SummaryTableB"))
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
              htmlOutput(outputId = ns("ModelFitB")),
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
                plotOutput(outputId = ns("ForestPlotPairB"))
              ),
              downloadButton(outputId = ns('forestpairB_download'), label = "Download forest plot"),
              radioButtons(inputId = ns('forestpairB_choice'), label = NULL, choices = c('pdf','png'), inline = TRUE)
            )
          )
        )
      )
    ),
    
    # Consider impact of new study
    conditionalPanel(
      ns = NS(id),
      condition = "input.FreqRun != 0 || input.BayesRun != 0",
      br(),
      h3("Step 2: Consider what a new study may look like and it's potential impact"),
      br(),
      # Evidence Base Summary
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
            radioButtons(
              inputId = ns("EvBase_choice"),
              label = NULL,
              choices = c(
                "Frequentist MA" = "freq",
                "Bayesian MA" = "bayes"
              ),
              inline = TRUE
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
      ),
      column(
        width = 7,
        align = 'center',
        bsCollapse(
          id = ns("LanganPlot"),
          open = "Extended Funnel Plot",
          bsCollapsePanel(
            title = "Extended Funnel Plot",
            style = 'success',
            h6("This panel presents a funnel plot of the current evidence base. Options to extend the plot (below) encourage the user to consider where a new study may lie and it's potential impact."),
            withSpinner(
              type = 6,
              plotOutput(outputId = ns("Langan"))
            ),
            conditionalPanel(
              ns = NS(id),
              condition = "input.LanganOptions.includes('contour')",
              div(
                style = "position: absolute; right: 40px;",
                dropMenu(
                  dropdownButton(
                    label = "Significance contours look strange?",
                    circle = FALSE,
                    size = 'xs'
                  ),
                  align = 'left',
                  arrow = FALSE,
                  h6("Limitations with the significance contours"),
                  p("There exist two known 'artefacts' from the methods used to currently draw the significance contours. We apologise for this and will be working on solving these issues."),
                  p(strong("Contours 'drop' at edges")),
                  img(src = "www/PlotLim_edging.png", width = 200, align = "center"),
                  p("The contours may appear to suddenly 'drop' to the bottom of the plot at the far edges. The contours should naturally follow the rest of the curve trajectory. "),
                  p(strong("White triangle")),
                  img(src = "www/PlotLim_triangle.png", width = 200, align = "center"),
                  p("Most contour plots create a 'set of curtains' appearance. However, for some analyses, both contours may curve to the same side of the plot. In such cases, a white triangle may then be present within a shaded area. This triangle should match the colouring above/around it.")
                )
              )
            ),
            br(),
            checkboxGroupInput(
              inputId = ns("LanganOptions"),
              label = NULL,
              choices = c(
                "Null Line" = "plot.zero",
                "Pooled Effect Line" = "plot.summ",
                "Pooled Summary Diamond" = "summ",
                "Predictive Interval" = "pred.interval",
                "Significance Contours" = "contour"
              ),
              selected = c('plot.summ', 'summ'),
              inline = TRUE
            ),
            fluidRow(
              div(
                style = "display: inline-block;",
                radioButtons(
                  inputId = ns('Lang_method'),
                  label = "Fixed or Random Effects",
                  choices = c(
                    'Fixed' = 'fixed',
                    'Random' = 'random'
                  ),
                  inline = TRUE
                )
              ),
              div(
                style = "display:inline-block; width: 10px;",
              ),
              div(
                style = "display: inline-block;",
                numericInput(inputId = ns('Lang_pvalue'), label = "Sig. level for contours", value = 0.05, min = 0, max = 1)
              ),
              div(
                style = "display: inline-block;vertical-align:top;",
                dropMenu(
                  dropdownButton(size = 'xs',icon = icon('info')),
                  align = 'left',
                  h6("Information"),
                  p("The Significance Contours will be calculated based on a desired outcome that the new evidence base has the following significance level (p-value).")
                )
              ),
              div(
                style = "display:inline-block; width: 10px;"
              ),
              div(
                style = "display: inline-block;",
                downloadButton(outputId = ns('Langan_download'), label = "Download Funnel plot")
              ),
              div(
                style = "display:inline-block; width: 10px;"
              ),
              div(
                style = "display: inline-block;",
                radioButtons(inputId = ns('langan_choice'), label = NULL, choices = c('pdf', 'png'), inline = TRUE)
              )
            )
          )
        )
      )
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
      column(
        width = 5,
        align = 'center',
        bsCollapse(
          id = ns("CalcSettings"),
          open = "Power Calculation Settings",
          bsCollapsePanel(
            title = "Power Calculation Settings",
            style = 'info',
            fluidRow(
              div(
                style = "display: inline-block;vertical-align:top;",
                textInput(inputId = ns("samplesizes"), label = "Total sample size(s)", value = "100")
              ),
              div(
                style = "display: inline-block;vertical-align:top;",
                dropMenu(
                  dropdownButton(size = 'xs', icon = icon('info')),
                  align = 'left',
                  h6("Information"),
                  p("Studies are assumed to have two arms of equal sizes."),
                  p("If entering multiple sample sizes, please separate them with a semi-colon (e.g. 100; 200; 300).")
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 15px;"
              ),
              div(
                style = "display: inline-block;vertical-align:top;",
                numericInput(inputId = ns("its"), label = "Number of iterations", value = 100, min = 1)
              )
            ),
            fluidRow(
              div(
                style = "display: inline-block;vertical-align:top;",
                tagList(
                  selectInput(
                    inputId = ns("impact_type"),
                    label = "Type of impact on evidence base",
                    choices = c(
                      "Significant p-value" = "pvalue",
                      "95% confidence interval of certain width" = "ciwidth",
                      "Lower bound of 95% confidence interval of certain value" = "lci",
                      "Upper bound 95% confidence interval of certain value" = "uci"
                    )
                  ),
                  checkboxInput(inputId = ns("plot_sims"), label = "Plot simulated trials onto extended funnel plot?", value = FALSE),
                  actionButton(inputId = ns("calc_help"), label = "Help", class = "btn-xs", style = "position: absolute; left: 40px;")
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 35px;"
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 300px;",
                uiOutput(outputId = ns("CutOff"))
              )
            )
          )
        ),
        actionButton(inputId = ns("CalcRun"), label = "Run Sample Size Calculations", class = "btn-primary btn-lg")
      ),
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
    
    #------------------#
    # Warning messages #
    #------------------#
    BadSampleSizes <- function() {
      showModal(modalDialog(
        title = "Unsuitable Sample Sizes",
        easyClose = FALSE,
        p("The total sample size is assuming two arms of equal size. Therefore, please enter ", tags$strong("even integers.")),
        br(),
        modalButton("Close warning"),
        footer = NULL
      ))
    }
    
    NoBayesian <- function() {
      showModal(modalDialog(
        title = "Feature not yet available",
        easyClose = FALSE,
        p("Calculating the power of new studies with set sample size(s) is not ready yet within the Bayesian framework. Please ", tags$strong("choose frequentist.")),
        br(),
        modalButton("Close warning"),
        footer = NULL
      ))
    }
    
    NoNMA <- function() {
      showModal(modalDialog(
        title = "Feature not yet available",
        easyClose = FALSE,
        p("Synthesising evidence with an NMA is not quite ready yet. Please ", tags$strong("choose pairwise.")),
        br(),
        modalButton("Close warning"),
        footer = NULL
      ))
    }
    
    NoRandomContours <- function() {
      showModal(modalDialog(
        title = "Feature not yet available",
        easyClose = FALSE,
        p("Drawing the significance contours for a random-effects meta-analysis is not quite ready yet. Please either ", tags$strong("choose fixed-effects"), " or ", tags$strong("uncheck contours option.")),
        br(),
        modalButton("Close warning"),
        footer = NULL
      ))
    }
    
    FixedPredInt <- function() {
      showModal(modalDialog(
        title = "Option combination not applicable",
        easyClose = FALSE,
        p("Within a fixed-effects model, the between-study heterogeneity is set to zero, therefore a 95% predictive interval would be equivalent to the 95% confidence interval (represented by the width of the diamond) and is not a plottable option."),
        br(),
        modalButton("Close warning"),
        footer = NULL
      ))
    }
    
    SigContourOnly <- function() {
      showModal(modalDialog(
        title = "Feature not yet available",
        easyClose = FALSE,
        p("Currently, the contours on the extended funnel plot are only for when the desired impact of the new evidence base is related to levels of p-values."),
        p("Therefore, if you wish to plot the simulated 'new trials' from the power calculations, please either: ", tags$ol(tags$li("ensure that the ", strong("type of impact"), " is set to ", strong("Significant p-value"), ", or"), tags$li("uncheck the ", strong("contours"), " option"))),
        p("If you do not wish to plot the simulated 'new trials', please ", strong("uncheck"), " the plot simulated trials option."),
        br(),
        modalButton("Close warning"),
        footer = NULL
      ))
    }
    
    DiffSigValues <- function() {
      showModal(modalDialog(
        title = "Significance values don't match",
        easyClose = FALSE,
        p("If you wish to plot the simulated 'new trials' on top of the extended funnel plot with contours, then the significance level/cut-off value needs to match."),
        p("Please ensure that the ", strong("Sig. level for contours"), " plot option is set to the same value as the ", strong("cut-off calculator"), " option."),
        p("If you do not wish to plot the simulated 'new trials' with the contour option of the extended funnel plot, please ", strong("uncheck"), " the plot simulated trials and/or contour option."),
        br(),
        modalButton("Close warning"),
        footer = "If this error appears after changing the impact type to 'significant p-value' due to a previous warning message, and your significance levels/cut-off values match, please ignore."
      ))
    }
    
    NoPlotMultipleSampleSizes <- function() {
      showModal(modalDialog(
        title = "Feature not yet available",
        easyClose = FALSE,
        p("Plotting the simulated 'new trials' of multiple sample sizes is not yet available. Please either ", tags$strong("uncheck 'plot simulated trials'"), " option or ", tags$strong("specify one sample size.")),
        br(),
        modalButton("Close warning"),
        footer = NULL
      ))
    }

    
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
    
    
    ## Frequentist Meta-Analysis ##
    
    freqpair <- frequentist_analysis_panel_server("frequentist_analysis", action_button=reactive({input$FreqRun}), 
                                                  data = data, FixRand = FixRand, 
                                                  outcome = outcome, Pair_Ref = Pair_Ref, 
                                                  ContBin = ContBin, Pair_Trt = Pair_Trt)
    
    observeEvent( input$FreqRun, {      # reopen panel when a user re-runs analysis
      updateCollapse(session = session, id = "FreqID", open = "Frequentist Analysis")
    })
    
    
    ### Summary sentence of meta-analysis ###
    #-----------------------------------#

    BayesSummaryText <- eventReactive( input$BayesRun, {
      paste0(
        "Results for ", strong(FixRand()), "-effects ",
        strong("Pairwise"), " meta-analysis of ", strong(outcome()), "s using ",
        strong("Bayesian"), " methodology, with vague prior ", strong(prior()),
        " and reference treatment ", strong(Pair_Ref()), "."
      )
    })
    output$SynthesisSummaryBayes <- renderText({ BayesSummaryText() })
    
    
    
    
    LongData <- reactive({               # convert wide format to long if need be
      Wide2Long(data = data()$data)
    })
    
    observeEvent( input$BayesRun, {                           # reopen panel when a user re-runs analysis
      #NoBayesian()
      updateCollapse(session = session, id = "BayesID", open = "Bayesian Analysis")
    })
    
    
    PairwiseSummary_functionB <- function(outcome, MA.Model, model) {   # MA.Model has to have MAData, MA.Fixed and MA.Random
      line0 <- strong("Results")
      line1 <- paste0("Number of studies: ", nrow(MA.Model$MA.Fixed$data_wide)) # same for fixed or random
      if (model == 'random') {
        line2 <- paste0(
          "Pooled estimate: ", round(MA.Model$MAdata[MA.Model$MAdata$Study == 'RE Model', 'est'], 2),
          " (95% CI: ", round(MA.Model$MAdata[MA.Model$MAdata$Study == 'RE Model', 'lci'], 2),
          " to ", round(MA.Model$MAdata[MA.Model$MAdata$Study == 'RE Model', 'uci'], 2), ")"
        ) # already exponentiated where needed within BayesPair function
        if (outcome == 'OR') {
          line3 <- "Between study standard-deviation (log-odds scale): "
        } else if (outcome == 'RR') {
          line3 <- "Between study standard-deviation (log-probability scale): "
        } else {
          line3 <- "Between study standard-deviation: "
        }
        line3 <- paste0(
          line3, round(MA.Model$MA.Random$fit_sum['tau[1]', 1], 3),
          " (95% CI: ", round(MA.Model$MA.Random$fit_sum['tau[1]', 4], 3),
          " to ", round(MA.Model$MA.Random$fit_sum['tau[1]', 8], 3), ")"
        )
      } else {
        line2 <- paste0(
          "Pooled estimate: ", round(MA.Model$MAdata[MA.Model$MAdata$Study == 'FE Model', 'est'], 2),
          " (95% CI: ", round(MA.Model$MAdata[MA.Model$MAdata$Study == 'FE Model', 'lci'], 2),
          " to ", round(MA.Model$MAdata[MA.Model$MAdata$Study == 'FE Model', 'uci'], 2), ")"
        ) # already exponentiated where needed within BayesPair function
        line3 <- "For fixed models, between study standard-deviation is set to 0."
      }
      HTML(paste(line0, line1, line2, line3, sep = "<br/>"))
    }
    PairwiseModelFit_functionB <- function(MA.Model) {
      HTML(paste("Rhat: ", round(MA.Model$Rhat.max, 2)))
    }
    
    bayespair <- eventReactive( input$BayesRun, {         # run Bayesian pairwise MA and obtain plots etc.
      #NoBayesian()
      information <- list()
      information$MA <- BayesPair(
        CONBI = ContBin(),
        data = WideData(),
        trt = Pair_Trt(),
        ctrl = Pair_Ref(),
        outcome = outcome(),
        chains = chains(),
        iter = iter(),
        warmup = burn(),
        model = 'both',
        prior = prior()
      )
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
    
    
    output$ForestPlotPairB <- renderPlot({      # Forest plot
      bayespair()$Forest
    })
    
    output$SummaryTableB <- renderUI({          # Summary table
      bayespair()$Summary
    })
    
    output$ModelFitB <- renderUI({              # Model fit statistic
      bayespair()$ModelFit
    })
    
    output$TracePlot <- renderPlot({            # Trace plot
      bayespair()$Trace
    })
    
    ### Pairwise Sample Size Calculations ###
    #-----------------------------------#
    
    # Forest plot of current evidence base
    output$EvBase <- renderPlot({
      if (input$EvBase_choice!= 'freq') {
        NoBayesian()
      } else {
        if (freqpair()$MA$MA.Fixed$measure %in% c('OR', 'RR')) {
          forest.rma(freqpair()$MA$MA.Fixed, atransf = exp, ylim = -2.5)
          addpoly(freqpair()$MA$MA.Random)
        } else {
          forest.rma(freqpair()$MA$MA.Fixed, ylim = -2.5)
          addpoly(freqpair()$MA$MA.Random)
        }
        title("Forest plot of studies and overall pooled estimates")
      }
    })
    
    # Settings for UI
    OneOrMultiple <- eventReactive( input$CalcRun, {         # function to be used in update Collapse below
      if (grepl(';', input$samplesizes)) {
        return('Power Plot of Results')
      }
      if (!grepl(';', input$samplesizes)) {
        return('Table of power results')
      }
    })
    
    observeEvent( input$CalcRun, {                           # reopen panel when a user re-runs calculator
      updateCollapse(session = session, id = "Calculator", open = OneOrMultiple())
    })
    
    # Function for checking if recalc option needs to be TRUE or FALSE (TRUE if only the impact type and/or cut-off have changed)
    Recalc <- reactiveVal(FALSE)  # initialise
    ### Create set of constantly updated reactive values of cached inputs
    tmpInputs <- reactiveVal()  # initialised
    tmp_pairwise <- reactive({
      if (input$EvBase_choice == 'freq') {
        FreqPair(data = WideData(), outcome = outcome(), model = 'both', CONBI = ContBin()) # if I use freqpair(), then the forest plot on evidence synthesis tab doesn't load - minor bug for now (extra run-time and hope no-one changes frequentist settings without re-running)
      } else {
        bayespair()$MA
      }
    })
    inputCache <- reactive({
      list(
        sample = input$samplesizes,
        NMA = tmp_pairwise(),
        nit = input$its,
        FreqBayes = input$EvBase_choice
      )
    })
    
    # compare previous input settings to decide on recalc option
    observeEvent(input$CalcRun, {
      #compare previous two sets of inputs
      if (!is.null(tmpInputs()) &&
          setequal(inputCache()$sample, tmpInputs()$sample) &&
          setequal(inputCache()$NMA, tmpInputs()$NMA) &&
          setequal(inputCache()$nit, tmpInputs()$nit) &&
          setequal(inputCache()$FreqBayes, tmpInputs()$FreqBayes)) {
        Recalc(TRUE)
      } else {
        Recalc(FALSE)
      }
      tmpInputs(inputCache())
    })
    
    pairwise_MA <- reactive({
      if (input$EvBase_choice == 'freq') {
        freqpair()$MA
      } else {
        bayespair()$MA
      }
    })
    
    # Calculate
    CalcResults <- eventReactive( input$CalcRun, {
      list1 <- list()
      list1$sample_sizes <- as.integer(unlist(str_split(input$samplesizes, ";"), use.names = FALSE)) # convert to vector of integers
      if (grepl(".", input$samplesizes, fixed = TRUE) | grepl(".", toString(list1$sample_sizes/2), fixed = TRUE)) {     # If any decimals or odds have been entered
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
          list1$data <- metapow_multiple(SampleSizes = list1$sample_sizes, NMA = pairwise_MA(), data = WideData(), nit = input$its, inference = input$impact_type, pow = input$cutoff, measure = outcome(), recalc = Recalc(), updateProgress = updateProgress, chains = chains(), iter = iter(), warmup = burn(), prior = prior())
        } else if (length(list1$sample_sizes) == 1) {
          list1$singleresult <- metapow(NMA = pairwise_MA(), data = WideData(), n = list1$sample_sizes, nit = input$its, inference = input$impact_type, pow = input$cutoff, measure = outcome(), recalc = Recalc(), chains = chains(), iter = iter(), warmup = burn(), prior = prior())
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
    
    # Langan Plot #
    
    output$Langan <- renderPlot({
      if (input$Lang_method == 'random' && 'contour' %in% input$LanganOptions) {   # significance contours not available for random-effects
        NoRandomContours()
      } else if (input$Lang_method == 'fixed' && 'pred.interval' %in% input$LanganOptions) {  # Warning how predictive intervals are not of use within fixed-effects models
        FixedPredInt()
      } else if (input$plot_sims && input$impact_type != 'pvalue' && 'contour' %in% input$LanganOptions) { # the significance contours only relate to p-value impact, whereas the power calculator has other options
        SigContourOnly()
      } else if (input$plot_sims && input$impact_type == 'pvalue' && !is.null(input$cutoff) && input$Lang_pvalue != input$cutoff && 'contour' %in% input$LanganOptions) { # the contour cut-offs/sig levels need to be the same
        DiffSigValues()
      } else if (input$plot_sims && length(as.integer(unlist(str_split(input$samplesizes, ";"), use.names = FALSE))) > 1) {   # haven't added functionlity to Langan plot yet so plot multiple sets of sample sizes
        NoPlotMultipleSampleSizes()
      } else {
        extfunnel(
          SS = freqpair()$MA$MAdata$yi,
          seSS = freqpair()$MA$MAdata$sei,
          method = input$Lang_method,
          outcome = outcome(),
          sig.level = input$Lang_pvalue,
          legend = TRUE,
          points = TRUE,
          contour = {'contour' %in% input$LanganOptions},
          summ = {'summ' %in% input$LanganOptions},
          pred.interval = {'pred.interval' %in% input$LanganOptions},
          plot.zero = {'plot.zero' %in% input$LanganOptions},
          plot.summ = {'plot.summ' %in% input$LanganOptions},
          expxticks = {
            if (outcome() %in% c('OR', 'RR')) {
              c(0.25, 0.5, 1, 2, 4)
            }
          },
          sim.points = {
            if (input$plot_sims) {
              CalcResults()$singleresult$sim_study
            }
          }
        )
        # remaining settings not in UI: contour.points, summ.pos, ylim, xlim, xticks, yticks, zero, xlab, ylab, legendpos
      }
    })
    
    ### Interactive UI ###
    #----------------#
    
    # Interactive help boxes #
    
    steps <- reactive(data.frame(
      category = c(rep("CalcSettings", 5)),
      element = c("#samplesizes", "#its", "#impact_type", "#cutoff", "#plot_sims"),
      intro = c("This is where you specify sample sizes for which you wish to estimate power. You can enter one sample size, or multiple by separating them with a semi-colon (;). Currently, it is assumed that future designed trials have two arms of equal size.",
                "Choose how many iterations (i.e. times the algorithm is run) you wish to have per simulation (sample size). If you choose a higher number of iterations, the simulations will take longer but give more precise estimates (narrower confidence intervals), and vice versa.",
                "Making an 'impact' on the current evidence base can be done in multiple ways - choose here which method you wish to focus on (1. Having a significant p-value; 2. Having a 95% confidence interval of a certain width; 3. Having the lower bound of the 95% CI above a certain value; 4. Having the upper bound of the 95% CI below a certain value).",
                "Depending on which type of impact has been chosen, please choose a specific cut-off value for which you define as 'impactful' (e.g. a p-value of less than 0.05).",
                "Choose to plot the results from every simulated 'new study' into the extended funnel plot to visually see how the power is calculated (when viewed alongside the significance contours")
    ))
    
    # Calculator settings #
    observeEvent(
      input$calc_help,
      introjs(
        session,
        options = list(
          steps = steps() %>% filter(category == "CalcSettings"),
          showBullets = FALSE,
          showProgress = TRUE,
          showStepNumbers = FALSE,
          nextLabel = "Next",
          prevLabel = "Prev",
          skipLabel = "Skip"
        )
      )   # IMPACT_TYPE NOT WORKING and don't know why...
    )

    
    # Cut-off information #
    
    CutOffSettings <- function(type, outcome, MAFix, MARan) {
      sumFix <- summary(MAFix)
      sumRan <- summary(MARan)
      if (type == 'pvalue') {
        label <- paste("P-value less than ...")
        initial <- 0.05
        current <- paste("<i>Current p-values are ", strong(round(sumFix$pval, 3)), " (FE) and ", strong(round(sumRan$pval, 3)), " (RE)</i><br>")
      } else if (type == 'ciwidth') {
        label <- paste("Width less than ...")
        initial <- 0.5
        if (outcome %in% c("OR", "RR")) {
          current <- paste("<i>Current width of 95% confidence intervals are ", strong(round(exp(sumFix$ci.ub) - exp(sumFix$ci.lb), 2)), " (FE) and ", strong(round(exp(sumRan$ci.ub) - exp(sumRan$ci.lb), 2)), " (RE)</i><br>")
        } else {
          current <- paste("<i>Current width of 95% confidence intervals are ", strong(round(sumFix$ci.ub - sumFix$ci.lb, 2)), " (FE) and ", strong(round(sumRan$ci.ub - sumRan$ci.lb, 2)), " (RE)</i><br>")
        }
      } else if (type == 'lci') {
        label <- paste("Lower bound greater than ...")
        initial <- 1.1
        if (outcome %in% c("OR", "RR")) {
          current <- paste("<i>Current lower bounds are ", strong(round(exp(sumFix$ci.lb), 2)), " (FE) and ", strong(round(exp(sumRan$ci.lb), 2)), " (RE)</i><br>")
        } else {
          current <- paste("<i>Current lower bounds are ", strong(round(sumFix$ci.lb, 2)), " (FE) and ", strong(round(sumRan$ci.lb, 2)), " (RE)</i><br>")
        }
      } else {
        label <- paste("Upper bound less than ...")
        initial <- 0.9
        if (outcome %in% c("OR", "RR")) {
          current <- paste("<i> Current upper bounds are ", strong(round(exp(sumFix$ci.ub), 2)), " (FE) and ", strong(round(exp(sumRan$ci.ub), 2)), " (RE)</i><br>")
        } else {
          current <- paste("<i> Current upper bounds are ", strong(round(sumFix$ci.ub, 2)), " (FE) and ", strong(round(sumRan$ci.ub, 2)), " (RE)</i><br>")
        }
      }
      list(label = label, initial = initial, current = current)
    }
    
    output$CutOff <- renderUI({
      cutsettings <- CutOffSettings(input$impact_type, outcome(), freqpair()$MA$MA.Fixed, freqpair()$MA$MA.Random)
      tagList(
        numericInput(ns('cutoff'), label = paste(cutsettings$label), value = cutsettings$initial),
        HTML(cutsettings$current)
      )
    })
    
    # Calculator Results #
    
    SingMult <- eventReactive( input$CalcRun, {           # single or multiple sample sizes
      if (grepl(';', input$samplesizes)) {
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
    observeEvent(input$CalcRun, {
      updateRadioButtons(session, "powplot_options", selected = input$powplot_options)  # remember plot settings from before re-running calculator
    })
    
    # evidence base #
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
        if (input$EvBase_choice == 'freq') {
          if (freqpair()$MA$MA.Fixed$measure %in% c('OR', 'RR')) {
            forest.rma(freqpair()$MA$MA.Fixed, atransf = exp, ylim = -2.5)
            addpoly(freqpair()$MA$MA.Random)
          } else {
            forest.rma(freqpair()$MA$MA.Fixedz, ylim = -2.5)
            addpoly(freqpair()$MA$MA.Random)
          }
          title("Forest plot of studies and overal pooled estimates")
        }
        dev.off()
      }
    )
    
    # extended funnel plot #
    output$Langan_download <- downloadHandler(
      filename = function() {
        paste0('ExtFunnelPlot.', input$langan_choice)
      },
      content = function(file) {
        plot <- extfunnel(
          SS = freqpair()$MA$MAdata$yi,
          seSS = freqpair()$MA$MAdata$sei,
          method = input$Lang_method,
          outcome = outcome(),
          sig.level = input$Lang_pvalue,
          legend = TRUE,
          points = TRUE,
          contour = {'contour' %in% input$LanganOptions},
          summ = {'summ' %in% input$LanganOptions},
          pred.interval = {'pred.interval' %in% input$LanganOptions},
          plot.zero = {'plot.zero' %in% input$LanganOptions},
          plot.summ = {'plot.summ' %in% input$LanganOptions},
          expxticks = {
            if (outcome() %in% c('OR', 'RR')) {
              c(0.25, 0.5, 1, 2, 4)
            }
          },
          sim.points = {
            if (input$plot_sims) {
              CalcResults()$singleresult$sim_study
            }
          }
        )
        if (input$langan_choice == 'png') {
          ggsave(file, plot, height = 7, width = 12, units = "in", device = "png")
        } else {
          ggsave(file, plot, height = 7, width = 12, units = "in", device = "pdf")
        }
      }
    )
    
    
    ### Pairwise Meta-Analysis ###
    #------------------------#
    
    

    
    output$forestpairB_download <- downloadHandler(
      filename = function() {
        paste0("PairwiseAnalysis.", input$forestpairB_choice)
      },
      content = function(file) {
        plot <- bayespair()$Forest
        if (input$forestpairB_choice == 'png') {
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
    
  })
}