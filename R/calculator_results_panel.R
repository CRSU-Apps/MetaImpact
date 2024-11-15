#' Module UI for the calculator results panel.
#' 
#' @param id ID of the module
#' @return Div for the home page
calculator_results_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    column(
      width = 7,
      align = 'center',
      uiOutput(outputId = ns("CalculatorResults"))
    )
  )
}

#' Module server for the calculator results panel.
#' 
#' @param id ID of the module
#' @param calc_button Action button for running the calculator
#' @param samplesizes Input for sample sizes
#' @param pairwise_MA Reactive object containing chosen meta-analysis
#' @param WideData Reactive wide format of data
#' @param its User selection for input$its (number of iterations for simulations)
#' @param impact_type User selection for input$impact_type (type of desired impact)
#' @param cutoff User selection for input$cutoff (cut off value for desired impact)
#' @param outcome The outcome type of the data (reactive)
#' @param Recalc Reactive logical indicating whether the simulations need to be rerun
#' @param chains User selection for input$chains (number of chains for Bayesian)
#' @param iter User selection for input$iter (number of iterations for Bayesian)
#' @param burn User selection for input$burn (Number of iterations to burn for Bayesian)
#' @param prior User selection for input$prior (Choice of prior distribution for Bayesian)
#' @return Reactive object containing calculator results
calculator_results_panel_server <- function(id, calc_button, samplesizes, pairwise_MA, WideData, its, impact_type, cutoff, outcome, Recalc, chains, iter, burn, prior) {     
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Run calculator and obtain results #
    CalcResults <- eventReactive( calc_button(), {
      print("running calculator")
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
          print("running multiple")
          list1$data <- metapow_multiple(SampleSizes = list1$sample_sizes, NMA = pairwise_MA(), data = WideData(), nit = its(), inference = impact_type(), pow = cutoff(), measure = outcome(), recalc = Recalc(), updateProgress = updateProgress, chains = chains(), iter = iter(), warmup = burn(), prior = prior())
        } else if (length(list1$sample_sizes) == 1) {
          print("running single")
          list1$singleresult <- metapow(NMA = pairwise_MA(), data = WideData(), n = list1$sample_sizes, nit = its(), inference = impact_type(), pow = cutoff(), measure = outcome(), recalc = Recalc(), chains = chains(), iter = iter(), warmup = burn(), prior = prior())
        }
        calc_res <<- list1
        return(calc_res)
      }
    })
    # Bayesian takes a while doing 'something' before starting to run simulations (which also means when using recalc option, it takes longer than I think it should?)
    
    # Assign plots #
    output$powplot <- renderPlot({    # only if multiple sample sizes entered
      print("creating power plot")
      metapowplot(PowerData = CalcResults()$data, ModelOpt = input$powplot_options, SampleSizes = CalcResults()$sample_sizes)
    })
    
    output$powtable <- renderTable({
      print("creating power table")
      powdata <- CalcResults()$data
      names(powdata) <- c("Total Sample Size", "Model", "Power estimate (%)", "Lower 95% CI bound", "Upper 95% CI bound")
      powdata
    }, digits = 1)
    
    output$singleresult <- renderUI({
      print("creating single power result")
      HTML(paste0("<b>Fixed-effects</b>: ", CalcResults()$singleresult$power$Fixed*100, "% power (95% CI: ", round(CalcResults()$singleresult$CI_lower$Fixed*100, 1), "% to ", round(CalcResults()$singleresult$CI_upper$Fixed*100, 1), "%)<br>",
                  "<b>Random-effects</b>: ", CalcResults()$singleresult$power$Random*100, "% power (95% CI: ", round(CalcResults()$singleresult$CI_lower$Random*100, 1), "% to ", round(CalcResults()$singleresult$CI_upper$Random*100, 1), "%)"))
    })
    

    # Ascertain if looking at one or multiple sample sizes and assign results and necessary #
    
    # code for results
    SingMult <- eventReactive( calc_button(), {           
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
    
    # code for collapse panel
    OneOrMultiple <- eventReactive( calc_button(), {         # function to be used in update Collapse below
      if (grepl(';', samplesizes())) {
        return('Power Plot of Results')
      }
      if (!grepl(';', samplesizes())) {
        return('Table of power results')
      }
    })
    observeEvent( calc_button(), {                           # reopen panel when a user re-runs calculator
      updateCollapse(session = session, id = "Calculator", open = OneOrMultiple())
    })
    
    # UI for results (changes depending on whether single or multiple sample sizes were requested) #
    
    output$CalculatorResults <- renderUI({
      panel <- OneOrMultiple()   # ascertain which panel should be open based on whether one or multiple sample sizes have been inputted
      conditionalPanel(
        ns = ns,
        condition = "input.calc_button!= 0",
        bsCollapse(
          id = ns("Calculator"),
          open = panel,
          multiple = TRUE,
          bsCollapsePanel(
            title = "Power Plot of Results",
            style = 'success',
            conditionalPanel(
              ns = ns,
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
              ns = ns,
              condition = "output.SingMult == 'single'",
              p("Only one sample size has been entered.")
            )
          ),
          bsCollapsePanel(
            title = "Table of power results",
            style = 'success',
            conditionalPanel(
              ns = ns,
              condition = "output.SingMult == 'multiple'",
              withSpinner(tableOutput(outputId = ns("powtable")), type = 6),
              downloadButton(outputId = ns('powtable_download'), label = "Download (CSV)")
            ),
            conditionalPanel(
              ns = ns,
              condition = "output.SingMult == 'single'",
              withSpinner(htmlOutput(outputId = ns("singleresult")), type = 6)
            )
          )
        )
      )
    })
    observeEvent(calc_button(), {
      updateRadioButtons(session, "powplot_options", selected = input$powplot_options)  # remember plot settings from before re-running calculator
    })
    
    # downloads #
    
    # plot
    output$powplot_download <- downloadHandler(
      filename = function() {
        paste0("PowerPlot.", input$powplot_choice)
      },
      content = function(file) {
        plot <- metapowplot(PowerData = CalcResults()$data, ModelOpt = input$powplot_options, SampleSizes = CalcResults()$sample_sizes)
        if (input$powplot_choice == 'png') {
          ggsave(file, plot, height = 7, width = 12, units = "in", device = "png")
        } else {
          ggsave(file, plot, height = 7, width = 12, units = "in", device = "pdf")
        }
      }
    )
    # data
    output$powtable_download <- downloadHandler(
      filename = "PowerData.csv",
      content = function(file) {
        write.csv({
          powdata <- CalcResults()$data
          names(powdata) <- c("Total Sample Size", "Model", "Power estimate (%)", "Lower 95% CI bound", "Upper 95% CI bound")
          powdata},
          file,
          row.names = FALSE
        )
      }
    )
    
    return(CalcResults)
    
  })
}