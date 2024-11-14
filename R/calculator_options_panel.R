#' Module UI for the calculator options panel.
#' 
#' @param id ID of the module
#' @return Div for the home page
calculator_options_panel_ui <- function(id) {
  ns <- NS(id)
  div(
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
    )
  )
}

#' Module server for the calculator options panel.
#' 
#' @param id ID of the module
#' @param EvBase_choice User selected input for which evidence base to use
#' @param data data inputted from the user (or default) (reactive)
#' @param outcome The outcome type of the data (reactive)
#' @param ContBin Indicator of whether data is continuous or binary (reactive)
#' @param freqpair Reactive object containing information from frequentist analysis
#' @return list of user selected options and other reactives needed for calculator
calculator_options_panel_server <- function(id, EvBase_choice, data, outcome, ContBin, freqpair) {     
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Compare calculator options to ascertain whether simulations needs to be re-run (not needed if only the impact type and/or cut-off have changed)
    
    Recalc <- reactiveVal(FALSE)  # initialise
    ### Create set of constantly updated reactive values of cached inputs
    tmpInputs <- reactiveVal()  # initialised
    tmp_pairwise <- reactive({
      if (EvBase_choice() == 'freq') {
        FreqPair(data = data(), outcome = outcome(), model = 'both', CONBI = ContBin()) # if I use freqpair(), then the forest plot on evidence synthesis tab doesn't load - minor bug for now (extra run-time and hope no-one changes frequentist settings without re-running in-between changing calculator settings)
      } else {
        bayespair()$MA
      }
    })
    inputCache <- reactive({
      list(
        sample = input$samplesizes,
        NMA = tmp_pairwise(),
        nit = input$its,
        FreqBayes = EvBase_choice()
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
    
    # Cut-off settings #
    
    output$CutOff <- renderUI({
      cutsettings <- CutOffSettings(input$impact_type, outcome(), freqpair()$MA$MA.Fixed, freqpair()$MA$MA.Random)
      tagList(
        numericInput(ns('cutoff'), label = paste(cutsettings$label), value = cutsettings$initial),
        HTML(cutsettings$current)
      )
    })
    
    # Return user selected settings
    return(
      list(
        samplesizes = reactive({ input$samplesizes }),
        its = reactive({ input$its }),
        impact_type = reactive({ input$impact_type }),
        plot_sims = reactive({ input$plot_sims }),
        CalcRun = reactive({ input$CalcRun }),
        cutoff = reactive({ input$cutoff }),
        Recalc = Recalc
      )
    )
    
  })
}