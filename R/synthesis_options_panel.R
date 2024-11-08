#' Module UI for the synthesis options panel.
#' 
#' @param id ID of the module
#' @return Div for the home page
synthesis_options_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    bsCollapse(
      id = ns("SynthesisInputs"),
      open = "Synthesis Options",
      bsCollapsePanel(
        title = "Synthesis Options",
        style = 'info',
        column(
          width = 6,
          conditionalPanel(
            ns = NS(id),
            condition = "output.ContBin == 'continuous'",
            radioButtons(
              inputId = ns("OutcomeCont"),
              label = "Outcome for continuous data:",
              choices = c(
                "Mean Difference (MD)" = "MD",
                "Standardised Mean Difference (SMD)" = "SMD"
              )
            )
          ),
          conditionalPanel(
            ns = NS(id),
            condition = "output.ContBin == 'binary'",
            radioButtons(
              inputId = ns("OutcomeBina"),
              label = "Outcome for binary data:",
              choices = c(
                "Odds Ratio (OR)" = "OR",
                "Risk Ratio (RR)" = "RR",
                "Risk Difference (RD)" = "RD"
              )
            )
          ),
          radioButtons(
            inputId = ns("FixRand"),
            label = "Model selection:",
            choices = c(
              "Fixed-effects model (FE)" = "fixed",
              "Random-effects model (RE)" = "random"
            )
          )
        ),
        column(
          width = 6,
          fluidRow(
            column(
              width = 6,
              selectInput(inputId = ns("Pair_Trt"), label = "Select Treatment", choices = NULL, selectize = FALSE)
            ),
            column(
              width = 6,
              selectInput(inputId = ns("Pair_Ref"), label = "Select Reference", choices = NULL, selectize = FALSE)
            )
          ),
          fluidRow(
            bsCollapsePanel(
              title = "Bayesian options",
              style = 'info',
              column(
                width = 6,
                radioButtons(
                  inputId = ns("prior"),
                  label = "Vague prior for between study standard deviation:",
                  choices = c(
                    "Half-Cauchy(0,0.5)" = "half-cauchy",
                    "Uniform(0,2)" = "uniform",
                    "Half-Normal(0,1)" = "half-normal"
                  )
                ),
                actionButton(inputId = ns("bayes_help"), label = "Help", class = "btn-xs", style = "position: absolute; left: 0; top: 220px")
              ),
              column(
                width = 6,
                numericInput(inputId = ns("chains"), label = "Number of chains:", value = 2, min = 1),
                numericInput(inputId = ns("iter"), label = "Number of iterations:", value = 4000, min = 1),
                numericInput(inputId = ns("burn"), label = "Burn-in:", value = 400, min = 1)
              )
            )
          )
        )
      )
    )
  )
}

#' Module server for the synthesis options panel.
#' 
#' @param id ID of the module
#' @param data Data loaded by the user or default data loaded from data_page module
#' @return list of user selected options
synthesis_options_panel_server <- function(id, data) {     
  moduleServer(id, function(input, output, session) {
    
    # Update available reference treatments when the data changes
    observe({
      updateSelectInput(inputId = "Pair_Ref", choices = data()$levels, selected = data()$levels[1])
    }) %>% bindEvent(data())
    
    # Update available interventions when treatment changes
    observe({
      items <- data()$levels
      items <- items[items != input$Pair_Ref]
      
      selected <- input$Pair_Trt
      if (is.null(selected) || !selected %in% items) {
        selected <- items[1]
      }
      updateSelectInput(inputId = "Pair_Trt", choices = items, selected = selected)
      
    }) %>% bindEvent(input$Pair_Ref)
    
    
    ## Automatically detect if continuous or binary and display relevant outcome options
    
    ContBin <- reactive({           
      if (max(grepl("^Mean", names(data()$data)))) {
        return('continuous')
      } else if (max(grepl("^R", names(data()$data)))) {
        return ('binary')
      }
    })
    output$ContBin <- renderText({
      ContBin()
    })
    outputOptions(output, "ContBin", suspendWhenHidden = FALSE) #needed for UI options, but doesn't need displaying itself
    
    outcome <- reactive({                  # different outcome variables if continuous or binary
      if (ContBin() == 'continuous') {
        input$OutcomeCont
      } else {
        input$OutcomeBina
      }
    })
    
    # Interactive help boxes #
    
    steps <- reactive(data.frame(
      category = c(rep("BayesSettings", 4)),
      element = c("#prior", "#chains", "#iter", "#burn"),
      intro = c("Choose which vague prior to use to initially model the between-study standard deviation (used for random-effects models)",
                "Choose the number of chains. A chain represents a run-through of the analysis, with each chain starting with different values to aid robustness. The results then incorporate all chains.",
                "The number of iterations to run through. A higher number of iterations is likely to lead to more robust results but does take longer.",
                "The number of iterations to 'burn' (i.e. not include in the results) at the start. In early iterations, estimated parameters are unlikely to have converged and thus are likely to give spurious results.")
    ))
    
    # Bayesian settings #
    observeEvent(
      input$bayes_help,
      introjs(
        session,
        options = list(
          steps = steps() %>% filter(category == "BayesSettings"),
          showBullets = FALSE,
          showProgress = TRUE,
          showStepNumbers = FALSE,
          nextLabel = "Next",
          prevLabel = "Prev",
          skipLabel = "Skip"
        )
      )
    )
    
    return(
      list(
        Pair_ref = reactive({ input$Pair_Ref}),
        Pair_trt = reactive({ input$Pair_Trt}),
        FixRand = reactive({ input$FixRand }),
        ContBin = ContBin,
        outcome = outcome,
        prior = reactive({ input$prior }),
        chains = reactive({ input$chains }),
        iter = reactive({ input$iter }),
        burn = reactive({ input$burn })
      )
    )
    
  }
)}