# MetaImpact UI

fluidPage(
  useShinyjs(),
  includeCSS(path = "www/app.css"),
  rintrojs::introjsUI(),
  navbarPage(
    id = "MetaImpact",
    title = "MetaImpact",
    theme = shinytheme(theme = "readable"),
    
    # Home Tab
    tabPanel(
      title = "Home",
      home_page_ui(id = "home")
    ),
    
    # Walk-Through
    tabPanel(
      title = "Walk-Through",
      walk_through_page_ui(id = "walk_through")
    ),

    # Data Tab
    tabPanel(
      title = "Data",
      DataPageUi(id = "data")
    ),

    # Evidence Synthesis Tab
    tabPanel(
      title = "Calculator",
      # Meta-analysis
      h3("Step 1: Create evidence base via meta-analysis"),
      br(),
      # Run analysis buttons
      fluidRow(
        align = 'center',
        column(
          width = 4,
          actionButton(inputId = "FreqRun", label = "Run frequentist meta-analysis", class = "btn-primary btn-lg"),
          div(style = "height:20px"),
          actionButton(inputId = "BayesRun", label = "Run Bayesian meta-analysis", class = "btn-primary btn-lg")
        ),
        # Inputs
        column(
          width = 8,
          bsCollapse(
            id = "SynthesisInputs",
            open = "Synthesis Options",
            bsCollapsePanel(
              title = "Synthesis Options",
              style = 'info',
              column(
                width = 6,
                conditionalPanel(
                  condition = "output.ContBin == 'continuous'",
                  radioButtons(
                    inputId = "OutcomeCont",
                    label = "Outcome for continuous data:",
                    choices = c(
                      "Mean Difference (MD)" = "MD",
                      "Standardised Mean Difference (SMD)" = "SMD"
                    )
                  )
                ),
                conditionalPanel(
                  condition = "output.ContBin == 'binary'",
                  radioButtons(
                    inputId = "OutcomeBina",
                    label = "Outcome for binary data:",
                    choices = c(
                      "Odds Ratio (OR)" = "OR",
                      "Risk Ratio (RR)" = "RR",
                      "Risk Difference (RD)" = "RD"
                    )
                  )
                ),
                radioButtons(
                  inputId = "FixRand",
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
                    selectInput(inputId = "Pair_Trt", label = "Select Treatment", choices = NULL)
                  ),
                  column(
                    width = 6,
                    selectInput(inputId = "Pair_Ctrl", label = "Select Comparator", choices = NULL)
                  )
                ),
                fluidRow(
                  bsCollapsePanel(
                    title = "Bayesian options",
                    style = 'info',
                    column(
                      width = 6,
                      radioButtons(
                        inputId = "prior",
                        label = "Vague prior for between study standard deviation:",
                        choices = c(
                          "Half-Cauchy(0,0.5)" = "half-cauchy",
                          "Uniform(0,2)" = "uniform",
                          "Half-Normal(0,1)" = "half-normal"
                        )
                      ),
                      actionButton(inputId = "bayes_help", label = "Help", class = "btn-xs", style = "position: absolute; left: 0; top: 220px")
                    ),
                    column(
                      width = 6,
                      numericInput(inputId = "chains", label = "Number of chains:", value = 2, min = 1),
                      numericInput(inputId = "iter", label = "Number of iterations:", value = 4000, min = 1),
                      numericInput(inputId = "burn", label = "Burn-in:", value = 400, min = 1)
                    )
                  )
                )
              )
            )
          )
        )
      ),
      # Outputs
      # Frequentist
      conditionalPanel(
        condition = "input.FreqRun != 0",
        fluidRow(
          p(htmlOutput(outputId = "SynthesisSummaryFreq")),
          p("To change the model options, please adjust synthesis options above and re-run analysis."),
          bsCollapse(
            id = "FreqID",
            open = "Frequentist Analysis",
            bsCollapsePanel(
              title = "Frequentist Analysis",
              style = 'success',
              column(
                width = 5,
                align = 'center',
                withSpinner(
                  type = 6,
                  htmlOutput(outputId = "SummaryTableF")
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
                htmlOutput(outputId = "ModelFitF")
              ),
              column(
                width = 6,
                align = 'center',
                offset = 1,
                withSpinner(
                  type = 6,
                  plotOutput(outputId = "ForestPlotPairF")
                ),
                downloadButton(outputId = 'forestpairF_download', label = "Download forest plot"),
                radioButtons(inputId = 'forestpairF_choice', label = NULL, choices = c('pdf','png'), inline = TRUE)
              )
            )
          )
        )
      ),
      # Bayesian
      conditionalPanel(
        condition = "input.BayesRun != 0",
        fluidRow(
          p(htmlOutput(outputId = "SynthesisSummaryBayes")),
          p("To change the model options, please adjust synthesis options above and re-run analysis."),
          bsCollapse(
            id = "BayesID",
            open = "Bayesian Analysis",
            bsCollapsePanel(
              title = "Bayesian Analysis",
              style = 'success',
              column(
                width = 5,
                align = 'center',
                withSpinner(
                  type = 6,
                  htmlOutput(outputId = "SummaryTableB")
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
                htmlOutput(outputId = "ModelFitB"),
                plotOutput(outputId = "TracePlot"),
                downloadButton(outputId = 'tracepair_download', label = "Download trace plot"),
                radioButtons(inputId = 'tracepair_choice', label = "", choices = c('pdf','png'), inline = TRUE)
              ),
              column(
                width = 6,
                align = 'center',
                offset = 1,
                withSpinner(
                  type = 6,
                  plotOutput(outputId = "ForestPlotPairB")
                ),
                downloadButton(outputId = 'forestpairB_download', label = "Download forest plot"),
                radioButtons(inputId = 'forestpairB_choice', label = NULL, choices = c('pdf','png'), inline = TRUE)
              )
            )
          )
        )
      ),

      # Consider impact of new study
      conditionalPanel(
        condition = "input.FreqRun != 0 || input.BayesRun != 0",
        br(),
        h3("Step 2: Consider what a new study may look like and it's potential impact"),
        br(),
        # Evidence Base Summary
        column(
          width = 5,
          align = 'center',
          bsCollapse(
            id = "EvidenceBase",
            open = "Current Evidence Base",
            bsCollapsePanel(
              title = "Current Evidence Base",
              style = 'primary',
              h6("This panel presents the current evidence base from which the sample size calculations are based on. If you wish to change this, please alter the synthesis options above accordingly."),
              withSpinner(
                type = 6,
                plotOutput(outputId = "EvBase")
              ),
              radioButtons(
                inputId = "EvBase_choice",
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
                  downloadButton(outputId = 'evbase_download', label = "Download forest plot")
                ),
                div(
                  style = "display:inline-block; width: 10px;",
                  br()
                ),
                div(
                  style = "display: inline-block;",
                  radioButtons(inputId = 'evbase_choice', label = NULL, choices = c('pdf', 'png'), inline = TRUE)
                )
              )
            )
          )
        ),
        column(
          width = 7,
          align = 'center',
          bsCollapse(
            id = "LanganPlot",
            open = "Extended Funnel Plot",
            bsCollapsePanel(
              title = "Extended Funnel Plot",
              style = 'success',
              h6("This panel presents a funnel plot of the current evidence base. Options to extend the plot (below) encourage the user to consider where a new study may lie and it's potential impact."),
              withSpinner(
                type = 6,
                plotOutput(outputId = "Langan")
              ),
              conditionalPanel(
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
                inputId = "LanganOptions",
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
                    inputId = 'Lang_method',
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
                  numericInput(inputId = 'Lang_pvalue', label = "Sig. level for contours", value = 0.05, min = 0, max = 1)
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
                  downloadButton(outputId = 'Langan_download', label = "Download Funnel plot")
                ),
                div(
                  style = "display:inline-block; width: 10px;"
                ),
                div(
                  style = "display: inline-block;",
                  radioButtons(inputId = 'langan_choice', label = NULL, choices = c('pdf', 'png'), inline = TRUE)
                )
              )
            )
          )
        )
      ),

      # Sample Size Calculator
      conditionalPanel(
        condition = "input.FreqRun != 0 || input.BayesRun != 0",
        br(),
        br(),
        h3("Step 3: Calculate power of new study of certain sample size(s)"),
        br(),
        bsAlert(anchorId = "SampleSizeAlertUI"), #error warning about sample sizes
        # Calculator Settings
        column(
          width = 5,
          align = 'center',
          bsCollapse(
            id = "CalcSettings",
            open = "Power Calculation Settings",
            bsCollapsePanel(
              title = "Power Calculation Settings",
              style = 'info',
              fluidRow(
                div(
                  style = "display: inline-block;vertical-align:top;",
                  textInput(inputId = "samplesizes", label = "Total sample size(s)", value = "100")
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
                  numericInput(inputId = "its", label = "Number of iterations", value = 100, min = 1)
                )
              ),
              fluidRow(
                div(
                  style = "display: inline-block;vertical-align:top;",
                  tagList(
                    selectInput(
                      inputId = "impact_type",
                      label = "Type of impact on evidence base",
                      choices = c(
                        "Significant p-value" = "pvalue",
                        "95% confidence interval of certain width" = "ciwidth",
                        "Lower bound of 95% confidence interval of certain value" = "lci",
                        "Upper bound 95% confidence interval of certain value" = "uci"
                      )
                    ),
                    checkboxInput(inputId = "plot_sims", label = "Plot simulated trials onto extended funnel plot?", value = FALSE),
                    actionButton(inputId = "calc_help", label = "Help", class = "btn-xs", style = "position: absolute; left: 40px;")
                  )
                ),
                div(
                  style = "display: inline-block;vertical-align:top; width: 35px;"
                ),
                div(
                  style = "display: inline-block;vertical-align:top; width: 300px;",
                  uiOutput(outputId = "CutOff")
                )
              )
            )
          ),
          actionButton(inputId = "CalcRun", label = "Run Sample Size Calculations", class = "btn-primary btn-lg")
        ),
        # Results
        column(
          width = 7,
          align = 'center',
          uiOutput(outputId = "CalculatorResults")
        )
      )
    )
  )
)
