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
      h2(
        "MetaImpact V1.0.0",
        tags$sup("Beta", style = "color:#6CC0ED"), 
        align = "left"
      ),
      br(),
      fluidRow(
        column(
          width = 7,
          h4("About"),
          p("This app encourages researchers to design a future study such that it's inclusion would make 
            an impact on the current evidence-base."),
          br(),
          p("The app contains three main sections:"),
          p(strong("Walk-Through"), " - an interactive educational walk-through of the methods underlying MetaImpact"),
          p(strong("Data"), " - upload your data or use an example dataset"),
          p(
            strong("Calculator"), " - meta-analyse the current evidence base of studies to estimate a current best estimate of the relative treatment effect, 
            then calculate the sample size of a new study such that it has an impact on the new evidence base and resulting synthesis"
          ),
          br(),
          p(
            strong("We strongly recommend new users to MetaImpact to go through the 'Walk-Through' tab first, before using the other features."),
            style = "color:#ba0979"
          )
        ),
        column(
          width = 4,
          offset = 1,
          align = 'center',
          p(strong("Introduction Video")),
          p("Please see below a presentation introducing MetaImpact from ESMARConf2023 recorded in March 2023"),
          tags$iframe(
            width = 560,
            height = 315,
            src = "https://www.youtube.com/embed/tcban07zOiw",
            title = "YouTube video player",
            frameborder = 0,
            allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share",
            allowfullscreen = TRUE
          )
        )
      ),
      br(),
      h4("Authors"),
      p("Clareece Nevill, Terry Quinn, Nicola Cooper, Alex Sutton"),
      p("This app builds upon the work from the following publications:"),
      p(
        tags$a(
          align = "left",
          href = "https://doi.org/10.1002/sim.2704", "Sutton, A.J., Cooper, N.J., Jones, D.R., Lambert, P.C., Thompson, J.R. and Abrams, K.R. (2007), 
                  Evidence-based sample size calculations based upon updated meta-analysis. Statist. Med., 26: 2479-2500."
        )
      ),
      p(
        tags$a(
          align = "left",
          href = "https://doi.org/10.1016/j.jclinepi.2011.10.009", "Langan, D., Higgins, J.P.T, Gregory, W., Sutton, A.J.,
          Graphical augmentations to the funnel plot assess the impact of additional evidence on a meta-analysis. J. Clin. Epi., 65 (2012): 511-519."
        )
      ),
      br(),
      h4("Extra"),
      p(
        "The code for MetaImpact is open-source and available on the ",
        tags$a(href = "https://github.com/CRSU-Apps/MetaImpact", "CRSU GitHub Page."),
        " This includes a list of known tasks to be completed before releasing a full version of MetaImpact."
      ),
      p("If you have any questions, queries, or feedback, feel free to email Clareece Nevill at clareece.nevill@le.ac.uk"),
      p("DOI for MetaImpact:"),
      tags$div(
        tags$a(href = "https://doi.org/10.5281/zenodo.7951025"),
        tags$img(src = "https://zenodo.org/badge/DOI/10.5281/zenodo.7951025.svg", alt = "DOI")
      ),
      br(),
      renderFooter(),
      br()
    ),

    # Walk-Through
    tabPanel(
      title = "Walk-Through",
      fluidPage(
        # 'hidden' needed, otherwise everything is initially loaded, then removed (i.e. flashes on load up)
        shinyjs::hidden(
          div(
            class = 'page',
            id = 'page1',
            h2("This tab will walk you through the methods underpinning MetaImpact."),
            h4("By going through this walk-though, you should be able to understand the methods, advocate them to encourage evidence-based practice, and be able to use the Calculator tab for your own data"),
            br(),
            h4(
              "The walk-through will use an example dataset extracted from the paper by Pham et al which can be found ",
              a(href = "https://bmjopen.bmj.com/content/9/5/e022031", "here.")
            ),
            h4("The dataset is based on a (network) meta-analyses reviewing the effect anti-vasuclar endothelial growth factor has on diabetic macular oedema."),
            h4(
              "The binary outcome for visual acuity here is the ",
              em("number of people that improved their best-corrected VA by gaining 15+ letters during a vision test.")
            ),
            h4("The meta-analysis is looking at the effect of ranibizumab (RANI) against comparator treatment bevacizumab (BEVA)."),
            br(),
            h4("Use the buttons below to navigate through the walk-through tab.")
          ),
         
          div(
            class = 'page',
            id = 'page2',
            h2("Fixed-effects meta-analysis (frequentist framework)"),
            h4("The forest plot below summarises the current evidence base, under a fixed-effects model and frequentist framework."),
            h4("Currently, the meta-analysis is showing an improvement in visual acuity when receiving RANI, but this is not statistically significant (i.e. the odds ratio 95% confidence interval (indicated by diamond at the bottom of the plot) crosses the line of no effect)."),
            br(),
            withSpinner(
              type = 6,
              plotOutput(outputId = 'page2Forest')
            )
          ),
          
          div(
            class = 'page',
            id = 'page3',
            fluidRow(
              h2("Design new trial to have impact on current evidence base"),
              h4(
                "It can be argued that if a new trial is being designed with the same treatments and outcomes, it should be designed in such a way that is has an impact on the current evidence base. 
                For example, if a current meta-analysis has a non-statistically significant result, a new trial should be designed in such a way that when it is added to the current meta-analysis (evidence base) it results in a statistically significant result."
              ),
              h4("Lets design a new trial such that when its added to the example meta-analysis, we make an impact on the resulting evidence base. The current p-value is 0.379, lets aim to have a resulting meta-analysis with p-value of 0.15 or less."),
              h4(
                "We're going to keep things simple and assume a trial with two arms with equal numbers of participants, therefore, we just need to calculate the total sample size. 
                The calculator runs many simulations, where for each iteration: a new study is simulated (that has an (underlying) effect consistent with the current meta-analysis outcome estimate); added to the meta-analysis; and the resulting 'impact' (here we're looking at the p-value) is assessed."
              ),
              h4("Press the 'run' button to start the simulations!")
            ),
            br(),
            fluidRow(
              actionButton(inputId = "WalkFreqCalcRun3", label = "Run Sample Size Calculations", class = "btn-primary btn-lg")
            ),
            br(),
            fluidRow(
              conditionalPanel(
                condition = "input.WalkFreqCalcRun3 != 0",
                h4(
                  "The 'power' to have the desired impact (p-value of 0.15 or less), calculated as the % of simulations when the addition of the simulated study reduced the p-value to 0.15 or less, was calculated for total sample sizes of 250, 500, 750, and 1000.
                   The number of iterations/simulations to calculate the power was set at 100 (i.e. 100 updated meta-analyses were conducted, each with a 'new trial').
                   If we had increased the number of iterations, our power estimates would be more accurate (i.e. smaller 95% CI), but be more computationally expensive."
                ),
                br(),
                h4("The power results for each sample size are available below"),
                br(),
                column(
                  width = 5,
                  withSpinner(
                    type = 6,
                    tableOutput(outputId = "page3powtable")
                  )
                ),
                column(
                  width = 7,
                  withSpinner(
                    type = 6,
                    plotOutput(outputId = 'page3powplot')
                  )
                )
              )
            )
          ),
       
          div(
            class = 'page',
            id = 'page4',
            h2("Visually understanding the power calculation"),
            h4("We saw that new trials of sample sizes 250, 500, 750, and 1,000 each gave 3%, 9%, 19%, and 22% power of giving the desired impact on the new evidence base/meta-analysis (p-value of 0.15 or less). But how were those values calculated?"),
            h4(
              "Below is a funnel plot of the original studies (outcome effect (odds ratio) versus the standard error (which is related to sample size)). 
              Shaded regions (defined by contour equations) can be added to illustrate areas of the plot that would give a desired impact (e.g. defined here as an updated meta-analysis with p-value < 0.15) from including a 'new' simulated trial with the corresponding effect size and standard error. Press the button below to add these regions."
            ),
            br(),
            fluidRow(
              column(
                width = 10,
                withSpinner(
                  type = 6,
                  plotOutput(outputId = 'page4Langan')
                )
              ),
              column(
                width = 2,
                conditionalPanel(
                  condition = "input.WalkAddSims != 0",
                  selectInput(
                    inputId = 'WalkSizeChoice',
                    label = "Choose sample size",
                    choices = c('250', '500', '750', '1000'),
                    selected = '1000'
                  ),
                  checkboxInput(inputId = "WalkZoomSims", label = "Zoom in on Simulations", value = FALSE)
                )
              )
            ),
            conditionalPanel(
              condition = "input.WalkAddContours == 0",
              actionButton(inputId = "WalkAddContours", label = "Add Significance Regions", class = "btn-primary btn-large")
            ),
            br(),
            conditionalPanel(
              condition = "input.WalkAddContours != 0",
              h4(
                "We can now see what trial treatment effects (i.e. effect size and standard error) would lead to certain impacts on the new evidence base. Details of how the contour equations for defining these regions are calculated can be found in the paper by Langan et al ",
                a(href = "https://www.jclinepi.com/article/S0895-4356(11)00327-1/fulltext", "here.")
              ),
              h4("Now let's add onto the plot treatment effects from all 100 simulated 'new trials' of sample size 1,000 that were used for the power calculation. Press the button below to add the simulations (once loaded you may want to click the zoom option to make this clearer)."),
              conditionalPanel(
                condition = "input.WalkAddSims == 0",
                actionButton(inputId = "WalkAddSims", label = "Add Simulations", class = "btn-primary btn-large")
              )
            ),
            br(),
            conditionalPanel(
              condition = "input.WalkAddSims != 0",
              h4("We can now see that of the 100 simulated new trials (iterations of the calculator), 22 of them are within the shaded region indicating the desired impact (a significant effect greater than the null effect at the 15% level, i.e. the region of darkest shading). Therefore, the power of a new study of sample size 1,000 having the desired impact is 22%."),
              h4("Why not select one of the other sample sizes tested (from the drop down menu to the right of the plot) and see how the simulated trials fall on the different regions.")
            )
          ),
       
          div(
            class = 'page',
            id = 'page5',
            h2("Random-effects versus Fixed-effects"),
            h4("The fixed-effects model assumes that every study in the meta-analysis has the same underlying treatment effect (i.e. they only vary due to random error). However, this assumption is not always suitable."),
            h4("Instead, a random-effects model assumes that every study has it's own underlying treatment effect, but that each of these underlying effects come from a common/shared distribution (i.e. variation isn't purely due to random error, but the underlying effects are 'similar')."),
            br(),
            fluidRow(
              column(
                width = 4,
                h4("In this example, when we fit a random-effects model, the between study heterogeneity (on the log-odds ratio scale) was estimated at 0.033 (a fixed-effects model forces this to equal 0). The presence of heterogeneity can infer that a random-effects model may be a better 'fit'."),
                h4("The forest plot to the right shows both the fixed-effects and random-effects results. The point estimates are very similar, but the random-effects estimate has a slightly wider confidence interval (due to the added variance from the between-study heterogeneity).")
              ),
              column(
                width = 7,
                offset = 1,
                withSpinner(
                  type = 6,
                  plotOutput(outputId = 'page5Forest')
                )
              )
            )
          ),
       
          div(
            class = 'page',
            id = 'page6',
            h2("How does the random-effects model affect the sample size calculations?"),
            h4("A natural follow-on question is how does the choice of moving from fixed-effects to random-effects affect the power calculations from the sample size calculator."),
            br(),
            actionButton(inputId = "WalkFreqCalcRun6", label = "Run Sample Size Calculations", class = "btn-primary btn-lg"),
            br(),
            conditionalPanel(
              condition = "input.WalkFreqCalcRun6 != 0",
              fluidRow(
                column(
                  width = 3,
                  h4(
                    "The power is much lower when using a random-effects model! A primary reason for this reduction is that when confidence intervals/p-values are calculated, they include both within- and between-study variation - this is what leads to the wider confidence intervals.
                    In turn, it is then 'harder' to move the pooled treatment effects to the desired levels of statistical significance. Even with the largest study sizes, the between-study variation is still present and driving this 'conundrum'."
                  ),
                  h4(
                    "You can find out more statistical details about this in the paper by Sutton et al ",
                    a(href = "https://onlinelibrary.wiley.com/doi/10.1002/sim.2704", "here.")
                  ),
                  br(),
                  h4("An extra element is that the simulated 'new trials' behave slightly differently under the random-effects model."),
                  h4(
                    "When simulating a new trial, the calculator uses parameters from the current meta-analysis, this includes whether it was fitted under a fixed- or random-effects model. 
                    As such, the 'new trials' are allowed to 'vary' more under the random-effects model, to mirror the assumption of each trial having their own underlying study effect."
                  ),
                  conditionalPanel(
                    condition = "input.WalkFreqRandomSims == 0",
                    actionButton(inputId = "WalkFreqRandomSims", label = "Let's see with our example...", class = "btn-primary btn-large")
                  )
                ),
                column(
                  width = 4,
                  align = 'center',
                  offset = 1,
                  h4("Fixed-effects results"),
                  withSpinner(
                    type = 6,
                    tableOutput(outputId = "page6fixpowtable")
                  ),
                  withSpinner(
                    type = 6,
                    plotOutput(outputId = "page6fixpowplot"),
                  )
                ),
                column(
                  width = 4,
                  align = 'center',
                  h4("Random-effects results"),
                  withSpinner(
                    type = 6,
                    tableOutput(outputId = "page6ranpowtable")
                  ),
                  withSpinner(
                    type = 6,
                    plotOutput(outputId = "page6ranpowplot")
                  )
                )
              )
            ),
            conditionalPanel(
              condition = "input.WalkFreqRandomSims != 0",
              fluidRow(
                column(
                  width = 3,
                  h4("This can be seen in the funnel plots to the right. The simulated studies (for sample size 1,000) are more spread out (in terms of resulting odds ratio) under the random-effects model than the fixed-effects."),
                  h4("In the random-effects model, the predictive interval is also plotted which represents the likely result of a new study. This is the same as the confidence interval under the fixed-effects model as any extra variablility due to between-study differences is set to 0.")
                ),
                column(
                  width = 4,
                  align = 'center',
                  offset = 1,
                  h4("Fixed-effects results"),
                  withSpinner(
                    type = 6,
                    plotOutput(outputId = "page6LanganFix")
                  )
                ),
                column(
                  width = 4,
                  align = 'center',
                  h4("Random-effects results"),
                  withSpinner(
                    type = 6,
                    plotOutput(outputId = "page6LanganRan")
                  )
                )
              )
            )
          ),
       
          div(
            class = 'page',
            id = 'page7',
            h2("Bayesian versus Frequentist framework"),
            h4("Up until now, this walk-through was using a frequentist framework. But would there be any difference in using a Bayesian framework?"),
            br(),
            h4("Often, researchers like to use a Bayesian framework as it is more flexible towards more complex models."),
            h4("Below you can see the forest plots for our example random-effects models under a frequentist and Bayesian framework - the estimates are very similar."),
            h4("With regards to meta-analysis, a main difference between these frameworks is how the between-study heterogeneity is modelled. Under a frequentist framework, it is modelled with a single number. However, under a Bayesian framework, it can be modelled with a distribution to represent the uncertainty in the estimation of the heterogeneity (which can be considerable)."),
            h4("This will have a knock-on effect on the sample size calculations - an effect that could be argued to be more accurate."),
            br(),
            h5("Unfortunately, MetaImpact doesn't currently have Bayesian functionality for the Calculator element, however, this is in development."),
            br(),
            fluidRow(
              align = 'center',
              column(
                width = 6,
                h4("Frequentist"),
                withSpinner(
                  type = 6,
                  plotOutput(outputId = 'page7ForestFreq')
                )
              ),
              column(
                width = 6,
                h4("Bayesian"),
                withSpinner(
                  type = 6,
                  plotOutput(outputId = 'page7ForestBayes')
                )
              )
            )
          ),
       
          div(
            class = 'page',
            id = 'page8',
            h2("End of Walk-Through"),
            br(),
            h4("Thank you for going through this walk-through of the methods underpinning MetaImpact."),
            h4("You are now welcome to explore the rest of the application, including uploading your own data (or continue using example data that is supplied and usable throughout the app), and using further options within the calculator (such as changing the significance threshold and type of impact desired).")
          )
       
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 4,
            actionButton(inputId = "prevBtn", label = "< Previous")
          ),
          column(
            width = 1,
            offset = 2,
            actionButton(inputId = "nextBtn", label = "Next >")
          )
        )
      )
    ),

    # Data Tab
    tabPanel(
      title = "Data",
      column(
        width = 4,
        h4("Choose Data"),
        p("Please upload your data as a .csv file, formatted as described on the right-hand side of this page. Treatment coding (i.e. numbering rather than labels) and specifying whether the outcome is continuous or binary is not necessary."),
        fileInput(inputId = "data", label = "", buttonLabel = "Select", placeholder = "No file selected"),
        br(),
        p("If you wish to explore the app without using your own data, you are welcome to choose one of the example datasets below."),
        p(
          "Example datasets are based on (network) meta-analyses reviewing the effect anti-vasuclar endothelial growth factor has on diabetic macular oedema.
          Visual acuity (VA) outcomes were reported and chosen for these examples.
          The continuous outcome example is extracted from a meta-analysis by Virgili et al which can be found ",
          a(href = "https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD007419.pub6/full", "here."),
          "The binary outcome example is extracted from a meta-analysis by Pham et al which can be found ",
          a(href = "https://bmjopen.bmj.com/content/9/5/e022031", "here.")
        ),
        p("Options to explore an example network meta-analysis will be available in the near future."),
        radioButtons(
          inputId = "ChooseExample",
          label = "Example Datasets Available",
          choices = c(
            "Continuous outcome: Change in VA in terms of LogMAR (negative change in LogMAR = improved vision)" = "continuousEx",
            "Binary outcome: Number of people that improved their best-corrected VA by gaining 15+ letters during a vision test" = "binaryEx"
          ),
          width = '100%'
        )
      ),
      column(
        width = 4,
        # View data
        h4("View Data"),
        uiOutput(outputId = "data")
      ),
      column(
        width = 4,
        h4("Format Requirements"),
        tabsetPanel(
          id = 'format_instructions',
          tabPanel(
            title = "Binary Data",
            p("MetaImpact can take data in wide format (every row is per study) or long format (every row is per  study arm)."),
            p("The data file should contain five or eight columns columns for long or wide format respectively. Headings of columns are case sensitive."),
            p("The following columns are needed:"),
            tags$ul(
              tags$li(
                "A column labelled ", tags$strong("StudyID"), " containing the study identifier, starting from 1, then 2, 3, 4... etc."
              ),
              tags$li(
                "A column labelled ", tags$strong("Study"), " containing the name (e.g., author,year) of the study. The study name must be unique for each study."
              ),
              tags$li(
                "For long format, a column labelled ", tags$strong("T"), " containing the name or label of treatment used in each arm of the study.
                For wide format, two columns labelled ", tags$strong("T.1 & T.2"), " containing the name or label of treatment given for study arm 1 and 2 respectively "
              ),
              tags$li(
                "For long format, a column labelled ", tags$strong("R"), " containing the number of participants with the outcome of interest in each arm of the study.
                For wide format, two columns labelled ", tags$strong("R.1 & R.2"), " containing the number of participants with the outcome of interest for study arm 1 and 2 respectively "
              ),
              tags$li(
                "For long format, a column labelled ", tags$strong("N"), " containing the number of participants in each arm of the study.
                For wide format, two columns labelled ", tags$strong("N.1 & N.2"), " containing the number of participants for study arm 1 and 2 respectively "
              )
            )
          ),
          tabPanel(
            title = "Continuous Data",
            p("MetaImpact can take data in wide format (every row is per study) or long format (every row is per  study arm)."),
            p("The data file should contain six or ten columns columns for long or wide format respectively. Headings of columns are case sensitive."),
            p("The following columns are needed:"),
            tags$ul(
              tags$li(
                "A column labelled ", tags$strong("StudyID"), " containing the study identifier, starting from 1, then 2, 3, 4... etc."
              ),
              tags$li(
                "A column labelled ", tags$strong("Study"), " containing the name (e.g., author,year) of the study. The study name must be unique for each study."
              ),
              tags$li(
                "For long format, a column labelled ", tags$strong("T"), " containing the name or label of treatment used in each arm of the study.
                For wide format, two columns labelled ", tags$strong("T.1 & T.2"), " containing the name or label of treatment given for study arm 1 and 2 respectively "
              ),
              tags$li(
                "For long format, a column labelled ", tags$strong("Mean"), " containing the mean value of the outcome in each arm of the study.
                For wide format, two columns labelled ", tags$strong("Mean.1 & Mean.2"), " containing the the mean value of the outcome for study arm 1 and 2 respectively "
              ),
              tags$li(
                "For long format, a column labelled ", tags$strong("SD"), " containing the standard deviation value of the outcome in each arm of the study.
                For wide format, two columns labelled ", tags$strong("SD.1 & SD.2"), " containing the the standard deviation value of the outcome for study arm 1 and 2 respectively "
              ),
              tags$li(
                "For long format, a column labelled ", tags$strong("N"), " containing the number of participants in each arm of the study.
                For wide format, two columns labelled ", tags$strong("N.1 & N.2"), " containing the number of participants for study arm 1 and 2 respectively "
              )
            )
          )
        )
      )
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
