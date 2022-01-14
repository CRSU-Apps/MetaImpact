# MetaImpact UI #
#---------------#

# load libraries #
#----------------#
library(shiny)
library(shinythemes)
library(shinyBS)
library(shinyWidgets)

# load user-written functions #
#-----------------------------#


# UI Content #
#------------#
shinyUI(fluidPage(navbarPage(id="MetaImpact", title="MetaImpact",
                   theme = shinytheme("readable"),
                   
# Home Tab #
#----------#

tabPanel("Home",
         
         h4("About"),
         p("This app is part of a Pre-Doctoral Fellowship looking into how to design a future study such that it's inclusion would make 
           an impact on the current evidence-base."),
         p("The app contains four other tabs:"),
         p(strong("Data"), " - upload your data or use an example dataset"),
         p(strong("Evidence Synthesis"), " - meta-analyse data such that an evidence base is created"),
         p(strong("Sample Size Calculator"), " - calculate the sample size of a new study such that it has an impact on the evidence base"),
         p(strong("Education"), " - interactive displays to help understand the maths and assumptions behind app and study design"),
         br(),
         
         h4("Authors"),
         p("Pre-Doctoral Fellow: Clareece Nevill; Supervisors: Alex Sutton & Nicola Cooper; Collaborators: Suzanne Freeman, Terry Quinn & Lucinda Bullingham")),
                   
# Data Tab #
#----------#
                   
tabPanel("Data",
         column(5, h4("Choose Data"),                    # Insert own data or choose example data
                p("Please upload your data as a .csv file. Other formatting rules will apply {yet to be decided, will depend on what I program to auto-detect}."),
                fileInput(inputId="data", label="", buttonLabel="Select", placeholder="No file selected"),
                br(),
                p("If you wish to explore the app, you are welcome to choose one of the example datasets below."),
                p("Both example datasets are based on a (network) meta-analysis reviewing the affect anti-vasuclar endothelial growth factor has on diabetic macular oedema. 
                  Visual acuity (VA) outcomes were reported and chosen for these examples. The paper by Virgili et al can be found ", a(href="https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD007419.pub6/full", "here.")),
                p("To explore an example network meta-analysis, NMA options are available on the 'Evidence Synthesis' tab."),
                radioButtons("ChooseExample", "Example Datasets Available", c("Continuous outcome: Change in VA in terms of LogMAR (negative change in LogMAR = improved vision)" = "continuousEx", 
                                                                              "Binary outcome: Number of people that improved their VA by gaining 3+ lines during a vision test" = "binaryEx"), width='100%')),
         column(7, h4("View Data"),
                uiOutput("data"))),                      # View data
  # Make it such that a user can still use example data even after uploading their own
  # NEEDS CORRECTING - pairwise binary data is not from same paper (details in files)
        
                   
# Evidence Synthesis Tab #
#------------------------#
                   
tabPanel("Evidence Synthesis",
         # Run analysis buttons #
         fluidRow(align = 'center',
           column(4, actionButton("FreqRun", "Run frequentist meta-analysis", class="btn-primary btn-lg"),
                     div(style = "height:20px"),
                     actionButton("BayesRun", "Run Bayesian meta-analysis", class="btn-primary btn-lg")),
           # Inputs #
           column(8, bsCollapsePanel(title="Synthesis Options", style='info',
                    switchInput('Pairwise_NMA', onLabel = "Pairwise", offLabel = "NMA", value=TRUE, onStatus='info', offStatus='info'),
                    column(6, conditionalPanel(condition = "output.ContBin=='continuous'",
                                    radioButtons("OutcomeCont", "Outcome for continuous data:", c("Mean Difference (MD)" = "MD","Standardised Mean Difference (SMD)" = "SMD"))),
                              conditionalPanel(condition = "output.ContBin=='binary'",
                                     radioButtons("OutcomeBina", "Outcome for binary data:", c("Odds Ratio (OR)" = "OR","Risk Ratio (RR)" = "RR", "Risk Difference (RD)" = "RD"))),
                              radioButtons("FixRand", "Model selection:", c("Fixed-effects model (FE)" = "fixed", "Random-effects model (RE)" = "random"))),
                    column(6, conditionalPanel(condition = "input.Pairwise_NMA",
                                               column(6, selectInput(inputId = "Pair_Trt", label = "Select Treatment", choices = NULL)),
                                               column(6, selectInput(inputId = "Pair_Ctrl", label = "Select Comparator", choices = NULL))),
                              conditionalPanel(condition = "!input.Pairwise_NMA",
                                               selectInput(inputId = "Reference", label = "Select Reference Treatment", choices = NULL)),
                              radioButtons("prior", "Choice of vague between-study prior (Bayesian only):", c("Standard deviation ~ Uniform(0,2)" = "uniform", "Precision ~ Gamma(0.1,0.1)" = "gamma", "Standard deviation ~ Half-Normal(0,1)" = "half-normal")))))),
         # Outputs #
         # Frequentist #
         conditionalPanel(condition = "input.FreqRun!=0",
          fluidRow(p(htmlOutput("SynthesisSummaryFreq")),
                  p("To change the model options, please adjust synthesis options above and re-run analysis."),
                  bsCollapse(id="FreqID", open="Frequentist Analysis", bsCollapsePanel(title="Frequentist Analysis", style='success',
                    conditionalPanel(condition = "!input.Pairwise_NMA",   # NMA results
                      column(4, plotOutput("NetworkPlotF")), #Network plot
                      column(6,offset=2, plotOutput("ForestPlotNMAF"))),  # Forest plot
                    conditionalPanel(condition = "input.Pairwise_NMA",    # Pairwise results
                      column(5,align='center', htmlOutput("SummaryTableF")), #Summary table
                      column(6, offset=1, plotOutput("ForestPlotPairF"))))))), #Forest plot
         # Bayesian #
         conditionalPanel(condition = "input.BayesRun!=0",
          fluidRow(p(htmlOutput("SynthesisSummaryBayes")),
                  p("To change the model options, please adjust synthesis options above and re-run analysis."),
                  bsCollapse(id="BayesID", open="Bayesian Analysis", bsCollapsePanel(title="Bayesian Analysis", style='success',
                  column(4, plotOutput("NetworkPlotB")),
                  column(6,offset=2, plotOutput("ForestPlotB"), htmlOutput("TauB"), tableOutput("DICB")))))
                  )),
          # See if network plots can be ordered the same as each other
          # All outputs will need further formatting (including sizing) plus addition of tau for frequentist, and # of studies (for NMA only).
          # Regarding 'Run' buttons -> some formatting doesn't wait for button to be pressed again -> to be fixed (NMA only needs fixing).


# Sample Size Calculator Tab #
#----------------------------#

tabPanel("Sample Size Calculator",
         # Evidence Base Summary #
         column(5, align='center', bsCollapse(id="EvidenceBase", open="Current Evidence Base", 
                                   bsCollapsePanel(title="Current Evidence Base", style='primary',
                                              h6("This panel presents the current evidence base from which the sample size calculations are based on. If you wish to change this, please go back to the ", actionLink("link_to_tabpanel_evsynth", "Evidence synthesis tab"), " and alter the synthesis options accordingly."),
                                              plotOutput("EvBase"),
                                              radioButtons("EvBase_choice", "", c("Frequentist MA" = "freq", "Bayesian MA" = "bayes"), inline=TRUE))),
                                   bsCollapse(id="CalcSettings", open="Calculation Settings",
                                   bsCollapsePanel(title="Calculation Settings", style='info',
                                              fluidRow(div(style="display: inline-block;vertical-align:top;", textInput("samplesizes", "Total sample size(s)", value = "100")),
                                                       div(style="display: inline-block;vertical-align:top;", dropMenu(dropdownButton(size='xs',icon=icon('info')), align='left',
                                                                          h6("Information"),
                                                                          p("Studies are assumed to have two arms of equal sizes."),
                                                                          p("If entering multiple sample sizes, please separate them with a semi-colon (e.g. 100; 200; 300)."))),
                                                       div(style="display: inline-block;vertical-align:top; width: 15px;",HTML("<br>")),
                                                       div(style="display: inline-block;vertical-align:top;", numericInput("#its", "Number of iterations", value=100, min=1))),
                                              fluidRow(div(style="display: inline-block;vertical-align:top;", selectInput("impact_type", "Type of impact on evidence base", 
                                                                          c("Significant p-value" = "pvalue", "95% confidence interval of certain width" = "ciwidth", "Lower bound of 95% confidence interval of certain value" = "lci", "Upper bound 95% confidence interval of certain value" = "uci"))),
                                                       div(style="display: inline-block;vertical-align:top; width: 35px;",HTML("<br>")),
                                                       div(style="display: inline-block;vertical-align:top; width: 300px;", uiOutput("CutOff"))))),
                                   actionButton("CalcRun", "Run Sample Size Calculations", class="btn-primary btn-lg"))),


# Education Tab #
#---------------#

tabPanel("Education"))))


# Consider this to help explain how to use the app: https://stackoverflow.com/questions/61811177/how-to-make-an-infomation-button-in-shiny-dashboard