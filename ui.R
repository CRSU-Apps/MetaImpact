# MetaImpact UI #
#---------------#

# load libraries #
#----------------#
library(shiny)
library(shinythemes)
library(shinyBS)
library(shinyWidgets)
library(shinycssloaders)
library(rintrojs)

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
         p("The app contains four pages:"),
         p(strong("Data"), " - upload your data or use an example dataset"),
         p(strong("Evidence Synthesis"), " - meta-analyse data such that an evidence base is created"),
         p(strong("Sample Size Calculator"), " - calculate the sample size of a new study such that it has an impact on the evidence base"),
         p(strong("Education"), " - interactive displays to help understand the maths and assumptions behind app and study design - ", tags$strong("COMING SOON", style="color:#FE2020")),
         br(),
         
         h4("Authors"),
         p("Pre-Doctoral Fellow: Clareece Nevill; Supervisors: Alex Sutton & Nicola Cooper; Collaborators & Research Support: Suzanne Freeman, Terry Quinn & Lucinda Billingham")),
                   
# Data Tab #
#----------#
                   
tabPanel("Data",
         column(5, h4("Choose Data"),                    # Insert own data or choose example data
                p("Please upload your data as a .csv file, with the same column headers as for MetaInsight, however, treatment coding and specifying binary/continuous is not necessary. {Will formalise, and possible auto-detect more format features of the data.}"),
                fileInput(inputId="data", label="", buttonLabel="Select", placeholder="No file selected"),
                br(),
                p("If you wish to explore the app without using your own data, you are welcome to choose one of the example datasets below."),
                p("Example datasets are based on (network) meta-analyses reviewing the effect anti-vasuclar endothelial growth factor has on diabetic macular oedema. 
                  Visual acuity (VA) outcomes were reported and chosen for these examples. 
                  The continuous outcome example is extracted from the paper by Virgili et al which can be found ", a(href="https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD007419.pub6/full", "here."),
                  "The binary outcome example is extracted from the paper by Pham et al which can be found ", a(href="https://bmjopen.bmj.com/content/9/5/e022031", "here.")),
                p("Options to explore an example network meta-analysis will be available in the near future."),
                #p("To explore an example network meta-analysis, NMA options are available on the 'Evidence Synthesis' tab."),
                radioButtons("ChooseExample", "Example Datasets Available", c("Continuous outcome: Change in VA in terms of LogMAR (negative change in LogMAR = improved vision)" = "continuousEx", 
                                                                              "Binary outcome: Number of people that improved their best-corrected VA by gaining 15+ letters during a vision test" = "binaryEx"), width='100%')),
         column(7, h4("View Data"),
                uiOutput("data"))),                      # View data
  # Make it such that a user can still use example data even after uploading their own (maybe a tick box after uploading their own to 'use' example instead or 'remove data')
        
                   
# Evidence Synthesis Tab #
#------------------------#
                   
tabPanel("Evidence Synthesis",
         # Run analysis buttons #
         fluidRow(align = 'center',
           column(4, actionButton("FreqRun", "Run frequentist meta-analysis", class="btn-primary btn-lg"),
                     div(style = "height:20px"),
                     actionButton("BayesRun", "Run Bayesian meta-analysis", class="btn-primary btn-lg")),
           # Inputs #
           column(8, bsCollapse(id="SynthesisInputs", open="Synthesis Options",
                    bsCollapsePanel(title="Synthesis Options", style='info',
                    switchInput('Pairwise_NMA', onLabel = "Pairwise", offLabel = "NMA", value=TRUE, onStatus='info', offStatus='info'),
                    column(6, conditionalPanel(condition = "output.ContBin=='continuous'",
                                    radioButtons("OutcomeCont", "Outcome for continuous data:", c("Mean Difference (MD)" = "MD","Standardised Mean Difference (SMD)" = "SMD"))),
                              conditionalPanel(condition = "output.ContBin=='binary'",
                                     radioButtons("OutcomeBina", "Outcome for binary data:", c("Odds Ratio (OR)" = "OR","Risk Ratio (RR)" = "RR", "Risk Difference (RD)" = "RD"))),
                              radioButtons("FixRand", "Model selection:", c("Fixed-effects model (FE)" = "fixed", "Random-effects model (RE)" = "random"))),
                    column(6, fluidRow(conditionalPanel(condition = "input.Pairwise_NMA",
                                               column(6, selectInput(inputId = "Pair_Trt", label = "Select Treatment", choices = NULL)),
                                               column(6, selectInput(inputId = "Pair_Ctrl", label = "Select Comparator", choices = NULL))),
                              conditionalPanel(condition = "!input.Pairwise_NMA",
                                               selectInput(inputId = "Reference", label = "Select Reference Treatment", choices = NULL))),
                              fluidRow(bsCollapsePanel(title="Bayesian options", style='info',
                                                       column(6, radioButtons("prior", "Vague prior for between study standard deviation:", c("Half-Cauchy(0,0.5)" = "half-cauchy", "Uniform(0,2)" = "uniform", "Half-Normal(0,1)" = "half-normal"))),
                                                       column(6, numericInput("chains", "Number of chains:", value=2, min=1),
                                                                 numericInput("iter", "Number of iterations:", value=1000, min=1),
                                                                 numericInput("burn", "Burn-in:", value=100, min=1))
                                                       ))
                           ))))),
         # Outputs #
         # Frequentist #
         conditionalPanel(condition = "input.FreqRun!=0",
          fluidRow(p(htmlOutput("SynthesisSummaryFreq")),
                  p("To change the model options, please adjust synthesis options above and re-run analysis."),
                  bsCollapse(id="FreqID", open="Frequentist Analysis", bsCollapsePanel(title="Frequentist Analysis", style='success',
                    conditionalPanel(condition = "!input.Pairwise_NMA",   # NMA results
                      column(4, withSpinner(plotOutput("NetworkPlotF"), type=6)), #Network plot
                      column(6,offset=2, withSpinner(plotOutput("ForestPlotNMAF"), type=6))),  # Forest plot
                    conditionalPanel(condition = "input.Pairwise_NMA",    # Pairwise results
                      column(5,align='center', withSpinner(htmlOutput("SummaryTableF"), type=6)), #Summary table
                      column(6, align='center', offset=1, withSpinner(plotOutput("ForestPlotPairF"), type=6),    #Forest plot
                             downloadButton('forestpairF_download', "Download forest plot"), radioButtons('forestpairF_choice', "", c('pdf','png'), inline=TRUE))))))), 
         # Bayesian #
         conditionalPanel(condition = "input.BayesRun!=0",
          fluidRow(p(htmlOutput("SynthesisSummaryBayes")),
                  p("To change the model options, please adjust synthesis options above and re-run analysis."),
                  bsCollapse(id="BayesID", open="Bayesian Analysis", bsCollapsePanel(title="Bayesian Analysis", style='success',
                    conditionalPanel(condition = "!input.Pairwise_NMA",     # NMA results
                      column(4, withSpinner(plotOutput("NetworkPlotB"), type=6)),
                      column(6,offset=2, withSpinner(plotOutput("ForestPlotB"), type=6), htmlOutput("TauB"), tableOutput("DICB"))),
                    conditionalPanel(condition = "input.Pairwise_NMA",
                      column(5, align='center', withSpinner(htmlOutput("SummaryTableB"), type=6)),  # Summary table
                      column(6, align='centre', offset=1, withSpinner(plotOutput("ForestPlotPairB"), type=6),   # Forest plot
                             downloadButton('forestpairB_download', "Download forest plot"), radioButtons('forestpairB_choice', "", c('pdf','png'), inline=TRUE)))))
                  ))),
          # See if network plots can be ordered the same as each other
          # All outputs will need further formatting (including sizing) plus addition of tau for frequentist, and # of studies (for NMA only).
          # Regarding 'Run' buttons -> some formatting doesn't wait for button to be pressed again -> to be fixed (NMA only needs fixing).


# Sample Size Calculator Tab #
#----------------------------#

tabPanel("Sample Size Calculator",
         introjsUI(), # help pages
         bsAlert("SampleSizeAlertUI"), #error warning about sample sizes
         # Evidence Base Summary #
         column(5, align='center', bsCollapse(id="EvidenceBase", open="Current Evidence Base", 
                                   bsCollapsePanel(title="Current Evidence Base", style='primary',
                                              h6("This panel presents the current evidence base from which the sample size calculations are based on. If you wish to change this, please go back to the ", actionLink("link_to_tabpanel_evsynth", "Evidence synthesis tab"), " and alter the synthesis options accordingly."),
                                              withSpinner(plotOutput("EvBase"), type=6),
                                              radioButtons("EvBase_choice", "", c("Frequentist MA" = "freq", "Bayesian MA" = "bayes"), inline=TRUE),
                                              fluidRow(div(style="display: inline-block;", downloadButton('evbase_download', "Download forest plot")),
                                                       div(style="display:inline-block; width: 10px;", HTML("<br>")),
                                                       div(style="display: inline-block;", radioButtons('evbase_choice', "", c('pdf', 'png'), inline=TRUE))))),
        # Calculator Settings #
                                   bsCollapse(id="CalcSettings", open="Calculation Settings",
                                   bsCollapsePanel(title="Calculation Settings", style='info',
                                              fluidRow(div(style="display: inline-block;vertical-align:top;", introBox(textInput("samplesizes", "Total sample size(s)", value = "100"), 
                                                                data.step=1, data.intro="This is where you specify sample sizes for which you wish to estimate power. You can enter one sample size, or multiple by separating them with a semi-colon (;). Currently, it is assumed that future designed trials have two arms of equal size.")),
                                                       div(style="display: inline-block;vertical-align:top;", dropMenu(dropdownButton(size='xs',icon=icon('info')), align='left',
                                                                          h6("Information"),
                                                                          p("Studies are assumed to have two arms of equal sizes."),
                                                                          p("If entering multiple sample sizes, please separate them with a semi-colon (e.g. 100; 200; 300)."))),
                                                       div(style="display: inline-block;vertical-align:top; width: 15px;",HTML("<br>")),
                                                       div(style="display: inline-block;vertical-align:top;", introBox(numericInput("its", "Number of iterations", value=100, min=1),
                                                                data.step=2, data.intro="Choose how many iterations (i.e. times the algorithm is run) you wish to have per simulation (sample size). If you choose a higher number of iterations, the simulations will take longer but give more precise estimates (narrower confidence intervals), and vice versa."))),
                                              fluidRow(div(style="display: inline-block;vertical-align:top;", tagList(introBox(selectInput("impact_type", "Type of impact on evidence base", 
                                                                          c("Significant p-value" = "pvalue", "95% confidence interval of certain width" = "ciwidth", "Lower bound of 95% confidence interval of certain value" = "lci", "Upper bound 95% confidence interval of certain value" = "uci")),
                                                                          data.step=3, data.intro="Making an 'impact' on the current evidence base can be done in multiple ways - choose here which method you wish to focus on (1. Having a significant p-value; 2. Having a 95% confidence interval of a certain width; 3. Having the lower bound of the 95% CI above a certain value; 4. Having the upper bound of the 95% CI below a certain value)."),
                                                                          actionButton("calc_help", "Help", class="btn-xs", style="position: absolute; left: 40px;"))),
                                                       div(style="display: inline-block;vertical-align:top; width: 35px;",HTML("<br>")),
                                                       div(style="display: inline-block;vertical-align:top; width: 300px;", uiOutput("CutOff"))))),
                                   actionButton("CalcRun", "Run Sample Size Calculations", class="btn-primary btn-lg")),
        # Results #
        column(7, align='center', uiOutput("CalculatorResults"))
        ),


# Education Tab #
#---------------#

tabPanel("Education"))))

