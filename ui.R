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
         p("This app encourages researchers to design a future study such that it's inclusion would make 
           an impact on the current evidence-base."),
         p("The app contains three main sections:"),
         p(strong("Walk-Through"), " - an interactive educational walk-through of the methods underlying MetaImpact"),
         p(strong("Data"), " - upload your data or use an example dataset"),
         p(strong("Calculator"), " - meta-analyse data to create an evidence base, then calculate the sample size of a new study such that it has an impact on the resulting evidence base"),
         br(),
         
         h4("Authors"),
         p("Clareece Nevill, Terry Quinn, Nicola Cooper, Alex Sutton")),


# Walk-Through #
#--------------#

tabPanel("Walk-Through"),


                   
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
  # Make it such that a user can still use example data even after uploading their own (maybe a tick box after uploading their own to 'use' example instead or 'remove data') -> consider Ryan's addition to MetaInsight

        
                   
# Evidence Synthesis Tab #
#------------------------#
                   
tabPanel("Calculator",
         # Meta-analysis #
         #---------------#
         h3("Step 1: Create evidence base via meta-analysis"),
         br(),
         # Run analysis buttons #
         fluidRow(align = 'center',
           column(4, actionButton("FreqRun", "Run frequentist meta-analysis", class="btn-primary btn-lg"),
                     div(style = "height:20px"),
                     actionButton("BayesRun", "Run Bayesian meta-analysis", class="btn-primary btn-lg")),
           # Inputs #
           column(8, bsCollapse(id="SynthesisInputs", open="Synthesis Options",
                    bsCollapsePanel(title="Synthesis Options", style='info',
                    column(6, conditionalPanel(condition = "output.ContBin=='continuous'",
                                    radioButtons("OutcomeCont", "Outcome for continuous data:", c("Mean Difference (MD)" = "MD","Standardised Mean Difference (SMD)" = "SMD"))),
                              conditionalPanel(condition = "output.ContBin=='binary'",
                                     radioButtons("OutcomeBina", "Outcome for binary data:", c("Odds Ratio (OR)" = "OR","Risk Ratio (RR)" = "RR", "Risk Difference (RD)" = "RD"))),
                              radioButtons("FixRand", "Model selection:", c("Fixed-effects model (FE)" = "fixed", "Random-effects model (RE)" = "random"))),
                    column(6, fluidRow(column(6, selectInput(inputId = "Pair_Trt", label = "Select Treatment", choices = NULL)),
                                       column(6, selectInput(inputId = "Pair_Ctrl", label = "Select Comparator", choices = NULL))),
                              fluidRow(bsCollapsePanel(title="Bayesian options", style='info',
                                                       column(6, radioButtons("prior", "Vague prior for between study standard deviation:", c("Half-Cauchy(0,0.5)" = "half-cauchy", "Uniform(0,2)" = "uniform", "Half-Normal(0,1)" = "half-normal")),
                                                              actionButton("bayes_help", "Help", class="btn-xs", style="position: absolute; left: 0; top: 220px")),
                                                       column(6, numericInput("chains", "Number of chains:", value=2, min=1),
                                                                 numericInput("iter", "Number of iterations:", value=4000, min=1),
                                                                 numericInput("burn", "Burn-in:", value=400, min=1))
                                                       ))
                           ))))),
         # Outputs #
         # Frequentist #
         conditionalPanel(condition = "input.FreqRun!=0",
          fluidRow(p(htmlOutput("SynthesisSummaryFreq")),
                  p("To change the model options, please adjust synthesis options above and re-run analysis."),
                  bsCollapse(id="FreqID", open="Frequentist Analysis", bsCollapsePanel(title="Frequentist Analysis", style='success',
                    column(5,align='center', withSpinner(htmlOutput("SummaryTableF"), type=6), #Summary table
                             fluidRow(div(style="display: inline-block;", p(strong("Model fit statistics"))),
                                      div(style="display: inline-block;", dropMenu(dropdownButton(size='xs',icon=icon('info')), align='left',
                                        h6("Model fit statistics"),
                                        p("Akaike information criterion (AIC) and Bayesian information criterion (BIC) measure 'model performance' whilst taking into account model complexity."),
                                        p("The smaller the AIC or BIC, the 'better' the model. Values are best interpreted between models rather than alone.")))),
                             htmlOutput("ModelFitF")), 
                      column(6, align='center', offset=1, withSpinner(plotOutput("ForestPlotPairF"), type=6),    #Forest plot
                             downloadButton('forestpairF_download', "Download forest plot"), radioButtons('forestpairF_choice', "", c('pdf','png'), inline=TRUE)))))), 
         # Bayesian #
         conditionalPanel(condition = "input.BayesRun!=0",
          fluidRow(p(htmlOutput("SynthesisSummaryBayes")),
                  p("To change the model options, please adjust synthesis options above and re-run analysis."),
                  bsCollapse(id="BayesID", open="Bayesian Analysis", bsCollapsePanel(title="Bayesian Analysis", style='success',
                    column(5, align='center', withSpinner(htmlOutput("SummaryTableB"), type=6),   # Summary table
                                fluidRow(div(style="display: inline-block;", p(strong("Model assessment"))),
                                         div(style="display: inline-block;", dropMenu(dropdownButton(size='xs',icon=icon('info')), align='left',
                                                                                      h6("Model assessment"),
                                                                                      p("For Bayesian models it is key that the model has converged (i.e. that the MCMC algorithm found the optimal solution)"),
                                                                                      p("If a model has converged, Rhat should be smaller than 1.01 and the trace plot (parameter estimates over all iterations) should be 'spiky' and show no signs of distinct pattens. Also note that for ORs and RRs, the parameter estimate has been log-transformed.")))),
                                htmlOutput("ModelFitB"),
                                plotOutput("TracePlot"),                            # Trace plot
                             downloadButton('tracepair_download', "Download trace plot"), radioButtons('tracepair_choice', "", c('pdf','png'), inline=TRUE)),                           
                      column(6, align='center', offset=1, withSpinner(plotOutput("ForestPlotPairB"), type=6),   # Forest plot
                             downloadButton('forestpairB_download', "Download forest plot"), radioButtons('forestpairB_choice', "", c('pdf','png'), inline=TRUE))))
                  )),
         
         # Consider impact of new study #
         #------------------------------#
         conditionalPanel(condition = "input.FreqRun!=0 || input.BayesRun!=0",
          br(),
          h3("Step 2: Consider what a new study may look like and it's potential impact"),
          br(),
          # Evidence Base Summary #
          column(5, align='center', bsCollapse(id="EvidenceBase", open="Current Evidence Base", 
                                              bsCollapsePanel(title="Current Evidence Base", style='primary',
                                                              h6("This panel presents the current evidence base from which the sample size calculations are based on. If you wish to change this, please alter the synthesis options above accordingly."),
                                                              withSpinner(plotOutput("EvBase"), type=6),
                                                              radioButtons("EvBase_choice", "", c("Frequentist MA" = "freq", "Bayesian MA" = "bayes"), inline=TRUE),
                                                              fluidRow(div(style="display: inline-block;", downloadButton('evbase_download', "Download forest plot")),
                                                                       div(style="display:inline-block; width: 10px;", HTML("<br>")),
                                                                       div(style="display: inline-block;", radioButtons('evbase_choice', "", c('pdf', 'png'), inline=TRUE)))))),
          column(7, align='center', bsCollapse(id="LanganPlot", open="Extended Funnel Plot",
                                               bsCollapsePanel(title="Extended Funnel Plot", style='success',
                                                               h6("This panel presents a funnel plot of the current evidence base. Options to extend the plot (below) encourage the user to consider where a new study may lie and it's potential impact."),
                                                               withSpinner(plotOutput("Langan"), type=6),
                                                               checkboxGroupInput("LanganOptions", "", 
                                                                                  choices=c("Null Line"="plot.zero", "Pooled Effect Line"="plot.summ",
                                                                                            "Pooled Summary Diamond"="summ", "Predictive Interval"="pred.interval",
                                                                                            "Significance Contours"="contour"), selected=c('plot.summ','summ'), inline=TRUE),
                                                               fluidRow(div(style="display: inline-block;", radioButtons('Lang_method', "Fixed or Random Effects", c('Fixed', 'Random'), inline=TRUE)),
                                                                        div(style="display:inline-block; width: 10px;", HTML("<br>")),
                                                                        div(style="display: inline-block;", numericInput('Lang_pvalue', "Sig. level for contours", value=0.05)),
                                                                        div(style="display: inline-block;vertical-align:top;", dropMenu(dropdownButton(size='xs',icon=icon('info')), align='left',
                                                                                                                                        h6("Information"),
                                                                                                                                        p("The Significance Contours will be calculated based on a desired outcome that the new evidence base has the following significance level."))),
                                                                        div(style="display:inline-block; width: 10px;", HTML("<br>")),
                                                                        div(style="display: inline-block;", downloadButton('Langan_download', "Download Funnel plot")),
                                                                        div(style="display:inline-block; width: 10px;", HTML("<br>")),
                                                                        div(style="display: inline-block;", radioButtons('langan_choice', "", c('pdf', 'png'), inline=TRUE)))
                                                               )))),
          
         
         # Sample Size Calculator #
         #------------------------#
         conditionalPanel(condition = "input.FreqRun!=0 || input.BayesRun!=0",
          br(), br(),
          h3("Step 3: Calculate power of new study of certain sample size(s)"),
          br(),
          introjsUI(), # help pages
          bsAlert("SampleSizeAlertUI"), #error warning about sample sizes
          # Calculator Settings #
          column(5, align='center',
                bsCollapse(id="CalcSettings", open="Calculation Settings",
                bsCollapsePanel(title="Calculation Settings", style='info',
                                              fluidRow(div(style="display: inline-block;vertical-align:top;", textInput("samplesizes", "Total sample size(s)", value = "100")),
                                                       div(style="display: inline-block;vertical-align:top;", dropMenu(dropdownButton(size='xs',icon=icon('info')), align='left',
                                                                          h6("Information"),
                                                                          p("Studies are assumed to have two arms of equal sizes."),
                                                                          p("If entering multiple sample sizes, please separate them with a semi-colon (e.g. 100; 200; 300)."))),
                                                       div(style="display: inline-block;vertical-align:top; width: 15px;",HTML("<br>")),
                                                       div(style="display: inline-block;vertical-align:top;", numericInput("its", "Number of iterations", value=100, min=1))),
                                              fluidRow(div(style="display: inline-block;vertical-align:top;", tagList(selectInput("impact_type", "Type of impact on evidence base", 
                                                                          c("Significant p-value" = "pvalue", "95% confidence interval of certain width" = "ciwidth", "Lower bound of 95% confidence interval of certain value" = "lci", "Upper bound 95% confidence interval of certain value" = "uci")),
                                                                          checkboxInput("plot_sims", "Plot simulated trials onto extended funnel plot?", value=TRUE),
                                                                          actionButton("calc_help", "Help", class="btn-xs", style="position: absolute; left: 40px;"))),
                                                       div(style="display: inline-block;vertical-align:top; width: 35px;",HTML("<br>")),
                                                       div(style="display: inline-block;vertical-align:top; width: 300px;", uiOutput("CutOff"))))),
                                    actionButton("CalcRun", "Run Sample Size Calculations", class="btn-primary btn-lg")),
        # Results #
          column(7, align='center', uiOutput("CalculatorResults"))
          ))

)))

