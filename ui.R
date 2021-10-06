# MetaImpact UI #
#---------------#

# load libraries #
#----------------#
library(shiny)
library(shinythemes)
library(shinyBS)

# load user-written functions #
#-----------------------------#

# UI Content #
#------------#
shinyUI(fluidPage(navbarPage(title="MetaImpact",
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
                p("Both example datasets are based on a network meta-analysis reviewing the affect anti-vasuclar endothelial growth factor has on diabetic macular oedema. 
                  Visual acuity (VA) outcomes were reported and chosen for these examples. The paper by Virgili et al can be found ", a(href="https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD007419.pub6/full", "here.")),
                radioButtons("ChooseExample", "Example Datasets Available", c("Continuous outcome: Change in VA in terms of LogMAR (negative change in LogMAR = improved vision)" = "continuousEx", 
                                                                              "Binary outcome: Number of people that improved their VA by gaining 3+ lines during a vision test" = "binaryEx"), width='100%')),
         column(7, h4("View Data"),
                uiOutput("data"))),                      # View data
        
                   
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
                    column(6, conditionalPanel(condition = "output.ContBin=='continuous'",
                                    radioButtons("OutcomeCont", "Outcome for continuous data:", c("Mean Difference (MD)" = "MD","Standardised Mean Difference (SMD)" = "SMD"))),
                              conditionalPanel(condition = "output.ContBin=='binary'",
                                     radioButtons("OutcomeBina", "Outcome for binary data:", c("Odds Ratio (OR)" = "OR","Risk Ratio (RR)" = "RR", "Risk Difference (RD)" = "RD"))),
                              radioButtons("FixRand", "Model selection:", c("Fixed-effects model (FE)" = "fixed", "Random-effects model (RE)" = "random"))),
                    column(6, selectInput(inputId = "Reference", label = "Select Reference Treatment", choices = NULL),
                              radioButtons("prior", "Choice of vague prior (Bayesian only):", c("Option 1" = "1", "Option 2" = "2")))))),
         # Outputs #
         # Frequentist #
         conditionalPanel(condition = "input.FreqRun!=0",
          fluidRow(p(htmlOutput("SynthesisSummaryFreq")),
                  p("To change the model options, please adjust synthesis options above and re-run analysis."),
                  bsCollapse(id="FreqID", open="Frequentist Analysis", bsCollapsePanel(title="Frequentist Analysis", style='success',
                    column(4, plotOutput("NetworkPlotF")), #Network plot
                    column(6,offset=2, plotOutput("ForestPlotF")))))), #Forest plot
         # Bayesian #
         conditionalPanel(condition = "input.BayesRun!=0",
          fluidRow(p(htmlOutput("SynthesisSummaryBayes")),
                  p("To change the model options, please adjust synthesis options above and re-run analysis."),
                  bsCollapse(id="BayesID", open="Bayesian Analysis", bsCollapsePanel(title="Bayesian Analysis", style='success',
                  column(4, plotOutput("NetworkPlotB")),
                  column(6,offset=2, plotOutput("ForestPlotB"), htmlOutput("TauB"), tableOutput("DICB")))))
                  )),
          # See if network plots can be ordered the same as each other
          # All outputs will need further formatting (including sizing)
          # Regarding 'Run' buttons -> some formatting doesn't wait for button to be pressed again -> to be fixed.

# Sample Size Calculator Tab #
#----------------------------#

tabPanel("Sample Size Calculator"),

# Education Tab #
#---------------#

tabPanel("Education"))))