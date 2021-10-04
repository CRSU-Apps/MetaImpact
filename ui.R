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

tabPanel("Home"),
                   
# Data Tab #
#----------#
                   
tabPanel("Data",
         radioButtons("ContBin", "Example Dataset", c("Continuous Data" = "continuous", "Binary Data" = "binary")),
         uiOutput("data")),
# Make it autodetect continuous or binary for user input data #
                   
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
                    column(6, conditionalPanel(condition = "input.ContBin=='continuous'",
                                    radioButtons("OutcomeCont", "Outcome for continuous data:", c("Mean Difference (MD)" = "MD","Standardised Mean Difference (SMD)" = "SMD"))),
                              conditionalPanel(condition = "input.ContBin=='binary'",
                                     radioButtons("OutcomeBina", "Outcome for binary data:", c("Odds Ratio (OR)" = "OR","Risk Ratio (RR)" = "RR", "Risk Difference (RD)" = "RD"))),
                              radioButtons("FixRand", "Model selection:", c("Fixed-effects model (FE)" = "fixed", "Random-effects model (RE)" = "random"))),
                    column(6, selectInput(inputId = "Reference", label = "Select Reference Treatment", choices = NULL),
                              radioButtons("prior", "Choice of vague prior (Bayesian only):", c("Option 1" = "1", "Option 2" = "2")))))),
         # Outputs #
         # Frequentist #
         fluidRow(p(htmlOutput("SynthesisSummaryFreq")),
                  p("To change the model options, please see the synthesis options above."),
                  bsCollapsePanel(title="Frequentist Analysis", style='success',
                    column(4, plotOutput("NetworkPlotF")), #Network plot
                    column(6,offset=2, plotOutput("ForestPlotF")))), #Forest plot
         # Bayesian #
         fluidRow(p(htmlOutput("SynthesisSummaryBayes")),
                  p("To change the model options, please see the synthesis options above."),
                  bsCollapsePanel(title="Bayesian Analysis", style='success',
                  column(4, plotOutput("NetworkPlotB")),
                  column(6,offset=2, plotOutput("ForestPlotB"), htmlOutput("TauB"), tableOutput("DICB"))))
                  ),
          # See of network plots can be ordered the same as each other
          # All outputs will need further formatting (including sizing)
          # Consider 'run' buttons and having results automatically appear but able to collapse if wanted

# Sample Size Calculator Tab #
#----------------------------#

tabPanel("Sample Size Calculator"),

# Education Tab #
#---------------#

tabPanel("Education"))))