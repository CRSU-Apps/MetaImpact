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
shinyUI(navbarPage(title="MetaImpact",
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
         # Inputs #
         fluidRow(bsCollapse(bsCollapsePanel(title="Synthesis Options", style='primary',
                  column(3, conditionalPanel(condition = "input.ContBin=='continuous'",
                                   radioButtons("OutcomeCont", "Outcome for continuous data:", c("Mean Difference (MD)" = "MD","Standardised Mean Difference (SMD)" = "SMD"))),
                            conditionalPanel(condition = "input.ContBin=='binary'",
                                   radioButtons("OutcomeBina", "Outcome for binary data:", c("Odds Ratio (OR)" = "OR","Risk Ratio (RR)" = "RR", "Risk Difference (RD)" = "RD")))),
                  column(3, radioButtons("FixRand", "Model selection:", c("Fixed-effects model (FE)" = "fixed", "Random-effects model (RE)" = "random"))),
                  column(3, radioButtons("FreqBaye", "Methodology choice:", c("Frequestist" = "frequentist", "Bayesian" = "Bayesian"))),
                  column(3, selectInput(inputId = "Reference", label = "Select Reference Treatment", choices = NULL),
                         conditionalPanel(condition = "input.FreqBaye=='Bayesian'",
                                   radioButtons("prior", "Choice of vague prior:", c("Option 1" = "1", "Option 2" = "2"))))))),
         # Outputs #
         fluidRow(align = 'center',
           htmlOutput("SynthesisSummary")),
         fluidRow(column(4, plotOutput("NetworkPlot")), #Network plot
                  column(6,offset=2, plotOutput("ForestPlot"))), #Forest plot
         fluidRow(uiOutput("BayesTest"))
                  ),
          # investigate whether we can make the network and forest plot bigger
          # Add extra info such as tau, DIC etc.

# Sample Size Calculator Tab #
#----------------------------#

tabPanel("Sample Size Calculator"),

# Education Tab #
#---------------#

tabPanel("Education")))