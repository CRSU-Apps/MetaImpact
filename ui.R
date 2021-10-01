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
         fluidRow(column(4, plotOutput("NetworkPlotF")), #Network plot
                  column(6,offset=2, plotOutput("ForestPlotF"))), #Forest plot
         fluidRow(column(4, plotOutput("NetworkPlotB"))),
                  column(6,offset=2, plotOutput("ForestPlotB"), htmlOutput("TauB"), tableOutput("DICB"))
                  ),
          # Need to rethink arrangement of elements
          # See of network plots can be ordered the same as each other
          # All outputs will need further formatting (including sizing)
          # Consider 'run' buttons and having results automatically appear but able to collapse if wanted

# Sample Size Calculator Tab #
#----------------------------#

tabPanel("Sample Size Calculator"),

# Education Tab #
#---------------#

tabPanel("Education")))