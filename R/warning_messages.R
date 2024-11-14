### Text for warning messages ###

BadSampleSizes <- function() {
  showModal(modalDialog(
    title = "Unsuitable Sample Sizes",
    easyClose = FALSE,
    p("The total sample size is assuming two arms of equal size. Therefore, please enter ", 
      tags$strong("even integers.")),
    br(),
    modalButton("Close warning"),
    footer = NULL
  ))
}

NoBayesian <- function() {
  showModal(modalDialog(
    title = "Feature not yet available",
    easyClose = FALSE,
    p("Calculating the power of new studies with set sample size(s) is not ready yet within the Bayesian framework. Please ", 
      tags$strong("choose frequentist"), 
      " (and run a frequentist analysis in step 1 if not done so already)"),
    br(),
    modalButton("Close warning"),
    footer = NULL
  ))
}

NoNMA <- function() {
  showModal(modalDialog(
    title = "Feature not yet available",
    easyClose = FALSE,
    p("Synthesising evidence with an NMA is not quite ready yet. Please ", 
      tags$strong("choose pairwise.")),
    br(),
    modalButton("Close warning"),
    footer = NULL
  ))
}

NoRandomContours <- function() {
  showModal(modalDialog(
    title = "Feature not yet available",
    easyClose = FALSE,
    p("Drawing the significance contours for a random-effects meta-analysis is not quite ready yet. Please either ", 
      tags$strong("choose fixed-effects"), 
      " or ", tags$strong("uncheck contours option.")),
    br(),
    modalButton("Close warning"),
    footer = NULL
  ))
}

FixedPredInt <- function() {
  showModal(modalDialog(
    title = "Option combination not applicable",
    easyClose = FALSE,
    p("Within a fixed-effects model, the between-study heterogeneity is set to zero, therefore a 95% predictive interval would be equivalent to the 95% confidence interval (represented by the width of the diamond) and is not a plottable option."),
    br(),
    modalButton("Close warning"),
    footer = NULL
  ))
}

SigContourOnly <- function() {
  showModal(modalDialog(
    title = "Feature not yet available",
    easyClose = FALSE,
    p("Currently, the contours on the extended funnel plot are only for when the desired impact of the new evidence base is related to levels of p-values."),
    p("Therefore, if you wish to plot the simulated 'new trials' from the power calculations, please either: ", 
      tags$ol(tags$li("ensure that the ", strong("type of impact"), " is set to ", strong("Significant p-value"), ", or"), tags$li("uncheck the ", strong("contours"), " option"))),
    p("If you do not wish to plot the simulated 'new trials', please ", 
      strong("uncheck"), " the plot simulated trials option."),
    br(),
    modalButton("Close warning"),
    footer = NULL
  ))
}

DiffSigValues <- function() {
  showModal(modalDialog(
    title = "Significance values don't match",
    easyClose = FALSE,
    p("If you wish to plot the simulated 'new trials' on top of the extended funnel plot with contours, then the significance level/cut-off value needs to match."),
    p("Please ensure that the ", strong("Sig. level for contours"), 
      " plot option is set to the same value as the ", strong("cut-off calculator"), " option."),
    p("If you do not wish to plot the simulated 'new trials' with the contour option of the extended funnel plot, please ", 
      strong("uncheck"), " the plot simulated trials and/or contour option."),
    br(),
    modalButton("Close warning"),
    footer = "If this error appears after changing the impact type to 'significant p-value' due to a previous warning message, and your significance levels/cut-off values match, please ignore."
  ))
}

NoPlotMultipleSampleSizes <- function() {
  showModal(modalDialog(
    title = "Feature not yet available",
    easyClose = FALSE,
    p("Plotting the simulated 'new trials' of multiple sample sizes is not yet available. Please either ", 
      tags$strong("uncheck 'plot simulated trials'"), " option or ", tags$strong("specify one sample size.")),
    br(),
    modalButton("Close warning"),
    footer = NULL
  ))
}