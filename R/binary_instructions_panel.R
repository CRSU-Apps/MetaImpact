
#' UI for the binary data instructions panel.
#'
#' @param id ID of the module.
#'
#' @return Div containing panel.
BinaryInstructionsPanelUi <- function(id) {
  ns <- NS(id)
  
  div(
    p("MetaPairwise can take data in wide format (every row is per study) or long format (every row is per  study arm)."),
    p("The data file should contain five or eight columns columns for long or wide format respectively. Headings of columns are case sensitive."),
    p("The following columns are needed:"),
    tags$ul(
      tags$li(
        "A column labelled ",
        tags$strong("Study"),
        " containing the name (e.g., author, year) of the study. The study name must be unique for each study."
      ),
      tags$li(
        "For long format, a column labelled ",
        tags$strong("T"),
        " containing the name or label of treatment used in each arm of the study.",
        " For wide format, two columns labelled ",
        tags$strong("T.1 & T.2"),
        " containing the name or label of treatment given for study arm 1 and 2 respectively."
      ),
      tags$li(
        "For long format, a column labelled ",
        tags$strong("R"),
        " containing the number of participants with the outcome of interest in each arm of the study.",
        " For wide format, two columns labelled ",
        tags$strong("R.1 & R.2"),
        " containing the number of participants with the outcome of interest for study arm 1 and 2 respectively."
      ),
      tags$li(
        "For long format, a column labelled ",
        tags$strong("N"),
        " containing the number of participants in each arm of the study.",
        " For wide format, two columns labelled ",
        tags$strong("N.1 & N.2"),
        " containing the number of participants for study arm 1 and 2 respectively."
      )
    ),
    downloadButton(outputId = ns("long_download"), label = "Download long format example"),
    downloadButton(outputId = ns("wide_download"), label = "Download wide format example")
  )
}

#' Server for the binary instructions panel.
#'
#' @param id ID of the module.
BinaryInstructionsPanelServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$long_download <- downloadHandler(
        filename = "AntiVEGF_Binary_Pairwise_Long.csv",
        content = function(file) {
          file.copy("data/AntiVEGF_Binary_Pairwise_Long.csv", file)
        }
      )
      
      output$wide_download <- downloadHandler(
        filename = "AntiVEGF_Binary_Pairwise_Wide.csv",
        content = function(file) {
          file.copy("data/AntiVEGF_Binary_Pairwise_Wide.csv", file)
        }
      )
    }
  )
}