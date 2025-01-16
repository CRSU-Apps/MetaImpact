#' Module UI for the home page.
#' 
#' @param id ID of the module
#' @return Div for the home page
home_page_ui <- function(id) {
  ns <- NS(id)
  div(
    h2(
      "MetaImpact V1.0.0", 
      align = "left"
    ),
    br(),
    fluidRow(
      column(
        width = 3,
        img(src = "images/MetaImpact.png", 
            style = "vertical-align: middle; width: -webkit-fill-available; max-height: 300px; max-width: 300px; margin: auto;"),
        hr(),
        p(tags$strong("Latest Updates:")),
        p(tags$strong("Major update (16th January 2025 v1.0.0):")),
        tags$ul(
          tags$li("Fixed bugs regarding UI elements and plots"),
          tags$li("Added adjustment to sampling distribution given that tau", tags$sup("2"), " is unknown"),
          tags$li("Temporarily removed Bayesian options for creating evidence base - will be brought back when Bayesian framework is available for all app features")
        ),
        p(tags$strong("Minor update (4th April 2024 v1.0.0-beta-2):")),
        tags$ul(
          tags$li("Updated funding statement"),
          tags$li("Minor formatting improvements")
        )
      ),
      column(
        width = 3,
        offset = 1,
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
    p("Clareece Nevill, Janion Nevill, Will Robinson, Terry Quinn, Nicola Cooper, Alex Sutton"),
    p("This app builds upon the work from the following publications:"),
    p(
      tags$a(
        align = "left",
        href = "https://doi.org/10.1002/sim.2704",
        "Sutton, A.J., Cooper, N.J., Jones, D.R., Lambert, P.C., Thompson, J.R. and Abrams, K.R. (2007), 
            Evidence-based sample size calculations based upon updated meta-analysis. Statist. Med., 26: 2479-2500."
      )
    ),
    p(
      tags$a(
        align = "left",
        href = "https://doi.org/10.1016/j.jclinepi.2011.10.009",
        "Langan, D., Higgins, J.P.T, Gregory, W., Sutton, A.J., 
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
    p("If you have any questions, queries, or feedback, please email our development team at apps@crsu.org.uk"),
    p("DOI for MetaImpact:"),
    tags$div(
      tags$a(href = "https://doi.org/10.5281/zenodo.7951024"),
      tags$img(src = "https://zenodo.org/badge/DOI/10.5281/zenodo.7951024.svg", alt = "DOI")
    ),
    br(),
    renderFooter(),
    br()
  )
}

#' Module server for the home page.
#' 
#' @param id ID of the module
home_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Do nothing
  })
}