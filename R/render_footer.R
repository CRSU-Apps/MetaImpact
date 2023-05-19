renderFooter <- function() {
  tagList(
    fluidRow(shinydashboard::box(
      tags$a(href = "https://www.nihr.ac.uk/", target = "_blank",
             img(src = "images/funded-by-nihr-logo.png",
                 alt = "Funded by NIHR",
                 class = "footer-img nihr-img img-responsive center-block")),
      includeMarkdown("md/funding_statement.md"),
      width = 12
    )),
    fluidRow(column(
      width = 6,
      tags$a(href = "https://www.gla.ac.uk/", target = "_blank",
             img(src = "images/university-of-glasgow-logo.png", 
                 alt = "University of Glasgow", 
                 class = "footer-img img-responsive center-block")
      )),
      column(
        width = 6,
        tags$a(href = "https://le.ac.uk/", target = "_blank",
               img(src = "images/university-of-leicester-logo.jpg",
                   alt = "University of Leicester",
                   class = "footer-img img-responsive center-block"))
      )))
}