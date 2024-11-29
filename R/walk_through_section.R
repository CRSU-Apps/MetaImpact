#' Module UI for the walk-through page.
#' 
#' @param id ID of the module
#' @return Div for the home page
walk_through_section_ui <- function(id) {
  ns <- NS(id)
  div(
    fluidPage(
      # 'hidden' needed, otherwise everything is initially loaded, then removed (i.e. flashes on load up)
      shinyjs::hidden(
        div(
          class = 'page',
          id = ns('page1'),
          h2("This tab will walk you through the methods underpinning MetaImpact."),
          h4("By going through this walk-though, you should be able to understand the methods, advocate them to encourage evidence-based practice, and be able to use the Calculator tab for your own data"),
          br(),
          h4(
            "The walk-through will use an example dataset extracted from the paper by Pham et al which can be found ",
            a(href = "https://bmjopen.bmj.com/content/9/5/e022031", "here.")
          ),
          h4("The dataset is based on a (network) meta-analyses reviewing the effect anti-vasuclar endothelial growth factor has on diabetic macular oedema."),
          h4(
            "The binary outcome for visual acuity here is the ",
            em("number of people that improved their best-corrected VA by gaining 15+ letters during a vision test.")
          ),
          h4("The meta-analysis is looking at the effect of ranibizumab (RANI) against comparator treatment bevacizumab (BEVA)."),
          br(),
          h4("Use the buttons below to navigate through the walk-through tab.")
        ),
        
        div(
          class = 'page',
          id = ns('page2'),
          walkthrough_page_two_ui(id = ns("page2"))
        ),
        
        div(
          class = 'page',
          id = ns('page3'),
          walkthrough_page_three_ui(id = ns("page3"))
        ),
        
        div(
          class = 'page',
          id = ns('page4'),
          walkthrough_page_four_ui(id = ns("page4"))
        ),
        
        div(
          class = 'page',
          id = ns('page5'),
          walkthrough_page_five_ui(id = ns("page5"))
        ),
        
        div(
          class = 'page',
          id = ns('page6'),
          walkthrough_page_six_ui(id = ns("page6"))
        ),
        
        div(
          class = 'page',
          id = ns('page7'),
          walkthrough_page_seven_ui(id = ns("page7"))
        ),
        
        div(
          class = 'page',
          id = ns('page8'),
          h2("End of Walk-Through"),
          br(),
          h4("Thank you for going through this walk-through of the methods underpinning MetaImpact."),
          h4("You are now welcome to explore the rest of the application, including uploading your own data (or continue using example data that is supplied and usable throughout the app), and using further options within the calculator (such as changing the significance threshold and type of impact desired).")
        )
        
      ),
      br(),
      fluidRow(
        column(
          width = 1,
          offset = 4,
          actionButton(inputId = ns("prevBtn"), label = "< Previous")
        ),
        column(
          width = 1,
          offset = 2,
          actionButton(inputId = ns("nextBtn"), label = "Next >")
        )
      )
    )
  )
}

#' Module server for the walk-through page.
#' 
#' @param id ID of the module
walk_through_section_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ## Code for stepping through the multiple pages ##
    rv <- reactiveValues(page = 1)
    
    observe({
      shinyjs::toggleState(id = "prevBtn", condition = rv$page > 1)
      shinyjs::toggleState(id = "nextBtn", condition = rv$page < 8)
      shinyjs::hide(selector = ".page")     # hides previously loaded page
      shinyjs::show(paste0("page", rv$page))
    })
    
    navPage <- function(direction) {
      rv$page <- rv$page + direction
    }
    
    observeEvent(input$prevBtn, navPage(-1))
    observeEvent(input$nextBtn, navPage(1))
    
    ## Datasets ##
    
    WalkData <- rio::import("data/AntiVEGF_Binary_Pairwise_Long.csv") %>%
      CleanData() %>%
      WrangleUploadData()
    
    WalkCalcResultsData <- rio::import("WalkThroughResults.csv")
    
    ## Page 2 content - conduct frequentist meta-analysis ##
    
    WalkFreq <- walkthrough_page_two_server(id = "page2", WalkData = WalkData)
    
    ## Page 3 content - calculate power of a new study ##
    
    walkthrough_page_three_server(id = "page3", WalkCalcResultsData = WalkCalcResultsData)
    
    ## Page 4 content - understanding the power calculation##
    
    walkthrough_page_four_server(id = "page4", WalkFreq = WalkFreq)
    
    ## Page 5 content - random vs fixed effects ##
    
    walkthrough_page_five_server(id = "page5", WalkFreq = WalkFreq)
    
    ## Page 6 content - illustrating how random effects affect power results ##
    
    walkthrough_page_six_server(id = "page6", WalkCalcResultsData = WalkCalcResultsData, 
                    WalkFreq = WalkFreq)
    
    ## page 7 content - frequentist vs Bayesian ##
    
    walkthrough_page_seven_server(id = "page7", WalkFreq = WalkFreq)
    
  })
}