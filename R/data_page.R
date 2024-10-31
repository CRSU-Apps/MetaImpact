
# Functions to call based on selected sort criteria
.data_page_sort_criteria <- list(
  "File order" = function(df) NullSort(df),
  "Study name" = function(df) SortByStudyName(df),
  "Participant count" = function(df) SortByParticipantCount(df)
)

#' UI for the load data page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
data_page_ui <- function(id) {
  ns = NS(id)
  div(
    column(
      width = 4,
      # Insert own data or choose example data
      h4("Choose Data"),
      p("Please upload your data as a .csv or .xlsx file, formatted as described on the right-hand side of this page. Treatment coding (i.e. numbering rather than labels) and specifying whether the outcome is continuous or binary is not necessary."),
      fileInput(
        inputId = ns("data"),
        label = NULL,
        buttonLabel = "Select",
        accept = c(".csv", ".xlsx")
      ),
      selectInput(
        inputId = ns("sort_criteria"),
        label = div(
          title = "Sort the studies in the forest plots and other analysis outputs",
          "Sort studies by:",
          icon(name = "circle-question")
        ),
        choices = names(.data_page_sort_criteria),
        selectize = FALSE
      ),
      p("If you wish to explore the app without using your own data, you are welcome to choose one of the example datasets below."),
      p(
        "Example datasets are based on (network) meta-analyses reviewing the effect anti-vasuclar endothelial growth factor has on diabetic macular oedema. Visual acuity (VA) outcomes were reported and chosen for these examples. The continuous outcome example is extracted from a meta-analysis by Virgili et al which can be found ",
        a(href = "https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD007419.pub6/full", "here."),
        "The binary outcome example is extracted from a meta-analysis by Pham et al which can be found ",
        a(href = "https://bmjopen.bmj.com/content/9/5/e022031", "here.")
      ),
      radioButtons(
        inputId = ns("ChooseExample"),
        label = "Example Datasets Available",
        choices = c(
          "Binary outcome: Number of people that improved their best-corrected VA by gaining 15+ letters during a vision test" = "binaryEx",
          "Continuous outcome: Change in VA in terms of LogMAR (negative change in LogMAR = improved vision)" = "continuousEx"
        ),
        width = '100%'
      )
    ),
   
    column(
      width = 4,
      h4("View Data"),
      div(
        style = 'overflow-x: scroll',
        uiOutput(outputId = ns("data"))
      )
    ),
   
    column(
      width = 4,
      h4("Format Requirements"),
      
      tabsetPanel(
        id = 'format_instructions',
        
        tabPanel(
          title = "Binary Data",
          BinaryInstructionsPanelUi(id = ns("binary_instructions"))
        ),
    
        tabPanel(
          title = "Continuous Data",
          ContinuousInstructionsPanelUi(id = ns("continuous_instructions"))
        )
      )
    )
  )
}

#' Server for the load data page.
#'
#' @param id ID of the module.
#'
#' @return Reactive containing list of:
#' - data = loaded data frame.
#' - levels = factors of the data
data_page_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      BinaryInstructionsPanelServer(id = "binary_instructions")
      ContinuousInstructionsPanelServer(id = "continuous_instructions")
      
      # Read in user or default data
      loaded_data <- reactive({
        file <- input$data
        if (is.null(file)) {
          if (input$ChooseExample == 'continuousEx') {
            data <- rio::import("data/AntiVEGF_Continuous_Pairwise_Long.csv")
          } else {
            data <- rio::import("data/AntiVEGF_Binary_Pairwise_Long.csv")
          }
        } else {
          data <- rio::import(file = file$datapath)
        }
        
        return(data)
      })
      
      # Clean and sort data
      cleaned_data <- reactive({
        cleaned_data <- CleanData(loaded_data())
        # Sort data according to selected criteria
        cleaned_data <- .data_page_sort_criteria[[input$sort_criteria]](cleaned_data)
        
        return(cleaned_data)
      })
      
      wrangled_data <- reactive({
        WrangleUploadData(cleaned_data())
      })
      
      # Create a table which displays the raw data just uploaded by the user
      output$data <- renderTable({
        cleaned_data()
      })
      
      # Extract treatment names/levels
      data_levels <- reactive({
        return(
          levels(
            as_vector(
              lapply(
                wrangled_data()[grep(pattern = "^T", names(wrangled_data()), value = TRUE)],
                factor
              )
            )
          )
        )
      })
      
      return(
        reactive({
          list(
            data = wrangled_data(),
            levels = data_levels()
          )
        })
      )
    }
  )
}