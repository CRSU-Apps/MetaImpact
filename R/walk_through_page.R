#' Module UI for the walk-through page.
#' 
#' @param id ID of the module
#' @return Div for the home page
walk_through_page_ui <- function(id) {
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
          h2("Fixed-effects meta-analysis (frequentist framework)"),
          h4("The forest plot below summarises the current evidence base, under a fixed-effects model and frequentist framework."),
          h4("Currently, the meta-analysis is showing an improvement in visual acuity when receiving RANI, but this is not statistically significant (i.e. the odds ratio 95% confidence interval (indicated by diamond at the bottom of the plot) crosses the line of no effect)."),
          br(),
          withSpinner(
            type = 6,
            plotOutput(outputId = ns('page2Forest'))
          )
        ),
        
        div(
          class = 'page',
          id = ns('page3'),
          fluidRow(
            h2("Design new trial to have impact on current evidence base"),
            h4(
              "It can be argued that if a new trial is being designed with the same treatments and outcomes, it should be designed in such a way that is has an impact on the current evidence base. 
                For example, if a current meta-analysis has a non-statistically significant result, a new trial should be designed in such a way that when it is added to the current meta-analysis (evidence base) it results in a statistically significant result."
            ),
            h4("Lets design a new trial such that when its added to the example meta-analysis, we make an impact on the resulting evidence base. The current p-value is 0.379, lets aim to have a resulting meta-analysis with p-value of 0.15 or less."),
            h4(
              "We're going to keep things simple and assume a trial with two arms with equal numbers of participants, therefore, we just need to calculate the total sample size. 
                The calculator runs many simulations, where for each iteration: a new study is simulated (that has an (underlying) effect consistent with the current meta-analysis outcome estimate); added to the meta-analysis; and the resulting 'impact' (here we're looking at the p-value) is assessed."
            ),
            h4("Press the 'run' button to start the simulations!")
          ),
          br(),
          fluidRow(
            actionButton(inputId = ns("WalkFreqCalcRun3"), label = "Run Sample Size Calculations", class = "btn-primary btn-lg")
          ),
          br(),
          fluidRow(
            conditionalPanel(
              ns = NS(id),
              condition = "input.WalkFreqCalcRun3 != 0",
              h4(
                "The 'power' to have the desired impact (p-value of 0.15 or less), calculated as the % of simulations when the addition of the simulated study reduced the p-value to 0.15 or less, was calculated for total sample sizes of 250, 500, 750, and 1000.
                   The number of iterations/simulations to calculate the power was set at 100 (i.e. 100 updated meta-analyses were conducted, each with a 'new trial').
                   If we had increased the number of iterations, our power estimates would be more accurate (i.e. smaller 95% CI), but be more computationally expensive."
              ),
              br(),
              h4("The power results for each sample size are available below"),
              br(),
              column(
                width = 5,
                withSpinner(
                  type = 6,
                  tableOutput(outputId = ns("page3powtable"))
                )
              ),
              column(
                width = 7,
                withSpinner(
                  type = 6,
                  plotOutput(outputId = ns('page3powplot'))
                )
              )
            )
          )
        ),
        
        div(
          class = 'page',
          id = ns('page4'),
          h2("Visually understanding the power calculation"),
          h4("We saw that new trials of sample sizes 250, 500, 750, and 1,000 each gave 3%, 9%, 19%, and 22% power of giving the desired impact on the new evidence base/meta-analysis (p-value of 0.15 or less). But how were those values calculated?"),
          h4(
            "Below is a funnel plot of the original studies (outcome effect (odds ratio) versus the standard error (which is related to sample size)). 
              Shaded regions (defined by contour equations) can be added to illustrate areas of the plot that would give a desired impact (e.g. defined here as an updated meta-analysis with p-value < 0.15) from including a 'new' simulated trial with the corresponding effect size and standard error. Press the button below to add these regions."
          ),
          br(),
          fluidRow(
            column(
              width = 10,
              withSpinner(
                type = 6,
                plotOutput(outputId = ns('page4Langan'))
              )
            ),
            column(
              width = 2,
              conditionalPanel(
                ns = NS(id),
                condition = "input.WalkAddSims != 0",
                selectInput(
                  inputId = ns('WalkSizeChoice'),
                  label = "Choose sample size",
                  choices = c('250', '500', '750', '1000'),
                  selected = '1000'
                ),
                checkboxInput(inputId = ns("WalkZoomSims"), label = "Zoom in on Simulations", value = FALSE)
              )
            )
          ),
          conditionalPanel(
            ns = NS(id),
            condition = "input.WalkAddContours == 0",
            actionButton(inputId = ns("WalkAddContours"), label = "Add Significance Regions", class = "btn-primary btn-large")
          ),
          br(),
          conditionalPanel(
            ns = NS(id),
            condition = "input.WalkAddContours != 0",
            h4(
              "We can now see what trial treatment effects (i.e. effect size and standard error) would lead to certain impacts on the new evidence base. Details of how the contour equations for defining these regions are calculated can be found in the paper by Langan et al ",
              a(href = "https://www.jclinepi.com/article/S0895-4356(11)00327-1/fulltext", "here.")
            ),
            h4("Now let's add onto the plot treatment effects from all 100 simulated 'new trials' of sample size 1,000 that were used for the power calculation. Press the button below to add the simulations (once loaded you may want to click the zoom option to make this clearer)."),
            conditionalPanel(
              ns = NS(id),
              condition = "input.WalkAddSims == 0",
              actionButton(inputId = ns("WalkAddSims"), label = "Add Simulations", class = "btn-primary btn-large")
            )
          ),
          br(),
          conditionalPanel(
            ns = NS(id),
            condition = "input.WalkAddSims != 0",
            h4("We can now see that of the 100 simulated new trials (iterations of the calculator), 22 of them are within the shaded region indicating the desired impact (a significant effect greater than the null effect at the 15% level, i.e. the region of darkest shading). Therefore, the power of a new study of sample size 1,000 having the desired impact is 22%."),
            h4("Why not select one of the other sample sizes tested (from the drop down menu to the right of the plot) and see how the simulated trials fall on the different regions.")
          )
        ),
        
        div(
          class = 'page',
          id = ns('page5'),
          h2("Random-effects versus Fixed-effects"),
          h4("The fixed-effects model assumes that every study in the meta-analysis has the same underlying treatment effect (i.e. they only vary due to random error). However, this assumption is not always suitable."),
          h4("Instead, a random-effects model assumes that every study has it's own underlying treatment effect, but that each of these underlying effects come from a common/shared distribution (i.e. variation isn't purely due to random error, but the underlying effects are 'similar')."),
          br(),
          fluidRow(
            column(
              width = 4,
              h4("In this example, when we fit a random-effects model, the between study heterogeneity (on the log-odds ratio scale) was estimated at 0.033 (a fixed-effects model forces this to equal 0). The presence of heterogeneity can infer that a random-effects model may be a better 'fit'."),
              h4("The forest plot to the right shows both the fixed-effects and random-effects results. The point estimates are very similar, but the random-effects estimate has a slightly wider confidence interval (due to the added variance from the between-study heterogeneity).")
            ),
            column(
              width = 7,
              offset = 1,
              withSpinner(
                type = 6,
                plotOutput(outputId = ns('page5Forest'))
              )
            )
          )
        ),
        
        div(
          class = 'page',
          id = ns('page6'),
          h2("How does the random-effects model affect the sample size calculations?"),
          h4("A natural follow-on question is how does the choice of moving from fixed-effects to random-effects affect the power calculations from the sample size calculator."),
          br(),
          actionButton(inputId = ns("WalkFreqCalcRun6"), label = "Run Sample Size Calculations", class = "btn-primary btn-lg"),
          br(),
          conditionalPanel(
            ns = NS(id),
            condition = "input.WalkFreqCalcRun6 != 0",
            fluidRow(
              column(
                width = 3,
                h4(
                  "The power is much lower when using a random-effects model! A primary reason for this reduction is that when confidence intervals/p-values are calculated, they include both within- and between-study variation - this is what leads to the wider confidence intervals.
                    In turn, it is then 'harder' to move the pooled treatment effects to the desired levels of statistical significance. Even with the largest study sizes, the between-study variation is still present and driving this 'conundrum'."
                ),
                h4(
                  "You can find out more statistical details about this in the paper by Sutton et al ",
                  a(href = "https://onlinelibrary.wiley.com/doi/10.1002/sim.2704", "here.")
                ),
                br(),
                h4("An extra element is that the simulated 'new trials' behave slightly differently under the random-effects model."),
                h4(
                  "When simulating a new trial, the calculator uses parameters from the current meta-analysis, this includes whether it was fitted under a fixed- or random-effects model. 
                    As such, the 'new trials' are allowed to 'vary' more under the random-effects model, to mirror the assumption of each trial having their own underlying study effect."
                ),
                conditionalPanel(
                  ns = NS(id),
                  condition = "input.WalkFreqRandomSims == 0",
                  actionButton(inputId = ns("WalkFreqRandomSims"), label = "Let's see with our example...", class = "btn-primary btn-large")
                )
              ),
              column(
                width = 4,
                align = 'center',
                offset = 1,
                h4("Fixed-effects results"),
                withSpinner(
                  type = 6,
                  tableOutput(outputId = ns("page6fixpowtable"))
                ),
                withSpinner(
                  type = 6,
                  plotOutput(outputId = ns("page6fixpowplot")),
                )
              ),
              column(
                width = 4,
                align = 'center',
                h4("Random-effects results"),
                withSpinner(
                  type = 6,
                  tableOutput(outputId = ns("page6ranpowtable"))
                ),
                withSpinner(
                  type = 6,
                  plotOutput(outputId = ns("page6ranpowplot"))
                )
              )
            )
          ),
          conditionalPanel(
            ns = NS(id),
            condition = "input.WalkFreqRandomSims != 0",
            fluidRow(
              column(
                width = 3,
                h4("This can be seen in the funnel plots to the right. The simulated studies (for sample size 1,000) are more spread out (in terms of resulting odds ratio) under the random-effects model than the fixed-effects."),
                h4("In the random-effects model, the predictive interval is also plotted which represents the likely result of a new study. This is the same as the confidence interval under the fixed-effects model as any extra variablility due to between-study differences is set to 0.")
              ),
              column(
                width = 4,
                align = 'center',
                offset = 1,
                h4("Fixed-effects results"),
                withSpinner(
                  type = 6,
                  plotOutput(outputId = ns("page6LanganFix"))
                )
              ),
              column(
                width = 4,
                align = 'center',
                h4("Random-effects results"),
                withSpinner(
                  type = 6,
                  plotOutput(outputId = ns("page6LanganRan"))
                )
              )
            )
          )
        ),
        
        div(
          class = 'page',
          id = ns('page7'),
          h2("Bayesian versus Frequentist framework"),
          h4("Up until now, this walk-through was using a frequentist framework. But would there be any difference in using a Bayesian framework?"),
          br(),
          h4("Often, researchers like to use a Bayesian framework as it is more flexible towards more complex models."),
          h4("Below you can see the forest plots for our example random-effects models under a frequentist and Bayesian framework - the estimates are very similar."),
          h4("With regards to meta-analysis, a main difference between these frameworks is how the between-study heterogeneity is modelled. Under a frequentist framework, it is modelled with a single number. However, under a Bayesian framework, it can be modelled with a distribution to represent the uncertainty in the estimation of the heterogeneity (which can be considerable)."),
          h4("This will have a knock-on effect on the sample size calculations - an effect that could be argued to be more accurate."),
          br(),
          h5("Unfortunately, MetaImpact doesn't currently have Bayesian functionality for the Calculator element, however, this is in development."),
          br(),
          fluidRow(
            align = 'center',
            column(
              width = 6,
              h4("Frequentist"),
              withSpinner(
                type = 6,
                plotOutput(outputId = ns('page7ForestFreq'))
              )
            ),
            column(
              width = 6,
              h4("Bayesian"),
              withSpinner(
                type = 6,
                plotOutput(outputId = ns('page7ForestBayes'))
              )
            )
          )
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
walk_through_page_server <- function(id) {
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
    
    ## Dataset ##
    
    WalkData <- rio::import("data/AntiVEGF_Binary_Pairwise_Long.csv") %>%
      CleanData() %>%
      WrangleUploadData()
    
    ## Page 2 content ##
    
    WalkData <- SwapTrt(CONBI = 'binary', data = Long2Wide(WalkData), trt = 'RANI')
    WalkFreq <- FreqPair(data = WalkData, outcome = 'OR', model = 'both', CONBI = 'binary')  #conduct frequentist MA
    
    output$page2Forest <- renderPlot({
      metafor::forest(WalkFreq$MA.Fixed, atransf = exp, at = log(c(0.05, 0.25, 1, 4, 16)))
      title("Forest plot of studies with overall estimate from fixed-effects model")
    })
    
    ## Page 3 content ##
    
    WalkCalcResultsData <- rio::import("WalkThroughResults.csv")
    
    output$page3powplot <- renderPlot({
      metapowplot(PowerData = WalkCalcResultsData, ModelOpt = 'fixed', SampleSizes = c(250, 500, 750, 1000))
    })
    
    output$page3powtable <- renderTable({
      powdata <- WalkCalcResultsData[WalkCalcResultsData$Model == 'Fixed-effects', c(1, 3:5)]
      names(powdata) <- c("Total Sample Size", "Power estimate (%)", "Lower 95% CI bound", "Upper 95% CI bound")
      powdata
    }, digits = 1)
    
    ## Page 4 content ##
    
    WalkSims <- reactive({
      data <- rio::import(paste0("WalkThroughSims", input$WalkSizeChoice, ".csv"))
      return(data)
    })
    
    WalkZoomValues <- data.frame(
      xlim = list(
        S250 = log(c(0.4, 3)),
        S500 = log(c(0.5, 2.2)),
        S750 = log(c(0.5, 2.2)),
        S1000 = log(c(0.6, 2))
      ),
      ylim = list(
        S250 = c(0.2, 0.4),
        S500 = c(0.1, 0.3),
        S750 = c(0, 0.3),
        S1000 = c(0, 0.25)
      )
    )
    
    output$page4Langan <- renderPlot({
      extfunnel(
        SS = WalkFreq$MAdata$yi,
        seSS = WalkFreq$MAdata$sei,
        method = 'fixed',
        outcome = 'OR',
        expxticks = c(0.25, 0.5, 1, 2, 4),
        xlab = "Odds Ratio",
        legend = TRUE,
        contour = input$WalkAddContours != 0,
        sig.level = 0.15,
        sim.points = {
          if (input$WalkAddSims != 0) {
            WalkSims()
          }
        },
        xlim = {
          if (input$WalkZoomSims) {
            WalkZoomValues[[paste0('xlim.S', input$WalkSizeChoice)]]
          }
        },
        ylim = {
          if (input$WalkZoomSims) {
            WalkZoomValues[[paste0('ylim.S', input$WalkSizeChoice)]]
          }
        }
      )
    })
    
    ## Page 5 content ##
    
    output$page5Forest <- renderPlot({
      forest.rma(
        WalkFreq$MA.Fixed,
        atransf = exp,
        ylim = -2.5,
        at = log(c(0.05, 0.25, 1, 4, 16))
      )
      addpoly(WalkFreq$MA.Random)
      title("Forest plot of studies and overall pooled estimates")
    })
    
    ## Page 6 content ##
    
    
    output$page6fixpowtable <- renderTable({
      powdata <- WalkCalcResultsData[WalkCalcResultsData$Model == 'Fixed-effects', c(1, 3:5)]
      names(powdata) <- c("Total Sample Size", "Power estimate (%)", "Lower 95% CI bound", "Upper 95% CI bound")
      powdata
    }, digits = 1)
    
    output$page6fixpowplot <- renderPlot({
      metapowplot(PowerData = WalkCalcResultsData, ModelOpt = 'fixed', SampleSizes = c(250, 500, 750, 1000))
    })
    
    output$page6ranpowtable <- renderTable({
      powdata <- WalkCalcResultsData[WalkCalcResultsData$Model == 'Random-effects', c(1, 3:5)]
      names(powdata) <- c("Total Sample Size", "Power estimate (%)", "Lower 95% CI bound", "Upper 95% CI bound")
      powdata
    }, digits = 1)
    
    output$page6ranpowplot <- renderPlot({
      metapowplot(PowerData = WalkCalcResultsData, ModelOpt = 'random', SampleSizes = c(250, 500, 750, 1000))
    })
    
    WalkSims1000 <- rio::import("WalkThroughSims1000.csv")
    
    output$page6LanganFix <- renderPlot({
      extfunnel(
        SS = WalkFreq$MAdata$yi,
        seSS = WalkFreq$MAdata$sei,
        method = 'fixed',
        outcome = 'OR',
        expxticks = c(0.25, 0.5, 1, 2, 4),
        xlab = "Odds Ratio",
        legend = TRUE,
        sim.points = WalkSims1000,
        xlim = log(c(0.5, 2.5)),
        ylim = c(0, 0.25)
      )
    })
    
    output$page6LanganRan <- renderPlot({
      extfunnel(
        SS = WalkFreq$MAdata$yi,
        seSS = WalkFreq$MAdata$sei,
        method = 'random',
        outcome = 'OR',
        expxticks = c(0.25, 0.5, 1, 2, 4),
        xlab = "Odds Ratio",
        legend = TRUE,
        sim.points = WalkSims1000,
        pred.interval = TRUE,
        xlim = log(c(0.5, 2.5)),
        ylim = c(0, 0.25)
      )
    })
    
    ## page 7 content ##
    
    output$page7ForestFreq <- renderPlot({
      metafor::forest(WalkFreq$MA.Random, atransf = exp, at = log(c(0.05, 0.25, 1, 4, 16)))
      title("Forest plot of studies with overall estimate from fixed-effects model")
    })
    
    
    load("BayesForestRand.rdata")
    
    output$page7ForestBayes <- renderPlot({
      BayesRandomForest
    })
  })
}