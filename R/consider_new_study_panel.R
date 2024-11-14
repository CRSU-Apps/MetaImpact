#' Module UI for 'consider new study' panel.
#' 
#' @param id ID of the module
#' @return Div for the home page
consider_new_study_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    br(),
    h3("Step 2: Consider what a new study may look like and it's potential impact"),
    br(),
    # Evidence Base Summary
    column(
      width = 5,
      align = 'center',
      bsCollapse(
        id = ns("EvidenceBase"),
        open = "Current Evidence Base",
        bsCollapsePanel(
          title = "Current Evidence Base",
          style = 'primary',
          h6("This panel presents the current evidence base from which the sample size calculations are based on. If you wish to change this, please alter the synthesis options above accordingly."),
          withSpinner(
            type = 6,
            plotOutput(outputId = ns("EvBase"))
          ),
          radioButtons(
            inputId = ns("EvBase_choice"),
            label = NULL,
            choices = c(
              "Frequentist MA" = "freq",
              "Bayesian MA" = "bayes"
            ),
            inline = TRUE
          ),
          fluidRow(
            div(
              style = "display: inline-block;",
              downloadButton(outputId = ns('evbase_download'), label = "Download forest plot")
            ),
            div(
              style = "display:inline-block; width: 10px;",
              br()
            ),
            div(
              style = "display: inline-block;",
              radioButtons(inputId = ns('evbase_choice'), label = NULL, choices = c('pdf', 'png'), inline = TRUE)
            )
          )
        )
      )
    ),
    column(
      width = 7,
      align = 'center',
      bsCollapse(
        id = ns("LanganPlot"),
        open = "Extended Funnel Plot",
        bsCollapsePanel(
          title = "Extended Funnel Plot",
          style = 'success',
          h6("This panel presents a funnel plot of the current evidence base. Options to extend the plot (below) encourage the user to consider where a new study may lie and it's potential impact."),
          withSpinner(
            type = 6,
            plotOutput(outputId = ns("Langan"))
          ),
          conditionalPanel(
            ns = NS(id),
            condition = "input.LanganOptions.includes('contour')",
            div(
              style = "position: absolute; right: 40px;",
              dropMenu(
                dropdownButton(
                  label = "Significance contours look strange?",
                  circle = FALSE,
                  size = 'xs'
                ),
                align = 'left',
                arrow = FALSE,
                h6("Limitations with the significance contours"),
                p("There exist two known 'artefacts' from the methods used to currently draw the significance contours. We apologise for this and will be working on solving these issues."),
                p(strong("Contours 'drop' at edges")),
                img(src = "www/PlotLim_edging.png", width = 200, align = "center"),
                p("The contours may appear to suddenly 'drop' to the bottom of the plot at the far edges. The contours should naturally follow the rest of the curve trajectory. "),
                p(strong("White triangle")),
                img(src = "www/PlotLim_triangle.png", width = 200, align = "center"),
                p("Most contour plots create a 'set of curtains' appearance. However, for some analyses, both contours may curve to the same side of the plot. In such cases, a white triangle may then be present within a shaded area. This triangle should match the colouring above/around it.")
              )
            )
          ),
          br(),
          checkboxGroupInput(
            inputId = ns("LanganOptions"),
            label = NULL,
            choices = c(
              "Null Line" = "plot.zero",
              "Pooled Effect Line" = "plot.summ",
              "Pooled Summary Diamond" = "summ",
              "Predictive Interval" = "pred.interval",
              "Significance Contours" = "contour"
            ),
            selected = c('plot.summ', 'summ'),
            inline = TRUE
          ),
          fluidRow(
            div(
              style = "display: inline-block;",
              radioButtons(
                inputId = ns('Lang_method'),
                label = "Fixed or Random Effects",
                choices = c(
                  'Fixed' = 'fixed',
                  'Random' = 'random'
                ),
                inline = TRUE
              )
            ),
            div(
              style = "display:inline-block; width: 10px;",
            ),
            div(
              style = "display: inline-block;",
              numericInput(inputId = ns('Lang_pvalue'), label = "Sig. level for contours", value = 0.05, min = 0, max = 1)
            ),
            div(
              style = "display: inline-block;vertical-align:top;",
              dropMenu(
                dropdownButton(size = 'xs',icon = icon('info')),
                align = 'left',
                h6("Information"),
                p("The Significance Contours will be calculated based on a desired outcome that the new evidence base has the following significance level (p-value).")
              )
            ),
            div(
              style = "display:inline-block; width: 10px;"
            ),
            div(
              style = "display: inline-block;",
              downloadButton(outputId = ns('Langan_download'), label = "Download Funnel plot")
            ),
            div(
              style = "display:inline-block; width: 10px;"
            ),
            div(
              style = "display: inline-block;",
              radioButtons(inputId = ns('langan_choice'), label = NULL, choices = c('pdf', 'png'), inline = TRUE)
            )
          )
        )
      )
    )
  )
}

#' Module server for 'consider new study' panel.
#' 
#' @param id ID of the module
#' @param freqpair Reactive object of frequentist analysis
#' @param plot_sims_btn Input selection to plot simulations onto Langan plot
#' @param impact_type_btn Input selection regarding type of impact
#' @param cutoff_btn Input selection for the cut-off for impact
#' @param sample_sizes_btn Input selection for the sample sizes
#' @param outcome The outcome type of the data (reactive)
#' @param CalcResults Reactive object of the calculator results (needed to obtain simulation results for plotting on Langan)
#' @return Input for choice of evidence base
consider_new_study_panel_server <- function(id, freqpair, plot_sims_btn, impact_type_btn, cutoff_btn, sample_sizes_btn, outcome, CalcResults) {     
  moduleServer(id, function(input, output, session) {
    
    # Forest plot of current evidence base #
    output$EvBase <- renderPlot({
      if (input$EvBase_choice == 'bayes') {
        NoBayesian()
      } else if (input$EvBase_choice == 'freq') {
        if (freqpair()$MA$MA.Fixed$measure %in% c('OR', 'RR')) {
          forest.rma(freqpair()$MA$MA.Fixed, atransf = exp, ylim = -2.5)
          addpoly(freqpair()$MA$MA.Random)
        } else {
          forest.rma(freqpair()$MA$MA.Fixed, ylim = -2.5)
          addpoly(freqpair()$MA$MA.Random)
        }
        title("Forest plot of studies and overall pooled estimates")
      }
    })
    
    # Langan Plot #
    output$Langan <- renderPlot({
      if ((input$Lang_method == 'random') && ('contour' %in% input$LanganOptions)) {   # significance contours not available for random-effects
        NoRandomContours()
      } else if ((input$Lang_method == 'fixed') && ('pred.interval' %in% input$LanganOptions)) {  # Warning how predictive intervals are not of use within fixed-effects models
        FixedPredInt()
      } else if ((plot_sims_btn()) && (impact_type_btn() != 'pvalue') && ('contour' %in% input$LanganOptions)) { # the significance contours only relate to p-value impact, whereas the power calculator has other options
        SigContourOnly()
      } else if ((plot_sims_btn()) && (impact_type_btn() == 'pvalue') && (!is.null(cutoff_btn())) && (input$Lang_pvalue != cutoff_btn()) && ('contour' %in% input$LanganOptions)) { # the contour cut-offs/sig levels need to be the same
        DiffSigValues()
      } else if ((plot_sims_btn()) && (length(as.integer(unlist(str_split(sample_sizes_btn(), ";"), use.names = FALSE))) > 1)) {   # haven't added functionlity to Langan plot yet so plot multiple sets of sample sizes
        NoPlotMultipleSampleSizes()
      } else {
        extfunnel(
          SS = freqpair()$MA$MAdata$yi,
          seSS = freqpair()$MA$MAdata$sei,
          method = input$Lang_method,
          outcome = outcome(),
          sig.level = input$Lang_pvalue,
          legend = TRUE,
          points = TRUE,
          contour = {'contour' %in% input$LanganOptions},
          summ = {'summ' %in% input$LanganOptions},
          pred.interval = {'pred.interval' %in% input$LanganOptions},
          plot.zero = {'plot.zero' %in% input$LanganOptions},
          plot.summ = {'plot.summ' %in% input$LanganOptions},
          expxticks = {
            if (outcome() %in% c('OR', 'RR')) {
              c(0.25, 0.5, 1, 2, 4)
            }
          },
          sim.points = {
            if (plot_sims_btn()) {
              CalcResults()$singleresult$sim_study
            }
          }
        )
        # remaining settings not in UI: contour.points, summ.pos, ylim, xlim, xticks, yticks, zero, xlab, ylab, legendpos
      }
    })
    
    # Downloads #
    # evidence base #
    output$evbase_download <- downloadHandler(
      filename = function() {
        paste0("EvidenceBase.", input$evbase_choice)
      },
      content = function(file) {
        if (input$evbase_choice == 'pdf') {
          pdf(file = file)
        } else {
          png(file = file)
        }
        if (input$EvBase_choice == 'freq') {
          if (freqpair()$MA$MA.Fixed$measure %in% c('OR', 'RR')) {
            forest.rma(freqpair()$MA$MA.Fixed, atransf = exp, ylim = -2.5)
            addpoly(freqpair()$MA$MA.Random)
          } else {
            forest.rma(freqpair()$MA$MA.Fixedz, ylim = -2.5)
            addpoly(freqpair()$MA$MA.Random)
          }
          title("Forest plot of studies and overal pooled estimates")
        }
        dev.off()
      }
    )
    
    # extended funnel plot #
    output$Langan_download <- downloadHandler(
      filename = function() {
        paste0('ExtFunnelPlot.', input$langan_choice)
      },
      content = function(file) {
        plot <- extfunnel(
          SS = freqpair()$MA$MAdata$yi,
          seSS = freqpair()$MA$MAdata$sei,
          method = input$Lang_method,
          outcome = outcome(),
          sig.level = input$Lang_pvalue,
          legend = TRUE,
          points = TRUE,
          contour = {'contour' %in% input$LanganOptions},
          summ = {'summ' %in% input$LanganOptions},
          pred.interval = {'pred.interval' %in% input$LanganOptions},
          plot.zero = {'plot.zero' %in% input$LanganOptions},
          plot.summ = {'plot.summ' %in% input$LanganOptions},
          expxticks = {
            if (outcome() %in% c('OR', 'RR')) {
              c(0.25, 0.5, 1, 2, 4)
            }
          },
          sim.points = {
            if (plot_sims_btn()) {
              CalcResults()$singleresult$sim_study
            }
          }
        )
        if (input$langan_choice == 'png') {
          ggsave(file, plot, height = 7, width = 12, units = "in", device = "png")
        } else {
          ggsave(file, plot, height = 7, width = 12, units = "in", device = "pdf")
        }
      }
    )
    
    return(reactive({ input$EvBase_choice }))
    
  })
}