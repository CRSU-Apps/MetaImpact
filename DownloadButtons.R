### Download Buttons ###

### Sample Size Calculator ###
  #------------------------#

# power plot #
output$powplot_download <- downloadHandler(
  filename = function() {
    paste0('PowerPlot.', input$powplot_choice)
  },
  content = function(file) {
    plot <- metapowplot(PowerData=CalcResults()$data, ModelOpt=input$powplot_options, SampleSizes=CalcResults()$sample_sizes)
    if (input$powplot_choice=='png') {
      ggsave(file,plot, height=7, width=12, units="in", device="png")
    } else {
      ggsave(file,plot, height=7, width=12, units="in", device="pdf")
    }
  }
)

# power data #
output$powtable_download <- downloadHandler(
  filename = function() {
    paste0('PowerData.csv')
  },
  content = function(file) {
    write.csv({
      powdata <- CalcResults()$data
      names(powdata) <- c("Total Sample Size", "Model", "Power estimate (%)", "Lower 95% CI bound", "Upper 95% CI bound")
      powdata}, file, row.names=FALSE, col.names=TRUE)
  }
)

# evidence base #
output$evbase_download <- downloadHandler(
  filename = function() {
    paste0("EvidenceBase.", input$evbase_choice)
  },
  content = function(file) {
    if (input$evbase_choice=='pdf') {pdf(file=file)}
    else {png(file=file)}
    if (input$EvBase_choice=='freq') {
      if (freqpair()$MA$MA.Fixed$measure %in% c('OR','RR')) {
        forest.rma.CN(freqpair()$MA$MA.Fixed, freqpair()$MA$MA.Random, atransf=exp)
      } else {
        forest.rma.CN(freqpair()$MA$MA.Fixed, freqpair()$MA$MA.Random)
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
    plot <- extfunnel(SS = freqpair()$MA$MAdata$yi, seSS = freqpair()$MA$MAdata$sei, method = input$Lang_method, outcome = outcome(),
                      sig.level = input$Lang_pvalue, legend = TRUE, points = TRUE,
                      contour = {'contour' %in% input$LanganOptions}, summ = {'summ' %in% input$LanganOptions}, pred.interval = {'pred.interval' %in% input$LanganOptions}, plot.zero = {'plot.zero' %in% input$LanganOptions}, plot.summ = {'plot.summ' %in% input$LanganOptions},
                      expxticks = {if (outcome() %in% c('OR','RR')) {c(0.25,0.5,1,2,4)}},
                      sim.points = {if (input$plot_sims) {CalcResults()$singleresult$sim_study}})
    if (input$langan_choice=='png') {
      ggsave(file,plot, height=7, width=12, units="in", device="png")
    } else {
      ggsave(file,plot, height=7, width=12, units="in", device="pdf")
    }
  }
)


### Pairwise Meta-Analysis ###
  #------------------------#


output$forestpairF_download <- downloadHandler(
  filename = function() {
    paste0("PairwiseAnalysis.", input$forestpairF_choice)
  },
  content = function(file) {
    if (input$forestpairF_choice=='pdf') {pdf(file=file)}
    else {png(file=file)}
    if (input$FixRand=='fixed') { 
      if (outcome()=='OR' | outcome()=='RR') {
          forest(freqpair()$MA$MA.Fixed, atransf=exp)
          title("Forest plot of studies with overall estimate from fixed-effects model")
      } else {
          forest(freqpair()$MA$MA.Fixed)
          title("Forest plot of studies with overall estimate from fixed-effects model")
      }
    } else {
      if (outcome()=='OR' | outcome()=='RR') {
          forest(freqpair()$MA$MA.Random, atransf=exp)
          title("Forest plot of studies with overall estimate from random-effects model")
      } else {
          forest(freqpair()$MA$MA.Random)
          title("Forest plot of studies with overall estimate from random-effects model")
      }
    }
    dev.off()
  }
)

output$forestpairB_download <- downloadHandler(
  filename = function() {
    paste0("PairwiseAnalysis.", input$forestpairB_choice)
  },
  content = function(file) {
    plot <- bayespair()$Forest
    if (input$forestpairB_choice=='png') {
      ggsave(file, plot, height=7, width=12, units="in", device="png")
    } else {
      ggsave(file, plot, height=7, width=12, units="in", device="pdf")
    }
  }
)

output$tracepair_download <- downloadHandler(
  filename = function() {
    paste0("PairwiseTrace.", input$tracepair_choice)
  },
  content = function(file) {
    plot <- bayespair()$Trace
    if (input$forestpairB_choice=='png') {
      ggsave(file, plot, height=7, width=12, units="in", device="png")
    } else {
      ggsave(file, plot, height=7, width=12, units="in", device="pdf")
    }
  }
)





### NMA ###
  #-----#

# To do once ironed out NMA functionality #



