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



### NMA ###
  #-----#

# To do once ironed out NMA functionality #



