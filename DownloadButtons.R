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

# I think these functions could be much smaller by just calling the freqpair() and bayespair() list elements that relate to forest/trace plots?

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
    if (input$FixRand=='fixed') { 
      plot <- BayesPairForest(bayespair()$MA$MAdata, outcome=outcome(), model='fixed')
      plot <- plot + ggtitle("Forest plot of studies with overall estimate from fixed-effects model") +
        theme(plot.title = element_text(hjust = 0.5))
    } else if (input$FixRand=='random') {
      plot <- BayesPairForest(bayespair()$MA$MAdata, outcome=outcome(), model='random')
      plot <- plot + ggtitle("Forest plot of studies with overall estimate from random-effects model") +
        theme(plot.title = element_text(hjust = 0.5))
    }
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
    if (input$FixRand=='fixed') { 
      plot <- stan_trace(bayespair()$MA$MA.Fixed$fit, pars="theta")
      plot <- plot + ggtitle("Trace plot of the pooled estimate over iterations") +
        theme(plot.title = element_text(hjust = 0.5))
    } else if (input$FixRand=='random') {
      plot <- stan_trace(bayespair()$MA$MA.Random$fit, pars=c("theta","tau"))
      plot <- plot + ggtitle("Trace plot of the pooled estimate and between-study SD over iterations") +
        theme(plot.title = element_text(hjust = 0.5))
    }
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



