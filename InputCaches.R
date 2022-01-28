### Separate file for the caching of inputs inbetween each time the calculator is run ###
### Having to do it like this as I never solved how to run it in a loop of some sort ###
### I tried lapply as stackoverlow posts suggest, but I could never access the resulting reactive Values successfully ###
### Done 50 times as the hope of the user never going beyond this within one session ###
### Run-time shouldn't be affected too badly due to the if statement ###

observe(
  if (input$CalcRun==0) {
    tmpInputs[['1']] <- inputCache()
  })
observe(
  if (input$CalcRun==1) {
    tmpInputs[['2']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==2) {
    tmpInputs[['3']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==3) {
    tmpInputs[['4']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==4) {
    tmpInputs[['5']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==5) {
    tmpInputs[['6']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==6) {
    tmpInputs[['7']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==7) {
    tmpInputs[['8']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==8) {
    tmpInputs[['9']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==9) {
    tmpInputs[['10']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==10) {
    tmpInputs[['11']] <- inputCache()
  })
observe(
  if (input$CalcRun==11) {
    tmpInputs[['12']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==12) {
    tmpInputs[['13']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==13) {
    tmpInputs[['14']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==14) {
    tmpInputs[['15']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==15) {
    tmpInputs[['16']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==16) {
    tmpInputs[['17']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==17) {
    tmpInputs[['18']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==18) {
    tmpInputs[['19']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==19) {
    tmpInputs[['20']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==20) {
    tmpInputs[['21']] <- inputCache()
  })
observe(
  if (input$CalcRun==21) {
    tmpInputs[['22']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==22) {
    tmpInputs[['23']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==23) {
    tmpInputs[['24']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==24) {
    tmpInputs[['25']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==25) {
    tmpInputs[['26']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==26) {
    tmpInputs[['27']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==27) {
    tmpInputs[['28']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==28) {
    tmpInputs[['29']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==29) {
    tmpInputs[['30']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==30) {
    tmpInputs[['31']] <- inputCache()
  })
observe(
  if (input$CalcRun==31) {
    tmpInputs[['32']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==32) {
    tmpInputs[['33']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==33) {
    tmpInputs[['34']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==34) {
    tmpInputs[['35']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==35) {
    tmpInputs[['36']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==36) {
    tmpInputs[['37']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==37) {
    tmpInputs[['38']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==38) {
    tmpInputs[['39']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==39) {
    tmpInputs[['40']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==40) {
    tmpInputs[['41']] <- inputCache()
  })
observe(
  if (input$CalcRun==41) {
    tmpInputs[['42']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==42) {
    tmpInputs[['43']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==43) {
    tmpInputs[['44']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==44) {
    tmpInputs[['45']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==45) {
    tmpInputs[['46']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==46) {
    tmpInputs[['47']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==47) {
    tmpInputs[['48']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==48) {
    tmpInputs[['49']] <- inputCache()
  }
)
observe(
  if (input$CalcRun==49) {
    tmpInputs[['50']] <- inputCache()
  }
)