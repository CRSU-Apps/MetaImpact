#### Langan Plots ####
library(metafor)
library(ggplot2)

### Test Data antibiotics vs. control for the common cold to alleviate symptoms by 7 days ###
raw_data <- data.frame(StudyID=c(1,2,3,4,5,6), Study=c("Herne_1980","Hoaglund_1950","Kaiser_1996","Lexomboon_1971","McKerrow_1961","Taylor_1977"),
                   R.1=c(7,39,97,8,5,12), N.1=c(7+39,39+115,97+49,8+166,5+10,12+117), T.1=rep("Treatment",6),
                   R.2=c(10,51,94,4,8,3), N.2=c(10+12,51+104,94+48,4+83,8+10,3+56), T.2=rep("Control",6))
# Obtain study effects and standard errors #
MAdata <- escalc(measure='OR', ai=R.1, bi=N.1-R.1, ci=R.2, di=N.2-R.2, data=raw_data)   # gives ES and seES on logOR scale
MAdata$sei <- sqrt((1/raw_data$R.1) + (1/(raw_data$N.1-raw_data$R.1)) + (1/raw_data$R.2) + (1/(raw_data$N.2-raw_data$R.2)))



## Piece together first plot (studies, summary diamond, contours) ###

Langan <- ggplot(MAdata, aes(x=yi, y=sei)) +
  #plot individual studies in current MA
  geom_point() +
  # formatting of plot
  labs(x = "Odds Ratio", y = "Standard Error", title = "Funnel contour plot") +
  theme_classic() +
  scale_x_continuous(expand=c(0,0)) +   # check R/Stata codes to see if they have a standard method of how wide to plot; also need to log-transform ticks
  scale_y_continuous(limits=c(0,1), expand=c(0,0))

Langan
