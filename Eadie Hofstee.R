#Load ggplot2
library(ggplot2)


EadieHofstee <- function(enz.data){
  EHmodel <- lm(enz.data[,2] ~ enz.data[,5])
  Y.intercept <- c(0, EHmodel$coefficients[[1]])
  X.intercept <- c(-EHmodel$coefficients[[1]]/EHmodel$coefficients[[2]], 0)
  EHplot <-  ggplot(enz.data, aes(x= enz.data[,5], y= enz.data[,2])) + 
    geom_point(size=3, shape=20) +
    #geom_smooth(method=lm, formula = y ~ x, se=FALSE) +
    ggtitle("Eadie Hofstee Plot") +
    xlab(names(enz.data)[5]) +
    ylab(names(enz.data)[2]) +
    xlim(0, (X.intercept[1]*1.05)) +
    ylim(0, (Y.intercept[2]*1.05)) +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5)) +
    geom_abline(intercept = EHmodel$coefficients[[1]], slope = EHmodel$coefficients[[2]], color = "blue", size = 1)
  return(EHplot)
  # need to give formula, intercepts and Km and Vmax
}

#need to make activation of this conditional...
EadieHofstee(enz.data)

EHmodel <- lm(enz.data[,2] ~ enz.data[,5])