#Load ggplot2
library(ggplot2)

#Eadie Hofstee plot function
EadieHofstee <- function(enz.data){
  EHmodel <- lm(enz.data[,2] ~ enz.data[,5])
  EH.Y.intercept <- c(0, EHmodel$coefficients[[1]])
  EH.X.intercept <- c(-EHmodel$coefficients[[1]]/EHmodel$coefficients[[2]], 0)
  EH.equation <- paste("y = ", (signif(EHmodel$coefficients[[2]], 3)), "x + ", (signif(EHmodel$coefficients[[1]], 3)), collapse="", sep="")
  EHplot <-  ggplot(enz.data, aes(x= enz.data[,5], y= enz.data[,2])) + 
    geom_point(size=3, shape=20) +
    ggtitle("Eadie Hofstee Plot") +
    xlab(names(enz.data)[5]) +
    ylab(names(enz.data)[2]) +
    xlim(0, (EH.X.intercept[1]*1.05)) +
    ylim(0, (EH.Y.intercept[2]*1.05)) +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5)) +
    geom_abline(intercept = EHmodel$coefficients[[1]], slope = EHmodel$coefficients[[2]], color = "blue", size = 1)
  return(EHplot)
  return(EH.equation)
}

EadieHofstee(enz.data)

# EH formula
EH.formula <- function(enz.data){
  EHmodel <- lm(enz.data[,2] ~ enz.data[,5])
  EH.Y.intercept <- paste("y intercept = (", 0,", ", (signif(EHmodel$coefficients[[1]], 3)), ")", collapse="", sep="")
  EH.X.intercept <- paste("x intercept = (", signif(c(-EHmodel$coefficients[[1]]/EHmodel$coefficients[[2]]), 3), ", ", 0, ")", collapse="", sep="")
  EH.equation <- paste("y = ", (signif(EHmodel$coefficients[[2]], 3)), "x + ", (signif(EHmodel$coefficients[[1]], 3)), collapse="", sep="")
  EH.data <- c(EH.X.intercept, EH.Y.intercept, EH.X.intercept, EH.equation)
  return(EH.data)
}
EH.formula(enz.data)