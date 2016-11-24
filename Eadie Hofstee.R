#Load ggplot2
library(ggplot2)


EadieHofstee <- function(enz.data){
  EHmodel <- lm(enz.data[,2] ~ enz.data[,5])
  fun_EH <- function(x) y = (EHmodel$coefficients[[2]]*x) + EHmodel$coefficients[[1]]
  EHplot <-  ggplot(enz.data, aes(x= enz.data[,5], y= enz.data[,2])) + 
    geom_point(size=3, shape=20) +
    #geom_smooth(method=lm, formula = y ~ x, se=FALSE) +
    ggtitle("Eadie Hofstee Plot") +
    xlab(names(enz.data)[5]) +
    ylab(names(enz.data)[2]) +
    xlim(0, max(enz.data[,5])) +
    ylim(0, max(enz.data[,2])) +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5)) +
    stat_function(fun=fun_EH,color="blue")
  return(EHplot)
  # need to give formula, intercepts and Km and Vmax
}

#need to make activation of this conditional...
EadieHofstee(enz.data)

EHmodel <- lm(enz.data[,2] ~ enz.data[,5])