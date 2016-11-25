## libraries
library(ggplot2)

## Test data
S <- c(30,50,0,1,2,5,8,12)
V <- c(72.0,60.1,0,11.1,25.4,44.8,54.5,58.2)

## Read in to a data frame
ReadIn <- function(S, V){
  Inv.V <- 1/V # Lineweaver-Burk
  Inv.S <- 1/S
  V.div.S <- V/S # Eadie-Hofstee
  S.div.V <- S/V # Hanes-Woolf
  enz.data <- data.frame(S, V, Inv.V, Inv.S, V.div.S, S.div.V) # read into df
  enz.data <- enz.data[order(S),]
  names(enz.data)[3] <- "1/V"
  names(enz.data)[4] <- "1/S"
  names(enz.data)[5] <- "V/S"
  names(enz.data)[6] <- "S/V"
  return(enz.data)
}
enz.data <- ReadIn(S, V)


## Km Vmax 
Km.Vmax <- function(enz.data){
  EHmodel <- lm(enz.data[,2] ~ enz.data[,5])
  Vmax <- EHmodel$coefficients[[1]]
  Km <- EHmodel$coefficients[[1]]/(-EHmodel$coefficients[[1]]/EHmodel$coefficients[[2]])
  data <- c(Vmax, Km)
  return(data)
}
Km.Vmax(enz.data)


## Eadie Hofstee plot function
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

## EH formula
EH.formula <- function(enz.data){
  EHmodel <- lm(enz.data[,2] ~ enz.data[,5])
  EH.Y.intercept <- paste("y intercept = (", 0,", ", (signif(EHmodel$coefficients[[1]], 3)), ")", collapse="", sep="")
  EH.X.intercept <- paste("x intercept = (", signif(c(-EHmodel$coefficients[[1]]/EHmodel$coefficients[[2]]), 3), ", ", 0, ")", collapse="", sep="")
  EH.equation <- paste("y = ", (signif(EHmodel$coefficients[[2]], 3)), "x + ", (signif(EHmodel$coefficients[[1]], 3)), collapse="", sep="")
  EH.data <- c(EH.X.intercept, EH.Y.intercept, EH.X.intercept, EH.equation)
  return(EH.data)
}
EH.formula(enz.data)
