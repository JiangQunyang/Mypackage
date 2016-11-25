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


## Regression
# http://www.dummies.com/education/math/statistics/how-to-calculate-a-regression-line/
# http://www1.lsbu.ac.uk/water/enztech/determination.html

Km.Vmax <- function(enz.data){
  EHmodel <- lm(enz.data[,2] ~ enz.data[,5])
  Vmax <- EHmodel$coefficients[[1]]
  Km <- EHmodel$coefficients[[1]]/(-EHmodel$coefficients[[1]]/EHmodel$coefficients[[2]])
  data <- c(Vmax, Km)
  return(data)
}
Km.Vmax(enz.data)
