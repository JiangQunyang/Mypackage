## Test data
S <- c(0,1,2,5,8,12,30,50)
V <- c(0,11.1,25.4,44.8,54.5,58.2,72.0,60.1)

## Read in to a data frame
ReadIn <- function(S, V){
  Inv.V <- 1/S # Lineweaver-Burk
  Inv.S <- 1/V
  V.div.S <- V/S # Eadie-Hofstee
  S.div.V <- S/V # Hanes-Woolf
  enz.data <- data.frame(S, V, Inv.V, Inv.S, V.div.S, S.div.V) # read into df
  return(enz.data)
}

## Regression
# http://www.dummies.com/education/math/statistics/how-to-calculate-a-regression-line/
# http://www1.lsbu.ac.uk/water/enztech/determination.html

# the double reciprocal [Lineweaver-Burk] plot is preferred to test 
# for the qualitative correctness of a proposed mechanism, and the 
# Eadie-Hofstee plot is preferred for discovering deviations from linearity

# Gradient 
