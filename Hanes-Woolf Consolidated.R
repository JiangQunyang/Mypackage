

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
enz.data <- ReadIn(S, V)

#HANES WOOLF PLOT FUNCTION 

HanesWoolf <-function(enz.data){
  
  library(ggplot2)  
  library(ggpmisc)
 HWdata <- data.frame(enz.data[,1], enz.data[,6])  
 HWplot <-  ggplot(HWdata, aes(x=HWdata[,1] , y=HWdata[,2])) +
    geom_point(size=3, shape=20) +
    ggtitle("Hanes-Woolf Plot") +
   
    ylim(0, max(HWdata[,2])) +
    xlab("[S]") +
    ylab("[S/v]") +
    theme_bw()+
    theme(plot.title = element_text(hjust=0.5))+
   geom_smooth(method=lm, formula = y ~ x, se=FALSE)
 return(HWplot)
 }
    
#PRINT 
 
HanesWoolf(enz.data)

#####IGNORE THIS UNLESS YOU HAVE ANY IDEA WHAT YOU ARE DOING #####

REGRESS <- lm(enz.data[,6] ~ enz.data[,1], data = enz.data)

REGRESS

attributes(REGRESS)


REGRESS$coefficients[1]
REGRESS$coefficients[[1]]
REGRESS$coefficients[2]
REGRESS$coefficients[[2]]

 