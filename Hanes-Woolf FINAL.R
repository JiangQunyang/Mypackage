

## Test data
S <- c(0,1,2,5,8,12,30,50)
V <- c(0,11.1,25.4,44.8,54.5,58.2,72.0,60.1)

#Alternative Test Data
#S <- c(2.5,3.5,5,10,20,50)
#V <- c(32.3,40,50.8,72,87.7,115.4)

## Read in to a data frame
ReadIn <- function(S, V){
  Inv.V <- 1/V # Lineweaver-Burk (y)
  Inv.S <- 1/S #Lineweaver-Burk (x)
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



#### Hanes Woolf Plot Function ####

HanesWoolf <-function(enz.data){
  
  library(ggplot2)  
  
  if (enz.data[1,1]==0 && enz.data[1,2]==0) enz.data<-enz.data[-1,]    # if the first measurement S or V is the enz.data dataframe is equal to 0, the whole row gets deleted because the linear model cannot deal with values that are NA/NaN/Inf
                                                                       #which is what you get if you try and divide things by 0 

  HWdata <- data.frame(enz.data[,1], enz.data[,6])  
  
  HWmodel <- lm(enz.data[,6] ~ enz.data[,1]) 
  Y.intercept <- c(0, HWmodel$coefficients[[1]])
  X.intercept <- c(-HWmodel$coefficients[[1]]/HWmodel$coefficients[[2]], 0)
  
  HWplot <-  ggplot(HWdata, aes(x=enz.data[,1] , y=enz.data[,6])) +
    
    geom_point(size=3, shape=20) +
    
    ggtitle("Hanes-Woolf Plot") +
    
    ylim(0, max(HWdata[,2])) +
    xlab("[S]") +
    ylab("[S/v]") +
    xlim(X.intercept[1], max(enz.data[,1])) +
    ylim(0, max(enz.data[,6])) +
    theme_bw()+
    theme(plot.title = element_text(hjust=0.5))+
    
    geom_abline(intercept = HWmodel$coefficients[[1]], slope = HWmodel$coefficients[[2]], color = "blue", size = 1)+
    geom_vline(xintercept = 0, size=0.5)
  
   
  return(HWplot)
}

#PRINT 

HanesWoolf(enz.data)

###### It sometimes gives the message: "Scale for 'y' is already present. Adding another scale for 'y', which will replace the existing scale."
    #after producing the graph. However, this doesn't seem to have any effect on the function so it can be ignored. 


# HW formula  -Gives the equation of the line as well as the x and y intercepts  

HW.formula <- function(enz.data){
 
   if (enz.data[1,1]==0 && enz.data[1,2]==0) enz.data<-enz.data[-1,] #again, we have to get rid of any 0 values because it can't make a model when there are NA/NaN/Inf values in 'x'
  
  HWmodel <- lm(enz.data[,6] ~ enz.data[,1]) 
  HW.Y.intercept <- paste("y intercept = (", 0,", ", (signif(HWmodel$coefficients[[1]], 3)), ")", collapse="", sep="")
  HW.X.intercept <- paste("x intercept = (", signif(c(-HWmodel$coefficients[[1]]/HWmodel$coefficients[[2]]), 3), ", ", 0, ")", collapse="", sep="")
  HW.equation <- paste("y = ", (signif(HWmodel$coefficients[[2]], 3)), "x + ", (signif(HWmodel$coefficients[[1]], 3)), collapse="", sep="")
  HW.data <- c(HW.X.intercept, HW.Y.intercept, HW.X.intercept, HW.equation)
  return(HW.data)
}

HW.formula(enz.data)
