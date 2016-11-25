########## INITIALISING ##########

## libraries
library(ggplot2)

## Test data
S <- c(30,50,0,1,2,5,8,12)
V <- c(72.0,60.1,0,11.1,25.4,44.8,54.5,58.2)

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
  if (enz.data[1,1]==0 && enz.data[1,2]==0) enz.data<-enz.data[-1,] # if the first measurement S or V is the enz.data dataframe is equal to 0, the whole row gets deleted because the linear model cannot deal with values that are NA/NaN/Inf
  return(enz.data)                                                  #which is what you get if you try and divide things by 0 
}
enz.data <- ReadIn(S, V)
  
  

########## FUNCTIONS ##########
  
  
  
## NADPH Standard Curve 
  
#This is the standard NADPH curve measured by spectrophotometry, plotting the line graph between abs(absorbance)~concentration(ctr).
#This is under 340nm(wavelenth),the extinction coefficient(molbs) is 6.22(M-1*cm-1), and normally, the lt(lenth of cuvette)=1(cm)


#Dummy data to test function:

#x<-round(runif(6,min=0,max=2),2) the maximum number of samples is 30. and it is beginning to overlap.
#stdNADPH(x) 
#plot graph  

stdNADPH<-function(abs,molbs=6.22,lt=1,line=TRUE){

  if(line==TRUE){
  abs<-sort(abs)
  ctr<-abs/(molbs*lt)
  df<-data.frame(ctr,abs)
  p<-ggplot(data=df,aes(x=ctr,y=abs))+
    geom_point()+
    stat_smooth(method="lm",se=FALSE)+
    xlab("NADPH Concentration (mol/L)")+
    ylab("Absorbance (340nm)")+
    labs(title ="NADPH Standard Curve")+
    theme(plot.title = element_text(hjust=0.5))+
    theme_bw()+
    annotate(geom="text", x=max(abs)/7, y= max(abs)*0.7, label="Abs         Ctr",  color="red")

    for (i in 1:length(abs)){
    p <- p + annotate(geom="text", x =max(abs)/7.3, y = (max(abs)*0.7 - i/20), label=abs[i])
    p <- p + annotate(geom="text", x = max(abs)/6.65, y = (max(abs)*0.7 - i/20), label=round(ctr[i], 3))
  }
  return(p)
  }else{
    abs<-sort(abs)
    ctr<-abs/(molbs*lt)
    df<-data.frame(ctr,abs)
    p<-ggplot(data=df,aes(x=ctr,y=abs))+
      geom_point()+
      xlab("NADPH concentration (mol/L)")+
      ylab("Absorbance (340nm)")+
      labs(title ="NADPH Standard Curve")+
      theme(plot.title = element_text(hjust=0.5))+
      theme_bw()+
      annotate(geom="text", x=max(abs)/7, y= max(abs)*0.7, label="Abs         Ctr",  color="red")

    for (i in 1:length(abs)){
      p <- p + annotate(geom="text", x =max(abs)/7.3, y = (max(abs)*0.7 - i/20), label=abs[i])   # 2 should be the maximal number, when the number over 2.5 these result will be too overlap to read results.
                                                                                         #So may set a if loop for the number over 2.5. The test by using following code: x<-round(runif(10,min=0,max=3),2).
      p <- p + annotate(geom="text", x = max(abs)/6.65, y = (max(abs)*0.7 - i/20), label=round(ctr[i], 3))
    }
    return(p)
  }

}

#PRINT   
stdNADPH(abs,molbs=6.22,lt=1,line=TRUE) #can also work if you only enter stdNADPH(x)
 
  
  
## Lineweaver-Burk

LineweaverBurk <- function(enz.data) {
  
  library(ggplot2)
 
  LBdata<-data.frame(enz.data[,4],enz.data[,3])
  
   LBmodel <- lm(enz.data[,3] ~ enz.data[,4]) 
   Y.intercept <- c(0, LBmodel$coefficients[[1]])
   X.intercept <- c(-LBmodel$coefficients[[1]]/LBmodel$coefficients[[2]], 0)

   LBplot <-  ggplot(LBdata, aes(x=enz.data[,4] , y=enz.data[,3])) +
    
    geom_point(size=3, shape=20) +
    
    ggtitle("Lineweaver Burk Plot") +
    xlab("[1/S]") +
    ylab("[1/v]") +
    xlim(X.intercept[1], max(enz.data[,4])) +
    ylim(0, max(enz.data[,3])) +
    theme_bw()+
    theme(plot.title = element_text(hjust=0.5))+
    
    geom_abline(intercept = LBmodel$coefficients[[1]], slope = LBmodel$coefficients[[2]], color = "blue", size = 1)+
    geom_vline(xintercept = 0, size=0.5)
    
  return (LBplot)
}

#PRINT

LineweaverBurk(enz.data)

# LB formula  -Gives the equation of the line as well as the x and y intercepts  

LB.formula <- function(enz.data){

  LBmodel <- lm(enz.data[,3] ~ enz.data[,4]) 
  LB.Y.intercept <- paste("y intercept = (", 0,", ", (signif(LBmodel$coefficients[[1]], 3)), ")", collapse="", sep="")
  LB.X.intercept <- paste("x intercept = (", signif(c(-LBmodel$coefficients[[1]]/LBmodel$coefficients[[2]]), 3), ", ", 0, ")", collapse="", sep="")
  LB.equation <- paste("y = ", (signif(LBmodel$coefficients[[2]], 3)), "x + ", (signif(LBmodel$coefficients[[1]], 3)), collapse="", sep="")
  LB.data <- c(LB.X.intercept, LB.Y.intercept, LB.X.intercept, LB.equation)
  return(LB.data)
}

#PRINT
LB.formula(enz.data)
  
  

##Hanes-Woolf

HanesWoolf <-function(enz.data){
  
  library(ggplot2)  
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
  
  HWmodel <- lm(enz.data[,6] ~ enz.data[,1]) 
  HW.Y.intercept <- paste("y intercept = (", 0,", ", (signif(HWmodel$coefficients[[1]], 3)), ")", collapse="", sep="")
  HW.X.intercept <- paste("x intercept = (", signif(c(-HWmodel$coefficients[[1]]/HWmodel$coefficients[[2]]), 3), ", ", 0, ")", collapse="", sep="")
  HW.equation <- paste("y = ", (signif(HWmodel$coefficients[[2]], 3)), "x + ", (signif(HWmodel$coefficients[[1]], 3)), collapse="", sep="")
  HW.data <- c(HW.X.intercept, HW.Y.intercept, HW.X.intercept, HW.equation)
  return(HW.data)
}

HW.formula(enz.data)
 
  
  
## Eadie Hofstee
  
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
    geom_abline(intercept = EHmodel$coefficients[[1]], slope = EHmodel$coefficients[[2]], color = "blue", size = 1)+
    geom_vline(xintercept = 0, size=0.5)
  return(EHplot)
  return(EH.equation)
}

EadieHofstee(enz.data)

# EH formula  -Gives the equation of the line as well as the x and y intercepts      
EH.formula <- function(enz.data){
  EHmodel <- lm(enz.data[,2] ~ enz.data[,5])
  EH.Y.intercept <- paste("y intercept = (", 0,", ", (signif(EHmodel$coefficients[[1]], 3)), ")", collapse="", sep="")
  EH.X.intercept <- paste("x intercept = (", signif(c(-EHmodel$coefficients[[1]]/EHmodel$coefficients[[2]]), 3), ", ", 0, ")", collapse="", sep="")
  EH.equation <- paste("y = ", (signif(EHmodel$coefficients[[2]], 3)), "x + ", (signif(EHmodel$coefficients[[1]], 3)), collapse="", sep="")
  EH.data <- c(EH.X.intercept, EH.Y.intercept, EH.X.intercept, EH.equation)
  return(EH.data)
}

EH.formula(enz.data)
  
  
  
## Km Vmax (Using Eadie Hofstee method)
  
Km.Vmax <- function(enz.data){
  EHmodel <- lm(enz.data[,2] ~ enz.data[,5])
  Vmax <- EHmodel$coefficients[[1]]
  Km <- EHmodel$coefficients[[1]]/(-EHmodel$coefficients[[1]]/EHmodel$coefficients[[2]])
  data <- c(Vmax, Km)
  return(data)
}
Km.Vmax(enz.data)

## Michaelis-Menten 
  

mich.ment <-function (S,V,MMcurve=TRUE) {
  library(ggplot2) 
  
  if(MMcurve==TRUE){
    
  EHmodel <- lm(enz.data[,2] ~ enz.data[,5])
  Vmax <- EHmodel$coefficients[[1]]
  Km <- EHmodel$coefficients[[1]]/(-EHmodel$coefficients[[1]]/EHmodel$coefficients[[2]])
  fun_MM <- function(x)y=Vmax*x/(x+Km)
  
  endata <-data.frame(S,V)
  enplot<-ggplot(endata, aes (x = S, y = V))+ geom_point(color="black") + 
    xlab("Substrate (mM)") +
    ylab("Velocity (nmol/s)") +
    theme_bw() +
    labs(title ="\nMichaelis-Menten Plot\n")+
    theme(plot.title = element_text(hjust=0.5))+
    stat_function(fun=fun_MM,color="blue")+ xlim(0, max(S))
  
  return(enplot)}
  
  else {EHmodel <- lm(enz.data[,2] ~ enz.data[,5])
  Vmax <- EHmodel$coefficients[[1]]
  Km <- EHmodel$coefficients[[1]]/(-EHmodel$coefficients[[1]]/EHmodel$coefficients[[2]])
  
  endata <-data.frame(S,V)
  enplot<-ggplot(endata, aes (x = S, y = V))+ geom_point(color="black") + 
    xlab("Substrate (mM)") +
    ylab("Velocity (nmol/s)") +
    theme_bw() +
    labs(title ="\nMichaelis-Menten Plot\n")+
    theme(plot.title = element_text(hjust=0.5))
  return(enplot)}
}

#the resulting graph:

mich.ment(S,V,MMcurve = TRUE)
  
###################################### THIS IS THE CURRENT EXTENT OF OUR PACKAGE #########################################
 
