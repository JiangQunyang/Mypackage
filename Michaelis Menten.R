#Dummy data- the user should replace this with actual values
#The values of Vmax and Km can be taken from another function able to calculate them
S <- c(0,1,2,5,8,12,30,50)
V <- c(0,11.1,25.4,44.8,54.5,58.2,72.0,60.1)
Vmax <- 73.26
Km <- 3.44 

#we can define labels for the axes prior to plotting so we don't have to type them up over and over
Xlab <- "Substrate (mM)"  
Ylab <- "Velocity (nmol/s)"

#We make a data frame combining S and V called "endata"

endata <-data.frame(S,V)

#We also need to create a function for the Michaelis-Menten curve we want to fit onto our graph
fun_MM <- function(x)y=Vmax*x/(x+Km)

#Now we unify this into a function resulting in the plotting of our graph through ggplot

mich.ment <-function (S,V) {
  library(ggplot2) 
  endata <-data.frame(S,V)
  enplot<-ggplot(endata, aes (x = S, y = V))+ geom_point(color="black") + 
    xlab(Xlab) +
    ylab(Ylab) +
    theme_bw() +
    labs(title ="\nMichaelis-Menten Plot\n")+
    theme(plot.title = element_text(hjust=0.5))+
    stat_function(fun=fun_MM,color="blue")+ xlim(0, max(S))
  
  return(enplot)}

#the resulting graph:
mich.ment(S,V)


#I also managed to make a function that makes the Michaelis Menten curve an optional addition 

mich.ment2 <-function(S,V,MMcurve=TRUE) {
  
  if(MMcurve==TRUE){
    library(ggplot2) 
    endata <-data.frame(S,V)
    enplot<-ggplot(endata, aes (x = S, y = V))+ geom_point(color="black") + 
      xlab(Xlab) +
      ylab(Ylab) +
      theme_bw() +
      labs(title ="\nMichaelis-Menten Plot\n")+
      theme(plot.title = element_text(hjust=0.5))+
      stat_function(fun=fun_MM,color="blue")+ xlim(0, max(S))
    
      return(enplot)}
  
  else {
    library(ggplot2) 
    endata <-data.frame(S,V)
    enplot<-ggplot(endata, aes (x = S, y = V))+ geom_point(color="black") + 
      xlab(Xlab) +
      ylab(Ylab) +
      theme_bw() +
      labs(title ="\nMichaelis-Menten Plot\n")+
      theme(plot.title = element_text(hjust=0.5))+
    
      return(enplot)}
  }

#the resulting graph:
 mich.ment2(S,V,MMcurve=TRUE)
 
#changing it to MMcurve=FALSE will remove the line, in case you only want to plot the data as points. 


