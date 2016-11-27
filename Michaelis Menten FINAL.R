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


