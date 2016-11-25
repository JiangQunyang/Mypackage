#This is the standar NADPH curve by spectrophotomete,ploting the line graph between abs(absorbance)~concentration(ctr).
#This is under 340nm(wavelenth),the extincyion coefficient(molbs) is 6.22(M-1*cm-1), and normally, the lt(lenth of cuvette)=1(cm)

#plot graph
library(ggplot2)
stdNADPH<-function(abs,molbs=6.22,lt=1,line=TRUE){

  if(line==TRUE){
  ctr<-abs/(molbs*lt)
  df<-data.frame(ctr,abs)
  p<-ggplot(data=df,aes(x=ctr,y=abs))+
    geom_point()+
    stat_smooth(method="lm",se=FALSE)+
    xlab("NADPH concentration (mol/L)")+
    ylab("Absorbance (340nm)")+
    theme_bw()+
    annotate(geom="text", x=max(abs)/7, y= 0.6, label="Abs         Ctr",  color="red")

  for (i in 1:length(abs)){
    p <- p + annotate(geom="text", x =max(abs)/7.45, y = (0.6 - i/20), label=abs[i])
    p <- p + annotate(geom="text", x = max(abs)/6.50, y = (0.6 - i/20), label=round(ctr[i], 3))
  }
  return(p)
  }
  else{
    ctr<-abs/(molbs*lt)
    df<-data.frame(ctr,abs)
    p<-ggplot(data=df,aes(x=ctr,y=abs))+
      geom_point()+
      xlab("NADPH concentration (mol/L)")+
      ylab("Absorbance (340nm)")+
      theme_bw()+
      annotate(geom="text", x=max(abs)/7, y= 0.6, label="Abs         Ctr",  color="red")

    for (i in 1:length(abs)){
      p <- p + annotate(geom="text", x =max(abs)/7.45, y = (0.6 - i/20), label=abs[i])
      p <- p + annotate(geom="text", x = max(abs)/6.50, y = (0.6 - i/20), label=round(ctr[i], 3))
    }
    return(p)
  }

}

x<-c(0.2,0.5,0.7,1.3)
stdNADPH(x,line = TRUE)
