#This is the standar NADPH curve by spectrophotomete,ploting the line graph between abs(absorbance)~concentration(ctr).
#This is under 340nm(wavelenth),the extincyion coefficient(molbs) is 6.22(M-1*cm-1), and normally, the lt(lenth of cuvette)=1(cm)

#plot graph
library(ggplot2)
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


x<-round(runif(6,min=0,max=2),2)#the maximal numbers of sample is 30. and it is begainning to overlap.

stdNADPH(x)
