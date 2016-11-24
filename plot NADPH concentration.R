#This is the standar NADPH curve,ploting the line graph between abs(absorbance)~concentration(ctr).
#This is under 340nm(wavelenth),the extincyion coefficient(molbs) is 6.22(M-1*cm-1), and normally, the lt(lenth of cuvette)=1(cm)

#plot graph
stdNADPH<-function(abs,molbs=6.22,lt=1){
  ctr<-abs/(molbs*lt)
  df<-data.frame(ctr,abs)
  p<-ggplot(data=df,aes(x=ctr,y=abs))+
    geom_point()+
    stat_smooth(method="lm",se=FALSE)+
    xlab("NADPH concentration (mol/L)")+
    ylab("Absorbance (340nm)")+
    theme_bw()
    return(p)
}

#this is a eg. x<-c(0.2,0.5,0.8,1.3)  stdNADPH(x)
