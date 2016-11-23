
absplot<-function(concentration,absorbance){
  df<-data.frame(concentration,absorbance)
  p<-ggplot(data=df,aes(x=concentration,y=absorbance))+
    geom_point()+
    stat_smooth(method="lm",se=FALSE)+
    xlab("Enzyme concentration (mg/ml)")+
    ylab("Absorbance (595nm)")+
    ggtitle(paste("Enzyme Assay",Sys.Date()))+
    theme_bw()
    return(p)
}

# Protein Concentrations
prot <- c(0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000,
          0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000)

# Absorbance from my protein assay
abs <- c(0.329, 0.352, 0.349, 0.379, 0.417, 0.491, 0.668, 0.956,
         0.327, 0.341, 0.355, 0.383, 0.417, 0.446, 0.655, 0.905)
absplot(prot,abs)
