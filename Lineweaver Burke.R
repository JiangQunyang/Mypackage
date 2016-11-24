#Load ggplot2
library(ggplot2)


# First, the user must input their enzyme's reaction velocity (v) and substrate concentrations (S)
# Example data is provided here but feel free to change it and play around.
S <- c(2.5,3.5,5,10,20,50)
v <- c(32.3,40,50.8,72,87.7,115.4)

# We then put S and v into a data frame, called "enzdat".
enzdat <- data.frame(S, v)
enzdat

# Then divide 1 by S and v for Lineweaver Burke Plot (giving us 1/S and 1/v), and place into a data frame called "lbpdata"
lbpdata <- data.frame(1/enzdat)
# We then rename the column headers in lbpdata from S and v to lS and lv.
names(lbpdata) <- c("lS", "lv")
lbpdata

#Now we plot the "lbpdata" data frame with ggplot to produce a Lineweaver Burke Plot.
lbp <-  ggplot(lbpdata, aes(x= lS, y= lv)) + 
        geom_point(size=3, shape=20) +
        # Line of best fit
        geom_smooth(method=lm, formula = y ~ x, se=FALSE) +
        ggtitle("Lineweaver Burke Plot") +
        xlab("1/S") +
        ylab("1/v") +
        theme_bw()


# We then need to calculate and display the equation for the line of best fit we just plotted.
# This equation will allow us to calculate useful information such as Vmax, Kcat, and Km.

#We first apply a linear model (lm) to the data frame and call it "m". 
m <- lm(lv ~ lS, lbpdata)

lm_eqn = function(m) {
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}

p1 = lbp + geom_text(aes(x=max(S)/2, y =max(ratio)/2, hjust=0, label = lm_eqn(lm(lv ~ lS, lbpdata))), parse = TRUE)
p1

# The Lineweaver Burke Plot now displays the equation for the line of best fit.
