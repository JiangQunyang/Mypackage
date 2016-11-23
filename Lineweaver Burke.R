#Load ggplot2
library(ggplot2)


# First, the user must input their enzyme's reaction velocity (v) and substrate concentrations (S)
# Example data is provided here but feel free to change it and play around.
S <- c(2.5,3.5,5,10,20,50)
v <- c(32.3,40,50.8,72,87.7,115.4)

# We then put S and v into a data frame, called "enzdat".
enzdat <- data.frame(S, v)

# Then divide 1 by S and v for Lineweaver Burke Plot (giving us 1/S and 1/v), and place into a data frame called "lbpdata"
lbpdata <- data.frame(1/enzdat)
lbpdata

#Now we plot the "lbpdata" data frame with ggplot to produce a Lineweaver Burke Plot.
lbp <-  ggplot(lbpdata, aes(x=x, y=x)) + 
        geom_point(size=3, shape=20) +
        # Line of best fit
        geom_smooth(method=lm, formula = y ~ x, se=FALSE) +
        ggtitle("Lineweaver Burke Plot") +
        xlab("1/S") +
        ylab("1/v") +
        theme_linedraw()
lbp

# We then need to calculate and display the equation for the line of best fit we just plotted.
# This equation will allow us to calculate useful information such as Vmax, Kcat, and Km. 

# If anyone can work out how to calculate the line of best fit’s equation then stick it here…