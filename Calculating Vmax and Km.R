# First, the user must input their enzyme's reaction velocity (v) and substrate concentrations (S)
# Example data is provided here but feel free to change it and play around.
S <- c(2.5,3.5,5,10,20,50)
v <- c(32.3,40,50.8,72,87.7,115.4)

# We then put S and v into a data frame, called "enzdat".
enzdat <- data.frame(S, v)


# Next step - how do we get the values of Km and Vmax?

# This is the theoretical formula
# "velocity = Vmax times S divided by (Km plus S)", stored in MMcurve
MMcurve<-formula(v~Vmax*S/(Km+S))

# nls is non-linear least squares optimiser.
# If you're not sure why a Michael-Menten equation is non-linear, ask
# (Hint: it is not because it is a curve when plotted!)

# start sets the initial guess - do not have to be very good
# should be possible to make reasonable guesses 
# from a quick inspection - this is why we visualise the data first
bestfit <- nls(MMcurve, enzdat, start=list(Vmax=10,Km=2))

# Build a theoretical line defining the best fit curve
# First, make a finely detailed set of points between 0 and 50, at 0.1 intervals
# These will be the substrate concentrations that are used to calculate 
# the predicted velocities
SconcRange <- seq(0,50,1)

# Then, calculate the predicted velocities using the predict function
theorLine <- predict(bestfit,list(S=SconcRange))

# Best fit values of Km and Vmax obtained by coef function, stored in bestFitVals
bestFitVals <- coef(bestfit)

# Now plot the data, the best fit line, and put the best fit coefficients in the plot
plot (enzdat, 
      xlab="[S]", 
      ylab="Velocity", 
      title(main="Fitted MM data"), 
      pch=17, col=c2, cex=2)

# Draw the line
# lines() function adds to the existing plot as does points()
# here, we want a line of best fit, not points, so we use lines()
lines(SconcRange,theorLine,col=c1)

# Add text with the calculated values
# This is a fudge, there must be better ways, also adding errors on parameter values.

text(30,80, "Vmax=")
text(36,80,round(bestFitVals[1],2))
text(30,70, "Km=")
text(35,70,round(bestFitVals[2],2))

# END OF SCRIPT

