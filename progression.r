library(DAAG)
data(progression)

# display the log log plot of time vs distance
plot(log(Time) ~ log(Distance), data=progression)

# display the log log plot of time vs distance with a simple linear 
# regression using only points 
xyplot(log(Time) ~ log(Distance), data=progression, type=c("p","r"))

# display the log log plot of time vs distance with a LOESS fit 
# regression using only points 
xyplot(log(Time) ~ log(Distance), data=progression,
       type=c("p","smooth"))

# display the residuals from the linear model for each value on
# a log plot of the distance
res <- resid(lm(log(Time) ~ log(Distance), data=progression))
plot(res ~ log(Distance), data=progression,
     ylab="Residuals from regression line on log scales")