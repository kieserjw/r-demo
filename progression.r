library(DAAG)
library(ggplot2)
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


# display the progression of record times for each individual event vs year
distlist <- c(0.1, 0.2, 0.4, 0.8, 1.5,  1.6093, 3, 10, 42.195)
for (d in distlist){

    # get a sorted matrix of each event to search for anomalies
    newdata <- subset(progression, Distance == d, select=c(year, Time))
    Time <- newdata[['Time']][order(newdata[['year']])]
    year <- sort(newdata[['year']])
    anomalies <- matrix(, nrow=length(Time), ncol=2, dimnames=list(NULL, c("year", "Time")))

    # look for times that are slower than previous records
    index <- 1
    for (i in 2:length(Time)){
        if (Time[i] > Time[i-1] && year[i] > year[i-1]){
            anomalies[index,] <- c(year[i], Time[i])
            index <- index + 1
        }
    }

    # create the base plt
    p <- ggplot(data=newdata, aes(x=year, y=Time, group=1)) +
         geom_point() + ggtitle(paste(toString(d)," km"))

    # if there are anomalies, add them to the plot
    if (!is.na(anomalies[1,1])){
        dframe <- as.data.frame(anomalies)
        pts <- geom_point(data=dframe, colour="red", size = 3)
        p <- p + pts
    }
    print(p)
}
