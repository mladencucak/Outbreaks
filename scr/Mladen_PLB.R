blightdat<-read.csv(here::here("tmp" , "PLB Outbreaks no oakpark.csv") , head=TRUE)
head(blightdat)
summary(blightdat)
attach(blightdat)
+#+---plot the cumulative outbreaks per year---------+
hist2005<-hist(DOY[which(yr=="2003")])
plot(DOY[which(yr=="2003")],cumulative[which(yr=="2003")], ty="b",
    pch=19, col="darkblue", cex=1.5,
    xlab="day of year", ylab="cumulative number of reports")
plot(DOY[which(yr=="2004")],cumulative[which(yr=="2004")], ty="b",
    pch=19, col="darkblue", cex=1.5,
    xlab="day of year", ylab="cumulative number of reports")
plot(DOY[which(yr=="2005")],cumulative[which(yr=="2005")], ty="b",
     pch=19, col="darkblue", cex=1.5,
     xlab="day of year", ylab="cumulative number of reports")
plot(DOY[which(yr=="2006")],cumulative[which(yr=="2006")], ty="b",
     pch=19, col="darkblue", cex=1.5,
     xlab="day of year", ylab="cumulative number of reports")
plot(DOY[which(yr=="2007")],cumulative[which(yr=="2007")], ty="b",
     pch=19, col="darkblue", cex=1.5,
     xlab="day of year", ylab="cumulative number of reports")
plot(DOY[which(yr=="2008")],cumulative[which(yr=="2008")], ty="b",
     pch=19, col="darkblue", cex=1.5,
     xlab="day of year", ylab="cumulative number of reports")
plot(DOY[which(yr=="2009")],cumulative[which(yr=="2009")], ty="b",
     pch=19, col="darkblue", cex=1.5,
     xlab="day of year", ylab="cumulative number of reports")
plot(DOY[which(yr=="2010")],cumulative[which(yr=="2010")], ty="b",
     pch=19, col="darkblue", cex=1.5,
     xlab="day of year", ylab="cumulative number of reports")
plot(DOY[which(yr=="2011")],cumulative[which(yr=="2011")], ty="b",
     pch=19, col="darkblue", cex=1.5,
     xlab="day of year", ylab="cumulative number of reports")
plot(DOY[which(yr=="2012")],cumulative[which(yr=="2012")], ty="b",
     pch=19, col="darkblue", cex=1.5,
     xlab="day of year", ylab="cumulative number of reports")
plot(DOY[which(yr=="2013")],cumulative[which(yr=="2013")], ty="b",
     pch=19, col="darkblue", cex=1.5,
     xlab="day of year", ylab="cumulative number of reports")
plot(DOY[which(yr=="2014")],cumulative[which(yr=="2014")], ty="b",
     pch=19, col="darkblue", cex=1.5,
     xlab="day of year", ylab="cumulative number of reports")

yr_agg1<-aggregate(DOY~yr, FUN=min) #Julian day of first report
yr_agg2<-aggregate(DOY~yr, FUN=max) #Julian day of last report
yr_agg3<-aggregate(DOY~yr, FUN=length) #number of reports

plot(yr_agg1, ty="b", col="blue", pch=19, cex=1.5)
plot(yr_agg2, ty="b", col="red", pch=19, cex=1.5)
plot(yr_agg2, ty="b", col="red", pch=19, cex=1.5,
     ylim=c(min(yr_agg1[,2]),max(yr_agg2[,2])),
     ylab="Julian day", xlab="year")
points(yr_agg1, ty="b", col="blue", pch=19, cex=1.5)
plot(yr_agg1[,2],yr_agg3[,2], col="red", pch=19, cex=1.5,
     ylim=c(min(yr_agg3[,2]),max(yr_agg3[,2])),
     ylab="Number of reports", xlab="Julian day of first report")

#+------------Regression models-----------+
#simple linear
lm1<-lm(yr_agg3$DOY~yr_agg1$DOY)
summary(lm1)
AIC(lm1)
plot(yr_agg1[,2],yr_agg3[,2], col="red", pch=19, cex=1.5,
     ylim=c(min(yr_agg3[,2]),max(yr_agg3[,2])),
     ylab="Number of reports", xlab="Julian day of first report")
  lines(yr_agg1[,2],lm1$fitted.values, lwd=3, lty=2, col="blue")


correlmat<-cbind(yr_agg1[,2],yr_agg3[,2])
acf(correlmat, type="correlation")

startend<-vector(length=length(yr_agg1[,2])+length(yr_agg3[,2]))
start<-yr_agg1$DOY
end<-yr_agg2$DOY
startend <- rbind(start, end)

plot(startend[1,], startend[2,], pch=19, col="darkblue", cex=1.5)
lm2<-lm(startend[2,]~startend[1,])
summary(lm2)
plot(startend[1,], startend[2,], pch=19, col="darkblue", cex=1.5)
  lines(startend[1,], lm2$fitted.values, col="red", lwd=3, lty=2)
  
  
#+---------Get parameters from Rafael's analysis-------+
logis_parms<-read.csv(here::here("tmp" , "paramter_est_per_year.csv") , head=TRUE)
logis_parms
attach(logis_parms)
plot(xmid, yr_agg3[-8,2], col="red", pch=19, cex=1.5)
plot(xmid, Asym, col="red", pch=19, cex=1.5)
length(yr_agg2[,2])
length(Asym)
yr_agg2
cordat<-cbind(xmid,Asym,yr_agg1[-8,2],yr_agg2[-8,2],yr_agg3[-8,2])
lower.tri(cor(cordat))*cor(cordat)
plot(yr_agg3[-8,2], Asym, col="red", pch=19, cex=1.5)
plot(year2, yr_agg3[-8,2], col="red", ty="b", pch=19, cex=1.5)
  points(year2, Asym, pch=19, ty="b", col="blue", cex=1.5)

#+----------TSA---------------+
#Series are really too short to be doing this, but...
#First on the max value of the raw data

max_arima<-arima(yr_agg3$DOY,order=c(2,1,0))
max_arima
BICmax<-BIC(max_arima) 
BICmax
residmax<-residuals(max_arima)
plot.new()
plot(yr_agg3$yr, residmax, ty="b", lty=3, lwd=2, col="darkblue", pch=19)
armaxfit<-yr_agg3$DOY-residmax
plot(yr_agg3$yr, yr_agg3$DOY, pch=19, col="darkblue")
lines(yr_agg3$yr,armaxfit, ty="l", col="deepskyblue", lty=2, lwd=3)
armax_fcast<-predict(max_arima, n.ahead=5)
armax_fcast$pred
armax_fcvals <- armax_fcast$pred
armax_fc_se <- armax_fcast$se

blightyears<-seq(2015,2019,1)
plot(yr_agg3$yr, yr_agg3$DOY, pch=19, col="darkblue", cex=1.5,
     xlab="year", ylab="blight outbreaks", xlim=c(2004,2020),
     ylim=c(0,max(yr_agg3$DOY)))
  lines(yr_agg3$yr,armaxfit, lty=2, col="blue", lwd=3)
  lines(blightyears,armax_fcvals, lty=2, col="red", lwd=3)
  lines(blightyears,armax_fcvals+2*armax_fc_se, lty=3, lwd=3, col="deepskyblue2")
  lines(blightyears,armax_fcvals-2*armax_fc_se, lty=3, lwd=3, col="deepskyblue2")


