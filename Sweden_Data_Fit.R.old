

rm(list=ls())
graphics.off()

setwd("~/Downloads/Corona_Data")

corona.cases=c(1,12,1,1,15,22,42,43,24,42,56,94,146,187,141,138,65,90)



xlabels.dat=c("31 Jan","26-29 Feb",
              "1 Mar",
              "2 Mar",
              "3 Mar",
              "4 Mar",
              "5 Mar",
              "6 Mar",
              "7 Mar",
              "8 Mar",
              "9 Mar",
              "10 Mar",
              "11 Mar",
              "12 Mar",
              "13 Mar",
              "14 Mar",
              "15 Mar",
              "16 Mar")



plot(corona.cases)
par(las=c(3))
barplot(corona.cases,ylim=c(0,1500),names=xlabels.dat,main="New cases per day",ylab="N of new cases",xlab="Day")


# ?par
curve(exp(x/3.1),add=T,col=2,lwd=2,lty=2)
abline(h=c(90,1000))
abline(v=16.35,lwd=2,lty=3)

