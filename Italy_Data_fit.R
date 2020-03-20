
rm(list=ls())
graphics.off()
# options(warn=1)

setwd("~/Downloads/Corona_Data")


# Read most recent data
global.data=read.csv(url("https://covid.ourworldindata.org/data/ecdc/full_data.csv"))

# global.data=read.csv("full_data.csv")

global.data

italy.rows=which(global.data[,2]=="Italy")
italy.data=global.data[italy.rows,]
italy.data[,1]

length(italy.data[,1])

day.zero=which(italy.data[,1]=="2020-03-09")
x.start=-which(italy.data[,1]=="2020-03-09")+1
x=x.start:(length(italy.data[,1])-day.zero)
# Check whether the quarantine day 2020-03-09 falls on 0
# data.frame(x,italy.data[,1])
##########################ĘĘ
#Total fits
###################

 # Fitting data from total cases
 total.cases=italy.data[,5]
 data.to.fit=data.frame(x,total.cases)
 data.fit=summary(nls(total.cases~(a*exp( x/b  )+c),data=data.to.fit,start=list(a=100,b=4,c=0),trace = T)   )

 # Plotting
 par(mfrow=c(2,1),mar=c(2.5,2.5,1.5,0.5),mgp=c(1.2,0.5,0),las=0)
 plot(x,italy.data[,5],main="Total number of cases in Italy",xlab="Day since lockdown",ylab="Total number of cases",xlim=c(-38,20),ylim=c(0.1,4*max(italy.data[,5],na.rm=T   )),log="")
 polygon(c(0,0,12,12),c(-100000,100000000,100000000,-100000),col=rgb(0,0,0,0.1),border="NA")
 curve( data.fit$coefficients[1,1]*exp(x/data.fit$coefficients[2,1])+ data.fit$coefficients[3,1],add=T,col=2,lwd=2)
 abline(h=data.fit$coefficients[1,1]*exp(12/data.fit$coefficients[2,1])+data.fit$coefficients[3,1],lty=2)
 abline(v=c(0,12),lwd=2,lty=3)
 text(12,data.fit$coefficients[1,1]*exp(12/data.fit$coefficients[2,1])+data.fit$coefficients[3,1],round(data.fit$coefficients[1,1]*exp(12/data.fit$coefficients[2,1])+data.fit$coefficients[3,1],-3),pos=3,col=1,cex=1.2)
 
 
 # Fitting data from total deaths
 total.deaths=italy.data[,6]
 data.to.fit=data.frame(x,total.deaths)
 data.fit=summary(nls(total.deaths~a*exp(x/b)+c,data=data.to.fit,start=list(a=100,b=6,c=3),trace = T))
 
 # Plotting
 plot(x,italy.data[,6],main="Total number of deaths in Italy",xlab="Day since lockdown",ylab="Total number of deaths",xlim=c(-38,20),ylim=c(0.1,4*max(italy.data[,6],na.rm=T)  ),log="")
 polygon(c(0,0,12,12),c(-100000,100000,100000,-100000),col=rgb(0,0,0,0.1),border="NA")
 curve( data.fit$coefficients[1,1]*exp(x/data.fit$coefficients[2,1])+ data.fit$coefficients[3,1],add=T,col=2,lwd=2)
 abline(h=data.fit$coefficients[1,1]*exp(12/data.fit$coefficients[2,1])+data.fit$coefficients[3,1],lty=2)
 abline(v=c(0,12),lwd=2,lty=3)
 text( 12,data.fit$coefficients[1,1]*exp(12/data.fit$coefficients[2,1])+data.fit$coefficients[3,1],    round(data.fit$coefficients[1,1]*exp(12/data.fit$coefficients[2,1])+data.fit$coefficients[3,1],-3),pos=3,col=1,cex=1.2)

# EOF