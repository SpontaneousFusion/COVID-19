#
# corona duomenys
#
#
# 2020-03-20
# 

require(minpack.lm)
rm(list=ls(all=TRUE))


global.data <- read.csv("https://covid.ourworldindata.org/data/full_data.csv") # duomenys tiesiai is webo

global.data$date <- as.Date(global.data$date, format= "%Y-%m-%d")

# lockdown datos

i0 <- as.Date("2020-03-10",format= "%Y-%m-%d")
c0 <- as.Date("2020-01-23",format= "%Y-%m-%d")

# kinijos duomenys

fff <- subset(global.data, location == "China", select = c(date, total_cases))

  x <- as.numeric(fff$date - c0)
  y <- fff$total_cases
  
  ffc <- data.frame(cbind(x,y))
  c=0
  prad <- list(A=55000, b=4,  d=14)  
  
  fit2 <- nlsLM(y ~ A/(1+exp(-(x-d)/b))
                , start=prad, data=subset(ffc, y < 60000) ) 
  # yra diagnostinis suolis duomenys, todel nujaunami duomenys, po pasikeitusios metodikos
  
  a <- summary(fit2)$coef[,1]
  
  # Italija
  
  fff <- subset(global.data, location == "Italy", select = c(date, total_cases))
  x <- as.numeric(fff$date - i0)
  y <- fff$total_cases
  ffi <- data.frame(cbind(x,y))

  d = a[3]
  #b = a[2]
  fit_i <- nlsLM(y ~ A/(1+exp(-(x-d)/b))
                 , start=list(A=60000, b=4), data=ffi )
  # italija fitinima su prielaida, kad poveikis pasijaus kaip ir Kinijoje ~14 diena.
  
  ai <- summary(fit_i)$coef[,1]
  
  pilka <- "grey50"
  
  minx = -24
  maxx = 36
  dx = 6
  ranx <- c(minx, maxx)
  maxt <- seq(minx, maxx, dx)
  mixt <- seq(minx, maxx, dx/6)
  
  miny <- 0
  maxy <- signif(predict(fit_i, list(x=24)),2)
  dy <- 2e4
  rany <- c(miny,maxy)
  mayt <- seq(miny, maxy, dy)
  miyt <- seq(miny-dy, maxy+dy, dy/4)
  
  
  # diagramos langas
  plot.new()
  plot.window(xlim=ranx, ylim=rany, lwd=1)
  rect(0, 0-dy, a[3], maxy+dy, col="gray90", border = NA)
  axis(1, lwd=1, tck=.02, at=maxt, label=T, col=pilka, col.axis=pilka)
  axis(1, lwd=1, tck=.01, at=mixt, label=F, col=pilka)
  axis(2, lwd=1, tck=.01, las=2, at=mayt, label=mayt/1e6, col=pilka, col.axis=pilka)
  axis(2, lwd=1, tck=.01, las=2, at=miyt, label=F, col=pilka)
  axis(3, lwd=1, tck=.02, at=maxt, label=F, col=pilka)
  axis(3, lwd=1, tck=.01, at=mixt, label=F, col=pilka)
  axis(4, lwd=1, tck=.01, at=mayt, labels=F, col=pilka)
  axis(4, lwd=1, tck=.01, at=miyt, label=F, col=pilka)
  title( xlab="Days since lockdown", ylab="Total cases")
  box(col=pilka)
  
  ff1 <- seq(-12, 36, .05)
  
  vingis <- predict(fit_i, list(x=a[3]))
  abline(h=vingis, lty=3)
  abline(v=c(0, a[3]), lty=3)
  
  lines(ff1, predict(fit_i, list(x=ff1)), lty=1, col="DarkOrange", lwd=2)
  points(x,y)
  
  text(minx+dx/2, vingis, paste(signif(vingis,2)), pos=1)
  text(minx+dx/2, vingis, paste(signif(ai[1],2)), pos=3)
  
  