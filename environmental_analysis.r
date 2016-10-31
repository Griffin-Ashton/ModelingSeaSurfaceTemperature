library(doParallel)
library(stringr)

setwd("~/WPX Distribution/analysis")
options(digits=8)
ap.loc <- read.csv('palmatadistribution.csv')

#Generate a new date vector to be modified
daterip<-as.factor(ap.loc$Date)

#Split our Dates into individual day, month, year columns
new.date<-strptime(daterip,format="%m/%d/%Y") 

day<-1+new.date$yday #R counts jan 1 as day "0" and i prefer it to be day "1" so i add one to all days
month<-1+new.date$mon #R counts jan as month "0" so again i add "1" as i prefer jan to be month 1
year<- 3800+new.date$year #R kind of assumes we count forward from they year 1900 (don't ask me why!) so we have to add 1900 to get correct year

#Converts our new vector from character
#to Date
#Bind separate columns for Day, Month, and Year to our data set. 
ap.loc <- cbind(ap.loc, day)
ap.loc <- cbind(ap.loc, month)
ap.loc <- cbind(ap.loc, year)
date.new <- as.Date(new.date)
ap.loc <- cbind(ap.loc, date.new)

for(i in 1:length(ap.loc$year)){
  if(ap.loc$year[i] < 1997){
    ap.loc$year[i] = ap.loc$year[i] + 100
  }
}


ap.loc$Disease <- as.character(ap.loc$Disease)

dis.pres = NULL
for( i in 1:length(ap.loc$Disease)){
  if(ap.loc$Disease[i]=="WS"){
    dis.pres[i] = 1
  }
  else{
    dis.pres[i] = 0
  }
}

ap.loc <- cbind(ap.loc, dis.pres)

id <- seq(1,length(ap.loc$Disease),1)

ap.loc <- cbind(id, ap.loc)

#Extracting SST Data

#Guide Save - http://www.geo.ut.ee/aasa/LOOM02331/R_idw_interpolation.html

sst.test <- read.csv("2002300_d.csv")
x.range <- as.numeric(c(-87.95837,-62.04164)) #min/max longitude
y.range <- as.numeric(c(8.541622,26.70839)) #min/max latitude
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.0416), y=seq(from= y.range[1], to=y.range[2], by = 0.0416 ))
coordinates(grd) = ~x+y
gridded(grd) = TRUE
grd2 <- raster(grd)

ap.loc2 <- as.data.frame(cbind(ap.loc$Longitude,ap.loc$Latitude,ap.loc$id))
xy <- ap.loc2[,c(1,2)]
ap.spdf <- SpatialPointsDataFrame(coords = xy, data=ap.loc2)

ap.rasterize <- rasterize(ap.spdf, grd2, field=ap.spdf$V3)

idx <- ap.rasterize[values(ap.rasterize)!="NA"]

ap.reduce <- ap.loc[idx,]

xy <- data.frame(xyFromCell(ap.rasterize, 1:ncell(ap.rasterize)))
v <- getValues(ap.rasterize)
dxy <- data.frame(x=xy[,1],y=xy[,2],v)
dxy <- dxy[complete.cases(dxy),]

ap.reduce <- merge(ap.reduce,dxy,by='id',by.y='v')
ap.reduce <- subset(ap.reduce, year!=2004)

survey.list <- list.files()
idx <- which(nchar(survey.list)!=13)
survey.list <- survey.list[-idx]

celsius <- function(kelvin){
  result = kelvin - 273.15
  return(result)
}

surveytemp.d <- list()
surveytemp.n <- list()
mean.temp.d.30 <- list()
mean.temp.n.30 <- list()
max.temp.d.30 <- list()
min.temp.d.30 <- list()
max.temp.n.30 <- list()
min.temp.n.30 <- list()
rate.cng.d <- list()
rate.cng.n <- list()


.combine=function(x,y)rbindlist(list(x,y))

cl <- makeCluster(40)
registerDoParallel(cl)

results = foreach (i = 1:length(ap.reduce$id)) %dopar% {
  
  climate.tmp <- NULL
  year = ap.reduce$year[i]
  day = ap.reduce$day[i]
  if(day < 100){
    day = paste(0,day,sep='')
  }
  surveydate = paste(year,day,sep='')
  surveydate.d = paste(surveydate,'d.csv',sep="_")
  surveydate.n = paste(surveydate,'n.csv',sep="_")
  
  if(length(which(surveydate.d == survey.list)) == 1 ){
    survey.d = read.csv(surveydate.d)
  }else{
    day.alt = as.numeric(day) - 1
    if(day.alt <100){
      day.alt = paste(0,day.alt,sep='')
    }
    surveydate = paste(year,day.alt,sep='')
    surveydate.d = paste(surveydate,'d.csv',sep="_")
    survey.d = read.csv(survey.d)
  }
  
  if(length(which(surveydate.n == survey.list)) == 1){
    survey.n = read.csv(surveydate.n)
  }else{
    day.alt = as.numeric(day)-1
    if(day.alt <100){
      day.alt = paste(0,day.alt,sep='')
    }
    surveydate = paste(year,day.alt,sep='')
    surveydate.n = paste(surveydate,'n.csv',sep="_")
  }
  
  #Survey Temperature for Day
  idx <- which(ap.reduce$x[i] == survey.d$x & ap.reduce$y[i]==survey.d$y)
  surveytemp.d = survey.d$var1.pred[idx]
  
  #Survey Temperature for Night
  idx <- which(ap.reduce$x[i] == survey.n$x & ap.reduce$y[i]==survey.n$y)
  surveytemp.n = survey.n$var1.pred[idx]
  
  ##30 Day Values
  #Day Range
  rng = seq(as.numeric(day)-30,as.numeric(day),1)
  
  if(day < 100){
    for(z in 1:length(rng)){
      rng[z] <- paste(0,rng[z],sep='')
    }
  }
  
  survey.rng = paste(year,rng,sep='')
  survey.rng.d = paste(survey.rng,'d.csv',sep="_")
  survey.rng.n = paste(survey.rng,'n.csv',sep="_")
  
  survey.rng.d <- intersect(survey.rng.d, survey.list)
  survey.rng.n <- intersect(survey.rng.n, survey.list)
  for(j in 1:length(survey.rng.d)){
    survey.d = read.csv(survey.rng.d[j])
    x.char <- as.character(ap.reduce$x[i])
    x.s.char <- as.character(survey.d$x)
    y.char <- as.character(ap.reduce$y[i])
    y.s.char <- as.character(survey.d$y)
    idx <- which(x.char == x.s.char & y.char == y.s.char)
    
    tmp.d[j]= survey.d$var1.pred[idx]
  }
  
  for(j in 1:length(survey.rng.n)){
    survey.n = read.csv(survey.rng.n[j])
    x.char <- as.character(ap.reduce$x[i])
    x.s.char <- as.character(survey.n$x)
    y.char <- as.character(ap.reduce$y[i])
    y.s.char <- as.character(survey.n$y)
    idx <- which(x.char == x.s.char & y.char == y.s.char)
    idx <- which(x.char == x.s.char & y.char==y.s.char)
    
    tmp.n[j] = survey.n$var1.pred[idx]
  }
  
  mean.temp.d.30 = celsius(mean(tmp.d))
  max.temp.d.30 = celsius(max(tmp.d))
  min.temp.d.30 = celsius(min(tmp.d))
  mean.temp.n.30 = celsius(mean(tmp.n))
  max.temp.n.30 = celsius(max(tmp.n))
  min.temp.n.30 = celsius(min(tmp.n))
  
  #60 Day Values
  #Day Range
  rng = seq(as.numeric(day)-60,as.numeric(day)-30,1)
  
  if(day < 100){
    for(z in 1:length(rng)){
      rng[z] <- paste(0,rng[z],sep='')
    }
  }
  
  survey.rng = paste(year,rng,sep='')
  survey.rng.d = paste(survey.rng,'d.csv',sep="_")
  survey.rng.n = paste(survey.rng,'n.csv',sep="_")
  
  survey.rng.d <- intersect(survey.rng.d, survey.list)
  survey.rng.n <- intersect(survey.rng.n, survey.list)
  
  tmp.d <- NULL
  tmp.n <- NULL
  for(j in 1:length(survey.rng.d)){
    survey.d = read.csv(survey.rng.d[j])
    x.char <- as.character(ap.reduce$x[i])
    x.s.char <- as.character(survey.d$x)
    y.char <- as.character(ap.reduce$y[i])
    y.s.char <- as.character(survey.d$y)
    idx <- which(x.char == x.s.char & y.char == y.s.char)
    
    tmp.d[j]= survey.d$var1.pred[idx]
  }
  
  for(j in 1:length(survey.rng.n)){
    survey.n = read.csv(survey.rng.n[j])
    x.char <- as.character(ap.reduce$x[i])
    x.s.char <- as.character(survey.n$x)
    y.char <- as.character(ap.reduce$y[i])
    y.s.char <- as.character(survey.n$y)
    idx <- which(x.char == x.s.char & y.char == y.s.char)
    
    tmp.n[j] = survey.n$var1.pred[idx]
  }
  mean.tmp.d.60 <- celsius(mean(tmp.d))
  mean.tmp.n.60 <- celsius(mean(tmp.n))
  rate.cng.d <- mean.tmp.d.60 - mean.temp.d.30
  rate.cng.n <- mean.tmp.n.60 - mean.temp.n.30
  
  climate.tmp = ap.reduce[i,]
  climate.tmp = cbind(climate.tmp, mean.temp.d.30,max.temp.d.30,min.temp.d.30,mean.temp.n.30,max.temp.n.30,min.temp.n.30, mean.tmp.d.60,mean.tmp.n.60,rate.cng.d,rate.cng.n)
  
}

ws.climate = NULL
for( i in 1:length(results)){
  ws.climate <- rbind(ws.climate, t(unlist(results[i])))
}


stopCluster(cl)

