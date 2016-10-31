library(doParallel)
library(stringr)

cl <- makeCluster(40)
registerDoParallel(cl)
avhhr.list <- list.files()

idx <-which(str_sub(avhhr.list, -3,-1) == ".nc")

avhhr.list <- avhhr.list[idx]

foreach (i = 1:length(avhhr.list)) %dopar% {
  
  library(ncdf4)
  library(raster)
  library(sp)
  library(rasterVis)
  library(fields)
  library(gstat)
  
  fname<-avhhr.list[i]
  fid <- nc_open(fname)
  
  sst <- ncvar_get(fid,'sea_surface_temperature')
  lon <- ncvar_get(fid, 'lon')
  lat <- ncvar_get(fid, 'lat')
  nc_close(fid)
  
  lon.idx <- which(lon <= -62 & lon >= -88,5)
  lat.idx <- which(lat <= 26.75 & lat >= 8.5)
  sst.idx <- intersect(lon.idx, lat.idx)
  
  lon2 <- lon[lon.idx]
  lat2 <- lat[lat.idx]
  sst2 <- sst[lon.idx,lat.idx]
  
  r1 <- raster(sst2, xmn=min(lat2),xmx=max(lat2),ymn=min(lon2),ymx=max(lon2))
  r2 <- t(r1)
  r3 <- rasterToPoints(r2, spatial = TRUE)
  
  x.range <- as.numeric(c(-87.95837,-62.04164)) #min/max longitude
  y.range <- as.numeric(c(8.541622,26.70839)) #min/max latitude
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.0416), y=seq(from= y.range[1], to=y.range[2], by = 0.0416 ))
  coordinates(grd) = ~x+y
  gridded(grd) = TRUE
  
  int <- idw(formula = layer~1, locations=r3, newdata=grd)
  int.output <- as.data.frame(int)
  int.output2 <- int.output[,1:3]
  
  output = substring(fname, 73, 81)
  csv = "csv"
  output2 <- paste(output,csv, sep=".")
  
  write.csv(int.output2, file=output2)
  
}

stopCluster(cl)