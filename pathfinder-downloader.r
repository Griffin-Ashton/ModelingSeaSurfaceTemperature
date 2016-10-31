### Developed by Ashton Griffin
##  griffin.ashton@gmail.com

#Load the necessary packages.
library(ncdf4)
library(raster)
library(RCurl)
library(rvest)

#Set the range of years for the period you would like data from. 
the.years <- seq(1981,2012,1)
for( i in 1:the.years){
  ###Web Scraping the links by year.   
  year = the.years[i]
  wd <- paste("~/WPX Distribution/SST/avhhr",year,sep='/')
  setwd(wd)
  base = 'http://data.nodc.noaa.gov/pathfinder/Version5.2'
  url  = paste(base,year, sep='/')
  scrape.year = read_html(url)
  year.text = html_text(scrape.year)
  year.split = strsplit(year.text, '\n')
  link.index = unlist(year.split)
  link.index = as.data.frame(link.index)
  link.index = link.index[8:length(link.index[,1]),]
  link.index = as.character(link.index)
  
  files <- NULL
  for(j in 1:length(link.index)){
    idx <- NULL
    idx <- strsplit(link.index[j], split=' ')
    files[j] <- idx[[1]][2]
  }
  idx.na <- which((is.na(files)))
  files <- files[-idx.na]
  
  for(j in 1:length(files)){
    #System sleep is necessary otherwise the server kicks you. 
    Sys.sleep(1)
    dl.url = paste(url,files[j],sep='/')
    download.file(dl.url,files[j])
  }
  
}