
## Lu 13-14 Jan 2014
## Lu 15-16 Jan 2014 I realised that we don't need to use a 2D matrix 
## as we already have all info in the 3D matrix (the brick)!!!
## so the apply functions are replaced by calc from raster package
## outputs are then a raster object
## TODO  remove objetcts after used

require(raster); require(rgdal)
####The 1st section will be repalce by AWAP_GRIDS-monthly.r that gets drids from BoM website
#the load_month didn't work on my PC

#i'm using the flt images Matt gave me 1900-2013
path = c('D:/work/awap_rain_mth_1990-2013/rain')
awap.grids = dir(path, pattern = "flt$", full.names=T, recursive=T) 

#re-save them as tiff to use less memory
for (i in seq_len(length(awap.grids))) {
  r <- raster(awap.grids[[i]])
  name <- paste (awap.grids[[i]])
  writeRaster(r, filename=name, format="GTiff", overwrite=TRUE)
}


##open tif files and brick them
#for some reason brick or stack only don't work, both together do

awap.grids <- dir(path, pattern = "tif$", full.names=T, recursive=T) 

ptm <- proc.time()
rasterbrick <- brick(stack(awap.grids[1:12]))
projection(rasterbrick)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proc.time() - ptm
## timing for ALL years
## user  system elapsed 
## 889.14   26.00  919.03


## I'm not sure what's more efficient, if changing the drought function 
## to do the calc on matrices or just running the function on the vectors
## calc matrices it's silly, the calculation ca nbe done with the raster at pixel
## level using calc or overlay from raster package
## the apply functions can be used to replace loops in the orginal function that reads data.frames


#rasterdroughtIndex<-function(data,years,droughtThreshold=.375){

##Ivan's comments (...)
#The values returned for a RasterStack or RasterBrick are always a matrix, 
#with the rows representing cells, and the columns representing layers
#b is a matrix that contains grid values
#to be replaced by getValues(raster) >> problem: Error: cannot allocate vector of size 5.8 Gb

# ptm <- proc.time()
# b <- getValues(rasterbrick) # not needed as calc or overlay does the same with the rasterbrick
# proc.time() - ptm


# b<-getValuesBlock(rasterbrick, row=500, nrows=5, col=500, ncols=5)
# # TODO estimate the max and min date from the data filenames
# x<-apply(b, 1, function(x) ts(x,start=c(1900, 01),end=c(2013,12),frequency=12))
############################################################
ptm <- proc.time()
xx= calc(rasterbrick, fun=function(x) ts(x,start=c(1900, 01),end=c(1900,12),frequency=12))
proc.time() - ptm

# this can be used to modify the original function
#sixmnthtot<-apply(x, 2, function(x) c(rep(NA,5),x+lag(x,1)+lag(x,2)+lag(x,3)+lag(x,4)+lag(x,5)))

ptm <- proc.time()
xx.sixmnthtot <- calc(xx, fun=function(x) 
  c(rep(NA,5),x+lag(x,1)+lag(x,2)+lag(x,3)+lag(x,4)+lag(x,5)))
proc.time() - ptm
## user  system elapsed 
## 88.23    0.48   89.32

# TODO it might be faster to use rollapply, and also we can make the lag length 
require(zoo)
?rollapply
#sixmnthtot2<- apply(x, 2, function(x) rollapply(x, 6, sum)) ###TODO check later


############################################################
##rank by months
# TODO select for each month ie all Januarys are ranked seperate from Febs etc
#rank <- apply(x, 2, function(x) {return((rank(x)-1)/(length(x)-1))})
#get months from raster names
months <- substr(names(xx),20,21)
tw <-c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

#actually I'm not sure this is correct...

# for(i in tw){
# rankmnth <- apply(x, 2, 
#                   function(x) ifelse(i == months,
#                                      {return((rank(x)-1)/(length(x)-1))},i))
# }

#it runs faster if the loop is inside fun
ptm <- proc.time()
xx.rankmnth <- calc(xx, fun=function(x) for(i in tw){ifelse(i == months,
{return((rank(x)-1)/(length(x)-1))},i)})
proc.time() - ptm
## user  system elapsed 
## 46.38    0.06   46.47

############################################################
## calc drought index
#index <- apply(rankmnth, 2, function(x) 8*(x-.5))
ptm <- proc.time()
xx.index <- calc(xx.rankmnth, fun=function(x) 8*(x-.5))
proc.time() - ptm
## user  system elapsed 
## 7.10    0.05    7.14


# .375 is refering to palmer's benchmark but we could let the user vary this
#drought <- apply(x, 2, function(x) x<=quantile(x,.375)) #TO DO: replace number by argument droughtThreshold
ptm <- proc.time()
xx.drought <- calc(xx, fun=function(x) x<=quantile(x,.375)) #TO DO: replace number by argument droughtThreshold
proc.time() - ptm
## user  system elapsed 
## 136.72    0.07  137.03 

ptm <- proc.time()
xx.indexBelowThreshold <- xx.index*xx.drought
proc.time() - ptm
## user  system elapsed 
## 0.67    0.12    0.80
############################################################
##count
# x1 <- index<=-1
# x2 <- apply(x1, 2, function(x) (cumsum(!x) + 1) * x )
# seq <- apply(x1, 2, function(x) seq_along(x))
# match <- apply(x2, 2, function(x) match(x,x))
# count<- (seq - match + 1) * x1 #double check #to be a brick


## TODO check function
ptm <- proc.time()
xx.count <- calc(xx.index, fun=function(x) ifelse(x<=-1, (seq_along(x) - 
                match((cumsum(!x) + 1) * x,(cumsum(!x) + 1) * x) + 
                1),x) )
proc.time() - ptm
## user  system elapsed 
## 42.23    0.08   42.40 


## plot any random pixel
plot(xx.count@data@values[750, 1:12], type = "l", ylim=c(0,10))
abline(5,0)
##TO DO: count2 and sums, convert matrices to bricks.

# In the enhanced version rather than stop counting when the rescaled percentiles rise above -1.0, 
# we keep counting the months (or adding the negative anomalies) 
# if the rescaled percentile is below 0.0 AND the drought threshold has already been reached. 
# If the threshold has not been reached, then stop counting (or adding) as before 
# if the rescaled percentile rises above -1.0.

## Ivan: why do you stat the loop in 2? line 97 of your function
xx.count2 <- overlay(xx.count,xx.index, fun=function(x,j) ifelse(x>=5 & j<=0, x + 1,x) )

plot(xx.count2@data@values[750, 1:12], type = "l", col='blue', ylim=c(0,10))
abline(5,0)

############################################################
##SUMS


xx.sum <- calc(xx.index, fun=function(x) unsplit(lapply(split(ifelse(x >= -1, 0, x),
                                         (cumsum(!x < -1) + 1) * x < -1),cumsum),
                                         ((cumsum(!x < -1) + 1) * x < -1)))


#check warning msje
xx.sum2 <- overlay(xx.sum, xx.index, fun=function(x,j) if(x<= -17.5 & j <= 0){
                                                          sum(x,j)}else{x})


