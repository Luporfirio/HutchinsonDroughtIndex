getwd()
##Lu 13-14 Jan 2014
require(raster); require(rgdal)
##path?
awap.grids = dir(pattern = "grid$", full.names=T) 
#  list.files('AWAP_GRIDS', pattern=glob2rx('totals*.grid'), full.names=T)
for(i in 1:12){
  #i = 1
  #file.copy(awap.grids[i], sprintf("foo%s.grid", i))}
  r <- raster(awap.grids[i])
  #str(r)
  #image(r)
  fname <- gsub(".grid",".tif", awap.grids[i])
  # TODO project this please lu!
  writeRaster(r, filename= fname, type = "GTiff")
  #file.remove(awap.grids[i])
}
## for some reason brick or stack only don't work, both together do
awap.grids <- dir(pattern = 'tif')
rasterbrick <- brick(stack(awap.grids)) #takes too l

## I'm not sure what's more efficient, if changing the drought function 
## to do the cal on matrices or just running the function on the vectors

##option 1 modif function

#rasterdroughtIndex<-function(data,years,droughtThreshold=.375){

##Ivan's comments (...)
#The values returned for a RasterStack or RasterBrick are always a matrix, 
#with the rows representing cells, and the columns representing layers
#b is a matrix that contains grid values
#to be replaced by getValues(raster) >> problem: Error: cannot allocate vector of size 5.8 Gb
b<-getValuesBlock(rasterbrick, row=500, nrows=5, col=500, ncols=5)
# TODO estimate the max and min date from the data filenames
x<-apply(b, 1, function(x) ts(x,start=c(1900, 01),end=c(1900,12),frequency=12))
sixmnthtot<-apply(x, 2, function(x) c(rep(NA,5),x+lag(x,1)+lag(x,2)+lag(x,3)+lag(x,4)+lag(x,5)))
# TODO it might be faster to use rollapply, and also we can make the lag length 
# variable
require(zoo)
?rollapply
##rank
# TODO select for each month ie all Januarys are ranked seperate from Febs etc
rank <- apply(x, 2, function(x) {return((rank(x)-1)/(length(x)-1))})
index <- apply(rank, 2, function(x) 8*(x-.5)) #to be a brick
# .375 is refering to palmer's benchmark but we could let the user vary this
drought <- apply(x, 2, function(x) x<=quantile(x,.375)) #TO DO: replace number by argument droughtThreshold
indexBelowThreshold <- index*drought #to be a  brick

##count
x1 <- index<=-1
x2 <- apply(x1, 2, function(x) (cumsum(!x) + 1) * x )
seq <- apply(x1, 2, function(x) seq_along(x))
match <- apply(x2, 2, function(x) match(x,x))
count<- (seq - match + 1) * x1 #double check #to be a brick

##TO DO: count2 and sums, convert matrices to bricks.
plot(count[,1], type = "l")