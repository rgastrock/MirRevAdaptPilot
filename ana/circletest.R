#test circle for where the centre is, so that we can fix the offset in experiment
#run one trial and only get step 0

df <- read.csv(file = 'data/circlefit/p001-1-0.csv') #manually change filenames every test

df <- df[which(df$step == 0),] #get only step 0, because we drew a circle with the pen

plot(df$mousex_px, df$mousey_px)

#remove the line going to centre (look at df to identify which point this line starts from)
points(-15,-185,pch=2, col='red') #roughly the line starts at (mouseX, mouseY)
# x going down is another indicator that these are samples for the line (cut samples below corresonding time of this point)
dfnew <- df[which(df$time_ms < 2800),] #change ms per try
plot(dfnew$mousex_px, dfnew$mousey_px)

dat <- data.frame(dfnew$mousex_px, dfnew$mousey_px)
colnames(dat) <- c('x','y')

#monitor pixels
px_x <- 1024
px_y <- 768
#tablet workspace in cm (12.1 x 8.4 inches)
cm_x <- 30.734
cm_y <- 21.336

#picel per cm ratio
PPC_x <- px_x/cm_x
PPC_y <- px_y/cm_y

# from here, we calculate mouse x and y in cm
mousex_cm <- dat$x/PPC_x
mousey_cm <- dat$y/PPC_y

plot(mousex_cm, mousey_cm, asp=1)

#use modified circle fitting function
#if unsure of radius, function will specify radius of your data
fit1 <- circleFitR(x=mousex_cm, y=mousey_cm, radius = 4.8)

points(fit1$x, fit1$y, col='red') #centre of circle from data points

#see if a line will fit data well
a <- seq(0, 2*pi, length.out = 181)
lines((sin(a)*4.8)+(fit1$x), (cos(a)*4.8)+(fit1$y), asp=1, col='blue') #multiply by radius, then add offsets

#' @title Get origin for points that should fall on a circle
#' @param x The x-coordinates of the data.
#' @param y The y-coordinates of the data. 
#' @param radius The radius of the circle the data should fall on. Usually we 
#' have data in centimeters, and movements of 12 centimeters.
#' @return A vector with an x and y coordinate best fitting the circle.
#' @description This utility function can be used to see used to find the 
#' optimal central point, where optimal means finding the location where
#' the mean distance of each data point to the central point matches the given
#' radius best.
#' @details 
#' ?
#' @examples
#' 
#' data("localization_aligned")
#' data("localization_unaligned")
#' 
#' localization_aligned <- convert2cm(localization_aligned, from='r')
#' localization_aligned <- convert2cm(localization_aligned, from='t')
#' 
#' par(mfrow=c(1,2))
#' 
#' plot(localization_aligned$tapx_cm, localization_aligned$tapy_cm, main='aligned',xlab='cm', ylab='cm', asp=1,bty='n', xlim=c(-2,12),ylim=c(-2,12),col='blue')
#' segments(localization_aligned$tapx_cm, localization_aligned$tapy_cm, localization_aligned$handx_cm, localization_aligned$handy_cm, col='blue')
#' lines(c(-1,11),c(0,0),col='gray')
#' lines(c(0,0),c(-1,11),col='gray')
#' lines(cos(seq(0,pi/2,pi/200))*10,sin(seq(0,pi/2,pi/200))*10, col='gray')
#' centre <- circleFit(localization_aligned$tapx_cm, localization_aligned$tapy_cm, radius=10)
#' points(centre$x, centre$y, col='red')
#' lines( (cos(seq(0,pi/2,pi/200))*10) + centre$x, (sin(seq(0,pi/2,pi/200))*10) + centre$y, col='red', lty=2)
#' points(localization_aligned$tapx_cm - centre$x, localization_aligned$tapy_cm - centre$y, col='green')
#' segments(localization_aligned$tapx_cm - centre$x, localization_aligned$tapy_cm - centre$y, localization_aligned$handx_cm, localization_aligned$handy_cm,, col='green')
#' 
#' 
#' localization_unaligned <- convert2cm(localization_unaligned, from='r')
#' localization_unaligned <- convert2cm(localization_unaligned, from='t')
#' 
#' plot(localization_unaligned$tapx_cm, localization_unaligned$tapy_cm, main='unaligned',xlab='cm', ylab='cm', asp=1,bty='n', xlim=c(-2,12),ylim=c(-2,12),col='blue')
#' segments(localization_unaligned$tapx_cm, localization_unaligned$tapy_cm, localization_unaligned$handx_cm, localization_unaligned$handy_cm, col='blue')
#' lines(c(-1,11),c(0,0),col='gray')
#' lines(c(0,0),c(-1,11),col='gray')
#' lines(cos(seq(0,pi/2,pi/200))*10,sin(seq(0,pi/2,pi/200))*10, col='gray')
#' centre <- circleFit(localization_unaligned$tapx_cm, localization_unaligned$tapy_cm, radius=10)
#' points(centre$x, centre$y, col='red')
#' lines( (cos(seq(0,pi/2,pi/200))*10) + centre$x, (sin(seq(0,pi/2,pi/200))*10) + centre$y, col='red', lty=2)
#' points(localization_unaligned$tapx_cm - centre$x, localization_unaligned$tapy_cm - centre$y, col='green')
#' segments(localization_unaligned$tapx_cm - centre$x, localization_unaligned$tapy_cm - centre$y, localization_unaligned$handx_cm, localization_unaligned$handy_cm,, col='green')
#' @export
circleFit <- function(x, y, radius=12, verbosity=0) {
  
  coords  <- data.frame(x, y)
  
  if ('optimx' %in% installed.packages()) {
    
    library(optimx)
    
    lower <- c(min(coords$x)-radius,min(coords$y)-radius)
    upper <- c(max(coords$x)+radius,max(coords$y)+radius)
    
    circlefit <- optimx(par=c('x'=0, 'y'=0), circleFitError, gr = NULL, method='L-BFGS-B', lower=lower, upper=upper, coords=coords, radius=radius)
    
    return(list('x'=circlefit$x, 'y'=circlefit$y))
    
  } else {
    
    if (verbosity > 0) cat('optimx not installed, falling back on optim\n')
    
    circlefit <-  optim(par = c('x'=0, 'y'=0), circleFitError, gr = NULL, coords=coords, radius=radius)
    
    return(list('x'=circlefit$par['x'],'y'=circlefit$par['y']))
    
  }
  
}

circleFitR <- function(x, y, radius = 5, verbosity=0) {
  
  coords  <- data.frame(x, y)
  

    
    library(optimx)
    
    lower <- c(min(coords$x)-radius,min(coords$y)-radius,0.001)
    upper <- c(max(coords$x)+radius,max(coords$y)+radius,radius*3)
    
    circlefit <- optimx(par=c('x'=0, 'y'=0, 'radius'=radius), circleFitRError, gr = NULL, method='L-BFGS-B', lower=lower, upper=upper, coords=coords)
    
    return(list('x'=circlefit$x, 'y'=circlefit$y, 'radius'=circlefit$radius))
    

  
}

circleFitRError <- function(par, coords){
  
  return(mean((sqrt((coords$x - par['x'])^2 + (coords$y - par['y'])^2) - par['radius'])^2, na.rm=TRUE))
  
}
#' @title Errors between coordinates' radial distance to a point and a given 
#' radius
#' @param par A vector with elements x and y denoting the central point's 
#' coordinates.
#' @param data A data frame with columns x and y.
#' @param radius The radius the points in the data should have relative to a 
#' central point.
#' @return The mean difference between the radius and the distance between all
#' coordinates in the data and the central point.
#' @description This utility function can be used to see how well a set of 2D 
#' cartesian functions fit on a circle. The criterion it uses is radial 
#' distance to a given point. First, the distances of all the data to this 
#' point is calculated. Then the target radius is subtracted, resulting in a
#' vector of errors. The mean of the squared errors is returned. This number 
#' should always be positive and lower is better.
#' This function is used to find the optimal central point.
#' @details 
#' ?
#' @examples
#' ?
#' @export
circleFitError <- function(par, coords, radius){
  
  return(mean((sqrt((coords$x - par['x'])^2 + (coords$y - par['y'])^2) - radius)^2, na.rm=TRUE))
  
}


#script to test circle where targets are located

df <- read.csv(file = 'data/p000-1-0.csv')

minX <- min(df$mousex_px)
maxX <- max(df$mousex_px)
Xval <- c(minX, maxX)

minY <- min(df$mousey_px)
maxY <- max(df$mousey_px)
Yval <- c(minY, maxY)


averageX <- mean(Xval)
averageY <- mean(Yval)

Xval <- c(Xval, averageX)
Yval <- c(Yval, averageY)
allval <- rbind(Xval,Yval)
colnames(allval) <- c('min_val','max_val', 'mean_val')
