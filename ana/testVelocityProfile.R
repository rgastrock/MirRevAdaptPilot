library(signal)

getVelocityProfile <- function(isi){
  #plot the velocity profile of a reach within each trial
  #we apply a butterworth filter 
  #this corrects for the jumping data points or samples when moving
  #we only want the lower frequencies to pass through
  #in effect, we smooth the profile
  par(mfrow = c(6,2), mai = c(0.3,0.3,0.3,0.3))
  for (trialno in c(0:11)){
    dat <- read.csv(sprintf('data/p000-1-%d.csv', trialno))
    
    #X <- df$mousex_px
    #Y <- df$mousey_px
    #t <- df$time_ms
    
    #use the splined values
    samp <- getConstantSample(df=dat, isi=isi)
    
    #separate for X and Y
    #1st order, W, only low can pass
    #W goes from 0 to 1 (analogue vs digital, digital just converts analogue to binary)
    #1 is the most unsmoothed value, 0 is a straight smooth line
    #W is basically a cutoff, where lower values will pass 
    #(note that this is not a sharp cutoff, it slopes, 
    #so you would want the cutoff value to still be before this sloping down)
    #W has numerator of cutoff frequency (1 Hz means moved back and forth once in a sec, 3 means more movements in a sec)
    #denominator is Hz of signal (samples per second)
    #if isi = 10ms, W = 3/100; isi=20, W=3/50; isi=40, W=3/25
      #not much differences here, scale of X axis changes a bit (3.5 to 4 secs)
      #much f differences occur at right end of plot (tails)
    butterfilter <- butter(1, 3/25, type = 'low')
    filtX <- filtfilt(butterfilter, samp$newX)
    filtY <- filtfilt(butterfilter, samp$newY)
    
    #pythagorean theorem, essentially, to get distance
    #diff compares value 2 minues 1, 3 minus 2, etc.
    #distance over time
    velocity <- (sqrt((diff(filtX))^2 + (diff(filtY))^2))/ diff(samp$newT)
    
    #so now plot it
    #time will be missing one value as an effect of diff function in velocity calc
    plot(samp$newT[2:length(samp$newT)], velocity, type = "l", col="red")
  }
}


getConstantSample <- function(df, isi=20){
  
  #we take the X and Y coordinates of mouse pos
  #however time_ms samples inconsistently
  #so if we want coords at consistent time points
  #we would need to interpolate coords for these times
  #hence we do a spline
  X <- df$mousex_px
  Y <- df$mousey_px
  t <- df$time_ms
  
  #fmm is deafult, see help file for splinefun
  Xsplinefun <- splinefun(x=t, y = X, method = 'fmm')
  Ysplinefun <- splinefun(x=t, y = Y, method = 'fmm')
  
  #isi = interstimulus interval
  #we set it to constant samples of 20ms
  newT <- seq(t[1],t[length(t)],by=isi)
  
  newX <- Xsplinefun(x=newT)
  newY <- Ysplinefun(x=newT)
  
  #after spline, we get corresponding X, Y, time
  newdf <- data.frame(newX,newY,newT)
  
  return(newdf)
}
  

