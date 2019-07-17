
#Generic Functions----
getParticipantTaskData <- function(id, taskno, task) {
  
  if (id < 10){
    filepath <- sprintf('data/pilot/SELECTED/p00%d/p00%d-%d-%s_selected.txt', id, id, taskno,task) #creates the file path relative to current directory
  } else{
    filepath <- sprintf('data/pilot/SELECTED/p0%d/p0%d-%d-%s_selected.txt', id, id, taskno,task) #creates the file path relative to current directory
  }
  
  df <- read.table(file = filepath) #these files need headers to be added
  # count the number of columns
  CountCol <- ncol(df)
  
  
  # 19 columns, use these as headers
  if (CountCol == 19) {
    colnames(df) <- c("cursorx_cm", "cursory_cm", "homex_cm", "homey_cm", "mousex_cm", "mousey_cm",
                      "participant", "rotation", "step", "targetangle_deg", "targetx_cm", "targety_cm",
                      "time_ms", "trial", "trialselected_bool", "sampleselected_bool", "sampleinterpolated_bool",
                      "maxvelocity_idx", "unsure")
  }
  return(df)
}

rotateTrajectory <- function(X,Y,angle) {
  
  # create rotation matrix to rotate the X,Y coordinates
  th <- (angle/180) * pi
  R <- t(matrix(data=c(cos(th),sin(th),-sin(th),cos(th)),nrow=2,ncol=2))
  
  # put coordinates in a matrix as well
  coordinates <- matrix(data=c(X,Y),ncol=2)
  
  # rotate the coordinates
  Rcoordinates <- coordinates %*% R
  
  # return the rotated reach
  return(Rcoordinates)
  
}

getBSConfidenceInterval <- function(data, resamples) {
  
  data <- data[which(is.finite(data))] #need is.finite due to NA values
  
  #bootstrap to 95% CI with replacement (done when normal t-distribution is not assumed)
  #so multiplies data times 1000 and replaces the values
  samplematrix <- matrix(sample(data, size = resamples*length(data), replace = TRUE), nrow = resamples)
  #apply mean function to this new matrix
  BS <- apply(samplematrix, c(1), FUN=mean) #use mean instead of median (we mainly use mean for analyses, even though median is robust to outliers)
  #95% confidence that data falls within range
  #2.5% to 97.5%, with 50% being the 'median mean', which is close to actual mean
  return(quantile(BS, probs = c(0.025, 0.50, 0.975)))
  
}

t.interval <- function(data, variance = var(data, na.rm = TRUE), conf.level = 0.95) {
  #same as getConfidenceInterval, but assumes a normal t-distribution
  
  z <- qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
  
  xbar <- mean(data, na.rm = TRUE)
  sdx <- sqrt(variance/length(data))
  
  return(c(xbar - z * sdx, xbar, xbar + z * sdx)) 
  
}

#Learning Rates----

getTrialReachAngleAt <- function(trialdf, location = 'maxvel') {
  
  
  # location (string) determines where the angle of thereach is determines, it is one of:
  # maxvel: maximum velocity (default)
  # endpoint: end of the reach
  # cmX: the last sample before this distance from home, where X is replaced by a numeral
  
  # return a matrix of two numbers:
  reachangle = matrix(data=NA,nrow=1,ncol=1)
  
  # if the trial was rejected, return empty matrix now
  if (trialdf[1,'trialselected_bool'] == 0) {
    
    return(reachangle);
    
  }
  
  # extract the relevant reach information
  X <- trialdf[trialdf$sampleselected_bool == 1,'mousex_cm']
  Y <- trialdf[trialdf$sampleselected_bool == 1,'mousey_cm']
  MV <- trialdf[trialdf$sampleselected_bool == 1,'maxvelocity_idx']
  angle <- trialdf[1,'targetangle_deg']
  
  # print(X)
  
  # rotate the trajectory
  # (this avoids problems in the output of atan2 for large angles)
  trajectory <- rotateTrajectory(X,Y,-1*angle)
  X <- trajectory[,1]
  Y <- trajectory[,2]
  
  # now try find the specified location in this reach:
  # if we can't find it, we need to know
  invalidlocation <- TRUE
  
  # maximum velocity, should be in the data
  if (location == 'maxvel') {
    rown <- which(MV == 1)
    if (length(rown) > 1) {
      rown <- rown[1]
    }
    if (length(rown) == 0) {
      # no maximum velocity defined!
      return(reachangle)
    }
    invalidlocation <- FALSE
  }
  # end point, just the last point in the selected stretch of the reach
  if (location == 'endpoint') {
    rown <- length(X)
    invalidlocation <- FALSE
  }
  # cutoff in centimers, the last sample before this cutoff distance is reached
  # this assumes that people don't go back, or that there is only one movement from home to target
  if (substring(location,1,2) == 'cm') {
    distance <- as.numeric(substring(location, 3))
    
    # get the distance from home:
    dist <- sqrt(X^2 + Y^2)
    
    # if there are no selected samples below 3 cm: return NAs
    if (length(which(dist < distance)) == 0) {
      return(reachangle)
    }
    
    # find the last sample, where dist < 3
    rown <- max(which(dist < distance))
    invalidlocation <- FALSE
  }
  
  # if we don't have a valid location, we can't calculate an angle to return
  if (invalidlocation) {
    return(reachangle)
  }
  
  # calculate the angle at that point for the rotated trajectory
  # this is the angular deviation we are looking for
  angulardeviation <- (atan2(Y[rown],X[rown]) / pi) * 180
  
  # put the result in the little matrix:
  reachangle[1,1] <- angulardeviation
  #reachangle[1,2] <- angle #I don't know why I have to remove this for it to work!But it's the only thing keeping this function from being generic
  
  return(reachangle)
  
}

getReachAngles <- function(df, starttrial=0, endtrial=NULL, location = 'maxvel') {
  
  trialnumbers <- c(starttrial:endtrial)
  
  #place holders for variables in data frame
  trial <- c()
  targetangle <- c()
  reachdev <- c()
  
  for (trialnumber in trialnumbers) {
    
    indices <- which(df$trial == trialnumber) #rows of current trial
    
    if (length(indices) > 0) { 
      
      trialdf <- subset(df, trial == trialnumber) #get current trial number
      targetangle <- c(targetangle, trialdf$targetangle_deg[1]) #target angle in degrees (45, 90, or 135); 1 is the index to get this value
      reachdev <- c(reachdev, getTrialReachAngleAt(trialdf, location = location)) #relies on reach deviation function
      trial <- c(trial, trialnumber) #counter to keep going
      
    } else {
      #set values to NA if not greater than zero
      #this part helps to fill in missing values in data
      targetangle <- c(targetangle, NA)
      reachdev <- c(reachdev, NA)
      trial <- c(trial, trialnumber) #trial numbers would still be displayed (i.e., is not NA)
      
    }
    
    
  }
  
  #build a data frame
  angularreachdeviations <- data.frame(trial, targetangle, reachdev)
  return(angularreachdeviations)
  
}

getAlignedTrainingBiases <- function(df, location) {
  
  #skip first 15 trials
  df <- getReachAngles(df=df, starttrial = 0, endtrial = 48, location = location) 
  #get median reachdev for each angle
  trainingBiases <- aggregate(reachdev ~ targetangle, data= df, FUN = median) 
  return(trainingBiases)
  
}