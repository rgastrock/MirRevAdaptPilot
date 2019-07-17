getLaggedD <- function(id, task, taskno){
  #allows for this function to work with each file
  #specify pp id, the task type, and task number
  #note that task type and taskno have to match, depending on present csv files
  #Note to change filepath once data folder is arranged
  if (id < 10){
    dat <- read.csv(file = sprintf('data/pilot/RAW/p00%d/p00%d-%d-%s.csv', id, id, taskno,task))
  } else{
    dat <- read.csv(file = sprintf('data/pilot/RAW/p0%d/p0%d-%d-%s.csv', id, id, taskno,task))
  }
  time <- dat$time_ms
  
  laggedD <- diff(time)
  medlaggedD <- median(laggedD)
  #median value will be the same for all tasks and participants
  
  #minlaggedD <- min(laggedD)
  #max(laggedD)
  #mean(laggedD)
  
  return(medlaggedD)

}

getAllLaggedD <- function(id){
  #if pp id is odd
  #mirror is first than rotation
  if (id%%2 == 1){
    D1 <- getLaggedD(id = id, task = 'aligned', taskno = 1)
    D2 <- getLaggedD(id = id, task = 'random0', taskno = 3)
    D3 <- getLaggedD(id = id, task = 'mirror', taskno = 5)
    D4 <- getLaggedD(id = id, task = 'washout0', taskno = 7)
    D5 <- getLaggedD(id = id, task = 'random1', taskno = 9)
    D6 <- getLaggedD(id = id, task = 'rotation', taskno = 11)
    D7 <- getLaggedD(id = id, task = 'washout1', taskno = 13)
    
    allLaggedD <- rbind(D1,D2,D3,D4,D5,D6,D7)
    
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    D1 <- getLaggedD(id = id, task = 'aligned', taskno = 1)
    D2 <- getLaggedD(id = id, task = 'random0', taskno = 3)
    D3 <- getLaggedD(id = id, task = 'rotation', taskno = 5)
    D4 <- getLaggedD(id = id, task = 'washout0', taskno = 7)
    D5 <- getLaggedD(id = id, task = 'random1', taskno = 9)
    D6 <- getLaggedD(id = id, task = 'mirror', taskno = 11)
    D7 <- getLaggedD(id = id, task = 'washout1', taskno = 13)
    
    allLaggedD <- rbind(D1,D2,D3,D4,D5,D6,D7)
  }
  return(allLaggedD)
  #16.684 to 16.687 is the range of median values across tasks and participants
  #1000 ms in a sec, divided by 60 frames per second are these numbers
  #sampling rate is 60 Hz (Hz is X per second, in this case: frames per second)
}