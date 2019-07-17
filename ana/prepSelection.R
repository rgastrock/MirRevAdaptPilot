
getStepReach <- function(id, task, taskno){
  #allows for this function to work with each file
  #specify pp id, the task type, and task number
  #note that task type and taskno have to match, depending on present csv files
  #Note to change filepath once data folder is arranged
  if (id < 10){
    dat <- read.csv(file = sprintf('data/pilot/RAW/p00%d/p00%d-%d-%s.csv', id, id, taskno,task))
  } else{
    dat <- read.csv(file = sprintf('data/pilot/RAW/p0%d/p0%d-%d-%s.csv', id, id, taskno,task))
  }
  
  #use line below to only include particular steps
  #as of now raw files will have all steps
  #after selection, only steps 3 to 5 are included in file from GUI
  #ndat <- dat[dat$step == 3 | dat$step == 4 | dat$step == 5, ]
  ndat <- subset(dat, select = - trial_correct) #remove last column (trial_correct)
  #want it as txt file so that it can work with selection GUI
  if (id < 10){
    write.table(ndat, file = sprintf("data/pilot/RAW/p00%d/p00%d-%d-%s.txt", id, id, taskno,task), sep = "\t",
                row.names = FALSE)
  } else{
    write.table(ndat, file = sprintf("data/pilot/RAW/p0%d/p0%d-%d-%s.txt", id, id, taskno,task), sep = "\t",
                row.names = FALSE)
  }
}

getAllStepReach <- function(id){
  #if pp id is odd
  #mirror is first than rotation
  if (id%%2 == 1){
    getStepReach(id = id, task = 'aligned', taskno = 1)
    getStepReach(id = id, task = 'random0', taskno = 3)
    getStepReach(id = id, task = 'mirror', taskno = 5)
    getStepReach(id = id, task = 'washout0', taskno = 7)
    getStepReach(id = id, task = 'random1', taskno = 9)
    getStepReach(id = id, task = 'rotation', taskno = 11)
    getStepReach(id = id, task = 'washout1', taskno = 13)
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    getStepReach(id = id, task = 'aligned', taskno = 1)
    getStepReach(id = id, task = 'random0', taskno = 3)
    getStepReach(id = id, task = 'rotation', taskno = 5)
    getStepReach(id = id, task = 'washout0', taskno = 7)
    getStepReach(id = id, task = 'random1', taskno = 9)
    getStepReach(id = id, task = 'mirror', taskno = 11)
    getStepReach(id = id, task = 'washout1', taskno = 13)
  }
  
}