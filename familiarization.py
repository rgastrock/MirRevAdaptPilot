import pandas as pd
import scipy as sp
import time
import os
#import cProfile
#import re
import gc
from random import shuffle, seed
from psychopy import prefs
prefs.general['audioLib'] = ['pygame']
from psychopy import gui, visual, event, core, sound
from ctypes import *
#from Xlib import display


def runExp():
        #set up config object
        cfg = {}

        #participant info
        cfg = getParticipantID(cfg)
        
        #set up psychopy environment:
        cfg = openEnvironment(cfg)

        cfg = makeStimuli(cfg)

        # set random number seed (so participants have different trial orders)
        seed(cfg['id']*999)

        cfg = makeBlocks(cfg)
        #loop through blocks
        for blockno in range(0,len(cfg['blocks'])):
                #print(cfg['blocks'][blockno]['instruction'])
                #print(cfg['blocks'][blockno]['trials'])

                cfg['taskno'] = blockno
                blockDef = cfg['blocks'][blockno]

                #if instruc not empty, show it
                if len(cfg['blocks'][blockno]['instruction']) > 0:
                        cfg = showInstruc(cfg)



                #loop through trials
                ntrials = blockDef['trials'].shape[0]
                #if (cfg['mode']=='test'):
                #        ntrials = 12

                
                for trialno in range(ntrials):
                        cfg['trialno'] = trialno
                        cfg = runTrial(cfg)

                        gc.collect()
                        
                #each trial is put into a csv file at first, eventually they grouped accdg. to task
                taskdata = 0

                for trialno in range(ntrials):
                        
                        filename = "p%03d-%d-%d.csv" % (cfg['id'], cfg['taskno'],trialno)
                        #print(filename)
                        filepath = os.path.join('data/familiarization', filename)
                        trialdata = pd.DataFrame.from_csv(path = filepath, index_col = None )
                                
                        if isinstance(taskdata, pd.DataFrame):
                                taskdata = pd.concat([taskdata, trialdata])
                        else:
                                taskdata = trialdata

                #append whether it is a mirror/rotation block (perturbtype)
                if blockDef['tasktype'][:7] == 'perturb':
                        perturbtype = blockDef['trials'].iloc[0,2]                
                        filename = "p%03d-%d-%s.csv" % (cfg['id'], cfg['taskno'],perturbtype)
                else:
                        filename = "p%03d-%d-fam.csv" % (cfg['id'], cfg['taskno'])
                                
                #print(filename)
                filepath = os.path.join('data/familiarization',filename)
                taskdata.to_csv(filepath, index = False )
                        
                trialdata = pd.DataFrame()
                del trialdata
                taskdata = pd.DataFrame()
                del taskdata
                gc.collect()
        cfg = closeEnvironment(cfg)

def runTrial(cfg):
        blockno = cfg['taskno']
        trialno = cfg['trialno']
        blockDef = cfg['blocks'][blockno]

        trialDef = blockDef['trials'].iloc[trialno]
        
        target = trialDef.target
        rotation = trialDef.rotation
        perturbation = trialDef.perturbation
        axis = trialDef.axis

        #define target positions
        theta = target * sp.pi/180
        targetx = sp.cos(theta)*cfg['targetdistance']
        targety = sp.sin(theta)*cfg['targetdistance']
        targetx = targetx + cfg['home'].pos[0]
        targety = targety + cfg['home'].pos[1]

        cfg['targetgo'].pos = [targetx,targety]
        cfg['targetstay'].pos = [targetx,targety]
        #print(cfg['target'].pos)
        #cfg['cursor'].pos = [0,0]

        #define steps:
        # 0: reach home | only present for 1st trial of every block
        # 1: hold home for 300 ms
        # 2: planning - stay in position for 1 second, target revealed
        # 3: leave home
        # 4: reach target
        # 5: stay in position for 1 sec
        # 6: reach home
        # 7: end trial

        #set up variables for trial control:
        step = 0
        trialcorrect = float('NaN')
        #print(step)
        #create vectors to collect data samples:
        cursorx_px = []
        cursory_px = []
        mousex_px = []
        mousey_px = []
        time_ms = []
        step_vec = []
        correct = []
        #cursorendpt = []
        
        mousepos = cfg['mouse'].Pos()
        trial_start_time = mousepos[2]*1000

        #use below for setting up movement time constraints
        #movetooslow = False
        #moveduringerrorfeedback = False #set to true later if they do this then change color at last step when moving back home

        while (step < 7):
            # where would the cursor be?
            mousepos = cfg['mouse'].Pos()
            #set cursor position if step 5
            if (step == 5):
                    cfg['cursor'].pos = cursorendpt
            else:
                    #for everything else, including perturbations
                    # get cursor position
                    cursorpos = mousepos[0:2]

                    cfg['cursor'].pos = cursorpos
                    #print(cursorpos)
                    #print(step)

            if (step == 0):
                #reach home
                #need to bring cursor to home position

                movetooslow = False
                moveduringerrorfeedback = False #set to true later if they do this then change color at last step when moving back home

                # show home position and cursor
                cfg['home'].draw()
                cfg['cursor'].draw()
                # if distance between cursor and home position is lower than XYZ, move on to step 1 
                # a = cursorx - homex ; b = cursory - homey ; c^2 = a^2 + b^2; c = distance between cursor and home
                if sp.sqrt((cursorpos[0] - cfg['home'].pos[0])**2 + (cursorpos[1] - cfg['home'].pos[1])**2) < cfg['radius']:
                        #get the time point after step 0
                        step0end = time.time()
                        step = 1
                
            if (step == 1):
                #hold home for 300 ms
                cfg['home'].draw()
                cfg['cursor'].draw()

                # if distance between cursor and home position is lower than XYZ for 300 ms, move on to step 2
                # also needs to have been there for 300 ms
                #determine which samples to use
                #changed 100 to 300 below
                sample_idx = sp.array([s >= (time_ms[-1]-300) for s in time_ms]).nonzero()[0]
                if (time.time() - step0end) > 0.3 and len(sample_idx) > 0:
                        curX = sp.array(cursorx_px)[sample_idx] - cfg['home'].pos[0]
                        curY = sp.array(cursory_px)[sample_idx] - cfg['home'].pos[1]

                        maxdist = max((curX**2 + curY**2)**0.5) #is this the within 0.5cm of home?

                        if maxdist < cfg['radius']:
                                step1end = time.time()
                                step = 2
                else:
                        pass


            if (step == 2):

                #1 second wait
                #need to keep still for 1 second, otherwise will repeat step 1
                #show home and target
        

                cfg['home'].draw()
                cfg['cursor'].draw()
                cfg['targetstay'].draw()


                #want to control mouspos in X and Y directions
                #can probably make this more efficient?
                if mousepos[0] > (cfg['home'].pos[0] + cfg['radius']):
                        step = 1
                if mousepos[1] > (cfg['home'].pos[1] + cfg['radius']):
                        step = 1
                
                # if distance between cursor and home position is lower than XYZ for 1 sec, move to step 3
                sample_idx = sp.array([s >= (time_ms[-1]-1000) for s in time_ms]).nonzero()[0]
                #move to step 2, how much time has passed since going to step 2
                #then check for distance to home for the last second (which is what you have right now)
                if (time.time() - step1end) > 1 and len(sample_idx) > 0:
                        curX = sp.array(cursorx_px)[sample_idx] - cfg['home'].pos[0]
                        curY = sp.array(cursory_px)[sample_idx] - cfg['home'].pos[1]
                        #print(curX)
                        #print(curY)
                        maxdist = max((curX**2 + curY**2)**0.5) #is this the within 0.5cm of home?
                        #print(maxdist)
                        if maxdist < cfg['radius']:
                                step2end = time.time()
                                step = 3
                else:
                        pass                
            if (step == 3):
                #leave home
                #see target and leave home position

                        
                # show target
                cfg['targetgo'].draw()
                # show cursor
                cfg['cursor'].draw()
                        
                # if distance between cursor and home is higher than XYZ, move on to step 4
                if sp.sqrt((cursorpos[0] - cfg['home'].pos[0])**2 + (cursorpos[1] - cfg['home'].pos[1])**2) > cfg['radius']:
                        #step3end = time.time()
                        step = 4

            if (step == 4):
                #reach target
                # participant needs to reach towards distance of target

                # show target
                cfg['targetgo'].draw()
                # show cursor
                cfg['cursor'].draw()

                ##if distance between cursor and target is lower than XYZ, move on to step 5
                #if sp.sqrt((cursorpos[0] - cfg['targetgo'].pos[0])**2 + (cursorpos[1] - cfg['targetgo'].pos[1])**2) < cfg['radius']:
                #        step4end = time.time()
                #        step = 5

                #make a new condition, such that movement at a certain distance from home will be end of step
                #regardless of whether target was acquired
                #if distance between cursor and home is greater than target distance from home, move on to step 5
                if sp.sqrt((cursorpos[0] - cfg['home'].pos[0])**2 + (cursorpos[1] - cfg['home'].pos[1])**2) >= sp.sqrt((cfg['targetgo'].pos[0] - cfg['home'].pos[0])**2 + (cfg['targetgo'].pos[1] - cfg['home'].pos[1])**2):
                        cursorendpt = [cursorpos[0],cursorpos[1]]
                        mouseendpt = [mousepos[0],mousepos[1]]
                        #print(cursorendpt)
                        step4end = time.time()
                        step = 5
                        
            if (step == 5):
                    
                #1 second wait
                #cursor position from step 4 is held there for 1 second

                #print (cursorendpt)
                #show home and target
                cfg['targetgo'].draw()
                cfg['cursor'].draw()
                
                # if distance between cursor and target is lower than XYZ for 1 sec, move to step 6
                sample_idx = sp.array([s >= (time_ms[-1]-1000) for s in time_ms]).nonzero()[0]
                
                if (time.time() - step4end) > 1 and len(sample_idx) > 0:
                        mouseX = sp.array(mousex_px)[sample_idx] - mouseendpt[0]
                        mouseY = sp.array(mousey_px)[sample_idx] - mouseendpt[1]

                        maxdist = max((mouseX**2 + mouseY**2)**0.5) #is this the within 0.5cm of home?

                #also where you take into account feedback about movement time and whether they were moving while feedback was still
                        #what is an "adequate" distance to say that they have moved a little too much? Currently set to diameter of stimulus
                        #movement time from leaving home position to the when cursor stops should be less than 700ms
                        if maxdist > cfg['radius']*2 or not 0.4 <= (step4end - step2end) <= 0.7: #(step4end - step2end) > 0.7:
                                moveduringerrorfeedback = True                  
                                movetooslow = True

                        cursorpos = mousepos #so that first sample in step 6 will be correct
                        step = 6


            #this will make next trial start from step 1
            #because this is the same as step 0
            #so only every first trial on every block will have step 0
            if (step == 6):
                #reach home
                # show home and cursor
                if moveduringerrorfeedback == True or movetooslow == True:
                        cfg['homefail'].draw()
                        cfg['cursor'].draw()
                        trialcorrect = 0
                        # if distance between cursor and home is lower than XYZ, set step to 7
                        if sp.sqrt((cursorpos[0] - cfg['home'].pos[0])**2 + (cursorpos[1] - cfg['home'].pos[1])**2) < cfg['radius']:
                                step = 0
                                cfg['sound'].play()
                else:
                        cfg['homesuccess'].draw()
                        cfg['cursor'].draw()
                        trialcorrect = 1
                        
                        # if distance between cursor and home is lower than XYZ, set step to 7
                        if sp.sqrt((cursorpos[0] - cfg['home'].pos[0])**2 + (cursorpos[1] - cfg['home'].pos[1])**2) < cfg['radius']:
                                step = 7
        


            cfg['win'].flip()
            #print('window flipped')
            # add data to mouse X / Y vectors, cursor X / Y vectors, time vector
            cursorx_px.append(cursorpos[0]) 
            cursory_px.append(cursorpos[1])
            mousex_px.append(mousepos[0]) 
            mousey_px.append(mousepos[1])
            time_ms.append((mousepos[2]*1000)-trial_start_time)
            step_vec.append(step)
            correct.append(trialcorrect)

        d = {'step': step_vec,
             'time_ms': time_ms,
             'mousex_px': mousex_px,
             'mousey_px': mousey_px,
             'cursorx_px': cursorx_px,
             'cursory_px':cursory_px,
             'homex_px': cfg['home'].pos[0],
             'homey_px': cfg['home'].pos[1],
             'rotation': rotation,
             'participant': cfg['id'],
             'targetangle_deg': target,
             'targetx_px': cfg['targetgo'].pos[0],
             'targety_px': cfg['targetgo'].pos[1],
             'trial': cfg['trialno'],
             'trial_correct': correct}
             #'instruction':cfg['blocks'][cfg['blockno']]['instruction'][22:29]}

        trialdata = pd.DataFrame(d)
        filename = "p%03d-%d-%d.csv" % (cfg['id'],cfg['taskno'],cfg['trialno'])
        filepath = os.path.join('data/familiarization', filename)

        trialdata.to_csv(filepath, index=False)

        del cursorx_px
        del cursory_px
        del mousex_px
        del mousey_px
        del time_ms
        del step_vec
        del correct
        
        trialdata = pd.DataFrame()
        del trialdata

        return(cfg)


def getParticipantID(cfg):
        validID = False
        while not(validID):
                #print(validID)
                str_id = raw_input('participant ID (integer): ')
                int_id = int(str_id)
 
                if '%d'%int_id == str_id:
                        validID = True
    

        cfg['id'] = int_id
        return(cfg)
                                      
def showInstruc(cfg):
        # print(cfg['blocks'][cfg['blockno']]['instruction'])
        # show this on screen!
        
       
        instruction = visual.TextStim(cfg['win'], text = cfg['blocks'][cfg['taskno']]['instruction'], height = 16)
        waitingForSpace = True
        
        while waitingForSpace: 
                pressed = event.getKeys(keyList = ['space'])
                instruction.draw()
                cfg['win'].flip()
                if len(pressed) > 0:
                        cfg['win'].flip()
                        waitingForSpace = False

        
        return(cfg)

def openEnvironment(cfg):
        
        # 47.4 * 29.6 
        
        # 1680 / 47.4 ~ 34.443 pix per cm
        # 1050 / 29.6 ~ 35.4729 pix per cm
        
        # let's say 35 pix / cm?
        
        cfg['pixpercm'] = 35

        #winSize = [1350, 750]
        #winSize = [640,480]
        winSize = [1680, 1050]
        
        cfg['win'] = visual.Window(size = winSize, color =(0,0,0), units ='pix', fullscr=True, winType = 'pyglet') #, gamma=256)
        
        
        cfg['winSize'] = winSize
        
        cfg['psyMouse'] = event.Mouse(visible = False, newPos = None, win = cfg['win'])
        
        cfg = addMouse(cfg)
        
        return(cfg)



def addMouse(cfg):
        #this should always be done?        
        cfg['psyMouse'] = event.Mouse(visible = False, newPos = None, win = cfg['win'])

#        try:
#        # the X coordinate is scaled, so that on the 16:10 widescreen monitor, and square tablet, reaches are still proportional
#        # factor: 1.6
#        # both X and Y are then scaled up, so that the movement in centimeters is equal on the screen and tablet
#        # factor: 1.05
#        # we try to use one of the X11 ways to create a mouse object, because the time sampling is more accurate
#        #there are two ways to do this, the first one is simpler and works in Python 3.X
#        
#                try:
#                        class myMouse:
#                        #from Xlib import display - this needs to be on top
#
#                                def Pos(self):
#                                        #print('A X11 mouse')
#                                        #print(cfg.keys())                                        
#                                        qp = display.Display().screen().root.query_pointer()
#                                        return [(qp.root_x - (cfg['winSize'][0]/2))/(1.6*1.1), -1* (qp.root_y - (cfg['winSize'][1]/2)/1.1), time.time()] #winSize instead of width and height
#
#                except:
#                        #if the simpler method does not work, we try the harder one:
#                        class myMouse:
#                                Xlib = CDLL("libX11.so.6")
#                                display = Xlib.XOpenDisplay(None)
#                                if display == 0: sys.exit(2) # no display or can't be accessed...
#                                w = Xlib.XRootWindow(display, c_int(0)) #c_int(cfg['monitorIndex']-1)); but monitorIndex does not exist
#                                (root_id, child_id) = (c_uint32(), c_uint32())
#                                (root_x, root_y, win_x, win_y) = (c_int(), c_int(), c_int(), c_int())
#                                mask = c_uint()
#                              
#                                def Pos(self):
#                                        #print('X11 mouse')
#                                        ret = self.Xlib.XQueryPointer(self.display, c_uint32(self.w), byref(self.root_id), byref(self.child_id), byref(self.root_x), byref(self.root_y), byref(self.win_x), byref(self.win_y), byref(self.mask))
#                                        if ret == 0: sys.exit(1)
#                                        return [(self.root_x.value - (cfg['winSize']/2))/(1.6*1.1), -1 * (self.root_y.value - (cfg['winSize']/2))/1.1, time.time()] # c_int can't be used by regular Python to do math, but the values of c_ints are ints - also, we return the current time
#          
#        except:
         #print('PsychoPy mouse')
         # if the X11 methods don't seem to work, create an identically named object with identical behaviour based on PsychoPy
        class myMouse:
                  
                def Pos(self):
                    #print('PsychoPy mouse')
                    [X,Y] = cfg['psyMouse'].getPos()
                    #return [X/(1.6/1.1),Y*1.1,time.time()]
                    kX = 0.5*cfg['radius']
                    kY = 2*cfg['radius']
                    return [(X/(1.6/1.05))-kX,(Y*0.98)-kY,time.time()] #0.95 is really close too for Y
                    #kX and kY are offsets to recenter the circle in tablet (10 cm radius)
                    #1.6 is a factor that makes the square for the monitor (tablet is square, monitor is not)
                    #1.05 and 0.98 are trial and error factors that help to fit the stencil appropriately
          
        cfg['mouse'] = myMouse()
        
        return(cfg)


def closeEnvironment(cfg):
        
        cfg['psyMouse'].setVisible(True)
        cfg['win'].close() 
        return(cfg)

def makeStimuli(cfg):
        
        radius = 0.5*cfg['pixpercm'] #0.0375*2/3*cfg['winSize'][1]
        lineWidth = 4
        cfg['radius'] = radius

        # add home position (grey, darker than background (which is roughly 128,128,128))
        cfg['home'] = visual.Circle(win = cfg['win'], pos = [0,0], radius = radius, lineWidth = lineWidth)
        #cfg['home'].setFillColor(color=(64,64,64), colorSpace = 'rgb255')
        cfg['home'].setLineColor(color=(64,64,64), colorSpace = 'rgb255')

        # add home fail feedback (red)
        cfg['homefail'] = visual.Circle(win = cfg['win'], pos = [0,0], radius = radius, lineWidth = lineWidth)
        #cfg['homefail'].setFillColor(color=(64,64,64), colorSpace = 'rgb255')
        cfg['homefail'].setLineColor(color=(255,0,0), colorSpace = 'rgb255')

        # add home success feedback (blue)
        cfg['homesuccess'] = visual.Circle(win = cfg['win'], pos = [0,0], radius = radius, lineWidth = lineWidth)
        #cfg['homesuccess'].setFillColor(color=(64,64,64), colorSpace = 'rgb255')
        cfg['homesuccess'].setLineColor(color=(0,0,255), colorSpace = 'rgb255')

        # add target stay (grey, darker than bg)
        cfg['targetstay'] = visual.Circle(win = cfg['win'], pos = [0,-0.25*cfg['winSize'][1]], radius = radius, lineWidth = lineWidth)
        #cfg['targetstay'].setFillColor(color=(64,64,64), colorSpace = 'rgb255')
        cfg['targetstay'].setLineColor(color=(64,64,64), colorSpace = 'rgb255')
        
        # add target go (dark blue)
        cfg['targetgo'] = visual.Circle(win = cfg['win'], pos = [0,-0.25*cfg['winSize'][1]], radius = radius, lineWidth = lineWidth)
        #cfg['targetgo'].setFillColor(color=(64,64,64), colorSpace = 'rgb255')
        cfg['targetgo'].setLineColor(color=(0,0,255), colorSpace = 'rgb255')

        #would want the targetdistance to be set, maybe 40% of distance from home to edge of window
        if (cfg['winSize'][1] == 1050):
                cfg['targetdistance'] = cfg['pixpercm'] * 9 #12 because stencil is 10
        else:
                cfg['targetdistance'] = cfg['pixpercm'] * 5 #5
        
        # add cursor (white)
        cfg['cursor'] = visual.Circle(win = cfg['win'], pos = [0,-0.25*cfg['winSize'][1]], radius = radius, lineWidth = lineWidth)
        #cfg['cursor'].setFillColor(color=(255,255,255), colorSpace = 'rgb255')
        cfg['cursor'].setLineColor(color=(255,255,255), colorSpace = 'rgb255')



        #most likely will not need sound
        #add Beep sound
        sound.init(rate=44100, stereo=True, buffer=128)
        cfg['sound'] = sound.Sound('ding.wav', secs = 1)
        
        return(cfg)


def makeBlocks(cfg):

        # empty list with blocks to do:
        blocks = []

        #set properties of blocks:
        tasks = ['aligned']
        
        rot_axis = [0,0,0,0,90,90,90,90]
        rot_dir = [1,1,-1,-1,1,1,-1,-1]
        rot_place = [1,2,1,2,1,2,1,2]

        #conditions = pd.DataFrame({'rot_axis' : rot_axis, 'rot_dir' : rot_dir, 'rot_place' : rot_place})

        #index by participant number
        orderIndex = cfg['id'] % 8 #0,1,2,3,4,5,6,7; 8
        
        pp_rot_axis = rot_axis[orderIndex]
        pp_rot_dir = rot_dir[orderIndex]
        pp_rot_place = rot_place[orderIndex]
        
        #ppcond = conditions.iloc(orderIndex) #replaced irow with iloc get a row from df depending on index
        #print(conditions)
        #all tasks are fixed but we need to counterbalance between rot and mir
        #orders = {'1':['rotation','mirror'], '2':['mirror','rotation']}

        #pptaskorder = orders['%d'%pp_rot_place]

        

        #pptaskorder[0] #0 or 1 for value here

        #generate instructions before each task
        instruct = ['reach with cursor']
        
        trialTargets = sp.array([7.5,15,22.5,187.5,195,202.5])
        
        taskBlockNos = [2] #e.g. 8 times 6 trials in a block
        # tasks with 0 blocks actually are resting state tasks (i.e. no blocks is correct)
        
        taskRotations = [0]
        
        for taskno in range(len(tasks)):

                task = tasks[taskno]
                
                blockDef = {}
                blockDef['instruction'] = instruct[taskno]

                target = []
                rotation = []
                perturbation = []
                axis = []


                if task == 'aligned':
                        targets = list(sp.array(trialTargets) + 0) + list((sp.array(trialTargets) - 30) + 90) #+ list(sp.array(trialTargets) + 135)
                        #print(target)
                        target = []

                        if pp_rot_axis == 90:
                                targets = list((sp.array(targets) + 90) % 360) #modulo: so that after 360 deg, it returns to 0
                                        
                        if pp_rot_dir == -1:
                                targets = list((sp.array(targets) + 90) % 360)
                        trialSets = taskBlockNos[taskno]
                        for trialSet in range(taskBlockNos[taskno]):
                                trialindices = list(sp.arange(len(targets)))#array range
                                shuffle(trialindices)
                                #print(trialindices)
                                #then we are creating lists and putting them on a df later
                                target = target + list(sp.array(targets)[trialindices])
                        rotation = [0] * len(target)
                        perturbation = ['none'] * len(target)
                        axis = [float('NaN')] * len(target)
                                

                                
                #convert block lists to df:
                #print(len(rotation))
                #print(len(target))
                #print(len(perturbation))
                trialDefs = pd.DataFrame({'target':target, 'rotation':rotation, 'perturbation':perturbation, 'axis':axis})
                blockDef['trials'] = trialDefs
                blockDef['tasktype'] = task
                #add it to the list of block definitions:
                blocks.append(blockDef)

        cfg['blocks'] = blocks
        return(cfg)


runExp()
#cProfile.run('runExp()')
