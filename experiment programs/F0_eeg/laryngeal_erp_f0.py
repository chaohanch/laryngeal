 
#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This experiment was created using PsychoPy3 Experiment Builder (v3.0.0b10),
    on October 19, 2018, at 11:15
If you publish work using this script please cite the PsychoPy publications:
    Peirce, JW (2007) PsychoPy - Psychophysics software in Python.
        Journal of Neuroscience Methods, 162(1-2), 8-13.
    Peirce, JW (2009) Generating stimuli for neuroscience using PsychoPy.
        Frontiers in Neuroinformatics, 2:10. doi: 10.3389/neuro.11.010.2008
"""

from __future__ import division  # so that 1/3=0.333 instead of 1/3=0
from psychopy import visual, core, data, event, logging, sound, gui, parallel, prefs
from psychopy.constants import *  # things like STARTED, FINISHED
import numpy as np  # whole numpy lib is available, prepend 'np.'
from numpy import sin, cos, tan, log, log10, pi, average, sqrt, std, deg2rad, rad2deg, linspace, asarray
from numpy.random import random, randint, normal, shuffle
import random
import os  # handy system and path functions
###### PSYCHOPY STUFF ##################

print('Using %s (with %s) for sounds' % (sound.audioLib, sound.audioDriver))

# Ensure that relative paths start from the same directory as this script
_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)

# Store info about the experiment session
expName = 'F0'  # from the Builder filename that created this script
# expInfo = {'participant':'', 'first language':'','session':'001', 'P_port':'T', 'group (a/r)' : ''}
expInfo = {'Experiment': expName, 'participant':'0001', 'session':'01', 'P_port':'T'}
dlg = gui.DlgFromDict(dictionary=expInfo, title=expName)
if dlg.OK == False: core.quit()  # user pressed cancel
expInfo['date'] = data.getDateStr()  # add a simple timestamp
expInfo['expName'] = expName
pp = expInfo['P_port']
# group_name = expInfo['group (a/r)']
#
# if group_name == 'a':
#     tn = 5
# else:
#     tn = 4

# tn = 4

# Data file name stem = absolute path + name; later add .psyexp, .csv, .log, etc
filename = _thisDir + os.sep + 'data/%s_%s_%s_%s' %('CUE2024', expInfo['participant'], expName, expInfo['date'])

# An ExperimentHandler isn't essential but helps with data saving
thisExp = data.ExperimentHandler(name=expName, version='',
    extraInfo=expInfo, runtimeInfo=None,
    originPath=None,
    savePickle=True, saveWideText=True,
    dataFileName=filename)
#save a log file for detail verbose info
logFile = logging.LogFile(filename+'.log', level=logging.EXP)
logging.console.setLevel(logging.WARNING)  # this outputs to the screen, not a file
logging.exp('Sound library is {}'.format(sound.Sound))

endExpNow = False  # flag for 'escape' or other condition => quit the exp



#### CREATE STIMULUS LISTS #####################

stimdir = "stimuli/"

# Set range of number of standards before each deviant
interdeviant_min = 5
interdeviant_max = 11


# Set ISI range (in seconds)
isi_min = 0.7
isi_max = 0.95

# Set number of "repetitions" (Number of times listener will hear each deviant)
num_reps = 70


triggers = {}
trigger_codes = [x.strip() for x in open('trigger_codes.txt','r')]
for line in trigger_codes:
    parts = line.split('\t')
    triggers[parts[0]] = parts[1]

# For each "repetition"
# Create a "sequence" for each standard.
# 1. Sample a random number of standards (number chosen from the interdeviant range)
# 2. Sample a random ISI which will correspond to each standard
# 3. Include the deviant and random ISI
# Create a Conditions list from this information

blocks = []
# block_codes = [x.strip() for x in open('blocks_'+group_name+'.txt','r')]
block_codes = [x.strip() for x in open('blocks.txt','r')]
for line in block_codes:
    blocks.append(line.split())

# get the participant number info to determine block order
participant = int(expInfo['participant'])
if participant % 4 == 0:
    order = [0,1,2,3]
elif participant % 4 == 1:
    order = [1,0,3,2]
elif participant % 4 == 2:
    order = [2,3,0,1]
elif participant % 4 == 3:
    order = [3,2,1,0]


# intialize a list for token related info
conditions = []

logging.exp(msg='Block order is %s' % [a[0] for a in blocks])
for i in range(len(blocks)):
    block_name = blocks[order[i]][0]
    standards = blocks[order[i]][1].split(',')
    deviants_a = blocks[order[i]][2].split(',')
    deviants= []
    for j in range(num_reps):
        deviants.append(deviants_a[random.randint(0,len(deviants_a)-1)])
    random.shuffle(deviants)

    for deviant in deviants:
        num_standards = random.choice(range(interdeviant_min, interdeviant_max+1))
        for k in range(num_standards):
            standard = standards[random.randint(0,len(standards)-1)]
            conditions.append({
                'stim':standard,
                'cat':'standard',
                'isi':random.uniform(isi_min,isi_max)-0.1,
                'trigger':triggers[standard],
                'block':block_name})
        conditions.append({
            'stim':deviant,
            'cat':'deviant',
            'isi':random.uniform(isi_min,isi_max)-0.1,
            'trigger':int(triggers[deviant])+100,
            'block':block_name})



################################################


# Start Code - component code to be run before the window creation

# Setup the Window
win = visual.Window(size=[1920, 1080], monitor='testMonitor', color = -1, fullscr=True)
win.mouseVisible=False
message = visual.TextStim(win, pos=(0, 0.3), wrapWidth=1.3, height=0.1, color=1)
message2 = visual.TextStim(win, pos=(0, -0.3), wrapWidth=1.3, height=0.1, color=1)
message.setAutoDraw(True)
message2.setAutoDraw(True)
message.setText("Please wait for the experimenter to begin the experiment.")
message2.setText("")
win.flip()
event.waitKeys()
message.setText("")
win.flip()

# store frame rate of monitor if we can measure it successfully
expInfo['frameRate']=win.getActualFrameRate()
if expInfo['frameRate']!=None:
    frameDur = 1.0/round(expInfo['frameRate'])
else:
    frameDur = 1.0/60.0 # couldn't get a reliable measure so guess

# Initialize components for Routine "trial"
trialClock = core.Clock()
ISI = core.StaticPeriod(win=win, screenHz=expInfo['frameRate'], name='ISI')
if pp != 'F':
    p_port = parallel.ParallelPort(address=u'0x3FB8')
aud_stim = sound.Sound(stimdir+'f0_dorsal_high_132_5.wav', secs=-1)
aud_stim.setVolume(1)

# Create some handy timers
globalClock = core.Clock()  # to track the time since experiment started

# set up handler to look after randomisation of conditions etc
trials = data.TrialHandler(nReps=1, method='sequential',
    extraInfo=expInfo, originPath=None,
    trialList=conditions,
    seed=None, name='trials')
thisExp.addLoop(trials)  # add the loop to the experiment
thisTrial = trials.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb=thisTrial.rgb)
if thisTrial != None:
    for paramName in thisTrial.keys():
        exec(paramName + '= thisTrial.' + paramName)

deviant_num = 0
for thisTrial in trials:
    currentLoop = trials
    # abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
    if thisTrial != None:
        for paramName in thisTrial.keys():
            exec(paramName + '= thisTrial.' + paramName)

    #------Prepare to start Routine "trial"-------
    t = 0
    trialClock.reset()  # clock
    frameN = -1
    # update component parameters for each repeat
    aud_stim.setSound(stimdir+stim+'.wav')
    aud_stim.setVolume(0.8)
    stim_dur = aud_stim.getDuration()
    # keep track of which components have finished
    trialComponents = []
    trialComponents.append(ISI)
    if pp != "F":
        trialComponents.append(p_port)
    trialComponents.append(aud_stim)
    for thisComponent in trialComponents:
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED


    #-------Start Routine "trial"-------
    continueRoutine = True
    while continueRoutine:
        # get current time
        t = trialClock.getTime()
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        # start/stop aud_stim
        if t >= isi and aud_stim.status == NOT_STARTED:
            # this part has been changed - otherwise use above.
            if pp != "F":
                p_port.status=STARTED
                win.callOnFlip(p_port.setData, int(trigger))
            # keep track of start time/frame for later
            aud_stim.tStart = t  # underestimates by a little under one frame
            aud_stim.frameNStart = frameN  # exact frame index
            win.callOnFlip(aud_stim.play)  # start the sound (it finishes automatically)

        if t >= (isi+stim_dur+0.1): #most of one frame period left
            aud_stim.stop()  # stop the sound (if longer than duration)
            # changed
            if pp != "F":
                p_port.status = STOPPED
                p_port.setData(int(0))
            continueRoutine=False
        # *ISI* period
        if t >= 0.0 and ISI.status == NOT_STARTED:
            # keep track of start time/frame for later
            ISI.tStart = t  # underestimates by a little under one frame
            ISI.frameNStart = frameN  # exact frame index
            ISI.start(isi)
        elif ISI.status == STARTED: #one frame should pass before updating params and completing
            ISI.complete() #finish the static period



        # check for quit (the Esc key)
        if endExpNow or event.getKeys(keyList=["escape"]):
            core.quit()
        # refresh the screen
        win.flip()

    #-------Ending Routine "trial"-------
    # check if it's break time!
    if event.getKeys(keyList=["1","2","3","4"]):
            message.setText("Paused. Press SPACE to continue.")
            win.flip()
            if pp != 'F':
                p_port.setData(int(222))
            event.waitKeys(keyList=["space"])
            message.setText('')
            if pp != 'F':
                p_port.setData(int(223))

    if cat == 'deviant':
        deviant_num += 1
        if deviant_num % (num_reps) == 0 and deviant_num < (num_reps) * len(blocks):
            message.setText("BREAK!")
            message2.setText("Please wait for the experimenter.")
            win.flip()
            event.waitKeys(keyList=["space"])
            message.setText("")
            message2.setText("")
            win.flip()
    thisExp.nextEntry()

message.setText("Thank you!")
message2.setText("That's the end of the experiment. Please wait for the experimenter.")
win.flip()
event.waitKeys()
message.setText("")
message2.setText("")
win.flip()

win.close()
core.quit()
