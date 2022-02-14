from psychopy import visual, core, gui, event
import numpy as np
import csv
import os.path
import random

random.seed() # random seed based on clock

winWidth = 1440; winHeight = 810
win = visual.Window(
    size=[winWidth, winHeight],
    units="pix",
    fullscr=False,
    color=[0.5,0.5,0.5])
    
# GUI for experiment setup and subject details
#setupGUI = gui.Dlg(title="GLB01 Experiment")
#setupGUI.addText('Experiment Setup')
#setupGUI.addField('Participant Number:')
#setupGUI.addField('Show probabilities?:', choices=["Yes", "No"])
#setupGUI.addField('High Box cost:', '10')
#setupGUI.addField('Low Box cost:', '3')
#setupGUI.addField('Number of trials', '1')
#setupGUI.addText(' ') # blank line
#setupGUI.addText('Participant details')
#setupGUI.addField('Age:')
#setupGUI.addField('Gender', choices=["Male", "Female", "Other"])
#language = setupGUI.addField('English first language?', choices=["Yes", "No"])
#setup_data = setupGUI.show()  # show dialog and wait for OK or Cancel
#if setupGUI.OK:  # or if ok_data is not None
#    subNum = setup_data[0]
#    dataFile = "DATA\data_S" + str(subNum) + ".csv"
#    if os.path.exists(dataFile):
#        # file exists
#        print('That file already exists')
#        core.quit()
#else:
#    print('Setup cancelled')
#    core.quit()

# design of experiment
# cue 1 | cue 2 | cue_correct (L/R) | instruction (C/S)

dataFile = "DATA\data_temp.csv" # temp data filename

# STAGE 1 DESIGN
stg1Design = np.array([\
[1,3,1,1],[2,3,1,1],[1,4,1,1],[2,4,1,1],\
[3,1,2,1],[3,2,2,1],[4,1,2,1],[4,2,2,1]], dtype = int)

stg1blocks = 1
trialArr = np.empty([0,5], dtype = int)
for b in range(0,stg1blocks):
    np.random.shuffle(stg1Design) # randomise the trial order
    newBlock = np.append(stg1Design,np.ones((8,1),dtype=int)*(b+1), axis = 1) # add block numbers
    trialArr = np.append(trialArr, newBlock, axis = 0) 



# STAGE 2 DESIGN
stg2Design = np.array([\
[1,3,1,1],[2,3,1,1],[1,4,1,1],[2,4,1,1],\
[1,3,2,2],[2,3,2,2],[1,4,2,2],[2,4,2,2],
[3,1,2,1],[3,2,2,1],[4,1,2,1],[4,2,2,1],
[3,1,1,2],[3,2,1,2],[4,1,1,2],[4,2,1,2]], dtype = int)

stg2blocks = 1
#trialArr = np.empty([0,5], dtype = int)
for b in range(0,stg2blocks):
    np.random.shuffle(stg2Design) # randomise the trial order
    newBlock = np.append(stg2Design,np.ones((16,1),dtype=int)*(b+1), axis = 1) # add block numbers
    trialArr = np.append(trialArr, newBlock, axis = 0) 

stg2instAt = stg1blocks*8

print(trialArr)

stimCols = np.array([[255,0,0],[0,255,0],[0,0,255],[255,255,0]])
stimCols = np.insert(stimCols,0,0,axis=0)

# create the stimuli
fixation = visual.Circle(\
win=win, pos=[0,0], radius = 10, edges = 128, fillColorSpace = 'rgb255', fillColor = [255,255,255], lineWidth = 0)

instCircle = visual.Circle(\
win=win, pos=[0,0], radius = 40, edges = 128, fillColorSpace = 'rgb255', fillColor = [120,120,120], lineWidth = 0)

instSquare = visual.Rect(\
win=win, pos=[0,0], size = 80, fillColorSpace = 'rgb255', fillColor = [120,120,120], lineWidth = 0)

L_cue = visual.Circle(\
win=win, pos=[-400,0], radius = 100, edges = 128, fillColorSpace = 'rgb255', fillColor = [0,0,0], lineWidth = 0)

R_cue = visual.Circle(\
win=win, pos=[400,0], radius = 100, edges = 128, fillColorSpace = 'rgb255', fillColor = [0,0,0], lineWidth = 0)

R_text_background = visual.Rect(\
win=win, pos=[400,0], size = 40, fillColorSpace = 'rgb255', fillColor = [255,255,255])

L_text_background = visual.Rect(\
win=win, pos=[-400,0], size = 40, fillColorSpace = 'rgb255', fillColor = [255,255,255])

R_text = visual.TextStim(\
win=win, pos=[400,0], colorSpace = 'rgb255', color = (220,220,220))

L_text = visual.TextStim(\
win=win, pos=[-400,0], colorSpace = 'rgb255', color = (220,220,220))

letter_keys = np.array(['A', 'Z', 'K', 'M']).astype('U') # response prompts/keys 

fb_text = visual.TextStim(\
win=win, pos=[0,0], colorSpace = 'rgb255', color = (0,0,0))

for t in range(0,trialArr.shape[0]):
    
    np.random.shuffle(letter_keys) # shuffle the response keys
    
    if t == stg2instAt:
        instText = visual.TextStim(win=win, pos=[0,0], colorSpace = 'rgb255', color = (0,0,0))
        instText.text = "Instructions for Stage 2 here"
        instText.draw()
        win.flip()
        core.wait(3)
    
    # draw fixation
    fixation.draw()
    win.flip()
    core.wait(1)
    
    # draw instruction + fixation
    if trialArr[t][3] == 1: # circle instruction
        instCircle.draw()
    elif trialArr[t][3] == 2:
        instSquare.draw() 
    fixation.draw() # overlay fixation stimulus
    win.flip()
    core.wait(1)
    
    if trialArr[t][2] == 1: # if left cue is P
        corResp = letter_keys[0] 
    elif trialArr[t][2] == 2: # if right cue is P
        corResp = letter_keys[1] 
    
    # draw left stimulus
    L_cue.fillColor = stimCols[trialArr[t][0]] # get colour from trial structure
    L_cue.draw()
    L_text_background.draw()
    L_text.text = letter_keys[0]
    L_text.draw()
    # draw right stimulus
    R_cue.fillColor = stimCols[trialArr[t][1]] # get colour from trial structure
    R_cue.draw()
    R_text_background.draw()
    R_text.text = letter_keys[1]
    R_text.draw()

    win.flip()
#    core.wait(.5) # stimulus duration time
#    win.flip()
#    core.wait(0.2) # ITI
    
    key = event.waitKeys(maxWait=5,keyList=np.char.lower(letter_keys[0:2]))
    if key == np.char.lower(corResp):
        fb_text.text = "Correct"
    else:
        fb_text.text = "Incorrect"
    
    fb_text.draw()
    win.flip()
    core.wait(1)
    
    csvRow = [t , trialArr[t][0:3]] 
    
    with open(dataFile, 'a', newline='') as f:
        wr = csv.writer(f)
        wr.writerow(csvRow)  

win.flip()
core.wait(1)

win.close()