from psychopy import visual, core, gui
from psychopy.event import Mouse
import numpy as np
import csv
import os.path

winWidth = 960; winHeight = 540
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

design = np.array([\
[1,3],[2,3],[1,4],[2,4],\
[3,1],[3,2],[4,1],[4,2]], dtype = int)

blocks = 2
trialArr = np.empty([0,3], dtype = int)
for b in range(0,blocks):
    np.random.shuffle(design) # randomise the trial order
    newBlock = np.append(design,np.ones((8,1),dtype=int)*(b+1), axis = 1) # add block numbers
    trialArr = np.append(trialArr, newBlock, axis = 0) 

#np.insert(trialArr,0,[[0]],axis = 0) # add zeros row

print(trialArr)

stimCols = np.array([[255,0,0],[0,255,0],[0,0,255],[255,255,0]])
stimCols = np.insert(stimCols,0,0,axis=0)

# create the stimuli
fixation = visual.Circle(\
win=win, units = "pix", pos=[0,0], radius = 20, fillColorSpace = 'rgb255', fillColor = [120,120,120], lineWidth = 0)

L_cue = visual.Circle(\
win=win, units = "pix", pos=[-200,0], radius = 100, fillColorSpace = 'rgb255', fillColor = [0,0,0], lineWidth = 0)

R_cue = visual.Circle(\
win=win, units = "pix", pos=[200,0], radius = 100, fillColorSpace = 'rgb255', fillColor = [0,0,0], lineWidth = 0)

for t in range(0,trialArr.shape[0]):
    # draw fixation
    fixation.draw()

    # draw left stimulus
    L_cue.fillColor = stimCols[trialArr[t][0]]
    L_cue.draw()
    # draw right stimulus
    R_cue.fillColor = stimCols[trialArr[t][1]]
    R_cue.draw()

    win.flip()
    core.wait(1) # stimulus duration time
    win.flip()
    core.wait(0.2) # ITI

win.flip()
core.wait(1)

win.close()