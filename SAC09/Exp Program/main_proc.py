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
[3,1],[3,2],[4,1],[4,2]])

np.random.shuffle(design) # randomise the trial order

np.insert(

stimCols = np.array([[255,0,0],[0,255,0],[0,0,255],[255,255,0]])


# create the stimuli
fixation = visual.Circle(\
win=win, units = "pix", pos=[0,0], radius = 20, fillColorSpace = 'rgb255', fillColor = [120,120,120])

L_cue = visual.Circle(\
win=win, units = "pix", pos=[-200,0], radius = 100, fillColorSpace = 'rgb255', lineWidth = 0)

R_cue = visual.Circle(\
win=win, units = "pix", pos=[200,0], radius = 100, fillColorSpace = 'rgb255', lineWidth = 0)

# draw fixation
fixation.draw()

# draw left stimulus
L_cue.fillColor = [0,255,0]
L_cue.draw()

# draw right stimulus
#R_cue.fillColor = [0,0,255]
#R_cue.draw()

win.flip()
core.wait(2)

win.close()