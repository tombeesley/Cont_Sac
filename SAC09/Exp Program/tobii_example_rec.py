    1 import time
    2 import tobii_research as tr
    3 
    4 global_gaze_data = None
    5 
    6 
    7 def gaze_data_callback(gaze_data):
    8     global global_gaze_data
    9     global_gaze_data = gaze_data
   10 
   11 
   12 def gaze_data(eyetracker):
   13     global global_gaze_data
   14 
   15     print("Subscribing to gaze data for eye tracker with serial number {0}.".format(eyetracker.serial_number))
   16     eyetracker.subscribe_to(tr.EYETRACKER_GAZE_DATA, gaze_data_callback, as_dictionary=True)
   17 
   18     # Wait while some gaze data is collected.
   19     time.sleep(2)
   20 
   21     eyetracker.unsubscribe_from(tr.EYETRACKER_GAZE_DATA, gaze_data_callback)
   22     print("Unsubscribed from gaze data.")
   23 
   24     print("Last received gaze package:")
   25     print(global_gaze_data)