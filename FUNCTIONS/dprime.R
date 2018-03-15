dprime = function(hits, false.alarms) {
  
  hits_corrected = hits
  hits_corrected[hits_corrected == 1] = 0.99
  hits_corrected[hits_corrected == 0] = 0.01
  
  false.alarms_corrected = false.alarms
  false.alarms_corrected[false.alarms_corrected == 1] = 0.99
  false.alarms_corrected[false.alarms_corrected == 0] = 0.01
  
  qnorm(hits_corrected) - qnorm(false.alarms_corrected)
  
}
