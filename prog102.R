##########################################################################
## Driver: (Daniel Wong-Moon) (danielWM33)                                       ##
## Navigator: (Vaughn White) (Vaughnw31)                                    ##
## Date: (2025-01-23)                                                   ##
##########################################################################

library(marinecs100b)


# Writing a utility function ----------------------------------------------

# P1: How did you extract the temperature and exposure from the hottest day?
# Copy-paste the code here.

hottest_idx <- which.max(kefj_temperature)
# gives the index of the exact time of the highest temperature in data set
hottest_time <- kefj_datetime[hottest_idx]
# gives the exact time of the highest temperature
hottest_site <- kefj_site[hottest_idx]
# gives the site of the highest temperature
hotday_start <- as.POSIXct("2018-07-03 0:00:00", tz = "Etc/GMT+8")
hotday_start
# gives the starting time, of the day, of the highest temperature
hotday_end <- as.POSIXct("2018-07-03 23:59:59", tz = "Etc/GMT+8")
# gives the ending time, of the day, of the highest temperature

hotday_idx <- kefj_site == hottest_site & kefj_datetime >= hotday_start & kefj_datetime <= hotday_end
# gives the index (in TRUE rather than FALSE), of when the hot day is in session
hotday_datetime <- kefj_datetime[hotday_idx]
hotday_datetime
# gives the times of when the hot day is in session
hotday_temperature <- kefj_temperature[hotday_idx]
hotday_temperature
# gives the temp of when the hot day is in session
hotday_exposure <- kefj_exposure[hotday_idx]
hotday_exposure
# gives the exposure of when the hot day is in session


# Plot the data
plot_kefj(hotday_datetime, hotday_temperature, hotday_exposure)


# P2: Fill in the blanks below to write the Alaskan datetime utility function.
Alaskan_DateTime <- function(x) {
  alaskanTime <- as.POSIXct(x, tz = "Etc/GMT+8")
    return(alaskanTime)
}

Alaskan_DateTime("2013-01-27 0:00:00")


# Extracting data ---------------------------------------------------------

# P3: Make a copy of your code from P1 and edit it to plot the temperature and
# exposure for "Aialik" on 2012-06-01

table(kefj_site)
hottest_site <- kefj_site == "Aialik"
# gives the site of the highest temperature
hotday_start <- as.POSIXct("2012-06-01 0:00:00", tz = "Etc/GMT+8")
hotday_start
# gives the starting time, of the day, of the highest temperature
hotday_end <- as.POSIXct("2012-06-01 23:59:59", tz = "Etc/GMT+8")
hotday_end
# gives the ending time, of the day, of the highest temperature

hotday_idx <- kefj_site == "Aialik" & kefj_datetime >= hotday_start & kefj_datetime <= hotday_end
table(hotday_idx)
hotday_idx
# gives the index (in TRUE rather than FALSE), of when the hot day is in session
hotday_datetime <- kefj_datetime[hotday_idx]
hotday_datetime
# gives the times of when the hot day is in session
hotday_temperature <- kefj_temperature[hotday_idx]
hotday_temperature
# gives the temp of when the hot day is in session
hotday_exposure <- kefj_exposure[hotday_idx]
hotday_exposure
# gives the exposure of when the hot day is in session

# Plot the data
plot_kefj(hotday_datetime, hotday_temperature, hotday_exposure)

# P4: Make a copy of your code from P3 and edit it to plot the temperature and
# exposure for "Harris" on 2016-04-05.

table(kefj_site)
hottest_site <- kefj_site == "Aialik"
# gives the site of the highest temperature
hotday_start <- as.POSIXct("2012-06-01 0:00:00", tz = "Etc/GMT+9")
hotday_start
# gives the starting time, of the day, of the highest temperature
hotday_end <- as.POSIXct("2012-06-01 23:59:59", tz = "Etc/GMT+9")
hotday_end
# gives the ending time, of the day, of the highest temperature

hotday_idx <- kefj_site == "Harris" & kefj_datetime >= hotday_start & kefj_datetime <= hotday_end
table(hotday_idx)
hotday_idx
# gives the index (in TRUE rather than FALSE), of when the hot day is in session
hotday_datetime <- kefj_datetime[hotday_idx]
hotday_datetime
# gives the times of when the hot day is in session
hotday_temperature <- kefj_temperature[hotday_idx]
hotday_temperature
# gives the temp of when the hot day is in session
hotday_exposure <- kefj_exposure[hotday_idx]
hotday_exposure
# gives the exposure of when the hot day is in session

# Plot the data
plot_kefj(hotday_datetime, hotday_temperature, hotday_exposure)

# P5: Compare your solutions for P3 and P4 - what variables changed?

#Only variables changed is the site that we choose

# P6: What you would pick for the temperature extraction function and
# parameters' names?

#I would choose the parameter to be "site", extraction function as plotHotDay

# Writing extraction functions --------------------------------------------

# P7: Fill in the blanks in the code below to write your temperature extraction
# function.

tempEXTRACT <- function(inputTimeSTART, inputTimeEND, site) {
  tempEXTRACTSTART_new <- Alaskan_DateTime(inputTimeSTART)
  tempEXTRACTSTART_end <- Alaskan_DateTime(inputTimeEND)
  tempEXTRACT_idx <- kefj_site == site & kefj_datetime >= tempEXTRACTSTART_new & kefj_datetime <= tempEXTRACTSTART_end
  tempEXTRACT_vector <- kefj_temperature[tempEXTRACT_idx]
    return(tempEXTRACT_vector)
}

tempEXTRACT("2013-01-27 0:00:00", "2013-01-27 23:59:59", "Aialik")


# P8: Make a copy of your solution to P7, and edit it to create exposure and
# datetime extraction functions.

exposureEXTRACT <- function(inputTimeSTART, inputTimeEND, site) {
  exposureEXTRACTSTART_new <- Alaskan_DateTime(inputTimeSTART)
  exposureEXTRACTSTART_end <- Alaskan_DateTime(inputTimeEND)
  exposureEXTRACT_idx <- kefj_site == site & kefj_datetime >= exposureEXTRACTSTART_new & kefj_datetime <= exposureEXTRACTSTART_end
  exposureEXTRACT_vector <- kefj_exposure[exposureEXTRACT_idx]
  return(exposureEXTRACT_vector)
}

exposureEXTRACT("2013-01-27 0:00:00", "2013-01-27 23:59:59", "Aialik")

dateTimeEXTRACT <- function(inputTimeSTART, inputTimeEND, site) {
  dateTimeEXTRACTSTART_new <- Alaskan_DateTime(inputTimeSTART)
  dateTimeEXTRACTSTART_end <- Alaskan_DateTime(inputTimeEND)
  dateTimeEXTRACT_idx <- kefj_site == site & kefj_datetime >= dateTimeEXTRACTSTART_new & kefj_datetime <= dateTimeEXTRACTSTART_end
  dateTimeEXTRACT_vector <- kefj_datetime[dateTimeEXTRACT_idx]
  return(dateTimeEXTRACT_vector)
}

dateTimeEXTRACT("2013-01-27 0:00:00", "2013-01-27 23:59:59", "Aialik")

# P9: Export your annotated screenshot as a JPEG called "annotated_function.jpg"
# and add it to your copy of the module repository. (It should be in the same
# folder as this file.)

#done

# P10: Visualize Nuka Pass on July 1, 2018.

nukadate <- dateTimeEXTRACT("2018-07-01 0:00:00", "2018-07-01 23:59:59", "Nuka_Pass")
nukaexposure <- exposureEXTRACT("2018-07-01 0:00:00", "2018-07-01 23:59:59", "Nuka_Pass")
nukatemp <- tempEXTRACT("2018-07-01 0:00:00", "2018-07-01 23:59:59", "Nuka_Pass")
plot_kefj(nukadate, nukatemp, nukaexposure)

# P11: Save a copy of the Nuka Pass plot as "nuka_pass_2018-07-01.png" in this
# repo

#done

# P12: Compare the code you wrote to create the plot in this module to the code
# you wrote in PROG101. Qualitatively, how do they compare? Which one is easier
# to read and why?

# this code is easier to read, the functions to be called is neatly packed in
# functions, and organized.
