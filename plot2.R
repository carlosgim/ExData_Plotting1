#*******************************************************************************
# 
# Author: Carlos A. Giménez
#
# Context: "Exploratory Data Analysis" - Johns Hopkins University
# 
# Objetive: Our overall goal here is simply to examine how household energy 
# usage varies over a 2-day period in February, 2007. Your task is to 
# reconstruct the following plots below, all of which were constructed using the 
# base plotting system.
#*******************************************************************************

# Load libraries
require(data.table); require(dplyr)

# ------------------------------------------------------------------------------
# Load data
# =========
# References:
# The following descriptions of the 9 variables in the dataset are taken from 
# the UCI web site:

# 1.Date: Date in format dd/mm/yyyy
# 2.Time: time in format hh:mm:ss
# 3.Global_active_power: household global minute-averaged active power 
#   (in kilowatt)
# 4. Global_reactive_power: household global minute-averaged reactive power 
#   (in kilowatt)
# 5. Voltage: minute-averaged voltage (in volt)
# 6. Global_intensity: household global minute-averaged current intensity 
#   (in ampere)
# 7. Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). 
#   It corresponds to the kitchen, containing mainly a dishwasher, an oven and a 
#   microwave (hot plates are not electric but gas powered).
# 8. Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). 
#   It corresponds to the laundry room, containing a washing-machine, a tumble-
#   drier, a refrigerator and a light.
# 9. Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). 
#   It corresponds to an electric water-heater and an air-conditioner.

# Small exploratory sample:
sampleData <- read.table("household_power_consumption.txt", header = TRUE, 
                         nrows = 5, sep =";", stringsAsFactors = FALSE)

classes <- sapply(sampleData, class)

str(sampleData)

# Read big data:
largeData <- read.table("household_power_consumption.txt", header = TRUE, 
                       sep = ";", stringsAsFactors = FALSE,  
                       na.strings = c("?","NA"))

# I don't see the kind of missing value, then I will check with the follow form
# largeData[!complete.cases(largeData),]
# The missinga values has the form "?" and "NA"
# Then I run again.

# The column class change, that mean we have missing values
str(largeData); tbl_df(largeData)

# I convert the Date and Time variables to Date/Time classes in R using the
# strptime() and as.Date() functions.
TidyData <- largeData %>%
	mutate(Date = as.Date(Date, format = "%d/%m/%Y"))

# Aparently mutate don't support strptime :(
print('------------')
print('Convert time')
print('------------')

# Convert TidyData to character then apply strptime finally as.POSIXct
TidyData[,"Time"] <- as.POSIXct(strptime(as.character(TidyData[,"Time"]), 
	format = "%H:%M:%S"), format = "%H:%M:%S")

# check the format
str(TidyData)

# Now largeData is Tidy
# ------------------------------------------------------------------------------

# Processing Time
# ===============

# Select range of days: 2007-02-01 to 2007-02-02
print("Without dplyr *************************")
Datawork <- TidyData[TidyData$Date >= "2007-02-01" & 
	TidyData$Date <= "2007-02-02",]

# Check range of days
str(Datawork); tbl_df(Datawork)

# Extract time
justime <- format(as.POSIXct(Datawork$Time), "%H:%M:%S")

# Merge time and date and create a new variable: "timesmerg"
Datawork <- within(Datawork, {timesmerg = 
   format(as.POSIXct(paste(Datawork$Date, justime)), "%d/%m/%Y %H:%M:%S")})

Datawork$timesmerg <- strptime(Datawork$timesmerg, "%d/%m/%Y %H:%M:%S")
                          
# ------------------------------------------------------------------------------

# Plot
# ====
png(filename = "plot2.png", width = 480, height = 480, units = "px", 
    bg = "white")

with(Datawork, plot(timesmerg, Global_active_power, 
                    ylab = "Global Active Power (kilowatts)", type = "l"))
dev.off()

# ------------------------------------------------------------------------------
# end Script