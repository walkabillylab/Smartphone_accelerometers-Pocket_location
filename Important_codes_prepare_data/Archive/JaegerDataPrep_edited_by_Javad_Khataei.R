# Created by Javad Rahimipour Anaraki on 26/02/18 updated 05/03/18
# Ph.D. Candidate
# Department of Computer Science
# Memorial University of Newfoundland
# jra066 [AT] mun [DOT] ca | www.cs.mun.ca/~jra066

#   input: Data from Jaeger
#  output: Interpolated data

rm(list=ls())
#========================Libraries=========================
library(stringr)
library(data.table)
library(kimisc)
library(imputeTS)

#=========================Variables========================
OS <- Sys.info()
if (OS["sysname"] == "Windows") {
  path <-
    "Z:/Research/dfuller/Walkabilly/studies/smarphone_accel/data/Jaeger/"
  intrPath <-
    "Z:/Research/dfuller/Walkabilly/studies/smarphone_accel/data/"
} else {
  path <-
    "/Volumes/hkr-storage/Research/dfuller/Walkabilly/studies/smarphone_accel/data/Jaeger/"
  intrPath <-
    "/Volumes/hkr-storage/Research/dfuller/Walkabilly/studies/smarphone_accel/data/"
}
setwd(path)

#Required user id to be processed
uid <- "148"

#Imputation method
method <- "linear"

#Timezone
timeZone <- "America/St_Johns"
Sys.setenv(TZ = timeZone)


#===================Data files location====================
filenames <-
  list.files(paste(path, uid, sep = ""),
             pattern = "xls",
             full.names = TRUE)

intervals <-
  fread(paste(intrPath, "intervals.csv", sep = ""),
        sep = ",",
        data.table = FALSE)

timing <-
  fread(paste0(path, uid, "/timing.csv"),
        sep = ",",
        data.table = FALSE)


#================Loop over wear locations==================
for (i in 1:length(filenames)) {
  #Reading data
  inData <-
    read.table(
      file = filenames[i],
      header = FALSE,
      skip = 12,
      col.names = c(
        "Time",
        "PHR",
        "V-O2",
        "VO2/kg",
        "V-CO2",
        "V'E",
        "BF",
        "RER",
        "EqO2",
        "Speed",
        "Elev.",
        "MET"
      )
    )
  
  #Filtering data out based on uid and start and end date
  usrInfo <- intervals[intervals[, "userid"] == uid,]
  startDate <- usrInfo[, "start"]
  endDate <- usrInfo[, "end"]
  
  #Convert Jaeger Time to desired record_time
  colnames(inData)[1] <- "record_time"
  inData[, "record_time"] <- as.character(inData[, "record_time"])
  for (j in 1:nrow(inData)) {
    pos <-
      unlist(strsplit(inData[j, "record_time"], "[:]"))
    if (length(pos) > 2) {
      inData[j, "record_time"] <-
        as.character(
          as.POSIXlt(startDate[1], TZ = timeZone) + as.numeric(pos[3]) + as.numeric(pos[2]) *
            60 + as.numeric(pos[1]) * 60 * 60
        )
    } else {
      inData[j, "record_time"] <-
        as.character(as.POSIXlt(startDate[1], TZ = timeZone) + as.numeric(pos[2]) + as.numeric(pos[1]) *
                       60)
    }
  }
  
  #Fill in intervals in second level
  cData <-
    data.frame(seq.POSIXt(
      as.POSIXlt(startDate[1], TZ = timeZone),
      as.POSIXlt(endDate[1], TZ = timeZone),
      by = "sec"
    ))
  
  #Merging in data with second level data
  colnames(cData) <- "record_time"
  cData$record_time <- as.character(cData$record_time)
  lData <- merge(inData, cData, by = "record_time", all.y = TRUE)
  
  #Convert zeros and "-" to NAs and remove all NA columns | convert columns to numeric
  for (j in 2:ncol(lData)) {
    lData[which(lData[, j] == 0), j] <- NA
    lData[which(lData[, j] == "-"), j] <- NA
    lData[, j] <- as.numeric(as.character(lData[, j]))
  }
  
  #Remove columns with no values / save data for calculating MSE
  lData[, which(colSums(is.na(lData)) == nrow(lData))] <- NULL
  
  #Keep original data for plotting
  plotData <- lData
  
  #Set labels
  lData$activity <- "-"
  for (l in 1:nrow(timing)) {
    rows <-
      which((
        format(lData$record_time, format = '%H:%M:%S') >= format(strptime(timing[l, "Start time"], "%Y-%m-%d %I:%M:%S %p"), format = '%H:%M:%S')
      ) &
        (
          format(lData$record_time, format = '%H:%M:%S') <= format(strptime(timing[l, "End time"], "%Y-%m-%d %I:%M:%S %p"), format = '%H:%M:%S')
        ))
    lData[rows, "activity"] <- timing[l, "Task name"]
    
    #Impute data
    lData[rows, ] <-
      na.interpolation(lData[rows, ], option = method)
  }
  
  #Set label for intervals without label and apply imputation
  rows <- which(lData[, "activity"] == "-")
  lData[rows, "activity"] <- "transit"
  lData[rows,] <- na.interpolation(lData[rows,], option = method)
  
  #Plot original / imputed data
  plot(
    1:nrow(lData),
    lData$V.E,
    col = "green",
    xlab = "Time",
    ylab = "V.E"
  )
  par(new = TRUE)
  plot(
    1:nrow(plotData),
    plotData$V.E,
    col = "red",
    xlab = "Time",
    ylab = "V.E"
  )
  
  #Save the results as a CSV file
  fileName <-
    paste(unlist(strsplit(basename(filenames), "[.]"))[1], "_labeled.csv", sep = "")
  write.csv(lData, paste(path, uid, "/", fileName, sep = ""), row.names = FALSE)
}
