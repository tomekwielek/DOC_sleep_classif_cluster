#Main script for Sleep in DOC analysis (automatic sleep classification + clustering)
#Data - DOC (Salzburg, Liege), 
#subfunction(1): averageOverSbj.R for organizing the data, it returns list[2]
#first element is mean for VS, second for MCS
#subfunction(2): hierarchClust.R for hierarchical aglomerative clustering
#generates sampled and averaged matrix over sbjs OR single mean depending on the 
#setting in averageOverSbj.R
library(R.matlab)
library(xlsx)
library(dendextend)
library(colorspace)
library(ggplot2)
library(popbio) #mean.list
rm(list=ls())
source("H:\\DATA\\ANN_EEG\\R_automaticSleepClass\\4_averageOverSbj.R")
path <- "H:\\DATA\\PE&wSMI\\Data_ENt"
setwd(path)

dataSet <- "Salzburg" #specify which dataset to use 

samplesNO <- 50  #specify how many segments to sample
tau <- 4 #specify tau

data <- list() #storage for day and night
time <- list("Day", "Night")
set.seed(111)
if (dataSet == "Salzburg") {
  diagnosisPath <- "H:\\DATA\\PE&wSMI\\Data_ENt\\Coma_salzburg\\SlbContinous\\2.for_hierarchClust\\diag4clust.xlsx"
  #USe Full Set(all subjects irrespct the video)
  #diagnosisPath <- "H:\\DATA\\PE&wSMI\\Data_ENt\\Coma_salzburg\\SlbContinous\\2.for_hierarchClust\\diag4clustFullSet.xlsx"
  
  for (index in seq(1:2)){ #loop over Day and Night
        EEGpath <- paste("H:\\DATA\\PE&wSMI\\Data_ENt\\Coma_salzburg\\SlbContinous\\2.for_hierarchClust",time[[index]],sep="\\")
        #USe Full Set(all subjects irrespct the video)
        #EEGpath <- paste("H:\\DATA\\PE&wSMI\\Data_ENt\\Coma_salzburg\\SlbContinous\\2.for_hierarchClust",paste0(time[[index]],"FullSet"), sep="\\")
        EEGfilesNames <- list.files(EEGpath, pattern = "_PE_filt.mat")
        EEGfilesCount <- length(EEGfilesNames)
        data[[index]] <- averageOverSbj(tauEeg = tau, samplesNO = samplesNO)
    
      }
  
  } else if (dataSet == "Liege") {
    diagnosisPath <- "H:\\DATA\\PE&wSMI\\Data_ENt\\Coma_liege\\LiegeContinous\\2.for_hierarchClust\\diag4clust.xlsx"
    #USe Full Set(all subjects irrespct the video)
    #diagnosisPath <- "H:\\DATA\\PE&wSMI\\Data_ENt\\Coma_liege\\LiegeContinous\\2.for_hierarchClust\\diag4clustFullSet.xlsx"
    

      for (index in seq(1:2)){
        EEGpath = paste("H:\\DATA\\PE&wSMI\\Data_ENt\\Coma_liege\\LiegeContinous\\2.for_hierarchClust",time[[index]],sep="\\")
        #USe Full Set(all subjects irrespct the video)
        #EEGpath <- paste("H:\\DATA\\PE&wSMI\\Data_ENt\\Coma_liege\\LiegeContinous\\2.for_hierarchClust",paste0(time[[index]],"FullSet"), sep="\\")
        EEGfilesNames <- list.files(EEGpath, pattern = "_PE_filt.mat")
        EEGfilesCount <- length(EEGfilesNames)
        data[[index]] <- averageOverSbj(tauEeg = tau,  samplesNO = samplesNO)
      }
}
names(data) <- c("Day", "Night") # 1st elem of data containes Day data, second Night

################################################################################
#aggeragate day, night together into two df: VS and MCS
# VSnight <- cbind(rep(0, times = samplesNO), data$Night$VS)
# VSday <- cbind(rep(1, times = samplesNO), data$Day$VS)
# MCSnight <- cbind(rep(0, times = samplesNO), data$Night$MCS)
# MCSday <- cbind(rep(1, times = samplesNO), data$Day$MCS)
# VS <- rbind(VSnight, VSday)
# MCS <- rbind(MCSnight, MCSday)

#aggeragate day, night together into two df: VS and MCS for means
VSnight <- cbind(time = rep(0, times = length(data$Night$VS[[1]])), data$Night$VS)
VSday <- cbind(time = rep(1, times = length(data$Day$VS[[1]])), data$Day$VS)
MCSnight <- cbind(time = rep(0, times = length(data$Night$MCS[[1]])), data$Night$MCS)
MCSday <- cbind(time = rep(1, times = length(data$Day$MCS[[1]])), data$Day$MCS)
VS <- rbind(VSnight, VSday)
MCS <- rbind(MCSnight, MCSday)

VS <- data.frame(VS) ; colnames(VS)[1] <- "time"
MCS <- data.frame(MCS) ; colnames(MCS)[1] <- "time"
VS$time <- factor(VS$time,  labels = c("night", "day"))
MCS$time <- factor(MCS$time,  labels = c("night", "day"))

savePath <- "H:\\DATA\\ANN_EEG\\R_automaticSleepClass\\results\\"
setwd(savePath)
save(VS, file = paste("VS", dataSet,"4clustering.RData", sep = ""))
save(MCS, file = paste("MCS", dataSet,"4clustering.RData", sep = ""))









