#script makes prediction for DOC based on healthy model
#wirite csv file with prediction
#trnasform 5 class predictions into eyes open / closed pred, save
rm(list=ls())
library(R.matlab) ; library(ggplot2) ; library(cowplot) ; library(caret) ;library(ggthemes)
sbj_initials <- "AR"
path = paste("H:\\DATA\\PE&wSMI\\Data_ENt\\Coma_salzburg\\SlbContinous\\", sbj_initials, sep = "","\\")
#sbj_list <- c("AR", "DH", "JP", "KG", "RF", "SR", "RB", "WL","US")
setwd(path)

y <- glob2rx(paste(sbj_initials, "*_PE_filt.mat$", sep = ""))
z <- glob2rx(paste(sbj_initials, "*Eyes.txt$", sep = ""))
x <- glob2rx(paste(sbj_initials,"*filesStartStop.txt$", sep = ""))
PElist <- dir(path, pattern = y, ignore.case = TRUE)
eyesFile <- dir(path, pattern = z, ignore.case = TRUE)
StartStopdir <- dir(path, pattern = x, ignore.case = TRUE)
#
day <- readMat(paste(path, PElist[2], sep ="")) 
day2 <- readMat(paste(path, PElist[2], sep ="")) 
#day3 <- readMat(paste(path, PElist[2], sep ="")) 
#day5 <- readMat(paste(path, PElist[3], sep ="")) 
#day6 <- readMat(paste(path, PElist[4], sep ="")) 
night <- readMat(paste(path, PElist[1], sep =""))
tauEEG <- 3 

pe.day1 <- t(day$H[[tauEEG]][[1]])
pe.night <- t(night$H[[tauEEG]][[1]])
pe.day2 <- t(day2$H[[tauEEG]][[1]])
#pe.day3 <- t(day3$H[[tauEEG]][[1]])
#pe.day5 <- t(day5$H[[tauEEG]][[1]])
#pe.day6 <- t(day6$H[[tauEEG]][[1]])

change_colnames <- function(x){
  elec_count <- ncol(x)
  if (elec_count == 25 ){
    colnames(x) <- c("F3","F4","FC5","FC6","C3","C4","P3","P4","T3","T4","F7","F8","PO7",
                     "PO8","Fz","Cz","Pz","Oz","FCz","vEOGbi","ECG_bi","EMGbi","EMGlow_bi",
                     "Resp_bi", "hEOG_bi") #EMGup_bi renamed to EMGbi (healthy matching) 
  }
  else if (elec_count == 24){
    colnames(x) <- c("F3","F4","FC5","FC6","C3","C4","P3","P4","T3","T4","F7","F8","PO7",
                     "PO8","Fz","Cz","Pz","Oz","FCz","vEOGbi","ECG_bi","EMGbi","EMGlow_bi",
                     "hEOG_bi") 
  }
  return(x)
}


pe.day1 <- change_colnames(pe.day1)
pe.night <- change_colnames(pe.night)
pe.day2 <- change_colnames(pe.day2)
#pe.day3 <- change_colnames(pe.day3)
#pe.day5 <- change_colnames(pe.day5)
#pe.day6 <- change_colnames(pe.day6)
# load model trained on healthy
load("C:\\Users\\b1016533\\Desktop\\14electrodes\\tau3\\trained_model.RData")
elec_count <- ncol(pe.day1)
ElecHealth <- c("Fp1","Fpz","Fp2","F7","F3","Fz","F4","F8","T3","C3","Cz","C4","T4","T5","P3",
                "Pz","P4","T6","O1","Oz","O2","VEOG","EMG","FCz","EMGbi", "hEOG_bi")#,"EMGbi") #only eeg
if (elec_count == 25){
  ElecDoc <- c("F3","F4","FC5","FC6","C3","C4","P3","P4","T3","T4","F7","F8","PO7",
               "PO8","Fz","Cz","Pz","Oz","FCz","vEOGbi","ECG_bi","EMGbi","EMGlow_bi",
               "Resp_bi", "hEOG_bi")
}else if (elec_count == 24){
  ElecDoc <- c("F3","F4","FC5","FC6","C3","C4","P3","P4","T3","T4","F7","F8","PO7",
               "PO8","Fz","Cz","Pz","Oz","FCz","vEOGbi","ECG_bi","EMGbi","EMGlow_bi",
               "hEOG_bi")
}

pe.day1 <- pe.day1[, ElecDoc[ElecDoc %in% ElecHealth]] # sort electrodes
pe.night <- pe.night[, ElecDoc[ElecDoc %in% ElecHealth]] # sort electrodes
pe.day2 <- pe.day2[, ElecDoc[ElecDoc %in% ElecHealth]]
#pe.day3 <- pe.day3[, ElecDoc[ElecDoc %in% ElecHealth]]
#pe.day5 <- pe.day5[, ElecDoc[ElecDoc %in% ElecHealth]]
#pe.day6 <- pe.day6[, ElecDoc[ElecDoc %in% ElecHealth]]
#pe.day3 <- pe.day3[, ElecDoc[ElecDoc %in% ElecHealth]] # sort electrodes
#make a prediction using healthy model
pred.day1 <- predict(rf.fit, newdata = pe.day1) # predict sleep stage
##recode to plot as typical hypnogram (wake on top) [5 - wake, rem - 4, N1 - 3, N2 - 2, N3 -1]
pred.day1<- as.numeric(dplyr::recode(as.character(as.numeric(pred.day1)), "1"="5", "4"="1", "3"="2", "2"="3", "5"="4"))
pred.night <- predict(rf.fit, newdata = pe.night) 
pred.night <- as.numeric(dplyr::recode(as.character(as.numeric(pred.night)), "1"="5", "4"="1", "3"="2", "2"="3", "5"="4"))
pred.day2 <- predict(rf.fit, newdata = pe.day2) 
pred.day2 <-  as.numeric(dplyr::recode(as.character(as.numeric(pred.day2)), "1"="5", "4"="1", "3"="2", "2"="3", "5"="4"))


#pred.day6 <- predict(rf.fit, newdata = pe.day6) 
#pred.day6 <-  as.numeric(dplyr::recode(as.character(as.numeric(pred.day6)), "1"="5", "4"="1", "3"="2", "2"="3", "5"="4"))




#pred.day3 <- predict(rf.fit, newdata = pe.day3) 
#pred.day3 <-  as.numeric(dplyr::recode(as.character(as.numeric(pred.day3)), "1"="5", "4"="1", "3"="2", "2"="3", "5"="4"))

#save predictions as csv
#write.table(x = pred.day1, file = paste(strsplit(PElist[1],
#            split = "\\.")[[1]][1], ".csv", sep = ""),
#            row.names = FALSE,
#            quote = FALSE,
#            col.names = FALSE)
#write.table(x = pred.day2, file = paste(strsplit(PElist[2],
#            split = "\\.")[[1]][1], ".csv", sep = ""),
#            row.names = FALSE,
#            quote = FALSE,
#            col.names = FALSE)
#write.table(x = pred.night, file = paste(strsplit(PElist[3],
#            split = "\\.")[[1]][1], ".csv", sep = ""),
#            row.names = FALSE,
#            quote = FALSE,
#            col.names = FALSE)

#read eyes open closed, eyesFullVid contains eyes state over the whole video
library(lubridate)
myEyes <- data.frame(t(read.table(eyesFile)))
colnames(myEyes) <- c("start", "end", "eyes")
myEyes$start <-period_to_seconds(hms(myEyes$start))/30 #unit is 30 sec
myEyes$end <- period_to_seconds(hms(myEyes$end))/30
myEyes$duration <- myEyes$end - myEyes$start
videoLenght <- round(tail(myEyes$end,1))# take the last element of end, length of video
eyesFullVid <- data.frame(opclosed = rep(NA,videoLenght)) #init df for eyes
for (i in 1:nrow(myEyes)) {
    start <- myEyes$start[[i]]+1
    end <- myEyes$end[[i]]
    times <- myEyes$duration[i]
    eyesFullVid$opclosed[start:end] <- rep(as.character(myEyes$eyes[[i]]), 
                                           times = times)
}
###############################################################################
#read file info when day1 day2 night1 starts, ends
filesStartStop <- read.table(StartStopdir, header = T)
#find index of analysed subject in the files with eyes  
subject <- which(substr(filesStartStop$sbj, start = 1, stop = 3) == sbj_initials)
#
create_df <- function(prediction){
    deparse(substitute(prediction))
    if (deparse(substitute(prediction)) == "pred.day1"){
        idxStart <- "daystart"
        idxEnd <- "dayend"
    } else if (deparse(substitute(prediction)) == "pred.day2"){
      idxStart <- "day2start"
      idxEnd <- "day2end"
    } else if (deparse(substitute(prediction)) == "pred.day3"){
        idxStart <- "day3start"
        idxEnd <- "day3end"
    } else if (deparse(substitute(prediction)) == "pred.day5"){
      idxStart <- "day5start"
      idxEnd <- "day5end"
    } else if (deparse(substitute(prediction)) == "pred.day6"){
      idxStart <- "day6start"
      idxEnd <- "day6end"
    } else if (deparse(substitute(prediction)) == "pred.night"){
        idxStart <- "nightstart"
        idxEnd <- "nightend"
    }
    time <- seq(1,length(prediction)) # sequence for plotting the prediction
    df_plot <- data.frame(time, myPredict=as.numeric(prediction), trueEyes = rep(NA, length(time)),
                          predEyes = rep(NA, length(time)))
    df_plot$myPredict <- factor(df_plot$myPredict, levels=c(1,2,3,4,5),
                                labels = c("N3","N2","N1","REM","wake"))
    #div by 30 to get not minutes but 30s unit
    eyesGivenVid <- eyesFullVid[(period_to_seconds(hms(filesStartStop[[idxStart]][[subject]]))/30 +1):
                                    (period_to_seconds(hms(filesStartStop[[idxEnd]][[subject]]))/30),]
    length(eyesGivenVid) <- nrow(df_plot) #adjust the length, (usually couple minutes discrepance)                           
    df_plot$trueEyes <- factor(eyesGivenVid)
    return(df_plot)
} #end of function create_df
#
pred.day1.df <- create_df(pred.day1)
pred.day2.df <- create_df(pred.day2)
#pred.day3.df <- create_df(pred.day3)
pred.night.df <- create_df(pred.night)
#pred.day5.df <- create_df(pred.day5)
#pred.day6.df <- create_df(pred.day6)

#
#N2 and N3 predictions are cons. as prediction of eyes closed (df_plot$predEyes)
#recording time based on file name (df_plot$time_record)
oc_predict <- function(df_plot){
  deparse(substitute(df_plot))
  if (deparse(substitute(df_plot)) == "pred.day1.df"){
    dn <- "day1"
  }  
  else if (deparse(substitute(df_plot)) == "pred.day2.df"){
    dn <- "day2"
  }
  else if (deparse(substitute(df_plot)) == "pred.day3.df"){
    dn <- "day3"
  }
  else if (deparse(substitute(df_plot)) == "pred.day5.df"){
    dn <- "day5"
  }
  else if (deparse(substitute(df_plot)) == "pred.day6.df"){
    dn <- "day6"
  } else if (deparse(substitute(df_plot)) == "pred.night.df"){
    dn <- "night"
  }
  predClosed <- with(df_plot, (myPredict == "N2" | myPredict == "N3"))#either N2 or N3 cons as closed
  df_plot$predEyes[which(predClosed)] <- "C"
  #wake predictions are cons. as open prediction
  predOpen<- with(df_plot, myPredict == "wake")
  df_plot$predEyes[which(predOpen)] <- "O"
  df_plot$predEyes <- factor(df_plot$predEyes)
  #add var indicating time of recording
  df_plot$time_record <- rep(dn, nrow(df_plot))
  return(df_plot)
}

#list storing true/pred vectors
OClist <- list(OC.day1 = oc_predict(pred.day1.df),
               OC.day2 = oc_predict(pred.day2.df),
               OC.night = oc_predict(pred.night.df)) 
OClist$combo = with(OClist, rbind(OC.day1,OC.day2, OC.night))
#save, single excell table for single subject                   
write.csv(OClist$combo,file = paste(sbj_initials, "OCpred.csv", sep =""),row.names = F)
###########################################################################################################

#plot eyes open/closed periods and sleep predictions
plot_predictions <- function(df_plot, ggtit) {
  n <- ggplot(df_plot, aes(x=time, myPredict, colour = factor(trueEyes, 
                                                              levels = c("C", "nv", "O"), 
                                                              labels = c("Closed",  "Unscorable","Open"),
                                                              ordered = TRUE), 
                           group =1))+
    geom_point(size =1.5) +
    #geom_line(aes(x = time, hmm), colour = "red", size = 1) +
    scale_colour_manual(values=c("Closed"="#3366CC", "Open"="#FF9900", "O/C"="#66AA00", "Unscorable"="#999999")) +
    geom_jitter(height =0.4) + 
    scale_x_continuous(breaks = seq(min(df_plot$time), max(df_plot$time), by = 60))  + 
    ggtitle(label = ggtit) +
    xlab("Time [0.5 min]") +
    ylab("Healthy-like") +
    labs(color='Eyes State')
  #scale_fill_manual(legend_tit)
  return(n)
} 
#plot
#plot_predictions(pred.day1.df, ggtit = "Day1")
d1 <- plot_predictions(pred.day1.df, ggtit = "Day1")
#d2 <- plot_predictions(pred.day2.df, ggtit = "Day2")
#d3 <- plot_predictions(pred.day3.df, ggtit = "Day3")
#d5 <- plot_predictions(pred.day5.df, ggtit = "Day5")
#d6 <- plot_predictions(pred.day3.df, ggtit = "Day6")
n <- plot_predictions(pred.night.df, ggtit = "Night")
par(mar = c(2,2,2,3))

title <- ggdraw() + draw_label("#3")

my_grid <- plot_grid(d1,n, nrow =2, labels = c('A', 'B'))
my_grid <- plot_grid(title, my_grid, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins

path_plots <- "D:\\oldPC\\SleepClassifPaper\\predictionsSlbPlots\\final_plots_paper\\tau3\\"
setwd(path_plots)
save_name <- paste(sbj_initials, "3.pdf", sep ="")
pdf(file = save_name, width = 12, height = 7)
my_grid
dev.off()