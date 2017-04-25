#make prediction for DOC based on healthy model
#wirite csv file containing  predictions
#transform 5 class predictions into eyes open / closed pred; N2 and  N3 are considered as eye closed predictionn...
#whereas wake as eyes open
#plot original 5 class predictions and label them by using eyes open information
#files needed: *.mat with PE values, *.txt with eyes open, filesStartStopLiege.txt when given EEG file start w.r.t video time,
#.RData containing model obtained fform healthy
rm(list=ls())
library(R.matlab) ; library(ggplot2) ; library(cowplot) ; library(caret) ;library(ggthemes)
path = "H:\\DATA\\PE&wSMI\\Data_ENt\\Coma_liege\\LiegeContinous\\"
#sbj_list <- c("ABd", "AEs", "ALi", "FGo", "GDa", "GDo", "Gue", "MDe", "MKu", "MLa", "MLo", "MVe", "NVe", "RBo")
setwd(path)
#where d1, n2, n starts/ ends wrt. the eyes open/closed
filesStartStop <- read.table("H:\\DATA\\PE&wSMI\\Data_ENt\\Coma_liege\\eyes_state\\filesStartStopLiege.txt", header = T)
sbj_initials <- "ABd"
#for (sbj_initials in sbj_list) {
  y <- glob2rx(paste(sbj_initials, "*_PE.mat$", sep = ""))
  z <- glob2rx(paste(sbj_initials, "*Eyes.txt$", sep = ""))
  PElist <- dir(path, pattern = y, ignore.case = TRUE)
  eyesFile <- dir(path, pattern = z, ignore.case = TRUE)
  tauEEG <- 3 
  #
  if (length(PElist) < 3){
    day2 <- readMat(paste(path, PElist[1], sep ="")) 
    night <- readMat(paste(path, PElist[2], sep =""))
    pe.day2 <- t(day2$H[[tauEEG]][[1]])
    pe.night <- t(night$H[[tauEEG]][[1]])
  } else if (length(PElist) == 3){
    day <- readMat(paste(path, PElist[1], sep ="")) 
    day2 <- readMat(paste(path, PElist[2], sep ="")) 
    night <- readMat(paste(path, PElist[3], sep =""))
    pe.day1 <- t(day$H[[tauEEG]][[1]])
    pe.night <- t(night$H[[tauEEG]][[1]])
    pe.day2 <- t(day2$H[[tauEEG]][[1]])
  }
  
  change_colnames <- function(x){
      elec_count <- ncol(x)
      if (elec_count == 16 ){
        colnames(x) <- c("Fz","F3","F4","Cz","C3","C4","T3","T4","Pz","P3","P4","Oz","ECG12","FR","EMGbi","hEOG_bi")
      }
       else if (elec_count == 15){
         colnames(x) <- c("Fz","F3","F4","Cz","C3","C4","T3","T4","Pz","P3","P4","Oz","ECG12","EMGbi","hEOG_bi")
       }
      
      return(x)
      }
  # load model trained on healthy
  load("C:\\Users\\b1016533\\Desktop\\14electrodes\\tau3\\trained_model.RData")
  elec_count <- ncol(pe.day2)
  ElecHealth <- c("Fp1","Fpz","Fp2","F7","F3","Fz","F4","F8","T3","C3","Cz","C4","T4","T5","P3",
                  "Pz","P4","T6","O1","Oz","O2","VEOG","EMG","FCz","EMGbi", "hEOG_bi")
  if (elec_count == 16){
    ElecDoc <- c("Fz","F3","F4","Cz","C3","C4","T3","T4","Pz","P3","P4","Oz","ECG12","FR","EMGbi","hEOG_bi")
  }else if (elec_count == 15){
    ElecDoc <- c("Fz","F3","F4","Cz","C3","C4","T3","T4","Pz","P3","P4","Oz","ECG12","EMGbi","hEOG_bi")
  }
  
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
  
  
  if (exists("pe.day1")){
    pe.day1 <- change_colnames(pe.day1)
    pe.day1 <- pe.day1[, ElecDoc[ElecDoc %in% ElecHealth]] # sort electrodes
    pred.day1 <- predict(rf.fit, newdata = pe.day1)  #make a prediction using a healthy model
    ##recode to plot as typical hypnogram (wake on top) [5 - wake, rem - 4, N1 - 3, N2 - 2, N3 -1]
    pred.day1<- as.numeric(dplyr::recode(as.character(as.numeric(pred.day1)), "1"="5", "4"="1", "3"="2", "2"="3", "5"="4"))
    pred.day1.df <- create_df(pred.day1)
    
    pe.night <- change_colnames(pe.night)
    pe.night <- pe.night[, ElecDoc[ElecDoc %in% ElecHealth]] # sort electrodes
    pred.night <- predict(rf.fit, newdata = pe.night) 
    pred.night <- as.numeric(dplyr::recode(as.character(as.numeric(pred.night)), "1"="5", "4"="1", "3"="2", "2"="3", "5"="4"))
    pred.night.df <- create_df(pred.night)
    
    pe.day2 <- change_colnames(pe.day2)
    pe.day2 <- pe.day2[, ElecDoc[ElecDoc %in% ElecHealth]] # sort electrodes
    pred.day2 <- predict(rf.fit, newdata = pe.day2) 
    pred.day2 <-  as.numeric(dplyr::recode(as.character(as.numeric(pred.day2)), "1"="5", "4"="1", "3"="2", "2"="3", "5"="4"))
    pred.day2.df <- create_df(pred.day2)
  } else{
    pe.night <- change_colnames(pe.night)
    pe.night <- pe.night[, ElecDoc[ElecDoc %in% ElecHealth]] # sort electrodes
    pred.night <- predict(rf.fit, newdata = pe.night) 
    pred.night <- as.numeric(dplyr::recode(as.character(as.numeric(pred.night)), "1"="5", "4"="1", "3"="2", "2"="3", "5"="4"))
    pred.night.df <- create_df(pred.night)
    
    pe.day2 <- change_colnames(pe.day2)
    pe.day2 <- pe.day2[, ElecDoc[ElecDoc %in% ElecHealth]] # sort electrodes
    pred.day2 <- predict(rf.fit, newdata = pe.day2) 
    pred.day2 <-  as.numeric(dplyr::recode(as.character(as.numeric(pred.day2)), "1"="5", "4"="1", "3"="2", "2"="3", "5"="4"))
    pred.day2.df <- create_df(pred.day2)
    }
    
  # save predictions as csv
  # write.table(x = pred.day1, file = paste(strsplit(PElist[1],
  #             split = "\\.")[[1]][1], ".csv", sep = ""),
  #             row.names = FALSE,
  #             quote = FALSE,
  #             col.names = FALSE)
  # write.table(x = pred.day2, file = paste(strsplit(PElist[2],
  #             split = "\\.")[[1]][1], ".csv", sep = ""),
  #             row.names = FALSE,
  #             quote = FALSE,
  #             col.names = FALSE)
  # write.table(x = pred.night, file = paste(strsplit(PElist[3],
  #             split = "\\.")[[1]][1], ".csv", sep = ""),
  #             row.names = FALSE,
  #             quote = FALSE,
  #             col.names = FALSE)
  
  
  #N2 and N3 predictions are cons. as prediction of eyes closed (df_plot$predEyes)
  #recording time based on file name (df_plot$time_record)
  oc_predict <- function(df_plot){
      deparse(substitute(df_plot))
      if (deparse(substitute(df_plot)) == "pred.day1.df"){
        dn <- "day1"
      }  
      else if (deparse(substitute(df_plot)) == "pred.day2.df"){
        dn <- "day2"
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
  if (exists("pe.day1")){
    OClist <- list(OC.day1 = oc_predict(pred.day1.df),
                   OC.day2 = oc_predict(pred.day2.df),
                   OC.night = oc_predict(pred.night.df)) 
    OClist$combo = with(OClist, rbind(OC.day1,OC.day2,OC.night))
  }else {
    OClist <- list(OC.day2 = oc_predict(pred.day2.df),
                   OC.night = oc_predict(pred.night.df)) 
  OClist$combo = with(OClist, rbind(OC.day2,OC.night))
  }
  
  #save, single excell table for single subject
  #setwd("H:\\DATA\\PE&wSMI\\Data_ENt\\Coma_liege\\LiegeContinous\\subjectsOCpred\\tau3\\")
  #write.csv(OClist$combo,file = paste(sbj_initials, "OCpred.csv", sep =""),row.names = F)
  
  ###########################################################################################################
  #plot eyes open/closed periods and sleep predictions
  plot_predictions <- function(df_plot, ggtit) {
      n <- ggplot(df_plot, aes(x=time, myPredict, colour = trueEyes, group =1))+
          geom_point(size =1.5) +
          #geom_line(aes(x = time, hmm), colour = "red", size = 1) +
          scale_colour_manual(values=c("C"="#3366CC", "O"="#FF9900", "O/C"="#66AA00", "nv"="#999999")) +
          geom_jitter(height =0.4) + 
          scale_x_continuous(breaks = seq(min(df_plot$time), max(df_plot$time), by = 60))  + 
          ggtitle(label = ggtit) +
          xlab("Time [0.5 min]") +
          ylab("Prediction") +
          labs(color='Eyes State') 
        #scale_fill_manual(legend_tit)
      return(n)
  } 
  #plot
  #plot_predictions(pred.day1.df, ggtit = "Day1")
  
  par(mar = c(2,2,2,3))
  if (exists("pe.day1")){
    my_grid <- plot_grid(plot_predictions(pred.day1.df, ggtit = "Day1"), 
                         plot_predictions(pred.night.df, ggtit = "Night"),
                         plot_predictions(pred.day2.df, ggtit = "Day2"),
                         nrow =3, labels = c('A', 'B', 'C'))
  } else{
    my_grid <- plot_grid(plot_predictions(pred.night.df, ggtit = "Night"),
                         plot_predictions(pred.day2.df, ggtit = "Day2"),
                         nrow =3, labels = c('A', 'B'))
  }
  path_plots <- "D:\\oldPC\\SleepClassifPaper\\predictionsLiegePlots\\final_plots_paper\\tau3\\"
  
  setwd(path_plots)
  #add title to the grid plot
  title <- ggdraw() + draw_label("#2")
  my_grid <- plot_grid(title, my_grid, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
  save_name <- paste(sbj_initials, ".pdf", sep ="")
  
  #pdf(file = save_name, width = 15, height = 6)
  #my_grid
  #dev.off()
  
  my_grid 
  
#} #end of subj loop 
  
