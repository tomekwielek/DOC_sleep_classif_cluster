#script for between subject supervised sleep stages classification
#neural networks or random forest
#uses function: prepDataNnet.R for reading and structuring the PE data
#plots results: true hypnogram against the prediction
rm(list=ls())
path = "H:\\DATA\\ANN_EEG\\R_automaticSleepClass\\"
setwd(path)
library(cowplot)
source("2_prepDataNnet.R")
no_sbj <-26
tauEEG <- 3
tauPhysio <- 3
#physioLocations <- c(24:29) #Slory data (used for master thesis)
physioLocations <-c(22:26) ##Insomnia data  
myData <- prepDataNnet(no_sbj, tauEEG, tauPhysio, physioLocations) #custom function to read the data
myPE<- do.call(rbind,myData$PE)# stack PEs accros all subjects
myStag <- do.call(rbind, myData$STAG)
#colnames for Sjesta
#colnames(myPE) <- c("Fp1","Fpz","Fp2","F7","F3","Fz","F4","F8","T3","C3","Cz","C4","T4","T5","P3",
#                       "Pz","P4","T6","O1","Oz","O2","A1","A2","HL","HR","VEOG","EMGbi","ECG","RESP")
#colnames for Insomnia
ElecHealth <- c("Fp1","Fpz","Fp2","F7","F3","Fz","F4","F8","T3","C3","Cz","C4","T4","T5","P3",
  "Pz","P4","T6","O1","Oz","O2","VEOG","EMG","FCz","hEOG_bi","EMGbi")
colnames(myPE) <- ElecHealth
#electrodes of continous Liege data (Abdu)
ElecDoc <- c("Fz","F3","F4","Cz","C3","C4","T3","T4","Pz","P3","P4","Oz","ECG12","FR","EMGbi","hEOG_bi")
#arange cols based on elec from continous Liege data, (its match when testing on the continous LIEGE) 
#myPE <- myPE[,ElecDoc[ElecDoc %in% ElecHealth]] #when commented out use full electrodes set

#myPE <- subset(myPE, select = c(-A1, -A2)) #remove mastoids (Sjesta)
colnames(myStag) <- c("NO","Sleep") #myData$STAG contains ID number ("NO") and staging ("Sleep")

indArrousal <-which(myStag$Sleep == 6) #index arrousal
indUnscorable <- which(myStag$Sleep == 9) #index unscorable(?)

#myStag$Sleep[which(myStag$Sleep == 4)] <- 3 #recode S3 and S4 to N3 (Sjesta)
 
if(length(indArrousal) > 0 || length(indUnscorable) > 0) { #remove arrousal and unscorable if present
    myPE <- myPE[-c(indArrousal,indUnscorable),]
    myStag <- myStag[-c(indArrousal,indUnscorable),]
    }

level <- sort(unique(as.numeric(myStag$Sleep)), na.last = TRUE, decreasing = FALSE)
myStag$Sleep <-factor(myStag$Sleep, levels = level, labels = c("wake", "N1", "N2", "N3", "rem")) #for INSOM 
#myStag$Sleep <-factor(myStag$Sleep, levels = level, labels = c("wake", "S1", "S2", "S3","S4", "rem"))

rm(myData)

library(doParallel) #for parallel processing
library(AppliedPredictiveModeling)
library(caret)
library(e1071)
set.seed(998)
registerDoParallel(cores = 4)

DataStacked <- data.frame(myStag$Sleep, myPE)# standard full set
#DataStacked <- data.frame(myStag$Sleep, subset(myPE, select = c(-A1, -A2, - HL, -ECG, -RESP))) # reduced when testig INSOMNIA
colnames(DataStacked)[1] <- "Stage"  

#DataStacked <- within(DataStacked, rm(EMGbi)) # 
################################################################
# STARTCLASSIFICATION:
# setting for INTERNAL cross validation 

fitControl <- trainControl(
    method = "cv",
    number = no_sbj-1, # number of folds depends on the #sbj
    allowParallel = TRUE,
    verboseIter = TRUE,
    returnData = FALSE,
    returnResamp = "final")
    
NO <- sort(unique(myStag$NO))
rf.predict <- list() #initialize list to store prediction of each external fold (whole sbj)
rf.performance <- list() #initialize list to store accuarcy and kappa of each external fold (whole sbj)
rf.importance <- list()
nn.predict <- list() 
nn.performance <- list()
cm <- list()
f1Score <- list()
trueStage <- list()
#function for f1 calculation
calcF1Scores=function(act,prd){
    #treats the vectors like classes
    #act and prd must be whole numbers
    df=data.frame(act=act,prd=prd);
    scores=list();
    for(i in seq(min(act),max(act))){
        tp=nrow(df[df$prd==i & df$act==i,]);        
        fp=nrow(df[df$prd==i & df$act!=i,]);
        fn=nrow(df[df$prd!=i & df$act==i,]);
        f1=(2*tp)/(2*tp+fp+fn)
        scores[[i]]=f1;
    }      
    print(scores)
    return(scores);
}
# EXTERNAL and nested INTERNAL CV
indexing <- seq(1:nrow(myPE))
for(i in 1:no_sbj) {
   #i <-1
    start <- min(which(myStag$NO == NO[i])) #start for moving window, i is sbj id
    end <- max(which(myStag$NO == NO[i])) #end for moving window, i is sbj id
   
    inTesting <- window(indexing, start = start, end = end)#window includes 1 sbj, varies in each iter,
                                                            #inTestig its an vector index of given sbj
    training <- DataStacked[-as.numeric(inTesting),]
    testing <- DataStacked[as.numeric(inTesting),]
  
    #FIT RANDOM FOREST:
    rf.fit <- train(Stage ~., data = training,
                    method = "rf",
                    tuneGrid = expand.grid(.mtry = c(6,8,10,12,14)), #,
                    n.tree = 1000,
                    trControl = fitControl,
                    verbose = TRUE,
                    metric = "Accuracy",
                    preProcess=c("center", "scale"))
    rf.predict[[i]] <- predict(rf.fit, newdata = subset(testing, select = -Stage))
    cm[[i]]<- confusionMatrix(rf.predict[[i]], testing$Stage)
    rf.performance[[i]] <- cm$overall[1:2] # performance as accuracy and kappa
    #rf.importance[[i]] <- varImp(rf.fit, scale = TRUE)
    #rf.performance.av <-  colMeans(matrix(sapply(rf.performance, unname), 
     #                                 nrow = length(rf.performance), ncol = 2))
    error_estimates <- sapply(cm, '[[', 3) #extract performance over folds(sbjs)
    mean(error_estimates[1,]) ; mean(error_estimates[2,]) # get the mean accuracy and kappa
    
    trueStage[[i]] <- testing$Stage #list the true staging
    f1Score[[i]] <- unlist(calcF1Scores(as.numeric(trueStage[[i]]),as.numeric(rf.predict[[i]])))
    true_vs_pred <- data.frame(true = trueStage[[i]], pred = rf.predict[[i]]) 
    filename <- paste( as.character(i), "trueVSpred", sep = "")
    #save prediction and true
    write.table(true_vs_pred, file = paste(filename, ".txt", sep = ""), row.names = F, quote = F)
}   
###############################################################################
#SVM and other models, compare

# FIT linear SVM: 

svm.fit <- train(Stage ~., data = training,
                method ='svmLinear',
                preProcess = 'range',
                trControl = fitControl,
                tuneGrid = expand.grid(.C = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)))
svm.predict <- predict(svm.fit, newdata = subset(testing, select = -Stage))
svm.confusionMatrix <- confusionMatrix(svm.predict, testing$Stage)
#svm.importance <- varImp(svm.fit, scale = TRUE)
#plot(rf.importance, main = 'Feature importance for Random Forest')

# FIT rbf SVM: 
svmRBF.fit <- train(Stage ~., data = training,
                 method ='svmRadial',
                 preProcess = 'range',
                 trControl = fitControl,
                 tuneGrid = expand.grid(.C = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
                                        .sigma = c(0.001, 0.01, 0.1)))
svmRBF.predict <- predict(svmRBF.fit, newdata = subset(testing, select = -Stage))
svmRBF.confusionMatrix <- confusionMatrix(svmRBF.predict, testing$Stage)
svmRBF.importance <- varImp(svmRBF.fit , scale = TRUE)

# Compare the models
models <- resamples(list(NNet = nn.fit, RF = rf.fit,
                         NB = nb.fit, SVM = svm.fit,
                         SVM_RBF = svmRBF.fit))
                       

dotplot(models)
save(models,file = "models9sbjSlory.RData")
library(gridExtra)
today <- as.character(Sys.Date())

results <- summary(models)
png(paste0(today, '-', 'models-accuracy.jpg'), width = 480, height = 240)
grid.table(results$statistics$Accuracy)
dev.off()
png(paste0(today, '-', 'models-kappa.jpg'), width = 480, height = 240)
grid.table(results$statistics$Kappa)
dev.off()


layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(nn.fit)
plot(rf.fit)
plot(svmRBF.fit)



#plot prediction and true staging
library(dplyr) ; library(ggplot2)
#for(i in 1:no_sbj) {
  i<- 4 #4th gets average F1 = 0.7
  start <- min(which(myStag$NO == NO[i])) #start for moving window, i is sbj id
  end <- max(which(myStag$NO == NO[i])) #end for moving window, i is sbj id
  
  testing$StageInt <- as.numeric(myStag$Sleep[start:end])
  time <- seq(1, length(rf.predict[[i]]))
  df_plot <- data.frame(time, predict=as.numeric(rf.predict[[i]]), true=as.numeric(myStag$Sleep[start:end]))
  #recode to plot as typical hypnogram (wake on top)
  df_plot$predict <- as.numeric(dplyr::recode(as.character(df_plot$predict), "1"="5", "4"="1", "3"="2", "2"="3", "5"="4"))
  df_plot$true <- as.numeric(dplyr::recode(as.character(df_plot$true), "1"="5", "4"="1", "3"="2", "2"="3", "5"="4"))
  #find overlap
  intersectionIndex <- time[df_plot$true == df_plot$predict]#index points where true overlap with pred
  df_plot$overlap <- rep(NA,length(time))
  df_plot$overlap[intersectionIndex] <- df_plot$true[intersectionIndex] #subset based on intersections

  #df_plot$prediction_r <- as.numeric(dplyr::recode(as.character(df_plot$predict), "1"="5", "4"="1", "3"="2", "2"="3", "5"="4"))
  #df_plot$true_r <- as.numeric(dplyr::recode(as.character(df_plot$true), "1"="5", "4"="1", "3"="2", "2"="3", "5"="4"))
  #df_plot$overlap_r <- as.numeric(dplyr::recode(as.character(df_plot$inter), "1"="5", "4"="1", "3"="2", "2"="3", "5"="4"))
  
  colnames(df_plot) <- c("times", "prediction", "true","overlap")
  
  rfPlot <- ggplot(df_plot, aes(time)) + 
    geom_line(aes(y = true, colour = "true") , size = 0.4, alpha = 1) +
    
    geom_line(aes(y = prediction, colour = "prediction"), size = 0.4, alpha = 0.3) +
    geom_point(aes(y = overlap, colour = "overlap"),  size = 0.2, shape = 20) +
    
    scale_y_continuous( labels = c("N3", "N2", "N1", "REM", "Wake")) +
    scale_x_continuous(breaks = seq(0,1000,60)) +
    labs(x = "Time [min]") +
    theme(legend.position="top", legend.title=element_blank(), 
          axis.title.y = element_blank(),
          text = element_text(size=15),
          panel.background = element_rect(fill = NA, colour = NA),
          plot.background = element_rect(colour = NA),
          panel.grid.major = element_line(colour=NA), ##f0f0f0 grey
          panel.grid.minor = element_blank()) 
    
  rfPlot
#}
load("C:\\Users\\b1016533\\Desktop\\26electrodes\\tau3\\trained_model.RData")
##################################################################
# from https://rpubs.com/Koundy/71792
theme_Publication <- function(base_size=14, base_family="TT Times New Roman") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
  + theme(plot.title = element_text(face = "bold",
                                    size = rel(1.2), hjust = 0.5),
          text = element_text(),
          panel.background = element_rect(colour = NA),
          plot.background = element_rect(colour = NA),
          panel.border = element_rect(colour = NA),
          axis.title = element_text(face = "bold",size = rel(1)),
          axis.title.y = element_blank(),
          axis.title.x = element_text(vjust = -0.2),
          axis.text = element_text(), 
          axis.line = element_line(colour="black"),
          axis.ticks = element_line(),
          panel.grid.major = element_line(colour="#f0f0f0"),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = NA),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.key.size= unit(0.2, "cm"),
          legend.margin = unit(0, "cm"),
          legend.title = element_blank(),
          legend.background = element_rect(fill=alpha('black', 0.2)), 
          plot.margin=unit(c(10,5,5,5),"mm"),
          strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
          strip.text = element_text(face="bold")
          
  ))
  
}



rfPlot <- rfPlot 
pdf("C:\\Users\\b1016533\\Desktop\\results_paper17.01.17\\hh3.pdf",width=9,height=3)
rfPlot
dev.off()

