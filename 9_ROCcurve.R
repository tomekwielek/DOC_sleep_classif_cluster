#compute several performance measures for DOC subjects (output where N1 and N2 id considered as eyes closed
#and wake as open) : see performance variable
#Eyes closed is the postive class (see recall and precission: line 17-18)
#plot TPR vs FPR
#function(){
library(caret) ; library(reshape2) ; library(ggthemes)  ; library(ggrepel)
 rm(list=ls())
    path = "H:\\DATA\\PE&wSMI\\Data_ENt\\Coma_liege\\LiegeContinous\\subjectsOCpred\\tau3SlbLieg\\" #path to the *OCpred.csv files
    #path = "H:\\DATA\\PE&wSMI\\Data_ENt\\Coma_salzburg\\SlbContinous\\subjectsOCpred\\tau3\\tau3SlbLieg\\"
    setwd(path)
    y <- glob2rx(paste("*OCpred.csv$"))
    predList <- dir(path, pattern = y, ignore.case = TRUE)
    performance <- data.frame(matrix(nrow =length(predList),ncol=6))
    colnames(performance)<-c("sbj","recall","precision","specific", "fpr", "f1")
    #diag <- c("UWS","UWS","MCS","MCS","MCS","MCS","UWS","UWS","UWS","UWS","MCS","UWS" )

    for (i in 1:length(predList)) {
        mydata <- read.csv(paste(path,predList[i], sep=""))
        #consider O/C as open or "nv" (nv is removed afterwards)
        mydata$trueEyes_CorO <- dplyr::recode(mydata$trueEyes, "O/C"="O")
        #remove "nv" class
        mydata <- mydata[ mydata$trueEyes_CorO != "nv" ,]
        #remove NAs, in predEyes, happens when REM or N1
        mydataComplete <- mydata[complete.cases(mydata),]
        mydataComplete$trueEyes_CorO <- factor(mydataComplete$trueEyes_CorO)
        
        recall <- with(mydataComplete, sensitivity(predEyes, trueEyes_CorO, positive = "C"))
        precision <- with(mydataComplete, posPredValue(predEyes,trueEyes_CorO, positive = "C"))
        specific <- with(mydataComplete, specificity(predEyes, trueEyes_CorO))
        fpr <- 1 - specific 
        f1 <- (2 * precision * recall) / (precision + recall)
        performance[i,] <- c(substr(predList[i], start=1, stop = 3), recall,precision,specific, fpr, f1)
    }
#}
# perf2 <- melt(data = performance, id.vars = "diag")
# p <- ggplot(aes(x=sbj, y=as.numeric(value), shape = variable),data = perf2)
# p + geom_point(size = 3) +
#     #geom_hline(aes(yintercept = 0.5)) +
#     xlab("Subject") +
#     ylab("Classification performance [F1 score]") +
#     theme(legend.title = element_blank()) + 
#     ggtitle("Eyes open / eyes closed predictions") + 
#     theme_few() + scale_colour_few(2)


# plot ROC curve, omit sbjs when recal or spec NA
#read diagnosis
setwd("H:\\DATA\\PE&wSMI\\Data_ENt\\Coma_liege\\LiegeContinous\\subjectsOCpred\\tau3SlbLieg\\")

diagnosis <- read.csv(file = "diag_bothSL.csv", col.names = c("name", "diag"), header = FALSE)
for (i in 1 : nrow(performance)){
  performance$diag[i] <- diagnosis$diag[substr(performance$sbj, 1,2)[i] == substr(diagnosis$name, 1,2)]
}


performance$diag <- factor(performance$diag, labels = c("UWS", "MCS"))
library(dplyr)            
performanceRoc <- select(performance, sbj, fpr, recall, diag) 
f <- ggplot(aes(x = as.numeric(fpr), y = as.numeric(recall)), data = na.omit(performanceRoc))
df <- data.frame(x = 0:1 , y = 0:1) #for random line
f + geom_point(size = 2.5) +
    geom_label_repel(aes(label = diag),
                        size = 5,fontface = "bold",
                        box.padding = unit(0.5, "lines"),
                        point.padding = unit(0.5, "lines"), 
                        segment.color = '#555555',
                        segment.size = 0.5,
                        arrow = arrow(length = unit(0.01, 'npc')) )+
                        scale_y_continuous(limits = c(0,1), breaks = seq(0,2,0.2)) +
                        xlab("FPR or (1-specificity)") +
                        ylab("TPR or sensitivity") +
                        geom_line(data = df, aes(x = x, y = y, colour = "random guess"),linetype = 2)  +
                        theme(legend.title = element_blank(),
                              legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
                              panel.background = element_rect(fill = NA, colour = NA),
                              text = element_text(size = 25),
                              axis.line.x = element_line(color="black", size = 1),
                              axis.line.y = element_line(color="black", size = 1))

                  
                        


                        
