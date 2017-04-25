#script for claster analysis of DOC
#loads Lieg and Slb data [VS, MCS], orders  the columns, and means them
#first element of each of the list is VS, second MCS
#last part of the scipt boostrap clustering (commented)
rm(list = ls())
library(popbio) #mean.list
path <- "H:\\DATA\\ANN_EEG\\R_automaticSleepClass\\results\\"

setwd(path)
VSlieg <- get(load("VSLiege4clustering.RData")); rm(VS)
VSsalzburg <- get(load("VSSalzburg4clustering.RData")) ; rm(VS)
MCSlieg <- get(load("MCSLiege4clustering.RData")) ; rm(MCS)
MCSsalzburg<- get(load("MCSSalzburg4clustering.RData")) ; rm(MCS)

#matrices from Slb and Liege
VS <- mean.list(list(as.matrix(VSlieg[2:15]), as.matrix(VSsalzburg[2:15])))
MCS <- mean.list(list(as.matrix(MCSlieg[2:15]), as.matrix(MCSsalzburg[2:15])))


# combine VS/MCS for scaling, split, add Diagnosis
#MCS <- MCSsalzburg ; VS <- VSsalzburg #only Slb
#VS <- VSlieg; MCS <- MCSlieg # only Liege

#bothGr <-  rbind(VS[,2:15], MCS[,2:15]) #1 group!!
#bothGr <-  rbind(VS, MCS) # Liege + Slb!!
#bothGr <- apply(bothGr, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))) #max_min    

#bothGr <- scale(bothGr)
#VS <- bothGr[1:100,]
#MCS <- bothGr[101:200,]

VS <- data.frame(VSlieg$time, VS)
colnames(VS)[1] <- "time"
MCS <- data.frame(MCSlieg$time, MCS)
colnames(MCS)[1] <- "time"

# add viariable: averElec and summarise using dplyr
#VS$averElec <- rowMeans(VS[, -1])
#MCS$averElec <- rowMeans(MCS[, -1])
#summarise(group_by(VS, time), mean = mean(averElec), sd = sd(averElec)) 

source("H:\\DATA\\ANN_EEG\\R_automaticSleepClass\\6_hierarchClust.R")
#VS <- VS[,c("time","Fz","Cz","T3","T4","Pz","Oz")] #select elec for clustering
#MCS <- MCS[,c("time","Fz","Cz","T3","T4","Pz","Oz")]
hierarchClust(VS)
hierarchClust(MCS)

#dev.copy(pdf,filename="VSHeatMap.pdf",width=800,height=800)
#dev.off ();

# boostrap clustering to estimate p values for clusters stability

#set.seed(123)
#vsClust <- pvclust(as.matrix(t(VS)), method.dist = "manhattan", 
#                   method.hclust = "average", 
#                   nboot = 1000, parallel = T)
#plot(vsClust)
#pvrect(vsClust, alpha=0.9)

#mcsClust <- pvclust(as.matrix(t(MCS)), method.dist = "manhattan", 
#                    method.hclust = "average", 
#                    nboot = 1000, parallel = T)
#plot(mcsClust)
#pvrect(mcsClust, alpha=0.9)

path_plots <- "C:\\Users\\b1016533\\Desktop\\results_paper17.01.17\\"
setwd(path_plots)

pdf(file = "heatVS.pdf", width = 8, height = 15)
hierarchClust(MCS)
dev.off()


