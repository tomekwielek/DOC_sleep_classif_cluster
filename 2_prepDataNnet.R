prepDataNnet <- function (no_sbj,tauEEG = 3, tauPhysio = 1, physioLocations) {
## functions takes as arguments: "no_sbj" number of subjects to be used,
    ## "tauEEG" tau of PE for EEG channels (usually = 3)
    ## "tauPhysio" tau of PE for physiological channels (usually = 1)
    ## "physioLocations" arguments specify where are the physios (data set specific!!)
    
    ## other data set specific paraemetes: ncol(peAll),ncol(pePhysio)
rm(list=ls())
library(R.matlab)
library(gdata)
#path = 'F:\\DATA\\ANN_EEG\\ss_PE_30sec_forANN\\' #Slory data (used for master thesis)
path = "H:\\DATA\\PE&wSMI\\Data_ENt\\SleepInDoc_controls\\EXP\\30s_long_segm&staging\\filtered\\" #Insomnia data    
setwd(path)
#x <- glob2rx("*CPSG.xlsx") #Slory data
x <- glob2rx("*.txt") #Insomnia data
y <- glob2rx("*PE_filt.mat") 
Excellist <- dir(path, pattern = x, ignore.case = TRUE) #'Excellist' list with sleep stag (excel or txt)
PElist <- dir(path, pattern = y, ignore.case = TRUE)

sbj <- seq(1:no_sbj)
nfeatures=26 #nfeatures is data set specific!
peAll <- matrix(0, nrow=0, ncol=nfeatures) # init. empty matrix for all electrodes 
pePhysio <- matrix(0, nrow=0, ncol=nfeatures) # init. empty matrix for physio electrodes 
subjectsStore <- lapply(1:no_sbj, function(x) matrix(x, nrow=0, ncol=nfeatures))
stagingStore <- lapply(1:no_sbj, function(x) matrix(x, nrow = 0, ncol=2))

for(i in 1:no_sbj) #
{
  PEfile <- readMat(paste(path, PElist[i], sep ="")) 
  peAll <- t(PEfile$H[[tauEEG]][[1]]) #read all (EEG and physio) with tau specified for EEG
  
  pePhysio <- t(PEfile$H[[tauPhysio]][[1]][physioLocations,]) # read only physio with tau specified
  peAll[,physioLocations] <- pePhysio #merge EEG and physio with specific taus
  
  excelFile <- paste(path, Excellist[i], sep = "") 
  #stag <- read.xls(excelFile, sheet = 1, header = FALSE, perl = "C:\\Perl64\\bin\\perl.exe")
  stag <- read.table(excelFile,header = FALSE) #Insomnia data
  #no <- rep(substring(Excellist[i],2,3L), nrow(stag)) #extract sbjs. no from file name, #Slory data!
  no <- rep(substring(Excellist[i],11,13), nrow(stag))
  stag <- cbind(no, stag)
    if (nrow(stag) < nrow(peAll)) { #makes sure the length of PEs and stag is equal
      peAll <- peAll[1:nrow(stag),]
    }
      else if(nrow(stag) > nrow(peAll)){
          stag <- stag[1:nrow(peAll),]
      } 

  subjectsStore[[i]] <- peAll
  stagingStore[[i]] <- stag
}

myData<- list("STAG" = stagingStore,"PE" = subjectsStore) #pack staging and pe values to a list to allow returning of both

#indArrousal <-which(my_data$V1 == 6) #index arrousal
#indUnscorable <- which(my_data$V1 == 9) #index unscorable(?)

#if(length(indArrousal) > 0 || length(indUnscorable) > 0) { #remove arrousal and unscorable if present
#  my_data <- my_data[-c(indArrousal,indUnscorable),]
#}

#my_data <- my_data[-c(indArrousal,indUnscorable), ]#remove arrousal and unscorable (?) from the full set

return(myData)


} # end of the function

