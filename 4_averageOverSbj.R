#function is used by script sleepInDocMain.R
#reads data (EEG and/or physio), read excel diag data, sample rand obserwations for each sbj
#split into vs and mcs (note it assumes the same order of sbjs names  in EEG and diag files)
#use function from popbio package to mean list of matrices, gets av. over subjects
#time argument only to pick excell sheet
#samplesNO argument specifies sampling of the sleep segments
#return either listVSandMcs (sampled and averaged matrix) or
#listVSavMCSav (single mean)
averageOverSbj <- function(tauEeg, samplesNO = 50){
    All <- list() ; meanFullmat <- list()
    Names <- list()
    Diag <- list()
    set.seed(123)
    diagnosis <- read.xlsx(diagnosisPath, sheetName = "diag", header = F)
    #diagnosis$X1 <- gsub("'", "", diagnosis$X1) #edit the quatation marks
    #browser()
    for (i in 1:EEGfilesCount){ 
        DataEeg <- readMat(paste(EEGpath, EEGfilesNames[i], sep = "\\"))
        
        #if (filesNamesEeg[i] == filesNamesPhysio[i]){ #check subj names in EEG and physio
        #    DataPhysio <- readMat(paste(pathPhysio, filesNamesEeg[i], sep = ""))
            #Names[i] <- EEGfilesNames[i]
            Diag[i] <- diagnosis[which(diagnosis$X1 == substr(EEGfilesNames[i],1,3)),2]#search matching names,take diagnosis, compare first 3 letters
            PE <- t(DataEeg$H[[tauEeg]][[1]])
            #Slb and Liege have different elec. setup, change colnames accordingly
            if (dataSet == "Salzburg"){
                #pick and order 14 electrodes (originally SLB has 25), so that match with Liege order (see physio differences between Slb and Liege)  
                #"Fz","F3","F4","Cz","C3","C4","T3","T4","Pz","P3","P4","Oz","EMGup_bi","vEOGbi"
                #"EMGup_bi","vEOGbi" are called EMG and EOG respect
                 PE <-  subset(PE, select = c(15, 1, 2, 16, 5, 6, 9, 10, 17, 7, 8, 18, 22, 20))
                
            }

            else if(dataSet == "Liege" & ncol(PE) == 16 ){#skip ECG12, FR
                 PE <-  subset(PE, select = c(1,2,3,4,5,6,7,8,9,10,11,12,15,16))
            } 
  
            else if(dataSet == "Liege" & ncol(PE) == 15 ){#skip ECG12 (RESO was not exported for some sbjs)
                 PE <-  subset(PE, select = c(1,2,3,4,5,6,7,8,9,10,11,12,14,15))
            }
                    
            
            colnames(PE) <- c("Fz","F3","F4","Cz","C3","C4","T3","T4","Pz","P3","P4","Oz","EMG","EOG")
            #meanFullmat[i] <- round(mean(PE), digits = 4)
            n_row <- nrow(PE) #no of segments, used for sampling
            #browser()
            All[i] <- list(PE[sample(c(1:n_row),samplesNO), ]) # sample rand 50 obs

    }
    #browser()

    #meanFullmat <- data.frame(unlist(meanFullmat), unlist(Names)) #av over whole matrix, no sampling
    #colnames(meanFullmat) <- c("PE_value", "Name")
    #vsMeanFull <- meanFullmat[!unlist(Diag),] #select only VS based on diagnosis (averaged data)
    #mcsMeanFull <- meanFullmat[as.logical(unlist(Diag)),]

    vs <- All[!unlist(Diag)] #select only VS based on diagnosis (sampled data)
    mcs <- All[as.logical(unlist(Diag))]
    
    #vsArr <- array(unlist(vs), dim = c(dim(vs[[1]]),length(vs)))#list into 3d array [sbj,rows, cols]
    #meanVS <- apply(vsArr, 1:2, mean) #average over vs
    
    #mcsArr <- array(unlist(mcs), dim = c(dim(mcs[[1]]),length(mcs)))
    #meanMCS <- apply(mcsArr, 1:2, mean) # average over mcs
    
    meanVS <- mean.list(vs) #average over subjects
    meanMCS <- mean.list(mcs) 
    
    listVSandMcs <- list(meanVS, meanMCS)
    #listVSavMCSav <- list(vsMeanFull,mcsMeanFull)
    #browser()
    names(listVSandMcs) <- c("VS", "MCS") # first elem of the returned list is VS, second MCS
    return(listVSandMcs)
    #return means for single subjects (2 look for outliers)
    # meanSingleVS <- sapply(vs, mean)
    # meanSingleMCS <- sapply(mcs, mean)
    # listVSandMcs <- list(meanSingleVS, meanSingleMCS)
    # names(listVSandMcs) <- c("VS", "MCS")
    # return(listVSandMcs)
}

