# DOC_sleep_classif_cluster

## Overview 
Automatic sleep classification for healthy and clinical population based on brain and body signal (PSG). The goal is to train classifier on helthy sleep data and ultimately test on DOC subjects (Disorders of Consciousness) where the standard sleep scoring is ultimately difficult. Input for the classifer is Permutation Entropy values (signal complexity, Band&Pompe,2002) computed for 30s segments over 26 EEG/PSG channels. In addition to supervised classifcation we perform unsupervised hierarchical clustering analysis. The generalization for DOC is validated through videos and eyes open/closed informaiton.

## References
Permuatation Entropy paper: https://doi.org/10.1103/PhysRevLett.88.174102

## Main Functionalities
[1] *1_nnetwClass.R* load matlab files containing both PE values (input) and sleep staging (target), restructure (i.e: col. naming, pruning, reshaping), leave_sbj_out cross validation + internal cv for hyperparameters, fit Random Forest or SVM, compute F1 score, plot prediction against true staging for each healthy subject, save trained model (later used for DOC).
<br> [2] *3_sleeepInDocMain.R* Prepare DOC data for clustering: sampling, incorporating diagnosis (VS vs MCS), splitting into day/night, saving.
<br> [3] *6_hierarchClust.R* and *5_SlbLiegOrderColsMerge.R* hierarchical clustering separately for VS and MCS, visualisation as heatmap where rows are labeled by day/night info.
<br> [4] *7_predictLiege.R* load DOC data, load healthy-trained model, perform cross-classification, plot predictions plus information about patient's eyes state, for each subject and separately for day and night period 
<br> [5] *9_ROCcurve.R* compute several performance measures for DOC subjects (e.g F1 score) and plot True Positive Rate against False Postive Rate
