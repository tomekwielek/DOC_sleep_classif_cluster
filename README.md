# DOC_sleep_classif_cluster

## Overview 
Automatic sleep classification for healthy and clinical population based on brain and body signal (PSG). The goal is to train classifier on helthy sleep data and ultimately test on DOC subjects (Disorders of Consciousness) where the standard sleep scoring is ultimately difficult. Input for the classifer is Permutation Entropy values (signal complexity, Band&Pompe,2002) computed for 30s segments over 26 EEG/PSG channels. In addition to supervised classifcation we perform unsupervised hierarchical clustering analysis .

## References
Permuatation Entropy paper: https://doi.org/10.1103/PhysRevLett.88.174102
