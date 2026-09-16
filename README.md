# Code for Modelling Diminishing Returns in Component Network Meta-Analysis
This repository contains the R and WinBUGs code to accompany the paper "Modelling Diminishing Returns in Component Network Meta-Analysis: Development, Evaluation, and Application" by Smith et al.

The folder OpenBUGS_model_txt_files contains .txt files with the model to be fitted written in BUGS code.

The folder R_data_gen_functions contains two R functions needed for generating data.

The folder R_files_to_run_models contains R code for running each model. These files call the functions from R_model_functions.

The folder R_model_functions contains R functions which call OpenBUGS and fit the models. The files call the files from the folder OpenBUGS_model_txt_files.

The folders Scenario1, ..., Scenario8 contain R code to generate the datasets for each scenario. These files call the functions from the folder R_data_gen_functions. There are no data generating files for scenarios 9-12 becuase the data is the same as scenarios 1-4.

The folder Results contains R files for analysing the results of the simulation study and producing plots.

The folder BreastCancerData contains the reconstrcted IPD for the metastatic breast cancer example presented in the paper and R files for running the fractional polynomial, piecewise exponential and Royston-Parmar models on the breast cancer data and produce a forest plot of the RMSTD results. 
