# ACC-injury-risk-modelling

# Information on this repository is provided here

About the folder:
source/: Contains all the R code
out/: Contains the final data to be shared with Programme Manager -list of 500 indiviudal
data/: Contains raw data provided for the analysis

About the files:

Code related to the modelling,
1. 1_load_preprocess.R :- Contains code to load required libraries,fetch the data and does some basic pre-processing and and feature engineering
2. 2_model_rf_baseline :- Contains code for baseline Random Forest Model 1
3. 3_model_rf_smote :- Contains code for SMOTE Random Forest Model 2
4. 4_diagnostics.R :- Contains code used to generate diagnostics for both the models
5. 5_population_selection.R :-  Contains code used to select 500 individuals
6. 6_visualisation.R :- Contains code used to produce basic graphs comparing top 500 individuals vs top 500 indivduals by stratified sampling.

For final modelling results, go to out/ACC_intervention_list_500.csv

Single point of execution,  

7. main_R:- Contains code to Run all R scripts

Others,
8. ACC-Model-Results-20250126.html :- Intended for technical audience. Markdown file that provides an overview of the task, methodology and modeling summary.

9.ACC PM Report 20250128.pdf :-A short report intended for the program manager.

10. ACC20250128.ppt :- Slide deck in ppt for presenting on the day.

11. ACC_20250128.pdf :- Slide deck in pdf for presenting on the day.

How to run?

You can run each of those files individually by,

1. Open the source/msain.R file and click ‘Source’

(Or)

2. source(“source/ filename.R”)


Packages needed to run the code: 

Code to install the package: (Copy/Paste and Run)
# Code to install the required packages (Copy/Paste and Run)

if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("randomForest")) install.packages("randomForest")
if (!require("caret")) install.packages("caret")
if (!require("zoo")) install.packages("zoo")
if (!require("pROC")) install.packages("pROC")
if (!require("smotefamily")) install.packages("smotefamily")
if (!require("fastDummies")) install.packages("fastDummies")
if (!require("corrplot")) install.packages("corrplot")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("plotly")) install.packages("plotly")
if (!require("xgboost")) install.packages("xgboost")


Kindly contact sujithroy.apps@gmail.com for any clarifications.