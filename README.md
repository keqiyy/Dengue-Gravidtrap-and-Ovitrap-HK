# Dengue-Gravidtrap-and-Ovitrap-HK

***
This study details a methodological framework to estimate the delayed and nonlinear impact of climatic predictors (monthly mean temperature and monthly total rainfall) on activities of mosquitoes (extensiveness and abundance), which indicates the risk of dengue fever. The approach models the impact of climate factors in Hong Kong by using distributed lag nonlinear models (DLNM) with Bayesian hierarchical models fitted using integrated nested Laplace approximations in R (R-INLA). The modelling approach considers climate factors and random effects including year, month, and districts. These exposure-lag-response associations can reveal how climate factors might impact activities of mosquitoes in the months leading up to an outbreak in Hong Kong.
***

A description of each file and folder is provided below:

**00_Data_Preprocess.R**: R script to process raw data, transferring daily data to monthly data. This is for reference. The final data file (in .csv format) is provided in Raw Data which is named as "Three districts all dataset-1.csv". 

**01_load_packages_data.R**: R script to load and process data needed to run DLNMs in INLA models.

**02_Data_Visualization.R**: R script to explore and visualise climatic datasets.

**03_Model_Construction.R**: R script to run DLNM-INLA models of increasing complexity.

**04_LOOCV.R**: R script to validate the predictiveness of models by leave-one-month-out cross validation.

**05_lag_nonlinear_output.R**: R script to explore and visualise exposure-lag-response associations between climate factors and mosquitoes'activities (extensiveness and abundance).

**06_Normalization.R**: R script to normalize models to compare their accuracy.

**Raw Data**: a folder containing the database and data description.

**Figures**: a folder to save the figures generated by the R scripts.

**Model**: a folder to save the model outputs.

The analysis was performed using R version 4.2.0 (2022-12-14).
