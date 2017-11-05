# MLstudio

## Overview

The ML Studio is an interactive platform for data visualization, statistical modeling and machine learning applications. Based on Shiny and shinydashboard interface, with Plotly interactive data visualization, DT HTML tables and H2O machine learning and deep learning algorithms, the ML Studio provide toolbox for the data science pipeline workflow.

### **Currently available features:**
1. Data Management -
  - Ability to load data from installed R package, R environment and/or csv file
  - Modify variable attribution
  - Data summary with dplyr functions
2. Interactive data visualization tool with the Plotly package, that include:
- Scatter, line, histogram correlation, etc.
- Time series plots – seasonality, correlation etc.
3. Machine learning and deep learning algorithms with the H2O package, currently only classification models available (Deep Learning, Random Forest, GBM, GLM)

### **Under construction features:**
1. Machine learning - 
  - Regression models
  - The caret function and models 
  - H2O grid search and autoML
  - Deep learning models with Keras
2. Time series and forecasting - 
  - Tools for time series analysis
  - Forecasting models 
3. Data visualization 
  – extending the current functionality



Please run the following R code to lunch the app into web browser (the app run best on google chrome):
source("https://raw.githubusercontent.com/RamiKrispin/Shiny-App/master/Shiny%20Modeling%20Git.R")

Please note - the package automatically installed the required packages, however the installation of the H2O package may require some additional Java add-on and it recommended to install it in R before lunching the app. 

H2O installation code can be find here (under the “INSTALL IN R” tab):

https://h2o-release.s3.amazonaws.com/h2o/rel-weierstrass/3/index.html
