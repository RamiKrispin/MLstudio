# **MLstudio**

## **Overview**

The ML Studio is an interactive platform for data visualization, statistical modeling and machine learning applications. Based on [Shiny](http://shiny.rstudio.com/) and [shinydashboard](https://rstudio.github.io/shinydashboard/) interface, with [Plotly](https://plot.ly/r/) interactive data visualization, [DT](https://rstudio.github.io/DT/) HTML tables and [H2O](https://www.h2o.ai/) machine learning and deep learning algorithms, the ML Studio provides a set of tools for the data science pipeline workflow.

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
  - In depth model summary
  - Ability to compare, select and save models 
  - Regression models
  - The caret function and models 
  - H2O grid search and autoML
  - Deep learning models with Keras
2. Time series and forecasting - 
  - Tools for time series analysis
  - Forecasting models with the forecast package 
3. Data visualization 
  – extending the current functionality

### **Installation**

The package is available for installation with the devtools package (if devetools package is not installed please use `install.packages("devtools")` to install it).

```r
# Install the MLstudio
devtools::install_github("RamiKrispin/MLstudio")
```
Please note – the H2O package may require additional Java adds-in (if not installed) and therefor is listed under the “Suggests” packages list of the MLstudio package (and not under the Imports or Depends list) and won’t be installed automatically during the installation of the MLstudio package. More information about the installation of H2O can be find in [H2O documentation](http://h2o-release.s3.amazonaws.com/h2o/rel-weierstrass/7/index.html) (under the "INSTALL IN R" tab).

### **Launch the App**

The app is called from R and opened on the default web browser (running best on Google Chrome). To open the app please use:

```r
# Launch the MLstudio
runML()
```
