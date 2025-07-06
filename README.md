# Mall Customer Segmentation Project

## Overview
This project analyzes customer data from the `Mall_Customers.csv` dataset to segment customers based on demographic and behavioral attributes. It employs various clustering algorithms and predictive modeling techniques to uncover patterns and insights for targeted marketing. The project includes data preprocessing, exploratory data analysis (EDA), clustering, predictive modeling, and an interactive Shiny dashboard.

## Dataset
The dataset (`Mall_Customers.csv`) contains the following columns:
- **CustomerID**: Unique identifier for each customer
- **Gender**: Customer's gender (Male/Female)
- **Age**: Customer's age
- **Annual Income (k$)**: Annual income in thousands of dollars
- **Spending Score (1-100)**: Spending score (1-100) based on customer behavior

## Project Structure
The project is implemented in R and includes the following components:

### 1. Data Loading and Preprocessing
- Validates the dataset, handles missing values with median imputation, and removes outliers using the IQR method.
- Scales numerical features and creates derived features like `Income_to_Age`, `Spending_to_Income`, `Age_Squared`, `Income_Log`, `Spending_Log`, `Income_Spending_Interaction`, `Age_Group`, `Income_Bin`, and `Spending_Bin`.

### 2. Exploratory Data Analysis (EDA)
- Generates summary statistics, skewness, kurtosis, and normality tests.
- Visualizes distributions (histograms, boxplots, violin plots) and correlations using a correlation matrix.

### 3. Clustering Analysis
- Assesses clustering tendency with the Hopkins statistic.
- Applies algorithms: K-means, Hierarchical, DBSCAN, Gaussian Mixture Model (GMM), Fuzzy C-means, Spectral, K-medoids, and Affinity Propagation.
- Evaluates clusters with silhouette scores and performs subgroup analysis by Gender and Age Group.

### 4. Cluster Visualization
- Creates PCA plots, 3D scatter plots, and scatter plots for feature pairs, saved as PNG and PDF files.

### 5. Predictive Modeling
- Trains models to predict Gender using Logistic Regression, Random Forest, SVM, and XGBoost, evaluated with confusion matrices.

### 6. Sensitivity Analysis
- Tests clustering robustness by adding noise at varying levels.

### 7. Shiny Dashboard
- Provides an interactive interface to explore clustering results and profiles.

### 8. Cluster Profiling
- Summarizes cluster characteristics (e.g., average age, income, spending score).

### 9. Unit Tests
- Validates data loading, preprocessing, and clustering results.

## Dependencies
Install the required R packages:
```R
install.packages(c("ggplot2", "plotrix", "purrr", "cluster", "gridExtra", "grid", "NbClust", "factoextra",
                   "plotly", "dbscan", "mclust", "caret", "dplyr", "corrplot", "e1071", "moments",
                   "rgl", "tidyverse", "FactoMineR", "fpc", "clustertend", "randomForest", "kernlab",
                   "fclust", "boot", "shiny", "DT", "ClusterR", "apcluster", "flexclust", "vegan",
                   "MASS", "xgboost", "neuralnet", "testthat", "jsonlite", "knitr", "rmarkdown"))
```
