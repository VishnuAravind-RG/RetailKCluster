# 🛍️ Mall Customer Segmentation

This project performs a **comprehensive customer segmentation analysis** using a dataset of mall customers. It includes preprocessing, feature engineering, multiple clustering algorithms, cluster evaluation, predictive modeling, sensitivity analysis, and a Shiny dashboard for interactive exploration.

---

## 📁 Dataset

- **Source**: `Mall_Customers.csv`
- **Expected Columns**:
  - `CustomerID`
  - `Gender`
  - `Age`
  - `Annual Income (k$)`
  - `Spending Score (1-100)`

---

## ⚙️ Project Structure

### ✅ Data Preprocessing

- Removal of duplicates
- Missing value imputation using **median**
- Outlier detection via **IQR**
- Feature scaling using **standardization**

### 🛠️ Feature Engineering

- Income to Age ratio
- Spending to Income ratio
- Age Squared
- Log-transformations of income & spending
- Binning (quartile-based) and age groups

### 📊 Exploratory Data Analysis (EDA)

- Summary statistics (mean, SD, skewness, kurtosis)
- **Shapiro-Wilk** normality tests
- Correlation matrix and visualizations (histograms, boxplots, violin plots)
- **Interactive plots** using `plotly`

---

## 🔍 Clustering Techniques Applied

| Method                   | Notes |
|--------------------------|-------|
| **K-Means**              | Elbow, Silhouette, Gap Statistics |
| **Hierarchical Clustering** | Dendrograms (Ward.D2, complete, single, average) |
| **DBSCAN**               | Noise point detection |
| **Gaussian Mixture Model (GMM)** | Log-likelihood and BIC evaluation |
| **Fuzzy C-means**        | Membership probability matrix |
| **Spectral Clustering**  | PCA-based similarity |
| **K-Medoids (PAM)**      | Robust to noise/outliers |
| **Affinity Propagation** | No need to specify `k` |

### 📐 Cluster Evaluation Metrics

- Silhouette Score
- Dunn Index
- Davies-Bouldin Index
- Jaccard Similarity (for cluster stability)

---

## 🧠 Predictive Modeling (Gender Classification)

- **Logistic Regression**
- **Random Forest**
- **SVM (Radial Kernel)**
- **XGBoost**

Each model uses Age, Income, Spending Score, and Cluster ID as features. Models evaluated via **Confusion Matrix**.

---

## 🔁 Sensitivity Analysis

- Noise injection in scaled data (5–20%)
- K-means robustness tested under noisy conditions

---

## 🧪 Unit Testing

Implemented using `testthat`:
- Validates data loading and column structure
- Verifies number of clusters

---

## 📊 Subgroup Clustering

Separate clustering for:
- Each **Gender**
- Each **Age Group**

---

## 📈 Visualizations

- PCA 2D scatter plots for each clustering method
- 3D scatter plots (Age vs Income vs Spending Score)
- Feature-pair clustering scatter plots
- Shiny Dashboard for interactive analysis

---

## 💡 Shiny Dashboard

- Built with `shiny` + `plotly` + `DT`
- Choose clustering method and subgroup filters
- Dynamic PCA visualization and summary tables

---

## 📂 Output Files

- `preprocessed_customer_data.csv`
- `preprocessed_customer_data_no_outliers.csv`
- `feature_engineered_customer_data.csv`
- `final_customer_data_with_clusters.csv`
- Cluster summaries: `cluster_summary_<method>.csv`
- Visualizations: `.png`, `.pdf`, `.html`

---

## 📑 Report

Render the RMarkdown report with:

```r
rmarkdown::render("customer_segmentation_report.Rmd", output_file = "customer_segmentation_report.pdf")
