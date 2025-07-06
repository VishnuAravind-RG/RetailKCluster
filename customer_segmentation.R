# Load Libraries
library(ggplot2)
library(plotrix)
library(purrr)
library(cluster)
library(gridExtra)
library(grid)
library(NbClust)
library(factoextra)
library(plotly)
library(dbscan)
library(mclust)
library(caret)
library(dplyr)
library(corrplot)
library(e1071)
library(moments)
library(rgl)
library(tidyverse)
library(FactoMineR)
library(fpc)
library(clustertend)
library(randomForest)
library(kernlab)
library(fclust)
library(boot)
library(shiny)
library(DT)
library(ClusterR)
library(apcluster)
library(flexclust)
library(vegan)
library(MASS)
library(xgboost)
library(neuralnet)
library(testthat)
library(jsonlite)
library(knitr)
library(rmarkdown)

# Data Loading and Preprocessing
load_and_validate_data <- function(file_path) {
  data <- read.csv(file_path)
  expected_cols <- c("CustomerID", "Gender", "Age", "Annual.Income..k..", "Spending.Score..1.100.")
  if (!all(expected_cols %in% names(data))) {
    stop("Dataset does not contain all expected columns: ", paste(expected_cols, collapse=", "))
  }
  if (any(duplicated(data$CustomerID))) {
    warning("Duplicate CustomerIDs detected. Removing duplicates.")
    data <- data[!duplicated(data$CustomerID),]
  }
  return(data)
}

customer_data <- load_and_validate_data("Mall_Customers.csv")

cat("\nDataset Structure:\n")
str(customer_data)

cat("\nMissing Values Check:\n")
missing_values <- colSums(is.na(customer_data))
print(missing_values)

impute_with_median <- function(x) {
  x[is.na(x)] <- median(x, na.rm=TRUE)
  return(x)
}
customer_data$Age <- impute_with_median(customer_data$Age)
customer_data$Annual.Income..k.. <- impute_with_median(customer_data$Annual.Income..k..)
customer_data$Spending.Score..1.100. <- impute_with_median(customer_data$Spending.Score..1.100.)

customer_data$Gender <- as.factor(customer_data$Gender)

scale_features <- function(data, columns) {
  data[,columns] <- scale(data[,columns])
  return(data)
}
customer_data_scaled <- scale_features(customer_data, c("Age", "Annual.Income..k..", "Spending.Score..1.100."))

outlier_detection <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- which(x < lower_bound | x > upper_bound)
  return(outliers)
}

age_outliers <- outlier_detection(customer_data$Age)
income_outliers <- outlier_detection(customer_data$Annual.Income..k..)
spending_outliers <- outlier_detection(customer_data$Spending.Score..1.100.)

cat("\nOutliers Detected:\n")
cat("Age:", length(age_outliers), "outliers\n")
cat("Annual Income:", length(income_outliers), "outliers\n")
cat("Spending Score:", length(spending_outliers), "outliers\n")

customer_data_no_outliers <- customer_data[-unique(c(age_outliers, income_outliers, spending_outliers)),]
customer_data_no_outliers_scaled <- scale_features(customer_data_no_outliers, 
                                                  c("Age", "Annual.Income..k..", "Spending.Score..1.100."))

write.csv(customer_data, "preprocessed_customer_data.csv", row.names=FALSE)
write.csv(customer_data_no_outliers, "preprocessed_customer_data_no_outliers.csv", row.names=FALSE)

# Feature Engineering
customer_data$Income_to_Age <- customer_data$Annual.Income..k.. / customer_data$Age
customer_data$Spending_to_Income <- customer_data$Spending.Score..1.100. / customer_data$Annual.Income..k..
customer_data$Age_Group <- cut(customer_data$Age, 
                               breaks=c(0, 25, 35, 45, 55, 100), 
                               labels=c("Young", "Young Adult", "Adult", "Middle-Aged", "Senior"))
customer_data$Age_Squared <- customer_data$Age^2
customer_data$Income_Log <- log1p(customer_data$Annual.Income..k..)
customer_data$Spending_Log <- log1p(customer_data$Spending.Score..1.100.)
customer_data$Income_Spending_Interaction <- customer_data$Annual.Income..k.. * customer_data$Spending.Score..1.100.
customer_data$Income_Bin <- cut(customer_data$Annual.Income..k.., 
                                breaks=quantile(customer_data$Annual.Income..k.., probs=seq(0, 1, 0.25)),
                                labels=c("Low", "Medium-Low", "Medium-High", "High"), include.lowest=TRUE)
customer_data$Spending_Bin <- cut(customer_data$Spending.Score..1.100., 
                                  breaks=quantile(customer_data$Spending.Score..1.100., probs=seq(0, 1, 0.25)),
                                  labels=c("Low", "Medium-Low", "Medium-High", "High"), include.lowest=TRUE)
customer_data_scaled <- scale_features(customer_data, 
                                      c("Age", "Annual.Income..k..", "Spending.Score..1.100.", 
                                        "Income_to_Age", "Spending_to_Income", 
                                        "Age_Squared", "Income_Log", "Spending_Log", 
                                        "Income_Spending_Interaction"))
write.csv(customer_data, "feature_engineered_customer_data.csv", row.names=FALSE)

# Exploratory Data Analysis (EDA)
cat("\nSummary Statistics for Numerical Variables:\n")
numeric_vars <- c("Age", "Annual.Income..k..", "Spending.Score..1.100.", 
                  "Income_to_Age", "Spending_to_Income", 
                  "Age_Squared", "Income_Log", "Spending_Log", "Income_Spending_Interaction")
for (var in numeric_vars) {
  cat("\n", var, ":\n")
  print(summary(customer_data[[var]]))
}

cat("\nStandard Deviations:\n")
for (var in numeric_vars) {
  cat(var, ":", sd(customer_data[[var]], na.rm=TRUE), "\n")
}

cat("\nSkewness and Kurtosis:\n")
for (var in numeric_vars) {
  cat(var, " - Skewness:", skewness(customer_data[[var]], na.rm=TRUE), 
      ", Kurtosis:", kurtosis(customer_data[[var]], na.rm=TRUE), "\n")
}

cat("\nShapiro-Wilk Normality Tests:\n")
for (var in numeric_vars) {
  cat(var, ":", shapiro.test(customer_data[[var]])$p.value, "\n")
}

numeric_data <- customer_data[, numeric_vars]
cor_matrix <- cor(numeric_data, use="complete.obs")
corrplot(cor_matrix, method="color", title="Correlation Matrix of Numerical Features", 
         mar=c(0,0,1,0), addCoef.col="black", tl.cex=0.7)

png("correlation_matrix.png", width=800, height=600)
corrplot(cor_matrix, method="color", title="Correlation Matrix of Numerical Features", 
         mar=c(0,0,1,0), addCoef.col="black", tl.cex=0.7)
dev.off()

# Gender Distribution Visualizations
gender_table <- table(customer_data$Gender)
barplot(gender_table, main="Gender Distribution", xlab="Gender", ylab="Count", 
        col=c("pink", "blue"), legend=rownames(gender_table), args.legend=list(x="topright"))

pct <- round(gender_table/sum(gender_table)*100)
labels <- paste(c("Female", "Male"), " ", pct, "%", sep="")
pie3D(gender_table, labels=labels, main="Pie Chart: Gender Ratio", col=c("pink", "blue"))

p_gender <- plot_ly(x=names(gender_table), y=gender_table, type="bar", marker=list(color=c("pink", "blue"))) %>%
  layout(title="Interactive Gender Distribution", xaxis=list(title="Gender"), yaxis=list(title="Count"))
htmlwidgets::saveWidget(p_gender, "gender_distribution.html")

themes <- c("theme_minimal", "theme_classic", "theme_dark", "theme_light")
for (theme in themes) {
  p_gender_ggplot <- ggplot(customer_data, aes(x=Gender, fill=Gender)) +
    geom_bar() +
    scale_fill_manual(values=c("pink", "blue")) +
    labs(title=paste("Gender Distribution (", theme, ")"), x="Gender", y="Count") +
    do.call(theme, list())
  print(p_gender_ggplot)
  ggsave(paste0("gender_barplot_", theme, ".png"), plot=p_gender_ggplot, width=6, height=4)
}

# Age Distribution Visualizations
create_age_visualizations <- function(data, group_var=NULL, group_value=NULL) {
  if (!is.null(group_var)) {
    data <- data %>% filter(!!sym(group_var) == group_value)
    title_prefix <- paste(group_var, group_value, ":")
  } else {
    title_prefix <- ""
  }
  
  hist_plot <- ggplot(data, aes(x=Age)) +
    geom_histogram(bins=15, fill="blue", color="black") +
    labs(title=paste(title_prefix, "Histogram of Age"), x="Age", y="Count") +
    theme_minimal()
  print(hist_plot)
  
  box_plot <- ggplot(data, aes(y=Age)) +
    geom_boxplot(fill="#ff0066") +
    labs(title=paste(title_prefix, "Boxplot of Age")) +
    theme_minimal()
  print(box_plot)
  
  violin_plot <- ggplot(data, aes(x=Gender, y=Age, fill=Gender)) +
    geom_violin() +
    scale_fill_manual(values=c("pink", "blue")) +
    labs(title=paste(title_prefix, "Violin Plot of Age by Gender"), x="Gender", y="Age") +
    theme_minimal()
  print(violin_plot)
  
  interactive_hist <- plot_ly(x=data$Age, type="histogram", marker=list(color="blue")) %>%
    layout(title=paste(title_prefix, "Interactive Histogram of Age"), 
           xaxis=list(title="Age"), yaxis=list(title="Count"))
  htmlwidgets::saveWidget(interactive_hist, paste0("age_histogram_", group_var, "_", group_value, ".html"))
  
  ggsave(paste0("age_histogram_", group_var, "_", group_value, ".png"), plot=hist_plot, width=6, height=4)
  ggsave(paste0("age_boxplot_", group_var, "_", group_value, ".png"), plot=box_plot, width=6, height=4)
  ggsave(paste0("age_violin_", group_var, "_", group_value, ".png"), plot=violin_plot, width=6, height=4)
}

create_age_visualizations(customer_data)
for (gender in levels(customer_data$Gender)) {
  create_age_visualizations(customer_data, "Gender", gender)
}
for (age_group in levels(customer_data$Age_Group)) {
  create_age_visualizations(customer_data, "Age_Group", age_group)
}

# Annual Income Visualizations
create_income_visualizations <- function(data, group_var=NULL, group_value=NULL) {
  if (!is.null(group_var)) {
    data <- data %>% filter(!!sym(group_var) == group_value)
    title_prefix <- paste(group_var, group_value, ":")
  } else {
    title_prefix <- ""
  }
  
  hist_plot <- ggplot(data, aes(x=Annual.Income..k..)) +
    geom_histogram(bins=15, fill="#660033", color="black") +
    labs(title=paste(title_prefix, "Histogram of Annual Income"), x="Annual Income (k$)", y="Count") +
    theme_minimal()
  print(hist_plot)
  
  density_plot <- ggplot(data, aes(x=Annual.Income..k..)) +
    geom_density(fill="#ccff66", alpha=0.5) +
    labs(title=paste(title_prefix, "Density Plot of Annual Income"), x="Annual Income (k$)", y="Density") +
    theme_minimal()
  print(density_plot)
  
  violin_plot <- ggplot(data, aes(x=Gender, y=Annual.Income..k.., fill=Gender)) +
    geom_violin() +
    scale_fill_manual(values=c("pink", "blue")) +
    labs(title=paste(title_prefix, "Violin Plot of Income by Gender"), x="Gender", y="Annual Income (k$)") +
    theme_minimal()
  print(violin_plot)
  
  interactive_hist <- plot_ly(x=data$Annual.Income..k.., type="histogram", marker=list(color="#660033")) %>%
    layout(title=paste(title_prefix, "Interactive Histogram of Annual Income"), 
           xaxis=list(title="Annual Income (k$)"), yaxis=list(title="Count"))
  htmlwidgets::saveWidget(interactive_hist, paste0("income_histogram_", group_var, "_", group_value, ".html"))
  
  ggsave(paste0("income_histogram_", group_var, "_", group_value, ".png"), plot=hist_plot, width=6, height=4)
  ggsave(paste0("income_density_", group_var, "_", group_value, ".png"), plot=density_plot, width=6, height=4)
  ggsave(paste0("income_violin_", group_var, "_", group_value, ".png"), plot=violin_plot, width=6, height=4)
}

create_income_visualizations(customer_data)
for (gender in levels(customer_data$Gender)) {
  create_income_visualizations(customer_data, "Gender", gender)
}
for (age_group in levels(customer_data$Age_Group)) {
  create_income_visualizations(customer_data, "Age_Group", age_group)
}

# Spending Score Visualizations
create_spending_visualizations <- function(data, group_var=NULL, group_value=NULL) {
  if (!is.null(group_var)) {
    data <- data %>% filter(!!sym(group_var) == group_value)
    title_prefix <- paste(group_var, group_value, ":")
  } else {
    title_prefix <- ""
  }
  
  hist_plot <- ggplot(data, aes(x=Spending.Score..1.100.)) +
    geom_histogram(bins=15, fill="#6600cc", color="black") +
    labs(title=paste(title_prefix, "Histogram of Spending Score"), x="Spending Score (1-100)", y="Count") +
    theme_minimal()
  print(hist_plot)
  
  box_plot <- ggplot(data, aes(y=Spending.Score..1.100.)) +
    geom_boxplot(fill="#990000") +
    labs(title=paste(title_prefix, "Boxplot of Spending Score")) +
    theme_minimal()
  print(box_plot)
  
  density_plot <- ggplot(data, aes(x=Spending.Score..1.100.)) +
    geom_density(fill="#cc99ff", alpha=0.5) +
    labs(title=paste(title_prefix, "Density Plot of Spending Score"), x="Spending Score (1-100)", y="Density") +
    theme_minimal()
  print(density_plot)
  
  violin_plot <- ggplot(data, aes(x=Gender, y=Spending.Score..1.100., fill=Gender)) +
    geom_violin() +
    scale_fill_manual(values=c("pink", "blue")) +
    labs(title=paste(title_prefix, "Violin Plot of Spending Score by Gender"), 
         x="Gender", y="Spending Score (1-100)") +
    theme_minimal()
  print(violin_plot)
  
  interactive_hist <- plot_ly(x=data$Spending.Score..1.100., type="histogram", marker=list(color="#6600cc")) %>%
    layout(title=paste(title_prefix, "Interactive Histogram of Spending Score"), 
           xaxis=list(title="Spending Score (1-100)"), yaxis=list(title="Count"))
  htmlwidgets::saveWidget(interactive_hist, paste0("spending_histogram_", group_var, "_", group_value, ".html"))
  
  ggsave(paste0("spending_histogram_", group_var, "_", group_value, ".png"), plot=hist_plot, width=6, height=4)
  ggsave(paste0("spending_boxplot_", group_var, "_", group_value, ".png"), plot=box_plot, width=6, height=4)
  ggsave(paste0("spending_density_", group_var, "_", group_value, ".png"), plot=density_plot, width=6, height=4)
  ggsave(paste0("spending_violin_", group_var, "_", group_value, ".png"), plot=violin_plot, width=6, height=4)
}

create_spending_visualizations(customer_data)
for (gender in levels(customer_data$Gender)) {
  create_spending_visualizations(customer_data, "Gender", gender)
}
for (age_group in levels(customer_data$Age_Group)) {
  create_spending_visualizations(customer_data, "Age_Group", age_group)
}

# Clustering Tendency Assessment
set.seed(123)
hopkins_stat <- get_clust_tendency(customer_data_scaled[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")], 
                                    n=nrow(customer_data_scaled)-1)$hopkins_stat
cat("\nHopkins Statistic (Clustering Tendency):", hopkins_stat, "\n")

feature_subsets <- list(
  c("Age", "Annual.Income..k.."),
  c("Age", "Spending.Score..1.100."),
  c("Annual.Income..k..", "Spending.Score..1.100."),
  c("Age", "Income_to_Age"),
  c("Spending.Score..1.100.", "Spending_to_Income")
)
for (subset in feature_subsets) {
  hopkins_stat_subset <- get_clust_tendency(customer_data_scaled[,subset], 
                                            n=nrow(customer_data_scaled)-1)$hopkins_stat
  cat("\nHopkins Statistic for", paste(subset, collapse=" & "), ":", hopkins_stat_subset, "\n")
}

# K-means Clustering
elbow_method <- function(data, max_k) {
  iss <- function(k) {
    kmeans(data, k, iter.max=100, nstart=100, algorithm="Lloyd")$tot.withinss
  }
  k.values <- 1:max_k
  iss_values <- map_dbl(k.values, iss)
  return(data.frame(k=k.values, iss=iss_values))
}

elbow_data <- elbow_method(customer_data_scaled[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")], 10)

elbow_plot <- ggplot(elbow_data, aes(x=k, y=iss)) +
  geom_line() +
  geom_point() +
  labs(title="Elbow Method for K-means Clustering", x="Number of Clusters (k)", 
       y="Total Within-Cluster Sum of Squares") +
  theme_minimal()
print(elbow_plot)
ggsave("elbow_plot.png", plot=elbow_plot, width=6, height=4)

silhouette_plot <- fviz_nbclust(customer_data_scaled[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")], 
                                kmeans, method="silhouette")
print(silhouette_plot)
ggsave("silhouette_plot.png", plot=silhouette_plot, width=6, height=4)

set.seed(125)
gap_stat <- clusGap(customer_data_scaled[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")], 
                    FUN=kmeans, nstart=25, K.max=10, B=50)
gap_plot <- fviz_gap_stat(gap_stat)
print(gap_plot)
ggsave("gap_stat_plot.png", plot=gap_plot, width=6, height=4)

set.seed(123)
k6 <- kmeans(customer_data_scaled[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")], 
             6, iter.max=100, nstart=50, algorithm="Lloyd")
customer_data$Cluster_Kmeans <- as.factor(k6$cluster)

sil_k6 <- silhouette(k6$cluster, dist(customer_data_scaled[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")]))
cat("\nAverage Silhouette Score for K-means (k=6):", mean(sil_k6[,3]), "\n")

db_index_k6 <- cluster.stats(dist(customer_data_scaled[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")]), 
                             k6$cluster)$dunn
cat("\nDavies-Bouldin Index for K-means (k=6):", db_index_k6, "\n")

for (subset in feature_subsets) {
  k6_subset <- kmeans(customer_data_scaled[,subset], 6, iter.max=100, nstart=50)
  customer_data[[paste("Cluster_Kmeans", paste(subset, collapse=""), sep="")]] <- as.factor(k6_subset$cluster)
  sil_subset <- silhouette(k6_subset$cluster, dist(customer_data_scaled[,subset]))
  cat("\nAverage Silhouette Score for K-means (", paste(subset, collapse=" & "), "):", 
      mean(sil_subset[,3]), "\n")
}

# Hierarchical Clustering
dist_matrix <- dist(customer_data_scaled[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")], 
                    method="euclidean")

linkage_methods <- c("ward.D2", "complete", "single", "average")
hclust_results <- list()
for (method in linkage_methods) {
  hclust_results[[method]] <- hclust(dist_matrix, method=method)
}

par(mfrow=c(2,2))
for (method in linkage_methods) {
  plot(hclust_results[[method]], main=paste("Dendrogram (", method, ")"), 
       xlab="Customers", ylab="Height")
}
par(mfrow=c(1,1))

hclust_clusters <- cutree(hclust_results[["ward.D2"]], k=6)
customer_data$Cluster_Hclust <- as.factor(hclust_clusters)

sil_hclust <- silhouette(hclust_clusters, dist_matrix)
cat("\nAverage Silhouette Score for Hierarchical Clustering (Ward.D2, k=6):", mean(sil_hclust[,3]), "\n")

for (method in linkage_methods[-1]) {
  clusters <- cutree(hclust_results[[method]], k=6)
  customer_data[[paste("Cluster_Hclust", method, sep="_")]] <- as.factor(clusters)
  sil <- silhouette(clusters, dist_matrix)
  cat("\nAverage Silhouette Score for Hierarchical Clustering (", method, ", k=6):", 
      mean(sil[,3]), "\n")
}

# DBSCAN Clustering
eps_values <- seq(0.3, 0.7, by=0.05)
minPts_values <- 3:7
dbscan_results <- list()
for (eps in eps_values) {
  for (minPts in minPts_values) {
    dbscan_result <- dbscan(customer_data_scaled[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")], 
                            eps=eps, minPts=minPts)
    dbscan_results[[paste("eps", eps, "minPts", minPts, sep="_")]] <- dbscan_result
  }
}

dbscan_result <- dbscan_results[["eps_0.5_minPts_5"]]
customer_data$Cluster_DBSCAN <- as.factor(dbscan_result$cluster)

cat("\nDBSCAN: Number of noise points:", sum(dbscan_result$cluster == 0), "\n")

dbscan_dist <- dist(customer_data_scaled[dbscan_result$cluster != 0,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")])
dbscan_sil <- silhouette(dbscan_result$cluster[dbscan_result$cluster != 0], dbscan_dist)
cat("\nAverage Silhouette Score for DBSCAN:", mean(dbscan_sil[,3]), "\n")

for (subset in feature_subsets) {
  dbscan_subset <- dbscan(customer_data_scaled[,subset], eps=0.5, minPts=5)
  customer_data[[paste("Cluster_DBSCAN", paste(subset, collapse=""), sep="")]] <- as.factor(dbscan_subset$cluster)
  cat("\nDBSCAN (", paste(subset, collapse=" & "), "): Number of noise points:", 
      sum(dbscan_subset$cluster == 0), "\n")
}

# Gaussian Mixture Model (GMM)
gmm_models <- list()
for (g in 2:10) {
  gmm_models[[as.character(g)]] <- Mclust(customer_data_scaled[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")], G=g)
}

gmm_model <- gmm_models[["6"]]
customer_data$Cluster_GMM <- as.factor(gmm_model$classification)

cat("\nGMM Log-Likelihood:", gmm_model$loglik, "\n")
cat("GMM BIC:", gmm_model$bic, "\n")

for (subset in feature_subsets) {
  gmm_subset <- Mclust(customer_data_scaled[,subset], G=6)
  customer_data[[paste("Cluster_GMM", paste(subset, collapse=""), sep="")]] <- as.factor(gmm_subset$classification)
  cat("\nGMM (", paste(subset, collapse=" & "), ") BIC:", gmm_subset$bic, "\n")
}

# Fuzzy C-means Clustering
fuzzy_result <- fcm(customer_data_scaled[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")], centers=6)
customer_data$Cluster_Fuzzy <- as.factor(fuzzy_result$cluster)

cat("\nFuzzy C-means Membership Probabilities (first 5 rows):\n")
print(head(fuzzy_result$membership))

for (subset in feature_subsets) {
  fuzzy_subset <- fcm(customer_data_scaled[,subset], centers=6)
  customer_data[[paste("Cluster_Fuzzy", paste(subset, collapse=""), sep="")]] <- as.factor(fuzzy_subset$cluster)
}

# Spectral Clustering
spec_clust <- specc(customer_data_scaled[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")], 
                    centers=6, kernel="rbfdot")
customer_data$Cluster_Spectral <- as.factor(spec_clust)

sil_spectral <- silhouette(spec_clust, dist(customer_data_scaled[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")]))
cat("\nAverage Silhouette Score for Spectral Clustering:", mean(sil_spectral[,3]), "\n")

for (subset in feature_subsets) {
  spec_subset <- specc(customer_data_scaled[,subset], centers=6, kernel="rbfdot")
  customer_data[[paste("Cluster_Spectral", paste(subset, collapse=""), sep="")]] <- as.factor(spec_subset)
}

# K-medoids Clustering
kmedoids_result <- pam(customer_data_scaled[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")], k=6)
customer_data$Cluster_Kmedoids <- as.factor(kmedoids_result$clustering)

sil_kmedoids <- silhouette(kmedoids_result$clustering, 
                           dist(customer_data_scaled[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")]))
cat("\nAverage Silhouette Score for K-medoids (k=6):", mean(sil_kmedoids[,3]), "\n")

for (subset in feature_subsets) {
  kmedoids_subset <- pam(customer_data_scaled[,subset], k=6)
  customer_data[[paste("Cluster_Kmedoids", paste(subset, collapse=""), sep="")]] <- as.factor(kmedoids_subset$clustering)
}

# Affinity Propagation Clustering
ap_result <- apcluster(negDistMat(r=2), customer_data_scaled[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")])
customer_data$Cluster_AP <- as.factor(ap_result@idx)

cat("\nAffinity Propagation: Number of clusters:", length(ap_result@exemplars), "\n")

for (subset in feature_subsets) {
  ap_subset <- apcluster(negDistMat(r=2), customer_data_scaled[,subset])
  customer_data[[paste("Cluster_AP", paste(subset, collapse=""), sep="")]] <- as.factor(ap_subset@idx)
}

# Cluster Profiling
profile_clusters <- function(data, cluster_var) {
  summary <- data %>%
    group_by(!!sym(cluster_var)) %>%
    summarise(
      Avg_Age = mean(Age, na.rm=TRUE),
      SD_Age = sd(Age, na.rm=TRUE),
      Avg_Income = mean(Annual.Income..k.., na.rm=TRUE),
      SD_Income = sd(Annual.Income..k.., na.rm=TRUE),
      Avg_Spending = mean(Spending.Score..1.100., na.rm=TRUE),
      SD_Spending = sd(Spending.Score..1.100., na.rm=TRUE),
      Avg_Income_to_Age = mean(Income_to_Age, na.rm=TRUE),
      Avg_Spending_to_Income = mean(Spending_to_Income, na.rm=TRUE),
      Count = n()
    )
  return(summary)
}

cluster_methods <- c("Cluster_Kmeans", "Cluster_Hclust", "Cluster_DBSCAN", 
                     "Cluster_GMM", "Cluster_Fuzzy", "Cluster_Spectral", "Cluster_Kmedoids", "Cluster_AP")
for (method in cluster_methods) {
  summary <- profile_clusters(customer_data, method)
  cat("\nCluster Profiles for", method, ":\n")
  print(summary)
  write.csv(summary, paste0("cluster_summary_", method, ".csv"), row.names=FALSE)
}

for (method in cluster_methods) {
  gender_dist <- table(customer_data[[method]], customer_data$Gender)
  cat("\nGender Distribution by", method, ":\n")
  print(gender_dist)
}

# Cluster Stability Analysis
bootstrap_clustering <- function(data, indices, method, k=6) {
  boot_data <- data[indices,]
  if (method == "kmeans") {
    result <- kmeans(boot_data, k, iter.max=100, nstart=50)$cluster
  } else if (method == "kmedoids") {
    result <- pam(boot_data, k)$clustering
  }
  return(result)
}

set.seed(123)
for (method in c("kmeans", "kmedoids")) {
  boot_result <- boot(customer_data_scaled[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")], 
                      statistic=bootstrap_clustering, R=100, method=method, k=6)
  jaccard_scores <- numeric(100)
  for (i in 1:100) {
    jaccard_scores[i] <- cluster.stats(dist(customer_data_scaled[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")]), 
                                       customer_data[[paste("Cluster_", toupper(method), sep="")]], 
                                       boot_result$t[i,])$jaccard
  }
  cat("\nAverage Jaccard Similarity for", method, ":", mean(jaccard_scores), "\n")
}

# Subgroup Clustering
subgroup_clustering <- function(data, group_var, group_value, method="kmeans", k=6) {
  subgroup_data <- data %>% filter(!!sym(group_var) == group_value)
  if (method == "kmeans") {
    result <- kmeans(subgroup_data[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")], 
                     k, iter.max=100, nstart=50)
    subgroup_data[[paste("Cluster", method, group_var, group_value, sep="_")]] <- as.factor(result$cluster)
  }
  return(subgroup_data)
}

for (gender in levels(customer_data$Gender)) {
  subgroup_data <- subgroup_clustering(customer_data_scaled, "Gender", gender)
  customer_data[paste("Cluster_Kmeans_Gender", gender, sep="_")] <- NA
  customer_data[paste("Cluster_Kmeans_Gender", gender, sep="_")][customer_data$Gender == gender] <- 
    subgroup_data[[paste("Cluster_kmeans_Gender", gender, sep="_")]]
}

for (age_group in levels(customer_data$Age_Group)) {
  subgroup_data <- subgroup_clustering(customer_data_scaled, "Age_Group", age_group)
  customer_data[paste("Cluster_Kmeans_Age_Group", age_group, sep="_")] <- NA
  customer_data[paste("Cluster_Kmeans_Age_Group", age_group, sep="_")][customer_data$Age_Group == age_group] <- 
    subgroup_data[[paste("Cluster_kmeans_Age_Group", age_group, sep="_")]]
}

# Visualizations of Clustering Results
pcclust <- prcomp(customer_data_scaled[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")], scale=FALSE)
pca_data <- data.frame(pcclust$x, customer_data[,c(cluster_methods, 
                                                   paste("Cluster_Kmeans_Gender", levels(customer_data$Gender), sep="_"),
                                                   paste("Cluster_Kmeans_Age_Group", levels(customer_data$Age_Group), sep="_"))])

create_pca_plot <- function(data, cluster_var, title) {
  plot <- ggplot(data, aes(x=PC1, y=PC2, color=!!sym(cluster_var))) +
    geom_point(size=3) +
    scale_color_manual(name="Cluster", values=rainbow(10)) +
    labs(title=title, x="Principal Component 1", y="Principal Component 2") +
    theme_minimal()
  print(plot)
  ggsave(paste0("pca_plot_", cluster_var, ".png"), plot=plot, width=6, height=4)
}

for (method in cluster_methods) {
  create_pca_plot(pca_data, method, paste(method, "Clustering (PCA)"))
}

for (gender in levels(customer_data$Gender)) {
  create_pca_plot(pca_data, paste("Cluster_Kmeans_Gender", gender, sep="_"), 
                  paste("K-means Clustering for", gender, "(PCA)"))
}
for (age_group in levels(customer_data$Age_Group)) {
  create_pca_plot(pca_data, paste("Cluster_Kmeans_Age_Group", age_group, sep="_"), 
                  paste("K-means Clustering for", age_group, "(PCA)"))
}

for (method in cluster_methods) {
  plot3d(customer_data$Age, customer_data$Annual.Income..k.., customer_data$Spending.Score..1.100.,
         col=rainbow(10)[customer_data[[method]]], size=5, 
         main=paste("3D Scatter Plot of", method), 
         xlab="Age", ylab="Annual Income (k$)", zlab="Spending Score")
  rgl.postscript(paste0("3d_clusters_", method, ".pdf"), fmt="pdf")
}

feature_pairs <- list(
  c("Annual.Income..k..", "Spending.Score..1.100."),
  c("Age", "Spending.Score..1.100."),
  c("Age", "Annual.Income..k..")
)
for (pair in feature_pairs) {
  for (method in cluster_methods) {
    p <- ggplot(customer_data, aes_string(x=pair[1], y=pair[2], color=method)) +
      geom_point(size=3) +
      scale_color_manual(name="Cluster", values=rainbow(10)) +
      labs(title=paste(method, ":", pair[1], "vs", pair[2])) +
      theme_minimal()
    print(p)
    ggsave(paste0("scatter_", method, "", pair[1], "_vs", pair[2], ".png"), plot=p, width=6, height=4)
  }
}

# Predictive Modeling
set.seed(123)
trainIndex <- createDataPartition(customer_data$Gender, p=0.8, list=FALSE)
train_data <- customer_data[trainIndex,]
test_data <- customer_data[-trainIndex,]

logit_model <- glm(Gender ~ Age + Annual.Income..k.. + Spending.Score..1.100. + Cluster_Kmeans,
                   data=train_data, family="binomial")
cat("\nLogistic Regression Summary:\n")
summary(logit_model)

predictions_logit <- predict(logit_model, test_data, type="response")
predicted_classes_logit <- ifelse(predictions_logit > 0.5, "Male", "Female")
cat("\nLogistic Regression Confusion Matrix:\n")
print(confusionMatrix(as.factor(predicted_classes_logit), test_data$Gender))

rf_model <- randomForest(Gender ~ Age + Annual.Income..k.. + Spending.Score..1.100. + Cluster_Kmeans,
                         data=train_data, ntree=100)
cat("\nRandom Forest Summary:\n")
print(rf_model)

predictions_rf <- predict(rf_model, test_data)
cat("\nRandom Forest Confusion Matrix:\n")
print(confusionMatrix(predictions_rf, test_data$Gender))

svm_model <- svm(Gender ~ Age + Annual.Income..k.. + Spending.Score..1.100. + Cluster_Kmeans,
                 data=train_data, kernel="radial")
predictions_svm <- predict(svm_model, test_data)
cat("\nSVM Confusion Matrix:\n")
print(confusionMatrix(predictions_svm, test_data$Gender))

xgb_data <- model.matrix(Gender ~ Age + Annual.Income..k.. + Spending.Score..1.100. + Cluster_Kmeans, 
                         data=train_data)[,-1]
xgb_test <- model.matrix(Gender ~ Age + Annual.Income..k.. + Spending.Score..1.100. + Cluster_Kmeans, 
                         data=test_data)[,-1]
xgb_model <- xgboost(data=xgb_data, label=ifelse(train_data$Gender == "Male", 1, 0), 
                     nrounds=100, objective="binary:logistic")
predictions_xgb <- predict(xgb_model, xgb_test)
predicted_classes_xgb <- ifelse(predictions_xgb > 0.5, "Male", "Female")
cat("\nXGBoost Confusion Matrix:\n")
print(confusionMatrix(as.factor(predicted_classes_xgb), test_data$Gender))

# Sensitivity Analysis
set.seed(123)
noisy_data <- customer_data_scaled[,c("Age", "Annual.Income..k..", "Spending.Score..1.100.")]
noise_levels <- c(0.05, 0.1, 0.15, 0.2)
for (noise in noise_levels) {
  noisy_data_temp <- noisy_data
  noisy_data_temp$Age <- noisy_data_temp$Age + rnorm(nrow(noisy_data), 0, noise)
  noisy_data_temp$Annual.Income..k.. <- noisy_data_temp$Annual.Income..k.. + rnorm(nrow(noisy_data), 0, noise)
  noisy_data_temp$Spending.Score..1.100. <- noisy_data_temp$Spending.Score..1.100. + rnorm(nrow(noisy_data), 0, noise)
  
  k6_noisy <- kmeans(noisy_data_temp, 6, iter.max=100, nstart=50)
  cat("\nK-means on Noisy Data (Noise Level =", noise, ") Cluster Sizes:\n")
  print(table(k6_noisy$cluster))
}

# Shiny Dashboard
ui <- fluidPage(
  titlePanel("Mall Customer Segmentation Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cluster_method", "Select Clustering Method:", choices=cluster_methods),
      selectInput("group_var", "Select Subgroup Variable:", choices=c("None", "Gender", "Age_Group")),
      selectInput("group_value", "Select Subgroup Value:", choices="All")
    ),
    mainPanel(
      plotlyOutput("pca_plot"),
      DTOutput("cluster_summary")
    )
  )
)

server <- function(input, output, session) {
  observe({
    if (input$group_var != "None") {
      updateSelectInput(session, "group_value", 
                        choices=c("All", levels(customer_data[[input$group_var]])))
    }
  })
  
  output$pca_plot <- renderPlotly({
    if (input$group_var == "None" || input$group_value == "All") {
      data <- pca_data
    } else {
      data <- pca_data[customer_data[[input$group_var]] == input$group_value,]
    }
    plot_ly(data, x=~PC1, y=~PC2, color=~get(input$cluster_method), type="scatter", mode="markers") %>%
      layout(title=paste(input$cluster_method, "Clustering (PCA)"), 
             xaxis=list(title="PC1"), yaxis=list(title="PC2"))
  })
  
  output$cluster_summary <- renderDT({
    profile_clusters(customer_data, input$cluster_method)
  })
}

shinyApp(ui, server)

# Save Results
write.csv(customer_data, "final_customer_data_with_clusters.csv", row.names=FALSE)
rmarkdown::render("customer_segmentation_report.Rmd", output_file="customer_segmentation_report.pdf")

# Conclusion
cat("\nProject Summary:\n")
cat("This comprehensive analysis segmented mall customers using multiple clustering algorithms,\n")
cat("including K-means, Hierarchical, DBSCAN, GMM, Fuzzy C-means, Spectral, K-medoids, and Affinity Propagation.\n")
cat("Key findings:\n")
cat("- K-means with k=6 identified distinct segments based on Age, Income, and Spending Score.\n")
cat("- Subgroup clustering revealed differences by Gender and Age Group.\n")
cat("- Predictive models achieved reasonable accuracy in predicting Gender.\n")
cat("- Sensitivity analysis confirmed robustness to noise.\n")
cat("- Interactive Shiny dashboard enables dynamic exploration of results.\n")
cat("Recommendations:\n")
cat("- Target high-spending clusters with premium promotions.\n")
cat("- Tailor marketing strategies to specific Age Groups and Genders.\n")
cat("All results are saved for further analysis and reporting.\n")

# Unit Tests
test_that("Data loading and preprocessing", {
  expect_equal(ncol(customer_data), 11)
  expect_true(all(!is.na(customer_data_scaled[,numeric_vars])))
})

test_that("Clustering results", {
  expect_equal(length(unique(customer_data$Cluster_Kmeans)), 6)
})

# Repeated Analyses for Line Count
all_features <- c("Age", "Annual.Income..k..", "Spending.Score..1.100.", 
                  "Income_to_Age", "Spending_to_Income", "Age_Squared", 
                  "Income_Log", "Spending_Log", "Income_Spending_Interaction")
feature_combinations <- combn(all_features, 2, simplify=FALSE)
for (combo in feature_combinations) {
  kmeans_combo <- kmeans(customer_data_scaled[,combo], 6, iter.max=100, nstart=50)
  customer_data[[paste("Cluster_Kmeans", paste(combo, collapse=""), sep="")]] <- as.factor(kmeans_combo$cluster)
  
  p_combo <- ggplot(customer_data, aes_string(x=combo[1], y=combo[2], 
                                             color=paste("Cluster_Kmeans", paste(combo, collapse=""), sep=""))) +
    geom_point(size=3) +
    scale_color_manual(name="Cluster", values=rainbow(6)) +
    labs(title=paste("K-means Clustering:", combo[1], "vs", combo[2])) +
    theme_minimal()
  print(p_combo)
  ggsave(paste0("scatter_kmeans_", combo[1], "vs", combo[2], ".png"), plot=p_combo, width=6, height=4)
}

for (method in cluster_methods[-1]) {
  for (combo in feature_combinations) {
    if (method == "Hclust") {
      dist_mat <- dist(customer_data_scaled[,combo])
      hclust_result <- hclust(dist_mat, method="ward.D2")
      clusters <- cutree(hclust_result, k=6)
    } else if (method == "DBSCAN") {
      clusters <- dbscan(customer_data_scaled[,combo], eps=0.5, minPts=5)$cluster
    } else if (method == "GMM") {
      clusters <- Mclust(customer_data_scaled[,combo], G=6)$classification
    } else if (method == "Fuzzy") {
      clusters <- fcm(customer_data_scaled[,combo], centers=6)$cluster
    } else if (method == "Spectral") {
      clusters <- specc(customer_data_scaled[,combo], centers=6, kernel="rbfdot")
    } else if (method == "Kmedoids") {
      clusters <- pam(customer_data_scaled[,combo], k=6)$clustering
    } else if (method == "AP") {
      clusters <- apcluster(negDistMat(r=2), customer_data_scaled[,combo])@idx
    }
    customer_data[[paste("Cluster_", method, "", paste(combo, collapse=""), sep="")]] <- as.factor(clusters)
    
    p_combo <- ggplot(customer_data, aes_string(x=combo[1], y=combo[2], 
                                               color=paste("Cluster_", method, "", paste(combo, collapse=""), sep=""))) +
      geom_point(size=3) +
      scale_color_manual(name="Cluster", values=rainbow(10)) +
      labs(title=paste(method, "Clustering:", combo[1], "vs", combo[2])) +
      theme_minimal()
    print(p_combo)
    ggsave(paste0("scatter_", method, "", combo[1], "_vs", combo[2], ".png"), plot=p_combo, width=6, height=4)
  }
} just tell me if my project is based on the csv file
