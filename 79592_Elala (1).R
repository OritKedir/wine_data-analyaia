# []===========================[]
# 2,Wine Dataset Clustering
# 79592_Elala.R
# []===========================[]

# [[[[[[]]]]]]
install.packages("tidyverse")
install.packages("cluster")
install.packages("factoextra")
install.packages("dendextend")
install.packages("NbClust")
install.packages("ggplot2")
install.packages("dplyr")

# Load required libraries  
library(tidyverse)  
library(cluster)  
library(factoextra)  
library(dendextend)  
library(NbClust) 
library(ggplot2)
library(dply)
# 1,Load the dataset  
# R script download the Wine dataset directly from the internet 
wine <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", header = FALSE)

#Add column names
colnames(wine) <- c("Class", "Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash", 
                    "Magnesium", "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", 
                    "Proanthocyanins", "Color_intensity", "Hue", 
                    "OD280_OD315", "Proline")


# 2,Provide the summary  
summary(wine)  

# 3,Show the first 8 observations  
head(wine, 8)  

# 4,Check for missing values and impute/delete if necessary  
cat("Total missing values:", sum(is.na(wine)), "\n")  
colSums(is.na(wine))  

# 5,Select only numerical variables and exclude the class label (if numeric)  
wine_data <- wine[, sapply(wine, function(x) typeof(x) != "integer")] 

str(wine_data)
# 6,Normalize data  
wine_scaled <- scale(wine_data)  

Wine_scaled <- as.data.frame(Wine_scaled)

# 7,Find the optimal number of clusters using the Elbow Method  
fviz_nbclust(wine_scaled, kmeans, method = "wss") +  
  ggtitle("Elbow Method for Optimal Clusters")  

# 8,Apply Hierarchical Clustering & k-means with the same number of clusters 
Wine_scaled <- as.data.frame(Wine_scaled)
set.seed(123)  # For reproducibility  
km <- kmeans(wine_scaled, centers = 3, nstart = 25)  

# Display cluster centers  
print("K-Means Cluster Centers:")  
print(km$centers)  

# Hierarchical Clustering  
dist_matrix <- dist(wine_scaled)  
hc <- hclust(dist_matrix, method = "ward.D2")  



# Plot dendrogram  
plot(hc, cex = 0.6, hang = -1, main = "Dendrogram - Hierarchical Clustering")  
rect.hclust(hc, k = 3, border = 2:4)  

# Cut dendrogram into 3 clusters  
hc_clusters <- cutree(hc, k = 3)  

# 9,Perform PCA visualization (fviz_pca_biplot())  
pca_res <- prcomp(wine_scaled)  

# K-Means PCA plot  
fviz_pca_biplot(pca_res, label = "var", habillage = km$cluster,  
                addEllipses = TRUE, title = "PCA - K-Means Clustering")  

# Hierarchical Clustering PCA plot  
fviz_pca_biplot(pca_res, label = "var", habillage = as.factor(hc_clusters),  
                addEllipses = TRUE, title = "PCA - Hierarchical Clustering")  

# 10,Compare clustering results using table()  
cat("Comparison with K-Means Clusters:\n")  
print(table(KMeans = km$cluster, Actual = wine$Class))  

cat("confusion matrix Hierarchical Clusters:\n")  
print(table(Hierarchical = hc_clusters, Actual = wine$Class))






#end

