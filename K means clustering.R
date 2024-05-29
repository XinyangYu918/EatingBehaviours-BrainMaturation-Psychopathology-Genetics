#### K-means clustering, based on TFEQ questionnaires collected at age 23
library(factoextra)
library("NbClust")

# Read and rescale data
data <- read.csv("./imagen_tfeq.csv")
data <- data[,-c(1,1)]
means <- apply(data,2,mean)
sds <- apply(data,2,sd)
nor <- scale(data,center = means,scale = sds)

# Calculate distance
distance = dist(nor)

# Apply kmeans and plot wss
fviz_nbclust(nor,kmeans,method = 'wss')
wss <- (nrow(nor)-1)*sum(apply(nor,2,var))
for (i in 2:15)
  wss[i] <- sum(kmeans(nor,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

# Use nbclust to determine optimal number of cluster and plot
res <- NbClust(nor, distance = "euclidean", min.nc = 3, max.nc = 8, method = "kmeans", index = "all")
res$All.index
res$Best.nc
res$All.CriticalValues
res$Best.partition

write.csv(res$All.index, file = "all index.csv")
capture.output(res, file = "./res.txt")

# Plot kmeans clustering results
km <- kmeans(nor, 3, nstart = 25)
print(km)
fviz_cluster(km, data = nor, palette = c("#E7B800", "#00AFBB", "#2E9FDF", "#FC4E07"), 
             ellipse.type = "euclid", star.plot = TRUE,
             repel = TRUE, ggtheme = theme_minimal())

# Save centers and clusters results
write.csv(km$centers,"centers_imagen.csv")
write.csv(km$cluster,"cluster_imagen.csv")
