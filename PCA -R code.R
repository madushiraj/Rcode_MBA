getwd()
brainData = read.csv("Brain Size Data.csv", header = TRUE)
brainDataPCA = prcomp(brainData[2:7], center = TRUE, scale = TRUE) #Scale & center data summary(brainDataPCA)
screeplot(brainDataPCA, type="lines", cex.main = 0.75) #Draw a line plot
brainDataPCA
par(mfcol=c(1,2)) 
plot(brainDataPCA$x[,1:2]) 
plot(brainDataPCA$x[,2:3])
iris.pca = prcomp(iris[1:4], center = TRUE, scale = TRUE) 
iris.pca
summary(iris.pca)
screeplot(iris.pca, type="lines", cex.main = 0.5)
pairs(iris.pca$x)
kc.iris.pca = kmeans(iris.pca$x[,1:2], 3) 
plot(iris.pca$x[,1:2], col=kc.iris.pca$cluster)
table(iris$Species, kc.iris.pca$cluster)
iris.log = log(iris[, 1:4]) #Log transform the dataset 
iris.log.pca = prcomp(iris.log[1:4], center = TRUE, scale = TRUE) 
summary(iris.log.pca)
kc.iris.log.pca = kmeans(iris.log.pca$x[,1:2], 3)
par(mfcol=c(1,2))
plot(iris.log.pca$x[,1:2])
plot(iris.log.pca$x[,1:2], col=kc.iris.log.pca$cluster)
table(iris$Species, kc.iris.log.pca$cluster) #Compare k-means and data
wine = read.csv("wine.data") 
head(wine)
plot(wine[,2:14]) #Skip 1st column which is plant variety
wine.pca = prcomp(wine[,2:14], scale =TRUE, center = TRUE)
kc.wine.pca = kmeans(wine.pca$x[,1:2], 3) 
par(mfcol=c(1,2))
plot(wine.pca$x[,1:2])
plot(wine.pca$x[,1:2], col=kc.wine.pca$cluster) 
table(wine$Class, kc.wine.pca$cluster) #Compare PCA based k-means and data
kc.wine = kmeans(wine[,2:14], 3)
table(wine$Class, kc.wine$cluster) #Compare k-means and data
