library(cluster)
library(factoextra)
library(kernlab)
library(gridExtra)
library(ggdendro)
source("funciones.R")

X <- read.csv("indicedelitos.csv", sep=" ")
X <- scale(X)

# Creación clusters -------------------------------------------------------

d_euclidean <- dist(X, method="euclidean")
d_correlation <- as.dist(sqrt(1- cor(t(X), method = "pearson")))


# 1. Dis. Euclidea
#   1.1. Metodos jerarquicos (Sale a partir de matriz distancias)
#     1.1.1 Dendograma  - Ultrametrica
#       1.1.1.1 Single
          single_euclidean <- plot_dendogramas("single", d_euclidean)
#       1.1.1.1 Complete
          complete_euclidean <- plot_dendogramas("complete", d_euclidean)
#       1.1.1.1 Average
          average_euclidean <- plot_dendogramas("average", d_euclidean)
#       1.1.1.1 Ward
          ward_euclidean <- plot_dendogramas("ward.D2", d_euclidean)

#   1.2. Metodos particionales (No hace falta matriz distancias)
#     1.2.1. K-Means (Centros inventados)
      kmeans_euclidean <- plot_shillouetes("kmeans", X, d_euclidean)
#     1.2.2. PAM (Centros son puntos reales)
      pam_euclidean <- plot_shillouetes("pam", X, d_euclidean)
#     1.2.3. Clustering espacial (Kernels)
      specc_euclidean <- plot_shillouetes("specc", X, d_euclidean)
      
      
# 2. Dis. Correlación
#   2.1. Metodos jerarquicos (Sale a partir de matriz distancias)
#     2.1.1 Dendograma  - Ultrametrica
#       1.1.1.1 Single
      single_correlation <- plot_dendogramas("single", d_correlation)
#       1.1.1.1 Complete
      complete_correlation <- plot_dendogramas("complete", d_correlation)
#       1.1.1.1 Average
      average_correlation <- plot_dendogramas("average", d_correlation)
#       1.1.1.1 Ward
      ward_correlation <- plot_dendogramas("ward.D2", d_correlation)
      
#   2.2. Metodos particionales (No hace falta matriz distancias)
#     2.2.1. K-Means (Centros inventados)
      kmeans_correlation <- plot_shillouetes("kmeans", X, d_correlation)
#     2.2.2. PAM (Centros son puntos reales)
      # plot_shillouetes("pam", X, d_correlation) // No se puede ya que las metricas unicamente son euclidean y manhattan
#     2.2.3. Clustering espacial (Kernels)
      specc_correlation <- plot_shillouetes("specc", X, d_correlation)


# Representación clusters -------------------------------------------------
      
### Creación PCA
pca <- prcomp(X)
plot(pca)

# COMUNILADES (¿Estan bien representadas las variables?)
C <- pca$x[, 1:2]
r <- cor(X,C)
com <- apply(r, 1, function(x)(sum(x**2)))
o <- order(com, decreasing = TRUE)
barplot(com[o], ylim=c(0,1), cex.names=0.6, las=2)

# ¿Estan bien representados los individuos?
v2i <- apply(C, 1, function(x)(sum(x*x)))  # El 1 indica que es para cada fila
vti <- apply(X, 1, function(x)(sum(x*x))) # El 1 indica que es para cada fila
cos2 <-v2i/vti
o <- order(cos2, decreasing = TRUE)
barplot(cos2[o], ylim=c(0,1), cex.names=0.6, las=2)

# EUCLIDEA - JERARQUICA
par(mfrow = c(2, 2))
plot(pca$x[,1], pca$x[,2], col=single_euclidean, main="Single")
plot(pca$x[,1], pca$x[,2], col=complete_euclidean, main="Complete")
plot(pca$x[,1], pca$x[,2], col=average_euclidean, main="Average")
plot(pca$x[,1], pca$x[,2], col=ward_euclidean, main="Ward")
par(mfrow = c(1, 1))

# EUCLIDEA - PARTICIONAL
par(mfrow = c(2, 2))
plot(pca$x[,1], pca$x[,2], col=3-kmeans_euclidean$cluster, main="K-Means")
plot(pca$x[,1], pca$x[,2], col=pam_euclidean$cluster, main="PAM")
plot(pca$x[,1], pca$x[,2], col=3-specc_euclidean@.Data, main="Clustering Spectral")
par(mfrow = c(1, 1))


# CORRELACION - JERARQUICA
par(mfrow = c(2, 2))
plot(pca$x[,1], pca$x[,2], col=single_correlation, main="Single")
plot(pca$x[,1], pca$x[,2], col=complete_correlation, main="Complete")
plot(pca$x[,1], pca$x[,2], col=average_correlation, main="Average")
plot(pca$x[,1], pca$x[,2], col=ward_correlation, main="Ward")
par(mfrow = c(1, 1))

# CORRELACION - PARTICIONAL
par(mfrow = c(1, 2))
plot(pca$x[,1], pca$x[,2], col=kmeans_correlation$cluster, main="K-Means")
plot(pca$x[,1], pca$x[,2], col=specc_correlation@.Data, main="Clustering Spectral")
par(mfrow = c(1, 1))
