library(cluster)
library(factoextra)
library(kernlab)
library(gridExtra)  
source("funciones.R")

X <- read.csv("indicedelitos.csv", sep=" ")
X <- scale(X)

d_pearson <- as.dist(sqrt(1- cor(t(X), method = "pearson")))
d_euclidean <- dist(X, method="euclidean")

# 1. Dis. Euclidea
#   1.1. Metodos jerarquicos (Sale a partir de matriz distancias)
#     1.1.1 Dendograma  - Ultrametrica
      plot_dendogramas(d_euclidean)
      
      #part1 <- cutree(hc, k=4) # guardar en 3 particiones
      #plot(d)
      #points(d, col=part1) # no parece muy buena partición, habría que probar con otra distancia

#   1.2. Metodos particionales (No hace falta matriz distancias)
#     1.2.1. K-Means (Centros inventados)
      plot_shillouetes("kmeans", X, d_euclidean)
#     1.2.2. PAM (Centros son puntos reales)
      plot_shillouetes("pam", X, d_euclidean)
#     1.2.3. Clustering espacial (Kernels)
      plot_shillouetes("specc", X, d_euclidean)
# 2. Dis. Correlación
#   2.1. Metodos jerarquicos (Sale a partir de matriz distancias)
#     2.1.1 Dendograma  - Ultrametrica
      plot_dendogramas(d_pearson)
#   2.2. Metodos particionales (No hace falta matriz distancias)
#     2.2.1. K-Means (Centros inventados)
      plot_shillouetes("kmeans", X, d_pearson)
#     2.2.2. PAM (Centros son puntos reales)
      # plot_shillouetes("pam", X, d_pearson) // No se puede ya que las metricas unicamente son euclidean y manhattan
#     2.2.3. Clustering espacial (Kernels)
      plot_shillouetes("specc", X, d_pearson)
      