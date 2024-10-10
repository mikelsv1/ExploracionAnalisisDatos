
load("/home/mikel/Escritorio/KISA/analisisDatos/repo/ExploracionAnalisisDatos/Practica2/objetos.RData")
n <- 211
m <- 28

# APARTADO 1
mds <- cmdscale(d, eig=TRUE)
plot(mds$eig, type="b", ylab="Valores propios", xlab="Componente")
# Nos quedamos con las primeras 10 para entrender mejor el análisis
plot(mds$eig[1:10], type="b", ylab="Valores propios", xlab="Componente")
abline(h=0, col="black", lty = 2)
mds$eig[1:10]/sum(mds$eig[1:10])*100 # porcentaje de variabilidad que aporta cada componente

# Determinamos q = 2 tras ver el plot anterior, y el Porcentaje de variabilidad que aporta cada componente
# CUIDADO algunos de estos valores propios son negativo por lo que la matriz D no es euclidea
# Decidimos ignorar los vp negativos y calculamos la precisión de la aproximación como (diapositiva 53):
sum(mds$eig[1], mds$eig[2])/sum(abs(mds$eig))*100 # Porcentaje de variabilidad que aporta las primeras 2 compoentes 

# Calculamos la proyección del nuevo individuo sobre el nuevo espacio construido
eig_matrix <- matrix(c(mds$eig[1], 0,
                0, mds$eig[2]), nrow=2, byrow=T) # colocamos los vp en una matriz para operar más facilmente

A <- -0.5 * d^2
H <- diag(n) - (1/n) * matrix(1, n, n)
b <- diag(H %*% A %*% H)

# Calcular las coordenadas para los nuevos individuos
x <- 0.5*solve(eig_matrix)%*%t(mds$points[,1:2])%*%(b-t(dnuevos)**2)

# Representarlos en el espacio
x <- t(x) # poner las coordenadas como columnas de la matriz

plot(x[,1], x[,2], type="n", 
     xlim = range(c(x[,1], mds$points[,1])), 
     ylim = range(c(x[,2], mds$points[,2])))
text(x[,1], x[,2], 1:m)
points(mds$points[,1:2], pch=16, col = rainbow(length(unique(clase)), alpha = 0.5)[as.factor(clase)])
legend("topright", legend = unique(clase), col = rainbow(length(unique(clase)), alpha = 0.5), pch = 16, cex=0.5)

# MEDIDAS DE BONDAD
# Mediad global
plot(cor(dnuevos,x))
# Medida local
K <- 5
porcentajes <- numeric(nrow(dnuevos))
dist_original <- as.matrix(dist(dnuevos))
dist_proyeccion <- as.matrix(dist(x))

for (i in 1:nrow(dnuevos)) {
  vecinos_original_total <- dist_original[i, ]
  vecinos_original_total[i] <- NA  # Ponemos NA para no considerar la distancia a sí mismo
  vecinos_original <- order(vecinos_original_total)[1:K]
  
  vecinos_proyeccion_total <- dist_proyeccion[i, ]
  vecinos_proyeccion_total[i] <- NA  # Ponemos NA para no considerar la distancia a sí mismo
  vecinos_proyeccion <- order(vecinos_proyeccion_total)[1:K]
  
  # Comparar los vecinos y contar las coincidencias
  vecinos_comunes <- length(intersect(vecinos_original, vecinos_proyeccion))
  
  # Calcular el porcentaje de coincidencias
  porcentajes[i] <- (vecinos_comunes / K) * 100
}
barplot(porcentajes, names.arg = 1:length(porcentajes), cex.names=0.8,
        xlab = "Nuevos individuos", ylab = "Porcentaje (%)", col="#4a8cc7")
abline(h=mean(porcentajes), col="red", lty=2)
text(x=3, y=90, "Mean", col="red")


# APARTADO 2
# Construcción de la matriz de kernel Gaussiano para sigma=0.2
sigma <- 0.2
K <- exp(-d^2 / (2 * sigma^2))
H <- diag(n) - matrix(1, n, n) / n
K_centered <- H %*% K %*% H
eig <- eigen(K_centered)
kpca_eigens <- eig$values
kpca_vectors <- eig$vectors
kernel_comps <- K_centered %*% kpca_vectors

# kernelpca <- kpca(~., data = as.data.frame(K), kernel = "rbfdot", kpar = list(sigma = sigma))
# kpca_eigens <- eig(kernelpca)
plot(kpca_eigens, type="b")
abline(h=0, col="black", lty = 2)
kpca_eigens/sum(kpca_eigens)*100

# En este caso si cogemos las q=2 primeras componentes tendríamos que aportan un porcentaje de variabilidad de:
(kpca_eigens[1] + kpca_eigens[2])/sum(kpca_eigens)*100

# Calcular la matriz de doble centrado de las nuevas variables respecto a las originales
k_nuevos <- exp(-dnuevos^2 / (2 * sigma^2))
media_global_nuevos <- mean(k_nuevos)
media_filas_nuevos <- rowMeans(k_nuevos)
media_columnas_nuevos <- colMeans(k_nuevos)

K_nuevos_centrado <- matrix(0, nrow=m, ncol=n)
for (i in 1:m) {
  for (j in 1:n) {
    K_nuevos_centrado[i, j] <- k_nuevos[i, j] - media_filas_nuevos[i] - media_columnas_nuevos[j] + media_global_nuevos
  }
}

dat_orig_proyectados <- K_centered %*% kpca_vectors
x <- K_nuevos_centrado%*%kpca_vectors[,1:2]

plot(x[,1], x[,2], type="n", 
     xlim = range(c(x[,1], dat_orig_proyectados[,1])), 
     ylim = range(c(x[,2], dat_orig_proyectados[,2])))
text(x[,1], x[,2], 1:m)
points(dat_orig_proyectados[,1:2], pch=16, col = rainbow(length(unique(clase)), alpha = 0.5)[as.factor(clase)])
legend("topright", legend = unique(clase), col = rainbow(length(unique(clase)), alpha = 0.5), pch = 16, cex=0.5)

# MEDIDAS DE BONDAD
# Mediad global
plot(cor(dnuevos,x))
# Medida local
K <- 5
porcentajes <- numeric(nrow(dnuevos))
dist_original <- as.matrix(dist(dnuevos))
dist_proyeccion <- as.matrix(dist(x))

for (i in 1:nrow(dnuevos)) {
  vecinos_original_total <- dist_original[i, ]
  vecinos_original_total[i] <- NA  # Ponemos NA para no considerar la distancia a sí mismo
  vecinos_original <- order(vecinos_original_total)[1:K]
  
  vecinos_proyeccion_total <- dist_proyeccion[i, ]
  vecinos_proyeccion_total[i] <- NA  # Ponemos NA para no considerar la distancia a sí mismo
  vecinos_proyeccion <- order(vecinos_proyeccion_total)[1:K]
  
  # Comparar los vecinos y contar las coincidencias
  vecinos_comunes <- length(intersect(vecinos_original, vecinos_proyeccion))
  
  # Calcular el porcentaje de coincidencias
  porcentajes[i] <- (vecinos_comunes / K) * 100
}
barplot(porcentajes, names.arg = 1:length(porcentajes), cex.names=0.8,
        xlab = "Variables", ylab = "Porcentaje (%)", col="#4a8cc7")
abline(h=mean(porcentajes), col="red", lty=2)
text(x=10, y=90, "Mean", col="red")
