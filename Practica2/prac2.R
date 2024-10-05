library(kernlab)
load("/home/mikel/Escritorio/KISA/analisisDatos/repo/ExploracionAnalisisDatos/Practica2/objetos.RData")
n <- 211
m <- 28

# APARTADO 1
mds <- cmdscale(d, eig=TRUE)
plot(mds$eig, type="b")
# Nos quedamos con las primeras 10 para entrender mejor el análisis
plot(mds$eig[1:10], type="b")
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
plot(x, type="n") 
text(x[,1], x[,2], 1:m)



# APARTADO 2
# Construcción de la matriz de kernel Gaussiano para sigma=0.2
sigma <- 0.2
K <- exp(-d^2 / (2 * sigma^2))

kernelpca <- kpca(~., data = as.data.frame(K), kernel = "rbfdot", kpar = list(sigma = sigma))
kpca_eigens <- eig(kernelpca)
plot(kpca_eigens, type="b")
abline(h=0, col="black", lty = 2)
kpca_eigens/sum(kpca_eigens)*100

# En este caso si cogemos las q=2 primeras componentes tendríamos que aportan un porcentaje de variabilidad de:
(kpca_eigens[1] + kpca_eigens[2])/sum(kpca_eigens)*100

# Calcular la matriz de doble centrado de las nuevas variables respecto a las originales
k_nuevos <- kernelMatrix(rbfdot(sigma=sigma), dnuevos, d)
media_global_nuevos <- mean(k_nuevos)
media_filas_nuevos <- rowMeans(k_nuevos)
media_columnas_nuevos <- colMeans(k_nuevos)

K_nuevos_centrado <- matrix(0, nrow=m, ncol=n)
for (i in 1:m) {
  for (j in 1:n) {
    K_nuevos_centrado[i, j] <- k_nuevos[i, j] - media_filas_nuevos[i] - media_columnas_nuevos[j] + media_global_nuevos
  }
}

x <- K_nuevos_centrado%*%pcv(kernelpca)[,1:2]
plot(x, type="n") 
text(x[,1], x[,2], 1:m)
