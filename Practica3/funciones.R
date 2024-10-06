plot_dendogramas <- function(d){
  metodos <- c("single", "complete", "average", "ward.D2")
  titulos <- c("Dendrograma - Mínimo (Single)",
               "Dendrograma - Máximo (Complete)",
               "Dendrograma - Média de grupos (Average)",
               "Dendrograma - Ward.D2")
  
  par(mfrow = c(2, 2)) # Pongo gráfica 2x2
  
  for (i in 1:length(metodos)) {
    hc <- hclust(d, method = metodos[i])
    plot(hc, main = titulos[i], hang = -0.1, sub = "", xlab = "Ciudades", ylab = "Distancia")
  }
  
  par(mfrow = c(1, 1)) # Restauro gráfica a 1x1
}





plot_shillouetes <- function(metodo, X, d) {
  mean_shil = c()
  
  for (k in 2:10) {
    if (identical(metodo, "specc")) {
      clusters <- specc(X, centers = k, kernel = "polydot")
      sil <- silhouette(clusters@.Data, d)
    } else {
      if (identical(metodo, "pam")) {
        clusters <- pam(X, k, metric = "euclidean")
      } else {
        clusters <- kmeans(X, centers = k)
      }
      sil <- silhouette(clusters$cluster, d)
    }
    mean_shil <- append(mean_shil, mean(sil[, 3]))
  }
  
  plot1 <- ggplot(data = data.frame(k = 2:10, mean_shil = mean_shil), aes(x = k, y = mean_shil)) +
    geom_line() +
    geom_point() +
    labs(x = "Número de Clusters", y = "Silhouette promedio", 
         title = "Valor silhouette promedio por clusters") +
    scale_x_continuous(breaks = 2:10)
  
  if (identical(metodo, "specc")) {
    sil <- silhouette(specc(X, centers = which.max(mean_shil) + 1)@.Data, d)
  } else if (identical(metodo, "pam")) {
    sil <- silhouette(pam(X, which.max(mean_shil) + 1)$cluster, d)
  } else {
    sil <- silhouette(kmeans(X, centers = which.max(mean_shil) + 1)$cluster, d)
  }
  plot2 <- fviz_silhouette(sil) + 
    ggtitle("Silueta de los clusters")
  
  grid.arrange(plot1, plot2, ncol = 2) # Usar grid.arrange para mostrar ambos gráficos
  
  par(mfrow = c(1, 1)) # Restauro gráfica a 1x1
}


