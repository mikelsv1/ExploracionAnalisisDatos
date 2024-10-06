plot_dendogramas <- function(metodo, d){
  mean_shil = c()
  
  hc <- hclust(d, metodo)
  
  ggdendro_data <- dendro_data(hc)
  
  plot1 <- ggplot() +
    geom_segment(data = ggdendro_data$segments, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_text(data = ggdendro_data$labels, aes(x = x, y = 0, label = label), angle = 90, hjust = 1, vjust = 0.5) +
    labs(title = "Dendograma", x = "Ciudades", y = "Distancia") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  for (k in 2:10) {
    clusters <- cutree(hc, k)
    sil <- silhouette(clusters, d)
    mean_shil <- append(mean_shil, mean(sil[, 3]))
  }
  
  plot2 <- ggplot(data = data.frame(k = 2:10, mean_shil = mean_shil), aes(x = k, y = mean_shil)) +
    geom_line() +
    geom_point() +
    labs(x = "Número de Clusters", y = "Silhouette promedio", 
         title = "Valor silhouette promedio por clusters") +
    scale_x_continuous(breaks = 2:10)
  
  best_model <- cutree(hc, which.max(mean_shil) + 1)
  sil <- silhouette(best_model, d)
  
  plot3 <- fviz_silhouette(sil) + 
    ggtitle("Silueta de los clusters")
  
  grid.arrange(plot1, plot2, plot3, ncol = 2)
  
  return(best_model)
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
  
  best_model <- NULL
  
  if (identical(metodo, "specc")) {
    best_model <- specc(X, centers = which.max(mean_shil) + 1)
    sil <- silhouette(best_model@.Data, d)
  } else if (identical(metodo, "pam")) {
    best_model <- pam(X, which.max(mean_shil) + 1)
    sil <- silhouette(best_model$cluster, d)
  } else {
    best_model <- kmeans(X, centers = which.max(mean_shil) + 1)
    sil <- silhouette(best_model$cluster, d)
  }
  plot2 <- fviz_silhouette(sil) + 
    ggtitle("Silueta de los clusters")
  
  grid.arrange(plot1, plot2, ncol = 2) # Usar grid.arrange para mostrar ambos gráficos
  
  return(best_model)
}
