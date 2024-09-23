# Cargar los paquetes necesarios
library(sf)
library(mapSpain)
library(ggplot2)


# Obtener mapa costas -----------------------------------------------------

mapa <- esp_get_prov()[esp_get_prov()$ine.prov.name %in% c("Araba/Álava", "Gipuzkoa", "Bizkaia"), ]


# Leer CSVs y transformar puntos ------------------------------------------

dat23 <- read.csv("samplePoints23.csv")
dat23$XETRS89 <- as.numeric(dat23$XETRS89)
dat23$YETRS89 <- as.numeric(dat23$YETRS89)

# dat13 <- read.csv("samplePoints13.csv")
# dat13$XETRS89 <- as.numeric(dat13$XETRS89)
# dat13$YETRS89 <- as.numeric(dat13$YETRS89)

### Función para transformar coordenadas en valores imprimibles

transformarCoordenadas <- function(dat)
{
  dat_sf <- st_as_sf(dat, coords = c("XETRS89", "YETRS89"), crs = 25830)  # 25830 es UTM zona 30N
  dat_etrs89 <- st_transform(dat_sf, crs = 4258)  # 4258 es European Terrestrial Reference System 1989
  dat_transformed <- as.data.frame(st_coordinates(dat_etrs89))
  colnames(dat_transformed) <- c("XETRS89", "YETRS89")
  dat$XETRS89 = dat_transformed$XETRS89
  dat$YETRS89 = dat_transformed$YETRS89
  return(dat)
}

dat23 <- transformarCoordenadas(dat23)
# dat13 <- transformarCoordenadas(dat13)

# ### ¿Existen los mismos puntos en 23 y 13?
# 
# setdiff(dat13$XETRS89, dat23$XETRS89) # Compara los valores de 13 sobre 23 (Muestra si hay alguno nuevo)
# setdiff(dat13$YETRS89, dat23$YETRS89)
# 
# setdiff(dat23$XETRS89, dat13$XETRS89) # Compara los valores de 23 sobre 13 (Muestra si hay alguno nuevo)
# setdiff(dat23$YETRS89, dat13$YETRS89)



# Obtener datos biomasa ---------------------------------------------------

dat23_bi <- read.csv("df_bi_23.csv")
dat23_bi <- dat23_bi[dat23_bi$Parameter %in% c("Abundancia (biomasa)", "Diversidad (biomasa)"), ]

### ¿Que puntos no se encuentran en los datos a usar?

setdiff(dat23$Code, dat23_bi$Sample.Point.Code)
setdiff(dat23_bi$Sample.Point.Code, dat23$Code)

### Normalizamos los datos

normalizarDatos <- function(dat)
{
  dat <- scale(dat, center=min(dat), scale=max(dat)-min(dat))
  return(dat)
}

dat23_bi$Value[dat23_bi$Parameter %in% c("Abundancia (biomasa)")] <- normalizarDatos(dat23_bi$Value[dat23_bi$Parameter %in% c("Abundancia (biomasa)")])
dat23_bi$Value[dat23_bi$Parameter %in% c("Diversidad (biomasa)")] <- normalizarDatos(dat23_bi$Value[dat23_bi$Parameter %in% c("Diversidad (biomasa)")])

# Imprimir plot -----------------------------------------------------------

# Crear el gráfico principal
main_plot <- ggplot() +
  geom_sf(data = mapa, fill = "lightblue", color = "black") +
  coord_sf(xlim = c(-3.09, -2.1), ylim = c(43.28, 43.65), expand = FALSE) +
  geom_point(data = dat23[dat23$Type == "LITORALES", ], 
             aes(x = XETRS89, y = YETRS89, color = "Abundancia", 
                 size = dat23_bi$Value[dat23_bi$Parameter == "Abundancia (biomasa)"])) +
  geom_point(data = dat23[dat23$Type == "LITORALES", ], 
             aes(x = XETRS89, y = YETRS89, color = "Diversidad", 
                 size = dat23_bi$Value[dat23_bi$Parameter == "Diversidad (biomasa)"]), shape = 21) +
  scale_size_continuous(name = "Tamaño", range = c(0.5, 5)) +
  scale_color_manual(name = "Variable", 
                     values = c("Abundancia" = "red", "Diversidad" = "green")) +
  theme(legend.position = "top", 
        legend.title = element_text(colour = "black", size = 10, face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  guides(size = "none") +
  labs(title = "Biomasa en la costa", x = "Longitud", y = "Latitud",
       caption = "Fuente: mapSpain")

secondary_plot <- ggplot() +
  geom_sf(data = mapa, fill = "lightblue", color = "black") +
  coord_sf(xlim = c(-2., -1.79), ylim = c(43.28, 43.42), expand = FALSE) +
  geom_point(data = dat23[dat23$Type == "LITORALES", ], 
             aes(x = XETRS89, y = YETRS89, color = "Abundancia", 
                 size = dat23_bi$Value[dat23_bi$Parameter == "Abundancia (biomasa)"])) +
  geom_point(data = dat23[dat23$Type == "LITORALES", ], 
             aes(x = XETRS89, y = YETRS89, color = "Diversidad", 
                 size = dat23_bi$Value[dat23_bi$Parameter == "Diversidad (biomasa)"]), shape = 21) +
  scale_size_continuous(name = "Tamaño", range = c(0.5, 5)) +
  scale_color_manual(name = "Variable", 
                     values = c("Abundancia" = "red", "Diversidad" = "green"))+
  theme_void() +  # Sin título ni leyenda
  theme(legend.position = "none",
        axis.title = element_blank(),   # Eliminar títulos de ejes
        axis.text = element_text(size = 8),  # Ajustar tamaño de texto de ejes si es necesario
        axis.ticks = element_line(),
        panel.grid.major = element_line(color = "grey", size = 0.5),  # Cuadrícula mayor
        panel.grid.minor = element_line(color = "lightgrey", size = 0.25),
        plot.background = element_rect(fill = "white", color = "black", size = 1)) +  # Mantener las marcas de los ejes
  scale_x_continuous(breaks = c(-2, -1.8)) +  # Mostrar solo los extremos y el medio
  scale_y_continuous(breaks = c(43.45, 43.4, 43.35, 43.3))

# Añadir el gráfico secundario al gráfico principal
final_plot <- main_plot +
  annotation_custom(
    grob = ggplotGrob(secondary_plot),
    xmin = -3, xmax = -1.7, ymin = 43.4, ymax = 43.65  # Ajusta estos límites según sea necesario
  )

# Mostrar el gráfico final
print(final_plot)

# coord_sf(xlim = c(-3.45, -1.7), ylim = c(43.13, 43.6), expand = FALSE)


# ggplot() +
#   geom_sf(data = mapa, fill = "lightblue", color = "black") +  # Mapa de las provincias
#   coord_sf(xlim = c(-3.09, -1.79), ylim = c(43.13, 43.6), expand = FALSE) +  # Limitar el mapa a la región costera
#   geom_point(data = dat23[dat23$Type == "LITORALES", ], 
#              aes(x = XETRS89, y = YETRS89, color = "Abundancia", 
#                  size = dat23_bi$Value[dat23_bi$Parameter == "Abundancia (biomasa)"])) +
#   geom_point(data = dat23[dat23$Type == "LITORALES", ], 
#              aes(x = XETRS89, y = YETRS89, color = "Diversidad", 
#                  size = dat23_bi$Value[dat23_bi$Parameter == "Diversidad (biomasa)"]), shape = 21) +
#   scale_size_continuous(name = "Tamaño", range = c(0.5, 5))+
#   scale_color_manual(name = "Variable", 
#                      values = c("Abundancia" = "red", "Diversidad" = "green")) +  # Colores personalizados
#   theme(legend.position = "top", 
#         legend.title = element_text(colour = "black", size = 10, face = "bold"),
#         plot.title = element_text(hjust = 0.5)) +
#   guides(size = "none") +
#   labs(title = "Biomasa en la costa", x = "Longitud", y = "Latitud",
#        caption = "Fuente: mapSpain")
