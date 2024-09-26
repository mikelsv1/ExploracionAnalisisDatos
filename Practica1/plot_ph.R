library(ggplot2)
library(dplyr)

# Cargar los datos
data23 <- read.csv("/home/mikel/Escritorio/KISA/analisisDatos/repo/ExploracionAnalisisDatos/Practica1/df_fq_23.csv")
data23$Date <- as.Date(data23$Date, format = "%d/%m/%Y")
data23$Month <- format(data23$Date, "%m")
ph_data23 <- subset(data23, Parameter == "pH")
ph_data23$Value <- as.numeric(ph_data23$Value)

ph_means23 <- ph_data23 %>%
  group_by(Month, Sample.Point.Code) %>%
  summarise(MeanValue23 = mean(Value))

ph_overall_means23 <- ph_data23 %>%
  group_by(Month) %>%
  summarise(OverallMean23 = mean(Value))

data13 <- read.csv("/home/mikel/Escritorio/KISA/analisisDatos/repo/ExploracionAnalisisDatos/Practica1/df_fq_13.csv")
data13$Date <- as.Date(data13$Date, format = "%d/%m/%Y")
data13$Month <- format(data13$Date, "%m")
ph_data13 <- subset(data13, Parameter == "pH")
ph_data13$Value <- as.numeric(ph_data13$Value)

ph_means13 <- ph_data13 %>%
  group_by(Month, Sample.Point.Code) %>%
  summarise(MeanValue13 = mean(Value))

ph_overall_means13 <- ph_data13 %>%
  group_by(Month) %>%
  summarise(OverallMean13 = mean(Value))

# Concatenar el año a Sample.Point.Code por si se quiere pone los dos años
# ph_means23$Sample.Point.Code <- paste(ph_means23$Sample.Point.Code, "2023", sep = "_")
# ph_means13$Sample.Point.Code <- paste(ph_means13$Sample.Point.Code, "2013", sep = "_")

month_labels <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                  "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

colors <- c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', 
            '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', 
            '#008080', '#e6beff', '#9a6324', 'grey30', '#800000', 
            '#aaffc3')

# Unir los datos para obtener los nombres de las zonas (año 2032)
sample_points <- read.csv("/home/mikel/Escritorio/KISA/analisisDatos/repo/ExploracionAnalisisDatos/Practica1/samplePoints23.csv")
ph_means23 <- ph_means23 %>%
  left_join(sample_points, by = c("Sample.Point.Code" = "Code"))

# Crear una nueva columna para la leyenda
ph_means23 <- ph_means23 %>%
  mutate(LegendLabel = paste(Sample.Point.Code, ", ", Sample.Point, sep = ""))

ggplot() +
  
  geom_smooth(data = ph_means23, aes(x = as.numeric(Month), y = MeanValue23, group = LegendLabel, color = LegendLabel),
              method = "loess", se = FALSE, size = 0.45) +
  
  stat_smooth(data = ph_overall_means23, aes(x = as.numeric(Month), y = OverallMean23),
              method = "loess", se = TRUE, level = 0.95, fill="grey60", color = "red", size = 1.45, linetype = "solid") +
  
  annotate("text", x = 12, y = 8.21, label = "Media 2023", 
           color = "red", vjust = -0.5, hjust = 1, size = 5) +
  
  # geom_smooth(data = ph_means13, aes(x = as.numeric(Month), y = MeanValue13, group = Sample.Point.Code, color = Sample.Point.Code),
  #             method = "loess", se = FALSE, size = 0.45) +
  # 
  # stat_smooth(data = ph_overall_means13, aes(x = as.numeric(Month), y = OverallMean13),
  #             method = "loess", se = TRUE, level = 0.95, color = "#1f8022", size = 1, linetype = "solid") +
  # 
  # annotate("text", x = 8, y = 8.25, label = "Media 2013", 
  #          color = "#1f8022", vjust = -0.5, hjust = 1, size = 5) +
  
  labs(title = "Media de pH por mes y zona de recogida",
       x = "Mes",
       y = "Nivel de pH") +
  
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Helvetica", color = "grey30"),
    axis.text.x = element_text(angle = 15, hjust = 1, color = "grey50"),
    axis.text.y = element_text(color = "grey50"),
    axis.title.y = element_text(margin = margin(r = 20)),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_x_continuous(breaks = seq(1, 12, 1), labels = month_labels) +
  scale_color_manual(values = colors, name="Zona de recogida") +
  geom_text(aes(x = 5, y=8.82, 
            label = "El área sombreada alrededor de la línea de la media muestra el intervalo de confianza al 95%."), 
            vjust = 1.5, size = 4, color = "grey30", 
            position = position_nudge(y = -0.5), 
            hjust = 0)


# El área sombreada alrededor de la línea 
# de la media muestra el intervalo de confianza (IC) en el que se 
# estima que se encuentre la verdadera media poblacional con un 95% 
# de certeza
