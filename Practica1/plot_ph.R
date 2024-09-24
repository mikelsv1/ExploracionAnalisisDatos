library(ggplot2)
library(dplyr)

data = read.csv("/home/mikel/Escritorio/KISA/analisisDatos/repo/ExploracionAnalisisDatos/Practica1/df_fq_23.csv")
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
data$Month <- format(data$Date, "%m")
ph_data <- subset(data, Parameter == "pH")
ph_data$Value <- as.numeric(ph_data$Value)

ph_means <- ph_data %>%
  group_by(Month, Sample.Point.Code) %>%
  summarise(MeanValue = mean(Value))

ph_overall_means <- ph_data %>%
  group_by(Month) %>%
  summarise(OverallMean = mean(Value))

month_labels <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                  "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

ggplot() +

  geom_smooth(data = ph_means, aes(x = as.numeric(Month), y = MeanValue, group = Sample.Point.Code),
              se = FALSE, color = "grey70", size = 0.5) +
  
  geom_smooth(data = ph_overall_means, aes(x = as.numeric(Month), y = OverallMean),
              se = FALSE, color = "red", size = 1, linetype = "solid") +
  
  annotate("text", x = 12, y = 8.21, label = "Media", 
           color = "red", vjust = -0.5, hjust = 1, size = 5) +
  
  labs(title = "Media de pH por Mes y Zona de Recogida",
       x = "Mes",
       y = "Nivel de pH") +
  
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Helvetica", color = "grey30"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "grey50"),
    axis.text.y = element_text(color = "grey50"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_x_continuous(breaks = seq(1, 12, 1), labels = month_labels)

