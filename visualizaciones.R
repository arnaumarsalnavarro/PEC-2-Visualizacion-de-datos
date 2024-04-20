# Treemap

# https://www.kaggle.com/datasets/syedanwarafridi/vehicle-sales-data

# Importamos librerías y datos:
library(dplyr)
library(treemapify)
library(ggplot2)

df <- read.csv("car_prices.csv")


# Agrupamos por marca y contamos la frecuencia de éstas:
df_agrupado <- df %>%
  group_by(make) %>%
  summarise(Frequency = n())

# Creamos uan variable para marcar la marca y la frecuencia en el gráfico:
df_agrupado <- df_agrupado %>%
  mutate(Label = paste(make, Frequency, sep = "\n"))

# Creamos el gráfico Treemap
ggplot(df_agrupado, aes(area = Frequency, fill = make, label = Label)) + geom_treemap() + 
  geom_treemap_text(colour = "white", place = "centre", size = 15) +
  theme(legend.position = "none")


#####################################################################################################

# Dot chart:

# https://www.kaggle.com/datasets/luiscorter/netflix-original-films-imdb-scores

df <- read.csv("NetflixOriginals.csv")

#df$IMDB.Score <- round(df$IMDB.Score, digits=0)
head(df)
df$Title <- iconv(df$Title, "UTF-8", "ASCII//TRANSLIT")

# Supongamos que tienes un DataFrame df con las columnas 'Title', 'Language' y 'IMDB.Score'
library(ggplot2)

# Creamos el gráfico de puntos
ggplot(df, aes(x = Language, y = IMDB.Score)) +
  geom_point() +
  labs(x = "Idioma de la película", y = "Calificación de la película", 
       title = "Dot Chart de Calificaciones IMDB por Idioma") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#####################################################################################################



# Contour plot:

# https://www.kaggle.com/datasets/ibrahimkiziloklu/solar-radiation-dataset

library(ggplot2)
library(akima)

df <- read.csv("meteo 2019.csv")
df <- df[, c("Relative.Humidity", "Temperature", "Pressure")]
head(df)

df$Relative.Humidity <- as.numeric(df$Relative.Humidity)
df$Temperature <- as.numeric(df$Temperature)
df$Temperature <- jitter(df$Temperature)
df$Pressure <- jitter(df$Pressure)


# Interpola los datos a una cuadrícula regular
interp_data <- with(df, akima::interp(x = Temperature, y = Pressure, z = Relative.Humidity))

# Convierte los datos interpolados a un dataframe
df_interp <- data.frame(
  Temperature = rep(interp_data$x, each = length(interp_data$y)),
  Pressure = rep(interp_data$y, times = length(interp_data$x)),
  Relative.Humidity = as.vector(interp_data$z)
)

df_interp$Relative.Humidity <- as.numeric(as.character(df_interp$Relative.Humidity))

# Crea el gráfico de contorno con los datos interpolados
ggplot(df_interp, aes(x = Temperature, y = Pressure, z = Relative.Humidity)) +
  geom_raster(aes(fill = Relative.Humidity)) +
  labs(x = "Temperatura (en grados Celsius)", y = "Presión atmosférica (en hPa)", fill = "Humedad (%)", title = "Contour Plot de Humedad por Temperatura y Presión")









