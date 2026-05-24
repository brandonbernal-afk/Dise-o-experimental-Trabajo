# Brandon Gael Sanchez Bernal
# 14/04/2026
# Datos Climatológicos

# Librería
library(readxl)

# Importar Excel
clima <- read_excel(file.choose())

# Revisar estructura
str(clima)

# Convertir localidad a factor
clima$Localidad <- as.factor(clima$Localidad)

# Convertir fecha
clima$Fecha <- as.Date(clima$Fecha)

# Extraer solo la hora
clima$Hora <- sub("1899-12-31 ", "", clima$Hora)

# Crear fecha y hora completas
clima$fechaHora <- as.POSIXct(
  paste(clima$Fecha, clima$Hora),
  format = "%Y-%m-%d %H:%M:%S"
)

# Revisar
head(clima$fechaHora)

# Filtrar datos de Cumbres
cumbres <- subset(clima, Localidad == "Cumbres")

# Ordenar datos
cumbres <- cumbres[order(cumbres$fechaHora), ]

# Graficar temperatura
plot(cumbres$fechaHora,
     cumbres$TEMP,
     type = "l",
     col = "indianred",
     lwd = 2,
     xlab = "Fecha",
     ylab = "Temperatura (°C)",
     main = "Serie de tiempo de temperatura - Cumbres",
     las = 2)

# Cuadrícula
grid()


