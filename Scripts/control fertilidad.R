# Brandon Gael Sanchez Bernal
# Prueba t de Student

# Librería para leer Excel
library(readxl)

# Importar archivo Excel
calidad <- read_excel(file.choose())

# Ver datos
View(calidad)

# Convertir tratamiento a factor
calidad$Tratamiento <- as.factor(calidad$Tratamiento)

# Revisar niveles
levels(calidad$Tratamiento)

# Histogramas por tratamiento
hist(subset(calidad$IE, calidad$Tratamiento == "Ctrl"),
     main = "Histograma Control",
     xlab = "IE",
     col = "lightblue")

hist(subset(calidad$IE, calidad$Tratamiento == "Fert"),
     main = "Histograma Fertilizante",
     xlab = "IE",
     col = "lightgreen")

# Boxplot comparativo
boxplot(calidad$IE ~ calidad$Tratamiento,
        xlab = "Tratamiento",
        ylab = "Índice Ecológico (IE)",
        col = c("lightblue", "lightgreen"),
        main = "Comparación entre tratamientos")

# Separar tratamientos
Ctrl <- subset(calidad$IE,
               calidad$Tratamiento == "Ctrl")

Fert <- subset(calidad$IE,
               calidad$Tratamiento == "Fert")

# Pruebas de normalidad
shapiro.test(Ctrl)
shapiro.test(Fert)

# Homogeneidad de varianzas
bartlett.test(calidad$IE ~ calidad$Tratamiento)

# Prueba t de Student
t.test(calidad$IE ~ calidad$Tratamiento,
       var.equal = TRUE)