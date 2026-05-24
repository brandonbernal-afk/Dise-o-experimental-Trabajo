# Brandon Gael Sanchez Bernal
# 13/04/2026
# ANOVA

library(readxl)

# Importar datos
localidad <- read_excel("Datos_Rascon_Anova.xlsx")

# Ver datos
View(localidad)

# Convertir Paraje a factor
localidad$Paraje <- as.factor(localidad$Paraje)

# Boxplot
boxplot(localidad$DAP ~ localidad$Paraje,
        xlab = "Paraje",
        ylab = "DBH (cm)",
        col = "lightgreen")

# Normalidad de datos
shapiro.test(localidad$DAP)

# Histograma
hist(localidad$DAP)

# Media
mean(localidad$EDAD)

# Transformaciones
localidad$logDAP <- log10(localidad$DAP)
localidad$sqrDAP <- sqrt(localidad$DAP)

# Histogramas
hist(localidad$logDAP)
hist(localidad$sqrDAP)

# Pruebas de normalidad
shapiro.test(localidad$logDAP)
shapiro.test(localidad$sqrDAP)

# Homogeneidad de varianzas
bartlett.test(localidad$sqrDAP ~ localidad$Paraje)

# ANOVA
paraje.aov <- aov(localidad$sqrDAP ~ localidad$Paraje)

# Resumen ANOVA
summary(paraje.aov)

# Tukey
TukeyHSD(paraje.aov)

# Gráfico Tukey
plot(TukeyHSD(paraje.aov))
