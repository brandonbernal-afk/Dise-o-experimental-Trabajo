resp <- data.frame(
  Tiempo = c(12, 15, 17, 18, 20, 21, 22, 26),
  Edad = c(13, 25, 20, 35, 45, 30, 60, 95)
)
resp

plot(resp$Tiempo, resp$Edad,
)

resp$Rango_Tiempo <- rank(resp$Tiempo, ties.method = "first")
resp$Rango_Edad <- rank(resp$Edad, ties.method = "first")

plot(resp$Rango_Tiempo, resp$Rango_Edad, 
     col = "blue", #color de datos
     pch = 19, #cambia la forma del simbolo
     cex = 1.2, #Modifica el tamaño del simbolo
     xlab = "Rango variable tiempo", #Eje X
     ylab = "Rango edad") #Eje Y

cor.test(resp$Rango_Tiempo, resp$Rango_Edad, method = "spearman")


# Kendall -----------------------------------------------------------------
tau <- data.frame(
  A = c(1,2,3,4,5,6),
  B = c(3,1,4,2,6,5)
)

cor.test(tau$A, tau$B, method = "kendall")

plot(tau$A, tau$B,
)
