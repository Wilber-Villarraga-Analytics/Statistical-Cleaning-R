library(readr)
base_suramericanos <- read_csv("base_suramericanos.csv")
View(base_suramericanos)
# Exploración básica
dim(base_suramericanos)
names(base_suramericanos)
str(base_suramericanos)
head(base_suramericanos)
# -------------------------------------------------
# 2. Copia de seguridad y versión de trabajo
# -------------------------------------------------
datos_raw <- base_suramericanos
datos_clean <- base_suramericanos
# -------------------------------------------------
# 3. Limpieza de variables numéricas
# -------------------------------------------------
#vars_num <- c("edad", "estatura_cm", "peso_kg","ingresos_usd","pib_per_capita_usd")

#for (v in vars_num) {
# 
  # Convertir a carácter
#  x <- as.character(datos_clean[[v]])
  
  # Quitar espacios
#  x <- trimws(x)
  
  # Eliminar comas
#  x <- gsub(",", "", x, fixed = TRUE)
  
  # Estandarizar para detectar inválidos
#  x_upper <- toupper(x)
  
  # Reemplazar valores inválidos por NA
#  x[x_upper == ""] <- NA
 # x[x_upper == "ERROR"] <- NA
 # x[x_upper == "UNKNOWN"] <- NA
  
  # Convertir a numérico
#  datos_clean[[paste0(v, "_num")]] <- as.numeric(x)}

# se ven que los datos ya son numericos esta bien si nos saltamos este paso

# Verificación de variables numéricas o caracter
summary(datos_clean)

sum(is.na(datos_clean$edad))

sum(is.na(datos_clean$estatura_cm))
# son iguales a los NA que se muestran en el summary

# verificar que el imc esta corecto

datos_clean$total_imc <- datos_clean$peso_kg / (datos_clean$estatura_cm/100)**2
datos_clean$total_imc <- round(datos_clean$total_imc,digits = 2)
datos_clean$Dif_Total <- datos_clean$imc - datos_clean$total_imc

summary(datos_clean$Dif_Total)

# Filas con inconsistencias
errores_total <- datos_clean[
  abs(datos_clean$Dif_Total) > 0.01 & !is.na(datos_clean$Dif_Total),
]

nrow(errores_total)

errores_total[, c("edad", "estatura_cm", "peso_kg","ingresos_usd","pib_per_capita_usd","imc","total_imc")]

# Corregir Total.Spent cuando haya inconsistencia
datos_clean$Total.Spent_corregido <- ifelse(
  abs(datos_clean$Dif_Total) > 0.01,
  datos_clean$total_imc,
  datos_clean$imc
)
summary(datos_clean)
# -------------------------------------------------
# 6. Limpieza y estandarización de variables categóricas
# -------------------------------------------------
cols_cat <- c("pais", "practica_deporte", "tipo_deporte")

for (col in cols_cat) {
  
  x <- as.character(datos_clean[[col]])
  x <- tolower(x)
  x <- trimws(x)
  
  # Reducir dobles espacios
  x <- gsub("  ", " ", x, fixed = TRUE)
  
  # Reemplazar inválidos por NA
  x[x == ""] <- NA
  x[x == "unknown"] <- NA
  x[x == "error"] <- NA
  
  datos_clean[[col]] <- x
}

# Verificación de categóricas
table(datos_clean$pais, useNA = "ifany")

table(datos_clean$practica_deporte, useNA = "ifany")

table(datos_clean$tipo_deporte, useNA = "ifany")

# -------------------------------------------------
# 7. Revisión final de calidad
# -------------------------------------------------

# NA por variable
colSums(is.na(datos_clean))

# Porcentaje de NA
round(colMeans(is.na(datos_clean)) * 100, 2)

# Revisar inválidos en columnas de texto
cols_text <- sapply(datos_clean, is.character)

sum(datos_clean[, cols_text] == "error", na.rm = TRUE)

sum(datos_clean[, cols_text] == "unknown", na.rm = TRUE)

# Revisar numéricas
summary(datos_clean$edad)
summary(datos_clean$estatura_cm)
summary(datos_clean$peso_kg)
summary(datos_clean$ingresos_usd)
summary(datos_clean$pib_per_capita_usd)
summary(datos_clean)

# -------------------------------------------------
# 8. Construcción de la base final limpia
# -------------------------------------------------
datos_final <- datos_clean[, c(
  "pais",
  "edad",
  "estatura_cm",
  "peso_kg",
  "practica_deporte",
  "tipo_deporte",
  "ingresos_usd",
  "pib_per_capita_usd",
  "Total.Spent_corregido"
)]

# Renombrar variables
names(datos_final) <- c(
  "pais",
  "edad",
  "estatura_cm",
  "peso_kg",
  "practica_deporte",
  "tipo_deporte",
  "ingresos_usd",
  "pib_per_capita_usd",
  "imc"
)

# Verificación inicial de datos_final
str(datos_final)

summary(datos_final)

colSums(is.na(datos_final))

# -------------------------------------------------
# 9. Imputación de variables numéricas con la mediana
# -------------------------------------------------
mediana_edad <- median(datos_final$edad, na.rm = TRUE)
datos_final$edad[is.na(datos_final$edad)] <- mediana_edad

mediana_usd <- median(datos_final$ingresos_usd, na.rm = TRUE)
datos_final$ingresos_usd[is.na(datos_final$ingresos_usd)] <- mediana_usd

# ver si se puede recalcular las variables

filas_na<-datos_final[!complete.cases(datos_final), ]
head(filas_na)

# no se pueden entonces vamos a rempplazar con la mediana

mediana_estatura_cm  <- median(datos_final$estatura_cm, na.rm = TRUE)
datos_final$estatura_cm[is.na(datos_final$estatura_cm)] <- mediana_estatura_cm 

mediana_peso_kg  <- median(datos_final$peso_kg, na.rm = TRUE)
datos_final$peso_kg[is.na(datos_final$peso_kg)] <- mediana_peso_kg 

# Recalcular Total_Spent a partir de Quantity y Price_Per_Unit

datos_final$imc <- datos_final$peso_kg / (datos_final$estatura_cm/100)**2

# -------------------------------------------------
# 10. Imputación de variables categóricas
# -------------------------------------------------
datos_final$practica_deporte[is.na(datos_final$practica_deporte)] <- "unknown"
datos_final$tipo_deporte[is.na(datos_final$tipo_deporte)] <- "unknown"

# -------------------------------------------------
# 11. Revisión final de la base lista para análisis
# -------------------------------------------------
colSums(is.na(datos_final))

summary(datos_final)

str(datos_final)

head(datos_final)

