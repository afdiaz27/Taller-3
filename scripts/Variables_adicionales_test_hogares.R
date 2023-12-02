########################## Obtención de nuevas variables para test #################################

##Cargue de paquetes

library (pacman)
p_load(tidyverse, # Manipular dataframes
       rio, # Importar datos fácilmente
       plotly, # Gráficos interactivos
       tidymodels) # Modelado de datos limpios y ordenados

##Establecimiento del directorio de trabajo y cargue de base de datos

setwd("C:/Users/de.sandoval10/Documents/GitHub/Taller-3/stores/Bases de datos - Versión 3")

######### Creación de variables para base test de hogares


### 1. No. de personas por habitación destinada a dormir

df_test_hogares<- read.csv("test_hogares_3.csv")

df_test_hogares <- df_test_hogares %>%
  mutate(personasxhab = Nper/cuartosHab)

hist(df_test_hogares$personasxhab, main="Histograma del personas por habitación", xlab="Personas x habitación para dormir",ylab="Densidad/Frecuencia",col="darkblue", border = ("grey"), breaks=100)

### 3. Sexo del jefe de hogar

df_test_personas <- read.csv("test_personas_3.csv")

# Crear una nueva columna en df_hogares para almacenar el sexo del jefe de hogar
df_test_hogares$sexo_jefe_hogar <- NA

# Iterar sobre cada hogar en df_train_hogares
for (i in 1:nrow(df_test_hogares)) {
  # Obtener el ID del hogar
  id_hogar_actual <- df_test_hogares$id[i]
  
  # Obtener la información del jefe de hogar y su sexo en la base de personas
  info_jefe_hogar <- df_test_personas[df_test_personas$id == id_hogar_actual, ]
  
  # Actualizar el sexo del jefe de hogar en la base de hogares
  df_test_hogares$sexo_jefe_hogar[i] <- info_jefe_hogar$Sexo[info_jefe_hogar$JefeHogar == 1]
}

df_test_hogares <- df_test_hogares %>%
  mutate(jefe_mujer = if_else(sexo_jefe_hogar == 2, "si", "no"))

### 4. Edad del jefe de hogar

# Crear una nueva columna en df_hogares para almacenar la edad del jefe de hogar
df_test_hogares$edad_jefe_hogar <- NA

# Iterar sobre cada hogar en df_train_hogares
for (i in 1:nrow(df_test_hogares)) {
  # Obtener el ID del hogar
  id_hogar_actual <- df_test_hogares$id[i]
  
  # Obtener la información del jefe de hogar y su sexo en la base de personas
  info_jefe_hogar <- df_test_personas[df_test_personas$id == id_hogar_actual, ]
  
  # Actualizar el sexo del jefe de hogar en la base de hogares
  df_test_hogares$edad_jefe_hogar[i] <- info_jefe_hogar$Edad[info_jefe_hogar$JefeHogar == 1]
}

hist(df_test_hogares$edad_jefe_hogar, main="Histograma de edad jefe de hogar", xlab="Edad jefe",ylab="Densidad/Frecuencia",col="darkblue", border = ("grey"), breaks=100)

### 5. Menores de edad por unidad de gasto

df_test_hogares$menores_edad <- 0

# Iterar sobre cada hogar en df_train_hogares

for (i in 1:nrow(df_test_hogares)) {
  # Obtener el ID del hogar actual
  id_hogar_actual <- df_test_hogares$id[i]
  
  # Filtrar las personas que pertenecen al hogar actual en df_train_personas
  personas_en_hogar <- df_test_personas[df_test_personas$id == id_hogar_actual, ]
  
  # Contar cuántas personas son menores de 18 años
  menores_edad_en_hogar <- sum(personas_en_hogar$Edad < 18)
  
  # Actualizar la columna en df_train_hogares con la cantidad de menores de edad
  df_test_hogares$menores_edad[i] <- menores_edad_en_hogar
}

### 6. Ocupación del jefe de hogar

# Crear una nueva columna en df_hogares para almacenar la edad del jefe de hogar
df_test_hogares$ocupacion_jefe_hogar <- NA

# Iterar sobre cada hogar en df_train_hogares
for (i in 1:nrow(df_test_hogares)) {
  # Obtener el ID del hogar
  id_hogar_actual <- df_test_hogares$id[i]
  
  # Obtener la información del jefe de hogar y su sexo en la base de personas
  info_jefe_hogar <- df_test_personas[df_test_personas$id == id_hogar_actual, ]
  
  # Actualizar el sexo del jefe de hogar en la base de hogares
  df_test_hogares$ocupacion_jefe_hogar[i] <- info_jefe_hogar$relab[info_jefe_hogar$JefeHogar == 1]
}

### 6. Educación del jefe de hogar

# Crear una nueva columna en df_hogares para almacenar la edad del jefe de hogar
df_test_hogares$educacion_jefe_hogar <- NA

# Iterar sobre cada hogar en df_train_hogares
for (i in 1:nrow(df_test_hogares)) {
  # Obtener el ID del hogar
  id_hogar_actual <- df_test_hogares$id[i]
  
  # Obtener la información del jefe de hogar y su sexo en la base de personas
  info_jefe_hogar <- df_test_personas[df_test_personas$id == id_hogar_actual, ]
  
  # Actualizar el sexo del jefe de hogar en la base de hogares
  df_test_hogares$educacion_jefe_hogar[i] <- info_jefe_hogar$maxEducLevel[info_jefe_hogar$JefeHogar == 1]
}


### 7. Porcentaje de edad de trabajo

df_test_hogares <- df_test_hogares %>%
  mutate(porcentaje_edad_trabajo = ((Nper-menores_edad_en_hogar)/Nper)*100)

### 8. Porcentaje de ocupados

df_test_hogares$ocupados <- 0

# Iterar sobre cada hogar en df_train_hogares

for (i in 1:nrow(df_test_hogares)) {
  # Obtener el ID del hogar actual
  id_hogar_actual <- df_test_hogares$id[i]
  
  # Filtrar las personas que pertenecen al hogar actual en df_train_personas
  personas_en_hogar <- df_test_personas[df_test_personas$id == id_hogar_actual, ]
  
  # Contar cuántas personas son menores de 18 años
  ocupados_hogar <- sum(personas_en_hogar$Ocu == 1)
  
  # Actualizar la columna en df_train_hogares con la cantidad de menores de edad
  df_test_hogares$ocupados[i] <- ocupados_hogar
}

df_test_hogares <- df_test_hogares %>%
  mutate(porcentaje_ocupados = ((Nper-menores_edad_en_hogar)-ocupados)/(Nper-menores_edad_en_hogar)*100)


### 10. Máximo nivel educativo de la unidad de gasto

df_test_hogares$maxEducLevel_hogar <- 0

# Iterar sobre cada hogar en df_train_hogares
for (i in 1:nrow(df_test_hogares)) {
  # Obtener el ID del hogar actual
  id_hogar_actual <- df_test_hogares$id[i]
  
  # Filtrar las personas que pertenecen al hogar actual en df_train_personas
  personas_en_hogar <- df_test_personas[df_test_personas$id == id_hogar_actual, ]
  
  # Encontrar el máximo nivel educativo en el hogar
  max_educ_hogar <- max(personas_en_hogar$maxEducLevel)
  
  # Actualizar la columna en df_train_hogares con el máximo nivel educativo
  df_test_hogares$maxEducLevel_hogar[i] <- max_educ_hogar
}

save(df_test_hogares,file = "C:/Users/de.sandoval10/Documents/GitHub/Taller-3/stores/Bases de datos - Versión 4/test_hogares_4_1.Rda")

