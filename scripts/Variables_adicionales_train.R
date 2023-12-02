########################## Obtención de nuevas variables para train #################################

##Cargue de paquetes

library (pacman)
p_load(tidyverse, # Manipular dataframes
       rio, # Importar datos fácilmente
       plotly, # Gráficos interactivos
       tidymodels) # Modelado de datos limpios y ordenados

##Establecimiento del directorio de trabajo y cargue de base de datos

setwd("C:/Users/de.sandoval10/Documents/GitHub/Taller-3/stores/Bases de datos - Versión 3")

######### Creación de variables para base train de hogares


### 1. No. de personas por habitación destinada a dormir

df_train_hogares<- read.csv("train_hogares_3.csv")

df_train_hogares <- df_train_hogares %>%
  mutate(personasxhab = Nper/cuartosHab)

hist(df_train_hogares$personasxhab, main="Histograma del personas por habitación", xlab="Personas x habitación para dormir",ylab="Densidad/Frecuencia",col="darkblue", border = ("grey"), breaks=100)

### 2. Ingreso promedio del hogar: a menor ingreso promedio, mayor el esfuerzo del hogar para mantener a todos los miembros

df_train_hogares <- df_train_hogares %>%
  mutate(ingresoxper = Ingtotug/Nper)

hist(df_train_hogares$ingresoxper, main="Histograma de ingreso por persona", xlab="Ingreso x persona",ylab="Densidad/Frecuencia",col="darkblue", border = ("grey"), breaks=100)

### 3. Sexo del jefe de hogar

df_train_personas <- read.csv("train_personas_3.csv")

# Crear una nueva columna en df_hogares para almacenar el sexo del jefe de hogar
df_train_hogares$sexo_jefe_hogar <- NA

# Iterar sobre cada hogar en df_train_hogares
for (i in 1:nrow(df_train_hogares)) {
  # Obtener el ID del hogar
  id_hogar_actual <- df_train_hogares$id[i]
  
  # Obtener la información del jefe de hogar y su sexo en la base de personas
  info_jefe_hogar <- df_train_personas[df_train_personas$id == id_hogar_actual, ]
  
  # Actualizar el sexo del jefe de hogar en la base de hogares
  df_train_hogares$sexo_jefe_hogar[i] <- info_jefe_hogar$Sexo[info_jefe_hogar$JefeHogar == 1]
}

df_train_hogares <- df_train_hogares %>%
  mutate(jefe_mujer = if_else(sexo_jefe_hogar == 2, "si", "no"))

### 4. Edad del jefe de hogar

# Crear una nueva columna en df_hogares para almacenar la edad del jefe de hogar
df_train_hogares$edad_jefe_hogar <- NA

# Iterar sobre cada hogar en df_train_hogares
for (i in 1:nrow(df_train_hogares)) {
  # Obtener el ID del hogar
  id_hogar_actual <- df_train_hogares$id[i]
  
  # Obtener la información del jefe de hogar y su sexo en la base de personas
  info_jefe_hogar <- df_train_personas[df_train_personas$id == id_hogar_actual, ]
  
  # Actualizar el sexo del jefe de hogar en la base de hogares
  df_train_hogares$edad_jefe_hogar[i] <- info_jefe_hogar$Edad[info_jefe_hogar$JefeHogar == 1]
}

hist(df_train_hogares$edad_jefe_hogar, main="Histograma de edad jefe de hogar", xlab="Edad jefe",ylab="Densidad/Frecuencia",col="darkblue", border = ("grey"), breaks=100)

### 5. Menores de edad por unidad de gasto

df_train_hogares$menores_edad <- 0

# Iterar sobre cada hogar en df_train_hogares

for (i in 1:nrow(df_train_hogares)) {
  # Obtener el ID del hogar actual
  id_hogar_actual <- df_train_hogares$id[i]
  
  # Filtrar las personas que pertenecen al hogar actual en df_train_personas
  personas_en_hogar <- df_train_personas[df_train_personas$id == id_hogar_actual, ]
  
  # Contar cuántas personas son menores de 18 años
  menores_edad_en_hogar <- sum(personas_en_hogar$Edad < 18)
  
  # Actualizar la columna en df_train_hogares con la cantidad de menores de edad
  df_train_hogares$menores_edad[i] <- menores_edad_en_hogar
}

### 6. Ocupación del jefe de hogar

# Crear una nueva columna en df_hogares para almacenar la edad del jefe de hogar
df_train_hogares$ocupacion_jefe_hogar <- NA

# Iterar sobre cada hogar en df_train_hogares
for (i in 1:nrow(df_train_hogares)) {
  # Obtener el ID del hogar
  id_hogar_actual <- df_train_hogares$id[i]
  
  # Obtener la información del jefe de hogar y su sexo en la base de personas
  info_jefe_hogar <- df_train_personas[df_train_personas$id == id_hogar_actual, ]
  
  # Actualizar el sexo del jefe de hogar en la base de hogares
  df_train_hogares$ocupacion_jefe_hogar[i] <- info_jefe_hogar$relab[info_jefe_hogar$JefeHogar == 1]
}

### 7. Educación del jefe de hogar

# Crear una nueva columna en df_hogares para almacenar la edad del jefe de hogar
df_train_hogares$educacion_jefe_hogar <- NA

# Iterar sobre cada hogar en df_train_hogares
for (i in 1:nrow(df_train_hogares)) {
  # Obtener el ID del hogar
  id_hogar_actual <- df_train_hogares$id[i]
  
  # Obtener la información del jefe de hogar y su sexo en la base de personas
  info_jefe_hogar <- df_train_personas[df_train_personas$id == id_hogar_actual, ]
  
  # Actualizar el sexo del jefe de hogar en la base de hogares
  df_train_hogares$educacion_jefe_hogar[i] <- info_jefe_hogar$maxEducLevel[info_jefe_hogar$JefeHogar == 1]
}


### 8. Porcentaje de edad de trabajo

df_train_hogares <- df_train_hogares %>%
  mutate(porcentaje_edad_trabajo = ((Nper-menores_edad_en_hogar)/Nper)*100)

### 8. Porcentaje de ocupados

df_train_hogares$ocupados <- 0

# Iterar sobre cada hogar en df_train_hogares

for (i in 1:nrow(df_train_hogares)) {
  # Obtener el ID del hogar actual
  id_hogar_actual <- df_train_hogares$id[i]
  
  # Filtrar las personas que pertenecen al hogar actual en df_train_personas
  personas_en_hogar <- df_train_personas[df_train_personas$id == id_hogar_actual, ]
  
  # Contar cuántas personas son menores de 18 años
  ocupados_hogar <- sum(personas_en_hogar$Ocu == 1)
  
  # Actualizar la columna en df_train_hogares con la cantidad de menores de edad
  df_train_hogares$ocupados[i] <- ocupados_hogar
}

df_train_hogares <- df_train_hogares %>%
  mutate(porcentaje_ocupados = ((Nper-menores_edad_en_hogar)-ocupados)/(Nper-menores_edad_en_hogar)*100)


### 9. Máximo nivel educativo de la unidad de gasto

df_train_hogares$maxEducLevel_hogar <- 0

# Iterar sobre cada hogar en df_train_hogares
for (i in 1:nrow(df_train_hogares)) {
  # Obtener el ID del hogar actual
  id_hogar_actual <- df_train_hogares$id[i]
  
  # Filtrar las personas que pertenecen al hogar actual en df_train_personas
  personas_en_hogar <- df_train_personas[df_train_personas$id == id_hogar_actual, ]
  
  # Encontrar el máximo nivel educativo en el hogar
  max_educ_hogar <- max(personas_en_hogar$maxEducLevel)
  
  # Actualizar la columna en df_train_hogares con el máximo nivel educativo
  df_train_hogares$maxEducLevel_hogar[i] <- max_educ_hogar
}

save(df_train_hogares,file = "C:/Users/de.sandoval10/Documents/GitHub/Taller-3/stores/Bases de datos - Versión 4/train_hogares_4.Rda")




