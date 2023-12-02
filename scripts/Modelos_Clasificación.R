########################## Modelos de clasificación #################################

##Cargue de paquetes

library (pacman)

p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       caret, # Classification And REgression Training
       psych,
       modelsummary,
       stargazer,
       foreach,
       ggplot2)

p_load("GGally","psych","rpart.plot","ROCR","gamlr","modelsummary","gtsummary","naniar","PerformanceAnalytics","pastecs",
       "writexl","dplyr","httr","tidyverse","rvest","rio","skimr","caret","ggplot2","stargazer",
       "readr","AER","MLmetrics","smotefamily","pROC","smotefamily","rpart","randomForest","rpart", "Metrics",
       "rattle")

setwd("C:/Users/dj.farfan10/Documents/GitHub/Taller-3/stores/Bases de datos - Versión 4")

load("train_hogares_4_1.Rda")
load("test_hogares_4_1.Rda")

df_train_hogares$personasxhab<-as.double(df_train_hogares$personasxhab)
df_train_hogares$Nper<-as.double(df_train_hogares$Nper)
df_train_hogares$edad_jefe_hogar<-as.double(df_train_hogares$edad_jefe_hogar)
df_train_hogares$porcentaje_ocupados<-as.double(df_train_hogares$porcentaje_ocupado)
df_train_hogares$menores_edad<-as.double(df_train_hogares$menores_edad)

df_test_hogares_2$personasxhab<-as.double(df_test_hogares_2$personasxhab)
df_test_hogares_2$Nper<-as.double(df_test_hogares_2$Nper)
df_test_hogares_2$edad_jefe_hogar<-as.double(df_test_hogares_2$edad_jefe_hogar)
df_test_hogares_2$porcentaje_ocupados<-as.double(df_test_hogares_2$porcentaje_ocupado)
df_test_hogares_2$menores_edad<-as.double(df_test_hogares_2$menores_edad)

variables_categoricas <- c("Clase",
                           "Propiedad",
                           "jefe_mujer",
                           "maxEducLevel_hogar",
                           "ocupacion_jefe_hogar")

df_train_hogares <- df_train_hogares %>% 
  mutate_at(variables_categoricas, as.factor)

df_test_hogares_2 <- df_test_hogares_2 %>% 
  mutate_at(variables_categoricas, as.factor)


# Se crea la variable sample ----

set.seed(4865)
p_load(caret)


# df para clasificación

summary(df_train_hogares$Pobre)

tabla_frecuencia <- table(df_train_hogares$Pobre)

porcentaje_Pobre <- prop.table(tabla_frecuencia) * 100

print(tabla_frecuencia)
print(porcentaje_Pobre)

df_train_hogares$Pobre <- factor(df_train_hogares$Pobre,levels = c(0,1),
                          labels=c("no","si"))


split1 <-createDataPartition(df_train_hogares$Pobre,p=0.7)[[1]]

length(split1)

training <- df_train_hogares[split1,]

other <- df_train_hogares[-split1,]

split2 <- createDataPartition(other$Pobre,p=1/3)[[1]]

evaluation <- other[split2,]

testing <- other[-split2,]

dim(training)
dim(testing)
dim(evaluation)

# Validar particiones
prop.table(table(df_train_hogares$Pobre))
prop.table(table(training$Pobre))
prop.table(table(testing$Pobre))
prop.table(table(evaluation$Pobre))

predict <- stats::predict

##### creación de receta de los modelos a usar

modelo <- as.formula("Pobre ~ personasxhab + Propiedad + Nper + jefe_mujer + edad_jefe_hogar+maxEducLevel_hogar+ocupacion_jefe_hogar+porcentaje_ocupados+menores_edad")

ffcv<-function(...)c(twoClassSummary(...), defaultSummary(...))
Control <- trainControl(method = "cv",
                        number = 5,
                        summaryFunction = ffcv,
                        classProbs = TRUE,
                        verbose=FALSE,
                        savePredictions = T)

######## Logit

logitM1 <- train(
  modelo,
  data = training,
  method = "glm",
  trControl = Control,
  family = "binomial",
  preProcess = c("center", "scale")
)

logitM1

testR <- data.frame(Pobre=testing$Pobre)
testR$logitM1 <- predict(logitM1,
                         newdata = testing,
                         type= "prob")[,1]

testR

testR <- testR %>% 
  mutate(
    logitM1=ifelse(logitM1>0.5,"si","no")
  )

with(testR,table(Pobre,logitM1))

logitM1


# Exportar resultados

d_submit <- df_test_hogares_2
d_submit$predict <- predict(logitM1,d_submit,type = "prob")[,1]

submit <- d_submit %>% 
  mutate(Pobre=ifelse(predict>0.5,1,0)) %>% 
  select(id,Pobre)
prop.table(table(submit$Pobre))


write.csv(submit,file = "../Bases de datos - Versión 4/logit1.csv",row.names = FALSE)


modelo2 <- as.formula("Pobre ~ personasxhab + Propiedad + jefe_mujer + edad_jefe_hogar+maxEducLevel_hogar+ocupacion_jefe_hogar+menores_edad")

ffcv<-function(...)c(twoClassSummary(...), defaultSummary(...))
Control <- trainControl(method = "cv",
                        number = 5,
                        summaryFunction = ffcv,
                        classProbs = TRUE,
                        verbose=FALSE,
                        savePredictions = T)

######## Logit 2

logitM2 <- train(
  modelo2,
  data = training,
  method = "glm",
  trControl = Control,
  family = "binomial",
  preProcess = c("center", "scale")
)

logitM2

testR <- data.frame(Pobre=testing$Pobre)
testR$logitM2 <- predict(logitM2,
                         newdata = testing,
                         type= "prob")[,1]

testR

testR <- testR %>% 
  mutate(
    logitM2=ifelse(logitM2>0.5,"si","no")
  )

with(testR,table(Pobre,logitM2))

logitM2


# Exportar resultados

d_submit <- df_test_hogares_2
d_submit$predict <- predict(logitM2,d_submit,type = "prob")[,1]

submit <- d_submit %>% 
  mutate(Pobre=ifelse(predict>0.5,1,0)) %>% 
  select(id,Pobre)
prop.table(table(submit$Pobre))


write.csv(submit,file = "../Bases de datos - Versión 4/logit2_clasificacion.csv",row.names = FALSE)