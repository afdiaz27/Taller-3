# Cargar pacman (contiene la función p_load)
library(pacman) 

# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse,
       rio, 
       tidymodels,
       randomForest,
       rattle, 
       spatialsample,
       caret,
       rpart,
       ranger,
       stargazer,
       glmnet)

train_hogares <- read.csv('C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-3/stores/Bases de datos - Versión 3/train_hogares_3.csv')
test_hogares <- read.csv('C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-3/stores/Bases de datos - Versión 3/test_hogares_3.csv')
train_personas <- read.csv('C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-3/stores/Bases de datos - Versión 3/train_personas_3.csv')
test_personas <- read.csv('C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-3/stores/Bases de datos - Versión 3/test_personas_3.csv')

lista_var_pers_train_categ  <- c('Sexo', 'JefeHogar', 'FormalSalud', 'SeguridadSocial', 'maxEducLevel', 'relab', 'SubsAlimen', 'SubsTrans', 'SubsFamil', 'SubsEducativo', 'Viaticos', 'Bonificaciones', 'FormalPension', 'MasHoras', 'PagosExtraPensArri', 'Ayuda', 'GanancFinan','EdadTrabajo','Ocu', 'Desocu', 'Inact')

train_personas <- train_personas %>% mutate_at(lista_var_pers_train_categ, as.factor)

test_personas <- test_personas %>% mutate_at(lista_var_pers_train_categ, as.factor)
glimpse(train_personas)
summary(train_personas)

table(test_personas$relab)


fitControl<-trainControl(method ="cv",
                         number=5)

########## Random Forest

tree_ranger <- train(
  Ingreso ~ Sexo+Edad+JefeHogar+FormalSalud+SeguridadSocial+maxEducLevel+relab+SubsAlimen+SubsTrans+SubsFamil+SubsEducativo+Viaticos+Bonificaciones+hoursWorkUsual+sizeFirm+FormalPension+MasHoras+PagosExtraPensArri+Ayuda+GanancFinan+EdadTrabajo+Ocu+Desocu+Inact,
  data=train_personas,
  method = "ranger",
  trControl = fitControl,
  tuneGrid=expand.grid(
    mtry = 5,
    splitrule = "variance",
    min.node.size = 5)
)
print(tree_ranger)


##Matriz de confusión para datos train

Ingreso_pred_train<-predict(tree_ranger, newdata = train_personas)
train_personas$Ingreso_pred_train<-Ingreso_pred_train

Ingreso_train_hogar<-aggregate(train_personas$Ingreso_pred_train, by=list(Category=train_personas$id), FUN=sum)

Ingreso_train_hogar$Ingreso_pred_train_hogar<-Ingreso_train_hogar$x
Ingreso_train_hogar$id<-Ingreso_train_hogar$Category

train_hogares_comparacion<-merge(train_hogares, Ingreso_train_hogar, by='id', all.x=TRUE)

train_hogares_comparacion<-train_hogares_comparacion %>% mutate(Ingpcug_pred = (Ingreso_pred_train_hogar+ArriendoEst)/Npersug)
train_hogares_comparacion<-train_hogares_comparacion %>% mutate(Pobre_pred = case_when(Ingpcug_pred >= Lp ~ 0,
                                                                                       Ingpcug_pred < Lp ~ 1))
train_hogares_comparacion <- train_hogares_comparacion %>% mutate_at(c('Pobre','Pobre_pred'), as.factor)
matriz_confusion <- conf_mat(data.frame(true = train_hogares_comparacion$Pobre, pred = train_hogares_comparacion$Pobre_pred), true,pred)

matriz_confusion

#Matriz de confusión para datos train con corrección de Edad

Ingreso_pred_train<-predict(tree_ranger, newdata = train_personas)
train_personas$Ingreso_pred_train<-Ingreso_pred_train

train_personas <- train_personas %>% mutate(Ingreso_pred_train_corr = case_when(EdadTrabajo == 0 ~ 0,
                                                                                EdadTrabajo == 1 ~ Ingreso_pred_train))

Ingreso_corr_train_hogar<-aggregate(train_personas$Ingreso_pred_train_corr, by=list(Category=train_personas$id), FUN=sum)

Ingreso_corr_train_hogar$Ingreso_pred_train_corr<-Ingreso_corr_train_hogar$x
Ingreso_corr_train_hogar$id<-Ingreso_corr_train_hogar$Category

train_hogares_comparacion_corr<-merge(train_hogares, Ingreso_corr_train_hogar, by='id', all.x=TRUE)

train_hogares_comparacion_corr<-train_hogares_comparacion_corr %>% mutate(Ingpcug_pred = (Ingreso_pred_train_corr+ArriendoEst)/Npersug)
train_hogares_comparacion_corr<-train_hogares_comparacion_corr %>% mutate(Pobre_pred = case_when(Ingpcug_pred >= Lp ~ 0,
                                                                                       Ingpcug_pred < Lp ~ 1))
train_hogares_comparacion_corr <- train_hogares_comparacion_corr %>% mutate_at(c('Pobre','Pobre_pred'), as.factor)
matriz_confusion <- conf_mat(data.frame(true = train_hogares_comparacion_corr$Pobre, pred = train_hogares_comparacion_corr$Pobre_pred), true,pred)

matriz_confusion

Ingreso_test<-predict(tree_ranger, newdata = test_personas)

test_personas$Ingreso<-Ingreso_test

summary(test_personas$Ingreso)

Ingreso_hogar<-aggregate(test_personas$Ingreso, by=list(Category=test_personas$id), FUN=sum)

Ingreso_hogar$Ingreso<-Ingreso_hogar$x
Ingreso_hogar$id<-Ingreso_hogar$Category

test_hogares_cargue<-merge(test_hogares, Ingreso_hogar, by='id', all.x=TRUE)

test_hogares_cargue<-test_hogares_cargue %>% mutate(Ingpcug = (Ingreso+ArriendoEst)/Npersug)
test_hogares_cargue<-test_hogares_cargue %>% mutate(Pobre = case_when(Ingpcug >= Lp ~ 0,
                                                                      Ingpcug < Lp ~ 1))

test_hogares_cargue<-test_hogares_cargue %>% select(id,Pobre)

write.csv(test_hogares_cargue,"C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-3/stores/Resultados/Predicción Ingreso/Random Forest/RandomForest_02.csv", row.names = FALSE)