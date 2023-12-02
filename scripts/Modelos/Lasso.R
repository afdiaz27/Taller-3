require("pacman")
p_load("tidyverse","stargazer","glmnet","rio","tidymodels")


train_hogares <- read.csv('C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-3/stores/Bases de datos - Versión 3/train_hogares_3.csv')
test_hogares <- read.csv('C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-3/stores/Bases de datos - Versión 3/test_hogares_3.csv')
train_personas <- read.csv('C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-3/stores/Bases de datos - Versión 3/train_personas_3.csv')
test_personas <- read.csv('C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-3/stores/Bases de datos - Versión 3/test_personas_3.csv')

lista_var_pers_train_categ  <- c('Sexo', 'JefeHogar', 'FormalSalud', 'SeguridadSocial', 'maxEducLevel', 'relab', 'SubsAlimen', 'SubsTrans', 'SubsFamil', 'SubsEducativo', 'Viaticos', 'Bonificaciones', 'FormalPension', 'MasHoras', 'PagosExtraPensArri', 'Ayuda', 'GanancFinan','EdadTrabajo','Ocu', 'Desocu', 'Inact')

train_personas <- train_personas %>% mutate_at(lista_var_pers_train_categ, as.factor)

test_personas <- test_personas %>% mutate_at(lista_var_pers_train_categ, as.factor)
glimpse(train_personas)
summary(train_personas)

train_fold <- vfold_cv(train_personas, v = 5)

recipe1 <- recipe(formula = Ingreso ~ Sexo+Edad+JefeHogar+FormalSalud+SeguridadSocial+maxEducLevel+relab+SubsAlimen+SubsTrans+SubsFamil+SubsEducativo+Viaticos+Bonificaciones+hoursWorkUsual+sizeFirm+FormalPension+MasHoras+PagosExtraPensArri+Ayuda+GanancFinan+EdadTrabajo+Ocu+Desocu+Inact, data = train_personas) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(Edad,sizeFirm,hoursWorkUsual)

specification1 <- linear_reg(mixture = 1, penalty = tune()) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

workf1 <- workflow() %>%
  add_recipe(recipe1) %>%
  add_model(specification1)

penalty_grid <- grid_regular(penalty(range = c(-5, 5)), levels = 30)
penalty_grid

tune_res1 <- tune_grid(workf1,
                       resamples = train_fold,
                       grid = penalty_grid,
                       metrics = metric_set(rmse)
)

best_penalty1 <- select_best(tune_res1, metric = "rmse")

modelo_01 <- finalize_workflow(workf1, best_penalty1)
modelo_01_fit <- fit(modelo_01, data = train_personas)

augment(modelo_01_fit, new_data = train_personas) %>%
  mae(truth = Ingreso, estimate = .pred)

##Matriz de confusión para datos train

Ingreso_pred_train<-deframe(predict(modelo_01_fit, train_personas))

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


##Predicción de test

pred_ingreso<-deframe(predict(modelo_01_fit, test_personas))

test_personas$Ingreso<-pred_ingreso

summary(test_personas$Ingreso)

Ingreso_hogar<-aggregate(test_personas$Ingreso, by=list(Category=test_personas$id), FUN=sum)

Ingreso_hogar$Ingreso<-Ingreso_hogar$x
Ingreso_hogar$id<-Ingreso_hogar$Category

test_hogares_cargue<-merge(test_hogares, Ingreso_hogar, by='id', all.x=TRUE)

test_hogares_cargue<-test_hogares_cargue %>% mutate(Ingpcug = (Ingreso+ArriendoEst)/Npersug)
test_hogares_cargue<-test_hogares_cargue %>% mutate(Pobre = case_when(Ingpcug >= Lp ~ 0,
                                                                      Ingpcug < Lp ~ 1))
                                                    
test_hogares_cargue<-test_hogares_cargue %>% select(id,Pobre)

write.csv(test_hogares_cargue,"C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller-3/stores/Resultados/Predicción Ingreso/Lasso/Lasso_01.csv", row.names = FALSE)
                                                    
                                                    