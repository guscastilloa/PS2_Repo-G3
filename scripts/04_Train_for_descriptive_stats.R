

##############################################################################-
# DATE:
#   2024/mar/31
# AUTHOR:
#  Jaime Buitrago
# DESCRIPTION:
#   Graphs
##############################################################################-

# Prepare workspace

rm (list=ls())
source("scripts/00_packages.R")
gc()

############################################################################-
# 1. Database preparation ----
############################################################################-

train <- fread("stores/raw/train_personas.csv")
db<- fread("stores/raw/train_hogares.csv")

train <- merge(train,db,
               by = 'id', 
               no.dups = TRUE,
               all = TRUE,
               suffixes = "")



############################################################################-
# 2. Transforming variables ----
############################################################################-

train_p <- train%>%
  select(id, Clase, Orden,Dominio,P6020,P6040,P6426,P6050, 
         P6090, P6800,P6100, P6210,P6240, P7472, P7422,
         Pet, Oc, Des, Ina, Ingtot)

train_h <- train %>% filter(Orden==1) %>% 
  select(Pobre, id, Clase, P5000, P5010, P5090,P5140, Nper, Npersug, Lp,
         Ingtotugarr, Ingpcug)


train_p$Genero <- ifelse(train_p$P6020 == 2, 1, 0) %>% as.numeric()
train_p$Menores_edad <- if_else(train_p$P6040<=14, 1, 0 , missing = NULL)
train_p$adulto_mayor <- if_else(train_p$P6040>=65, 1, 0 , missing = NULL)
train_p <- train_p %>%
  mutate(Desempleado = replace_na(Des, 0),
         Inactivo = replace_na(Ina, 0),
         Ocupado = replace_na(Oc, 0),
         Pet = replace_na(Pet,0))
train_p$eps <- ifelse(train_p$P6090 == 1, 1, 0) %>% as.numeric()

train_p$contributivo <- ifelse(train_p$P6100 == 1, 1, 0)
train_p$contributivo[train_p$P6100 != 1] <- 0
train_p$contributivo[train_p$P6100 == "."] <- 0
train_p$contributivo[is.na(train_p$P6100)] <- 0

#Está afiliado, es cotizante o beneficiario de alguna EPS.
train_p$eps <- ifelse(train_p$P6090 == 1, 1, 0) %>% as.numeric()

#Tipo de afiliación 
train_p$contributivo <- ifelse(train_p$P6100 == 1, 1, 0)
train_p$contributivo[train_p$P6100 != 1] <- 0
train_p$contributivo[train_p$P6100 == "."] <- 0
train_p$contributivo[is.na(train_p$P6100)] <- 0

#Rural
train_p$rural<-ifelse(train_p$Clase == 1, 1, 0)
train_p$rural <- ifelse(train_p$Clase == 1, 1, 0) %>% as.numeric()

# - Estudiante
train_p$estudiante <- ifelse(train_p$P6240 == 3, 1, 0)
train_p$estudiante[train_p$P6240 != 3] <- 0
train_p$estudiante[train_p$P6240 == "."] <- 0
train_p$estudiante[is.na(train_p$estudiante)] <- 0

# - Busca trabajo
train_p$busca_trabajo <- ifelse(train_p$P6240 == 2, 1, 0)
train_p$busca_trabajo[train_p$P6240 != 2] <- 0
train_p$busca_trabajo[train_p$P6240 == "."] <- 0
train_p$busca_trabajo[is.na(train_p$busca_trabajo)] <- 0

# - Amo(a) de casa
train_p$amo_casa <- ifelse(train_p$P6240 == 4, 1, 0)
train_p$amo_casa[train_p$P6240 != 4] <- 0
train_p$amo_casa[train_p$P6240 == "."] <- 0
train_p$amo_casa[is.na(train_p$amo_casa)] <- 0

# - Hijos en el hogar
train_p$hijos_hogar <- ifelse(train_p$P6050 == 3, 1, 0)
train_p$hijos_hogar[train_p$P6050 != 3] <- 0
train_p$hijos_hogar[train_p$P6050 == "."] <- 0
train_p$hijos_hogar[is.na(train_p$hijos_hogar)] <- 0

# - Primaria
train_p$primaria <- ifelse(train_p$P6210 == 1, 1, 0)
train_p$primaria[train_p$P6210 == "."] <- 0
train_p$primaria[is.na(train_p$primaria)] <- 0

# - Secundaria
train_p$secundaria <- ifelse(train_p$P6210 == 4, 1, 0)
train_p$secundaria[train_p$P6210 == "."] <- 0
train_p$secundaria[is.na(train_p$secundaria)] <- 0

# - Media
train_p$media <- ifelse(train_p$P6210 == 5, 1, 0)
train_p$media[train_p$P6210 == "."] <- 0
train_p$media[is.na(train_p$media)] <- 0

# - Superior
train_p$superior <- ifelse(train_p$P6210 == 6, 1, 0)
train_p$superior[train_p$P6210 == "."] <- 0
train_p$superior[is.na(train_p$superior)] <- 0

# - Edad (Sólo mayores de 18 años)
train_p <- rename(train_p, c("edad" = "P6040"))
train_p$edad_2 <- train_p$edad^2

# - Estudiante
train_p$estudiante <- ifelse(train_p$P6240 == 3, 1, 0)
train_p$estudiante[train_p$P6240 != 3] <- 0
train_p$estudiante[train_p$P6240 == "."] <- 0
train_p$estudiante[is.na(train_p$estudiante)] <- 0

#-Busca trabajo
train_p$busca_trabajo <- ifelse(train_p$P6240 == 2, 1, 0)
train_p$busca_trabajo[train_p$P6240 != 2] <- 0
train_p$busca_trabajo[train_p$P6240 == "."] <- 0
train_p$busca_trabajo[is.na(train_p$busca_trabajo)] <- 0

#-Amo(a) de casa
train_p$amo_casa <- ifelse(train_p$P6240 == 4, 1, 0)
train_p$amo_casa[train_p$P6240 != 4] <- 0
train_p$amo_casa[train_p$P6240 == "."] <- 0
train_p$amo_casa[is.na(train_p$amo_casa)] <- 0

#-Hijos en el hogar
train_p$hijos_hogar <- ifelse(train_p$P6050 == 3, 1, 0)
train_p$hijos_hogar[train_p$P6050 != 3] <- 0
train_p$hijos_hogar[train_p$P6050 == "."] <- 0
train_p$hijos_hogar[is.na(train_p$hijos_hogar)] <- 0

#-Experiencia trabajo actual
train_p <- rename(train_p, c("exp_trab_actual" = "P6426"))

#-Horas de trabajo a la semana
train_p <- rename(train_p, c("horas_trab_usual" = "P6800"))

#-Ciudad
train_p <- rename(train_p, c("ciudad" = "Dominio"))

#-Imputación de experiencia
train_p$exp_trab_actual <-ifelse(train_p$edad < 18 & 
                                   is.na(train_p$exp_trab_actual), 0, 
                                 train_p$exp_trab_actual)

train_p <- train_p %>% 
  group_by(id) %>% 
  mutate(mean_h = mean(horas_trab_usual, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(horas_trab_usual = if_else(is.na(horas_trab_usual) & train_p$edad >= 18, 
                                    mean_h, train_p$horas_trab_usual))

#-Imputación Horas 
train_p$horas_trab_usual <-ifelse(train_p$edad < 18 & 
                                    is.na(train_p$horas_trab_usual), 0, 
                                  train_p$horas_trab_usual)

train_p <- train_p %>% 
  group_by(id) %>% 
  mutate(variable = ifelse(all(is.na(horas_trab_usual)), 0, 
                           horas_trab_usual)) %>% 
  ungroup() %>% 
  mutate(horas_trab_usual = if_else(is.na(horas_trab_usual), 
                                    variable, train_p$horas_trab_usual))

train_p<- train_p %>% select("id", "Orden", "Clase",
                             "ciudad", "edad", "edad_2", "Genero", 
                             "estudiante", "busca_trabajo", "amo_casa",
                             "hijos_hogar", "Ingtot","exp_trab_actual",
                             "horas_trab_usual", 
                             "Menores_edad","adulto_mayor","Desempleado","Inactivo",
                             "Ocupado","hijos_hogar","edad_2","estudiante",
                             "busca_trabajo","amo_casa","hijos_hogar","eps","contributivo","rural","superior","media","secundaria","primaria")

#Hogares
train_h <- train_h %>% rename(num_cuartos = P5000, num_cuartos_dormir = P5010) 

#-Vivienda propia
train_h$vivienda_propia <- ifelse(train_h$P5090 == 1 | train_h$P5090==2 , 1, 0)
train_h$vivienda_propia[train_h$P5090 != 1 | train_h$P5090 != 2 ] <- 0
train_h$vivienda_propia[train_h$P5090 == "."] <- 0
train_h$vivienda_propia[is.na(train_h$P5090)] <- 0

#vivienda arriendo 
train_h$vivienda_arriendo<-ifelse(train_h$P5090 == 3, 1, 0)
train_h$vivienda_arriendo[train_h$P5090 != 3] <- 0
train_h$vivienda_arriendo[train_h$P5090 == "."] <- 0
train_h$vivienda_arriendo[is.na(train_h$P5090)] <- 0

train_h <-train_h%>% select("id","num_cuartos","num_cuartos_dormir","Nper"
                            ,"Npersug","Lp","Ingtotugarr","Ingpcug",
                            "vivienda_arriendo","vivienda_propia","Pobre")

#Unión de la base de datos 
train <- merge(train_h,train_p,
               by = 'id', 
               no.dups = TRUE,
               all = TRUE,
               suffixes = "")

# Pasar la base únicamente a hogares
train <- train %>% group_by(id) %>%
  summarize(edad = mean(edad,na.rm = TRUE),
            edad_2 = mean(edad_2,na.rm = TRUE),
            Genero = mean(Genero,na.rm = TRUE),
            estudiante = mean(estudiante,na.rm = TRUE),
            busca_trabajo = mean(busca_trabajo,na.rm = TRUE),
            amo_casa =mean(amo_casa,na.rm = TRUE),
            hijos_hogar = mean(amo_casa,na.rm = TRUE),
            primaria = mean(primaria,na.rm = TRUE),
            secundaria = mean(secundaria,na.rm = TRUE),
            media = mean(media,na.rm = TRUE),
            superior = mean(superior,na.rm = TRUE),
            Ingtot = sum(Ingtot,na.rm = TRUE),
            Ingtotugarr = mean(Ingtotugarr,na.rm = TRUE),
            exp_trab_actual = mean(exp_trab_actual,na.rm = TRUE),
            horas_trab_usual = mean(horas_trab_usual,na.rm = TRUE),
            Pobre = max(Pobre,na.rm = TRUE),
            Nper = mean(Nper,na.rm = TRUE),
            num_menores = sum(Menores_edad,na.rm = TRUE),
            num_adulto=sum(adulto_mayor,na.rm = TRUE),
            eps=mean(eps,na.rm = TRUE),
            rural=max(rural,na.rm = TRUE),
            num_cuartos=mean(num_cuartos,na.rm = TRUE),
            num_cuartos_dormir=mean(num_cuartos_dormir,na.rm = TRUE),
            Npersug=mean(Npersug,na.rm = TRUE),
            vivienda_arriendo=max(vivienda_arriendo,na.rm = TRUE),
            vivienda_propia=max(vivienda_propia,na.rm = TRUE),
            contributivo=mean(contributivo,na.rm = TRUE),
            Desempleado=mean(Desempleado,na.rm = TRUE),
            Inactivo=mean(Inactivo,na.rm = TRUE),
            Ocupado=mean(Ocupado,na.rm = TRUE),
            ciudad = first(ciudad))

arrow::write_parquet(train, sink = "stores/train.parquet")

############################################################################-
# 3. Graph ----
############################################################################-


ggplot(train, aes(x= Ingtotugarr/1000)) +
  geom_density()+
  geom_vline(aes(xintercept=mean( Ingtotugarr/1000)),
             color="blue", linetype="dashed", size=1)+
  xlab("Ingreso (miles)") +ylab("Densidad")+
  theme_apa()
ggsave("views/g1.jpg", scale=1, width = 7, height = 5 , units = 'in', dpi=600)

#End
