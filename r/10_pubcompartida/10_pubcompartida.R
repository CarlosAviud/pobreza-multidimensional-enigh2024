#------------------------------------------------------------------------------#
# Proyecto:                   PUBLICACIÓN COMPARTIDA -Transformación ENIGH 
# Objetivo:                   Procesos para Carencia por acceso a los servicios de salud por ámbito de residencia
#
# Encargadas:       
# Correos:                    samantha@mexicocomovamos.mx
# 
# Fecha de creación:          16 de julio de 2025
# Última actualización:       21 de julio de 2025
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

#Sys.setlocale("LC_TIME", "es_ES")
Sys.setlocale("LC_TIME")

# Desactivar notacion científica
options(scipen=999)

# Vaciar espacio de trabajo
rm(list=ls())

# Colores MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Verde, amarillo, naranja y rojo
mcv_blacks   <- c("black"  , "#D2D0CD", "#777777")            # Negros
mcv_morados  <- c("#6950D8", "#A99BE9")

mcv_subcategorías <- c( 
  "#C6B2E3", "#E8B32E", "#0A93C4", "#974DF0", 
  "#00D2D1", "#FF43FA", mcv_blacks[2], "#00b783","#ff6260","#E8D92E", mcv_blacks[3], "#0fD26F")


mcv_categorías <- c(
  "#4D5BF0", "#E84D9A", "#E8866D", "#ADADAF"
)

# Cargamos librerías
if (!require(pacman)) install.packages("pacman")
if(!require("foreign")) install.packages("foreign") & require("foreign")
if(!require("scales")) install.packages("scales") & require("scales")
if(!require("dplyr")) install.packages("dplyr") & require("dplyr")
if(!require("lubridate")) install.packages("lubridate") & require("lubridate")
if(!require("tidyr")) install.packages("tidyr") & require("tidyr")
if(!require("srvyr")) install.packages("srvyr") & require("srvyr")
if(!require("curl")) install.packages("curl") & require("curl")
if(!require("XML")) install.packages("XML") & require("XML")
if(!require("zoo"))       install.packages("zoo")       & require("zoo")
if(!require("stringi"))   install.packages("stringi")   & require("stringi")
if(!require("gganimate")) install.packages("gganimate") & require("gganimate")
if(!require("gridExtra")) install.packages("gridExtra") & require("gridExtra")
if(!require("ggthemes"))  install.packages("ggthemes")  & require("ggthemes")
if(!require("magick"))    install.packages("magick")    & require("magick")
if(!require("scales"))    install.packages("scales")    & require("scales")
if(!require("hrbrthemes")) install.packages("hrbrthemes") & require("hrbrthemes")
if(!require("RColorBrewer")) install.packages("RColorBrewer") & require("RColorBrewer")
if(!require("ggpubr")) install.packages("ggpubr") & require("ggpubr")
if(!require("stringr")) install.packages("stringr") & require("stringr")
if(!require("ggpp")) install.packages("ggpp") & require("ggpp")
if(!require("openxlsx")) install.packages("openxlsx") & require("openxlsx")
if(!require("mxmaps")) install.packages("mxmaps") & require("mxmaps")
if(!require("svglite")) install.packages("svglite") & require("svglite")
require(extrafont)
library(sf)
library(viridis)
library(ggalluvial)
library(cowplot)
p_load("tidyverse","stringr", "Hmisc", "EnvStats", "survey","srvyr", "data.table","bit64", "statar", "ggrepel")
loadfonts(device="pdf")
loadfonts(device="postscript")
require(tidyverse)


# Funciones con direcciones de las carpetas
paste_inp  <- function(x){paste0("02_datos_crudos/", x)}
paste_out  <- function(x){paste0("10_pubcompartida/03_datos_limpios/", x)}
paste_code <- function(x){paste0("10_pubcompartida/01_códigos/"      , x)}
paste_csv  <- function(x){paste0("10_pubcompartida/04_csvs/"         , x)}
paste_plant <- function(x){paste0("05_infobites/"    , x)}
paste_inf <- function(x){paste0("10_pubcompartida/05_infobites/"    , x)}

#entidades
entidad_labels <- c(
  "00" = "Nacional",        
  "01" = "Aguascalientes",
  "02" = "Baja California", 
  "03" = "Baja California Sur",
  "04" = "Campeche",        
  "05" = "Coahuila",
  "06" = "Colima",          
  "07" = "Chiapas",
  "08" = "Chihuahua",       
  "09" = "Ciudad de México",
  "10" = "Durango",         
  "11" = "Guanajuato",
  "12" = "Guerrero",        
  "13" = "Hidalgo",
  "14" = "Jalisco",         
  "15" = "México",
  "16" = "Michoacán",       
  "17" = "Morelos",
  "18" = "Nayarit",         
  "19" = "Nuevo León",
  "20" = "Oaxaca",          
  "21" = "Puebla",
  "22" = "Querétaro",       
  "23" = "Quintana Roo",
  "24" = "San Luis Potosí", 
  "25" = "Sinaloa",
  "26" = "Sonora",          
  "27" = "Tabasco",
  "28" = "Tamaulipas",      
  "29" = "Tlaxcala",
  "30" = "Veracruz",        
  "31" = "Yucatán",
  "32" = "Zacatecas"
)

salud_labels <- c(
  "servmed_1" = "Centro de Salud",
  "servmed_2" = "Hospital o Instituto",       
  "servmed_3" = "IMSS",
  "servmed_4" = "IMSS-Prospera/IMSS-Bienestar",         
  "servmed_5" = "ISSTE",
  "servmed_6" = "ISSTE estatal",        
  "servmed_7" = "Otro servicio médico público",
  "servmed_8" = "Consultorios privados",         
  "servmed_9" = "Consultorio de farmacias",
  "servmed_10" = "Curandero",       
  "servmed_11" = "Otro",
  "servmed_12" = "INSABI",
  "inst_1" = "Institución médica IMSS",
  "inst_2" = "Institución médica ISSSTE",
  "inst_3" = "Institución médica ISSSTE estatal",
  "inst_4" = "Institución médica PEMEX",
  "inst_5" = "Institución médica IMSS-Prospera/IMSS Bienestar",
  "inst_6" = "Otra institución médica",
  "ic_asalud" = "Carencia por acceso a servicios de salud",
  "segvol_2" = "Seguro privado"
  
)

# A. Transformación bases estilo CONEVAL 2016 -----------

# INDICADOR DE CARENCIA POR ACCESO A LOS SERVICIOS DE SALUD

# Base de datos población
pobla_16 <- read_csv(paste_inp("poblacion_2016.csv")) %>% rename_all(tolower)
pobla_18 <- read_csv(paste_inp("poblacion_2018.csv")) %>% rename_all(tolower)
pobla_20 <- read_csv(paste_inp("poblacion_2020.csv")) %>% rename_all(tolower)
pobla_22 <- read_csv(paste_inp("poblacion_2022.csv")) %>% rename_all(tolower)

# Acceso a servicios de salud por prestaciones laborales
ocupados_16 <- read_csv(paste_inp("trabajos_2016.csv"))%>% rename_all(tolower)
ocupados_18 <- read_csv(paste_inp("trabajos_2018.csv"))%>% rename_all(tolower)
ocupados_20 <- read_csv(paste_inp("trabajos_2020.csv"))%>% rename_all(tolower)
ocupados_22 <- read_csv(paste_inp("trabajos_2022.csv"))%>% rename_all(tolower)

# 
viviendas_16 <- read_csv(paste_inp("viviendas_2016.csv"))%>% rename_all(tolower)
viviendas_18 <- read_csv(paste_inp("viviendas_2018.csv"))%>% rename_all(tolower)
viviendas_20 <- read_csv(paste_inp("viviendas_2020.csv"))%>% rename_all(tolower)
viviendas_22 <- read_csv(paste_inp("viviendas_2022.csv"))%>% rename_all(tolower)

## 2016 ------------------
ocupados <- ocupados_16
pobla <- pobla_16
viviendas <- viviendas_16
anio <- 2016


# Tipo de trabajador: identifica la población subordinada e independiente
ocupados <-mutate(ocupados, 
                  tipo_trab=case_when(
                    #Subordinados
                    subor==1 ~ 1,
                    #Independientes que reciben un pago
                    subor==2 & indep==1 & tiene_suel==1 ~ 2,
                    subor==2 & indep==2 & pago==1 ~ 2,
                    #Independientes que no reciben un pago
                    subor==2 & indep==1 & tiene_suel==2 ~ 3,
                    subor==2 & indep==2 & (pago==2 | pago==3) ~ 3))


# Ocupación principal o secundaria
ocupados <- mutate(ocupados, 
                   ocupa=case_when(id_trabajo==1 ~ 1, id_trabajo==2 ~ 1)) %>% 
  dplyr::select(folioviv, foliohog, numren, id_trabajo, tipo_trab, ocupa)

# Distinción de prestaciones en trabajo principal y secundario
ocupados <- dcast(as.data.table(ocupados),  folioviv + foliohog + numren ~ 
                    id_trabajo, value.var=c("tipo_trab", "ocupa"), sep="", fill=0)


ocupados <- mutate(ocupados, 
                   # Identificación de la población trabajadora 
                   # (toda la que reporta al menos un empleo en la base de trabajos.csv)
                   trab=1) %>%
  select(folioviv, foliohog, numren, trab,starts_with("tipo_trab"), 
         starts_with("ocupa"))


fwrite(ocupados, paste_out("ocupados16.csv"), row.names=F)

ocupados_16 <- ocupados

# Población objetivo: no se incluye a huéspedes ni trabajadores domésticos
salud <- pobla
salud <- filter(salud, !(parentesco>=400 & parentesco <500 |
                           parentesco>=700 & parentesco <800))

salud <- left_join(salud, ocupados, by = c("folioviv", "foliohog", "numren"))

salud <- mutate(salud, 
                # PEA (personas de 16 años o más)
                pea=case_when(trab==1 & (edad>=16 & !is.na(edad)) ~ 1, # PEA: ocupada
                              (act_pnea1==1 | act_pnea2==1) & 
                                (edad>=16 & !is.na(edad)) ~ 2, # PEA: desocupada
                              (edad>=16 & !is.na(edad)) & 
                                ((act_pnea1!=1 | is.na(act_pnea1)) & 
                                   (act_pnea2!=1 | is.na(act_pnea2))) & 
                                ((act_pnea1>=2 & act_pnea1<=6) | 
                                   (act_pnea2>=2 & act_pnea2<=6)) ~ 0), # PNEA
                # Tipo de trabajo
                # Ocupación principal
                tipo_trab1=ifelse(pea==1, tipo_trab1, tipo_trab1), # Depende de un patrón, jefe o superior  
                tipo_trab1=ifelse((pea==0 | pea==2), NA_real_, tipo_trab1), # No depende de un jefe y recibe o tiene asignado un sueldo
                tipo_trab1=ifelse(is.na(pea), NA_real_, tipo_trab1), # No depende de un jefe y no recibe o no tiene asignado un sueldo
                
                # Ocupación secundaria
                tipo_trab2=ifelse(pea==1, tipo_trab2, tipo_trab2), # Depende de un patrón, jefe o superior  
                tipo_trab2=ifelse((pea==0 | pea==2), NA_real_, tipo_trab2), # No depende de un jefe y recibe o tiene asignado un sueldo
                tipo_trab2=ifelse(is.na(pea), NA_real_, tipo_trab2)) # No depende de un jefe y no recibe o no tiene asignado un sueldo

# Servicios médicos prestaciones laborales
salud <- mutate(salud, 
                # Ocupación principal
                smlab1=case_when(ocupa1==1 & atemed==1 & 
                                   (inst_1==1 | inst_2==2 |  inst_3==3 |inst_4==4) & 
                                   (inscr_1==1) ~ 1, # Con servicios médicos
                                 ocupa1==1 ~ 0), # Sin servicios médicos
                # Ocupación secundaria
                smlab2=case_when(ocupa2==1 & atemed==1 & 
                                   (inst_1==1 | inst_2==2 |inst_3==3 | inst_4==4) & 
                                   (inscr_1==1) ~  1,  # Con servicios médicos 
                                 ocupa2==1 ~ 0), # Sin servicios médicos
                # Contratación voluntaria de servicios médicos
                smcv=case_when(atemed==1 & 
                                 (inst_1==1 | inst_2==2 | inst_3==3 | inst_4==4) & 
                                 inscr_6==6 & (edad>=12 & !is.na(edad)) ~  1, # Sí cuenta
                               (edad>=12 & !is.na(edad)) ~ 0)) # No cuenta

# Acceso directo a servicios de salud
salud <- mutate(salud, 
                sa_dir=case_when(
                  # Ocupación principal
                  tipo_trab1==1 & (smlab1==1) ~ 1, # Con acceso
                  tipo_trab1==2 & (smlab1==1 | smcv==1) ~ 1, # Con acceso
                  tipo_trab1==3 & (smlab1==1 | smcv==1) ~ 1, # Con acceso
                  # Ocupación secundaria
                  tipo_trab2==1 & (smlab2==1) ~ 1, # Con acceso
                  tipo_trab2==2 & (smlab2==1 | smcv==1) ~ 1, # Con acceso
                  tipo_trab2==3 & (smlab2==1 | smcv==1 ) ~ 1,  # Con acceso
                  TRUE ~0)) 

# Núcleos familiares
salud <- mutate(salud,
                par=case_when((parentesco>=100 & parentesco<200) ~ 1, # Jefe o jefa del hogar
                              (parentesco>=200 & parentesco<300) ~ 2, # Cónyuge del  jefe/a
                              (parentesco>=300 & parentesco<400) ~ 3, # Hijo del jefe/a 
                              parentesco==601 ~ 4, # Padre o Madre del jefe/a
                              parentesco==615 ~ 5, # Suegro del jefe/a
                              TRUE ~ 6), # Sin parentesco directo
                
                # Asimismo, se utilizará la información relativa a la asistencia a la escuela
                inas_esc=case_when(asis_esc==1 ~ 0,   # Sí asiste
                                   asis_esc==2 ~ 1 )) # No asiste

# En primer lugar se identifican los principales parentescos respecto a la 
# jefatura del hogar y si ese miembro cuenta con acceso directo
salud <- mutate(salud,
                jef=case_when(par==1 & sa_dir==1 & 
                                (((inst_2==2 | inst_3==3) & inscr_6==6) & 
                                   (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) &
                                   (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                      is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7))) ~ NA_real_,
                              par==1 & sa_dir==1 ~ 1),
                
                cony=case_when(par==2 & sa_dir==1 & 
                                 (((inst_2==2 | inst_3==3) & inscr_6==6) &
                                    (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) &
                                    (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                       is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7) )) ~ NA_real_,
                               par==2 & sa_dir==1 ~ 1),
                hijo=case_when(par==3 & sa_dir==1 & 
                                 (((inst_2==2 | inst_3==3) & inscr_6==6) & 
                                    (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) & 
                                    (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                       is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7) )) ~ NA_real_,
                               par==3 & sa_dir==1  ~ 1))

salud <- as.data.table(salud)[, c("jef_sa", "cony_sa", "hijo_sa") :=
                                .(sum(jef, na.rm=TRUE),
                                  sum(cony, na.rm=TRUE),
                                  sum(hijo, na.rm=TRUE)), by=.(folioviv, foliohog)] %>% 
  mutate(jef_sa=if_else(jef_sa>0, 1,jef_sa), # Acceso directo a servicios de salud de la jefatura del hogar
         cony_sa=if_else(cony_sa>0, 1,cony_sa), # Acceso directo a servicios de salud del cónyuge de la jefatura del hogar
         hijo_sa=if_else(hijo_sa>0, 1,hijo_sa), # Acceso directo a servicios de salud de hijos(as) de la jefatura del hogar
         
         # Otros núcleos familiares: se identifica a la población con acceso a servicios de salud
         # mediante otros núcleos familiares a través de la afiliación o inscripción a 
         # servicios de salud por algún familiar dentro o fuera del hogar, muerte del 
         # asegurado o por contratación propia
         
         s_salud=case_when(atemed==1 & (inst_1==1 | inst_2==2 | inst_3==3 | inst_4==4) & 
                             (inscr_3==3| inscr_4==4 | inscr_6==6 | inscr_7==7) ~ 1,
                           !is.na(segpop) & !is.na(atemed) ~ 0)) 


# Indicador de carencia por servicios de salud

# Se considera en situación de carencia por acceso a servicios de salud
# a la población que:
#  
#  1. No cuente con adscripción o derecho a recibir servicios médicos de alguna 
#     institución  que  los  preste,  incluyendo  el  Seguro  Popular,  las  
#     instituciones  de  seguridad  social  (IMSS,  ISSSTE  federal  o  estatal,  
#     PEMEX, Ejército o Marina) o los servicios médicos privados


salud <- mutate(salud,
                
                # Indicador de carencia por acceso a los servicios de salud
                
                # Acceso directo
                ic_asalud=case_when(sa_dir==1 ~ 0,
                                    # Parentesco directo: jefatura
                                    par==1 & cony_sa==1 ~ 0, # No presenta carencia
                                    par==1 & pea==0 & hijo_sa==1 ~ 0, # No presenta carencia
                                    # Parentesco directo: cónyuge
                                    par==2 & jef_sa==1 ~ 0, # No presenta carencia
                                    par==2 & pea==0 & hijo_sa==1 ~ 0, # No presenta carencia
                                    # Parentesco directo: descendientes
                                    par==3 & edad<16 & jef_sa==1 ~ 0, # No presenta carencia
                                    par==3 & edad<16 & cony_sa==1 ~ 0, # No presenta carencia
                                    par==3 & (edad>=16 & edad<=25) & inas_esc==0 & jef_sa==1 ~ 0, # No presenta carencia
                                    par==3 & (edad>=16 & edad<=25) & inas_esc==0 & cony_sa==1 ~ 0, # No presenta carencia
                                    # Parentesco directo: ascendientes
                                    par==4 & pea==0 & jef_sa==1 ~ 0, # No presenta carencia
                                    par==5 & pea==0 & cony_sa==1 ~ 0, # No presenta carencia
                                    # Otros núcleos familiares
                                    s_salud==1 ~ 0, # No presenta carencia
                                    # Acceso reportado
                                    segpop==1 | (segpop==2 & atemed==1 & 
                                                   (inst_1==1 | inst_2==2 | inst_3==3 | 
                                                      inst_4==4 | inst_5==5 | inst_6==6)) | 
                                      segvol_2==2 ~ 0, # No presenta carencia
                                    TRUE~ 1)) %>% # Presenta carencia
  select(folioviv, foliohog, numren, sexo, 
         starts_with("sa_"), ends_with("_sa"), 
         segpop, atemed, starts_with("inst_"),
         starts_with("inscr_"), starts_with("segvol_"), starts_with("inst_"),
         ic_asalud, aten_sal, prob_sal, servmed_1,servmed_2,servmed_3,servmed_4,servmed_5,servmed_6,servmed_7,servmed_8,servmed_9,servmed_10,servmed_11, prob_anio, segvol_2)

fwrite(salud, paste_out("ic_asalud_16.csv"), row.names = FALSE)

salud_16 <- salud

## tablita para afiliación -----
viviendas <- viviendas%>%
  select(folioviv, factor)


pob_salud <- salud #%>%
# filter(
#   ic_asalud == 1
# )

sbv <- left_join(pob_salud, viviendas, by = c("folioviv")) 

f <- sbv %>%
  mutate(across(
    .cols = c(starts_with("inst_"), segvol_2),
    .fns = ~ case_when(
      !is.na(.) ~ 1,
      is.na(.)  ~ 0
    )
  ))

pob_w <- as_survey_design(f, weights=factor, nest=TRUE) #%>% srvyr::filter(!is.na(servmed_1))

# Define variables of interest
vars <- colnames(select(f, starts_with("inst_"),ic_asalud, segvol_2))


por <- as.data.frame(matrix(unlist(pob_w %>% 
                                     srvyr::select(vars) %>%
                                     summarise_all(survey_mean, vartype=NULL)), 
                            ncol = 1, byrow=T)*100) %>% round(3)

tot <- as.data.frame(matrix(unlist(pob_w %>% 
                                     srvyr::select(vars) %>%
                                     summarise_all(survey_total, vartype=NULL)), 
                            ncol =1 , byrow=T) / 1000000)

nac <- bind_cols(por, tot)

rownames(nac) <- c("inst_1","inst_2","inst_3","inst_4","inst_5","inst_6","ic_asalud", "segvol_2"
)

colnames(nac) <- c("Porcentaje", "Millones de personas")
nac

row.names(nac) <- salud_labels[row.names(nac)]

assign(paste0("nac_afiliad_", anio), nac)


## tablita para atención------
viviendas <- viviendas%>%
  select(folioviv, factor)


pob_salud <- salud %>%
  filter(
    aten_sal == 1,
    prob_sal == 1 ,
    prob_anio == anio
  )

sbv <- left_join(pob_salud, viviendas, by = c("folioviv")) 

f <- sbv %>%
  mutate(across(
    .cols = c(starts_with("servmed_")),
    .fns = ~ case_when(
      !is.na(.) ~ 1,
      is.na(.)  ~ 0
    )
  ))

f_long <- f %>%
  pivot_longer(
    cols = starts_with("servmed_"),
    names_to = "servicio",
    values_to = "usado"
  ) %>%
  filter(usado == 1)

pob_long <- as_survey_design(f_long, weights = factor, nest = TRUE)

nac <- pob_long %>%
  group_by(servicio) %>%
  summarise(visitas = survey_total(vartype = NULL)) %>%
  ungroup() %>%
  mutate(
    Porcentaje = round(100 * visitas / sum(visitas), 2),
    `Millones de visitas` = visitas / 1e6
  ) %>%
  select(servicio, Porcentaje, `Millones de visitas`)

nac$servicio <- salud_labels[nac$servicio]

assign(paste0("nac_aten_", anio), nac)

## 2018 ------------
ocupados <- ocupados_18
pobla <- pobla_18
viviendas <- viviendas_18
anio <- 2018

# Tipo de trabajador: identifica la población subordinada e independiente
ocupados <-mutate(ocupados, 
                  tipo_trab=case_when(
                    #Subordinados
                    subor==1 ~ 1,
                    #Independientes que reciben un pago
                    subor==2 & indep==1 & tiene_suel==1 ~ 2,
                    subor==2 & indep==2 & pago==1 ~ 2,
                    #Independientes que no reciben un pago
                    subor==2 & indep==1 & tiene_suel==2 ~ 3,
                    subor==2 & indep==2 & (pago==2 | pago==3) ~ 3))


# Ocupación principal o secundaria
ocupados <- mutate(ocupados, 
                   ocupa=case_when(id_trabajo==1 ~ 1, id_trabajo==2 ~ 1)) %>% 
  dplyr::select(folioviv, foliohog, numren, id_trabajo, tipo_trab, ocupa)

# Distinción de prestaciones en trabajo principal y secundario
ocupados <- dcast(as.data.table(ocupados),  folioviv + foliohog + numren ~ 
                    id_trabajo, value.var=c("tipo_trab", "ocupa"), sep="", fill=0)


ocupados <- mutate(ocupados, 
                   # Identificación de la población trabajadora 
                   # (toda la que reporta al menos un empleo en la base de trabajos.csv)
                   trab=1) %>%
  select(folioviv, foliohog, numren, trab,starts_with("tipo_trab"), 
         starts_with("ocupa"))

fwrite(ocupados, paste_out("ocupados18.csv"), row.names=F)

ocupados_18 <- ocupados


# Población objetivo: no se incluye a huéspedes ni trabajadores domésticos
salud <- pobla
salud <- filter(salud, !(parentesco>=400 & parentesco <500 |
                           parentesco>=700 & parentesco <800))

salud <- left_join(salud, ocupados, by = c("folioviv", "foliohog", "numren"))

salud <- mutate(salud, 
                # PEA (personas de 16 años o más)
                pea=case_when(trab==1 & (edad>=16 & !is.na(edad)) ~ 1, # PEA: ocupada
                              (act_pnea1==1 | act_pnea2==1) & 
                                (edad>=16 & !is.na(edad)) ~ 2, # PEA: desocupada
                              (edad>=16 & !is.na(edad)) & 
                                ((act_pnea1!=1 | is.na(act_pnea1)) & 
                                   (act_pnea2!=1 | is.na(act_pnea2))) & 
                                ((act_pnea1>=2 & act_pnea1<=6) | 
                                   (act_pnea2>=2 & act_pnea2<=6)) ~ 0), # PNEA
                # Tipo de trabajo
                # Ocupación principal
                tipo_trab1=ifelse(pea==1, tipo_trab1, tipo_trab1), # Depende de un patrón, jefe o superior  
                tipo_trab1=ifelse((pea==0 | pea==2), NA_real_, tipo_trab1), # No depende de un jefe y recibe o tiene asignado un sueldo
                tipo_trab1=ifelse(is.na(pea), NA_real_, tipo_trab1), # No depende de un jefe y no recibe o no tiene asignado un sueldo
                
                # Ocupación secundaria
                tipo_trab2=ifelse(pea==1, tipo_trab2, tipo_trab2), # Depende de un patrón, jefe o superior  
                tipo_trab2=ifelse((pea==0 | pea==2), NA_real_, tipo_trab2), # No depende de un jefe y recibe o tiene asignado un sueldo
                tipo_trab2=ifelse(is.na(pea), NA_real_, tipo_trab2)) # No depende de un jefe y no recibe o no tiene asignado un sueldo

# Servicios médicos prestaciones laborales
salud <- mutate(salud, 
                # Ocupación principal
                smlab1=case_when(ocupa1==1 & atemed==1 & 
                                   (inst_1==1 | inst_2==2 |  inst_3==3 |inst_4==4) & 
                                   (inscr_1==1) ~ 1, # Con servicios médicos
                                 ocupa1==1 ~ 0), # Sin servicios médicos
                # Ocupación secundaria
                smlab2=case_when(ocupa2==1 & atemed==1 & 
                                   (inst_1==1 | inst_2==2 |inst_3==3 | inst_4==4) & 
                                   (inscr_1==1) ~  1,  # Con servicios médicos 
                                 ocupa2==1 ~ 0), # Sin servicios médicos
                # Contratación voluntaria de servicios médicos
                smcv=case_when(atemed==1 & 
                                 (inst_1==1 | inst_2==2 | inst_3==3 | inst_4==4) & 
                                 inscr_6==6 & (edad>=12 & !is.na(edad)) ~  1, # Sí cuenta
                               (edad>=12 & !is.na(edad)) ~ 0)) # No cuenta

# Acceso directo a servicios de salud
salud <- mutate(salud, 
                sa_dir=case_when(
                  # Ocupación principal
                  tipo_trab1==1 & (smlab1==1) ~ 1, # Con acceso
                  tipo_trab1==2 & (smlab1==1 | smcv==1) ~ 1, # Con acceso
                  tipo_trab1==3 & (smlab1==1 | smcv==1) ~ 1, # Con acceso
                  # Ocupación secundaria
                  tipo_trab2==1 & (smlab2==1) ~ 1, # Con acceso
                  tipo_trab2==2 & (smlab2==1 | smcv==1) ~ 1, # Con acceso
                  tipo_trab2==3 & (smlab2==1 | smcv==1 ) ~ 1,  # Con acceso
                  TRUE ~0)) 

# Núcleos familiares
salud <- mutate(salud,
                par=case_when((parentesco>=100 & parentesco<200) ~ 1, # Jefe o jefa del hogar
                              (parentesco>=200 & parentesco<300) ~ 2, # Cónyuge del  jefe/a
                              (parentesco>=300 & parentesco<400) ~ 3, # Hijo del jefe/a 
                              parentesco==601 ~ 4, # Padre o Madre del jefe/a
                              parentesco==615 ~ 5, # Suegro del jefe/a
                              TRUE ~ 6), # Sin parentesco directo
                
                # Asimismo, se utilizará la información relativa a la asistencia a la escuela
                inas_esc=case_when(asis_esc==1 ~ 0,   # Sí asiste
                                   asis_esc==2 ~ 1 )) # No asiste

# En primer lugar se identifican los principales parentescos respecto a la 
# jefatura del hogar y si ese miembro cuenta con acceso directo
salud <- mutate(salud,
                jef=case_when(par==1 & sa_dir==1 & 
                                (((inst_2==2 | inst_3==3) & inscr_6==6) & 
                                   (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) &
                                   (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                      is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7))) ~ NA_real_,
                              par==1 & sa_dir==1 ~ 1),
                
                cony=case_when(par==2 & sa_dir==1 & 
                                 (((inst_2==2 | inst_3==3) & inscr_6==6) &
                                    (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) &
                                    (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                       is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7) )) ~ NA_real_,
                               par==2 & sa_dir==1 ~ 1),
                hijo=case_when(par==3 & sa_dir==1 & 
                                 (((inst_2==2 | inst_3==3) & inscr_6==6) & 
                                    (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) & 
                                    (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                       is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7) )) ~ NA_real_,
                               par==3 & sa_dir==1  ~ 1))

salud <- as.data.table(salud)[, c("jef_sa", "cony_sa", "hijo_sa") :=
                                .(sum(jef, na.rm=TRUE),
                                  sum(cony, na.rm=TRUE),
                                  sum(hijo, na.rm=TRUE)), by=.(folioviv, foliohog)] %>% 
  mutate(jef_sa=if_else(jef_sa>0, 1,jef_sa), # Acceso directo a servicios de salud de la jefatura del hogar
         cony_sa=if_else(cony_sa>0, 1,cony_sa), # Acceso directo a servicios de salud del cónyuge de la jefatura del hogar
         hijo_sa=if_else(hijo_sa>0, 1,hijo_sa), # Acceso directo a servicios de salud de hijos(as) de la jefatura del hogar
         
         # Otros núcleos familiares: se identifica a la población con acceso a servicios de salud
         # mediante otros núcleos familiares a través de la afiliación o inscripción a 
         # servicios de salud por algún familiar dentro o fuera del hogar, muerte del 
         # asegurado o por contratación propia
         
         s_salud=case_when(atemed==1 & (inst_1==1 | inst_2==2 | inst_3==3 | inst_4==4) & 
                             (inscr_3==3| inscr_4==4 | inscr_6==6 | inscr_7==7) ~ 1,
                           !is.na(segpop) & !is.na(atemed) ~ 0)) 


# Indicador de carencia por servicios de salud

# Se considera en situación de carencia por acceso a servicios de salud
# a la población que:
#  
#  1. No cuente con adscripción o derecho a recibir servicios médicos de alguna 
#     institución  que  los  preste,  incluyendo  el  Seguro  Popular,  las  
#     instituciones  de  seguridad  social  (IMSS,  ISSSTE  federal  o  estatal,  
#     PEMEX, Ejército o Marina) o los servicios médicos privados

salud <- mutate(salud,
                
                # Indicador de carencia por acceso a los servicios de salud
                
                # Acceso directo
                ic_asalud=case_when(sa_dir==1 ~ 0,
                                    # Parentesco directo: jefatura
                                    par==1 & cony_sa==1 ~ 0, # No presenta carencia
                                    par==1 & pea==0 & hijo_sa==1 ~ 0, # No presenta carencia
                                    # Parentesco directo: cónyuge
                                    par==2 & jef_sa==1 ~ 0, # No presenta carencia
                                    par==2 & pea==0 & hijo_sa==1 ~ 0, # No presenta carencia
                                    # Parentesco directo: descendientes
                                    par==3 & edad<16 & jef_sa==1 ~ 0, # No presenta carencia
                                    par==3 & edad<16 & cony_sa==1 ~ 0, # No presenta carencia
                                    par==3 & (edad>=16 & edad<=25) & inas_esc==0 & jef_sa==1 ~ 0, # No presenta carencia
                                    par==3 & (edad>=16 & edad<=25) & inas_esc==0 & cony_sa==1 ~ 0, # No presenta carencia
                                    # Parentesco directo: ascendientes
                                    par==4 & pea==0 & jef_sa==1 ~ 0, # No presenta carencia
                                    par==5 & pea==0 & cony_sa==1 ~ 0, # No presenta carencia
                                    # Otros núcleos familiares
                                    s_salud==1 ~ 0, # No presenta carencia
                                    # Acceso reportado
                                    segpop==1 | (segpop==2 & atemed==1 & 
                                                   (inst_1==1 | inst_2==2 | inst_3==3 | 
                                                      inst_4==4 | inst_5==5 | inst_6==6)) | 
                                      segvol_2==2 ~ 0, # No presenta carencia
                                    TRUE~ 1)) %>% # Presenta carencia
  select(folioviv, foliohog, numren, sexo, 
         starts_with("sa_"), ends_with("_sa"), 
         segpop, atemed, starts_with("inst_"),
         starts_with("inscr_"), starts_with("segvol_"), starts_with("inst_"),
         ic_asalud, aten_sal, prob_sal, servmed_1,servmed_2,servmed_3,servmed_4,servmed_5,servmed_6,servmed_7,servmed_8,servmed_9,servmed_10,servmed_11, prob_anio, segvol_2)

fwrite(salud, paste_out("ic_asalud_18.csv"), row.names=F)

salud_18 <- salud
## tablita para afiliación -----
viviendas <- viviendas%>%
  select(folioviv, factor)


pob_salud <- salud #%>%
# filter(
#   ic_asalud == 1
# )

sbv <- left_join(pob_salud, viviendas, by = c("folioviv")) 

f <- sbv %>%
  mutate(across(
    .cols = c(starts_with("inst_"),segvol_2),
    .fns = ~ case_when(
      !is.na(.) ~ 1,
      is.na(.)  ~ 0
    )
  ))

pob_w <- as_survey_design(f, weights=factor, nest=TRUE) #%>% srvyr::filter(!is.na(servmed_1))

# Define variables of interest
vars <- colnames(select(f, starts_with("inst_"),ic_asalud, segvol_2))


por <- as.data.frame(matrix(unlist(pob_w %>% 
                                     srvyr::select(vars) %>%
                                     summarise_all(survey_mean, vartype=NULL)), 
                            ncol = 1, byrow=T)*100) %>% round(3)

tot <- as.data.frame(matrix(unlist(pob_w %>% 
                                     srvyr::select(vars) %>%
                                     summarise_all(survey_total, vartype=NULL)), 
                            ncol =1 , byrow=T) / 1000000)

nac <- bind_cols(por, tot)

rownames(nac) <- c("inst_1","inst_2","inst_3","inst_4","inst_5","inst_6","ic_asalud","segvol_2"
)

colnames(nac) <- c("Porcentaje", "Millones de personas")
nac

row.names(nac) <- salud_labels[row.names(nac)]

assign(paste0("nac_afiliad_", anio), nac)


## tablita para atención------

viviendas <- viviendas%>%
  select(folioviv, factor)


pob_salud <- salud %>%
  filter(
    aten_sal == 1,
    prob_sal == 1 ,
    prob_anio == anio
  )

sbv <- left_join(pob_salud, viviendas, by = c("folioviv")) 

f <- sbv %>%
  mutate(across(
    .cols = c(starts_with("servmed_")),
    .fns = ~ case_when(
      !is.na(.) ~ 1,
      is.na(.)  ~ 0
    )
  ))

f_long <- f %>%
  pivot_longer(
    cols = starts_with("servmed_"),
    names_to = "servicio",
    values_to = "usado"
  ) %>%
  filter(usado == 1)

pob_long <- as_survey_design(f_long, weights = factor, nest = TRUE)

nac <- pob_long %>%
  group_by(servicio) %>%
  summarise(visitas = survey_total(vartype = NULL)) %>%
  ungroup() %>%
  mutate(
    Porcentaje = round(100 * visitas / sum(visitas), 2),
    `Millones de visitas` = visitas / 1e6
  ) %>%
  select(servicio, Porcentaje, `Millones de visitas`)

nac$servicio <- salud_labels[nac$servicio]

assign(paste0("nac_aten_", anio), nac)

## 2020 -----------------
ocupados <- ocupados_20
pobla <- pobla_20
viviendas <- viviendas_20
anio <- 2020

# Tipo de trabajador: identifica la población subordinada e independiente

ocupados <-mutate(ocupados, 
                  tipo_trab=case_when(
                    #Subordinados
                    subor==1 ~ 1,
                    #Independientes que reciben un pago
                    subor==2 & indep==1 & tiene_suel==1 ~ 2,
                    subor==2 & indep==2 & pago==1 ~ 2,
                    #Independientes que no reciben un pago
                    subor==2 & indep==1 & tiene_suel==2 ~ 3,
                    subor==2 & indep==2 & (pago==2 | pago==3) ~ 3))

# Ocupación principal o secundaria
ocupados <- mutate(ocupados, 
                   ocupa=case_when(id_trabajo==1 ~ 1, id_trabajo==2 ~ 1)) %>% 
  dplyr::select(folioviv, foliohog, numren, id_trabajo, tipo_trab, ocupa)

# Distinción de prestaciones en trabajo principal y secundario
ocupados <- dcast(as.data.table(ocupados),  folioviv + foliohog + numren ~ 
                    id_trabajo, value.var=c("tipo_trab", "ocupa"), sep="", fill=0) 

ocupados <- mutate(ocupados, 
                   # Identificación de la población trabajadora 
                   # (toda la que reporta al menos un empleo en la base de trabajos.csv)
                   trab=1) %>%
  select(folioviv, foliohog, numren, trab,starts_with("tipo_trab"), 
         starts_with("ocupa"))

fwrite(ocupados, paste_out("ocupados20.csv"), row.names=F)

ocupados_20 <- ocupados

# Población objetivo: no se incluye a huéspedes ni trabajadores domésticos
salud <- pobla
salud <- filter(salud, !(parentesco>=400 & parentesco <500 |
                           parentesco>=700 & parentesco <800))

salud <- left_join(salud, ocupados, by = c("folioviv", "foliohog", "numren"))

salud <- mutate(salud, 
                # PEA (personas de 16 años o más)
                pea=case_when(trab==1 & (edad>=16 & !is.na(edad)) ~ 1, # PEA: ocupada
                              (act_pnea1==1 | act_pnea2==1) & 
                                (edad>=16 & !is.na(edad)) ~ 2, # PEA: desocupada
                              (edad>=16 & !is.na(edad)) & 
                                ((act_pnea1!=1 | is.na(act_pnea1)) & 
                                   (act_pnea2!=1 | is.na(act_pnea2))) & 
                                ((act_pnea1>=2 & act_pnea1<=6) | 
                                   (act_pnea2>=2 & act_pnea2<=6)) ~ 0), # PNEA
                # Tipo de trabajo
                # Ocupación principal
                tipo_trab1=ifelse(pea==1, tipo_trab1, tipo_trab1), # Depende de un patrón, jefe o superior  
                tipo_trab1=ifelse((pea==0 | pea==2), NA_real_, tipo_trab1), # No depende de un jefe y recibe o tiene asignado un sueldo
                tipo_trab1=ifelse(is.na(pea), NA_real_, tipo_trab1), # No depende de un jefe y no recibe o no tiene asignado un sueldo
                
                # Ocupación secundaria
                tipo_trab2=ifelse(pea==1, tipo_trab2, tipo_trab2), # Depende de un patrón, jefe o superior  
                tipo_trab2=ifelse((pea==0 | pea==2), NA_real_, tipo_trab2), # No depende de un jefe y recibe o tiene asignado un sueldo
                tipo_trab2=ifelse(is.na(pea), NA_real_, tipo_trab2)) # No depende de un jefe y no recibe o no tiene asignado un sueldo

# Servicios médicos prestaciones laborales
salud <- mutate(salud, 
                # Ocupación principal
                smlab1=case_when(ocupa1==1 & atemed==1 & 
                                   (inst_1==1 | inst_2==2 |  inst_3==3 |inst_4==4) & 
                                   (inscr_1==1) ~ 1, # Con servicios médicos
                                 ocupa1==1 ~ 0), # Sin servicios médicos
                # Ocupación secundaria
                smlab2=case_when(ocupa2==1 & atemed==1 & 
                                   (inst_1==1 | inst_2==2 |inst_3==3 | inst_4==4) & 
                                   (inscr_1==1) ~  1, # Con servicios médicos  
                                 ocupa2==1 ~ 0), # Sin servicios médicos
                # Contratación voluntaria de servicios médicos
                smcv=case_when(atemed==1 & 
                                 (inst_1==1 | inst_2==2 | inst_3==3 | inst_4==4) & 
                                 inscr_6==6 & (edad>=12 & !is.na(edad)) ~  1, # Sí cuenta
                               (edad>=12 & !is.na(edad)) ~ 0)) # No cuenta

# Acceso directo a servicios de salud
salud <- mutate(salud, 
                sa_dir=case_when(
                  # Ocupación principal
                  tipo_trab1==1 & (smlab1==1) ~ 1, # Con acceso
                  tipo_trab1==2 & (smlab1==1 | smcv==1) ~ 1, # Con acceso
                  tipo_trab1==3 & (smlab1==1 | smcv==1) ~ 1, # Con acceso
                  # Ocupación secundaria
                  tipo_trab2==1 & (smlab2==1) ~ 1, # Con acceso
                  tipo_trab2==2 & (smlab2==1 | smcv==1) ~ 1, # Con acceso
                  tipo_trab2==3 & (smlab2==1 | smcv==1 ) ~ 1, # Con acceso
                  TRUE ~0)) # Sin acceso

# Núcleos familiares
salud <- mutate(salud,
                par=case_when((parentesco>=100 & parentesco<200) ~ 1, # Jefe o jefa del hogar 
                              (parentesco>=200 & parentesco<300) ~ 2, # Cónyuge del  jefe/a 
                              (parentesco>=300 & parentesco<400) ~ 3, # Hijo del jefe/a 
                              parentesco==601 ~ 4, # Padre o Madre del jefe/a
                              parentesco==615 ~ 5, # Suegro del jefe/a
                              TRUE ~ 6), # Sin parentesco directo
                
                # Asimismo, se utilizará la información relativa a la asistencia a la escuela
                inas_esc=case_when(asis_esc==1 ~ 0,   # Sí asiste
                                   asis_esc==2 ~ 1 )) # No asiste

# En primer lugar se identifican los principales parentescos respecto a la 
# jefatura del hogar y si ese miembro cuenta con acceso directo
salud <- mutate(salud,
                jef=case_when(par==1 & sa_dir==1 & 
                                (((inst_2==2 | inst_3==3) & inscr_6==6) & 
                                   (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) &
                                   (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                      is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7))) ~ NA_real_,
                              par==1 & sa_dir==1 ~ 1),
                cony=case_when(par==2 & sa_dir==1 & 
                                 (((inst_2==2 | inst_3==3) & inscr_6==6) &
                                    (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) &
                                    (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                       is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7) )) ~ NA_real_,
                               par==2 & sa_dir==1 ~ 1),
                hijo=case_when(par==3 & sa_dir==1 & 
                                 (((inst_2==2 | inst_3==3) & inscr_6==6) & 
                                    (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) & 
                                    (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                       is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7) )) ~ NA_real_,
                               par==3 & sa_dir==1  ~ 1))

salud <- as.data.table(salud)[, c("jef_sa", "cony_sa", "hijo_sa") :=
                                .(sum(jef, na.rm=TRUE),
                                  sum(cony, na.rm=TRUE),
                                  sum(hijo, na.rm=TRUE)), by=.(folioviv, foliohog)] %>% 
  mutate(jef_sa=if_else(jef_sa>0, 1,jef_sa),  # Acceso directo a servicios de salud de la jefatura del hogar
         cony_sa=if_else(cony_sa>0, 1,cony_sa), # Acceso directo a servicios de salud del cónyuge de la jefatura del hogar
         hijo_sa=if_else(hijo_sa>0, 1,hijo_sa), # Acceso directo a servicios de salud de hijos(as) de la jefatura del hogar
         
         # Otros núcleos familiares: se identifica a la población con acceso a servicios de salud
         # mediante otros núcleos familiares a través de la afiliación
         # o inscripción a servicios de salud por algún familiar dentro o 
         # fuera del hogar, muerte del asegurado o por contratación propia;
         
         s_salud=case_when(atemed==1 & (inst_1==1 | inst_2==2 | inst_3==3 | inst_4==4) & 
                             (inscr_3==3| inscr_4==4 | inscr_6==6 | inscr_7==7) ~ 1, # Sí cuenta
                           !is.na(pop_insabi) & !is.na(atemed) ~ 0)) # No cuenta

# Indicador de carencia por servicios de salud

# Se considera en situación de carencia por acceso a servicios de salud
# a la población que:
#  
#  1. No cuente con adscripción o derecho a recibir servicios médicos de alguna 
#     institución  que  los  preste,  incluyendo  el  Seguro  Popular,  las  
#     instituciones  de  seguridad  social  (IMSS,  ISSSTE  federal  o  estatal,  
#     PEMEX, Ejército o Marina) o los servicios médicos privados

salud <- mutate(salud,
                
                # Indicador de carencia por acceso a los servicios de salud
                
                # Acceso directo
                ic_asalud=case_when(sa_dir==1 ~ 0,
                                    # Parentesco directo: jefatura
                                    par==1 & cony_sa==1 ~ 0, # No presenta carencia
                                    par==1 & pea==0 & hijo_sa==1 ~ 0, # No presenta carencia
                                    # Parentesco directo: cónyuge
                                    par==2 & jef_sa==1 ~ 0, # No presenta carencia
                                    par==2 & pea==0 & hijo_sa==1 ~ 0, # No presenta carencia
                                    # Parentesco directo: descendientes
                                    par==3 & edad<16 & jef_sa==1 ~ 0, # No presenta carencia
                                    par==3 & edad<16 & cony_sa==1 ~ 0, # No presenta carencia
                                    par==3 & (edad>=16 & edad<=25) & inas_esc==0 & jef_sa==1 ~ 0, # No presenta carencia
                                    par==3 & (edad>=16 & edad<=25) & inas_esc==0 & cony_sa==1 ~ 0, # No presenta carencia
                                    # Parentesco directo: ascendientes
                                    par==4 & pea==0 & jef_sa==1 ~ 0, # No presenta carencia
                                    par==5 & pea==0 & cony_sa==1 ~ 0, # No presenta carencia
                                    # Otros núcleos familiares
                                    s_salud==1 ~ 0, # No presenta carencia
                                    # Acceso reportado
                                    pop_insabi==1 | (pop_insabi==2 & atemed==1 & 
                                                       (inst_1==1 | inst_2==2 | inst_3==3 | 
                                                          inst_4==4 | inst_5==5 | inst_6==6)) | 
                                      segvol_2==2 ~ 0, # No presenta carencia
                                    TRUE~ 1), # Presenta carencia
                # Población con presencia de discapacidad, sea física o mental
                discap=case_when((disc_camin>="1" & disc_camin<="2") ~1, # Con presencia de discapacidad
                                 (disc_ver  >="1" & disc_ver  <="2") ~1, # Con presencia de discapacidad
                                 (disc_brazo>="1" & disc_brazo<="2") ~1, # Con presencia de discapacidad
                                 (disc_apren>="1" & disc_apren<="2") ~1, # Con presencia de discapacidad  
                                 (disc_oir  >="1" & disc_oir  <="2") ~1, # Con presencia de discapacidad
                                 (disc_vest >="1" & disc_vest <="2") ~1, # Con presencia de discapacidad
                                 (disc_habla>="1" & disc_habla<="2") ~1, # Con presencia de discapacidad 
                                 (disc_acti>="1" & disc_acti<="2")  ~1,  # Con presencia de discapacidad
                                 (disc_camin=="&" & disc_ver=="&" &
                                    disc_brazo=="&" & disc_apren=="&" &
                                    disc_oir=="&" & disc_vest=="&" &
                                    disc_habla=="&" & disc_acti=="&") ~ NA_real_,
                                 TRUE ~0)) %>%  # Sin presencia de discapacidad
  select(folioviv, foliohog, numren, sexo, 
         starts_with("sa_"), ends_with("_sa"), 
         pop_insabi, atemed, starts_with("inst_"),
         starts_with("inscr_"), starts_with("segvol_"), starts_with("inst_"),
         ic_asalud,discap, aten_sal, prob_sal, starts_with("servmed_"),prob_anio,segvol_2)


fwrite(salud, paste_out("ic_asalud_20.csv"), row.names=F)

salud_20 <- salud

## tablita para afiliación -----
viviendas <- viviendas%>%
  select(folioviv, factor)


pob_salud <- salud #%>%
# filter(
#   ic_asalud == 1
# )

sbv <- left_join(pob_salud, viviendas, by = c("folioviv")) 

f <- sbv %>%
  mutate(across(
    .cols = c(starts_with("inst_"),segvol_2),
    .fns = ~ case_when(
      !is.na(.) ~ 1,
      is.na(.)  ~ 0
    )
  ))

pob_w <- as_survey_design(f, weights=factor, nest=TRUE) #%>% srvyr::filter(!is.na(servmed_1))

# Define variables of interest
vars <- colnames(select(f, starts_with("inst_"),ic_asalud,segvol_2))


por <- as.data.frame(matrix(unlist(pob_w %>% 
                                     srvyr::select(vars) %>%
                                     summarise_all(survey_mean, vartype=NULL)), 
                            ncol = 1, byrow=T)*100) %>% round(3)

tot <- as.data.frame(matrix(unlist(pob_w %>% 
                                     srvyr::select(vars) %>%
                                     summarise_all(survey_total, vartype=NULL)), 
                            ncol =1 , byrow=T) / 1000000)

nac <- bind_cols(por, tot)

rownames(nac) <- c("inst_1","inst_2","inst_3","inst_4","inst_5","inst_6","ic_asalud","segvol_2"
)

colnames(nac) <- c("Porcentaje", "Millones de personas")
nac

row.names(nac) <- salud_labels[row.names(nac)]

assign(paste0("nac_afiliad_", anio), nac)


## tablita para atención------
viviendas <- viviendas%>%
  select(folioviv, factor)


pob_salud <- salud %>%
  filter(
    aten_sal == 1,
    prob_sal == 1 ,
    prob_anio == anio
  )

sbv <- left_join(pob_salud, viviendas, by = c("folioviv")) 

f <- sbv %>%
  mutate(across(
    .cols = c(starts_with("servmed_")),
    .fns = ~ case_when(
      !is.na(.) ~ 1,
      is.na(.)  ~ 0
    )
  ))

f_long <- f %>%
  pivot_longer(
    cols = starts_with("servmed_"),
    names_to = "servicio",
    values_to = "usado"
  ) %>%
  filter(usado == 1)

pob_long <- as_survey_design(f_long, weights = factor, nest = TRUE)

nac <- pob_long %>%
  group_by(servicio) %>%
  summarise(visitas = survey_total(vartype = NULL)) %>%
  ungroup() %>%
  mutate(
    Porcentaje = round(100 * visitas / sum(visitas), 2),
    `Millones de visitas` = visitas / 1e6
  ) %>%
  select(servicio, Porcentaje, `Millones de visitas`)

nac$servicio <- salud_labels[nac$servicio]

assign(paste0("nac_aten_", anio), nac)

## 2022 -----------

ocupados <- ocupados_22
pobla <- pobla_22
viviendas <- viviendas_22
anio <- 2022

# Tipo de trabajador: identifica la población subordinada e independiente
ocupados <-mutate(ocupados, 
                  tipo_trab=case_when(
                    #Subordinados
                    subor==1 ~ 1,
                    #Independientes que reciben un pago
                    subor==2 & indep==1 & tiene_suel==1 ~ 2,
                    subor==2 & indep==2 & pago==1 ~ 2,
                    #Independientes que no reciben un pago
                    subor==2 & indep==1 & tiene_suel==2 ~ 3,
                    subor==2 & indep==2 & (pago==2 | pago==3) ~ 3))

# Ocupación principal o secundaria
ocupados <- mutate(ocupados, 
                   ocupa=case_when(id_trabajo==1 ~ 1, id_trabajo==2 ~ 1)) %>% 
  dplyr::select(folioviv, foliohog, numren, id_trabajo, tipo_trab, ocupa)

# Distinción de prestaciones en trabajo principal y secundario
ocupados <- dcast(as.data.table(ocupados),  folioviv + foliohog + numren ~ 
                    id_trabajo, value.var=c("tipo_trab", "ocupa"), sep="", fill=0)

ocupados <- mutate(ocupados, 
                   # Identificación de la población trabajadora 
                   # (toda la que reporta al menos un empleo en la base de trabajos.csv)
                   trab=1) %>%
  select(folioviv, foliohog, numren, trab,starts_with("tipo_trab"), 
         starts_with("ocupa"))


fwrite(ocupados, paste_out("ocupados22.csv"), row.names=F)

ocupados_22 <- ocupados

# Población objetivo: no se incluye a huéspedes ni trabajadores domésticos
salud <- pobla
salud <- filter(salud, !(parentesco>=400 & parentesco <500 |
                           parentesco>=700 & parentesco <800))

salud <- left_join(salud, ocupados, by = c("folioviv", "foliohog", "numren"))

salud <- mutate(salud, 
                # PEA (personas de 16 años o más)
                pea=case_when(trab==1 & (edad>=16 & !is.na(edad)) ~ 1, # PEA: ocupada
                              (act_pnea1==1 | act_pnea2==1) & 
                                (edad>=16 & !is.na(edad)) ~ 2, # PEA: desocupada
                              (edad>=16 & !is.na(edad)) & 
                                ((act_pnea1!=1 | is.na(act_pnea1)) & 
                                   (act_pnea2!=1 | is.na(act_pnea2))) & 
                                ((act_pnea1>=2 & act_pnea1<=6) | 
                                   (act_pnea2>=2 & act_pnea2<=6)) ~ 0), # PNEA
                # Tipo de trabajo
                # Ocupación principal
                tipo_trab1=ifelse(pea==1, tipo_trab1, tipo_trab1), # Depende de un patrón, jefe o superior  
                tipo_trab1=ifelse((pea==0 | pea==2), NA_real_, tipo_trab1), # No depende de un jefe y recibe o tiene asignado un sueldo
                tipo_trab1=ifelse(is.na(pea), NA_real_, tipo_trab1), # No depende de un jefe y no recibe o no tiene asignado un sueldo
                
                # Ocupación secundaria
                tipo_trab2=ifelse(pea==1, tipo_trab2, tipo_trab2), # Depende de un patrón, jefe o superior  
                tipo_trab2=ifelse((pea==0 | pea==2), NA_real_, tipo_trab2), # No depende de un jefe y recibe o tiene asignado un sueldo
                tipo_trab2=ifelse(is.na(pea), NA_real_, tipo_trab2)) # No depende de un jefe y no recibe o no tiene asignado un sueldo

# Servicios médicos prestaciones laborales
salud <- mutate(salud, 
                # Ocupación principal
                smlab1=case_when(ocupa1==1 & atemed==1 & 
                                   (inst_1==1 | inst_2==2 |  inst_3==3 |inst_4==4) & 
                                   (inscr_1==1) ~ 1, # Con servicios médicos
                                 ocupa1==1 ~ 0), # Sin servicios médicos
                # Ocupación secundaria
                smlab2=case_when(ocupa2==1 & atemed==1 & 
                                   (inst_1==1 | inst_2==2 |inst_3==3 | inst_4==4) & 
                                   (inscr_1==1) ~  1, # Con servicios médicos  
                                 ocupa2==1 ~ 0), # Sin servicios médicos
                # Contratación voluntaria de servicios médicos
                smcv=case_when(atemed==1 & 
                                 (inst_1==1 | inst_2==2 | inst_3==3 | inst_4==4) & 
                                 inscr_6==6 & (edad>=12 & !is.na(edad)) ~  1, # Sí cuenta
                               (edad>=12 & !is.na(edad)) ~ 0)) # No cuenta

# Acceso directo a servicios de salud
salud <- mutate(salud, 
                sa_dir=case_when(
                  # Ocupación principal
                  tipo_trab1==1 & (smlab1==1) ~ 1, # Con acceso
                  tipo_trab1==2 & (smlab1==1 | smcv==1) ~ 1, # Con acceso
                  tipo_trab1==3 & (smlab1==1 | smcv==1) ~ 1, # Con acceso
                  # Ocupación secundaria
                  tipo_trab2==1 & (smlab2==1) ~ 1, # Con acceso
                  tipo_trab2==2 & (smlab2==1 | smcv==1) ~ 1, # Con acceso
                  tipo_trab2==3 & (smlab2==1 | smcv==1 ) ~ 1, # Con acceso
                  TRUE ~0)) # Sin acceso

# Núcleos familiares
salud <- mutate(salud,
                par=case_when((parentesco>=100 & parentesco<200) ~ 1, # Jefe o jefa del hogar 
                              (parentesco>=200 & parentesco<300) ~ 2, # Cónyuge del  jefe/a 
                              (parentesco>=300 & parentesco<400) ~ 3, # Hijo del jefe/a 
                              parentesco==601 ~ 4, # Padre o Madre del jefe/a
                              parentesco==615 ~ 5, # Suegro del jefe/a
                              TRUE ~ 6), # Sin parentesco directo
                
                # Asimismo, se utilizará la información relativa a la asistencia a la escuela
                inas_esc=case_when(asis_esc==1 ~ 0,   # Sí asiste
                                   asis_esc==2 ~ 1 )) # No asiste

# En primer lugar se identifican los principales parentescos respecto a la 
# jefatura del hogar y si ese miembro cuenta con acceso directo
salud <- mutate(salud,
                jef=case_when(par==1 & sa_dir==1 & 
                                (((inst_2==2 | inst_3==3) & inscr_6==6) & 
                                   (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) &
                                   (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                      is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7))) ~ NA_real_,
                              par==1 & sa_dir==1 ~ 1),
                cony=case_when(par==2 & sa_dir==1 & 
                                 (((inst_2==2 | inst_3==3) & inscr_6==6) &
                                    (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) &
                                    (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                       is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7) )) ~ NA_real_,
                               par==2 & sa_dir==1 ~ 1),
                hijo=case_when(par==3 & sa_dir==1 & 
                                 (((inst_2==2 | inst_3==3) & inscr_6==6) & 
                                    (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) & 
                                    (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                       is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7) )) ~ NA_real_,
                               par==3 & sa_dir==1  ~ 1))

salud <- as.data.table(salud)[, c("jef_sa", "cony_sa", "hijo_sa") :=
                                .(sum(jef, na.rm=TRUE),
                                  sum(cony, na.rm=TRUE),
                                  sum(hijo, na.rm=TRUE)), by=.(folioviv, foliohog)] %>% 
  mutate(jef_sa=if_else(jef_sa>0, 1,jef_sa),  # Acceso directo a servicios de salud de la jefatura del hogar
         cony_sa=if_else(cony_sa>0, 1,cony_sa), # Acceso directo a servicios de salud del cónyuge de la jefatura del hogar
         hijo_sa=if_else(hijo_sa>0, 1,hijo_sa), # Acceso directo a servicios de salud de hijos(as) de la jefatura del hogar
         
         # Otros núcleos familiares: se identifica a la población con acceso a servicios de salud
         # mediante otros núcleos familiares a través de la afiliación
         # o inscripción a servicios de salud por algún familiar dentro o 
         # fuera del hogar, muerte del asegurado o por contratación propia;
         
         s_salud=case_when(atemed==1 & (inst_1==1 | inst_2==2 | inst_3==3 | inst_4==4) & 
                             (inscr_3==3| inscr_4==4 | inscr_6==6 | inscr_7==7) ~ 1, # Sí cuenta
                           !is.na(pop_insabi) & !is.na(atemed) ~ 0)) # No cuenta

# Indicador de carencia por servicios de salud

# Se considera en situación de carencia por acceso a servicios de salud
# a la población que:
#  
# 1. No se encuentra inscrita al Seguro Popular* o afiliada a alguna institución 
#    por prestación laboral, contratación voluntaria o afiliación de un 
#    familiar por parentesco directo a recibir servicios médicos por alguna
#    institución que los preste como: las instituciones de seguridad social 
#    (IMSS, ISSSTE federal o estatal, Pemex, Ejército o Marina), los servicios 
#    médicos privados, u otra institución médica.
# 
# *Se reporta la población que respondió estar afiliado o inscrito
# al Seguro Popular, o que tiene derecho a los servicios del
# Instituto de Salud para el Bienestar (INSABI), lo anterior
# de acuerdo con el cuestionario de la ENIGH 2022.


salud <- mutate(salud,
                
                # Indicador de carencia por acceso a los servicios de salud
                
                # Acceso directo
                ic_asalud=case_when(sa_dir==1 ~ 0,
                                    # Parentesco directo: jefatura
                                    par==1 & cony_sa==1 ~ 0, # No presenta carencia
                                    par==1 & pea==0 & hijo_sa==1 ~ 0, # No presenta carencia
                                    # Parentesco directo: cónyuge
                                    par==2 & jef_sa==1 ~ 0, # No presenta carencia
                                    par==2 & pea==0 & hijo_sa==1 ~ 0, # No presenta carencia
                                    # Parentesco directo: descendientes
                                    par==3 & edad<16 & jef_sa==1 ~ 0, # No presenta carencia
                                    par==3 & edad<16 & cony_sa==1 ~ 0, # No presenta carencia
                                    par==3 & (edad>=16 & edad<=25) & inas_esc==0 & jef_sa==1 ~ 0, # No presenta carencia
                                    par==3 & (edad>=16 & edad<=25) & inas_esc==0 & cony_sa==1 ~ 0, # No presenta carencia
                                    # Parentesco directo: ascendientes
                                    par==4 & pea==0 & jef_sa==1 ~ 0, # No presenta carencia
                                    par==5 & pea==0 & cony_sa==1 ~ 0, # No presenta carencia
                                    # Otros núcleos familiares
                                    s_salud==1 ~ 0, # No presenta carencia
                                    # Acceso reportado
                                    pop_insabi==1 | (pop_insabi==2 & atemed==1 & 
                                                       (inst_1==1 | inst_2==2 | inst_3==3 | 
                                                          inst_4==4 | inst_5==5 | inst_6==6)) | 
                                      segvol_2==2 ~ 0, # No presenta carencia
                                    TRUE~ 1), # Presenta carencia
                # Población con presencia de discapacidad, sea física o mental
                discap=case_when((disc_camin>="1" & disc_camin<="2") ~1, # Con presencia de discapacidad
                                 (disc_ver  >="1" & disc_ver  <="2") ~1, # Con presencia de discapacidad
                                 (disc_brazo>="1" & disc_brazo<="2") ~1, # Con presencia de discapacidad
                                 (disc_apren>="1" & disc_apren<="2") ~1, # Con presencia de discapacidad  
                                 (disc_oir  >="1" & disc_oir  <="2") ~1, # Con presencia de discapacidad
                                 (disc_vest >="1" & disc_vest <="2") ~1, # Con presencia de discapacidad
                                 (disc_habla>="1" & disc_habla<="2") ~1, # Con presencia de discapacidad 
                                 (disc_acti>="1" & disc_acti<="2")  ~1,  # Con presencia de discapacidad
                                 (disc_camin=="&" & disc_ver=="&" &
                                    disc_brazo=="&" & disc_apren=="&" &
                                    disc_oir=="&" & disc_vest=="&" &
                                    disc_habla=="&" & disc_acti=="&") ~ NA_real_,
                                 TRUE ~0)) %>%  # Sin presencia de discapacidad
  select(folioviv, foliohog, numren, sexo, 
         starts_with("sa_"), ends_with("_sa"), 
         pop_insabi, atemed, starts_with("inst_"),
         starts_with("inscr_"), starts_with("segvol_"), starts_with("inst_"),
         ic_asalud,discap, aten_sal, prob_sal, starts_with("servmed_"), prob_anio,segvol_2)

fwrite(salud, paste_out("ic_asalud_22.csv"), row.names=F)

salud_20 <- salud

## tablita para afiliación -----
viviendas <- viviendas%>%
  select(folioviv, factor)


pob_salud <- salud #%>%
# filter(
#   ic_asalud == 1
# )

sbv <- left_join(pob_salud, viviendas, by = c("folioviv")) 

f <- sbv %>%
  mutate(across(
    .cols = c(starts_with("inst_"),segvol_2),
    .fns = ~ case_when(
      !is.na(.) ~ 1,
      is.na(.)  ~ 0
    )
  ))

pob_w <- as_survey_design(f, weights=factor, nest=TRUE) #%>% srvyr::filter(!is.na(servmed_1))

# Define variables of interest
vars <- colnames(select(f, starts_with("inst_"),ic_asalud,segvol_2))


por <- as.data.frame(matrix(unlist(pob_w %>% 
                                     srvyr::select(vars) %>%
                                     summarise_all(survey_mean, vartype=NULL)), 
                            ncol = 1, byrow=T)*100) %>% round(3)

tot <- as.data.frame(matrix(unlist(pob_w %>% 
                                     srvyr::select(vars) %>%
                                     summarise_all(survey_total, vartype=NULL)), 
                            ncol =1 , byrow=T) / 1000000)

nac <- bind_cols(por, tot)

rownames(nac) <- c("inst_1","inst_2","inst_3","inst_4","inst_5","inst_6","ic_asalud","segvol_2"
)

colnames(nac) <- c("Porcentaje", "Millones de personas")
nac

row.names(nac) <- salud_labels[row.names(nac)]

assign(paste0("nac_afiliad_", anio), nac)


## tablita para atención------
viviendas <- viviendas%>%
  select(folioviv, factor)


pob_salud <- salud %>%
  filter(
    aten_sal == 1,
    prob_sal == 1 ,
    prob_anio == anio
  )

sbv <- left_join(pob_salud, viviendas, by = c("folioviv")) 

f <- sbv %>%
  mutate(across(
    .cols = c(starts_with("servmed_")),
    .fns = ~ case_when(
      !is.na(.) ~ 1,
      is.na(.)  ~ 0
    )
  ))

f_long <- f %>%
  pivot_longer(
    cols = starts_with("servmed_"),
    names_to = "servicio",
    values_to = "usado"
  ) %>%
  filter(usado == 1)

pob_long <- as_survey_design(f_long, weights = factor, nest = TRUE)

nac <- pob_long %>%
  group_by(servicio) %>%
  summarise(visitas = survey_total(vartype = NULL)) %>%
  ungroup() %>%
  mutate(
    Porcentaje = round(100 * visitas / sum(visitas), 2),
    `Millones de visitas` = visitas / 1e6
  ) %>%
  select(servicio, Porcentaje, `Millones de visitas`)

nac$servicio <- salud_labels[nac$servicio]

assign(paste0("nac_aten_", anio), nac)


# B. Cargar base general (la grande) de ent -----------
pob_16 <- read_csv("02_datos_crudos/pobreza_16.csv")%>%
  mutate(
    año = as.numeric(2016)
  )
pob_18 <- read_csv("02_datos_crudos/pobreza_18.csv")%>%
  mutate(
    año = as.numeric(2018)
  )
pob_20 <- read_csv("02_datos_crudos/pobreza_20.csv")%>%
  mutate(
    año = as.numeric(2020)
  )
pob_22 <- read_csv("02_datos_crudos/pobreza_22.csv")%>%
  mutate(
    año = as.numeric(2022)
  )

# C. Base de entidades generales (cuadro chiquito con entidades) --------

pob_ent_16 <- read_csv("02_datos_crudos/pob_ent_16.csv")%>%
  mutate(
    año = as.numeric(2016)
  )%>%
  pivot_longer(
    cols = ends_with(c(".x", ".y")),
    names_to = c("categoria", ".value"),
    names_sep = "\\."
  ) %>%
  rename(
    porcentaje = x,
    value = y
  )

pob_ent_18 <- read_csv("02_datos_crudos/pob_ent_18.csv")%>%
  mutate(
    año = as.numeric(2018)
  )%>%
  pivot_longer(
    cols = ends_with(c(".x", ".y")),
    names_to = c("categoria", ".value"),
    names_sep = "\\."
  ) %>%
  rename(
    porcentaje = x,
    value = y
  )

pob_ent_20 <- read_csv("02_datos_crudos/pob_ent_20.csv")%>%
  mutate(
    año = as.numeric(2020)
  )%>%
  pivot_longer(
    cols = ends_with(c(".x", ".y")),
    names_to = c("categoria", ".value"),
    names_sep = "\\."
  ) %>%
  rename(
    porcentaje = x,
    value = y
  )

pob_ent_22 <- read_csv("02_datos_crudos/pob_ent_22.csv")%>%
  mutate(
    año = as.numeric(2022)
  )%>%
  pivot_longer(
    cols = ends_with(c(".x", ".y")),
    names_to = c("categoria", ".value"),
    names_sep = "\\."
  ) %>%
  rename(
    porcentaje = x,
    value = y
  )

pob_ent_NAC <- read_csv("02_datos_crudos/df_pob_nacional.csv")%>%
  rename(
    categoria   = "indicador",
    porcentaje  = "percent",
    value       = "millones",
    año         = "anio"
  ) %>%
  mutate(
    ent = 0,
    porcentaje = porcentaje / 100  # <-- Convert percentage to proportion
  ) %>%
  select(ent, año, categoria, porcentaje, value)


df_combined_ent_NODECIL <- bind_rows(pob_ent_16, pob_ent_18, pob_ent_20, pob_ent_22, pob_ent_NAC) %>%
  mutate(
    cve_ent = sprintf("%02d", ent),  # convert to character with leading 0s
    nom_ent = recode(cve_ent, !!!entidad_labels))

fwrite(df_combined_ent_NODECIL, paste_out("entidadesCATS.csv"))

# 1. Transformación df_general -------------------------

#creación de función deciles
create_decil_groups <- function(population_df) {
  df_lineas <- population_df %>%
    mutate(decil = xtile(ictpc, n = 10, wt = factor))
  return(df_lineas)
}

#creación deciles para cada df (tablas grandotas)
df_lineas_2016 <- create_decil_groups(pob_16)
df_lineas_2018 <- create_decil_groups(pob_18)
df_lineas_2020 <- create_decil_groups(pob_20)
df_lineas_2022 <- create_decil_groups(pob_22)

#promedios - solo para analizar
prov_pc_16 <- df_lineas_2016 %>%
  drop_na(ictpc) %>%
  as_survey_design(weights = factor) %>%
  group_by(año, decil) %>%
  summarise(
    ictpc = survey_mean(ictpc, na.rm = T)
  ) %>%
  select(-ends_with("se")) %>%
  pivot_wider(
    names_from = año,
    values_from = ictpc
  )

# función para generar df as survey design por año por decil
calc_stats <- function(df_input, year, decil_num) { #(df que deseo transformar como survey, año que solo importa para agregar la columna de año y le ponga el número de año que le pones, el número de decil que se desea transformar como survey)
  
  #survey design
  pob_w <- as_survey_design(df_input, weights = factor, nest = TRUE) %>%
    srvyr::filter(!is.na(pobreza))
  
  #Define variables of interest
  vars <- colnames(select(pob_w, pobreza, pobreza_m, pobreza_e, vul_car, 
                          vul_ing,no_pobv, carencias, carencias3, ic_rezedu, 
                          ic_asalud, ic_segsoc, ic_cv, ic_sbv, ic_ali_nc, 
                          plp_e, plp))
  
  #porcentaje
  por <- pob_w %>%
    filter(decil == decil_num) %>%
    srvyr::select(vars) %>%
    summarise_all(survey_mean, vartype = NULL) %>%
    gather(key = "categoria", value = "percentage") %>%
    mutate(percentage = round(percentage * 100, 6))
  
  #totales
  tot <- pob_w %>%
    filter(decil == decil_num) %>%
    srvyr::select(vars) %>%
    summarise_all(survey_total, vartype = NULL) %>%
    gather(key = "categoria", value = "value") %>%
    mutate(value = value / 1000000)
  
  #juntar resultados
  result <- por %>%
    left_join(tot, by = "categoria") %>%
    mutate(decil = decil_num,
           año = year) %>%
    select(categoria, decil, año, value, percentage)
  
  return(result)
}

#función que utiliza la función para calcular las estadísticas de survey design por cada decil
process_year <- function(df_lineas, year) {
  
  map_dfr(1:10, ~calc_stats(df_lineas, year = year, decil_num = .x)) #map_dfr es para calcular resultados por línea, .x solo va tomando cada cifra específicada, en este caso del 1:10.
  
}

#Procesar bases
df_2016 <- process_year(df_lineas_2016, 2016)
df_2018 <- process_year(df_lineas_2018, 2018)
df_2020 <- process_year(df_lineas_2020, 2020)
df_2022 <- process_year(df_lineas_2022, 2022)

#Juntar bases y agregar romanos a deciles
df_combined <- bind_rows(df_2016, df_2018, df_2020, df_2022) %>%
  arrange(año, decil, categoria)%>%
  mutate(decil=case_when(decil==1  ~ "I",
                         decil==2  ~ "II",
                         decil==3  ~ "III",
                         decil==4  ~ "IV",
                         decil==5  ~ "V",
                         decil==6  ~ "VI",
                         decil==7  ~ "VII",
                         decil==8  ~ "VIII",
                         decil==9  ~ "IX",
                         decil==10 ~	"X"),
         decil = factor(decil, levels = c("I",
                                          "II",
                                          "III",
                                          "IV",
                                          "V",
                                          "VI",
                                          "VII",
                                          "VIII",
                                          "IX",
                                          "X")))

fwrite(df_combined, paste_out("decilesCATS.csv"))


#comprobando estadísticas con gráfica 30, debería de ser 50.4 para 2022, 35.7 para 2020, 20.1 para 2018
#link: https://www.coneval.org.mx/Medicion/MP/Documents/MMP_2022/Documento_de_analisis_sobre_la_medicion_multidimensional_de_la_pobreza_2022.pdf
suma2 <- df_combined%>%
  filter(categoria == "ic_asalud",
         año == 2020)%>%
  summarise(total_value = sum(value, na.rm = TRUE))


# 2. infobites ----------
category_labels <- c(
  "pobreza" = "Pobreza",
  "pobreza_m" = "Pobreza moderada",
  "pobreza_e" = "Pobreza extrema",
  "vul_car" = "Vulnerable por carencias",
  "vul_ing" = "Vulnerable por ingresos",
  "no_pobv" = "No pobres no vulnerables",
  "carencias" = "Con carencia",
  "carencias3" = "3+ carencias",
  "ic_rezedu" = "Carenica rezago educativo",
  "ic_asalud" = "Carencia acceso a salud",
  "ic_segsoc" = "Carencia seguridad social",
  "ic_cv" = "Carencia calidad vivienda",
  "ic_sbv" = "Carencia servicios vivienda",
  "ic_ali_nc" = "Carencia alimentación",
  "plp_e" = "Ingreso menor a línea de pobreza extrema",
  "plp" = "Ingreso menor a línea de pobreza"
)

## 1. barras por deciles cambio en los porcentajes  (graf 31)-------
change_rates <- df_combined %>%
  filter(categoria == "ic_asalud") %>%
  group_by(decil) %>%
  arrange(año) %>%
  mutate(
    change_por = percentage - lag(percentage),
    period = paste0(lag(año), "–", año)
  ) %>%
  filter(!is.na(change_por)) %>%
  ungroup()

total_change <- df_combined %>%
  filter(categoria == "ic_asalud") %>%
  group_by(decil) %>%
  arrange(año) %>%
  summarise(total_change = last(percentage) - first(percentage)) %>%
  ungroup()

g <- ggplot(change_rates, aes(x = factor(decil), y = change_por, fill = period)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_point(
    data = total_change,
    aes(x = factor(decil), y = total_change, fill = "Cambio total 2016-2022"),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.5
  )+
  geom_text(
    data = total_change,
    aes(x = factor(decil), y = total_change, label = round(total_change, 1)),
    vjust = -0.9,
    inherit.aes = FALSE,
    size = 4
  ) +
  geom_text(
    aes(label = round(change_por, 1)),
    position = position_dodge(width = 0.8),
    vjust = -.34,   
    size = 4
  ) +
  scale_fill_manual(values = c(
    "2016–2018" = "#377eb8",
    "2018–2020" = "#4daf4a",
    "2020–2022" = "#984ea3",
    "Cambio total 2016-2022" = "black"
  ))+
  scale_y_continuous(breaks = pretty_breaks(5),
                     expand = expansion(mult = c(0, 0.1)))+
  labs(title = str_wrap("Diferencia en el porcentaje de la población con carencia por acceso a los servicios de salud", width = 50),
       subtitle = "Por deciles del ICTPC",
       x = "Decil",
       y = "Cambio en puntos porcentuales",
       caption = "ICTPC: Ingreso corriente total per cápita",
       fill = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
        plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,5,0)),
        plot.caption = element_text(size = 20, colour = "#777777"),
        strip.text.x = element_text(size = 25),
        panel.grid.minor  = element_blank(),
        plot.margin= margin(0.3, 0.3, 1.6, 0.3, "cm"), # margin(top,right, bottom,left)
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(size = 15, family = "Ubuntu"),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 25, angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 25),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.position = "top")


g <- ggimage::ggbackground(g, paste_plant("00_plantillas/02_enigh.pdf"))
ggsave(g, filename = paste_inf("12_01_CambioPeriodos.png"),width = 16, height = 9, dpi = 200, bg= "transparent")


### 1.2 lollipops ------
df <- df_combined_ent_NODECIL %>%
  filter(categoria == "ic_asalud")%>%
  mutate(porcentaje = porcentaje * 100)


# Automatically detect comparison years
anio_inicio_comparacion <- 2018
anio_fin_comparacion <- max(df$año, na.rm = TRUE)


datos_cambios <- df %>%
  filter(año %in% c(anio_inicio_comparacion, anio_fin_comparacion)) %>%
  select(ent, año, porcentaje, nom_ent)%>%
  mutate(año = as.factor(año))


# ordenar por final year (como vector)
niveles <- datos_cambios %>%
  filter(año == anio_fin_comparacion) %>%
  arrange(porcentaje) %>%
  pull(nom_ent)

# aplicar el órden
datos_cambios <- datos_cambios %>%
  mutate(nom_ent = factor(nom_ent, levels = niveles)) %>%
  arrange(nom_ent)

# lado label
datos_cambios$lados <- lapply(niveles, function(i) {
  info_edo <- datos_cambios %>% 
    filter(nom_ent == i) %>% 
    arrange(año)
  
  if(info_edo$porcentaje[1] > info_edo$porcentaje[2]) {
    c("derecho", "izquierdo")
  } else {
    c("izquierdo", "derecho")
  }
}) %>% 
  unlist()

# crear segments
segmentos <- datos_cambios %>%
  pivot_wider(names_from = año, 
              values_from = porcentaje, 
              id_cols = nom_ent)%>%
  setNames(c("nom_ent", "anio_inicio", "anio_fin")) %>%
  mutate(
    diff = anio_fin - anio_inicio,
    xstart = if_else(diff > 0, anio_inicio + 0.9, anio_inicio - 0.9),
    xend   = if_else(diff > 0, anio_fin - 0.9, anio_fin + 0.9),
    color  = if_else(diff >= 0, "#9dd89d", "skyblue"))


# Plot
g <-  ggplot(datos_cambios,
             aes(y = nom_ent, 
                 x = porcentaje)) +
  geom_segment(data = segmentos,
               aes(x = xstart, xend = xend,
                   y = nom_ent, yend = nom_ent),
               size = 2,
               alpha = 0.5,
               color = segmentos$color,
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_point(aes(fill = año, color = año), size = 3, shape = 21) +
  geom_point(data = filter(datos_cambios, nom_ent == "Nacional"),
             aes(fill = año, color = año),
             size = 3, shape = 21,fill = "red", color = "red"
  ) +
  #texto quedandose del lado izq
  geom_text(aes(label = ifelse( año == anio_inicio_comparacion, str_c(round(porcentaje, 1), "%"), NA),
                hjust = lados, color = año),
            family = "Ubuntu", 
            nudge_x = -7 ,
            size = 8,
            show.legend = FALSE) +
  #texto quedandose del lado derecho
  geom_text(aes(label = ifelse( año == anio_fin_comparacion, str_c(round(porcentaje, 1), "%"), NA),
                hjust = lados, color = año),
            family = "Ubuntu", 
            nudge_x = 7 ,
            size = 8,
            show.legend = FALSE) +
  #texto quedanso del lado izquierdo ROJO NACIONAL
  geom_text(aes(label = ifelse(nom_ent == "Nacional" & año == anio_inicio_comparacion,str_c(round(porcentaje, 1), "%"),NA),
                hjust = lados, color = "red"),
            family = "Ubuntu", 
            size = 8,
            nudge_x = -7 ,
            show.legend = FALSE) +
  #texto quedanso del lado derecho ROJO NACIONAL
  geom_text(aes(label = ifelse(nom_ent == "Nacional" & año == anio_fin_comparacion,str_c(round(porcentaje, 1), "%"),NA),
                hjust = lados, color = "red"),
            family = "Ubuntu", 
            size = 8,
            nudge_x = 7 ,
            show.legend = FALSE) +
  scale_x_continuous( labels = percent_format(scale = 1),
                      expand = expansion(mult = c(0.1, 0.3))) +
  scale_fill_manual(values = c("2018" = "#1caff9", "2022" = "blue")) +
  scale_color_manual(values = c("2018" = "#1caff9", "2022" = "darkblue"))+
  labs(title = str_wrap(paste0("Cambio en carencia en salud ", anio_inicio_comparacion, "-",anio_fin_comparacion), width = 25),
       x = NULL, 
       y = NULL, 
       fill = NULL, 
       color = NULL) +
  theme_minimal()+
  theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
        plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,5,0)),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 25),
        panel.grid.minor  = element_blank(),
        plot.margin= margin(0.3, 0.3, 2.6, 0.3, "cm"), # margin(top,right, bottom,left)
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Ubuntu"),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.position = "top")


g <- ggimage::ggbackground(g, paste_plant("00_plantillas/01_inegi_long.pdf"))
ggsave(g, filename = paste_inf("12_01_02_CambioPeriodos_ent.png"),width = 11, height = 16, dpi = 200, bg= "transparent")


## 2. afiliaciones (graph 33) -------
# 'categoria' and 'año' a cada uno
nac16 <- nac_afiliad_2016 %>% 
  rownames_to_column(var = "categoria") %>%
  mutate(año = 2016)
nac16

nac18 <- nac_afiliad_2018 %>% 
  rownames_to_column(var = "categoria") %>%
  mutate(año = 2018)
nac18

nac20 <- nac_afiliad_2020 %>% 
  rownames_to_column(var = "categoria") %>%
  mutate(año = 2020)
nac20

nac22 <- nac_afiliad_2022 %>% 
  rownames_to_column(var = "categoria") %>%
  mutate(año = 2022)
nac22

df <- bind_rows(nac16, nac18, nac20, nac22)

d <- df%>%
  filter(año == 2022)



g <- ggplot(d, aes(x = reorder(categoria, Porcentaje), 
                   y = Porcentaje)) +
  geom_col(fill = ifelse(d$categoria == "Carencia por acceso a servicios de salud", "skyblue", "#3CB371")) +
  geom_text(aes(label = percent(Porcentaje / 100, accuracy = 0.1)),
            hjust = -0.1,
            size = 6) +
  coord_flip() +
  labs(
    title = str_wrap("Acceso a los servicios de salud, según institución y tipo de acceso", width = 35),
    subtitle = "2022",
    x = "",
    y = "Porcentaje",
    caption = "Una persona puede estar afiliada a más de una institución."
  ) +
  scale_x_discrete( labels = function(x) str_wrap(x, width = 30))+
  scale_y_continuous(labels = percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
        plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,5,0)),
        plot.caption = element_text(size = 15, colour = "#777777"),
        strip.text.x = element_text(size = 25),
        panel.grid.minor  = element_blank(),
        plot.margin= margin(0.3, 0.3, 1.6, 0.3, "cm"), # margin(top,right, bottom,left)
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Ubuntu"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 25, vjust = 0.5),
        axis.text.y = element_text(size = 20),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.position = "top")


g <- ggimage::ggbackground(g, paste_plant("00_plantillas/02_enigh.pdf"))
ggsave(g, filename = paste_inf("12_Afiliados_barras_camb_tot_salud.png"),
       width = 16, height = 9, dpi = 200, bg= "transparent")


## 3. Servicio (cuadro 4) -------
# 'categoria' and 'año' a cada uno
nac16 <- nac_aten_2016 %>% 
  mutate(año = 2016)
nac16

nac18 <- nac_aten_2018 %>% 
  mutate(año = 2018)
nac18

nac20 <- nac_aten_2020 %>% 
  mutate(año = 2020)
nac20

nac22 <- nac_aten_2022 %>% 
  mutate(año = 2022)
nac22

df <- bind_rows(nac16, nac18, nac20, nac22)


### 1. SANKEY por tipo de servicios -----
d <- df %>%
  filter(servicio != "Carencia por acceso a servicios de salud")%>%
  mutate(lab = paste0(str_to_title(servicio), "\n", prettyNum((Porcentaje),big.mark = ","), "%"),
         ID = as.numeric(match(servicio, unique(servicio))))



#preparación df
tt <- d %>%
  # Acomodar por orden de mayor a menor participación por año
  arrange(año, desc(Porcentaje))                        %>% 
  group_by(año)                                              %>% 
  mutate(ranking = row_number()) %>%
  ungroup() %>% 
  mutate(
    last_ranking = ifelse(
      año == max(año), 
      as.numeric(ranking), 
      NA
    ),
    last_values = ifelse(
      año == max(año), 
      Porcentaje, 
      NA
    )
  ) %>%
  group_by(ID) %>% 
  fill(last_ranking, last_values, .direction = "up") %>%
  fill(last_ranking, last_values, .direction = "down") %>%
  ungroup() %>%
  mutate(
    ent_etiqueta = case_when(
      last_ranking < 13 ~ paste0(servicio, "\n", round(last_values, 2), "%"),
      T ~ " Otros"
    ),
    rank_etiqueta = case_when(
      last_ranking < 13 ~ as.character(last_ranking),
      T ~ "99"
    )
  )


titulo <- "Personas que presentaron problemas \nde salud y recibieron atención médica."
subtitulo <- "Según lugar de atención. 2018-2022"
eje_y <- "Personas que recibieron atención médica (%)"

g <-
  ggplot(
    tt,
    aes(
      y = Porcentaje,
      x = as.character(año),
      stratum = reorder(ranking, -Porcentaje),
      alluvium = reorder(ID, -Porcentaje),
      fill = reorder(ent_etiqueta, as.numeric(rank_etiqueta))))  +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .7) +
  geom_text(aes(label = ifelse(ranking <= 4, 
                               paste0(#lugar, "\n", 
                                 round(Porcentaje,1), "%"), NA)),
            stat = "stratum", 
            size = 4, 
            family = "Ubuntu")+
  geom_text_repel(
    aes(label = ifelse(ranking > 4, 
                       paste0(round(Porcentaje,1), "%"), NA)),
    stat = "stratum", 
    size = 4, direction = "y", 
    nudge_x = .35,
    family = "Ubuntu"
  ) +
  scale_fill_manual("", values = mcv_subcategorías , labels = function(x) str_wrap(x, width = 30))+
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = titulo,
    subtitle = subtitulo,
    y = eje_y
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
    plot.subtitle = element_text(size = 35, colour = "#777777"),
    plot.margin= margin(0.3, 0.3, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
    plot.caption = element_text(size = 15, colour = "#777777"),
    strip.text.x = element_text(size = 15),
    panel.grid.minor  = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 25, angle = 90),
    axis.text.y = element_text(size = 25),
    text = element_text(family = "Ubuntu"),
    legend.text = element_text(size = 18),
    legend.position = "top"
  )

g <- ggimage::ggbackground(g,paste_plant("00_plantillas/02_enigh.pdf"))
ggsave(g, filename = paste_inf("12_01_sankeyserivicios.png"),width = 16, height = 9, dpi = 200, bg= "transparent")


### 2. SANKEY por tipo pub, priv, otro ------------
# 'categoria' y 'año' a cada uno
nac16 <- nac_aten_2016 %>% 
  mutate(año = 2016)
nac16

nac18 <- nac_aten_2018 %>% 
  mutate(año = 2018)
nac18

nac20 <- nac_aten_2020 %>% 
  mutate(año = 2020)
nac20

nac22 <- nac_aten_2022 %>% 
  mutate(año = 2022)
nac22


tipo_servicio <- tribble(
  ~servicio, ~tipo,
  "Centro de Salud", "público",
  "Hospital o Instituto", "público",
  "IMSS", "público",
  "IMSS-Prospera/IMSS-Bienestar", "público",
  "ISSTE", "público",
  "ISSTE estatal", "público",
  "INSABI", "público",
  "Otro servicio médico público", "público",
  "Consultorios privados", "privado",
  "Consultorio de farmacias", "privado",
  "Curandero", "otro",
  "Atención en otro lugar", "otro",
  "Otro", "otro"
)

df <- bind_rows(nac16, nac18, nac20, nac22)%>%
  left_join(tipo_servicio, by = "servicio")

d <- df %>%
  group_by(año, tipo) %>%
  summarise(`Millones de visitas` = sum(`Millones de visitas`, na.rm = TRUE)) %>%
  ungroup() %>%
  # Convert "millones" to full count
  mutate(
    `Millones de visitas` = `Millones de visitas` * 1e6,
    ID = as.numeric(factor(tipo)),
    lab = paste0(
      str_to_title(tipo), "\n",
      prettyNum(`Millones de visitas`, big.mark = ",", scientific = FALSE)
    )
  )


#preparación df
tt <- d %>%
  # Acomodar por orden de mayor a menor participación por año
  arrange(año, desc(`Millones de visitas`))                        %>% 
  group_by(año)                                              %>% 
  mutate(
    ranking = row_number(),
    total_visitas = sum(`Millones de visitas`, na.rm = TRUE),
    percentage = (`Millones de visitas` / total_visitas) * 100
  ) %>%
  ungroup() %>% 
  mutate(
    last_ranking = ifelse(
      año == max(año), 
      as.numeric(ranking), 
      NA
    ),
    last_values = ifelse(
      año == max(año), 
      `Millones de visitas`, 
      NA
    )
  ) %>%
  group_by(ID) %>% 
  fill(last_ranking, last_values, .direction = "up") %>%
  fill(last_ranking, last_values, .direction = "down") %>%
  ungroup() %>%
  mutate(
    ent_etiqueta = case_when(
      last_ranking < 11 ~ paste0(str_to_title(tipo)),
      T ~ " Otros"
    ),
    rank_etiqueta = case_when(
      last_ranking < 11 ~ as.character(last_ranking),
      T ~ "99"
    )
  )


titulo <- "Personas que presentaron problemas \nde salud y recibieron atención médica"
subtitulo <- "Según lugar de atención. 2018-2022"
eje_y <- "Personas que recibieron atención médica."
caption <- "Otras incluyen curandero, hierbero, comadrona, brujo, etc. y
atención en otro lugar"

g <-
  ggplot(
    tt,
    aes(
      y = `Millones de visitas`,
      x = as.character(año),
      stratum = reorder(ranking, -`Millones de visitas`),
      alluvium = reorder(ID, -`Millones de visitas`),
      fill = reorder(ent_etiqueta, as.numeric(rank_etiqueta))))  +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .7) +
  geom_text(aes(label = ifelse(ranking <= 2, 
                               paste0(comma(`Millones de visitas`),"\n(",round(percentage,1), "%)"), NA)),
            stat = "stratum", 
            size = 4, 
            family = "Ubuntu")+
  geom_text_repel(
    aes(label = ifelse(ranking > 2,
                       paste0(comma(`Millones de visitas`),"\n(",round(percentage,1), "%)"), NA)),
    stat = "stratum", 
    size = 4, direction = "y", 
    nudge_x = .35,
    family = "Ubuntu"
  ) +
  scale_fill_manual("", values = mcv_categorías , labels = function(x) str_wrap(x, width = 30))+
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = titulo,
    subtitle = subtitulo,
    y = eje_y,
    caption = caption
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
    plot.subtitle = element_text(size = 35, colour = "#777777"),
    plot.margin= margin(0.3, 0.3, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
    plot.caption = element_text(size = 15, colour = "#777777"),
    strip.text.x = element_text(size = 15),
    panel.grid.minor  = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 25, angle = 90),
    axis.text.y = element_text(size = 25),
    text = element_text(family = "Ubuntu"),
    legend.text = element_text(size = 18),
    legend.position = "top"
  )


g <- ggimage::ggbackground(g,paste_plant("00_plantillas/02_enigh.pdf"))
ggsave(g, filename = paste_inf("12_02_sankeyseriviciosCAT.png"),width = 16, height = 9, dpi = 200, bg= "transparent")



## 4. Carencia por entidad  (graf 34)-------
d <- df_combined_ent_NODECIL %>%
  filter(categoria == "ic_asalud",
         año == 2022)%>%
  mutate(
    nom_ent = factor(nom_ent, levels = c(setdiff(nom_ent, "Nacional"), "Nacional")),
    bar_color = ifelse(nom_ent == "Nacional", "#E63946", "#6950D8")  # red for Nacional, purple for others
  )


g <- g <- ggplot(d, 
                 aes(x = reorder(nom_ent, porcentaje), 
                     y = porcentaje, 
                     fill = bar_color)) +
  geom_col() +
  geom_text(aes(label = scales::percent(porcentaje, accuracy = 0.1)), 
            vjust = -0.3, size = 4) +  # labels on top of bars
  scale_fill_identity() +  # use the fill colors as is
  scale_y_continuous(labels = scales::percent_format(scale = 100),  # porcentaje is already 0–1
                     breaks = scales::pretty_breaks(n = 5),
                     expand = expansion(mult = c(0, 0.1))) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30))+
  labs(
    title = str_wrap("Porcentaje de población con carencia en acceso a servicios de salud", width = 45),
    subtitle = "Por entidad federativa",
    x = "Entidad federativa",
    y = "Porcentaje",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
    plot.subtitle = element_text(size = 35, colour = "#777777"),
    plot.margin= margin(0.3, 0.3, 1.8, 0.3, "cm"), # margin(top,right, bottom,left)
    plot.caption = element_text(size = 15, colour = "#777777"),
    strip.text.x = element_text(size = 15),
    panel.grid.minor  = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 25),
    text = element_text(family = "Ubuntu"),
    legend.text = element_text(size = 18),
  )


g <- ggimage::ggbackground(g, paste_plant("00_plantillas/02_enigh.pdf"))
ggsave(g, filename = paste_inf("12_2_CarenciaEntidad.png"),width = 16, height = 9, dpi = 200, bg= "transparent")


## 5. Heatmaps -------------
### 1.1 Nacionales  -------
#### 1.1.1 tot  ----------
d <- df_combined_ent_NODECIL%>%
  filter(ent == 0)

g <- ggplot(d, aes(x = año,
                          y = fct_rev(factor(categoria,
                                             levels = names(category_labels))),
                          fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = comma(round(value, 2))), color = "black", size = 5, family = "Ubuntu") +
  scale_y_discrete(labels = category_labels) +
  scale_fill_gradientn(
    name = "Población en millones",
    colors = terrain.colors(12),
    limits = c(0, max(d$value)),
    values = scales::rescale(c(0, 0.5, 1)), 
    labels = scales::comma,
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 25)) +
  labs(title = paste("Distribución por categoría"),
       x = "Decil",
       y = "Categoría") +
  theme_minimal() +
  theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
        plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,5,0)),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 25),
        panel.grid.minor  = element_blank(),
        plot.margin= margin(0.3, 0.3, 1.6, 0.3, "cm"), # margin(top,right, bottom,left)
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(size = 25, family = "Ubuntu"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15, vjust = 0.5),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 25),
        legend.position = "top")


g <- ggimage::ggbackground(g, paste_plant("00_plantillas/02_enigh.pdf"))
ggsave(g, filename = paste_inf("12_05_heatmap_Nac_tot.png"),
       width = 16, height = 9, dpi = 200, bg= "transparent")

#### 1.1.2 por  ----------
g <- ggplot(d, aes(x = año,
                          y = fct_rev(factor(categoria,
                                             levels = names(category_labels))),
                          fill = porcentaje)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = percent(round(porcentaje,4),accuracy = 0.1)), color = "black", size = 5, family = "Ubuntu") +
  scale_y_discrete(labels = category_labels) +
  scale_fill_gradientn(
    name = "Porcentja de la población",
    colors = terrain.colors(12),
    limits = c(0, max(d$porcentaje)),
    values = scales::rescale(c(0, 0.5, 1)), 
    labels = scales::percent_format(scale = 100),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 25)) +
  labs(title = paste("Distribución por categoría"),
       x = "Decil",
       y = "Categoría") +
  theme_minimal() +
  theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
        plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,5,0)),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 25),
        panel.grid.minor  = element_blank(),
        plot.margin= margin(0.3, 0.3, 1.6, 0.3, "cm"), # margin(top,right, bottom,left)
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(size = 25, family = "Ubuntu"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15, vjust = 0.5),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 25),
        legend.position = "top")


g <- ggimage::ggbackground(g, paste_plant("00_plantillas/02_enigh.pdf"))
ggsave(g, filename = paste_inf("12_05_02_heatmap_sindeciles_por.png"),
       width = 16, height = 9, dpi = 200, bg= "transparent")

### 1.2 Entidades  ----------
#### 1.2.1 tot -------
for (cat in names(category_labels)) {
  
  d <- df_combined_ent_NODECIL %>%
    filter(categoria == cat,
           ent != 0)
  
  
  g <- ggplot(d, 
              aes(x = año,
                  y = fct_rev(factor(nom_ent)),
                  fill = value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = comma(value)), color = "black", size = 7) +
    scale_y_discrete(labels = category_labels[cat],
                     expand = expansion(mult = c(0.05, 0.05))) +
    scale_fill_gradientn(
      name = "Población",
      colors = terrain.colors(12),
      limits = c(0, max(d$value)),
      labels = scales::comma,
      values = scales::rescale(c(0, 0.5, 1)),  # Adjust color distribution
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = .5,
        barwidth = 25)) +
    labs(title = str_wrap(paste("Distribución por entidad en: ", category_labels[cat]),width = 25),
         subtitle = paste0("En ... "),
         x = "Año",
         y = "Categoría") +
    theme_minimal() +
    theme(plot.title = element_text(size = 36, face = "bold", colour = "#6950D8", hjust = 1),
          plot.subtitle = element_blank(),
          plot.caption = element_text(size = 20),
          strip.text.x = element_text(size = 15),
          panel.grid.minor  = element_blank(),
          plot.margin= margin(0.3, 0.3, 1.9, 0.3, "cm"), # margin(top,right, bottom,left)
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 20, vjust = 0.5),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 25),
          legend.position = "top")
  
  
  
  # Save each plot
  g <- ggimage::ggbackground(g, paste_plant("00_plantillas/02_inegi_superlong.pdf"))
  ggsave(filename = paste_inf(paste0("12_01_02_01",cat,"_heatmap_tot.png")),plot = g,width = 9,height = 22,dpi = 200)
}

#### 1.5.2 por -------
for (cat in names(category_labels)) {
  
  d <- df_combined_ent_NODECIL %>%
    filter(categoria == cat,
           ent != 0)
  
  
  g <- ggplot(d, 
              aes(x = año,
                  y = fct_rev(factor(nom_ent)),
                  fill = porcentaje)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = percent(porcentaje,accuracy = 0.01)), color = "black", size = 7) +
    scale_y_discrete(labels = category_labels[cat],
                     expand = expansion(mult = c(0.05, 0.05))) +
    scale_fill_gradientn(
      name = "Población",
      colors = terrain.colors(12),
      limits = c(0, max(d$porcentaje)),
      labels = scales::percent_format(),
      values = scales::rescale(c(0, 0.5, 1)),  # Adjust color distribution
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = .5,
        barwidth = 25)) +
    labs(title = str_wrap(paste("Distribución por entidad en: ", category_labels[cat]),width = 25),
         subtitle = paste0("En ... "),
         x = "Año",
         y = "Categoría") +
    theme_minimal() +
    theme(plot.title = element_text(size = 36, face = "bold", colour = "#6950D8", hjust = 1),
          plot.subtitle = element_blank(),
          plot.caption = element_text(size = 20),
          strip.text.x = element_text(size = 15),
          panel.grid.minor  = element_blank(),
          plot.margin= margin(0.3, 0.3, 1.9, 0.3, "cm"), # margin(top,right, bottom,left)
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 20, vjust = 0.5),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 25),
          legend.position = "top")
  
  
  
  # Save each plot
  g <- ggimage::ggbackground(g, paste_plant("00_plantillas/02_inegi_superlong.pdf"))
  ggsave(filename = paste_inf(paste0("12_01_05_02_01",cat,"_heatmap_por.png")),plot = g,width = 9,height = 22,dpi = 200)
}
