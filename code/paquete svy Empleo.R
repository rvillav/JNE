library(haven)
library(survey)
library(dplyr)
library(tidyverse)

# Despejar la consola
rm(list = ls())
# Definir año y mes
# rutabase <- "//Buvmfswinp01/UT_LOS_RIOS/Presentaciones/Presentaciín Congreso UACH/" 
# base <- paste0(rutabase,"ene-2023-11-ond",".dta")
# ene <- read_dta(base)
ene <- read_sav('data/ene-2023-11-ond.sav')

#####################################################33 CREAR VARIABLES ####
# PET: Poblacion en edad de trabajar (15 aoos y mos)
ene <- ene %>% mutate(pet = ifelse(edad > 14, 1, 0))

# OCUPADOS
ene <- ene %>% mutate(oc = ifelse(between(as.numeric(cae_especifico),1,7),1,0))

ene <- ene %>% mutate(region2 = ifelse(region == 14, 1, 0))

# DESOCUPADOS
ene <- ene %>% mutate(des = ifelse(between(as.numeric(cae_especifico),8,9),1,0))

# FUERZA DE TRABAJO
ene <- ene %>% mutate(fdt = ifelse(between(as.numeric(cae_especifico),1,9),1,0))

# INACTIVOS
ene <- ene %>% mutate(inacv = ifelse(between(as.numeric(cae_especifico),10,28),1,0))

# cesante
ene <- ene %>% mutate(ces = ifelse(between(as.numeric(cae_especifico),8,8),1,0))

# busca trabajo por primera 
ene <- ene %>% mutate(bt = ifelse(between(as.numeric(cae_especifico),9,9),1,0))

#iniciadores disponible
ene <- ene %>% mutate(inid = ifelse(between(as.numeric(cae_especifico),10,10),1,0))

#iniciadores
ene <- ene %>% mutate(ini = ifelse(between(as.numeric(cae_especifico),10,11),1,0))

#potencial
ene <- ene %>% mutate(pot = ifelse((cae_especifico==12 | cae_especifico==14 | cae_especifico==16 | cae_especifico==18 | cae_especifico==20 | cae_especifico==22 | cae_especifico==25 | cae_especifico==26| cae_especifico==27),1,0))

#habitual
ene <- ene %>% mutate(hab = ifelse((cae_especifico==13 | cae_especifico==15 | cae_especifico==17 | cae_especifico==19 | cae_especifico==21 | cae_especifico==23 | cae_especifico==24 | cae_especifico==28),1,0))

#involuntario
ene <- ene %>% mutate(tpi = ifelse(cae_especifico>=1 & cae_especifico<=7 & habituales<=30 & 
                                     c10==1 & (c11==1 | c11==2),1,0))
#ocupados que buscan empleo
ene <- ene %>% mutate(obe = ifelse((e4==1 | e4==2 |e4==3 |e4==4 |e4==5 | e4==6),1,0))

#parcial
ene <- ene %>% mutate(tp = ifelse(cae_especifico>=1 & cae_especifico<=7 & habituales<=30 & habituales>=1,1,0))

#voluntario
ene <- ene %>% mutate(tpv = ifelse(cae_especifico>=1 & cae_especifico<=7 & habituales<=30 & 
                                     (c10==2 | c11==3 | c11==4),1,0))
#informal
ene <- ene %>% mutate(o_informal = ifelse(as.numeric(ocup_form == 2), 1, 0)) %>%replace_na(list(o_informal = 0))

# DISEoO MUESTRAL ####

DC  <-  svydesign(id = ~conglomerado,   
                  weights = ~fact_cal,  #Factores de expansion
                  strata=~estrato,     
                  data = ene)    # Base de datos
options(survey.lonely.psu="remove")


# 1. Estimacion Ocupados
OC <- svytotal(~oc,subset(DC, region ==14),na.rm=1)
OC1<- svyby(~oc, by = ~sexo, subset(DC, region ==14),svytotal, na.rm.all = FALSE)
print(OC)
print(OC1)

# 2. Estimacion Desocupados

DESOC <- svytotal(~des,subset(DC, region ==14),na.rm=1)
DESOC1<- svyby(~des, by = ~sexo, subset(DC, region ==14),svytotal, na.rm.all = FALSE)
print(DESOC)
print(DESOC1)


# 3. Fuerza de Trabajo

FUDT <- svytotal(~fdt,subset(DC, region ==14),na.rm=1)
FUDT1<- svyby(~fdt, by = ~sexo, subset(DC, region ==14),svytotal, na.rm.all = FALSE)
print(FUDT)
print(FUDT1)



# 4. tasa de desocupación

TD <- svyratio(~des,~fdt,subset(DC, region ==14),na.rm=1)
TD1<- svyby(~des,by = ~sexo, subset(DC, region ==14),svyratio, denominator = ~fdt,na.rm.all = FALSE)
print(TD)
print(TD1)

# 5. tasa de participación

TP <- svyratio(~fdt,~pet,subset(DC, region ==14),na.rm=1)
TP1<- svyby(~fdt,by = ~sexo, subset(DC, region ==14),svyratio, denominator = ~pet,na.rm.all = FALSE)
print(TP)
print(TP1)

# 6. tasa de ocupación

TO <- svyratio(~oc,~pet,subset(DC, region ==14),na.rm=1)
TO1<- svyby(~oc,by = ~sexo, subset(DC, region ==14),svyratio, denominator = ~pet,na.rm.all = FALSE)
print(TO)
print(TO1)

# 7. tasa de ocupación informal
TOI <- svyratio(~o_informal,~oc,subset(DC, region ==14),na.rm=1)
TOI1<- svyby(~o_informal,by = ~sexo, subset(DC, region ==14),svyratio, denominator = ~oc,na.rm.all = FALSE)
print(TOI)
print(TOI1)



#y como se hacia para calcular grados de libertad, intervalo de confianza, coeficiente de variación, número de observaciones, entre otros.

#1. Ejemplo ocupados



# a. Error estondar
SE<-SE(svytotal(~oc,subset(DC, region ==14),deff=TRUE,na.rm=1))

# b. Grados de libertad
GL<- as.numeric(ene %>% filter(ene$oc==1 & region ==14) %>% summarise(GL = n_distinct(conglomerado) - n_distinct(estrato)))

# c. Valor t
t<-qt(c(.975), df = GL)  

# d. Lomte inferior
LI<- as.numeric(OC) - SE*t

# e. Lomte inferior
LS<- as.numeric(OC) + SE*t

# f. Coeficiente de variacion
CV<- (SE/as.numeric(OC))*100

# g. NÚMERO DE OBSERVACIONES
N<-as.numeric(DC$variables %>% filter(DC$variables$oc==1 & region==14) %>% summarise(n = sum(oc)))

ocupados<-data.frame(as.numeric(OC), SE, GL, t, LI, LS, CV, N)
colnames(ocupados)<-c("ocupados", "error estandar", "grados de libertad", "valor t", "límite inferior", "límite superior", "coeficiente de variación", "número de observaciones")
print(ocupados)