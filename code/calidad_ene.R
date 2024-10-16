library(calidad)
library(readxl)
library(tidyverse)
library(dplyr)
library(survey)
library(haven)


# Clean environment
rm(list = ls())

# Load Data
ene <- read_sav('data/ene-2023-11-ond.sav')

#############################
# 1.- Codificación variables

ene <- ene %>% 
  mutate(pet = ifelse(edad > 14, 1, 0),# ---> Población en edad de trabajar.
         region2 = ifelse(region == 14, 1, 0), # ---> Region.
         fdt = if_else(cae_especifico %in% 1:9, 1,0), # ---> Fuerza de trabajo.
         oc = if_else(cae_especifico %in% 1:7, 1,0), # ---> Ocupados.
         des = if_else(cae_especifico %in% 8:9, 1,0), # ---> Desocupados
         inacv = ifelse(between(as.numeric(cae_especifico),10,28),1,0), # ---> Inactivos.
         ces = ifelse(between(as.numeric(cae_especifico),8,8),1,0), # ---> Cesantes.
         bt = ifelse(between(as.numeric(cae_especifico),9,9),1,0), # ---> Buscan trabajo por primera vez.
         o_informal = ifelse(as.numeric(ocup_form == 2), 1, 0)) %>% replace_na(list(o_informal = 0))  # ---> Ocupados informales.


#############################
# 2.- Disenio Muestral

DC  <-  svydesign(ids = ~conglomerado,   
                  weights = ~fact_cal,  # ---> Factores de expansion
                  strata = ~estrato,     
                  data = ene)    # ---> Base de datos

options(survey.lonely.psu="remove")

#############################
# 3.- Estimaciones

# Ocupados
# Desocupados
# Inactivos
# Informales

# A.- Ocupados
ocupados <- create_total('oc', subpop = 'region2', design = DC, ci = TRUE)
o_sexo <- create_total('oc', 'sexo',subpop = 'region2', design = DC, ci = TRUE)

# B.- Desocupados
desocupados <- create_total('des',subpop = 'region2', design = DC, ci = TRUE)
des_sexo <- create_total('des', 'sexo',subpop = 'region2', design = DC, ci = TRUE)

# C.- Inactivos
inactivos <- create_total('inacv',subpop = 'region2', design = DC, ci = TRUE)
ina_sexo <- create_total('inacv', 'sexo',subpop = 'region2', design = DC, ci = TRUE)

# D.- Ocupados informales
informales <- create_total('o_informal',subpop = 'region2', design = DC, ci = TRUE)
inf_sexo <- create_total('o_informal', 'sexo',subpop = 'region2', design = DC, ci = TRUE)

#############################
# 4.- Calidad estimaciones

tabla1 <- assess(ocupados)
tabla2 <- assess(o_sexo)
tabla3 <- assess(desocupados)
tabla4 <- assess(des_sexo)
tabla5 <- assess(inactivos)
tabla6 <- assess(ina_sexo)
tabla7 <- assess(informales)
tabla8 <- assess(inf_sexo)

#############################
# 5.- Principales Tasas

# A.- Tasa ocupación informal
toi <- create_prop('o_informal', denominator = 'oc', domains = 'region2', design = DC)
toi_sexo <- create_prop('o_informal', denominator = 'oc', domains = 'region2+sexo', design = DC)

# B.- Tasa de desocupación
tdes <- create_prop('des', denominator = 'fdt', domains = 'region2', design = DC)
tdes_sexo <- create_prop('des', denominator = 'fdt', domains = 'region2+sexo', design = DC)

# C.- Tasa de ocupación
to <- create_prop('oc', denominator = 'pet', domains = 'region2', design = DC)
to_sexo <- create_prop('oc', denominator = 'pet', domains = 'region2+sexo', design = DC)

# D.- Tasa de participación
tp <- create_prop('fdt', denominator = 'pet', domains = 'region2', design = DC)
tp_sexo <- create_prop('fdt', denominator = 'pet', domains = 'region2+sexo', design = DC)

#############################
# 5.- Calidad Tasa
tabla_toi <- assess(toi) %>% 
  filter(region2 == 1)

tabla_toi_sexo <- assess(toi_sexo) %>% 
  filter(region2 == 1)

tabla_des <- assess(tdes)
tabla_des_sexo <- assess(tdes_sexo)
tabla_to <- assess(to)
tabla_to_sexo <- assess(to_sexo)
tabla_tp <- assess(tp)
tabla_tp_sexo <- assess(tp_sexo)

#############################
# 6.- Otras Estimaciones

# A.- CAENES
caenes <- create_total('oc', domains = 'b14_rev4cl_caenes', subpop='region2', design = DC, ci = TRUE )

# B.- Categoria en la ocupación
cise <- create_total('oc', domains = 'categoria_ocupacion', subpop='region2', design = DC, ci = TRUE )

#############################
# 7.- Calidad otras estimaciones
tabla_caenes <- assess(caenes)
tabla_cise <- assess(cise)
