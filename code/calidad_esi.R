library(magrittr)

load('data/esi-2023---personas.rdata') # Se carga como esi_2023_personas

esi <- subset(esi_2023_personas)

#############################
# 1.- Codificación Variables
esi %<>% 
  mutate(oc = if_else(cae_especifico %in% 1:7, 1,0),
         ocup = if_else(oc == 1 & ocup_ref == 1,1,0),
         r14 = if_else(ocup == 1 & region == 14,1,0))

#############################
# 2.- Diseño complejo

dc <- svydesign(ids = ~conglomerado_correlativo,
                 weights = ~fact_cal_esi,
                 strata = ~estrato,
                 data = esi,
                 check.strata = TRUE)

options(survey.lonely.psu = 'remove')
#############################
# 3.- Ingreso medio

# A.- Ingreso por región
ingreso_region <- create_mean('ing_t_p','region', subpop = 'ocup', design = dc, ci = TRUE)

# B.- Ingreso Región de los Ríos
ingreso <- create_mean('ing_t_p','r14', subpop = 'ocup', design = dc, ci = TRUE)
  
ingresos_sexo <- create_mean('ing_t_p','r14+sexo', subpop = 'ocup', design = dc, ci = TRUE)

# C.- Ingreso Categoria
ingreso_categoria <- create_mean('ing_t_p', 'categoria_ocupacion', subpop = 'ocup', design = dc, ci = TRUE)
ingreso_categoria_sexo <- create_mean('ing_t_p', 'categoria_ocupacion+sexo', subpop = 'ocup', design = dc, ci = TRUE)

# D.- Ingreso Caenes
ingreso_caenes <- create_mean('ing_t_p', 'b14_rev4cl_caenes', subpop = 'ocup', design = dc, ci = TRUE)
ingreso_caenes_sexo <- create_mean('ing_t_p', 'b14_rev4cl_caenes+sexo', subpop = 'ocup', design = dc, ci = TRUE)


#############################
# 4.- Calidad estimaciones
tabla_region <- assess(ingreso_region)
tabla_ingreso <- assess(ingreso) %>% 
  filter(r14 == 1)

tabla_ingreso_sexo <- assess(ingresos_sexo)
tabla_categoria <- assess(ingreso_categoria)
tabla_categoria_sexo <- assess(ingreso_categoria_sexo)

tabla_caenes <- assess(ingreso_caenes)
tabla_caenes_sexo <- assess(ingreso_caenes_sexo)
