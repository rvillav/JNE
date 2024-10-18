# Ordenar por id_mediana ----
esi <- esi %>% 
  arrange(id_mediana)

#c. Se fijan las semillas 
set.seed(234262762)  

#d. Se realiza el diseño repetido de sub-bootstrap para 2.000 réplicas 
diseno_rep1 <- as.svrepdesign(dc, type = "subbootstrap", replicates = 2)


diseno_rep <- diseno_rep1
                 

# Se ejecuta la función modificada correspondiente a la regla de interpolación del método N°5. 
interpolacionMetodo5 <- function(x, w, p)  
{ 
  if (any(zero <- w == 0)) { 
    w <- w[!zero] 
    x <- x[!zero] 
  } 
  n <- length(x) 
  ii <- order(x) 
  x <- x[ii] 
  w <- w[ii] 
  cumw <- cumsum(w) 
  pk <- (cumw - w/2)/(cumw[n]) 
  #Modificación: si x es sólo 1 valor, muéstralo; en caso contrario; generar interpolación:   
  if(length(x) == 1){x}else{   
    
    approx(pk, x, p, method = "linear", rule = 2)$y 
  } 
} 


# TABULADOS ----
## INGRESO MEDIANO MENSUAL DE LAS PERSONAS 2019-2023 ----
### LAMINA N°2 ----

i_mediano <- vector("list", length(esi))
i_mediano_sexo <- vector("list", length(esi))


i_mediano <- svyby(~ing_t_p, 
                          by = ~ano_trimestre+region, 
                          data=esi, 
                          subset(diseno_rep,  
                                 ocup_ref == 1,
                                 region == 14
                          ),  
                          svyquantile, 
                          quantiles=c(0.5), 
                          interval.type="quantile",  
                          vartype=c("se", "cv", "ci"), 
                          ci=TRUE, 
                          na.rm.all = FALSE, 
                          qrule = interpolacionMetodo5)
  # segun sexo
i_mediano_sexo <- svyby(~ing_t_p, 
                               by = ~ano_trimestre+sexo+region, 
                               data=esi, 
                               subset(diseno_rep,  
                                      ocup_ref==1
                               ),  
                               svyquantile, 
                               quantiles=c(0.5), 
                               interval.type="quantile",  
                               vartype=c("se", "cv", "ci"), 
                               ci=TRUE, 
                               na.rm.all = FALSE, 
                               qrule = interpolacionMetodo5)
  


ingreso_mediano <- bind_rows(i_mediano)
ingreso_mediano_sexo <- bind_rows(i_mediano_sexo)

# Filtro Los Ríos
ingreso_mediano %<>% filter(region == 14)
ingreso_mediano_sexo %<>% filter(region == 14)

## INGRESO MEDIANO MENSUAL REAL DE LAS PERSONAS 2019-2023 ----
### LAMINA N°4 ----
i_mediano_real <- vector("list", length(esi))
i_mediano_real_sexo <- vector("list", length(esi))

for (i in seq_along(esi)) {
  i_mediano_real[[i]] <- svyby(~ing_def, 
                               by = ~ano_trimestre+region, 
                               data=esi[[i]], 
                               subset(diseno_rep[[i]],  
                                      ocup_ref == 1,
                                      r14 == 1),  
                               svyquantile, 
                               quantiles=c(0.5), 
                               interval.type="quantile",  
                               vartype=c("se", "cv", "ci"), 
                               ci=TRUE, 
                               na.rm.all = FALSE, 
                               qrule = interpolacionMetodo5)
  
  i_mediano_real_sexo[[i]] <- svyby(~ing_def, 
                                    by = ~ano_trimestre+sexo+region, 
                                    data=esi[[i]], 
                                    subset(diseno_rep[[i]],  
                                           ocup_ref==1,
                                           r14 == 1),  
                                    svyquantile, 
                                    quantiles=c(0.5), 
                                    interval.type="quantile",  
                                    vartype=c("se", "cv", "ci"), 
                                    ci=TRUE, 
                                    na.rm.all = FALSE, 
                                    qrule = interpolacionMetodo5)
}

ingreso_mediano_real <- bind_rows(i_mediano_real)
ingreso_mediano_real_sexo <- bind_rows(i_mediano_real_sexo)

# Filtro Los Ríos
ingreso_mediano_real %<>% filter(region == 14)
ingreso_mediano_real_sexo %<>% filter(region == 14)


## INGRESO MEDIANO MENSUAL DE LAS PERSONAS OCUPADAS SEGUN REGION 2023 ----
### LAMINA N°6 ----
mediana_region <- svyby(~ing_t_p, 
                        by = ~region, 
                        data=esi[[5]], 
                        subset(diseno_rep[[5]],  ocup_ref==1),  
                        svyquantile, 
                        quantiles=c(0.5), 
                        interval.type="quantile",  
                        vartype=c("se", "cv", "ci"), 
                        ci=TRUE, 
                        na.rm.all=FALSE, 
                        qrule=interpolacionMetodo5)

mediana_pais <- svyby(~ing_t_p, 
                      by = ~ano_trimestre, 
                      data=esi[[5]], 
                      subset(diseno_rep[[5]],  ocup_ref==1),  
                      svyquantile, 
                      quantiles=c(0.5), 
                      interval.type="quantile",  
                      vartype=c("se", "cv", "ci"), 
                      ci=TRUE, 
                      na.rm.all=FALSE, 
                      qrule=interpolacionMetodo5)


## INGRESO MEDIANO MENSUAL DE LAS PERSONAS OCUPADAS SEGUN CISE 2023----
### LAMINA N°7B ----
mediana_cise <- svyby(~ing_t_p, 
                      by = ~cise_reco+region, 
                      data=esi[[5]], 
                      subset(diseno_rep[[5]], ocup_ref==1),  
                      svyquantile, 
                      quantiles=c(0.5), 
                      interval.type="quantile",  
                      vartype=c("se", "cv", "ci"), 
                      ci=TRUE, 
                      na.rm.all=FALSE, 
                      qrule=interpolacionMetodo5)

mediana_cise <- mediana_cise %>% 
  filter(region == 14)

## INGRESO MEDIANO MENSUAL DE LAS PERSONAS OCUPADAS SEGUN CINE 2023----
### LAMINA N°8B ----
mediana_cine <- svyby(~ing_t_p, 
                      by = ~cine_reco+region, 
                      data=esi[[5]], 
                      subset(diseno_rep[[5]], ocup_ref==1),  
                      svyquantile, 
                      quantiles=c(0.5), 
                      interval.type="quantile",  
                      vartype=c("se", "cv", "ci"), 
                      ci=TRUE, 
                      na.rm.all=FALSE, 
                      qrule=interpolacionMetodo5)

mediana_cine <- mediana_cine %>% 
  filter(region == 14)
