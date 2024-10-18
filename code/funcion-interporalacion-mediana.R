# Se ejecuta la función modificada correspondiente a la regla de interpolación del método N°5. 
interpolacionMetodo5 <- function(x, w, p)  { 
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
