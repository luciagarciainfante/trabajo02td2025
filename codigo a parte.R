rm(list=ls())

source("teoriadecision_funciones_multicriterio.R")
source("teoriadecision_funciones_multicriterio_diagram.R")
source("teoriadecision_funciones_multicriterio_utiles.R")
library(formattable)
library(htmltools)
library(webshot2)
library(ahp)


## 1. AHP con libreria 
#ahp::RunGUI()

datos = Load("transporte.ahp")
Visualize(datos)

Calculate(datos)

t1b = AnalyzeTable(datos, sort = "orig")
formattable::as.htmlwidget(t1b)

t2 = AnalyzeTable(datos, variable = "priority")
formattable::as.htmlwidget(t2)


## 2. AHP con R. 
#Criterios
n.criterios = c("Tiempo","Coste","Comodidad","Puntualidad","Sostenibilidad")
tn1 = multicriterio.crea.matrizvaloraciones_mej(c(3,2,4,8,1/2,1/3,6,2,8,5),
                                                numalternativas = 5,
                                                v.nombres.alternativas = 
                                                  n.criterios)
stn1 = multicriterio.metodoAHP.variante1.autovectormayorautovalor(tn1)
(vpn1 = round(stn1$valoraciones.ahp,4))

# Subcriterios Comodidad
n.subcriterios = c("Espacio y esfuerzo", "Condiciones met.")
tn2 = multicriterio.crea.matrizvaloraciones_mej(c(1/3),
                                                numalternativas = 2,
                                                v.nombres.alternativas =
                                                  n.subcriterios)
stn2 = multicriterio.metodoAHP.variante1.autovectormayorautovalor(tn2)
(vpn2 = round(stn2$valoraciones.ahp,4))

c1 = 0.4237
c2 = 0.1207
c31 = 0.2477*0.25
c32 = 0.2477*0.75
c4 = 0.1756
c5 = 0.0324 

crisub = c(c1,c2,c31,c32,c4,c5)

# c1: Tiempo.
n.alternativas = 
  c("CochePropio","AutobusLE02","AutobusTB1LS",
    "Bicicleta","CercaniasCaminar")

tn3c1 = multicriterio.crea.matrizvaloraciones_mej(
  c(8,7,3,5,1/2,1/7,1/6,1/6,1/5,3),
  numalternativas = 5,
  v.nombres.alternativas = n.alternativas)
stn3c1 = multicriterio.metodoAHP.variante1.autovectormayorautovalor(tn3c1)
(vpn3c1 = round(stn3c1$valoraciones.ahp,4))

## c2. Coste
tn3c2 = multicriterio.crea.matrizvaloraciones_mej(
  c(1/6,1/6,1/8,3,1,1/3,7,1/3,7,9),
  numalternativas = 5,
  v.nombres.alternativas = n.alternativas)
stn3c2 = multicriterio.metodoAHP.variante1.autovectormayorautovalor(tn3c2)
(vpn3c2 = round(stn3c2$valoraciones.ahp,4))


## c31. Espacio personal y esfuerzo 
tn3c31 = multicriterio.crea.matrizvaloraciones_mej(
  c(6,5,9,7,1/2,6,3,8,4,1/5),
  numalternativas = 5,
  v.nombres.alternativas = n.alternativas)
stn3c31 = multicriterio.metodoAHP.variante1.autovectormayorautovalor(tn3c31)
(vpn3c31 = round(stn3c31$valoraciones.ahp,4))


## c32. Condiciones meterorologicas
tn3c32 = multicriterio.crea.matrizvaloraciones_mej(
  c(7,7,9,8,1,5,2,5,2,1/6),
  numalternativas = 5,
  v.nombres.alternativas = n.alternativas)
stn3c32 = multicriterio.metodoAHP.variante1.autovectormayorautovalor(tn3c32)
(vpn3c32 = round(stn3c32$valoraciones.ahp,4))


## c4. Puntualidad
tn3c4 = multicriterio.crea.matrizvaloraciones_mej(
  c(4,3,1/3,6,1/2,1/6,2,1/5,3,8),
  numalternativas = 5,
  v.nombres.alternativas = n.alternativas)
stn3c4 = multicriterio.metodoAHP.variante1.autovectormayorautovalor(tn3c4)
(vpn3c4 = round(stn3c4$valoraciones.ahp,4))

## c5. Sostenibilidad
tn3c5 = multicriterio.crea.matrizvaloraciones_mej(
  c(1/5,1/5,1/7,1/6,1,1/3,1/4,1/3,1/4,3),
  numalternativas = 5,
  v.nombres.alternativas = n.alternativas)
stn3c5 = multicriterio.metodoAHP.variante1.autovectormayorautovalor(tn3c5)
(vpn3c5 = round(stn3c5$valoraciones.ahp,4))


matper = matrix(c(0.5005,0.0347,0.0496,0.2663,0.1489,
                  0.0538,0.2180,0.2180,0.4786,0.0316,
                  0.5710,0.1302,0.1995,0.0281,0.0712,
                  0.6290,0.1255,0.1255,0.0309,0.0892, 
                  0.2575,0.0715,0.1127,0.5144,0.0439,
                  0.0363,0.1185,0.1185,0.4271,0.2997),
                ncol = 5, nrow = 6, byrow = T)

pond.globales = crisub %*% matper
colnames(pond.globales) = n.alternativas
pond.globales
round(pond.globales*100,2)

Mcrisub = matrix(crisub,nrow = 6, ncol = 5)
Mcrisub
pond.globales.parciales = Mcrisub*matper 
round(pond.globales.parciales*100,2)



Inconsistencia_crit = multicriterio.metodoAHP.coef.inconsistencia(tn1)
c(Inconsistencia_crit$mensaje, round(Inconsistencia_crit$RI.coef.inconsistencia,4) )

(Inconsistencia_sub = multicriterio.metodoAHP.coef.inconsistencia(tn2))
c(Inconsistencia_sub$mensaje, round(Inconsistencia_sub$RI.coef.inconsistencia,4) )

Inconsistenciac1= multicriterio.metodoAHP.coef.inconsistencia(tn3c1)
c(Inconsistenciac1$mensaje, round(Inconsistenciac1$RI.coef.inconsistencia,4) )

Inconsistenciac2 = multicriterio.metodoAHP.coef.inconsistencia(tn3c2)
c(Inconsistenciac2$mensaje, round(Inconsistenciac2$RI.coef.inconsistencia,4) )

Inconsistenciac31 = multicriterio.metodoAHP.coef.inconsistencia(tn3c31)
c(Inconsistenciac31$mensaje, round(Inconsistenciac31$RI.coef.inconsistencia,4) )

Inconsistenciac32 = multicriterio.metodoAHP.coef.inconsistencia(tn3c32)
c(Inconsistenciac32$mensaje, round(Inconsistenciac32$RI.coef.inconsistencia,4) )

Inconsistenciac4 = multicriterio.metodoAHP.coef.inconsistencia(tn3c4)
c(Inconsistenciac4$mensaje, round(Inconsistenciac4$RI.coef.inconsistencia,4) )

Inconsistenciac5 = multicriterio.metodoAHP.coef.inconsistencia(tn3c5)
c(Inconsistenciac5$mensaje, round(Inconsistenciac5$RI.coef.inconsistencia,4) )



## 3. Electre. 

## PASO 1. METER LOS DATOS ---

el1  = multicriterio.crea.matrizdecision(c(-23,-3.04,8,8,5,1,
                                           -60,-0.92,4,5,2,3,
                                           -55,-0.92,5,5,3,3,
                                           -29,0,4,1,8,7,
                                           -35,-3.6,2,3,1,5), 
                                         numalternativas = 5,
                                         numcriterios = 6, )
el1


## ITERACION 1 --- 
## Aplicamos el método electre una vez.
sal7 = multicriterio.metodoELECTRE_I(el1,
                                     pesos.criterios = crisub,
                                     nivel.concordancia.minimo.alpha = 0.7,
                                     no.se.compensan =c(10, 1, Inf, Inf, Inf, Inf),
                                     que.alternativas = TRUE)  #Se usan todas

qgraph::qgraph(sal7$relacion.dominante)
sal7$nucleo_aprox


## ITERACION 2 --- 
## Aplicamos el método electre una vez.

sal7b2 = multicriterio.metodoELECTRE_I(el1,
                                     pesos.criterios = crisub,
                                     nivel.concordancia.minimo.alpha = 0.7,
                                     no.se.compensan =c(10, 1, Inf, Inf, Inf, Inf),
                                     que.alternativas = c(1,4))  #Se usan todas

qgraph::qgraph(sal7b2$relacion.dominante)
sal7b2$nucleo_aprox


## ITERACION 3 --- 
## Aplicamos el método electre una vez.
sal7b3 = multicriterio.metodoELECTRE_I(el1,
                                      pesos.criterios = crisub,
                                      nivel.concordancia.minimo.alpha = 0.55,
                                      no.se.compensan =c(10, 1, Inf, Inf, Inf, Inf),
                                      que.alternativas = c(1,4))  #Se usan todas

sal7b3  #Con este objeto podemos ver todo el camino del método
str(sal7b3)
sal7b3$relacion.dominante
qgraph::qgraph(sal7b3$relacion.dominante)
sal7b3$nucleo_aprox



## ITERACION 4 --- 
## Aplicamos el método electre una vez.
sal7b4 = multicriterio.metodoELECTRE_I(el1,
                                       pesos.criterios = crisub,
                                       nivel.concordancia.minimo.alpha = 0.55,
                                       no.se.compensan =c(15, 4, Inf, Inf, Inf, Inf),
                                       que.alternativas = c(1,4))  #Se usan todas

sal7b4  #Con este objeto podemos ver todo el camino del método
qgraph::qgraph(sal7b4$relacion.dominante)
sal7b4$nucleo_aprox

