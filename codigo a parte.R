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
n.criterios = c("Tiempo", "Coste", "Comodidad", "Puntualidad", "Sostenibilidad")
tn1 = multicriterio.crea.matrizvaloraciones_mej(c(3,2,4,8,1/2,1/3,6,5,8,5),
                                                   numalternativas = 5,
                                                   v.nombres.alternativas = n.criterios)
stn1 = multicriterio.metodoAHP.variante1.autovectormayorautovalor(tn1)
(vpn1 = round(stn1$valoraciones.ahp,4))

# Subcriterios Comodidad
n.subcriterios = c("Espacio y esfuerzo", "Condiciones met.")
tn2 = multicriterio.crea.matrizvaloraciones_mej(c(1/3),
                                                numalternativas = 2,
                                                v.nombres.alternativas = n.subcriterios)
stn2 = multicriterio.metodoAHP.variante1.autovectormayorautovalor(tn2)
(vpn2 = round(stn2$valoraciones.ahp,4))

c1 = 0.3977
c2 = 0.1150
c31 = 0.3108*0.25
c32 = 0.3108*0.75
c4 = 0.1460
c5 = 0.0304

crisub = c(c1,c2,c31,c32,c4,c5)

# c1: Tiempo.
n.alternativas = c("CochePropio","AutobusLE02","AutobusTB1LS","Bicicleta","CercaniasCaminar")
tn3c1 = multicriterio.crea.matrizvaloraciones_mej(c(8,7,3,5,1/2,1/7,1/6,1/6,1/5,3),
                                                        numalternativas = 5,
                                                        v.nombres.alternativas = n.alternativas)
stn3c1 = multicriterio.metodoAHP.variante1.autovectormayorautovalor(tn3c1)
(vpn3c1 = round(stn3c1$valoraciones.ahp,4))

## c2. Coste
tn3c2 = multicriterio.crea.matrizvaloraciones_mej(c(1/6,1/6,1/8,3,1,1/3,7,1/3,7,9),
                                                  numalternativas = 5,
                                                  v.nombres.alternativas = n.alternativas)
stn3c2 = multicriterio.metodoAHP.variante1.autovectormayorautovalor(tn3c2)
(vpn3c2 = round(stn3c2$valoraciones.ahp,4))


## c3. Espacio personal y esfuerzo 
tn3c31 = multicriterio.crea.matrizvaloraciones_mej(c(6,5,9,7,1/2,6,3,8,4,1/5),
                                                  numalternativas = 5,
                                                  v.nombres.alternativas = n.alternativas)
stn3c31 = multicriterio.metodoAHP.variante1.autovectormayorautovalor(tn3c31)
(vpn3c31 = round(stn3c31$valoraciones.ahp,4))


## c4. Condiciones meterorologicas
tn3c32 = multicriterio.crea.matrizvaloraciones_mej(c(7,7,9,8,1,5,2,5,2,1/6),
                                                   numalternativas = 5,
                                                   v.nombres.alternativas = n.alternativas)
stn3c32 = multicriterio.metodoAHP.variante1.autovectormayorautovalor(tn3c32)
(vpn3c32 = round(stn3c32$valoraciones.ahp,4))


## c5. Puntualidad
tn3c4 = multicriterio.crea.matrizvaloraciones_mej(c(4,3,1/3,6,1/2,1/6,2,1/5,3,8),
                                                   numalternativas = 5,
                                                   v.nombres.alternativas = n.alternativas)
stn3c4 = multicriterio.metodoAHP.variante1.autovectormayorautovalor(tn3c4)
(vpn3c4 = round(stn3c4$valoraciones.ahp,4))

## c6. Sostenibilidad
tn3c5 = multicriterio.crea.matrizvaloraciones_mej(c(1/5,1/5,1/7,1/6,1,1/3,1/4,1/3,1/4,3),
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


# Inconsistencia.

(Inconsistencia0601 = multicriterio.metodoAHP.coef.inconsistencia(tb0601))
c(Inconsistencia0601$mensaje, round(Inconsistencia0601$RI.coef.inconsistencia,4) )

Inconsistencia0602a = multicriterio.metodoAHP.coef.inconsistencia(tb0602a)
c(Inconsistencia0602a$mensaje, round(Inconsistencia0602a$RI.coef.inconsistencia,4) )

Inconsistencia0602b = multicriterio.metodoAHP.coef.inconsistencia(tb0602b)
c(Inconsistencia0602b$mensaje, round(Inconsistencia0602b$RI.coef.inconsistencia,4) )

Inconsistencia0602c = multicriterio.metodoAHP.coef.inconsistencia(tb0602c)
c(Inconsistencia0602c$mensaje, round(Inconsistencia0602c$RI.coef.inconsistencia,4) )


## 3. Electre. 

## PASO 1. METER LOS DATOS ---

el1  = multicriterio.crea.matrizdecision(c(-23,-3.04,8,8,5,1,
                                           -60,-0.92,4,6,2,4,
                                           -55,-0.92,5,6,4,4,
                                           -29,0,4,1,2,8,
                                           -35,-3.6,2,3,1,5), 
                                         numalternativas = 5,
                                         numcriterios = 6, )
el1


## ITERACION 1 --- 
## Aplicamos el método electre una vez.
sal7 = multicriterio.metodoELECTRE_I(el1,
                                     pesos.criterios = crisub,
                                     nivel.concordancia.minimo.alpha = 0.7,
                                     no.se.compensan =c(50, 4, 7, 7, 7, 7),
                                     que.alternativas = TRUE)  #Se usan todas

sal7  #Con este objeto podemos ver todo el camino del método
str(sal7)
sal7$relacion.dominante
qgraph::qgraph(sal7$relacion.dominante)
sal7$nucleo_aprox


## ITERACION 2 --- 
## Aplicamos el método electre una vez.
sal7b = multicriterio.metodoELECTRE_I(el1,
                                     pesos.criterios = crisub,
                                     nivel.concordancia.minimo.alpha = 0.7,
                                     no.se.compensan =c(50, 4, 7, 7, 7, 7),
                                     que.alternativas = c(1,4))  #Se usan todas

sal7b  #Con este objeto podemos ver todo el camino del método
str(sal7b)
sal7b$relacion.dominante
qgraph::qgraph(sal7b$relacion.dominante)
sal7b$nucleo_aprox



## ITERACION 3 --- 
## Aplicamos el método electre una vez.
sal7b3 = multicriterio.metodoELECTRE_I(el1,
                                      pesos.criterios = crisub,
                                      nivel.concordancia.minimo.alpha = 0.55,
                                      no.se.compensan =c(50, 4, 8, 8, 8, 8),
                                      que.alternativas = c(1,4))  #Se usan todas

sal7b3  #Con este objeto podemos ver todo el camino del método
str(sal7b3)
sal7b3$relacion.dominante
qgraph::qgraph(sal7b3$relacion.dominante)
sal7b3$nucleo_aprox


