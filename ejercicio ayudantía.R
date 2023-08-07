#test test test
#otro test 
# Librerías ---------------------------------------------------------------
install.packages("readxl")
install.packages("dplyr")
install.packages("rio")

library(readxl)
library(dplyr)
library(rio)

# Cargar base de datos ----------------------------------------------------

## Desde PC local: ------
getwd() #ver directorio
# df <- read_excel("directorio/del/usuario")

df_casino <- read_excel("BDD_Ayudantia.xlsx", sheet = 1)
df_biblio <- read_excel("BDD_Ayudantia.xlsx", sheet = 2)
df_salas <- read_excel("BDD_Ayudantia.xlsx", sheet = 3)
# sheet es la pestaña del excel
## Desde Github: -----

df_biblio <- import("https://raw.githubusercontent.com/Hermanaros/Ayudantia/main/df_biblio.csv")
df_casino <- import("https://raw.githubusercontent.com/Hermanaros/Ayudantia/main/df_casino.csv")
df_salas <- import("https://raw.githubusercontent.com/Hermanaros/Ayudantia/main/df_salas.csv")

# Regresion lineal --------------------------------------------------------


modelo_biblio <- lm(data= df_biblio, Satisf_General_Biblioteca ~ Satisf_Disponibilidad_Libros +
     Satisf_Silencio + Satisf_Limpieza + Satisf_Orden + Satisf_Calidad_Atencion)
summary(modelo_biblio)

# revisar los valores p para cada predictor:el valor p debe ser menor a 0.05 (nivel confianza 95%)
# La unica variable con valor mayor a 0.05 es Satisf_Silencio (p value = 0.070960)
# Es decir: la satisfaccion con el silencio no es una variable significativa para explicar 
# la satisfacción general de la biblioteca.

## Regresion ajusada -----

modelo_biblio <- lm(data= df_biblio, Satisf_General_Biblioteca ~ Satisf_Disponibilidad_Libros +
                       Satisf_Limpieza + Satisf_Orden + Satisf_Calidad_Atencion)
summary(modelo_biblio)


# Satisfacción neta -------------------------------------------------------

#1 a 6 = detractor
#7 a 8 = neutro
#9 a 10 = promotor
n <- length(df_biblio$Satisf_Disponibilidad_Libros) # total de filas


## satisf disponibilidad libros-----
libro_promoter<-(df_biblio %>% filter(Satisf_Disponibilidad_Libros > 8) %>%  count())/n
libro_detractor<-(df_biblio %>% filter(Satisf_Disponibilidad_Libros < 7) %>%  count())/n
((libro_promoter) - (libro_detractor))*100
#satisf neta: 49.8008

## satisf limpieza -------
limpieza_promoter<-(df_biblio %>% filter(Satisf_Limpieza > 8) %>%  count())/n
limpieza_detractor<-(df_biblio %>% filter(Satisf_Limpieza < 7) %>%  count())/n
((limpieza_promoter) - (limpieza_detractor))*100
#satisf neta: 42.62948

## satisf orden -------
orden_promoter<-(df_biblio %>% filter(Satisf_Orden > 8) %>%  count())/n
orden_detractor<-(df_biblio %>% filter(Satisf_Orden < 7) %>%  count())/n
((orden_promoter) - (orden_detractor))*100
#satisf neta: 39.04382

## satisf Atencion -------
atencion_promoter<-(df_biblio %>% filter(Satisf_Calidad_Atencion > 8) %>%  count())/n
atencion_detractor<-(df_biblio %>% filter(Satisf_Calidad_Atencion < 7) %>%  count())/n
((atencion_promoter) - (atencion_detractor))*100
#satisf neta: 44.22311




# Salas -------------------------------------------------------------------


modelo_salas <- lm(data= df_salas, Satisf_General ~ Satisf_Temperatura + Satisf_Iluminacion + 
                     Satisf_Espacio + Satisf_CalidadEscritorios + Satisf_DisponibilidadComputadores +
                     Satisf_CalidadComputadores)

summary(modelo_salas)

#sacamos satisf_espacio por tener el p value mas grande (0.6796) y 
# revisamos si esto afecta a nuestros predictores.

modelo_salas <- lm(data= df_salas, Satisf_General ~ Satisf_Temperatura + Satisf_Iluminacion + 
                     Satisf_CalidadEscritorios + Satisf_DisponibilidadComputadores +
                     Satisf_CalidadComputadores)

summary(modelo_salas)

#sacamos Satisf_Iluminacion por tener el p value mas grande (0.39199) y 
# revisamos si esto afecta a nuestros predictores.

modelo_salas <- lm(data= df_salas, Satisf_General ~ Satisf_Temperatura + 
                     Satisf_CalidadEscritorios + Satisf_DisponibilidadComputadores +
                     Satisf_CalidadComputadores)

summary(modelo_salas)


# Satisfaccion neta -------------------------------------------------------

n <- length(df_salas$Satisf_Temperatura) # total de filas

## satisf temperatura sala-----
temper_promoter<-(df_salas %>% filter(Satisf_Temperatura  > 8) %>%  count())/n
temper_detractor<-(df_salas %>% filter(Satisf_Temperatura < 7) %>%  count())/n
((temper_promoter) - (temper_detractor))*100
#satisf neta: -8.938547

## satisf CalidadEscritorios -----
escritorio_promoter<-(df_salas %>% filter(Satisf_CalidadEscritorios  > 8) %>%  count())/n
escritorio_detractor<-(df_salas %>% filter(Satisf_CalidadEscritorios < 7) %>%  count())/n
((escritorio_promoter) - (escritorio_detractor))*100
#satisf neta: -35.19553

## satisf Satisf_DisponibilidadComputadores   -------
disp_PC_promoter<-(df_salas %>% filter(Satisf_DisponibilidadComputadores > 8) %>%  count())/n
disp_PC_detractor<-(df_salas %>% filter(Satisf_DisponibilidadComputadores < 7) %>%  count())/n
((disp_PC_promoter) - (disp_PC_detractor))*100
#satisf neta: -45.81006

## satisf Satisf_CalidadComputadores       -------
calidad_PC_promoter<-(df_salas %>% filter(Satisf_CalidadComputadores > 8) %>%  count())/n
calidad_PC_detractor<-(df_salas %>% filter(Satisf_CalidadComputadores < 7) %>%  count())/n
((calidad_PC_promoter) - (calidad_PC_detractor))*100
#satisf neta: 4.469274


# Casino ------------------------------------------------------------------

names(df_casino)

modelo_casino <- lm(data=df_casino, Satisf_General_Casino ~ Satisf_Comida + Satisf_Calidad_Atencion +
                      Satisf_Rapidez_Atencion)

summary(modelo_casino)

###satisfaccion


n <- length(df_casino$Satisf_Comida) # total de filas

names(df_casino)
## satisf comida-----
comida_promoter<-(df_casino %>% filter(Satisf_Comida  > 8) %>%  count())/n
comida_detractor<-(df_casino %>% filter(Satisf_Comida < 7) %>%  count())/n
((comida_promoter) - (comida_detractor))*100
#satisf neta: 45.66

## satisf Calidad Atencion -----
calidad_promoter<-(df_casino %>% filter(Satisf_Calidad_Atencion  > 8) %>%  count())/n
calidad_detractor<-(df_casino %>% filter(Satisf_Calidad_Atencion < 7) %>%  count())/n
((calidad_promoter) - (calidad_detractor))*100
#satisf neta: 46.79

## satisf Rapidez_Atencion  -------
rapidez_promoter<-(df_casino %>% filter(Satisf_Rapidez_Atencion  > 8) %>%  count())/n
rapidez_detractor<-(df_casino %>% filter(Satisf_Rapidez_Atencion < 7) %>%  count())/n
((rapidez_promoter) - (rapidez_detractor))*100
#satisf neta: 33.58


# Otros ejemplos -----------------------------------------------------------

#base aleatoria
set.seed(2022)
    
satisf_freno       <- runif(250, min=1, max = 10)
satisf_consumo     <- runif(250, min=1, max = 10)
satisf_aceleracion <- runif(250, min=1, max = 10)
satisf_diseño      <- runif(250, min=1, max = 10)
satisf_gral        <- runif(250, min=1, max = 10)

df <- as.data.frame(cbind(satisf_freno, satisf_consumo, satisf_aceleracion, satisf_diseño, satisf_gral))
       

# regresion ---------------------------------------------------------------

modelo_df <- lm(data= df, satisf_gral ~ satisf_freno + satisf_consumo + satisf_aceleracion +
                  satisf_diseño)      
summary(modelo_df)

modelo_df <- lm(data= df, satisf_gral ~ satisf_freno + satisf_consumo  +
                  satisf_diseño)      
summary(modelo_df)


# satisf ------------------------------------------------------------------
n <- length(df$satisf_freno) # total de filas

## satisf satisf_freno    -----
freno_promoter<-(df %>% filter(satisf_freno  > 8) %>%  count())/n
freno_detractor<-(df %>% filter(satisf_freno < 7) %>%  count())/n
((temper_promoter) - (temper_detractor))*100
#satisf neta: -8.938547

## satisf satisf_consumo  -----
consumo_promoter<-(df %>% filter(satisf_consumo  > 8) %>%  count())/n
consumo_detractor<-(df %>% filter(satisf_consumo < 7) %>%  count())/n
((consumo_promoter) - (consumo_detractor))*100
#satisf neta: -42

## satisf satisf_diseño      -------
diseño_promoter<-(df %>% filter(satisf_diseño > 8) %>%  count())/n
diseño_detractor<-(df %>% filter(satisf_diseño < 7) %>%  count())/n
((diseño_promoter) - (diseño_detractor))*100
#satisf neta: -47.6

x<- df$satisf_gral
hist(x, probability = TRUE, col = gray(0.9), main = "distribucion")


# Otros Ejemplos 2 --------------------------------------------------------

#base aleatoria
set.seed(2022)

satisf_freno       <- runif(250, min=1, max = 10) %>% rep(9,75)
satisf_freno       <- runif(rep(9, times=150), min=1, max = 10) 


satisf_consumo     <- runif(250, min=1, max = 10)
satisf_aceleracion <- runif(250, min=1, max = 10)
satisf_diseño      <- runif(250, min=1, max = 10)
satisf_gral        <- runif(250, min=1, max = 10)

df2 <- as.data.frame(cbind(satisf_freno, satisf_consumo, satisf_aceleracion, satisf_diseño, satisf_gral))
df2 <-round(df2,digits = 0)

df2 %>% filter(satisf_freno==9) %>% count()
  # regresion ---------------------------------------------------------------

modelo_df <- lm(data= df, satisf_gral ~ satisf_freno + satisf_consumo + satisf_aceleracion +
                  satisf_diseño)      
summary(modelo_df)

modelo_df <- lm(data= df, satisf_gral ~ satisf_freno + satisf_consumo  +
                  satisf_diseño)      
summary(modelo_df)


# satisf ------------------------------------------------------------------
n <- length(df$satisf_freno) # total de filas

## satisf satisf_freno    -----
freno_promoter<-(df %>% filter(satisf_freno  > 8) %>%  count())/n
freno_detractor<-(df %>% filter(satisf_freno < 7) %>%  count())/n
((temper_promoter) - (temper_detractor))*100
#satisf neta: -8.938547

## satisf satisf_consumo  -----
consumo_promoter<-(df %>% filter(satisf_consumo  > 8) %>%  count())/n
consumo_detractor<-(df %>% filter(satisf_consumo < 7) %>%  count())/n
((consumo_promoter) - (consumo_detractor))*100
#satisf neta: -42

## satisf satisf_diseño      -------
diseño_promoter<-(df %>% filter(satisf_diseño > 8) %>%  count())/n
diseño_detractor<-(df %>% filter(satisf_diseño < 7) %>%  count())/n
((diseño_promoter) - (diseño_detractor))*100
#satisf neta: -47.6

