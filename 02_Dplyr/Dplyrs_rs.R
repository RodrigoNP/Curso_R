
########
## Paquete Data Pliers
#######

rm(list = ls())

### Instalacion de paquetes ### 

# Cargamos paquetes (recuerda instalarlos antes)##
# instalamos con: install.packages('paquete')

library(tidyverse)


### Importacion de datos ###

## Datos ya en R 

# Si queremos ver la lista de datos ya presentes en R

 # data()

# Guardamos las bases de datos

cars<- mtcars

sueño<- sleep


## Directorios 

# Consultar el directorio
getwd()

# Cambiar el directorio
setwd('C:/Users/rodri/OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO/Curso_R/02_Dplyr/Data')

# Guardamos la base como un objeto

presidencial<- readxl::read_excel('presidencia.xlsx', skip = 5)

# Analogamente

presidencial<- readxl::read_excel('C:/Users/rodri/OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO/Curso_R/02_Dplyr/Data/presidencia.xlsx',
                                  skip = 5)

### Verbos ###

## rename()

presidencial<-presidencial %>% 
  rename(votos=TOTAL_VOTOS_CALCULADOS,
         lista_nominal=LISTA_NOMINAL_CASILLA)

## mutate() 
# Se utiliza para aniadir variables

presidencial<- presidencial %>% 
  mutate(participacion=votos/
           lista_nominal)

# Si queremos usar el formato funcion

presidencial<-mutate(presidencial,
                     participacion=votos/
                       lista_nominal)

# ifelse()

# Creemos una dummy que indique si una casilla es rural
presidencial<-presidencial %>% 
  mutate(
   rural=ifelse(CASILLA=='Rural',1,0)
  ) 


## filter () 

# Quedemonos con las observaciones en las que hubo votos por algun independiente
independientes<-presidencial %>% 
  filter(CAND_IND_01>0 | CAND_IND_02>0)


# select()

morena<- presidencial %>% 
  select(CLAVE_CASILLA, NOMBRE_ESTADO,
         contains('MORENA'))

### Agrupar ### 

# Agrupar por estado

presidencial<- presidencial %>% 
  group_by(NOMBRE_ESTADO) %>% 
  mutate(
   voto_morena= mean(MORENA, na.rm=T)
  )

# sumarise()

prom_estados<- presidencial %>% 
  group_by(NOMBRE_ESTADO) %>% 
  summarise(
      across(PAN:CAND_IND_02, sum)
  )


## rowwise() 

# Para operar con dplyr a lo largo de las filas

presidencial<- presidencial %>% 
  rowwise() %>% 
  mutate(
    total_morena= sum(c_across(contains('MORENA')))- voto_morena
    )


presidencial<- presidencial %>% 
  mutate(
    participativo=case_when(
      participacion<0.30 ~ 'poco',
      participacion>0.30 & participacion<=0.60 ~ 'promedio',
      participacion>0.60 ~ 'muy'
    )
  )


## pivot_longer()

presidencial_long<- presidencial %>% 
  pivot_longer(PAN:CAND_IND_02,
               names_to = 'partido',
               values_to = 'votos_partido') %>% 
  select(CLAVE_CASILLA, partido, votos_partido, everything())


# Agrupar con con un formato long()
# Para obtener los votos por partido a nivel nacional
voto.por.partido<- presidencial_long %>% 
  group_by(partido) %>% 
  summarise(
    nacional_partido=sum(votos_partido, na.rm = T)
  )

# arrange()

# usemos arrange para identificar al ganador para cada casilla

presidencial_long <- presidencial_long %>% 
  group_by(CLAVE_CASILLA) %>% 
  arrange(desc(votos_partido), .by_group = T) %>% 
  mutate(ranking=row_number(),
         ganador=first(partido))

## pivot_wider()

presidencial_wide<- presidencial_long %>% 
  pivot_wider(names_from = partido,
              values_from = votos_partido)
  