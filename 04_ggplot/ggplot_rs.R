###### 
## ggplot
######

rm(list = ls())

## Cargamos paquetes
library(tidyverse)

## Trabajemos con mtcars

cars<- mpg


## Graficando la relación entre tamaño y eficiencia
ggplot(data=cars)+
  geom_point(mapping = aes(x= displ, y= hwy))

# Podemos guardar las gráficas como objetos
figura.1<- ggplot(data=cars)+
  geom_point(mapping = aes(x= displ, y= hwy))

# Si queremos que lo muestre, lo podemos poner entre paréntesis

(figura.1<- ggplot(data=cars)+
    geom_point(mapping = aes(x= displ, y= hwy)))

# Una vez creada la grafica, podemos añadir elementos

(ggplot(data=cars)+
  geom_point(mapping = aes(x= displ, y= hwy))+
  xlab('Volumen Motor (l)')+
  ylab('Millas por galón')+
  ggtitle('Eficiencia')+
  theme_minimal()+
    coord_flip())
  
# Si ya tenemos la gráfica como objeto guardado, basta con poner el nombre del objeto seguido de los elementos

(figura.1+ 
    xlab('Volumen Motor (l)')+
    ylab('Millas por galón')+
    ggtitle('Eficiencia')+
    theme_minimal()+
    coord_flip())


### Parte de dplyr ###

# Podemos concatenar ggplot usando la pipa %>%. En este caso, omitimos el argumento de df y ponemos solo la aestetica dentro de ggplot()
# Por ejemplo repliquemos la gráfica, pero solo para los compactos

(figura.2<- cars %>% 
    filter(class=='compact') %>% 
    ggplot(aes(x=displ, y =hwy))+
    geom_point())


### Aestetica ###

# Podemos vincular a una caracteristica estetica del grafico a una variable de factor, que, a la postre, va a asignar un nivel a cada categoria

(aes<- ggplot(data=cars)+
    geom_point(mapping = aes(x= displ, y= hwy,
                             color=class)))

# Podemos haer lo mismo con formas

(aes.2<- ggplot(data=cars)+
    geom_point(mapping = aes(x= displ, y= hwy, 
                             color=class, shape=class)))

## Dentro vs fuera de aes() ##

# Para especificar que queremos un color en particular ponemos el argumento dentro de la geometria pero fuera de aes(). No vinculamos una cuestión estética con una variable, sino que solo forzamos que la figura sea un color especifico

(fuera_aes<- ggplot(data=cars)+
    geom_point(mapping = aes(x= displ, y= hwy) , 
               color='red'))

## aplha ##

(aes.3<- ggplot(data=cars)+
    geom_point(mapping = aes(x= displ, y= hwy, 
                             color=class, alpha=class)))

## Escalas de colores 

# Descarguemos la paleta Viridis

library(viridis)

(scale_color <- ggplot(data=cars)+
    geom_point(mapping = aes(x= displ, y= hwy,
                             color=class))+
    scale_color_viridis(discrete = T, 
                        option = 'magma'))

### Facets ###

# Podemos dividir los facets, haciendo una grafica para cada categoria

# Tenemos que especificar la formula con la variable para la que queremos descomponer la grafica antcipiada por ~ 

(facets <- ggplot(cars)+
    geom_point(mapping = 
                 aes(x= displ, y= hwy))+
    facet_wrap(~ class, nrow = 2))

# Podemos descomponer en varias variables especificando en la forumla

(facets <- ggplot(cars)+
    geom_point(mapping = 
                 aes(x= displ, y= hwy))+
    facet_wrap(cyl ~ class))

#### Otra sintaxis ####

# Podemos omitir cosas como mapeo x y y 
# Podemos incluir la aestetica en ggplot() pero el resto de los argumentos de la geometria siguen especificandose dentro de la geometria

(figura.1.2 <- ggplot(cars, 
                      aes(displ, hwy, color= class))+
   geom_point())

#### Geometrías ###

## geom_smooth() 
# aniade linea de regresion e intervalos de confianza

(smooth <- ggplot(cars)+
    geom_smooth(mapping = aes(
      x=displ, y= hwy,
      linetype= drv, color=drv
    )))

## Múltiples geometrías ##

# Con la sintaxis alternativa es muy snecillo aniadir multiples geometrias
# Solo especificamos las cosas comunes en ggplot()


(mult_geom <- ggplot(cars, 
                     aes(displ, hwy, 
                         color= drv, linetype=drv))+
    geom_point()+
    geom_smooth())

# Podemos jugar con las opciones

(mult_geom.2 <- ggplot(mpg,
                       aes(displ, hwy))+
    geom_point(aes(color=class))+
    geom_smooth(data= filter(mpg,
                             class=='subcompact'), color='red'))




## Grafico de barras ##

# su stat default es count

(bar <- ggplot(cars)+
  geom_bar( aes(class)))
  

# Creemos un df para hacer el grafico de barras

cars_count <- cars %>% 
  group_by(class) %>% 
  summarise(n=n())

# podemos hacer lo mismo, pero especificando las variables ya hechas

# stat= 'idenity'

(cars_count <- cars %>% 
  group_by(class) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(class, n))+
  geom_bar(stat = 'identity'))

## Gráfico de barras de proporciones

(bar_prop <- ggplot(cars)+
    geom_bar(aes(class, y=..prop..,
                 group=1)))

## fill vs color ##

(bar_color <- ggplot(cars)+
    geom_bar(aes(class,
                 color=drv)))

(bar_fill <- ggplot(cars)+
    geom_bar(aes(class,
                 fill=drv)))

## position = ' ' ##

(bar_position_fill <- ggplot(cars)+
    geom_bar(aes(class,
                 fill=drv), position='fill'))

(bar_position_dodge <- ggplot(cars)+
    geom_bar(aes(class,
                 fill=drv), position='dodge'))


#### Histograma y densidad ####

# para generar el df

set.seed(2022)

n <- 100
mean_h <- 30000
mean_m <- 24000
sd_h <- 3000
sd_m <- 5000


salarios <- tibble(
  id=seq(n),
  sexo= factor(rep(c('m','h'), each= n/2)),
  salario= round(c(rnorm(n/2, mean_m, sd_m),
                   rnorm(n/2, mean_h, sd_h)))
)



# Densidad con línea vertical

(density <- ggplot(salarios,
                   aes(salario))+
    geom_density()+
    geom_vline(aes(xintercept=mean(salario))))

# si queremos distinguir por sexo

(density_group <- ggplot(salarios,
                   aes(salario, color= sexo))+
    geom_density()+
    geom_vline(aes(xintercept=mean(salario)),
               color='red'))



