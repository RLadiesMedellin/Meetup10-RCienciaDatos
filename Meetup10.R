###################
# I. Elementos de R
###################

# 1. Ingresando una entrada:
x <- 1
print(x)
x

y <- "Hola"

# 2. Evaluación 
x <- 2
x
print(x)

x <- 1:50
x

# 3. Tipos de objetos
# R tiene 5 clases de objetos: character, numeric, integer, 
# complex, logical

# 4 . Atributos de objetos
# Son una especie de "metadata" para los objetos, que permiten
# obtener descripciones de los objetos: names, dimnames, 
# dimensions, class, length, ...

# 5. Creación de vectores
x <- c("a", "b", "c")
y <- c(0.5, 0.6)
z <- c(10L, 11L, 15L)
w <- c(0.5 +4i, 0.5-4i)
v <- c("TRUE", "FALSE")

# 6. Mezcla de objetos
x <- c(0.5, "a")
y <- c(TRUE, 2)

# 7. Coercion explicita
x <- 0:6
class(x)

as.numeric(x)
as.logical(x)
as.character(x)

x <- c("a", "b", "c")
class(x)

as.numeric(x)
as.logical(x)
as.complex(x)

# 8. Matriz
m <- matrix(nrow=2, ncol=3)
m

dim(m)

attributes(m)

m <- matrix(1:6, nrow=2, ncol=3)
m
dim(m)

x <- c(1,3,6)
y <- c(10,11,12)
m <- rbind(x,y)
m
m <- cbind(x,y)
m

# 9. Lista
l <- list(1, "a", "TRUE", 1+4i)
l

# 10. Factor
x <- factor(c("male", "female", "female", "female", "male"))
x

table(x)

x <- factor(c("male", "female", "female", "female", "male"),
            levels = c("male", "female"))
x

# 11. Datos faltante
x <- c(1, 5, NA, 10)
is.na(x)


# 12. Data frame
x <- data.frame(frec = 1:4, nombre = c("Prim","Sec","Tec","Prof"))
x
nrow(x)
ncol(x)

# 13. Names
x <- 1:3
names(x)
names(x) <- c("Medellín", "Cali", "Barranquilla")
names(x)


########################
# II. Leyendo datos en R
#######################

library(readr)
df <- read_delim("Delitos.csv", delim = ";",
                 escape_double = FALSE, trim_ws = TRUE)

class(df)

#######################################
# III. Gestionando data frames con dplyr
#######################################

# Identificando atributos en df_victimas
dim(df)

# Cambiando nombres de columnas
names(df)
require(janitor)
df %>% clean_names() -> df0
names(df0)

# Eliminando NA
require(tidyr)
df0 %>% drop_na() -> df1

# Usando select
df1 %>% select(grupo_edad_victima)

df1 %>% select(departamento, grupo_edad_victima,
               grupo_delito, total_victimas)

df1 %>% select(etapa:ley )

df1 %>% select(!pais)

df1 %>% select(!c(pais, pais_nacimiento))


# Usando filter
df1 %>% select(departamento, grupo_edad_victima,
               grupo_delito, total_victimas) -> df2

df1 %>% select(c(departamento, grupo_edad_victima,
                 grupo_delito, total_victimas)) %>%
  filter(grupo_delito=="HURTO")  -> df2

df1 %>% select(c(departamento, grupo_edad_victima,
                 grupo_delito, total_victimas)) %>%
  filter(grupo_delito=="HURTO" &
           grupo_edad_victima=="ADULTO MAYOR DE 60") -> df2

df1 %>% select(c(departamento, grupo_edad_victima,
                 grupo_delito, total_victimas)) %>%
  filter(grupo_delito=="HURTO" &
           grupo_edad_victima=="ADULTO MAYOR DE 60" &
           total_victimas > 50) -> df2

# Usando arrange
df1 %>% select(c(departamento, grupo_edad_victima,
                 grupo_delito, total_victimas)) %>%
  filter(grupo_delito=="HURTO" &
           grupo_edad_victima=="ADULTO MAYOR DE 60" &
           total_victimas > 50)  %>%
  arrange(desc(total_victimas)) -> df2

# Usando rename
df1 %>% select(c(departamento, grupo_edad_victima,
                 grupo_delito, total_victimas)) %>%
  filter(grupo_delito=="HURTO" &
           grupo_edad_victima=="ADULTO MAYOR DE 60" &
           total_victimas > 50)  %>%
  arrange(desc(total_victimas)) %>%
  rename(grupo = grupo_delito,
         total = total_victimas) -> df2

# Usando mutate
df1 %>% select(c(departamento, grupo_edad_victima,
                 grupo_delito, total_victimas)) %>%
  filter(grupo_delito=="HURTO" &
           grupo_edad_victima=="ADULTO MAYOR DE 60" &
           total_victimas > 50)  %>%
  arrange(desc(total_victimas)) %>%
  rename(grupo = grupo_delito,
         total = total_victimas) %>%
  mutate(total_inv = total^-1)  -> df2

# Usando group_by y summarise
df1 %>% select(c(departamento, grupo_edad_victima,
                 grupo_delito, total_victimas)) %>%
  filter(grupo_delito=="HURTO" &
           grupo_edad_victima=="ADULTO MAYOR DE 60" &
           total_victimas > 50)  %>%
  arrange(desc(total_victimas)) %>%
  rename(grupo = grupo_delito,
         total = total_victimas) %>%
  mutate(total_inv = total^-1) %>%
  group_by(departamento) %>%
  summarise(media = mean(total), n =n()) -> df2

######################################################
# III. Analizando data frames graficamente con ggplot2
######################################################

require(ggplot2)

# Hurtos a adultos mayores de 60 anios 
df2 %>%  
  ggplot(aes(x = departamento, y = n)) + 
  geom_bar(stat = "identity")

#Feminicidios por anio
df1 %>% select(c(departamento, grupo_edad_victima, 
                 grupo_delito, anio_denuncia, total_victimas)) %>% 
  filter(grupo_delito=="FEMINICIDIO" ) -> df3

df3 %>% 
  ggplot(aes(x=as.factor(anio_denuncia), y =total_victimas)) +
  geom_boxplot()

# Densidad de feminicidios
df3 %>% 
  ggplot(aes(x = total_victimas)) +
  geom_density()

# Densidad de feminicidios por grupo de edad 
df3 %>% 
  ggplot(aes(x=total_victimas, 
             group=grupo_edad_victima,
             fill=grupo_edad_victima)) +
  geom_density()
