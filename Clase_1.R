# Suma
a <- 2
b <- 4
a+b

#Instalar paquetes 
install.packages("BiocManager")
library(BiocManager)

#Ayuda para un comando
help("remove.packages")
?remove.packages

#Verificar la versión de la librería
packageVersion("ggplot2")

# R tools


R.version.string

Sys.info()[["sysname"]]

#¿cómo saber si tengo un paquete?
"ggplot2" %in% rownames(installed.packages())
%in% # verifica si lo de la izquiera está en la derecha.
  
"dplyr" %in% rownames(install.packages())

library(ggplot2)
library(dplyr)
packageVersion("ggplot2")
packageVersion("dplyr")


# OBJETOS EN R
identificador <- "gene_101"
class(identificador)

longitud <- 450L
class(longitud)

proporcion <- 0.53
class(proporcion)

anotado <- TRUE
class(anotado)

## conocer los datos (clases de objetoos)
class() #clase
typeof() #cómo se guarda en R
str() # datos : )


## 
caracter_numerico <- "2"
class(caracter_numerico)
num <- as.numeric(caracter_numerico)
class(num)

## Estructura de datos en R
# dataframe, vectores, listas, 

## Vector
genes <- c("gA", "gB", "gC", "gD")
length(genes) #longitus
genes[2] #obtener algo del vector

longitudes <- c(150, 400, 220, 150)
genes[longitudes >= 300]
longitudes / 3

sum(longitudes > 300) # te dice cuántos son mayores

# ejercicio:
ids <- c("g1", "g2", "g3", "g4", "g5")
tam <- c(90, 450, 310, 120, 600) 

ids[3]
ids[tam >= 300]
sum(tam >= 300)


# INtroducir valores faltantes
valores <- c("12", "18", "sin_datos")
valores <- as.numeric(valores)
  # Warning message:
  # NAs introduced by coercion 

anyNA(valores)
is.na(valores)

## Matrices y arreglos. 
conteos <- matrix(
  c(12, 8, 4, 6, 10, 2), 
  nrow=3,
  dimnames=list(c("g1", "g2", "g3"), c("M1", "M2"))
)
conteos
conteos[2,2] #fila, columna
conteos[2, 1]
conteos[,"M2"]
dim(conteos)
str(conteos)

# Arreglos
arreglos <- array(1:8, dim = c(2,2,2))
arreglos
dim(arreglos)
str(arreglos)

# dataframe
# diferencia con matriz?? dataframe admite columnas de diferentes tipos
anotaciones <- data.frame(
  gen = c("g1", "g2", "g3"),
  logitudes = c(450, 120, 800),
  prod = c("transp.", "hip.", "enzima")
)

anotaciones
str(anotaciones)
anotaciones[,2] #columnas
anotaciones$logitudes #columnas
anotaciones[2,] # filas

## LIstas: información de diferente longitud y tipo
muestra <- list(
  id = "M01",
  secuencias = c("ATCG", "CCCATTA"),
  anotaciones = anotaciones
)

#primer vector
muestra[1]
muestra$id
muestra[["secuencias"]] # vector
muestra["secuencias"] # elemento de la lista

# Factor: le asignas nivel a una variable no numéricas
condicion <- c("control", "tratamiento", "control")
condicion2 <- factor(c("control", "tratamiento", "control"), 
                    levels = c("control", "tratamiento"))
levels(condicion)
levels(condicion2)

# ejercicio:
letras <- c("A", "M", "A", "B", "M", "M", "B", "A", "B", "B")
letras2 <- factor(c("A", "M", "A", "B", "M", "M", "B", "A", "B", "B"), 
                  levels = c("A", "M", "B")) # siempre el que escribes 
#primero es el que toma como el más grande y viceverza
letras2


# Estructuras de control
## Condicionales
### if
secuencia <- "ATGCGGTAGTGCG"
longitud <- nchar(secuencia)
if (longitud >= 10) {
  print("La secuencia tiene al menos longitud de 10")
} 
### if else
if (longitud >= 10){
  print("La secuencia tiene al menos longitud de 10")
} else {
    print("la secuencia tiene longitud menor a 10")
}

# Ejercicio
conteo <- c(67)

if (anyNA(conteo) == TRUE){
  print("Dato faltante")
} else if(conteo < 0) {
  print("conteo inválido")
} else if (conteo == 0){
  print("sin lecturas")
} else if (conteo > 0){
  print("con lecturas")
}

conteos <- c(0, 12, 4, 0, 25)

estado <- ifelse(conteos > 0, "Detectado", "No detectado")
estado
any(conteos == 0)
which(conteos == 0)

## Ciclos
### for

for (element in collection){
  intrucciones
}

for (i in 1:5){
  print(i)
}

secuencias <- c("ATCGCGCCG", "CCCT", "ATATATAATATATATAT")
longitudes <- numeric(length(secuencias))
longitudes
for (i in seq_along(secuencias)){
  longitudes[i] <- nchar(secuencias[i])
}
longitudes

# seq_along accede a cada entrada de secuencias

# ejercicio:
clasif <- numeric(length(secuencias))

for (i in seq_along(longitudes)){
  if (longitudes[i] < 5){
    clasif[i] <- "corta"
    }
    else if (longitudes[i] >= 5){
      clasif[i] <- "larga"
    }
}
clasif
longitudes

### while
while (condition){
  instruction
}

i = 0
while (i){
  print(i)
  i <- i + 1
}

limite <- 50
numero <- 0

while (numero^2 <= limite){
  print(numero)
  numero <- numero + 1
}
numero

cadena <- c("A")
limite2 <- 8
longitud <- length(cadena)

while (longitud < limite2){
  paste0(cadena, "A")
  print(cadena)
}
cadena

#------------------CLASE 2---------------------------------#
longitud_adn <- function(secuencia){
  longitud <- nchar(secuencia)
  return(longitud)
}
longitud_adn("ATGTBATBJ")

#si puede encontrar caracteres en la secuencia original
contiene_motivo <- function(secuencia, motivo){
  grepl(motivo, secuencia, fixed = "TRUE")
}
contiene_motivo("atcTCT", "CT")

empieza_con <- function(secuencia, prefijo){
  startsWith(secuencia, prefijo)
}

empieza_con("ATATATATA", "C")
empieza_con("atatat", "a")

preparar_secuencia <- function(secuencia, tipo = "ADN", mayusculas=TRUE){
  if (mayusculas){
    secuencia <- toupper(secuencia)
  }
  if (tipo == "ARN"){
    secuencia <- chartr("T", "U", secuencia)
  }
  secuencia
}

preparar_secuencia("agcgcg")
preparar_secuencia("agcgcg", mayusculas = FALSE)
preparar_secuencia("agcgcg",  tipo == "ARN", mayusculas = TRUE)

resumir_adn <- function(id, secuencia){
  secuencia <- toupper(secuencia)
  longitud <- nchar(secuencia)

  list(id = id,
        longitud = longitud)
}

resultado <- resumir_adn("gen1", "atgcatgcatgc")
resultado$id

# que también me de 
resumir_adn <- function(id, secuencia){
  secuencia <- toupper(secuencia)
  longitud <- nchar(secuencia)
  basesg <- strsplit(secuencia, "G", fixed = TRUE)
  basesc <- strsplit(secuencia, "C", fixed = TRUE)
  longitudc <- length(basesc) 
  longitudg <- length(basesg)
  longitud_gc <- longitudg + longitudc
  gc <- longitud_gc / longitud
  
  list(id = id,
       longitud = longitud, 
       gc = gc)
}

resumir_adn("gen1", "ATGC")

validar_adn <- function(secuencia){
  if (!is.character(secuencia) || 
      length(secuencia) != 1 ||
      is.na(secuencia) ||
      !grepl("^[AGCT]+$", toupper(secuencia))){
    stop("se requiere una secuencia no vacía, con bases A,C,G,T")}
  TRUE
}
validar_adn("agct")
validar_adn("AH")

longitud_validada <- function(secuencia){
  stopifnot(is.character(secuencia),
            length(secuencia) == 1, 
            !is.na(secuencia))
  nchar(secuencia)
}
  
longitud_validada("B")

#Ejercicio

clasificar_secuencias <- function(secuencias){
 stopifnot(is.character(secuencias))
  
  es_valida <- revisar_secuencias(secuencias)
  
  list(
    validas = secuencias[es_valida],
    invalidas = secuencias[!es_valida])
}


clasificar_secuencias(c("AT", "GC", "", 3))  


# CORRER SCRIPTS
# EN R: source("ruta")
# Rscript [ruta]


