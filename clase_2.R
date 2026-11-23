# Cómo cargar y explorar datos
getwd()
list.files("/home/ejimenez/Documents/TercerSemestreLCGEJ/Bioinfo1/Intro_a_R")
file.exists("data_raw")
archivo_blast <- "data_raw/datos_sesion_02_blast/resultados_blast.tsv"
readLines(archivo_blast, n=5)

blast <- read.delim("data_raw/datos_sesion_02_blast/resultados_blast.tsv")
metadata <- read.csv("data_raw/datos_sesion_02_blast/metadata_secuencias.csv",
                     colClasses = c(codigo_muestra = "character"))

library(readr)
blast <- readr::read_tsv("data_raw/datos_sesion_02_blast/resultados_blast.tsv", 
                         show_col_types = FALSE)
class(blast)

readLines("data_raw/datos_sesion_02_blast/blast_comentarios.tsv")

#quitar las primeras lineas
blast_con_comentarios <- readr::read_tsv("data_raw/datos_sesion_02_blast/blast_comentarios.tsv",
                                         comment = "#", 
                                         show_col_types = FALSE)
View(blast_con_comentarios)

blast_na <- readr::read_tsv("data_raw/datos_sesion_02_blast/blast_valores_faltantes.tsv",
                            na = c("", "NA"), #nos ayuda a decirle culaes son valores faltantes
                            show_col_types = FALSE)

str(blast)
summary(blast)
readr::spec(blast)

#nos ayuda a saber si las declaraciones del dataset son congruentes
readr::problems(blast)
blast_error <- read_tsv(
  I("secuencia\tidentidad
  seq1\t98.5
  seq2\t95
  seq3\terror
  se4\t98"), 
  col_types = cols(
    secuencia = col_character(), 
    identidad = col_double()
  )
)

dim(blast)

#
# ¿Cuántos registros y columnas tiene la tabla? 20 y 6
nrow(blast)
ncol(blast)
#   ¿Cuántos query_id diferentes aparecen? 8
unique(blast$query_id)
#   ¿Cuántos resultados tiene cada consulta? 4

#   ¿Qué filas tienen pident mayor o igual a 95? 3
mayor <- which(
  blast$pident >= 95
) 
length(mayor)

#   ¿Hay valores faltantes? non
anyNA(blast)
