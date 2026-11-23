# Cargar librerías pertinentes.
library(readr)
# Comprobar que existen resultados_blast.tsv, metadata_secuencias.csv y secuencias.fasta. 
file.exists("/home/ejimenez/Documents/TercerSemestreLCGEJ/Bioinfo1/Intro_a_R/data_raw/datos_sesion_02_blast/resultados_blast.tsv")
file.exists("/home/ejimenez/Documents/TercerSemestreLCGEJ/Bioinfo1/Intro_a_R/data_raw/datos_sesion_02_blast/metadata_secuencias.csv")
file.exists("/home/ejimenez/Documents/TercerSemestreLCGEJ/Bioinfo1/Intro_a_R/data_raw/datos_sesion_02_blast/secuencias.fasta")

# Importar las dos tablas con readr y conservar codigo_muestra como texto. 

# metadata_secuencias.csv
metadata <- readr::read_csv(
  "data_raw/datos_sesion_02_blast/metadata_secuencias.csv",
  col_types = readr::cols(codigo_muestra = readr::col_character()),
  show_col_types = FALSE
)

# blast_comentarios.tsv
blast <- readr::read_tsv(
  "data_raw/datos_sesion_02_blast/resultados_blast.tsv",
  show_col_types = FALSE
)
# Mostrar dimensiones, nombres y tipos de columnas. 
## Metadata
dim(metadata)
colnames(metadata)
sapply(metadata, class)

## Blast
dim(blast)
colnames(blast)
sapply(blast, class)

# Identificar los query_id distintos y contar los resultados por consulta. 
cat("Existen", length(unique(resultados_blast$query_id)), "query_id distintos \n")
cat("Existen", length(which(sapply(resultados_blast, is.numeric))), "resultados por consulta \n")

# Mostrar las filas con pident >= 90 y explicar qué representa este filtro. 
filas_90 <- which(resultados_blast$pident >= 90)
cat("Las filas con pident mayor o igual a 95 son:", which(resultados_blast$pident >= 90))
resultados_blast[filas_90, ]
# Este filtro representa el paso de separar los alineamientos donde se reporta un porcentaje de identidad
# menor a 90% de las que son iguales o mayores a 90%. Este umbral puede variar pero determina
# qué tanto nos interesa que se parezca nuestra query a la diferencia. 


# Leer el FASTA e indicar cuántos encabezados contiene. 
secuencias.fasta <- readLines("/home/ejimenez/Documents/TercerSemestreLCGEJ/Bioinfo1/Intro_a_R/data_raw/datos_sesion_02_blast/secuencias.fasta")
encabezados <- secuencias.fasta[grep('>', secuencias.fasta )]
cat("el archivo contiene", length(entradas), "encabezados")

# Mostrar si alguna de las tablas tiene valores faltantes.

# ¿Hay algún NA en cada tabla? (TRUE/FALSE)
cat("¿resultados_blast tiene NA?:", anyNA(blast), "\n")
cat("¿metadata_secuencias tiene NA?:", anyNA(metadata), "\n")

# Escribe al final del script, como comentarios, tres observaciones sobre la estructura de 
# los datos, sin extraer conclusiones biológicas.

# Observaciones sobre la estructura de los datos:
#
# 1. La tabla de resultados de BLAST tiene 20 filas y 6 columnas, y cada fila
#    corresponde a un alineamiento entre una secuencia consulta y una secuencia de
#    referencia. Como solo hay 8 query_id distintos, una misma consulta aparece en
#    varias filas, de modo que la tabla no tiene una fila por secuencia sino una por
#    cada resultado obtenido.
#
# 2. La columna codigo_muestra se importó como texto de forma explícita, porque si
#    readr la leyera como número podría eliminar ceros a la izquierda o tratar como
#    cantidad lo que en realidad es un identificador. Las columnas de la tabla de
#    BLAST como pident se importaron como numéricas, lo que permite filtrarlas con
#    comparaciones como pident >= 90.
#
# 3. Ninguna de las dos tablas presenta valores faltantes, según anyNA(). Además, 
#    el archivo FASTA contiene 8 encabezados, número que coincide con los 8 query_id
   distintos de la tabla de BLAST.
