# install.packages(c("dplyr", "readr", "purrr"))
library(dplyr)
library(readr)
getwd()

carpeta_datos <- "data_raw/datos_sesion_04_integracion"
carpeta_blast <- file.path(carpeta_datos, "blast_muestras")

if (!dir.exists("results")) {
  dir.create("results")
}

blast_A <- read_tsv(
  file.path(carpeta_blast, "muestra_A.tsv"),
  show_col_types = FALSE
)

metadata <- read_csv(
  file.path(carpeta_datos, "metadata_secuencias.csv"),
  col_types = cols(codigo_muestra = col_character(), .default = col_guess())
)

head(blast_A)

head(metadata)

metadata %>%
  count(query_id) %>%
  arrange(desc(n))

blast_A %>%
  count(query_id) %>%
  arrange(desc(n))

blast_A_con_metadata <- blast_A %>%
  left_join(metadata, by = "query_id")

blast_A_con_metadata %>%
  select(query_id, subject_id, pident, organismo, origen) %>%
  head()

blast_C <- read_tsv(
  file.path(carpeta_blast, "muestra_C.tsv"),
  show_col_types = FALSE
)

nrow(blast_C)


blast_C %>%
  inner_join(metadata, by = "query_id") %>%
  nrow()

blast_C %>%
  anti_join(metadata, by = "query_id")


metadata %>%
  anti_join(blast_C, by = "query_id")


blast_C %>%
  full_join(metadata, by = "query_id") %>%
  select(query_id, subject_id, organismo) %>%
  arrange(query_id)

metadata_duplicada <- read_csv(
  file.path(carpeta_datos, "metadata_duplicada.csv"),
  col_types = cols(codigo_muestra = col_character(), .default = col_guess())
)

metadata_duplicada %>%
  count(query_id)


nrow(blast_A)

blast_A %>%
  left_join(metadata_duplicada, by = "query_id") %>%
  nrow()

metadata %>%
  count(query_id) %>%
  filter(n > 1)

# ---------------------CLASE 3 A LA QUE ELI ASISTE (18/11/26)-------------------
# Vamos a unir los 3 blast apilandolos (deben de tener la misma cantidad de columnas)
blast_B <- read_tsv(
  file.path(carpeta_blast, "muestra_B.tsv"),
  show_col_types = FALSE
)

blast_total <- bind_rows(
  muestra_A = blast_A,
  muestra_B = blast_B,
  muestra_C = blast_C,
  .id = "muestra" #hace una columna que se llama muestra
)

blast_total %>%
  count(muestra)

#opción por columnas:
## debe cumplir con que tenga la misma cantidad de filas.
bind_cols()


#############Con rizosfera##################
conteos <- read_tsv("/home/ejimenez/Documents/TercerSemestreLCGEJ/Bioinfo1/Intro_a_R/data_raw/datos_rizosfera/conteos_taxonomia_rizosfera.tsv")
metadata <- read_csv("/home/ejimenez/Documents/TercerSemestreLCGEJ/Bioinfo1/Intro_a_R/data_raw/datos_rizosfera/metadata_rizosfera.csv")

metadata %>%
  count(host_name, sort = TRUE)

niveles <- c("kingdom", "phylum", "class", "order", "family", "species")

conteos_limpios <- conteos %>%
  mutate(
    across(
      all_of(niveles),
      ~na_if(sub("^[a-z]__", "", .x), "")
    )
  )

View(conteos_limpios)

# Necesitamos convertir conteos_limpios a formato largo para poderla unir
# a la otra.
# recuperar los ids de las muestras
library(tidyr) # o library(tidyverse)
ids_muestras <- metadata$sample_id
conteos_largos <- conteos_limpios %>%
  pivot_longer(
    cols = all_of(ids_muestras),
    names_to = "sample_id", 
    values_to = "conteo"
  )

datos_riz <- conteos_largos %>% 
  left_join(
    metadata %>% 
      select(sample_id, host_name, country, year),
    by = "sample_id",
    relationship = "many-to-one"
  )


# tabla que para cada sample_id me diga el numero de conteos
total_muestra <- datos_riz %>%
  group_by(sample_id) %>%
  summarise(total_lecturas = sum(conteo),
            .groups = "drop")
head(total_muestra)
write_tsv(total_muestra, "results/total_muestra.tsv")

# tener las abundancias relativas por host_name
abundancias_genero <- datos_riz %>%
  mutate(genus = if_else(is.na(genus), "sin clasificar", genus)) %>%
  group_by(sample_id, host_name, genus) %>%
  summarize(conteo=sum(conteo), .groups = "drop") %>%
  group_by(sample_id) %>%
  mutate(abundancia_relativa = conteo/sum(conteo)) %>%
  ungroup()


#profa
abundancias_genero <- datos_riz %>%
  mutate(genus = if_else(is.na(genus), "Sin clasificar", genus)) %>%
  group_by(sample_id, host_name, genus) %>%
  summarise(conteo = sum(conteo), .groups = "drop") %>%
  group_by(sample_id) %>%
  mutate(abundancia_relativa = conteo / sum(conteo)) %>%
  ungroup()

abundancias_genero %>%
  select(sample_id, host_name, genus, conteo, abundancia_relativa) %>%
  head()
# mandar todo lo que tenga menos del 5% a otros. 
generos_5 <- abundancias_genero %>%
  mutate(
    taxon_final = if_else(
      genus != "sin clasficar" & abundancia_relativa < 0.5, 
      "otros", 
      genus
    ) %>% 
      group_by(
        sample_id, host_name, genus
      ) %>% 
      summarise(abundancia_relativa = sum(abundancia_relativa),
                .groups = "drop")
  )


#profa
generos_5 <- abundancias_genero %>%
  mutate(
    taxon_final = if_else(
      genus != "Sin clasificar" & abundancia_relativa < 0.05,
      "Otros",
      genus
    )
  ) %>%
  group_by(sample_id, host_name, taxon_final) %>%
  summarise(
    abundancia_relativa = sum(abundancia_relativa),
    .groups = "drop"
  )

# Cada muestra debe sumar 1 (salvo pequeñas diferencias numéricas).
comprobacion <- generos_5 %>%
  group_by(sample_id) %>%
  summarise(total = sum(abundancia_relativa), .groups = "drop")

stopifnot(all(abs(comprobacion$total - 1) < 1e-8))
head(comprobacion)


## Lo vamos a integrar con una función

#' Descripción: La función 'preparar_abundancias' toma una base de datos, 
#' un nivel taxonómico y un umbral para clasificar la abundancia relativa.
#' @param: 
#' tabla: la base de datos metagenómicos.
#' nivel: nivel taxonómico con el que se quiere clasificar, los únicos válidos 
#' son "kingdom", "phylum", "class", "order", "family", "genus", "species".
#' Por default = "genus".
#' umbral: indica el mínimo valor de abundancia relativa para considerar 
#' ese taxón significativo. Por default = 0.05
#' @return: 
#' La función devuelve una tabla donde por sample_id y host_name se agrupan los grupos que 
#' tienen una abundancia relativa mayor al umbral, junto con su abundancia relativa.
#' @examples: 
#' preparar_abundancias(datos_riz, "phylum", 0.05)

preparar_abundancias <- function(tabla, nivel = "genus", umbral = 0.05) {
  # Declarar los niveles aceptados por la función
  niveles_validos <- c(
    "kingdom", "phylum", "class", "order",
    "family", "genus", "species"
  )
  # Valida los argumentos dados a la función por el usuario
  stopifnot(
    nivel %in% niveles_validos,
    is.numeric(umbral), length(umbral) == 1,
    !is.na(umbral), umbral >= 0, umbral <= 1
  )
  # Declarar la etiqueta que se le asignará a los grupos que estén por abajo del umbral
  etiqueta_otros <- if (nivel == "phylum") {
    "Filos < 5%"
  } else if (nivel == "genus") {
    "Géneros < 5%"
  } else {
    "Taxones < 5%"
  }
  # Armar la tabla final donde se agrupen los que están por abajo del umbral 
  # por id de la muestra, taxón y nombre del host, y se le agregue la etiqueta
  # a los taxones según si están por arriba del umbral o no.
  tabla %>%
    mutate(
      taxon = .data[[nivel]],
      taxon = if_else(is.na(taxon) | taxon == "",
                      "Sin clasificar", taxon)
    ) %>%
    group_by(sample_id, host_name, taxon) %>%
    summarise(conteo = sum(conteo), .groups = "drop") %>%
    group_by(sample_id) %>%
    mutate(abundancia_relativa = conteo / sum(conteo)) %>%
    ungroup() %>%
    mutate(
      taxon_final = if_else(
        abundancia_relativa < umbral,
        etiqueta_otros,
        taxon
      )
    ) %>%
    group_by(sample_id, host_name, taxon_final) %>%
    summarise(
      abundancia_relativa = sum(abundancia_relativa),
      .groups = "drop"
    )
}

# asignarlo a un nombre de la tabla

filos_5 <- preparar_abundancias(datos_riz, "phylum", 0.05)
filos_5
genus_5 <- preparar_abundancias(datos_riz, "genus", 0.05)

# guardarlo como .tsv
write_tsv(genus_5, "results/generos_5.tsv")
write_tsv(filos_5, "results/filos_5.tsv")



################################GGPLOT2
library(ggplot2)
# Vamos a trabajar con estas bases de datos
# generos_5
# filos_5
# total_muestra
# metadata

profundidad <- total_muestra %>% 
  left_join(
    metadata %>% 
      select(
        sample_id, host_name, country, crop_system
      ),
    by = "sample_id",
    relationship = "one-to-one"
  )