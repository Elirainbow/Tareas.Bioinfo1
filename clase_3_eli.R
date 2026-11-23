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


abundancias_genero <- datos_riz %>%
  mutate(genus = if_else(is.na(genus), "sin clasificación", genus)) %>%
         group_by(sample_id, host_name, genus) %>%
           summarize(conteo=sum(conteo), .groups = "drop") %>%
           group_by(sample_id) %>%
           mutate(abundancia_relativa = conteo/sum(conteo)) %>%
           ungroup()

