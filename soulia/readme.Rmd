---
title: "Thèse Sara S."
author: "Francesco MONTI"
date: "`r Sys.Date()`"
output: 
  html_document: 
    toc: true
editor_options: 
  
  chunk_output_type: console
---

```{r setup, include=FALSE}
library(magrittr) # Operator %>% and additional pipe-friendly functions.
library(tidyverse) # The main "tidyverse" packages.
library(openxlsx)
library(ggplot2)
library(DT)
library(conflicted)
library(knitr)
library(fmckage)
library(htmltools)

knitr::opts_chunk$set(
  echo       = FALSE, # Should blocks with program code be shown in knitted documents?
  eval       = TRUE, # Should program code be evaluated?
  fig.height = 6, # Default height for plots.
  fig.width  = 10, # Default width for plots.
  fig.align  = "center" # Default alignment for plots in knitted documents.
)

theme_set(theme_bw()) # Default ggplot2 theme


conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

data <- read.xlsx("tableau_clean.xlsx", sheet = 1) %>%
  as_tibble() %>%
  select(id:commentaires)
```

```{r}
# Patient vide
data <- data[-204, ]
```


```{r}
data <- data %>%
  mutate(across(where(is.character), function(x) str_squish(x)),
    across(everything(), function(x) ifelse(as.character(x) == "", NA, x)),
    across(everything(), function(x) ifelse(as.character(x) == "?", NA, x)),
    taille = as.numeric(str_replace(taille, ",", ".")),
    taille = case_when(
      taille < 10 ~ taille * 100,
      taille < 100 ~ taille + 100,
      TRUE ~ taille
    ),
    imc = round(as.numeric(poids) / (as.numeric(taille) / 100)^2, 2)
  )

data <- data %>% mutate(oh = ifelse(oh %in% c("Jamais", "Non sevré", "Sevré"), oh, NA))

```


# Exploration données
## Dimensions
`r nrow(data)` patients, `r ncol(data)` colonnes.

## Données manquantes
Pour certaines variables, valeur manquante = "absence" très probablement, pour d'autre c'est plus difficile à dire. Dans tous les cas, scientifiquement ce n'est pas une position idéale.

```{r tableau valeurs manquantes}
apply(data, 2, function(x) sum(is.na(x))) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  `colnames<-`(c("variable", "valeurs_manquantes")) %>%
  mutate(pourcentage = round(valeurs_manquantes * 100 / nrow(data), 2)) %>%
  datatable(
    caption = htmltools::tags$caption(
      style = "caption-side: top; text-align: left;
                                     font-size: 18px; font-weight: bold;
                                     padding: 10px;",
      "Valeurs manquantes par variable"
    ),
    extensions = "Buttons",
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      scrollY = TRUE,
      dom = "Bfrtip",
      buttons = list(
        "copy",
        # list(extend = "csv",   filename = "consommation_oh"),
        list(extend = "excel", filename = "tableau_valeurs_manquantes")
        # list(extend = "pdf",   filename = "consommation_oh")
      )
    )
  )
```

```{r}
# need to fix chu labels
# Define caption (and reuse for filename)
caption_text <- "Valeurs manquantes par variable et ville"
data %>%
  group_by(ville_suivi) %>%
  summarise(
    across(everything(), ~ sum(is.na(.))),
    nb_patients = n()
  ) %>%
  rowwise() %>%
  mutate(
    na_count = sum(c_across(where(is.numeric))),
    na_count_per_row = na_count / nb_patients
  ) %>%
  relocate(na_count, .after = ville_suivi) %>%
  relocate(nb_patients, .after = ville_suivi) %>%
  relocate(na_count_per_row, .after = na_count) %>%
  arrange(desc(na_count_per_row)) %>%
  mutate( # na_pourcentage = na_count*100/(nb_patients*(ncol(.)-2)),
    na_pourcentage = na_count * 100 / (nb_patients * (ncol(data) - 2)),
    across(where(is.numeric), function(x) round(x, 2))
  ) %>%
  relocate(na_pourcentage, .after = na_count_per_row) %>%
  datatable(
    caption = htmltools::tags$caption(
      style = "caption-side: top; text-align: left;
                        font-size: 18px; font-weight: bold;
                        padding: 10px;",
      caption_text
    ),
    extensions = "Buttons",
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      scrollY = TRUE,
      dom = "Bfrtip",
      buttons = list(
        "copy",
        # list(extend = "csv",   filename = "consommation_oh"),
        list(extend = "excel", filename = caption_text)
        # list(extend = "pdf",   filename = "consommation_oh")
      )
    )
  )
```


## Sexe & age
```{r sexe et age}
# Define caption (and reuse for filename)
caption_text <- "Sexe"

data %>%
  ffreq("sexe") %>%
  select(-Variable) %>%
  datatable(
    caption = tags$caption(
      style = "caption-side: top; text-align: left; font-size: 18px;
               font-weight: bold; padding: 10px;",
      caption_text
    ),
    extensions = "Buttons",
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      scrollY = TRUE,
      dom = "Bfrtip",
      buttons = list(
        "copy",
        list(extend = "excel", filename = caption_text)
      )
    )
  )


# Define caption (and reuse for filename)
caption_text <- "Age by Sexe"
fmckage::fdescribe(data, "age", "sexe") %>%
  datatable(
    caption = tags$caption(
      style = "caption-side: top; text-align: left; font-size: 18px;
        font-weight: bold; padding: 10px;",
      caption_text
    ),
    extensions = "Buttons",
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      scrollY = TRUE,
      dom = "Bfrtip",
      buttons = list(
        "copy",
        # list(extend = "csv",   filename = "consommation_oh"),
        list(extend = "excel", filename = caption_text)
        # list(extend = "pdf",   filename = "consommation_oh")
      )
    )
  )
```

## Age diag VHD
```{r}
# Define caption (and reuse for filename)
caption_text <- "Age au diag VHD"
data %>%
  group_by(sexe) %>%
  summarise(
    mean =
      mean(age_diag_vhd, na.rm = TRUE),
    sd = sd(age_diag_vhd, na.rm = TRUE),
    min = min(age_diag_vhd, na.rm = TRUE),
    max = max(age_diag_vhd, na.rm = TRUE),
    range = max(age_diag_vhd, na.rm = TRUE) - min(age_diag_vhd, na.rm = TRUE),
    se = sd(age_diag_vhd, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), function(x) round(x, 2))) %>%
  datatable(
    caption = tags$caption(
      style = "caption-side: top; text-align: left; font-size: 18px;
               font-weight: bold; padding: 10px;",
      caption_text
    ),
    extensions = "Buttons",
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      scrollY = TRUE,
      dom = "Bfrtip",
      buttons = list(
        "copy",
        # list(extend = "csv",   filename = "consommation_oh"),
        list(extend = "excel", filename = caption_text)
        # list(extend = "pdf",   filename = "consommation_oh")
      )
    )
  )
```

## OH
```{r}
# Define caption (and reuse for filename)
caption_text <- "Consommation OH by sexe"

data %>%
  count(oh, sexe) %>%
  group_by(sexe) %>%
  mutate(pourcentage_by_sexe = round(n * 100 / sum(n), 2)) %>%
  arrange(sexe) %>%
  ungroup() %>%
  mutate(pourcentage_global = round(n * 100 / sum(n), 2)) %>%
  datatable(
    caption = tags$caption(
      style = "caption-side: top; text-align: left; font-size: 18px;
               font-weight: bold; padding: 10px;",
      caption_text
    ),
    extensions = "Buttons",
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      scrollY = TRUE,
      dom = "Bfrtip",
      buttons = list(
        "copy",
        # list(extend = "csv",   filename = "consommation_oh"),
        list(extend = "excel", filename = caption_text),
        # list(extend = "pdf",   filename = "consommation_oh")
      )
    )
  )
```

```{r}
data %>%
  mutate(
    sero_vih_t1 = ifelse(is.na(sero_vih_t1), 0, sero_vih_t1),
    sero_vhc_t1 = ifelse(is.na(sero_vhc_t1), 0, sero_vhc_t1)
  )



# si cv_detectable = NA mais reduction_cv_vhd_1 = x, mettre cv_detectable_1 = 0
```

```{r}
data %>%
  mutate(cirrhose_bin = ifelse(complications_cirrhose == "Non", 0, 1))
```

```{r}
data %>%
  select(id, vhb_traitement, vhd_traitement_1) %>%
  filter(vhd_traitement_1 != "Non traité" & vhb_traitement == "Non traité") %>%
  view()

# remplacer vhb traitement avec des vides
```
