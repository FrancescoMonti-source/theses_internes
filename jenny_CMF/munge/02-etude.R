# Munging 2nd excel file

# Renaming for comfort
etude = etude.Feuil1
    # removing old object
rm(etude.Feuil1)

names(etude) = c("patid","bilaterale","cote_opere","prelevement_iliaque", "age",
  "age_intervention","annee_intervention","etat_bd","avulsion_dentaire",
  "compliance_ttt","odf_prechir","odf_prechir_years","odf_prechir_months","duree_sej",
  "complications")

etude = etude %>%
    tibble() %>%
    mutate(age_intervention = convert_to_months(age_intervention),
           across(where(is.character), function(x) rm_accent(x)),
           avulsion_dentaire = case_when(
               avulsion_dentaire == "oui" ~ "1",
               avulsion_dentaire == "non" ~ "0",
               TRUE ~ NA_character_),
           compliance_ttt = case_when(
               compliance_ttt == "oui" ~ "1",
               compliance_ttt == "non" ~ "0",
               TRUE ~ NA_character_),
           odf_prechir = case_when(
               odf_prechir == "oui" ~ "1",
               odf_prechir == "non" ~ "0",
               TRUE ~ NA_character_),
           complications = case_when(
               complications == "complications" ~ "1",
               complications == "simples" ~ "0",
               TRUE ~ NA_character_)
           ) %>%
    select(-odf_prechir_years)

etude = left_join(etude,epi[c("patid","sexe")]) %>%
    relocate(sexe,.after=patid)
