# Renaming and merging
infections = left_join(infections.comb.infection.episodes.6.ACTES,
                       infections.comb.infection.episodes.6.DALL) %>%
    tibble %>%
    fix_colnames()

infections = full_join(infections.comb.infection.episodes.6,infections.comb.infection.episodes.6.ACTES) %>%
    full_join(infections.comb.infection.episodes.6.CODEA) %>%
    full_join(infections.comb.infection.episodes.6.DALL) %>% tibble %>% fix_colnames

# Removing non useful datasets
# rm(infections.comb.infection.episodes.6)
# rm(infections.comb.infection.episodes.6.ACTES)
# rm(infections.comb.infection.episodes.6.CODEA)
# rm(infections.comb.infection.episodes.6.DALL)

infections = infections %>%
    select(-codeactes,-dm,-actes) %>%
    distinct

infections = infections %>%
    mutate(dall = str_remove_all(dall,"DP'|DS'|DR'"),
           dall = str_remove_all(dall, "[:alpha:].{1,5}\\:")) %>%
    rename(date_reprise = datent)

# Restricting date to the relevant period of follow-up
infections = infections %>% filter(between(date_reprise,"2019-12-01","2021-01-31"))


pat_operes$infection = ifelse(pat_operes$patid %in% infections$patid,1,0)
pat_operes$infection_same_sejour = ifelse(pat_operes$evtid %in% infections$evtid,1,0)

