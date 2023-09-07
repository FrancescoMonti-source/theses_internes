# Fixing column names
implants_actes = implants.dmACTES %>% fix_colnames() %>% tibble
rm(implants.dmACTES)

implants_dms = implants.dmDMS %>% fix_colnames() %>% tibble
rm(implants.dmDMS)

# Filtering on relevant procedures
implants_actes = implants_actes %>% filter(code %in% spinal_actes)

# Filtering on relevant implants
implants_dms = implants_dms %>%
    mutate(libelle_lpp = tolower(rm_accent(libelle_lpp))) %>%
    filter(str_detect(libelle_lpp,"rachis"),
           str_detect(libelle_lpp,"plaque|tige|vis|implant|cage|crochet|coussinet|verrouillage|prothese"))


# Merging data frames
implants = left_join(implants_actes,implants_dms[c("patid","evtid","libelle_lpp")]) %>%
    distinct %>%
    rename(implant_label = libelle_lpp)
#
rm(implants.dm)
rm(implants_actes)
rm(implants_dms)


# Creating binary variable implant yes/no
pat_operes$implant = ifelse(pat_operes$evtid %in% implants$evtid,1,0)


