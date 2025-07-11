# Munging 1st excel file

# Renaming for comfort
epi = epid.Feuil1

# removing old object
rm(epid.Feuil1)

# Removing useless cols
epi = epi %>% select(-c(1:4,9,10,11,12))

# Renaming variables
names(epi) = c("patid","sexe", "complete","bilaterale","cote")

epi = epi %>%
    mutate(cote = coalesce(cote,bilaterale)) %>%
    filter(!is.na(sexe)) %>%
    mutate(across(where(is.character), function(x) rm_accent(x)),
           complete = ifelse(complete=="complete",1,0),
           bilaterale = ifelse(bilaterale=="bilaterale",1,0),
           cote = ifelse(bilaterale==1,"bilaterale",cote),
           sexe = ifelse(sexe=="G", "M", sexe))




