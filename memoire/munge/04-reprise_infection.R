reprise_by_acte.pmsiGENERAL = reprise_by_acte.pmsiGENERAL %>% fix_colnames()
reprise_by_cim10.pmsiGENERAL = reprise_by_cim10.pmsiGENERAL %>% fix_colnames()
reprise_by_doc.doceds.doc.infection.3 = reprise_by_doc.doceds.doc.infection.3 %>% fix_colnames()


reprise = full_join(reprise_by_acte.pmsiGENERAL,reprise_by_cim10.pmsiGENERAL) %>%
    full_join(reprise_by_doc.doceds.doc.infection.3) %>%
    mutate(datent = date(datent),
           recdate = date(recdate),
           datent = coalesce(datent,recdate)
           ) %>%
    select(-recdate) %>%
    distinct() %>%
    rename(date_reprise = datent)


rm(reprise_by_acte.pmsiACTES)
rm(reprise_by_acte.pmsiDIAG)
rm(reprise_by_acte.pmsiGENERAL)
rm(reprise_by_cim10.pmsiACTES)
rm(reprise_by_cim10.pmsiDIAG)
rm(reprise_by_cim10.pmsiGENERAL)
rm(reprise_by_doc.doceds.doc.infection.3)
