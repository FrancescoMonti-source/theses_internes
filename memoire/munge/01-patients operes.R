# Renaming columns
pat_operes_actes = pat_operes.pmsiACTES %>% fix_colnames()
rm(pat_operes.pmsiACTES)

pat_operes_diag = pat_operes.pmsiDIAG %>% fix_colnames()
rm(pat_operes.pmsiDIAG)

pat_operes_general = pat_operes.pmsiGENERAL %>% fix_colnames()
rm(pat_operes.pmsiGENERAL)

pat_operes_general = pat_operes_general %>%
    group_by(patid,evtid) %>%
    summarise(datent = min(datent),
              datsort = max(datsort))


# Defining the list of spinal procedures
spinal_actes = c("LHMH001","LHMH002","LHMH003","LHMH004","LHMH005","LHMH006","LDCA002",
                 "LDCA003","LDCA005","LDCA006","LDCA009","LDCA010","LFCA001","LFCA002",
                 "LFDA004","LFDA012","LGCA001","LHCA002","LHCA010","LHCA011","LHCA016",
                 "LHDA001","LHDA002","LDCA001","LDCA004","LDCA007","LDCA008","LDCA011",
                 "LDCA013","LDDA001","LECA001","LECA003","LECA005","LECA006","LECC001",
                 "LFCA004","LFCA005","LFCC001","LDCA012","LECA002","LECA004","LFCA003",
                 "LHCA001","LFDA001","LFDA002","LFDA003","LFDA005","LFDA006","LFDA007",
                 "LFDA008","LFDA009","LFDA010","LFDA011","LFDA013","LFDA014","LEMA001",
                 "LEMA002","LEMA003","LEMA004","LFMA001","LHMA003","LHMA004","LHMA006",
                 "LHMA011","LHMA013","LHMA014","LHMA015","LDPA008","LDPA009","LDPA010",
                 "LEPA001","LEPA002","LEPA003","LEPA004","LEPA005","LEPA006","LEPA007",
                 "LEPA008","LEPA009","LFPA001","LFPA002","LFPA003","LHFA001","LHFA003",
                 "LHFA013","LHFA025","LHFA027","LHFA028","LHFA029","LDPA001","LDPA002",
                 "LDPA003","LDPA004","LDPA005","LHMA007","LHMA016","LHPA003","LHPA006",
                 "LHPA010","LHFA016","LHFA019","LHFA024","LDFA003","LDFA004","LDFA005",
                 "LFFA001","LFFA005","LFFA006","LDAA001","LDAA002","LFAA001","LFAA002",
                 "LDFA002","LDPA006","LDPA007","LDFA009","LDFA012","LEFA004","LEFA006",
                 "LEFA007","LEFA008","LEFA010","LEFA012","LEFA014","LFFA008","LFFA009",
                 "LFFA013","LFFA014","LHFA031","LDFA010","LEFA001","LEFA005","LEFA009",
                 "LFFA012","LGFA001","LGFA002","LGFA003","LGFA004","LGFA005","LGFA006",
                 "LDGA001","LDGA002","LEGA001","LEGA002","LFGA001","LHGA004","LHGA006",
                 "LHGA007","ENNH002","LHFH001","LHMA008")

# Merging data frames
pat_operes = left_join(pat_operes_general, pat_operes_actes, by = c("patid","evtid")) %>%
    filter(codeacte %in% spinal_actes) %>%
    left_join(pat_operes_diag, by = c("patid","evtid")) %>%
    distinct %>%
    tibble

# Removing nomore useful datasets
rm(pat_operes_actes)
rm(pat_operes_diag)
rm(pat_operes_general)

