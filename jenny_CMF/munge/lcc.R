lcc = lcc.Feuil1

rm(lcc.Feuil1)

lcc = lcc %>%
    mutate(diff_preop = lcc_max_preop - lcc_mand_preop,
           diff_postop = lcc_max_postop - lcc_mand_postop)




