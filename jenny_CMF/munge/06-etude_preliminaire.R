prelim = etude_prelim.Feuil1 %>%
    tibble %>%
    fix_colnames() %>%
    rename(id = nb_de_fentes,
           complication_repo = complications_liees_a_la_repose)

rm(etude_prelim.Feuil1)


prelim = prelim %>%
    mutate(mean_vol_fente = (vol1_fente + vol2_fente)/2,
           mean_vol_greffe = (vol1_greffe + vol2_greffe)/2,
           comblement = mean_vol_greffe*100/mean_vol_fente)
