radio = radio.Feuil1 %>% tibble

rm(radio.Feuil1)

radio = radio %>% fix_colnames()
radio = radio %>%
    mutate(mean_vol_fente = (vol1_fente + vol2_fente)/2,
           mean_vol_greffe = (vol1_greffe + vol2_greffe)/2,
           comblement = mean_vol_greffe*100/mean_vol_fente) %>%
    rename("lrdl_1" = lrdl_1___19,
           "lrdl_2" = lrdl_1___20)

radio = radio %>%
    mutate(lrdm = (lrdm_1+lrdm_2)/2,
           lrdl = (lrdl_1 + lrdl_2)/2,
           hcrdm = (hcrdm_1 + hcrdm_2)/2,
           hcrdl = (hcrdl_1 + hcrdl_2)/2,
           coverture_mesiale_preop = hcrdm*100/lrdm,
           coverture_laterale_preop = hcrdl*100/lrdl)

radio = radio %>% rename("coverture_dm_postop" = cor_post_op_dm,
                 "coverture_dl_postop" = cordl)

radio = radio %>% mutate(gain_coverture_dm = as.numeric(radio$coverture_dm_postop)-radio$coverture_mesiale_preop,
                 gain_coverture_dl = as.numeric(radio$coverture_dl_postop)-radio$coverture_laterale_preop)
