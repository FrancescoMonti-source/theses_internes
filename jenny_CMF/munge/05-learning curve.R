# Munging learning curve excel file

    # Renaming for comfort
lc = learning_curve.Feuil1 %>% tibble()
    # removing old object
rm(learning_curve.Feuil1)

    # Converting surgery duration to minutes
lc$time = hms::as_hms(lc$time) %>%
    as.numeric() %>%
    divide_by(60)

    # Ordering by intervention date and assigning an ID to each surgery based on that order
lc = lc %>%
    arrange(date) %>%
    mutate(id = 1:nrow(.)) %>%
    relocate(id, .before = "date")

lc$interne = ifelse(is.na(lc$interne),0,1)
lc$double = ifelse(is.na(lc$double),0,1)
