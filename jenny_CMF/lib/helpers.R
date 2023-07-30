convert_to_months <- function(age_data) {
  age_in_months <- sapply(strsplit(as.character(age_data), "\\.|\\,"), function(x) {
    years <- as.numeric(x[1])
    months <- as.numeric(x[2])
    total_months <- years * 12 + months
    return(total_months)
  })
  return(age_in_months)
}

table_prop <- function(x,useNA = "ifany", name = "var") {
    tbl <- table(x, useNA = useNA)
    prop_tbl <- prop.table(tbl)
    result <- data.frame(
        as.character(names(tbl)),
        n = as.vector(tbl),
        prop = paste(round(as.vector(prop_tbl)*100, getOption("digits")), "%")
    )
    colnames(result) <- c(name, "n", "prop")
    return(result)
}



