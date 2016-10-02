library(dplyr)
library(ggplot2)
library(stringr)

load("output/autode_andmed.RData")

autode_andmed_graafikuks <- autode_andmed %>%
    filter(aasta >= 1996, !is.na(varv), varv != "") %>%
    mutate(aasta = as.numeric(aasta),
           varv = word(varv, -1))

autode_andmed_graafikuks %>%
    ggplot(aes(x = aasta, y = ..count.., fill = varv)) +
    geom_density(position = "fill")