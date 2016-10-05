## Graafikul autode värv aastate lõikes
library(dplyr)
library(ggplot2)
library(stringr)
library(ggthemes)
library(extrafont)
library(forcats)

load("output/autode_andmed.RData")

# töötle autode värvi andmed
autode_andmed_graafikuks <- autode_andmed %>%
    # viimase 20 aasta registreeritud autod 
    filter(aasta >= 1995, !is.na(varv), varv != "") %>%
    # töötle värvi nii, et kustutada ära eesliide (dark, light jne) 
    # ja nimeta paar värvi ümber
    mutate(aasta = as.numeric(aasta),
           varv = word(varv, -1),
           varv = ifelse(varv == "golden", "yellow", 
                         ifelse(varv == "silver", "gray", varv))) %>%
    filter(varv != "emeral")

# värvide esinemissagedud, et selle järgi määrata graafikule värvi HEX koodid
varvide_sagedus <- autode_andmed_graafikuks %>%
    mutate(varv = fct_rev(fct_infreq(varv))) %>%
    .$varv %>%
    summary()

# värvide HEX koodid autode arvu kahanevas järjekorras graafikul kujutamiseks
varvid_fill <- c("#c51b8a", "#d95f0e", "#feb24c", "#fff7bc", "#a63603", "#31a354", "#de2d26",
                 "white", "#045a8d", "#252525", "#969696")

# joonista graafik
autode_andmed_graafikuks %>%
    ggplot(aes(x = aasta, y = ..count.., fill = fct_rev(fct_infreq(varv)), 
               colour = fct_rev(fct_infreq(varv)))) +
    geom_density(position = "fill") +
    scale_fill_manual(values = varvid_fill) +
    scale_colour_manual(values = varvid_fill) +
    # kaota graafiku ümbere jääv tühi ruum ära (expand funktsioon) 
    scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(title = "Eesti populaarsemad autovärvid viimase 20 a jooksul",
         subtitle = "auto24.ee lehel müügil olevate kasutatud autode värv registreerimise aasta lõikes (andmed seisuga okt 2016)") +
    theme_tufte() +
    theme(axis.text = element_text(size = 9),
          text = element_text(family = "Chivo"),
          plot.title = element_text(size = 12, face = "bold"),
          axis.title = element_blank(),
          legend.position = "none")

# salvesta png fail
ggsave(filename = "output/autovarvid.png")