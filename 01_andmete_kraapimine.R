## auto24.ee lehel müügis olevate kasutatud autode värvus reg aasta lõikes
## tasub kraapida andmed kohe inglise keels, et värvid oleksid ka ing keeles

library(rvest)
library(purrr)
library(dplyr)
library(stringr)

# kuulutuste avalehe url
kuulutuste_url <- "http://eng.auto24.ee/kasutatud/nimekiri.php?bn=2&a=101102&ae=2&af=200&ag=0&otsi=otsi&ak=0"

# kraabi kuulutuste arv, et selle järgi lingid genereerida
kuulutuste_arv <- read_html(kuulutuste_url) %>%
    html_nodes("#abovesearchImgContainer+ .paginator strong") %>%
    html_text() %>%
    as.numeric()

# genereeri kuulutuste lingid (200 kuulutust per lehekülg)
kuulutuste_lingid <- str_c("http://eng.auto24.ee/kasutatud/nimekiri.php?bn=2&a=101102&ae=2&af=200&ag=0&otsi=otsi&ak=",
                           seq(from = 0, to = kuulutuste_arv, by = 200))

# funktsioon, mis kraabib automüügi kuulutustest automargi, reg aasta ja värvuse
kraabi_autode_andmed <- function(x){
    tryCatch(
        {
            lehekylg <- read_html(x)
            
            automark_odd <- lehekylg %>%
                html_nodes(".item-odd a") %>%
                html_text()
            
            automark_even <- lehekylg %>%
                html_nodes(".item-even a") %>%
                html_text()
            
            aasta_odd <- lehekylg %>%
                html_nodes(".item-odd .year") %>%
                html_text()
            
            aasta_even <- lehekylg %>%
                html_nodes(".item-even .year") %>%
                html_text()
            
            varv_odd <- lehekylg %>%
                html_nodes(".item-odd .color") %>%
                html_text()
            
            varv_even <- lehekylg %>%
                html_nodes(".item-even .color") %>%
                html_text()
            
            odd <- data_frame(automark_odd, aasta_odd, varv_odd)
            names(odd) <- c("automark", "aasta", "varv")
            
            even <- data_frame(automark_even, aasta_even, varv_even)
            names(even) <- c("automark", "aasta", "varv")
            
            kokku <- bind_rows(odd, even)
            
            Sys.sleep(sample(1:5,1))
            
            return(kokku)
        }, error = function(e) NULL
    )
}

# kraabi kuulutuste andmed kõigilt lehekülgedelt
autode_andmed <- map_df(kuulutuste_lingid, kraabi_autode_andmed)

save(autode_andmed, file = "output/autode_andmed.RData")