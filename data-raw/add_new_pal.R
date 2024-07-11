#script to add new palette to epicentre_pal
pacman::p_load(here,
               rio,
               magrittr,
               tibble,
               dplyr)

#function to add a new palette

add_new_pal <- function(palette_name, palette_type, color_names, hex_codes){

  if(length(color_names) != length(hex_codes)) {stop("Color names and Hex codes are not of the same length")}

  old_pal <- rio::import(file = here("data-raw",
                                     "palettes.csv")) %>%
    tibble::tibble()

  new_pal <- data.frame(palette = palette_name,
                        palette_type = palette_type,
                        color_name = color_names,
                        hex = hex_codes)

  new_pal <- bind_rows(old_pal, new_pal)

  return(new_pal)

}

color_vec <- c("epi-blue1", "epi-blue2", "epi-blue3", "epi-blue4", "epi-blue5")
hex_codes_vec <- c("#2E4473", "#687EA1", "#859BB8", "#93AAC4", "#A1B8CF")

epicentre_pal <- add_new_pal(palette_name = "epicentre-blues",
            palette_type = "sequential",
            color_names= color_vec,
            hex = hex_codes_vec)


usethis::use_data(epicentre_pal, overwrite = TRUE)
