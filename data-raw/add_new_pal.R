#script to add new palette to epicentre_pal
pacman::p_load(here,
               rio,
               magrittr,
               tibble,
               dplyr)

#Add a new palette
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

  export(new_pal, here::here("data-raw", "palettes.csv"))

  return(new_pal)

}

# Remove a palette
remove_pal <- function(pal_name) {

  new_pal <- rio::import(file = here("data-raw",
                                     "palettes.csv")) %>%
    tibble::tibble() |>

    filter(palette_name != pal_name)

  export(new_pal, here::here("data-raw", "palettes.csv"))

  return(new_pal)
}

# Work here -----------------------------------------------------------------

# color_vec <- c("confirmed", "probable", "suspect", "noncase")
# hex_codes_vec <- c("#9e2a2b", "#e09f3e", "#fff3b0", "#335c67")
#
# epicentre_pal <- add_new_pal(palette_name = "epicentre-epiclass",
#                              palette_type = "sequential",
#                              color_names= color_vec,
#                              hex = hex_codes_vec)

epicentre_pal <- import(here::here("data-raw", "palettes.csv"))

usethis::use_data(epicentre_pal, overwrite = TRUE)
