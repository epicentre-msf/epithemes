# Add logo to epitheme_gg

# library(tidyverse)
# species <- starwars |>
#   count(species) |>
#   filter(!is.na(species) & n > 1) |>
#   arrange(-n) |>
#   mutate(species = factor(species, species))
#
# l <- get_logo("epicentre.png",
#              x = unit(1, "npc") - unit(3 / 2, "lines"),
#              y = unit(-3, "lines"),
#              width = unit(10, "lines"))
#
# ggplot(data = iris) +
#    geom_histogram(aes(x = Sepal.Length)) +
#
#   coord_cartesian(clip = "off") +
#   epitheme_gg() +
#   theme(plot.margin = unit(c(1, 1, 3, 1), "lines"))+
#   annotation_custom(l)
