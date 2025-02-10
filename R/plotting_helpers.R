# Overlaying a table on a plot

# The defaults tthemes from ggmisc for geom_table()
# https://docs.r4photobiology.info/ggpp/reference/ttheme_gtdefault.html
# How to construct your own aesthetics for geom_table
# https://stackoverflow.com/questions/60274537/change-table-theme-when-using-ggpmiscgeom-table-npc
ttheme_custom <- function(base_size = 12,
                          core = list(bg_params = list(
                            fill = alpha("white", 0.5))),
                          base_colour = "black",
                          alpha=0.5,
                          base_family = "",
                          parse = FALSE,
                          padding = unit(c(1, 1), "mm"),
                          ...) {

  gridExtra::ttheme_default(base_size = base_size,
                            base_colour = base_colour,
                            base_family = base_family,
                            parse = parse,
                            padding = padding,
                            core = list(
                              bg_params = list(
                                fill = alpha("white", 0.65))), ...)
}
