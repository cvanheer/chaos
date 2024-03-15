# palettes
#' Title
#'
#' @return cvh_palette
#' @export
#' @importFrom palettes pal_palette
#' @examples chaos_palette()
#'
#' # pal_color(chaos_palette$bright_orange)
# as_tibble(pal_colour(c("#dd5129", "#0f7ba2", "#43b284", "#fab255")))
chaos_palette <- function(){

  cvh_palette <- palettes::pal_palette(bright_orange = "#FFAC1C",
                                       teracotta = "#E3735E",
                                       coral_pink = "#F88379",
                                       vermillion = "#E34234",
                                       cadmium_red = "#D22B2B",
                                       muted_red = "#CA3433",
                                       magenta = "#ff00d5",
                                       dark_teal = "#006666",
                                       light_teal = "#b2d8d8",
                                       jade_green = "#00A36C",
                                       seafoam_green = "#9FE2BF",
                                       cornflour_blue = "#6495ED",
                                       periwinkle = "#9A95E2",
                                       blue_grey = "#7393B3",
                                       pewter_grey = "#899499",
                                       smoke_grey = "#848884")
  return(cvh_palette)

}
