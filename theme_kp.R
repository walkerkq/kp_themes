#' Add personal theme to ggplot chart
#'
#' This function allows you to add your personal theme to your ggplot graphics.
#' @keywords 
#' @export
#' @examples
#' ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#' geom_point(aes(color = Species)) +
#' theme_kp()

theme_kp <- function () {
  
  base_size <- 12
  double_base <- base_size*2
  half_base <- base_size/2
  
  ggplot2::theme(
    
    # global text formatting
    text = ggplot2::element_text(family = "Avenir"),
    
    # plot element formatting
    plot.title = ggplot2::element_text(face = "bold",
                                       size = rel(1.5),
                                       lineheight = 1.2),
    plot.subtitle = ggplot2::element_text(face = "italic",
                                          size = rel(1.2),
                                          lineheight = 1.2,
                                          color = "grey40",
                                          margin=ggplot2::margin(half_base,0,half_base,0)),
    plot.margin = ggplot2::margin(double_base, double_base, double_base, double_base),
    
    # axis formatting
    axis.text = ggplot2::element_text(size = rel(0.8), colour = "grey40"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(t = half_base)),
    axis.text.y = ggplot2::element_text(margin=ggplot2::margin(r = half_base)),
    axis.title.x = ggplot2::element_text(margin=ggplot2::margin(t = double_base)),
    axis.title.y = ggplot2::element_text(margin=ggplot2::margin(r = double_base)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    # legend formatting
    legend.position = "right",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(color="grey40"),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(color="grey40"),
    
    # panel formatting
    panel.background = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="grey80"),
    panel.grid.major.x = ggplot2::element_blank()
    
  )
}

#' Personal colors
#'
#' @param ... a color name.
#' @return A named vector of hex colors.
#' @examples
#' bl_cols("red", "blue")
kp_cols <- function(...) {
  
  kp_colors <- c(purple = "#490B32",
                 red = "#9A031E",
                 orange = "#FB8B24",
                 dark_orange = "#E36414",
                 dark_blue = "#0F4C5C",
                 grey = "#66717E",
                 light_green = "#B3CBB9",
                 blue = "#5DA9E9"
  )
  
  cols <- c(...)
  
  if (is.null(cols))
    return (kp_colors)
  
  kp_colors[cols]
}


#' Expanded color lists based on a palette
#'
#' @param palette A palette defined in bl_palettes.
#' @param reverse Whether or not to reverse the palette.
#' @return A list of hex colors.
#' @examples
#' kp_pal(palette = "main", reverse = FALSE)
kp_pal <- function(palette = "main", reverse = FALSE, ...) {
  
  kp_palettes <- list(
    "main"  = kp_cols("purple", "blue", "red", "dark_blue"),
    
    "cool"  = kp_cols("purple", "dark_blue", "light_green", "blue"),
    
    "warm"  = kp_cols("red", "orange", "dark_orange"),
    
    "mixed" = kp_cols("purple", "red", "dark_orange", "orange", "dark_blue", "blue", "light_green", "grey")

  )
  
  pal <- kp_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


#' Personal colour scales
#'
#' @param palette A palette defined in kp_palettes.
#' @param reverse Whether or not to reverse the palette.
#' @param discrete Whether the scale is discrete or not.
#' @examples
#' ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'   geom_point(aes(color = Species)) +
#'   theme_kp() +
#'   scale_color_kp(palette = "cool")
scale_color_kp <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- kp_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("kp_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Personal fill scales
#'
#' @param palette A palette defined in kp_palettes.
#' @param reverse Whether or not to reverse the palette.
#' @param discrete Whether the scale is discrete or not.
#' @examples
#' cars <- aggregate(mpg ~ cyl + vs, mtcars, mean)
#' ggplot(cars, aes(cyl, mpg)) +
#'     geom_bar(stat="identity", position= "dodge", aes(fill = as.factor(vs))) +
#'     theme_kp() +
#'     scale_fill_kp(palette = "main")
scale_fill_kp <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- kp_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("kp_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}
