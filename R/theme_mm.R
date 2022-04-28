#' MountainMath theme
#'
#' @param background_colour Background colour for plot and panel
#' @param ... Additional parameters passed to theme()
#' @return A theme to be added to a ggplot object
#' @export
theme_mm <- function(background_colour = "#F8F4F0",...){
  ggplot2::theme_light() +
    ggplot2::theme(plot.title.position = "plot",
                   plot.caption.position = "plot",
                   plot.title = ggplot2::telement_text(hjust=0.5),
                   plot.subtitle = ggplot2::telement_text(hjust=0.5),
                   text = ggplot2::element_text(family = "Times New Roman"),
                   plot.caption = ggplot2::element_text(hjust=1),
                   plot.background = ggplot2::element_rect(fill=background_colour),  #F8F0F4
                   panel.background = ggplot2::element_rect(fill=background_colour),
                   legend.background = ggplot2::element_rect(fill=background_colour),
                   #legend.box.background = element_rect(fill=background_colour),
                   legend.key = ggplot2::element_rect(fill=background_colour),
                   panel.grid.major = ggplot2::element_line(colour="grey")) +
    ggplot2::theme(...)
}

#' MountainMath logo
#'
#' @return A theme to be added to a ggplot object
#' @export
add_mm_logo <- function(){
  logo <- png::readPNG(system.file("img", "logo.png", package="mountainmathHelpers"))
  footer <- grid::rasterGrob(logo,  width = grid::unit(1,"npc"),height = grid::unit(1,"npc"))

  patchwork::inset_element(footer,
                           left= 0,
                           bottom=-grid::unit(11, 'points'),
                           right=grid::unit(16, 'points'),
                           top= 5,
                           align_to = "plot",
                           clip = FALSE,
                           on_top = FALSE)
}
