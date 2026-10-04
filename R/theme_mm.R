#' MountainMath theme
#'
#' @param ... Additional parameters passed to theme()
#' @param background_colour Background colour for plot and panel
#' @return A theme to be added to a ggplot object
#' @export
theme_mm <- function(...,background_colour = "#F8F4F0"){
  ggplot2::theme_light() +
    ggplot2::theme(plot.title.position = "plot",
                   plot.caption.position = "plot",
                   plot.title = ggplot2::element_text(hjust=0.5),
                   plot.subtitle = ggplot2::element_text(hjust=0.5),
                   text = ggplot2::element_text(family = "Times New Roman"),
                   plot.caption = ggplot2::element_text(hjust=1),
                   plot.background = ggplot2::element_rect(fill=background_colour),  #F8F0F4
                   panel.background = ggplot2::element_rect(fill=background_colour),
                   legend.background = ggplot2::element_rect(fill=background_colour),
                   #legend.box.background = ggplot2::element_rect(fill=background_colour),
                   legend.key = ggplot2::element_rect(fill=background_colour),
                   strip.background=ggplot2::element_rect(fill="#606060"),
                   panel.grid.major = ggplot2::element_line(colour="grey")) +
    ggplot2::theme(...)
}

#' MountainMath logo
#'
#' @description
#' Adds the MountainMath logo to the bottom left corner of the graph. The logo is drawn into the caption row
#' that spans the bottom of the graph below axis titles and legends, with the caption text staying right aligned.
#' Graphs without a caption get a caption row just high enough to hold the logo.
#' This sets the `plot.caption` theme element, so it needs to be added after `theme_mm()` and after
#' any other theme adjustments to the caption.
#'
#' @param size height of the logo in points
#' @return A theme to be added to a ggplot object
#' @export
add_mm_logo <- function(size=14){
  if (!requireNamespace("png",quietly=TRUE)) {
    stop("The png package is required to add the logo.")
  }
  logo <- png::readPNG(system.file("img", "logo.png", package="mountainmathHelpers"))
  # trim transparent padding around the logo so that it sits right in the corner
  visible <- logo[,,4]>0
  logo <- logo[range(which(rowSums(visible)>0)) %>% (function(r)seq(r[1],r[2])),
               range(which(colSums(visible)>0)) %>% (function(r)seq(r[1],r[2])),]
  ggplot2::theme(plot.caption = element_mm_logo(logo,size),
                 plot.caption.position = "plot")
}

# caption text element that also draws the logo, unset text properties get inherited from the theme
element_mm_logo <- function(logo,size){
  structure(list(family=NULL,face=NULL,colour=NULL,size=NULL,hjust=NULL,vjust=NULL,angle=NULL,
                 lineheight=NULL,margin=NULL,debug=NULL,inherit.blank=FALSE,
                 logo=logo,logo_size=size),
            class=c("element_mm_logo","element_text","element"))
}

#' @exportS3Method ggplot2::element_grob
element_grob.element_mm_logo <- function(element,label=NULL,...){
  text_element <- do.call(ggplot2::element_text,
                          element[intersect(names(element),names(formals(ggplot2::element_text)))])
  caption <- ggplot2::element_grob(text_element,label=label,...)
  size <- grid::unit(element$logo_size,"pt")
  logo <- grid::rasterGrob(element$logo,x=grid::unit(0,"npc"),y=grid::unit(0,"npc"),
                           height=size,just=c("left","bottom"))
  grid::gTree(children=grid::gList(caption,logo),
              height=grid::unit.pmax(grid::grobHeight(caption),size),
              cl="mm_logo_caption")
}

# the caption row needs to be high enough for both caption text and logo
#' @exportS3Method grid::heightDetails
heightDetails.mm_logo_caption <- function(x){
  x$height
}
