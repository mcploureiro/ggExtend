
geom_ribbon_step <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity", direction = "hv",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRibbonStep,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      direction = direction,
      na.rm = na.rm,
      ...
    )
  )
}

GeomRibbonStep <- ggproto("GeomRibbonStep", GeomRibbon,
                          draw_group = function(data, panel_params, coord, direction = "hv", na.rm = FALSE){
                            data <- stairstep2(data, geom = "ribbon")
                            GeomRibbon$draw_group(data, panel_params, coord)
                          })

#Define new stairstep function that takes a ymin/ymax instead as well as a hard coded "y".
stairstep2 <- function(data, direction="hv", geom = "line") {
  direction <- match.arg(direction, c("hv", "vh"))
  geom <- match.arg(geom, c("line", "ribbon"))
  data <- as.data.frame(data)[order(data$x), ]
  n <- nrow(data)
  
  if (n <= 1) {
    # Need at least one observation
    return(data[0, , drop = FALSE])
  }
  
  if (direction == "vh") {
    xs <- rep(1:n, each = 2)[-2*n]
    ys <- c(1, rep(2:n, each = 2))
  } else {
    ys <- rep(1:n, each = 2)[-2*n]
    xs <- c(1, rep(2:n, each = 2))
  }
  
  if(geom == "line"){
    data.frame(
      x = data$x[xs],
      y = data$y[ys],
      data[xs, setdiff(names(data), c("x", "y"))]
    )
  }else if (geom == "ribbon"){
    data.frame(
      x    = data$x[xs],
      ymin = data$ymin[ys],
      ymax = data$ymax[ys],
      data[xs, setdiff(names(data), c("x","ymin","ymax"))]
    )
  }
}
