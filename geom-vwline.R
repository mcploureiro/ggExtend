
geom_vwline <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      lineend = "butt",
                      linejoin = "round",
                      linemitre = 1,
                      arrow = NULL,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomVwLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}


GeomVwLine <- ggproto("GeomPath", Geom,
                    required_aes = c("x", "y", "w"),
                    
                    default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
                    
                    handle_na = function(data, params) {
                      keep <- function(x) {
                        # from first non-missing to last non-missing
                        first <- match(FALSE, x, nomatch = 1) - 1
                        last <- length(x) - match(FALSE, rev(x), nomatch = 1) + 1
                        c(
                          rep(FALSE, first),
                          rep(TRUE, last - first),
                          rep(FALSE, length(x) - last)
                        )
                      }
                      # Drop missing values at the start or end of a line - can't drop in the
                      # middle since you expect those to be shown by a break in the line
                      missing <- !stats::complete.cases(data[c("x", "y", "size", "colour",
                                                               "linetype")])
                      kept <- stats::ave(missing, data$group, FUN = keep)
                      data <- data[kept, ]
                      
                      if (!all(kept) && !params$na.rm) {
                        warning("Removed ", sum(!kept), " rows containing missing values",
                                " (geom_path).", call. = FALSE)
                      }
                      
                      data
                    },
                    
                    draw_group = function(data, panel_params, coord, arrow = NULL,
                                          lineend = "butt", linejoin = "round", linemitre = 1,
                                          na.rm = FALSE) {
                      if (!anyDuplicated(data$group)) {
                        message_wrap("geom_path: Each group consists of only one observation. ",
                                     "Do you need to adjust the group aesthetic?")
                      }
                      
                      # must be sorted on group
                      data <- data[order(data$group), , drop = FALSE]
                      munched <- coord_munch(coord, data, panel_params)
                      
                      #MH ADDED
                      rescale_y <- function(data) rescale(data, from = panel_params$y.range)
                      munched$w <- squish_infinite(rescale_y(munched$w))
                      
                      # Silently drop lines with less than two points, preserving order
                      rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
                      munched <- munched[rows >= 2, ]
                      if (nrow(munched) < 2) return(zeroGrob())
                      
                      # Work out whether we should use lines or segments
                      attr <- plyr::ddply(munched, "group", function(df) {
                        linetype <- unique(df$linetype)
                        data.frame(
                          solid = identical(linetype, 1) || identical(linetype, "solid"),
                          constant = nrow(unique(df[, c("alpha", "colour","size", "linetype")])) == 1
                        )
                      })
                      solid_lines <- all(attr$solid)
                      constant <- all(attr$constant)
                      if (!solid_lines && !constant) {
                        stop("geom_path: If you are using dotted or dashed lines",
                             ", colour, size and linetype must be constant over the line",
                             call. = FALSE)
                      }
                      
                      # Work out grouping variables for grobs
                      n <- nrow(munched)
                      group_diff <- munched$group[-1] != munched$group[-n]
                      start <- c(TRUE, group_diff)
                      end <-   c(group_diff, TRUE)
                      
                      ##MH Changed -no groups fornow
                      vwlineGrob(munched$x, munched$y, munched$w,
                                 default.units = "native",
                                 gp = gpar(
                                   col = alpha(munched$colour, munched$alpha),
                                   fill = alpha(munched$colour, munched$alpha),
                                   lty = munched$linetype,
                                   linedend = lineend,
                                   linejoin = linejoin,
                                   linemitre = linemitre
                                 ))
                      
                      
                      # if (!constant) {
                      #   segmentsGrob(
                      #     munched$x[!end], munched$y[!end], munched$x[!start], munched$y[!start],
                      #     default.units = "native", arrow = arrow,
                      #     gp = gpar(
                      #       col = alpha(munched$colour, munched$alpha)[!end],
                      #       fill = alpha(munched$colour, munched$alpha)[!end],
                      #       lwd = munched$size[!end] * .pt,
                      #       lty = munched$linetype[!end],
                      #       lineend = lineend,
                      #       linejoin = linejoin,
                      #       linemitre = linemitre
                      #     )
                      #   )
                      # } else {
                      #   id <- match(munched$group, unique(munched$group))
                      #   polylineGrob(
                      #     munched$x, munched$y, id = id,
                      #     default.units = "native", arrow = arrow,
                      #     gp = gpar(
                      #       col = alpha(munched$colour, munched$alpha)[start],
                      #       fill = alpha(munched$colour, munched$alpha)[start],
                      #       lwd = munched$size[start] * .pt,
                      #       lty = munched$linetype[start],
                      #       lineend = lineend,
                      #       linejoin = linejoin,
                      #       linemitre = linemitre
                      #     )
                      #   )
                      # }
                    },
                    
                    draw_key = draw_key_path
)
