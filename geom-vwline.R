library(devtools)
library(vwline)
library(ggplot2)
library(grid)

# 
# ggplot(data.frame(x=c(1,2,3,3,4), y=c(1,2,7,3,4), w= c(1,2,0.5,4,5), g= c(1,1,1,2,2))) + geom_vwline(aes(x=x,y=y, w=w, group = g), stepWidth = TRUE)
# 
# ggplot(data.frame(x=c(1,2,3,3,4), y=c(1,2,7,3,4), w= c(1,2,0.5,4,5))) + geom_vwline(aes(x=x,y=y, w=w), stepWidth = TRUE)

# ggplot(data.frame(x=c(3,4), y=c(3,4), w= c(4,5))) + geom_vwline(aes(x=x,y=y, w=w), stepWidth = TRUE) + geom_line(aes(x=x,y=y), color = "red")
# 
# ggplot(data.frame(x=c(1,2), y=c(1,1.1), w= c(0.5,1))) + geom_vwline(aes(x=x,y=y, w=w), stepWidth = TRUE)
# ggplot(data.frame(x=c(1,2,3), y=c(1,1,1))) + geom_point(aes(x=x,y=y))

#### MH changed the sacle_apply function
scale_apply <- function(data, vars, method, scale_id, scales) {
  if (length(vars) == 0) return()
  if (nrow(data) == 0) return()
  
  n <- length(scales)
  if (any(is.na(scale_id))) stop()
  
  scale_index <- plyr::split_indices(scale_id, n)

  lapply(vars, function(var) {
    pieces <- lapply(seq_along(scales), function(i) {
      #MH Added - special case where we have a width...in future will need to adapt this so that its not just 1/2. Allow for multiple width specifications. Lkely do this for left/right variables??
        ### MH not quite right, this makes the x-limits too long
        #Calculate angle from one data point to the next. For y, take sin(that angle) and multiply by width at that point (hypotenuse). For x, take cos(that angle)
        y_ang <- cos(atan(diff(data[["y"]])/diff(data[["x"]])))
        x_ang <- sin(atan(diff(data[["y"]])/diff(data[["x"]])))
        
        #Each data point will have 2 angles (except first and last). Rep angles so we can calculate properly
        y_ang <- rep(y_ang, each = 2)
        x_ang <- rep(x_ang, each = 2)
        
        #Indexs for each data point. We want 1st & last to occur 1 time, all the rest to occur twice
        idx <- rep(seq_len(nrow(data)), times = c(1,rep(2, nrow(data)-2),1))
        
        pass_vals <- if(method == "train" & var == "y" & "w" %in% names(data)) {c(data[[var]][scale_index[[i]]][idx] - (0.5*data[["w"]][scale_index[[i]]][idx])*y_ang,
                                                                                  data[[var]][scale_index[[i]]][idx] + (0.5*data[["w"]][scale_index[[i]]][idx])*y_ang)}
                     else if(method == "train" & var == "x" & "w" %in% names(data)){c(data[[var]][scale_index[[i]]][idx] - (0.5*data[["w"]][scale_index[[i]]][idx])*x_ang,
                                                                                      data[[var]][scale_index[[i]]][idx] + (0.5*data[["w"]][scale_index[[i]]][idx])*x_ang)}
                     else{data[[var]][scale_index[[i]]]}
        
        scales[[i]][[method]](pass_vals)
    })
    # Join pieces back together, if necessary
    if (!is.null(pieces)) {
      unlist(pieces)[order(unlist(scale_index))]
    }
  })
}
assignInNamespace("scale_apply", scale_apply, "ggplot2")



geom_vwline <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        lineend = "butt",
                        linejoin = "round",
                        linemitre = 1,
                        stepWidth = FALSE,
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
                        #MH ADDED - simply reset the "w" column to start at 0 by adding in the panel_params$y.range[1] (min of y-range)
                        # All need to do now is get scales & ranges to calculate correctly on their own
                        rescale_y <- function(data) scales::rescale(data, from = panel_params$y.range)
                        munched$w <- scales::squish_infinite(rescale_y(panel_params$y.range[1] + data$w))
                        
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
                                   default.units = "native", stepWidth = FALSE,
                                  gp = gpar(
                                    col = alpha(munched$colour, munched$alpha),
                                    fill = alpha(munched$colour, munched$alpha),
                                    lty = munched$linetype,
                                    linedend = lineend,
                                    linejoin = linejoin,
                                    linemitre = linemitre
                                  ))
                                  
                        
                      },
                      
                      draw_key = draw_key_path
)
