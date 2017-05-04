library(ggplot2)

#Added Layout_matrix, order, and align
facet_matrix <- function(facets, nrow = NULL, ncol = NULL, scales = "fixed", layout_matrix = NULL, order = NULL, align = NULL,
                       shrink = TRUE, labeller = "label_value", as.table = TRUE,
                       switch = NULL, drop = TRUE, dir = "h", strip.position = 'top') {
  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  dir <- match.arg(dir, c("h", "v"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )
  if (!is.null(switch)) {
    .Deprecated("strip.position", old = "switch")
    strip.position <- if (switch == "x") "bottom" else "left"
  }
  strip.position <- match.arg(strip.position, c("top", "bottom", "left", "right"))
  if (identical(dir, "v")) {
    # swap
    nrow_swap <- ncol
    ncol_swap <- nrow
    nrow <- sanitise_dim(nrow_swap)
    ncol <- sanitise_dim(ncol_swap)
  } else {
    nrow <- sanitise_dim(nrow)
    ncol <- sanitise_dim(ncol)
  }
  
  # Check for deprecated labellers
  labeller <- check_labeller(labeller)
  
  ggproto(NULL, FacetMatrix,
          shrink = shrink,
          params = list(facets = plyr:::as.quoted(facets), free = free, layout_matrix = layout_matrix, order = order, align = align,
                        as.table = as.table, strip.position = strip.position,
                        drop = drop, ncol = ncol, nrow = nrow,
                        labeller = labeller,
                        dir = dir)
  )
}


#Create FacetMatrix as an extension of FacetWrap
FacetMatrix <- ggproto("FacetWrap", ggplot2::Facet,
                     shrink = TRUE,
                     
                     compute_layout = function(data, params) {

                       vars <- as.quoted(params$facets)
                       if (length(vars) == 0) return(layout_null())
                       
                       #Basically just getting facet values variable name is the facet variable
                       base <- plyr::unrowname(
                         combine_vars(data, params$plot_env, vars, drop = params$drop)
                       )
                       
                       
                       ##Add on NULL values to base. Counting 0s in layout matrix
                       order<-params$order
                       #If user specifies order, then map order onto the base and use that as ID
                       if (!is.null(order)){
                         order$id = 1:nrow(order)
                         ordered_base <- suppressMessages(plyr::join(base,order))
                         id <- structure(ordered_base$id, n = length(ordered_base$id))
                       } else {
                         #Assigning an ID to each facet variable value. Assigns the lowest value ID of 1, etc. No matter where it is
                         #If expanding the grid in this step, we need to figure out a way to map multiple values NA. So that 2 NA's dont get same ID
                         id <- plyr::id(base, drop = TRUE)
                         #IMPORTANT NOTE - Make is so for now, need order to be same length as id
                       }
                       #Getting total N of IDs we have
                       n <- attr(id, "n")
                       
                       #~~~~~~~~~~~~~~~~~~~~~~#
                       ###MH NEED TO REWRITE  #
                       #~~~~~~~~~~~~~~~~~~~~~~#
                       ##Setting up layout
                       if(!is.null(params$align) &
                          !is.null(params$layout_matrix)){
                         stop("Layout matrix and alignment are both specified. Please select one")
                       }else if (is.null(params$layout_matrix)) {
                           n_tot <- max(n)
                           cr <- grDevices::n2mfrow(n_tot)
                           n_pos <- cr[1] * cr[2]
                           #Default layout is top
                           layout_matrix <- matrix(c(rep(1,n_tot),rep(0,n_pos - n_tot)), nrow=cr[2], ncol=cr[1])
                           if(!is.null(params$align)){
                           if(params$align=="bottom"){
                              layout_matrix <- layout_matrix[nrow(layout_matrix):1,]
                           }
                           }
                       } else if(!is.null(params$layout_matrix)) layout_matrix = params$layout_matrix
                       
                       #Melting the layout matrix
                       matrix <- reshape2::melt(layout_matrix,varnames=c("ROW","COL"))
                       matrix <- matrix[order(matrix$ROW,matrix$COL),]
                       
                       
                       #Dynamically assigning the number of ROW/COLS that our final layout will have.
                       #dims <- wrap_dims(n, params$nrow, params$ncol)
                       #This is an easy re-write, user already specifies dimesnisons
                       dims <- dim(layout_matrix)
                       #Factorizing ID variable into PANEL variable
                       layout <- data.frame(PANEL = factor(id, levels = seq_len(n)))
                       
                       # For vertical direction, flip row and col
                       if (identical(params$dir, "v")) {
                         matrix[c("ROW", "COL")] <- matrix[c("COL", "ROW")]
                       }
                       
                       #Combining layout with the actual values of the facet
                       panels <- cbind(layout, plyr::unrowname(base))
                       panels <- panels[order(panels$PANEL), , drop = FALSE]
                       panels <- cbind(panels,matrix[which(matrix$value %in% 1),c("ROW","COL")])
                       rownames(panels) <- NULL

                       # Add scale identification
                       panels$SCALE_X <- if (params$free$x) seq_len(n) else 1L
                       panels$SCALE_Y <- if (params$free$y) seq_len(n) else 1L
                       
                       panels

                     },
                     map_data = function(data, layout, params) {
                       if (empty(data)) {
                         return(cbind(data, PANEL = integer(0)))
                       }
                       
                       vars <- as.quoted(params$facets)
                       #Get facet values for each row, converting to factor
                       facet_vals <- eval_facet_vars(vars, data, params$plot_env)
                       facet_vals[] <- lapply(facet_vals[], as.factor)
                       
                       missing_facets <- setdiff(names(vars), names(facet_vals))
                       if (length(missing_facets) > 0) {
                         
                         to_add <- unique(layout[missing_facets])
                         
                         data_rep <- rep.int(1:nrow(data), nrow(to_add))
                         facet_rep <- rep(1:nrow(to_add), each = nrow(data))
                         
                         data <- plyr::unrowname(data[data_rep, , drop = FALSE])
                         facet_vals <- plyr::unrowname(cbind(
                           facet_vals[data_rep, ,  drop = FALSE],
                           to_add[facet_rep, , drop = FALSE]))
                       }
                       
                       #
                       keys <- plyr::join.keys(facet_vals, layout, by = names(vars))
                       
                       #Assigning which Panel each datapoint will go in
                       data$PANEL <- layout$PANEL[match(keys$x, keys$y)]
                       #Ordering
                       data[order(data$PANEL), ]
                     },
                     draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
                       # If coord is non-cartesian and (x is free or y is free)
                       # then throw error
                       
                       if ((!inherits(coord, "CoordCartesian")) && (params$free$x || params$free$y)) {
                         stop("ggplot2 does not currently support free scales with a non-cartesian coord", call. = FALSE)
                       }
                       if (inherits(coord, "CoordFlip")) {
                         if (params$free$x) {
                           layout$SCALE_X <- seq_len(nrow(layout))
                         } else {
                           layout$SCALE_X <- 1L
                         }
                         if (params$free$y) {
                           layout$SCALE_Y <- seq_len(nrow(layout))
                         } else {
                           layout$SCALE_Y <- 1L
                         }
                       }
                       
                       #Rows/cols Determined by the layout matrix
                       nrow <- max(layout$ROW)
                       ncol <- max(layout$COL)

                       n <- nrow(layout)
                       
                       panel_order <- order(layout$ROW, layout$COL)
                       layout <- layout[panel_order, ]
                       panels <- panels[panel_order]
                       
                       panel_pos <- convertInd(layout$ROW, layout$COL, nrow)
                       
                       axes <- render_axes(ranges, ranges, coord, theme, transpose = TRUE)
                       
                       labels_df <- layout[names(params$facets)]
                       attr(labels_df, "facet") <- "wrap"
                       strips <- render_strips(
                         structure(labels_df, type = "rows"),
                         structure(labels_df, type = "cols"),
                         params$labeller, theme)
                       
                       # If user hasn't set aspect ratio, and we have fixed scales, then
                       # ask the coordinate system if it wants to specify one
                       aspect_ratio <- theme$aspect.ratio
                       if (is.null(aspect_ratio) && !params$free$x && !params$free$y) {
                         aspect_ratio <- coord$aspect(ranges[[1]])
                       }
                       
                       if (is.null(aspect_ratio)) {
                         aspect_ratio <- 1
                         respect <- FALSE
                       } else {
                         respect <- TRUE
                       }
                       
                       empty_table <- matrix(list(zeroGrob()), nrow = nrow, ncol = ncol)
                       panel_table <- empty_table
                       panel_table[panel_pos] <- panels
                       empties <- apply(panel_table, c(1,2), function(x) is.zero(x[[1]]))
                       
                       panel_table <- gtable_matrix("layout", panel_table,
                                                    widths = unit(rep(1, ncol), "null"),
                                                   heights = unit(rep(aspect_ratio, nrow), "null"), respect = respect, clip = "on", z = matrix(1, ncol = ncol, nrow = nrow))
                       
                       
                       panel_table$layout$name <- paste0('panel-', rep(seq_len(ncol), nrow), '-', rep(seq_len(nrow), each = ncol))
                       
                       panel_table <- gtable_add_col_space(panel_table,
                                                           theme$panel.spacing.x %||% theme$panel.spacing)
                       panel_table <- gtable_add_row_space(panel_table,
                                                           theme$panel.spacing.y %||% theme$panel.spacing)
                       #axes
                       #grid.draw(panel_table)
                       # Add axes. If we dont touch this code, it will add an axis to each facet.
                       #I think we want this as an option, but for now, lets try to recode so that we only
                       #Get axes on the bottom most panel
                       axis_mat_x_top <- empty_table
                       axis_mat_x_top[panel_pos] <- axes$x$top[layout$SCALE_X]
                       axis_mat_x_bottom <- empty_table
                       axis_mat_x_bottom[panel_pos] <- axes$x$bottom[layout$SCALE_X]
                       axis_mat_y_left <- empty_table
                       axis_mat_y_left[panel_pos] <- axes$y$left[layout$SCALE_Y]
                       axis_mat_y_right <- empty_table
                       axis_mat_y_right[panel_pos] <- axes$y$right[layout$SCALE_Y]

                       #This code removes the x-axis on top/bottom. Do this first then add back missing axes so we can get spacing correct 
                       if (!params$free$x) {
                         axis_mat_x_top[-1,]<- list(zeroGrob())

                         axis_mat_x_bottom[-nrow,]<- list(zeroGrob())
                      }
                       if (!params$free$y) {
                         axis_mat_y_left[, -1] <- list(zeroGrob())
                         
                         axis_mat_y_right[, -ncol] <- list(zeroGrob())
                       }
                       axis_height_top <- unit(apply(axis_mat_x_top, 1, max_height), "cm")
                       axis_height_bottom <- unit(apply(axis_mat_x_bottom, 1, max_height), "cm")
                       axis_width_left <- unit(apply(axis_mat_y_left, 2, max_width), "cm")
                       axis_width_right <- unit(apply(axis_mat_y_right, 2, max_width), "cm")
                       # Add back missing axes
                       #MH HAVE TO REWRITE THIS
                       ###IMPORTANT NOTE - When we add back these missing axes, this is what makes it so that in facet_Wrap,
                       ###if you have a row that is blank for a given column, the x-axis wont interfere with the spacing in the
                       ###other columns. See facet_wrap for a facet with 5 values.
                       #####WILL HAVE TO TACKLE THIS TO GET THE AXES WORKING PROPERLY
                       #Commenting this out for now because it was causing problems with the lower triangular layout
                       
                       #Getting the panels that should have axes. Panels that should have axes are: (1) first non-miss col in a row or 
                       #(2) first last non-miss row in a column 
                       #col_panels<-apply(min_col, 1, function(find, get) which(get[,1]==find[1] & get[,2]==find[2]),get=layout[,c("ROW","COL")])
                       #row_panels<-apply(max_row, 1, function(find, get) which(get[,1]==find[1] & get[,2]==find[2]),get=layout[,c("ROW","COL")])
                       
                       if (any(empties)) {
                         non_empty_ind <- which(!empties==T, arr.ind=T)
                         min_col <- aggregate(non_empty_ind, by = list(non_empty_ind[,"row"]), FUN = "min")[,c("row","col")]
                         max_col <- aggregate(non_empty_ind, by = list(non_empty_ind[,"row"]), FUN = "max")[,c("row","col")]
                         
                         min_row <- aggregate(non_empty_ind, by = list(non_empty_ind[,"col"]), FUN = "min")[,c("row","col")]
                         max_row <- aggregate(non_empty_ind, by = list(non_empty_ind[,"col"]), FUN = "max")[,c("row","col")]
                         #first_row <- which(apply(empties, 1, any))[1] - 1
                         #first_col <- which(apply(empties, 2, any))[1] - 1
                         #row_panels <- which(layout$ROW == first_row & layout$COL > first_col)
                         
                         #Bottom axes
                         row_panels_bottom <- apply(max_row, 1, function(find, get) which(get[,1]==find[1] & get[,2]==find[2]),get=layout[,c("ROW","COL")])
                         row_pos_bottom <- convertInd(layout$ROW[row_panels_bottom], layout$COL[row_panels_bottom], nrow)
                         row_axes_bottom <- axes$x$bottom[layout$SCALE_X[row_panels_bottom]]
                         
                         #Top axes
                         row_panels_top <- apply(min_row, 1, function(find, get) which(get[,1]==find[1] & get[,2]==find[2]),get=layout[,c("ROW","COL")])
                         row_pos_top <- convertInd(layout$ROW[row_panels_top], layout$COL[row_panels_top], nrow)
                         row_axes_top <- axes$x$top[layout$SCALE_X[row_panels_top]]
                         
                         
                         #col_panels <- which(layout$ROW > first_row & layout$COL == first_col)
                         #Left axes
                         col_panels_left <- apply(min_col, 1, function(find, get) which(get[,1]==find[1] & get[,2]==find[2]),get=layout[,c("ROW","COL")])
                         col_pos_left <- convertInd(layout$ROW[col_panels_left], layout$COL[col_panels_left], nrow)
                         col_axes_left <- axes$y$left[layout$SCALE_Y[col_panels_left]]
                         #Right axes
                         col_panels_right <- apply(max_col, 1, function(find, get) which(get[,1]==find[1] & get[,2]==find[2]),get=layout[,c("ROW","COL")])
                         col_pos_right <- convertInd(layout$ROW[col_panels_right], layout$COL[col_panels_right], nrow)
                         col_axes_right <- axes$y$right[layout$SCALE_Y[col_panels_right]]
                         
                         if (params$strip.position == "bottom" &&
                             theme$strip.placement != "inside" &&
                             any(!vapply(row_axes, is.zero, logical(length(row_axes))))) {
                           warning("Suppressing axis rendering when strip.position = 'bottom' and strip.placement == 'outside'", call. = FALSE)
                         } else {
                           axis_mat_x_bottom[row_pos_bottom] <- row_axes_bottom
                           axis_mat_x_top[row_pos_top] <- row_axes_top
                         }
                         if (params$strip.position == "right" &&
                             theme$strip.placement != "inside" &&
                             any(!vapply(col_axes, is.zero, logical(length(col_axes))))) {
                           warning("Suppressing axis rendering when strip.position = 'right' and strip.placement == 'outside'", call. = FALSE)
                         } else {
                           axis_mat_y_right[col_pos_right] <- col_axes_right
                           axis_mat_y_left[col_pos_left] <- col_axes_left
                         }
                       }

                       panel_table <- weave_tables_row(panel_table, axis_mat_x_top, -1, axis_height_top, "axis-t", 3)
                       panel_table <- weave_tables_row(panel_table, axis_mat_x_bottom, 0, axis_height_bottom, "axis-b", 3)
                       panel_table <- weave_tables_col(panel_table, axis_mat_y_left, -1, axis_width_left, "axis-l", 3)
                       panel_table <- weave_tables_col(panel_table, axis_mat_y_right, 0, axis_width_right, "axis-r", 3)
                       
                       
                       strip_padding <- convertUnit(theme$strip.switch.pad.wrap, "cm")
                       strip_name <- paste0("strip-", substr(params$strip.position, 1, 1))
                       strip_mat <- empty_table
                       strip_mat[panel_pos] <- unlist(unname(strips), recursive = FALSE)[[params$strip.position]]
                       if (params$strip.position %in% c("top", "bottom")) {
                         inside <- (theme$strip.placement.x %||% theme$strip.placement %||% "inside") == "inside"
                         if (params$strip.position == "top") {
                           placement <- if (inside) -1 else -2
                           strip_pad <- axis_height_top
                         } else {
                           placement <- if (inside) 0 else 1
                           strip_pad <- axis_height_bottom
                         }
                         strip_height <- unit(apply(strip_mat, 1, max_height), "cm")
                         panel_table <- weave_tables_row(panel_table, strip_mat, placement, strip_height, strip_name, 2, "on")
                         if (!inside) {
                           strip_pad[unclass(strip_pad) != 0] <- strip_padding
                           panel_table <- weave_tables_row(panel_table, row_shift = placement, row_height = strip_pad)
                         }
                       } else {
                         inside <- (theme$strip.placement.y %||% theme$strip.placement %||% "inside") == "inside"
                         if (params$strip.position == "left") {
                           placement <- if (inside) -1 else -2
                           strip_pad <- axis_width_left
                         } else {
                           placement <- if (inside) 0 else 1
                           strip_pad <- axis_width_right
                         }
                         strip_pad[unclass(strip_pad) != 0] <- strip_padding
                         strip_width <- unit(apply(strip_mat, 2, max_width), "cm")
                         panel_table <- weave_tables_col(panel_table, strip_mat, placement, strip_width, strip_name, 2, "on")
                         if (!inside) {
                           strip_pad[unclass(strip_pad) != 0] <- strip_padding
                           panel_table <- weave_tables_col(panel_table, col_shift = placement, col_width = strip_pad)
                         }
                       }
                       panel_table
                     },
                     vars = function(self) {
                       vapply(self$params$facets, as.character, character(1))
                     }
)
