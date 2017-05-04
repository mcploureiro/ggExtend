library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)

#Function to align a ggplot object with a tableGrob with the y-axis. Can align either left or right
##Pos = 0 puts table on left Pos = - 1 puts table on right
ggplot_align_table_s <- function( p, df, axis = "axis-l", pos = 0){
    #Get the axis information
    axis_num <- which(p$layout[,"name"] == "axis-l")
    axis_data <- p$layout[axis_num,]
    
    #Made these all dynamic
    axis_gt <- which(sapply(p[["grobs"]][[axis_num]][["children"]], is.gtable))
    axis_gt_class <- sapply(p[["grobs"]][[axis_num]][["children"]][[axis_gt]][["grobs"]], class)
    axis_gt_class_num <- which(sapply(axis_gt_class, function(x) any(x %in% "titleGrob")))
    
    axis_y_vals <-p[["grobs"]][[axis_num]][["children"]][[axis_gt]][["grobs"]][[axis_gt_class_num]][["children"]][[1]][["y"]] ##Try to rewrite this...it will be hard
    axis_y_unit <- attr(axis_y_vals, "unit")
    
    axis_y_vals_numeric <- as.numeric(axis_y_vals)
    #Getting difference in heights. Appending 0 and 1 so we can get differences at ends as well.
    y_diff <- diff(c(0,axis_y_vals_numeric,1))
    
    #Calculating the boxes heights. For each one it will be the:
    #   (1) 1/2 diff b/n previous y-val and current y-val and
    #   (2) 1/2 diff b/n next y-val and current y-val
    #We also return the y-value within the box where the text should be displayed. This is so the tableGrob will align with the labels
    #For the 1st and last we need to do this slightly differently
    calc_boxes <- sapply(1:(length(y_diff) - 1), function(i, x) {
      if(i == 1){
        box_y <- x[i] + (x[i + 1] / 2)
        just_y <- (x[i] / box_y)
      } else if(i == (length(x) - 1)){
        box_y <- x[i + 1] + (x[i] / 2)
        just_y <- (x[i] / 2) / box_y 
      } else{
        box_y <- (x[i] + x[i + 1]) / 2
        just_y <- (x[i] / 2) / box_y
      }
      return(c(box_y = box_y, just_y = just_y))
    }, x = y_diff)
    
    calc_boxes_t <- t(calc_boxes)
    
    #Need to use rev() because we want the last element to be first. So we need to rev() everything
    mytheme <- ttheme_default(core = list(fg_params = list(y = rev(calc_boxes_t[,"just_y"]))))
    table <- tableGrob( df[nrow(df):1,], theme = mytheme, cols = NULL)
    table$heights <- unit(rev(calc_boxes_t[,"box_y"]), axis_y_unit)
    
    
    #Add columns separately
    table_headers <- tableGrob( df[nrow(df):1,] )[1,]
    #Reset widths - dont need because should already be same b/n table_header and table

    #Get which column we add our grobs to
    add_col <- ifelse(pos == -1, -1, 1)
    
    #Add table/headers. For now add new column to left most always. I think you could customize if you want.
    p2 <- gtable_add_cols(p,  sum(table$widths)+unit(4,"mm"), pos = pos)
    p3 <- gtable_add_grob(p2, table, t = axis_data$t, l = add_col)
    
    #Add row right before the table
    p4 <- gtable_add_rows(p3, sum(table_headers$heights) + unit(4,"mm"), pos = axis_data$t - 1)
    p5 <- gtable_add_grob(p4, table_headers, t = axis_data$t, l = add_col )
    
    p5

}



#Function to align a ggplot object with a tableGrob with the x-axis. Can align either top or bottom
##Pos = 0 puts table on top. Pos = - 1 puts table on bottom
ggplot_align_table_t <- function( p, df, axis = "axis-b", pos = 0){
  #Get the axis information
  axis_num <- which(p$layout[,"name"] == "axis-b")
  axis_data <- p$layout[axis_num,]
  
  #Made these all dynamic
  axis_gt <- which(sapply(p[["grobs"]][[axis_num]][["children"]], is.gtable))
  axis_gt_class <- sapply(p[["grobs"]][[axis_num]][["children"]][[axis_gt]][["grobs"]], class)
  axis_gt_class_num <- which(sapply(axis_gt_class, function(x) any(x %in% "titleGrob")))
  
  axis_x_vals <-p[["grobs"]][[axis_num]][["children"]][[axis_gt]][["grobs"]][[axis_gt_class_num]][["children"]][[1]][["x"]] ##Try to rewrite this...it will be hard
  axis_x_unit <- attr(axis_x_vals, "unit")
  
  axis_x_vals_numeric <- as.numeric(axis_x_vals)
  x_diff <- diff(c(0,axis_x_vals_numeric,1))
  
  #Calculating the boxes widths. For each one it will be the:
  #   (1) 1/2 diff b/n previous x-val and current x-val and
  #   (2) 1/2 diff b/n next x-val and current y-val
  #We also return the x-value within the box where the text should be displayed. This is so the tableGrob will align with the labels
  #For the 1st and last we need to do this slightly differently
  calc_boxes <- sapply(1:(length(x_diff) - 1), function(i, x) {
    if(i == 1){
      box_x <- x[i] + (x[i + 1] / 2)
      just_x <- (x[i] / box_x)
    } else if(i == (length(x) - 1)){
      box_x <- x[i + 1] + (x[i] / 2)
      just_x <- (x[i] / 2) / box_x 
    } else{
      box_x <- (x[i] + x[i + 1]) / 2
      just_x <- (x[i] / 2) / box_x
    }
    return(c(box_x = box_x, just_x = just_x))
  }, x = x_diff)
  
  calc_boxes_t <- t(calc_boxes)

  mytheme <- ttheme_default(core = list(fg_params = list(x = rep(calc_boxes_t[,"just_x"], each = length(calc_boxes_t[,"just_x"])))),
                            colhead = list(fg_params = list(x = calc_boxes_t[,"just_x"])))
  table <- tableGrob( df, theme = mytheme, rows = NULL)
  table$widths <- unit((calc_boxes_t[,"box_x"]), axis_x_unit)
  
  
  #Add columns separately
  table_headers <- tableGrob( df )[1,]

  #Get which column we add our grobs to
  add_row <- ifelse(pos == -1, -1, 1)
  
  #Add table/headers. For now add new column to left most always. I think you could customize if you want.
  p2 <- gtable_add_rows(p,  sum(table$heights)+unit(4,"mm"), pos = pos)

  p3 <- gtable_add_grob(p2, table, t = add_row, l = axis_data$l)
  p3
  
  
}
