library(grid)
library(gtable)
library(ggplot2)


add_notes <- function(plot, notes, t = NULL, b = NULL, l = NULL, r = NULL){
  
  #Title and notes grobs
  notes_title_grob <- textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 8), just = "left", x = unit(0,"npc"))
  notes_text_grobs <- lapply(notes, function(x) textGrob(x, gp = gpar(fontsize = 7), x = unit(0, "npc"), just = "left"))
  
  #Extract info on where the top extent of xlabel is and where the left extent of panels are. This will dictate where we add the notes
  grobP <- ggplotGrob(p)
  #Heights
  notes_title_height <- unit(1,"mm") + grobHeight(notes_title_grob)
  notes_text_heights <- Reduce(unit.c, lapply(notes_text_grobs, function(x) grobHeight(x) + unit(2.5, "mm"))) #Need reduce to combine into a vector to work with gtable_add_rows
  notes_all_heights <- unit.c(notes_title_height, notes_text_heights) #Excluding padding on top. Will be added later
  
  #Getting positions to add grobs
  xlab_pos <- grobP$layout[grepl("xlab-b", grobP$layout$name),"t"]
  if(is.null(t)) t <- xlab_pos + 2 + seq_along(notes_all_heights)
  if(is.null(b)) b <- t
    
  #Getting panels, if facetting this will have multiple panels otherwise only one
  plots_pos <- grobP$layout[grepl("panel-", grobP$layout$name), "l"]
  if(is.null(l)) l <- min(plots_pos)
  if(is.null(r)) r <- max(plots_pos)
  
  
  grobP2 <- gtable_add_rows(grobP, heights = unit.c(unit(2,"mm"), notes_all_heights), pos = min(t) - 2) #Alternatively should start adding at t = xlab + 1
  grobP3 <- gtable_add_grob(grobP2, c(list(notes_title_grob), notes_text_grobs), t = t, b = b, l = l, r = r)
  
  grobP3
}
