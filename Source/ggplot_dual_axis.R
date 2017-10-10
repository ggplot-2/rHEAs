ggplot_dual_axis = function(p1, p2, which.axis = "x") {
  
  # Update plot with transparent panel
  p2 = p2 + 
    theme(panel.background = element_rect(fill = NA),
          panel.border = element_blank())
  
  grid.newpage()
  
  # Increase right margin if which.axis == "y"
  if(which.axis == "y") p1 = p1 + 
    theme(plot.margin = unit(c(0.7, 1.5, 0.4, 0.4), "cm"),
          panel.border = element_blank())
  
  # Extract gtable
  g1 = ggplot_gtable(ggplot_build(p1))
  g2 = ggplot_gtable(ggplot_build(p2))
  
  # Overlap the panel of the second plot on that of the first
  pp = c(subset(g1$layout, name == "panel", se = t:r))
  g = gtable_add_grob(g1, 
                      g2$grobs[[which(g2$layout$name=="panel")]], 
                      pp$t, 
                      pp$l, 
                      pp$b, 
                      pp$l)
  
  # Steal axis from second plot and modify
  # axis.lab = ifelse(which.axis == "x", "axis-b", "axis-l")
  # 
  # ia = which(g2$layout$name == axis.lab)
  # 
  # ga = g2$grobs[[ia]]
  # 
  # ax = ga$children[[2]]
  
  # Switch position of ticks and labels
  # if(which.axis == "x") ax$heights = rev(ax$heights) else ax$widths = rev(ax$widths)
  # 
  # ax$grobs = rev(ax$grobs)
  # 
  # if(which.axis == "x") 
  #   
  #   ax$grobs[[2]]$y = ax$grobs[[2]]$y - unit(1, "npc") + unit(0.15, "cm") else
  #     
  #     ax$grobs[[1]]$x = ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  
  # Modify existing row to be tall enough for axis
  if(which.axis == "x") g$heights[[2]] = g$heights[g2$layout[ia,]$t]
  
  # Add new row or column for axis label
  if(which.axis == "x") {
    
    g = gtable_add_grob(g, ax, 2, 4, 2, 4) 
    
    g = gtable_add_rows(g, g2$heights[1], 1)
    
    g = gtable_add_grob(g, g2$grob[[6]], 2, 4, 2, 4)
    
  } else {
    
    g = gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    
    g = gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b) 
    
    g = gtable_add_grob(g, g2$grob[[7]], pp$t, length(g$widths), pp$b - 1)
    
  }
  
  # Draw it
  grid.draw(g)
  
}