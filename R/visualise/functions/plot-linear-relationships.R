plotLinearRelationship = function(data, x, y, x_lims = FALSE, y_lims = FALSE, x_lab = FALSE){
  
  if(is.character(x_lab)){
    x_label = x_lab
  } else{
    x_label = x
  }
  y = paste0(y,"Change")

  plot <- data %>%
    ggplot(aes(y = .data[[y]], x = .data[[x]])) + 
    geom_point(na.rm = TRUE) + 
    geom_smooth(method = "lm", na.rm = TRUE) + 
    labs(y = y, x = x) +
    theme_bw()
  
  if(is.numeric(x_lims)) {
    plot <- plot + 
      xlim(x_lims)
  }
  
  if (is.numeric(y_lims)){
    plot <- plot + 
      ylim(y_lims)
  }
  
  plot
}
