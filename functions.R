plot_hist <- function(variable, x_lab, title, x_intercept, print_binwidth){
  # determine the binwidth
  x <- ceiling(
    max(DATA_prtcpnt[[variable]]) - 
    min(DATA_prtcpnt[[variable]])
  ) / 10
  
  while (x <= 1 & 1000 %% (x*1000) > 0.01){ # multiple of binwidth should be 1 (https://stackoverflow.com/questions/13614749/modulus-bug-in-r)
    
    y <- 1 %/% x # integer division 
    if (y %% 2 != 0){ # Is y even?
      y <- y + 1  
    }
    x <- 1 / y
  } 
  
  binwidth <- x
  
  ifelse(print_binwidth, print(binwidth), "") 
  
  # specify the left/lower limit of the scale
  x_lim_right <- ceiling(max(DATA_prtcpnt[[variable]])/(binwidth/2))*(binwidth/2)
  
  if(x_lim_right == max(DATA_prtcpnt[[variable]])){
    x_lim_right <- x_lim_right + (binwidth/2)
  }
    
  x_lim_left  <- floor(min(DATA_prtcpnt[[variable]])/(binwidth/2))*(binwidth/2)
  
  if(x_lim_left == min(DATA_prtcpnt[[variable]])){
    x_lim_left <- x_lim_left - (binwidth/2)
  }
  
  # create the plot
  plot <- ggplot(
    DATA_prtcpnt,
    aes_string(variable)
  ) +
    geom_histogram(
      aes(y =..density..),
      binwidth = binwidth,
      colour = "black", 
      fill = "grey"
    ) +
    labs(x = x_lab, y = "Density") +
    ggtitle(title) +
    theme(axis.title.y = element_text(
      size = 12, 
      vjust = 2
    )) +
    my_theme +
    stat_function(
      fun = dnorm, 
      args = list(
        mean = mean(DATA_prtcpnt[[variable]]), 
        sd   = sd(DATA_prtcpnt[[variable]])
      ),
      xlim = c(x_lim_left, x_lim_right)
    ) +
    geom_vline(
      xintercept = x_intercept,
      linetype = "dotted"
    )
  
  return(plot)
}

plot_feature <- function(variable, label, legend_position, group_colors){
  plot <- ggplot(
    data = DATA_stimuli,
    aes(
      y = variable, 
      color = Piece,
      x = "",
      group = Performer
    )
  ) +
    geom_jitter(aes(shape = Performer), width = 0.1) + # 16 or 20
    guides(color = guide_legend(override.aes = list(size = 3))) +
    scale_x_discrete(limits = "") +
    labs(y = label, color= "Piece", x = "") +
    scale_shape_discrete(name  = "Category") +
    scale_colour_manual(values = group_colors) +
    theme(legend.position  = legend_position) +
    my_theme
  
  return(plot)
}

plot_feature_only_piece <- function(variable, label, legend_position, group_colors){
  arg <- match.call()
  
  plot <- ggplot(
    data = DATA_stimuli,
    aes(
      y = eval(arg$variable), 
      color = Piece,
      x = ""
    )
  ) +
    geom_jitter(aes(), width = 0.1) + # 16 or 20
    guides(color = guide_legend(override.aes = list(size = 3))) +
    scale_x_discrete(limits = "") +
    labs(y = label, color= "Piece", x = "") +
    scale_colour_manual(values = group_colors) +
    theme(legend.position  = legend_position) +
    my_theme
  
  return(plot)
}

plot_feature_only_performer <- function(variable, label, legend_position){
  arg <- match.call()
  plot <- ggplot(
    data = DATA_stimuli,
    aes(
      y = eval(arg$variable), 
      x = "",
      group = Performer
    )
  ) +
    geom_jitter(aes(shape = Performer), width = 0.1) + # 16 or 20
    guides(color = guide_legend(override.aes = list(size = 3))) +
    scale_x_discrete(limits = "") +
    labs(y = label, group = "Category", x = "") +
    scale_shape_discrete(name  = "Category") +
    theme(legend.position  = legend_position) +
    my_theme
  
  return(plot)
}

plot_feature_performer_color <- function(variable, label, legend_position){
  arg <- match.call()
  
  plot <- ggplot(
    data = DATA_stimuli,
    aes(
      y = eval(arg$variable), 
      color = Performer,
      x = ""
    )
  ) +
    geom_jitter(aes(), width = 0.1) + # 16 or 20
    guides(color = guide_legend(override.aes = list(size = 3))) +
    scale_x_discrete(limits = "") +
    labs(y = label, color= "Performer", x = "") +
    scale_color_brewer(palette = "Set2") +
    theme(legend.position  = legend_position) +
    my_theme
  
  return(plot)
}