databahn <- function(yi, sei, 
                     tau2 = 0, 
                     title = NULL, 
                     xlab = "Mean Difference",
                     ylab = "Standard Error",
                     ci = c("normal", "t"),
                     point.fill = "#ff8da1",
                     point.color = "#3c556e",
                     point.shape = 21,
                     point.size = c("weights", "se", "none"),
                     theme = c("bw", "test", "grey", "gray", "dark")) {
  
  point.size <- match.arg(point.size)
  ci <- match.arg(ci)
  theme <- match.arg(theme)
  
  k <- length(yi)
  
  vi <- 1 / (sei^2 + tau2)
  beta <- sum(yi * vi) / sum(vi)
  
  if(point.size == "weights") {
    point.size.values <- vi
  } else if(point.size == "se") {
    point.size.values <- sei
  } else {
    point.size.values <- rep(1, k)
  }
  
  se_seq <- seq(0, max(sei), 0.001) # standard error sequence to draw confidence regions
  
  if(ci == "t") {
    
    cv_99 <- qt(0.005, df = k - 1, ncp = 0, lower.tail = FALSE) # critical value for 99% confidence interval
    cv_95 <- qt(0.025, df = k - 1, ncp = 0, lower.tail = FALSE) # critical value for 95% confidence interval
    cv_90 <- qt(0.050, df = k - 1, ncp = 0, lower.tail = FALSE) # critical value for 90% confidence interval
    
  } else { 
    
    cv_99 <- qnorm(0.005, mean = 0, sd = 1, lower.tail = FALSE) # critical value for 99% confidence interval
    cv_95 <- qnorm(0.025, mean = 0, sd = 1, lower.tail = FALSE) # critical value for 95% confidence interval
    cv_90 <- qnorm(0.050, mean = 0, sd = 1, lower.tail = FALSE) # critical value for 90% confidence interval
    
    
  }
    
  # vectors that define confidence regions 
  ll99 = beta - (cv_99*se_seq)
  ul99 = beta + (cv_99*se_seq)
  
  ll95 = beta - (cv_95*se_seq)
  ul95 = beta + (cv_95*se_seq)
  
  ll90 = beta - (cv_90*se_seq)
  ul90 = beta + (cv_90*se_seq)
  
  # a data frame of vectors that define confidence regions
  ci_data <- data.frame(se_seq, ll99, ul99, ll95, ul95, ll90, ul90)
  
  # create data for filling confidence regions 
  shaded_region_data_99 <- data.frame(
    x = c(ci_data$ll99, rev(ci_data$ul99)),
    y = c(ci_data$se_seq, rev(ci_data$se_seq))
  )
  
  shaded_region_data_95 <- data.frame(
    x = c(ci_data$ll95, rev(ci_data$ul95)),
    y = c(ci_data$se_seq, rev(ci_data$se_seq))
  )
  
  shaded_region_data_90 <- data.frame(
    x = c(ci_data$ll90, rev(ci_data$ul90)),
    y = c(ci_data$se_seq, rev(ci_data$se_seq))
  )
  
  # dataset for meta-analysis
  meta_data <- data.frame(yi = yi, sei = sei)
  
  if(point.size == "weights" | point.size == "se") {
    caption <- paste("Note: Point sizes are proportional to", ifelse(point.size == "weights", "meta-analytic weights", "standard errors"))
  } else {
    caption <- NULL
  }
  
  # plot
  databahn_plot <- ggplot(meta_data, aes(x = yi, y = sei)) +
    # draw lines for confidence regions
    geom_line(aes(y = se_seq, x = ll90), linetype = 'solid', alpha = 0.5, color = "black", lwd = 0.75, data = ci_data) +
    geom_line(aes(y = se_seq, x = ul90), linetype = 'solid', alpha = 0.5, color = "black", lwd = 0.75, data = ci_data) +
    geom_line(aes(y = se_seq, x = ll95), linetype = 'dashed', alpha = 0.5, color = "black", lwd = 0.75, data = ci_data) +
    geom_line(aes(y = se_seq, x = ul95), linetype = 'dashed', alpha = 0.5, color = "black", lwd = 0.75, data = ci_data) +
    geom_line(aes(y = se_seq, x = ll99), linetype = 'dotted', alpha = 0.5, color = "black", lwd = 0.75, data = ci_data) +
    geom_line(aes(y = se_seq, x = ul99), linetype = 'dotted', alpha = 0.5, color = "black", lwd = 0.75, data = ci_data) +
    # define plot region and reverse the standard error scale so that 0 is on top
    coord_cartesian(xlim = c(min(yi), max(yi))) + scale_y_reverse(limits = c(max(sei), 0)) +
    # fill confidence regions
    geom_polygon(data = shaded_region_data_90, aes(x = x, y = y), fill = "#9db4ca", alpha = 0.2) +
    geom_polygon(data = shaded_region_data_95, aes(x = x, y = y), fill = "#b7c7d8", alpha = 0.2) +
    geom_polygon(data = shaded_region_data_99, aes(x = x, y = y), fill = "#dde5ed", alpha = 0.2) +
    # the vertical line that passes through the average effect (or estimated)
    geom_line(aes(y = se_seq, x = beta), linetype = 'solid', alpha = 0.5, color = "white", lwd = 2, data = ci_data) +
    # plot standard errors proportional to their size
    # 'size = 1/sei^2' to highlight more informative studies
    geom_point(aes(size = point.size.values), fill = point.fill, color = point.color, shape = point.shape) + 
    # title, axis labels, etc
    labs(title = title,
         caption = caption,
         x = xlab,
         y = ylab) 

  # apply themes 
  if (theme == "bw") {
    databahn_plot <- databahn_plot + theme_bw()
  } else if(theme == "test") {
    databahn_plot <- databahn_plot + theme_test()
  } else if(theme == "grey" | theme == "gray") {
    databahn_plot <- databahn_plot + theme_grey() 
  } else if(theme == "dark") {
    databahn_plot <- databahn_plot + theme_dark()
  }
  
  # some more styling
  databahn_plot <- databahn_plot + theme(
    plot.title = element_text(colour = "#3c556e", face = "bold"),
    plot.subtitle = element_text(colour = "#4e6f90", face = "italic"),
    plot.caption = ggtext::element_markdown(hjust = 0, vjust = 0, colour = "#3c556e", face = "italic"),
    axis.title.x = element_text(colour = "#3c556e", face = "bold"),
    axis.title.y = element_text(colour = "#3c556e", face = "bold")
  )
  
  # remove unwanted legends
  databahn_plot <- databahn_plot  + theme(legend.position = "none")
  
  return(databahn_plot)
  
  } # databahn()
