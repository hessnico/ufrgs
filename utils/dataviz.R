res_diag <- function(model, dataframe) {
  
  res <- model$residuals
  fitted <- model$fitted.values
  actual <- dataframe$Gasto
  cooks_dist <- cooks.distance(lm)
  
  diag_plot <- data.frame(actual, fitted, res, cooks_dist)
  diag_plot$group <- factor(ifelse(diag_plot$cooks_dist > 0.05, 1, 0))
  
  par(mfrow=c(2,2))
  
  p1 <- ggplot(diag_plot, aes(x = 1:nrow(diag_plot), y = res)) + 
    geom_point() +
    geom_hline(yintercept = 0, color = "blue") +
    ggtitle("Resíduos") +
    xlab("Amostra")
  
  p2 <- ggplot(diag_plot, aes(x=res)) + 
    geom_histogram(bins = 50) +
    geom_density(color = "red", size = 1.2) +
    geom_vline(xintercept = 0, color = "red", size = 1.2) + 
    ggtitle("Histograma dos resíduos")
  
  
  p3 <- ggplot(diag_plot, aes(sample = res)) + 
    stat_qq(size = 2) + 
    stat_qq_line() + 
    ylab("Quantis teóricos") +
    xlab("Quantis da amostra") +
    ggtitle("qq-plot dos residuos") +
    theme(legend.position = "none")
  
  p4 <- ggplot(diag_plot, aes(1:nrow(diag_plot), cooks_dist, color=group, group=group)) +
    geom_point(size=3) +
    geom_segment(aes(1:nrow(diag_plot), xend=1:nrow(diag_plot), 0, yend=cooks_dist, color=group), data=diag_plot)  +
    theme_bw() +
    scale_color_manual(values=c("black", "red1")) +
    ylab("Distância de Cook") +
    xlab("Amostra") +
    theme(legend.position = "none")
  
  figure <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
  rm(p1, p2, p3, p4, res, fitted, diag_plot, actual, cooks_dist)
  
  return(figure)
}