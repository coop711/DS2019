pie_gg <-
function(df, ggtitle = "", font.family = ""){
  n <- length(names(df))
  y_coord <- df$Freq / 2 + c(0, cumsum(head(df$Freq, -1)))
  pie_label <- paste(levels(df$vote), format(df$Freq, big.mark = ","), 
                     sep = "\n") 
  p1 <- ggplot(df, aes(x = "", 
                       y = Freq, 
                       fill = vote)) 
  p2 <- p1 + 
    geom_bar(width = 1, 
             stat = "identity", 
             position = position_stack(reverse = TRUE))
  pie_1 <- p2 + 
    coord_polar(theta = "y", 
                start = 3 * pi / 2, 
                direction = -1)
  pie_2 <- pie_1 + 
    scale_y_continuous(name = "", 
                       breaks = NULL) +
    scale_x_discrete(name = "") 
  pie_3 <- pie_2 +
    scale_fill_manual(name = "", 
                      values = rainbow(n)[n:1])
  pie_4 <- pie_3 +
    theme_void(base_family = font.family)
  pie_5 <- pie_4 +
    guides(fill = "none")
  pie_6 <- pie_5 +
    geom_text(aes(y = y_coord), 
              label = pie_label, 
              family = font.family, 
              position = position_identity())
  pie_7 <- pie_6 +
    ggtitle(ggtitle) + 
    theme(plot.margin = unit(c(1, 1, 1.5, 1), "lines"), 
          plot.title = element_text(hjust = 0.5))
  return(pie_7)
}
