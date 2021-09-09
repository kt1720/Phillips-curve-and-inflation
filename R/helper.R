ggplot_forecast <- function(pd) {
  # Adapted from Johnny Hong
  p1a <- ggplot(data = pd, aes(x = date,y = observed))
  p1a <- p1a + geom_line(col = 'red')
  p1a <- p1a + geom_line(aes(y = fitted),col = 'blue')
  p1a <- p1a + geom_line(aes(y = forecast)) +
    geom_ribbon(aes(ymin = lo95,ymax = hi95),alpha = .25)
  p1a <- p1a + scale_x_date(date_breaks = "24 month", 
                            date_labels = "%Y")
  #  p1a <- p1a + scale_y_continuous(name = 'Units of Y')
  p1a <- p1a + theme(axis.text.x = element_text(size=10))
  p1a <- p1a + ggtitle("Model fit to CPI inflation data", 
                       subtitle = "(black=forecast, blue=fitted, red=data, shadow=95% conf. interval)" ) +
    xlab("Year")
  p1a <- p1a +  theme(plot.title = element_text(hjust = 0.5))
  p1a
}