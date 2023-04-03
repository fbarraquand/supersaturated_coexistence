##########################Used libraries : 

library(ggplot2)
library(plotly)
library(patchwork)


##########################Fig 3 : (Note that the execution time of this program is particularly long compared to other)
{ 
  dataresults_3ab <- read.table("./DataHW1999/dataresults_3ab.txt")
  dataresults_3ab <- read.table("./DataHW1999/dataresults_3ab.txt")
  dataresults_3cd <- read.table("./DataHW1999/dataresults_3cd.txt")
 
  names(dataresults_3ab) <- c("K", "N1")
  names(dataresults_3cd) <- c("K", "Maximum","Minimum")
  
  Fig_3a <- {ggplot(dataresults_3ab, aes(x = K, y = N1)) + 
      geom_point(shape = ".", size = 0.001) + 
      theme_classic() + 
      coord_cartesian(xlim = c(0.1, 0.5), ylim = c(0, 100))}
  
  Fig_3b <- {Fig_3a + coord_cartesian(xlim = c(0.2, 0.4), ylim = c(0, 25))} 
  
  Fig_3c <- {ggplot(dataresults_3cd) + 
      geom_point(aes(x = K, y = Minimum), color = "blue") + 
      geom_point(aes(x = K, y = Maximum), color = "red") + 
      theme_classic() + 
      labs(x = "K", y = "Extremums") + 
      coord_cartesian(xlim = c(0.1, 0.5),ylim = c(0, 100))}
  
  Fig_3d <- {Fig_3c+coord_cartesian(xlim = c(0.2, 0.4),ylim = c(0, 25))}
  
  Fig_3 <- {Fig_3a + Fig_3b + Fig_3c + Fig_3d} + plot_annotation(tag_levels = "a", tag_suffix = ")")
  
  ggsave("./Figures/Figure_3.png", dpi = 500)
  
}