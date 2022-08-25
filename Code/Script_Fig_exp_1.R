##########################Used libraries : 
{
  library(ggplot2)
  library(plotly)
}

##########################Fig_exp_1

stats_exp1 <- read.table("./DataExp/Exp1/stats_exp1.txt")  

set.seed(211232)
FigList <- c(1,round(runif(7, 2, dim(stats_exp1)[1])))

for(j in (1:8)){
  dataresults_Fig_exp1 <- read.table(paste("./DataExp/Exp1/dataresults_exp1_",as.character(stats_exp1[FigList[j], 1]), ".txt", sep = ""))
  
  Fig_j1 <- {plot_ly(dataresults_Fig_exp1, x = ~time, y = ~N1, name = "Species 1",  type = "scatter",  mode = "lines", line = list(color = "#FFD700")) %>%
      add_trace(y = ~N2,  name = "Species 2",  mode = "lines", line = list(color = "#FFFF00")) %>%
      add_trace(y = ~N3,  name = "Species 3",  mode = "lines", line = list(color = "#7FFF00")) %>%
      add_trace(y = ~N4,  name = "Species 4",  mode = "lines", line = list(color = "#008000")) %>%
      add_trace(y = ~N5,  name = "Species 5",  mode = "lines", line = list(color = "#20B2AA")) %>%
      add_trace(y = ~N6,  name = "Species 6",  mode = "lines", line = list(color = "#0000FF")) %>%
      layout(title = "The abundances of species 1-6", 
             xaxis = list(title = "Time (days)", range = c(0, 10000)), 
             yaxis = list(title = "Abundances"), 
             shapes = list(list(type = "line", 
                              x0 = 0.1, x1 = 0.1, xref = "paper", 
                              y0 = 0,  y1 = max(dataresults_Fig_exp1[,2:7]), 
                              line = list(color = "black", dash = "dot", width = 1)), 
                         list(type = "line", 
                              x0 = 0.3, x1 = 0.3, xref = "paper", 
                              y0 = 0,  y1 = max(dataresults_Fig_exp1[,2:7]), 
                              line = list(color = "black", dash = "dot", width = 1)), 
                         list(type = "line", 
                              x0 = 0.5, x1 = 0.5, xref = "paper", 
                              y0 = 0,  y1 = max(dataresults_Fig_exp1[,2:7]), 
                              line = list(color = "black", dash = "dot", width = 1))
                         
             ))}
  
  Fig_j2 <- {plot_ly(dataresults_Fig_exp1, x = ~time, y = ~N7, name = "Species 7",  type = "scatter",  mode = "lines", line = list(color = "#8A2BE2")) %>%
      add_trace(y = ~N8,  name = "Species 8",  mode = "lines", line = list(color = "#000000")) %>%
      add_trace(y = ~N9,  name = "Species 9",  mode = "lines", line = list(color = "#FF1493")) %>%
      add_trace(y = ~N10,  name = "Species 10",  mode = "lines", line = list(color = "#FF0000")) %>%
      add_trace(y = ~N11,  name = "Species 11",  mode = "lines", line = list(color = "#808080")) %>%
      add_trace(y = ~N12,  name = "Species 12",  mode = "lines", line = list(color = "#FF8C00")) %>%
      layout(title = "The abundances of species 7-12", 
             xaxis = list(title = "Time (days)", range = c(0, 10000)), 
             yaxis = list(title = "Abundances"), 
             shapes = list(list(type = "line", 
                              x0 = 0.1, x1 = 0.1, xref = "paper", 
                              y0 = 0,  y1 = max(dataresults_Fig_exp1[,8:13]), 
                              line = list(color = "black", dash = "dot", width = 1)), 
                         list(type = "line", 
                              x0 = 0.3, x1 = 0.3, xref = "paper", 
                              y0 = 0,  y1 = max(dataresults_Fig_exp1[,8:13]), 
                              line = list(color = "black", dash = "dot", width = 1)), 
                         list(type = "line", 
                              x0 = 0.5, x1 = 0.5, xref = "paper", 
                              y0 = 0,  y1 = max(dataresults_Fig_exp1[,8:13]), 
                              line = list(color = "black", dash = "dot", width = 1))
                         
             ))}
  
  
  assign(paste("Fig_exp1_", j+4*(j%/%5), sep = ""), Fig_j1)
  assign(paste("Fig_exp1_", j+4*((j%/%5)+1), sep = ""), Fig_j2)
}

Fig_exp1 <- subplot(Fig_exp1_1, Fig_exp1_2, Fig_exp1_3, Fig_exp1_4, Fig_exp1_5, Fig_exp1_6, Fig_exp1_7, Fig_exp1_8, Fig_exp1_9, Fig_exp1_10, Fig_exp1_11, Fig_exp1_12, Fig_exp1_13, Fig_exp1_14, Fig_exp1_15, Fig_exp1_16, 
                   nrows = 4, 
                   shareX = T, 
                   shareY = T)%>%
  layout(showlegend = F, 
         title = "", 
         annotations = list(list( 
           x = 0,   
           y = 1,   
           text = paste(stats_exp1[FigList[1], 1], "a)"),   
           xref = "paper",   
           yref = "paper",   
           xanchor = "center",   
           yanchor = "bottom",   
           showarrow = F
         ),
         list( 
           x = 0,   
           y = 0.725,   
           text = paste("b)"),   
           xref = "paper",   
           yref = "paper",   
           xanchor = "center",   
           yanchor = "bottom",   
           showarrow = F
         ),   
         list( 
           x = 0.25,   
           y = 1, 
           text = paste(stats_exp1[FigList[2], 1], "a)"),   
           xref = "paper",   
           yref = "paper",   
           xanchor = "center",   
           yanchor = "bottom",   
           showarrow = F
         ),
         list( 
           x = 0.25,   
           y = 0.725, 
           text = paste("b)"),   
           xref = "paper",   
           yref = "paper",   
           xanchor = "center",   
           yanchor = "bottom",   
           showarrow = F
         ),  
         list( 
           x = 0.5,   
           y = 1,   
           text = paste(stats_exp1[FigList[3], 1], "a)"),   
           xref = "paper",   
           yref = "paper",   
           xanchor = "center",   
           yanchor = "bottom",   
           showarrow = F
         ), 
         list( 
           x = 0.5,   
           y = 0.725,   
           text = paste("b)"),   
           xref = "paper",   
           yref = "paper",   
           xanchor = "center",   
           yanchor = "bottom",   
           showarrow = F
         ),
         list( 
           x = 0.75,   
           y = 1,   
           text = paste(stats_exp1[FigList[4], 1], "a)"), 
           xref = "paper",   
           yref = "paper",   
           xanchor = "center",   
           yanchor = "bottom",   
           showarrow = F
         ), 
         list( 
           x = 0.75,   
           y = 0.725,   
           text = paste("b)"), 
           xref = "paper",   
           yref = "paper",   
           xanchor = "center",   
           yanchor = "bottom",   
           showarrow = F
         ),
         list( 
           x = 0,   
           y = 0.475,   
           text = paste(stats_exp1[FigList[5], 1], "a)"),   
           xref = "paper",   
           yref = "paper",   
           xanchor = "center",   
           yanchor = "bottom",   
           showarrow = F
         ),
         list( 
           x = 0,   
           y = 0.225,   
           text = paste("b)"),   
           xref = "paper",   
           yref = "paper",   
           xanchor = "center",   
           yanchor = "bottom",   
           showarrow = F
         ),
         list( 
           x = 0.25,   
           y = 0.475,   
           text = paste(stats_exp1[FigList[6], 1], "a)"),   
           xref = "paper",   
           yref = "paper",   
           xanchor = "center",   
           yanchor = "bottom",   
           showarrow = F
         ),
         list( 
           x = 0.25,   
           y = 0.225,   
           text = paste("b)"),   
           xref = "paper",   
           yref = "paper",   
           xanchor = "center",   
           yanchor = "bottom",   
           showarrow = F
         ),
         list( 
           x = 0.5,   
           y = 0.475,   
           text = paste(stats_exp1[FigList[7], 1], "a)"),   
           xref = "paper",   
           yref = "paper",   
           xanchor = "center",   
           yanchor = "bottom",   
           showarrow = F
         ),
         list( 
           x = 0.5,   
           y = 0.225,   
           text = paste("b)"),   
           xref = "paper",   
           yref = "paper",   
           xanchor = "center",   
           yanchor = "bottom",   
           showarrow = F
         ), 
         list( 
           x = 0.75,   
           y = 0.475,   
           text = paste(stats_exp1[FigList[8], 1], "a)"), 
           xref = "paper",   
           yref = "paper",   
           xanchor = "center",   
           yanchor = "bottom",   
           showarrow = F), 
         list( 
           x = 0.75,   
           y = 0.225,   
           text = paste("b)"), 
           xref = "paper",   
           yref = "paper",   
           xanchor = "center",   
           yanchor = "bottom",   
           showarrow = F)
         ))

save_image(Fig_exp1, "./Figures/Figure_exp1.pdf")
