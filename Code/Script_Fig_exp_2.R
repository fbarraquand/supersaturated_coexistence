##########################Used libraries : 
{
  library(ggplot2)
  library(plotly)
}

##########################Fig_exp_2

stats_exp2 <- read.table("./DataExp/Exp2/stats_exp2.txt")

set.seed(81490)
FigList <- c(1,sample((2:dim(stats_exp2)[1]),7))#randomly picking among all the simulations

for(j in (1:8)){
  dataresultsFig_exp2 <- read.table(paste("./DataExp/Exp2/dataresults_exp2_", as.character(stats_exp2[FigList[j], 1]), ".txt", sep = ""))
  
  Fig_j1 <- {plot_ly(dataresultsFig_exp2, x = ~time, y = ~N1, name = "Species 1",  type = "scatter",  mode = "lines", line = list(color = "#FFD700")) %>%
      add_trace(y = ~N2,  name = "Species 2",  mode = "lines", line = list(color = "#FFFF00")) %>%
      add_trace(y = ~N3,  name = "Species 3",  mode = "lines", line = list(color = "#7FFF00")) %>%
      add_trace(y = ~N4,  name = "Species 4",  mode = "lines", line = list(color = "#008000")) %>%
      add_trace(y = ~N5,  name = "Species 5",  mode = "lines", line = list(color = "#20B2AA")) %>%
      add_trace(y = ~N6,  name = "Species 6",  mode = "lines", line = list(color = "#0000FF")) %>%
      layout(title = "The abundances of species 1-6", 
             xaxis = list(title = "Time (days)", range = c(0, 10000)), 
             yaxis = list(title = "Abundances"))}
  
  Fig_j2 <- {plot_ly(dataresultsFig_exp2, x = ~time, y = ~N7, name = "Species 7",  type = "scatter",  mode = "lines", line = list(color = "#8A2BE2")) %>%
      add_trace(y = ~N8,  name = "Species 8",  mode = "lines", line = list(color = "#00000")) %>%
      add_trace(y = ~N9,  name = "Species 9",  mode = "lines", line = list(color = "#FF1493")) %>%
      add_trace(y = ~N10,  name = "Species 10",  mode = "lines", line = list(color = "#FF0000")) %>%
      add_trace(y = ~N11,  name = "Species 11",  mode = "lines", line = list(color = "#808080")) %>%
      add_trace(y = ~N12,  name = "Species 12",  mode = "lines", line = list(color = "#FF8C00")) %>%
      layout(title = "The abundances of species 7-12", 
             xaxis = list(title = "Time (days)", range = c(0, 10000)), 
             yaxis = list(title = "Abundances"))}
  
  
  assign(paste("Fig_exp2_", j+4*(j%/%5), sep = ""), Fig_j1)
  assign(paste("Fig_exp2_", j+4*((j%/%5)+1), sep = ""), Fig_j2)
}

Fig_exp2 <- subplot(Fig_exp2_1, Fig_exp2_2, Fig_exp2_3, Fig_exp2_4, Fig_exp2_5, Fig_exp2_6, Fig_exp2_7, Fig_exp2_8, Fig_exp2_9, Fig_exp2_10, Fig_exp2_11, Fig_exp2_12, Fig_exp2_13, Fig_exp2_14, Fig_exp2_15, Fig_exp2_16, 
                   nrows = 4, 
                   shareX = T, 
                   shareY = T)%>%
  layout(showlegend = F, 
         title = "", 
         annotations = list(list( 
           x = 0,   
           y = 1,   
           text = paste(stats_exp2[FigList[1], 1], "a)"),   
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
           text = paste(stats_exp2[FigList[2], 1], "a)"),   
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
           text = paste(stats_exp2[FigList[3], 1], "a)"),   
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
           text = paste(stats_exp2[FigList[4], 1], "a)"), 
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
           text = paste(stats_exp2[FigList[5], 1], "a)"),   
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
           text = paste(stats_exp2[FigList[6], 1], "a)"),   
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
           text = paste(stats_exp2[FigList[7], 1], "a)"),   
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
           text = paste(stats_exp2[FigList[8], 1], "a)"), 
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

save_image(Fig_exp2, "./Figures/Figure_exp2.pdf")
