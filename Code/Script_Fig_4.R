##########################Used libraries : 

library(ggplot2)
library(plotly)

##########################Fig 4 :
{
  
  dataresults_4ab <- read.table("./DataHW1999/dataresults_4ab.txt")
  
  Fig_4a <- {plot_ly(dataresults_4ab,  x = ~time,  y = ~N1,  name = "Species 1",   type = "scatter",   mode = "lines") %>%
      add_trace(y = ~N2,   name = "Species 2",   mode = "lines") %>%
      add_trace(y = ~N3,   name = "Species 3",   mode = "lines") %>%
      add_trace(y = ~N4,   name = "Species 4",   mode = "lines") %>%
      add_trace(y = ~N5,   name = "Species 5",   mode = "lines") %>%
      add_trace(y = ~N6,   name = "Species 6",   mode = "lines") %>%
      layout(title = "The abundances of species 1-6",  xaxis = list(title = "Time (days)",  range = c(0,  10000)),  yaxis = list(title = "Species abundances",  range = c(0,  60)))}
  
  Fig_4b <- {plot_ly(dataresults_4ab,  x = ~time,  y = ~N7,  name = "Species 7",   type = "scatter",   mode = "lines") %>%
      add_trace(y = ~N8,   name = "Species 8",   mode = "lines") %>%
      add_trace(y = ~N9,   name = "Species 9",   mode = "lines") %>%
      add_trace(y = ~N10,   name = "Species 10",   mode = "lines") %>%
      add_trace(y = ~N11,   name = "Species 11",   mode = "lines") %>%
      add_trace(y = ~N12,   name = "Species 12",   mode = "lines") %>%
      layout(title = "The abundances of species 7-12",  xaxis = list(title = "Time (days)",  range = c(0,  10000)),  yaxis = list(title = "Species abundances",  range = c(0,  20)))}
  
  Fig_4 <- {subplot(Fig_4a,  Fig_4b,  margin = 0.075,  nrows = 2,  titleY = T,  titleX = T)%>%
      layout(scene = list(domain = list(x = c(0,  0.5),   y = c(0,  0))),  
             title = list(text = "",  
                        font = list(size = 15)),  
             showlegend = F,  
             annotations = list(list( 
               x = 0,    
               y = 1,    
               text = "a)",    
               xref = "paper",    
               yref = "paper",    
               xanchor = "center",    
               yanchor = "bottom",    
               showarrow = F),  
               list( 
                 x = 0,    
                 y = 0.45,    
                 text = "b)",    
                 xref = "paper",    
                 yref = "paper",    
                 xanchor = "center",    
                 yanchor = "bottom",    
                 showarrow = F))
      )
  }
  
  save_image(Fig_4,  "./Figures/Figure_4.pdf")
  
}