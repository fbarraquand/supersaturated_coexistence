##########################Used libraries : 

library(ggplot2)
library(plotly)

##########################Fig 2 :
{
  dataresults_2a <- read.table("./DataHW1999/dataresults_2a.txt")
  dataresults_2b <- read.table("./DataHW1999/dataresults_2b.txt")
  dataresults_2c <- read.table("./DataHW1999/dataresults_2c.txt")
  
  Fig_2a <- {plot_ly(dataresults_2a, x= ~time, y= ~N1, name = "Species 1", type = "scatter", mode = "lines") %>%
      add_trace(y= ~N2, name = "Species 2", mode="lines") %>%
      add_trace(y= ~N3, name = "Species 3", mode="lines") %>%
      add_trace(y= ~N4, name = "Species 4", mode="lines") %>%
      add_trace(y= ~N5, name = "Species 5", mode="lines") %>%
      layout(title="Time course of the abundances of five species competing for five resources", xaxis=list(title="Time (days)", range=c(0, 300)), yaxis=list(title="Species abundances", range=c(0, 60)))}
  
  Fig_2b<-{plot_ly(dataresults_2b, x = ~N1, y = ~N3, z = ~N5, type = "scatter3d", mode = "lines")%>%
      layout(title="The corresponding chaotic attractor. 
           The trajectory is plotted for three of the five species, for the period from t = 1000 to t = 2000 days.", 
             scene=list(xaxis=list(title="Species 1", range=c(6, 18)), 
                        yaxis=list(title="Species 3", range=c(15, 40)), 
                        zaxis=list(title="Species 5"), range=c(25, 55), 
                        aspectmode="cube", 
                        camera=list(eye=list(x = 1.7, y = 1.7, z = .2))))}
  
  Fig_2c<-{plot_ly(dataresults_2c, x=~time, y=~Tot, type="scatter", mode="lines")%>%
      layout(title="Time course of total community biomass", xaxis=list(title="Time (days)", range=c(0, 300)), yaxis=list(title="Total biomass", range=c(0, 150)))}
  
  Fig_2bc <- {subplot(Fig_2b, Fig_2c, margin=0.1, titleY=T, titleX = T)%>%
      layout(scene = list(domain = list(x = c(0, 0.5), y = c(0, 0))), 
             title="", 
             showlegend=F)
             
  }

  Fig_2 <- {subplot(Fig_2a, Fig_2b, plotly_empty(), Fig_2c, nrows = 2, margin = 0.05) %>% 
    layout(             xaxis = list(domain=list(x=c(0.5, 1), y=c(0, 0.5))), 
                        scene = list(domain=list(x=c(0, 0.5), y=c(0, 0.5))), 
                        xaxis2 = list(domain=list(x=c(0.5, 1), y=c(0, 0.5))), 
                        showlegend=FALSE, showlegend2=FALSE, 
                        title="", 
           annotations = list(list( 
             x = 0,  
             y = 1,  
             text = "a)",  
             xref = "paper",  
             yref = "paper",  
             xanchor = "center",  
             yanchor = "bottom",  
             showarrow = F
           ),  
           list( 
             x = 0,  
             y = 0.45,  
             text = "b)",  
             xref = "paper",  
             yref = "paper",  
             xanchor = "center",  
             yanchor = "bottom",  
             showarrow = F), 
           list( 
             x = 0.5,  
             y = 0.45,  
             text = "c)",  
             xref = "paper",  
             yref = "paper",  
             xanchor = "center",  
             yanchor = "bottom",  
             showarrow = F))
    )}
  
  save_image(Fig_2, "./Figures/Figure_2.pdf",scale=2)
  
  }
  