##########################Used libraries : 

library(ggplot2)
library(plotly)

##########################Fig 1 :
{

dataresults_1ab <- read.table("./DataHW1999/dataresults_1ab.txt")
dataresults_1c <- read.table("./DataHW1999/dataresults_1c.txt")
dataresults_1d <- read.table("./DataHW1999/dataresults_1d.txt")

Fig_1a <- {plot_ly(dataresults_1ab, x= ~time, y= ~N1, name = "Species 1", type = "scatter", mode = "lines") %>%
    add_trace(y= ~N2, name = "Species 2", mode="lines") %>%
    add_trace(y= ~N3, name = "Species 3", mode="lines")%>%
    layout(title="Time course of the abundances of three species competing for three ressources", xaxis=list(title="Time (days)", range=c(0, 200)), yaxis=list(title="Species abundances", range=c(0, 50)))}

Fig_1b<-{plot_ly(dataresults_1ab, x = ~N1, y = ~N2, z = ~N3, type = "scatter3d", mode = "lines")%>%
    layout(title="The corresponding limit cycle", 
           scene=list(xaxis=list(title="Species 1", range=c(0, 50)), 
                      yaxis=list(title="Species 2", range=c(0, 50)), 
                      zaxis=list(title="Species 3", range=c(0, 50)), 
                      camera=list(eye=list(x = 1.75, y = 1.75, z = .2))))}

Fig_1c <- {plot_ly(dataresults_1c, x= ~time, y= ~N1, name = "Species 1", type = "scatter", mode = "lines") %>%
    add_trace(y= ~N2, name = "Species 2", mode="lines") %>%
    add_trace(y= ~N3, name = "Species 3", mode="lines") %>%
    add_trace(y= ~N4, name = "Species 4", mode="lines") %>%
    add_trace(y= ~N5, name = "Species 5", mode="lines") %>%
    add_trace(y= ~N6, name = "Species 6", mode="lines") %>%
    layout(title="Small-amplitude oscilations of six species on three ressources", xaxis=list(title="Time (days)", range=c(0, 15000)), yaxis=list(title="Species abundances", range=c(0, 70)))}

Fig_1d <- {plot_ly(dataresults_1d, x= ~time, y= ~N1, name = "Species 1", type = "scatter", mode = "lines") %>%
    add_trace(y= ~N2, name = "Species 2", mode="lines") %>%
    add_trace(y= ~N3, name = "Species 3", mode="lines") %>%
    add_trace(y= ~N4, name = "Species 4", mode="lines") %>%
    add_trace(y= ~N5, name = "Species 5", mode="lines") %>%
    add_trace(y= ~N6, name = "Species 6", mode="lines") %>%
    add_trace(y= ~N7, name = "Species 7", mode="lines") %>%
    add_trace(y= ~N8, name = "Species 8", mode="lines") %>%
    add_trace(y= ~N9, name = "Species 9", mode="lines") %>%
    layout(title="Large-amplitude oscilations of nine species on three ressources", xaxis=list(title="Time (days)", range=c(0, 3000)), yaxis=list(title="Species abundances", range=c(0, 40)))}

Fig_1 <- {subplot(Fig_1a, Fig_1b, Fig_1c, Fig_1d, nrows=2, margin=0.1, titleY=T, titleX = T)%>%
    layout(scene = list(domain = list(x = c(0.5, 1), y = c(0.5, 1))), 
           showlegend=F, 
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
             x = 0.6,  
             y = 1,  
             text = "b)",  
             xref = "paper",  
             yref = "paper",  
             xanchor = "center",  
             yanchor = "bottom",  
             showarrow = F
           ),  
           list( 
             x = 0,  
             y = 0.425,  
             text = "c)",  
             xref = "paper",  
             yref = "paper",  
             xanchor = "center",  
             yanchor = "bottom",  
             showarrow = F
           ), 
           list( 
             x = 0.6,  
             y = 0.425,  
             text = "d)", 
             xref = "paper",  
             yref = "paper",  
             xanchor = "center",  
             yanchor = "bottom",  
             showarrow = F
           )))
}

save_image(Fig_1, "./Figures/Figure_1.pdf",scale=2)

}