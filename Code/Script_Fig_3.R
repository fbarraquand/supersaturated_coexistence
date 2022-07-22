##########################Used libraries : 

library(ggplot2)
library(plotly)


##########################Fig 3 :
{ 
 dataresults_3ab <- read.table("./DataHW1999/dataresults_3ab.txt")
 dataresults_3cd <- read.table("./DataHW1999/dataresults_3cd.txt")
 
 #Fig_3a : 
 {Fig_3a <- plot_ly()
  
  loopFig_3a <- lapply(
   1:dim(dataresults_3ab)[2], 
   function(j) Fig_3a <<- Fig_3a %>% 
    add_trace(x=~dataresults_3ab[, 1], 
         y=~dataresults_3ab[, j], 
         name=paste("N1", as.character(j), sep = "_"), 
         type="scatter", 
         mode="markers", 
         marker=list(size=1, color="black"))
  )
  
  Fig_3a <- Fig_3a%>% layout(title=list(text="Bifurcation diagram for five species competing for five ressources. Local minima and maxima of species 1,  
            from t=2, 000 to t=4, 000 days,  as a function of the half-saturation constant K41",  font=list(size=12)), 
               showlegend=F, 
               xaxis=list(title="Half-saturation constant K41,  of species 1", range=c(0.1, 0.5)), 
               yaxis=list(title="Abundancie of species 1", range=c(0, 100))) 
 }
 
 
 Fig_3b<-{Fig_3a %>%
   layout(title="Magnified part", xaxis=list(title="Half-saturation constant K41, of species 1", range=c(0.2, 0.4)), yaxis=list(title="Abundancie of species 1", range=c(0, 25)))}
 
 Fig_3c <- {plot_ly(dataresults_3cd, x= dataresults_3cd[, 1], y= dataresults_3cd[, 2], name = "Maximum", type = "scatter", mode = "lines",line=list(color="red")) %>%
   add_trace(y= dataresults_3cd[, 3], name = "Minimum", mode="lines",line=list(color="blue")) %>%
   layout(title="Local maximums and minimums of abundance of species 1 between t=2,000 and t=4,000 as a function of the half-saturation constant K31.", xaxis=list(title="Half-saturation constant, K41 of species 1", range=c(0.1, 0.5)), yaxis=list(title="Abundance of species 1", range=c(0, 100)))}
 
 Fig_3d<-{Fig_3c %>%
   layout(title="Magnified part", xaxis=list(title="Half-saturation constant K41, of species 1", range=c(0.2, 0.4)), yaxis=list(title="Abundancie of species 1", range=c(0, 25)))}

  
 Fig_3 <- {subplot(Fig_3a, Fig_3c, Fig_3b, Fig_3d, margin=0.1, nrows=2, titleY=T, titleX = T)%>%
   layout(scene = list(domain = list(x = c(0, 0.5),  y = c(0, 0))), 
       title=list(text="", font=list(size=12)), 
       showlegend=F, 
       annotations = list(list( 
        x = 0,  
        y = 0.425,  
        text = "b)",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = F), 
        list( 
         x = 0,  
         y = 1,  
         text = "a)",  
         xref = "paper",  
         yref = "paper",  
         xanchor = "center",  
         yanchor = "bottom",  
         showarrow = F),
        list( 
          x = 0.5,  
          y = 1,  
          text = "c)",  
          xref = "paper",  
          yref = "paper",  
          xanchor = "center",  
          yanchor = "bottom",  
          showarrow = F),
        list( 
          x = 0.5,  
          y = 0.425,  
          text = "d)",  
          xref = "paper",  
          yref = "paper",  
          xanchor = "center",  
          yanchor = "bottom",  
          showarrow = F))
   )
 }
 
 save_image(Fig_3, "./Figures/Figure_3.png")
 
}
