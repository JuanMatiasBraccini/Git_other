#Theme used for Parks Australia project
theme_PA<-function(Ttl.siz=18,Sbt.siz=16,str.siz=12,strx.siz=12,
                   cap.siz=10,
                   lgT.siz=14,leg.siz=12,axs.t.siz=10,axs.T.siz=14)
{
  font<-windowsFonts("Arial" = windowsFont("Arial"))
  theme_bw()%+replace% 
    theme(
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #axis.line = element_line(),
      #axis.ticks = element_line(),          #strip axis ticks
      
      # strip background
      strip.background = element_rect(
        fill = "grey90",
        colour = "grey90"),
      
      #text elements
        #title
      plot.title = element_text(             
        family = font,                           
        size = Ttl.siz,                         
        face = 'bold',                           #bold typeface
        hjust = 0,                               #left align
        vjust = 2),                              #raise slightly
      
        #subtitle
      plot.subtitle = element_text(          
        family = font,                           
        size = Sbt.siz,
        hjust = 0,                               #left align
        vjust = 2),                         
      
        #strip legend
      strip.text = element_text(
        family = font,
        size = str.siz),
      strip.text.x = element_text(
        family = font,
        size = strx.siz,
        margin = margin(.1,0,.1,0, "cm")),

      
        #caption
      plot.caption = element_text(          
        family = font,                           
        size = cap.siz,                          
        hjust = 1),                             #right align
      
        #legend
      legend.title=element_text(
        family = font,
        size=lgT.siz),
      legend.text=element_text(
        family = font,
        size=leg.siz),
      
        #axis titles
      axis.title = element_text(             
        family = font,                          
        size = axs.T.siz),                     
      
        #axis text
      axis.text = element_text(              
        family = font,                          
        size = axs.t.siz)                       
    )
}