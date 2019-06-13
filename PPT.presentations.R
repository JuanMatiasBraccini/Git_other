#script for creating power point presentatiosn

library( ReporteRs )
require( magrittr )

PPT.presentation=function(WD,Doc.name,Templt,Titl,Subtl,Add.dte,DATE,Titl.foot,Add.pge,slide.args)
{
  setwd(WD)
  
  # Create a PowerPoint document
  if(is.na(Templt)) doc = pptx( )
  if(!is.na(Templt)) doc <- pptx(template = Templt)
  
  
  #Title page
  #note: %>% (pipping) is used as a with() statement
  if(is.na(Templt)) doc <- doc %>% addSlide(slide.layout = "Title Slide" ) %>% addTitle(Titl) %>% addSubtitle(Subtl)
  if(!is.na(Templt)) doc <- doc %>% addSlide(slide.layout = "Title Only" ) %>% addTitle(Titl)   
  if(Add.dte=="YES")      doc <- addDate(doc,value=DATE)
  if(!is.na(Titl.foot))   doc <- addFooter(doc, Titl.foot)
  if(Add.pge=="YES")      doc <- addPageNumber(doc)
  
  
  
  #Add slides
  n.slides=length(slide.args)
  for(i in 1:n.slides)
  {
    SLD=slide.args[[i]]
    
    doc <- addSlide(doc, SLD$layout)
    doc <- addTitle(doc,SLD$Titl)
    
    if(SLD$type=="plot")
    {
      doc <- addPlot(doc, SLD$plotFunc, offx=SLD$offx, offy=SLD$offy, width=SLD$w, height=SLD$h)
      if(Add.pge=="YES") doc <- addPageNumber(doc, SLD$Page)
    }
    
    if(SLD$type=="table")
    {
      doc <- addFlexTable(doc, SLD$Table, offx=SLD$offx, offy=SLD$offy, width=SLD$w, height=SLD$h)
      doc <- addParagraph(doc, SLD$Text)
      if(Add.pge=="YES") doc <- addPageNumber(doc, SLD$Page)      
    }
    
    if(SLD$type=="Script")
    {
      doc <- addPlot(doc, SLD$plotFunc)

      doc <- addRScript(doc, text=SLD$code)
      if(Add.pge=="YES") doc <- addPageNumber(doc, SLD$Page)
    }
    
    if(SLD$type=="Image")
    {
      doc <- addImage(doc, SLD$path, offx=SLD$offx, offy=SLD$offy, width=SLD$w, height=SLD$h)
      doc <- addParagraph(doc, SLD$Text)      
    }
    
    if(SLD$type=="Double.Image")
    {
      doc <- addImage(doc, SLD$path, offx=SLD$offx, offy=SLD$offy, width=SLD$w, height=SLD$h)
      doc <- addParagraph(doc, SLD$Text)   
      
      doc <- addImage(doc, SLD$path2, offx=SLD$offx2, offy=SLD$offy, width=SLD$w, height=SLD$h)
      doc <- addParagraph(doc, SLD$Text2) 
    }
    
    if(SLD$type=="List")
    {
      # Unordered list
      doc <- addParagraph(doc, value= SLD$the_list,
                          par.properties =  parProperties(list.style = 'unordered'))
      if(Add.pge=="YES") doc <- addPageNumber(doc, SLD$Page)
      
    }
  }
  
  
  #Write the document 
  writeDoc(doc, Doc.name)
}
