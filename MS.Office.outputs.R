library(officer)
library(flextable)

#function for creating word tables
fn.word.table=function(TBL,Doc.nm)
{
  #create document
  doc=read_docx()
  
  
  #add table
  # create basic flextable
  f.table=qflextable(TBL)
  
  # set table borders
  #f.table=border_outer(f.table, part="header",
  #                     border=fp_border(color="black", width = 1))
  #f.table=border_inner_h(f.table, part="all",
  #                       border=fp_border(color="black", width = 1))
  #f.table=border_inner_v(f.table, part="all",
  #                       border=fp_border(color="black", width = 1))
  
  # set fonts
  f.table=font(f.table,  fontname = "Times", part = "all")
  f.table=flextable::fontsize(f.table, size = 12, part = "all")
  # also set the table's header font as bold
  f.table=bold(f.table, part = "header")
  
  # add the table to the document
  flextable::body_add_flextable(doc, 
                                value = f.table, 
                                align = "left" )
  
  
  # export the doc 
  print(doc, target=paste(Doc.nm,".docx",sep=''))
}

#function for creating word figures
#library(ReporteRs)
# fn.word.table=function(WD,TBL,Doc.nm,caption,paragph,HdR.col,HdR.bg,Hdr.fnt.sze,Hdr.bld,
#                        body.fnt.sze,Zebra,Zebra.col,Grid.col,Fnt.hdr,Fnt.body)
# {
#   mydoc = docx(Doc.nm)  #create r object
#   
#   # add title
#   if(!is.na(caption))mydoc = addParagraph(mydoc, caption, stylename = "TitleDoc" )
#   
#   # add a paragraph
#   if(!is.na(paragph))mydoc = addParagraph(mydoc , paragph, stylename="Citationintense")
#   
#   #add table
#   MyFTable=FlexTable(TBL,add.rownames =F,
#                      header.cell.props = cellProperties(background.color=HdR.bg), 
#                      header.text.props = textProperties(color=HdR.col,font.size=Hdr.fnt.sze,
#                                                         font.weight="bold",font.family =Fnt.hdr), 
#                      body.text.props = textProperties(font.size=body.fnt.sze,font.family =Fnt.body))
#   
#   # zebra stripes - alternate colored backgrounds on table rows
#   if(Zebra=="YES") MyFTable = setZebraStyle(MyFTable, odd = Zebra.col, even = "white" )
#   
#   # table borders
#   MyFTable = setFlexTableBorders(MyFTable,
#                                  inner.vertical = borderNone(),inner.horizontal = borderNone(),
#                                  outer.vertical = borderNone(),
#                                  outer.horizontal = borderProperties(color=Grid.col, style="solid", width=4))
#   
#   # set columns widths (in inches)
#   #MyFTable = setFlexTableWidths( MyFTable, widths = Col.width)
#   
#   mydoc = addFlexTable( mydoc, MyFTable)
#   # export the doc 
#   writeDoc( mydoc, file = paste(Doc.nm,".docx",sep=''))
#   
# }
# Example:
# fn.word.table(WD=getwd(),TBL=Numbers.SF,Doc.nm="Size.comp.n.observations",caption=NA,paragph=NA,
#               HdR.col='black',HdR.bg='white',Hdr.fnt.sze=10,Hdr.bld='normal',body.fnt.sze=10,
#               Zebra='NO',Zebra.col='grey60',Grid.col='black',
#               Fnt.hdr= "Times New Roman",Fnt.body= "Times New Roman")

# fn.word.figure=function(WD,fig,Doc.nm,caption,paragph)
# {
#   options( "ReporteRs-fontsize" = 12 )  #set font
#   mydoc = docx(Doc.nm)  #create r object
#   
#   # add title
#   doc = addParagraph(mydoc, caption, stylename = "TitleDoc" )
#   
#   # add a paragraph
#   doc = addParagraph(mydoc , paragph, stylename="Citationintense")
#   
#   # add a plot into mydoc 
#   mydoc = addPlot( mydoc, fig)
#   
#   # write the doc 
#   writeDoc( mydoc, file = paste(Doc.nm,".docx",sep=''))
# }
