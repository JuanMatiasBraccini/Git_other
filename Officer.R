if(!exists('handl_OneDrive')) source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')

library(officer)
library(flextable)

# create new word document
new.word.doc=function(){
  my.doc=read_docx()
  return(my.doc)
}
# add an empty line
add.empty.line=function(doc){
  body_add_par(doc, " ")
  return("empty line added")
}
#add page break
add.page.break=function(doc){
  body_add_break(doc, pos="after")
  return("page break added")
}
# start landscape
start.landscape=function(doc){
  doc=body_end_section_continuous(doc)
  return("landscape orientation started")
}
# end landscape
end.landscape=function(doc){
  doc=body_end_section_landscape(doc)
  return("landscape orientation ended")
}

# add a title
add.title=function(doc, my.title){
  my.prop=fp_text(font.size = 14, bold = TRUE, font.family = "Times")
  the.title=fpar(ftext(my.title, prop=my.prop))
  body_add_fpar(doc, the.title)
  body_add_par(doc, " ")
  return("title added")
}

# add an image, such as a jpg or png file
add.image=function(doc, image, h=5, w=5){
  body_add_img(doc, src=image,
               height=h, width=w,
               style="centered")
  return("image added")
}

# add a data frame as a table
add.table=function(doc, tbl, col.keys=NULL, col.digits=NULL){
  # create basic flextable
  f.table=qflextable(tbl)
  # set numbers of decimals for numeric variables, if specified
  if(!is.null(col.keys)){
    for(j in 1:length(col.keys)){
      f.table=colformat_num(x=f.table,
                            col_keys=col.keys[j],
                            digits=col.digits[j])
    }
  }
  
  # set table borders
  f.table=border_outer(f.table, part="all",
                       border=fp_border(color="black", width = 1))
  f.table=border_inner_h(f.table, part="all",
                         border=fp_border(color="black", width = 1))
  f.table=border_inner_v(f.table, part="all",
                         border=fp_border(color="black", width = 1))
  
  # set fonts
  f.table=font(f.table,  fontname = "Times", part = "all")
  # also set the table's header font as bold
  f.table=bold(f.table, part = "header")
  
  # add the table to the document
  flextable::body_add_flextable(doc, 
                                value = f.table, 
                                align = "left" )
  return("table added")
}


#Example
# create an histogram and save it as a png file:
png(filename="histogram.png", width = 6, height = 6, units = 'in', res = 300)
hist(mtcars$wt)
dev.off()
# create a data frame that will become a table in my report
wide.table=mtcars[1:6, ]
wide.table$car=rownames(wide.table)
wide.table=wide.table[, c(12, 1:11)]
narrow.table=wide.table[, 1:4]
# create a new document object
doc=new.word.doc()
# add the report title and an empty line
add.title(doc, "My report")
add.empty.line(doc)
add.title(doc, "narrow table")
add.table(doc, narrow.table, col.keys=c("mpg", "disp"), col.digits=c(1,0))
add.page.break(doc)
# add the histogram with an apropriate title
add.title(doc, "Histogram - portrait")
add.image(doc, "histogram.png", h=3, w=3)
# set the orientation to lndscape
start.landscape(doc)
add.title(doc, "narrow table - landscape")
add.table(doc, narrow.table, col.keys=c("mpg", "disp"), col.digits=c(1,0))
add.empty.line(doc)
# add the wide table in landsacape page orientation
add.title(doc, "wide table in landscape orientation")
add.table(doc, wide.table, col.keys=c("mpg", "disp"), col.digits=c(1,0))
#set the orientation back to portrait
end.landscape(doc)
add.title(doc, "narrow table")
add.table(doc, narrow.table, col.keys=c("mpg", "disp"), col.digits=c(1,0))
# generate the Word document using the print function
print(doc, target="My report.docx")

