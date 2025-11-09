
#########################
# The 'officer' package #
#########################

library(officer)
library(dplyr)
library(ggplot2)


# Create a Word document

mydoc <- read_docx() %>%

body_add_par(value="Introduction", style="heading 1") %>%
#body_add_toc(level=2) %>%
body_add_par(value="Background", style="heading 2") %>%
body_add_par(value="Hello world. This is some random text. You should now see a line break: \n This sentence appears in the next line.", style="Normal") %>%
body_add_par(value="Sub-heading", style="heading 3") %>%
body_add_par(value="New paragraph. Blabla.", style="Normal") %>%

#Insert URL:
slip_in_text("Further information at http://www.r-project.org", style=NULL, pos="after") %>%

body_add_table(value=head(iris), style="table_template") %>%
body_add_break()


#To do: Add the following plot using body_add_gg:

myplot <- ggplot(data=mtcars, aes(x=wt, y=mpg)) + ggtitle("Das ist ein Plot") + geom_point() + geom_smooth(method='lm', formula=y~x) + theme_bw() + theme(axis.line = element_line(colour = "black"), panel.background = element_blank(), plot.title=element_text(hjust=0.5))

setwd("D:\\R")
ggsave("mypic.png")

# Finished adding a plot.

body_add_gg(mydoc, myplot, width=5, height=5)

print(mydoc, target="D:\\R\\mydoc.docx")
print(mydoc, target="D:/R/mydoc.docx")


#In principle, the package can also read existing Word documents and extract information from them:
mydoc <- read_docx("Test.docx")
(content <- docx_summary(mydoc))
#But the result is not really such that one could work with the output. But it might be useful if one wants to extract information using grep, grepl or similar functions from stringr or stringi. Example from vignette:
table_cells <- subset(content, content_type %in% "table cell")
head(table_cells)
#...

#Create Powerpoint presentations:

mypres <- read_pptx()
#1st slide:
mypres <- add_slide(mypres, layout="Two Content", master="Office Theme")
mypres <- mypres %>%
 ph_with_text(type="title", str="Heading") %>%
 ph_with_text(type="ftr", str="Footnote") %>%
 ph_with_text(type="dt", str=format(Sys.Date())) %>%
 ph_with_text(type="sldNum", str="slide 1") %>%
 ph_with_text(str="Hello world! \nThis is the second bullet point \nand now the third \nand forth", type="body", index=1) %>%
 ph_with_text(str="Rechte Seite \nZweiter Punkt \nDrei \nVier", type="body", index=2)

 #2nd slide:
mypres <- mypres %>%
 add_slide(layout="Title and Content", master="Office Theme") %>%
 ph_with_table(type="body", value=head(mtcars))
 
#To be continued
print(mypres, target="D:\\R üben\\mypres.pptx")
print(mypres, target="C:/R üben/mypres.pptx")

