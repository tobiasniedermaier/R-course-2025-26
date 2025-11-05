
#######################
# das 'officer'-Paket #
#######################

library(officer)
library(dplyr)
library(ggplot2)


#Worddokument erstellen:

mydoc <- read_docx() %>%

body_add_par(value="Einleitung", style="heading 1") %>%
#body_add_toc(level=2) %>%
body_add_par(value="Hintergrund", style="heading 2") %>%
body_add_par(value="Das ist ein Test. Hier schreibe ich einen Text. Gleich sollte ein Zeilenumbruch kommen: \n Dieser Satz steht in einer neuen Zeile.", style="Normal") %>%
body_add_par(value="Zwischenüberschrift", style="heading 3") %>%
body_add_par(value="Neuer Absatz. Blabla.", style="Normal") %>%

#URL einfügen:
slip_in_text("Weitere Infos unter http://www.r-project.org", style=NULL, pos="after") %>%

body_add_table(value=head(iris), style="table_template") %>%
body_add_break()


#üben: folgenden Plot würde ich gerne einbauen mit body_add_gg:

myplot <- ggplot(data=mtcars, aes(x=wt, y=mpg)) + ggtitle("Das ist ein Plot") + geom_point() + geom_smooth(method='lm', formula=y~x) + theme_bw() + theme(axis.line = element_line(colour = "black"), panel.background = element_blank(), plot.title=element_text(hjust=0.5))

setwd("D:\\Daten Samsung Notebook\\Statistik\\R üben")
setwd("C:/Users/Tobi/Desktop/R üben/")
ggsave("mypic.png")

# Einschub Ploterzeugung Ende

body_add_gg(mydoc, myplot, width=5, height=5)

print(mydoc, target="D:\\Daten Samsung Notebook\\Statistik\\R üben\\mydoc.docx")
print(mydoc, target="C:/Users/Tobi/Desktop/R üben/mydoc.docx")


#im Prinzip kann man mit dem Paket auch existierende Worddokument einlesen und Infos daraus extrahieren:
mydoc <- read_docx("Test.docx")
(content <- docx_summary(mydoc))
#nur "im Prinzip", weil das Ergebnis nicht so ist, dass man wirklich damit weiterarbeiten könnte. Aber womöglich könnte man immerhin mit grep, grepl und Pendants von stringr oder stringi einige relevante Informationen aus dem Dokument herausziehen, ähnlich wie bei PDFs nach Bearbeitung mit pdftools. Bsp. aus Vignette:
table_cells <- subset(content, content_type %in% "table cell")
head(table_cells)
#...

#Powerpoint-Präsentationen erstellen:

mypres <- read_pptx()
#1. Folie:
mypres <- add_slide(mypres, layout="Two Content", master="Office Theme")
mypres <- mypres %>%
 ph_with_text(type="title", str="Überschrift") %>%
 ph_with_text(type="ftr", str="Fussnote") %>%
 ph_with_text(type="dt", str=format(Sys.Date())) %>%
 ph_with_text(type="sldNum", str="slide 1") %>%
 ph_with_text(str="Hallo Welt! \nDies ist der zweite Stichpunkt \nund der dritte \nund vierte", type="body", index=1) %>%
 ph_with_text(str="Rechte Seite \nZweiter Punkt \nDrei \nVier", type="body", index=2)

 #2. Folie:
mypres <- mypres %>%
 add_slide(layout="Title and Content", master="Office Theme") %>%
 ph_with_table(type="body", value=head(mtcars))
 
#to be continued
print(mypres, target="D:\\Daten Samsung Notebook\\Statistik\\R üben\\mypres.pptx")
print(mypres, target="C:/Users/Tobi/Desktop/R üben/mypres.pptx")
