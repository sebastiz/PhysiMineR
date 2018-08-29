library(knitr)

s="S:\\Gastroenterology\\Seb\\R\\Scripts\\Manometry\\BariumSwallowStudy.R"
spin(s)
o=spin(s,knit=FALSE)
knit2html(o,output="S:\\Gastroenterology\\Seb\\JavaPortableLauncher\\PhysiPopDONOTTOUCH\\Outputs\\out.html")