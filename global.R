#global.R
library(shiny)
library(shinyjqui)
library(shinythemes)
library(htmlwidgets)
library(colourpicker)
library(DT)
library(openxlsx)
library(showtext)
fonts <- list(
  "Helvetica Neue" = "C:/Users/shun/AppData/Local/Microsoft/Windows/Fonts/HelveticaNeue.ttf",
  "Arial" = "C:/Windows/Fonts/arial.ttf",
  "Calibri" = "C:/Windows/Fonts/calibri.ttf",
  "Segoe UI" = "C:/Windows/Fonts/segoeui.ttf",
  "Times New Roman" = "C:/Windows/Fonts/times.ttf",
  "Yu Mincho" = "C:/Windows/Fonts/yumin.ttf"
)
lapply(names(fonts), function(name) font_add(name, fonts[[name]]))
showtext_auto()
library(cowplot)
library(ggpubr)
library(svglite)
library(RColorBrewer)
library(tidyverse)
library(shinyjs)
library(utils)
library(tools)
library(stringi)
library(wesanderson)
library(grDevices)
library(stringr)
library(ggbeeswarm)
library(ggdist)
library(gghalves)
enableBookmarking(store = "server")



palet <- list("one" = list("YlOrRd","YlOrBr","YlGnBu","YlGn","Reds","RdPu","Purples","PuRd","PuBuGn","PuBu","Oranges","OrRd","Greys","Greens","GnBu","Blues","BuPu","BuGn"),
              "two"= list("Paired","Spectral","RdYlGn","RdYlBu","RdGy","RdBu","PuOr","PRGn","PiYG","BrBG"),
              "wesanderson" = list("GrandBudapest1","GrandBudapest2","Moonrise1","Moonrise2","Moonrise3","Royal1","Royal2","Cavalcanti1",
                                   "Chevalier1","Zissou1","FantasticFox1","Darjeeling1","Darjeeling2","Rushmore1"))


font <-c("Helvetica Neue","Arial","Calibri","Segoe UI","Times New Roman" ,"Yu Mincho")


source("server_modules/plot_stat_module.R", local = T)

# shokiti
shokiti_summ <- "SE"
type_fomu <- y ~ x
div4 <- 1
w <-7
data_type <- "csv"
sheet_name <- 1
options(dplyr.summarise.inform = FALSE)
custom_size_w <-  300
custom_size_h <-  300


# geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)