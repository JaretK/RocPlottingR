library(ggplot2)
library(magrittr)
library(ggpubr)
library(dplyr)
library(tidyr)
library(reshape2)

setup()

TITLE = "" # Title
SAVE_FILE = "" #absolute path of save file
file1 = "" #file 1, necessary
file1_LEGEND = "" # file 1 legend
file2 = "" # file 2, not required. Leave empty if not using
file2_LEGEND = "" # file 2 legend, not required. Leave empty if not using
X_variable = "" # X variable, false positive rate
Y_variable = "" # Y variable, true positive rate
X_axis = "" # x axis label
Y_axis = "" # y axis label
SAVE = TRUE #save file?
ImageMagickExportToPng = TRUE # use imagemagick command line (installed separately) to convert eps to png

####################
# 
# setup workspace
# 
setup <- function(){
  # Clear console
  cat("\014") 
  # Clean workspace
  rm(list=ls())
}

####################
# 
# Core Logic
# 

# add eps to save file if not present
if (!endsWith(SAVE_FILE, ".eps")){
  SAVE_FILE <- paste(trimws(SAVE_FILE), ".eps", sep="")
}
# make eps file container
if (SAVE){
  postscript(SAVE_FILE, width = 480, height = 480)
}
# variables to subset from input files
variables_to_select <- c(X_variable, Y_variable)
# read csv, select variables, and drop empties
csv1 = read.csv(file1) %>% select(one_of(variables_to_select)) %>% drop_na()
# assign legend value, which will be used later to color lines
csv1$Legend = file1_LEGEND
# add to csv_list
csv_list = list(csv1)

if (file2!=""){
  csv2 = read.csv(file2) %>% select(one_of(variables_to_select)) %>% drop_na()
  csv2$Legend = file2_LEGEND
  csv_list[[2]] = csv2
}
csv_df = do.call("rbind", csv_list)
mynamestheme <- theme(plot.title = element_text(family = "Arial", size = (20), hjust = 0.5),
                      legend.title = element_text(family = "Arial", size = (10)),
                      legend.text = element_text(family = "Arial", size = (10)),
                      legend.position = "top",
                      axis.title = element_text(family = "Arial", size = (10)),
                      axis.text = element_text(family = "Arial", size = (8)))
ggplot(csv_df, aes_string(x = X_variable, y = Y_variable, color = "Legend")) +
  geom_line() +
  ggpubr::theme_classic2() +
  ggtitle(TITLE) +
  scale_x_continuous(name=X_axis, limits=c(0, 1)) +
  scale_y_continuous(name=Y_axis, limits=c(0, 1))+
  mynamestheme

if (SAVE){
  dev.off()
}

if (ImageMagickExportToPng){
  SAVE_FILE_ESCAPED = gsub(" ", "\\\\ ", trimws(SAVE_FILE))
  PngFile = gsub(".eps",".png",SAVE_FILE_ESCAPED)
  COMMAND = sprintf("convert -density 300 %s -flatten -rotate 90 -resize 1024x1024 %s",SAVE_FILE_ESCAPED, PngFile)
  system(COMMAND)
}