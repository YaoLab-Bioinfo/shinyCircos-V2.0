options(shiny.maxRequestSize=200*1024^2)
options(warn=-1)
library(shiny)
library(circlize)
library(bs4Dash)
library(DT)
library(RColorBrewer)
library(shinyWidgets)
library(data.table)
library(shinyBS)
library(sortable)
library(shinyjqui)
library(shinycssloaders)
library(colourpicker)
library(gridBase)
library(ComplexHeatmap)
library(randomcoloR)
library(gtools)
source("plot.R")
source("writeCmd.R")

getmulcolor <- function(colorgroup, colortype, plotType = NULL){
  if(is.null(plotType)){ #links
    if("color" %in% colnames(colorgroup)){
      colorgroup1 <- mixedsort(unique(colorgroup$color))
      colnum <- length(colorgroup1)
      colortype_fun <- unlist(strsplit(colortype,split = "/"))[1]
      colortype_palettes <- unlist(strsplit(colortype,split = "/"))[2]
      maxcolnum <- as.numeric(unlist(strsplit(colortype,split = "/"))[3])
      if(colortype_fun == "grDevices::rainbow"){
        color <- rainbow(colnum)
      }else if(colortype_fun == "RColorBrewer::brewer.pal"){
        if(colnum <= maxcolnum){
          color <- sample(brewer.pal(maxcolnum, colortype_palettes),replace = FALSE,size = colnum)
        }else{
          color <- c(rep(brewer.pal(maxcolnum, colortype_palettes),colnum %/% maxcolnum) , brewer.pal(colnum %% maxcolnum, colortype_palettes))
        }
      }
      return(paste0(paste0(colorgroup1,":",color),collapse = ";"))
    }else{
      return("")
    }
  }else if(plotType == "heatmap-discrete"){
    colorgroup1 <- mixedsort(unique(c(as.matrix(colorgroup[,-c(1:3)]))))
    colnum <- length(colorgroup1)
    colortype_fun <- unlist(strsplit(colortype,split = "/"))[1]
    colortype_palettes <- unlist(strsplit(colortype,split = "/"))[2]
    maxcolnum <- as.numeric(unlist(strsplit(colortype,split = "/"))[3])
    if(colortype_fun == "grDevices::rainbow"){
      color <- rainbow(colnum)
    }else if(colortype_fun == "RColorBrewer::brewer.pal"){
      if(colnum <= maxcolnum){
        color <- sample(brewer.pal(maxcolnum, colortype_palettes),replace = FALSE,size = colnum)
      }else{
        color <- c(rep(brewer.pal(maxcolnum, colortype_palettes),colnum %/% maxcolnum) , brewer.pal(colnum %% maxcolnum, colortype_palettes))
      }
    }
    return(paste0(paste0(colorgroup1,":",color),collapse = ";"))
  }else if(plotType == "rect-discrete"){
    colorgroup1 <- mixedsort(unique(colorgroup[,4]))
    colnum <- length(colorgroup1)
    colortype_fun <- unlist(strsplit(colortype,split = "/"))[1]
    colortype_palettes <- unlist(strsplit(colortype,split = "/"))[2]
    maxcolnum <- as.numeric(unlist(strsplit(colortype,split = "/"))[3])
    if(colortype_fun == "grDevices::rainbow"){
      color <- rainbow(colnum)
    }else if(colortype_fun == "RColorBrewer::brewer.pal"){
      if(colnum <= maxcolnum){
        color <- sample(brewer.pal(maxcolnum, colortype_palettes),replace = FALSE,size = colnum)
      }else{
        color <- c(rep(brewer.pal(maxcolnum, colortype_palettes),colnum %/% maxcolnum) , brewer.pal(colnum %% maxcolnum, colortype_palettes))
      }
    }
    return(paste0(paste0(colorgroup1,":",color),collapse = ";"))
  }else{
    if("color" %in% colnames(colorgroup) | "stack" %in% colnames(colorgroup)){
      if("color" %in% colnames(colorgroup)){
        colorgroup1 <- mixedsort(unique(colorgroup$color))
      }else{
        colorgroup1 <- mixedsort(unique(colorgroup$stack))
      }
      colnum <- length(colorgroup1)
      colortype_fun <- unlist(strsplit(colortype,split = "/"))[1]
      colortype_palettes <- unlist(strsplit(colortype,split = "/"))[2]
      maxcolnum <- as.numeric(unlist(strsplit(colortype,split = "/"))[3])
      if(colortype_fun == "grDevices::rainbow"){
        color <- rainbow(colnum)
      }else if(colortype_fun == "RColorBrewer::brewer.pal"){
        if(colnum <= maxcolnum){
          color <- sample(brewer.pal(maxcolnum, colortype_palettes),replace = FALSE,size = colnum)
        }else{
          color <- c(rep(brewer.pal(maxcolnum, colortype_palettes),colnum %/% maxcolnum) , brewer.pal(colnum %% maxcolnum, colortype_palettes))
        }
      }
      return(paste0(paste0(colorgroup1,":",color),collapse = ";"))
    }else{
      return("")
    }
  }
}