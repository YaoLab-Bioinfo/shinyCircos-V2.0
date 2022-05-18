source("server.R")


legendplot <- function(tktype,data.TT,data.TT_old,i,legendpos){
  if(tktype == "heatmap-gradual"){
    break1 <- min(as.numeric(as.matrix(data.TT[,-c(1:3)])))
    break2 <- max(as.numeric(as.matrix(data.TT[,-c(1:3)])))
    midpoint <- (break1+break2)/2
    hmapcols <- gsub('\\"',"",hmapcols)
    hmapcols <- unlist(strsplit(hmapcols,"\\."))
    f <- colorRamp2(breaks = c(break1, midpoint, break2), colors = hmapcols)
    if(legendpos == "Right"){
      return(
        Legend(
          col_fun = f,
          title = paste0("track",i)
          
        )
      )
    }else{
      return(
        Legend(
          col_fun = f,
          title = paste0("track",i),
          direction = "horizontal"
        )
      )
    }
    
  }else if(tktype == "rect-gradual"){
    f <- colorRamp2(breaks = c(min(data.TT[,4]), mean(data.TT[,4]), max(data.TT[,4])), colors = rectcols)
    if(legendpos == "Right"){
      return(
        Legend(
          col_fun = f,
          title = paste0("track",i)
        )
      )
    }else{
      return(
        Legend(
          col_fun = f,
          title = paste0("track",i),
          direction = "horizontal"
        )
      )
    }
  }else if(tktype == "rect-discrete"){
    if(legendpos == "Right"){
      return(
        Legend(
          at = unique(data.TT_old[,4]),
          legend_gp = gpar(fill = unique(data.TT[,4])),
          nrow = 6,
          title = paste0("track",i)
        )
      )
    }else{
      return(
        Legend(
          at = unique(data.TT_old[,4]),
          legend_gp = gpar(fill = unique(data.TT[,4])),
          ncol = 4,
          by_row = TRUE,
          title = paste0("track",i)
        )
      )
    }
    
  }else if(tktype == "heatmap-discrete"){
    if(legendpos == "Right"){
      return(
        Legend(
          at = unique(c(t(data.TT_old[,4:length(data.TT_old)]))),
          legend_gp = gpar(fill = unique(c(t(data.TT)))),
          nrow = 6,
          title = paste0("track",i)
        )
      )
    }else{
      return(
        Legend(
          at = unique(c(t(data.TT_old[,4:length(data.TT_old)]))),
          legend_gp = gpar(fill = unique(c(t(data.TT)))),
          ncol = 4,
          by_row = TRUE,
          title = paste0("track",i)
        )
      )
    }
    
  }else{NULL}
  
}



plotcircos <- function(x , colorChr , plotTypes , chr_height , dis_Chr , data.CN , labels_inf_chr , rotation , gap.width , labelChr_size , outAxis_size){
  circos.par("start.degree" = 90 - rotation , "gap.degree" = gap.width , cell.padding=c(0,0,0,0) , track.margin=c(0,0))
  circos.genomicInitialize(x,plotType = plotTypes , axis.labels.cex = outAxis_size , labels.cex = labelChr_size)
  if(!is.null(labels_inf_chr)){
    if(labels_inf_chr[[2]]=="outside"){
      circos.genomicLabels(data.CN, labels.column = 4, connection_height = labels_inf_chr[[3]]/4, labels_height = (labels_inf_chr[[3]]/4)*3 , cex = ((((as.numeric(labels_inf_chr[[3]]))*4)/5)/max(strwidth(data.CN[,4])))-0.15 , line_col = labels_inf_chr[[4]] , col = labels_inf_chr[[4]] , padding = 0 , track.margin = c(0,0), side = "outside")
    }
  }
  if("labels" %in% plotTypes && !("axis" %in% plotTypes)){
    gapgap <- 0.02
  }else{
    gapgap <- 0
  }
  circos.genomicTrackPlotRegion(ylim = c(0, 1) , bg.col = colorChr, bg.border = NA , track.height = chr_height , track.margin = c(dis_Chr,gapgap))	
  if(!is.null(labels_inf_chr)){
    if(labels_inf_chr[[2]]=="inside"){
      circos.genomicLabels(data.CN, labels.column = 4, connection_height = labels_inf_chr[[3]]/4, labels_height = (labels_inf_chr[[3]]/4)*3 , cex = ((((as.numeric(labels_inf_chr[[3]]))*4)/5)/max(strwidth(data.CN[,4])))-0.15 , line_col = labels_inf_chr[[4]] , col = labels_inf_chr[[4]] , padding = 0 , track.margin = c(0,0), side = "inside")
    }
  }
}

plotcircos.notrack <- function(x , plotTypes , units , data.CN , labels_inf_chr , rotation , gap.width , labelChr_size , outAxis_size){
  circos.par("start.degree"=90-rotation, "gap.degree"=gap.width, cell.padding=c(0,0,0,0), track.margin=c(0,0))
  circos.genomicInitialize(x,plotType = plotTypes , axis.labels.cex = outAxis_size , labels.cex = labelChr_size)
  if(!is.null(labels_inf_chr)){
    if(labels_inf_chr[[2]]=="inside"){
      circos.genomicLabels(data.CN, labels.column = 4, connection_height = labels_inf_chr[[3]]/4, labels_height = (labels_inf_chr[[3]]/4)*3 , cex =((((as.numeric(labels_inf_chr[[3]]))*4)/5)/max(strwidth(data.CN[,4])))-0.15 , line_col = labels_inf_chr[[4]] , col = labels_inf_chr[[4]] , padding = 0 , track.margin = c(0,0), side = "inside")
    }
  }
  
}

plotcircos.cyto <- function(x , plotTypes , chr_height , dis_Chr , units , data.CN , labels_inf_chr , rotation , gap.width , labelChr_size , outAxis_size){ 
  circos.par("start.degree"=90-rotation, "gap.degree"=gap.width, cell.padding=c(0,0,0,0), track.margin=c(0,0))
  circos.genomicInitialize(x,plotType = plotTypes , axis.labels.cex = outAxis_size , labels.cex = labelChr_size)
  if(!is.null(labels_inf_chr)){
    if(labels_inf_chr[[2]]=="outside"){
      circos.genomicLabels(data.CN, labels.column = 4, connection_height = labels_inf_chr[[3]]/4, labels_height = (labels_inf_chr[[3]]/4)*3 , cex =((((as.numeric(labels_inf_chr[[3]]))*4)/5)/max(strwidth(data.CN[,4])))-0.15 , line_col = labels_inf_chr[[4]] , col = labels_inf_chr[[4]] , padding = 0 , track.margin = c(0,0), side = "outside")
    }
  }
  if("labels" %in% plotTypes && !("axis" %in% plotTypes)){
    gapgap <- 0.02
  }else{
    gapgap <- 0
  }
  circos.genomicTrackPlotRegion(x, ylim = c(0, 1), bg.border = NA, track.height = chr_height, track.margin = c(dis_Chr,gapgap) , panel.fun = function(region, value, ...){
    col = cytoband.col(value[[2]])
    circos.genomicRect(region, value, ybottom = 0, ytop = 1, col = col, border = NA, ...)
    xlim = get.cell.meta.data("xlim")
    circos.rect(xlim[1], 0, xlim[2], 1, border = "black")
    }, cell.padding = c(0, 0, 0, 0)) 
  if(!is.null(labels_inf_chr)){
    if(labels_inf_chr[[2]]=="inside"){
      circos.genomicLabels(data.CN, labels.column = 4, connection_height = labels_inf_chr[[3]]/4, labels_height = (labels_inf_chr[[3]]/4)*3 , cex =((((as.numeric(labels_inf_chr[[3]]))*4)/5)/max(strwidth(data.CN[,4])))-0.15 , line_col = labels_inf_chr[[4]] , col = labels_inf_chr[[4]] , padding = 0 , track.margin = c(0,0), side = "inside")
    }
  }
}

plotfig <- function(input , output , session , data.C , data.T , dis_Chr , data.L , data.N , colorChr , tra_Margin , labels_inf , labelChr , tra_hmap_typcolhmap , tra_border , tra_heatcol_dis , tra_heat_heatcoldiscus ,
                    trackChr , tratype , sam_datype , chr_height , datatypeChr , heightTra , sam_chr_type , tra_poi_poisiz , heatmapcols , tra_bgcol , legendtext , gap.width , tra_yaxis,
                    tra_hmap_poslines , tra_hmap_poslinhei , tra_hmap_cellbord , tra_hmap_cellbord_col , tra_hmap_heatmapcol , plotsize ,
                    tra_rect_rectcol , tra_trct_colrect , tra_rect_rectcoldis , tra_rect_rectcoldiscus , tra_transparency , tra_coltype , tra_colcol ,
                    tra_colorcus , tra_line_fillarea , tra_poipch , tra_colorline , tra_baseline , outAxis , fontSize , outAxis_size , labelChr_size , tra_bar_direction ,
                    tra_bar_Boundary , tra_bar_coldir1 , tra_bar_coldir2 , hltTrack.List , hltdata.List , tra_line_selrea , tra_bar_borderarea , colformatLinks , colorLinks ,
                    selcolorLinks , transparencyhltLinks , gracolinks , transparencyLinks , legendpos , addlegend , hlt_data , midplot){
  
  
  heilab <- 0
  if(!is.null(data.N)){
    heilab <- sum(as.numeric(labels_inf[,3]))
  }
  heihat <- 0
  tramar <- 0
  if(!is.null(data.T)){
    for (k in 1:length(data.T)) {
      if(tra_hmap_poslines[[k]] == "1"){
        if(tratype[[k]] == "heatmap-gradual"|tratype[[k]] =="heatmap-discrete"){
          heihat <- heihat + tra_hmap_poslinhei[[k]]
        }
      }
      tramar <- tramar + tra_Margin[[k]]
      
    }
  }else{
    heightTra <- list()
  }
  allheight <- sum(heihat,tramar,dis_Chr,heilab,as.numeric(chr_height),sum(as.numeric(unlist(heightTra))))
  if(!is.null(data.L)){
    if(allheight > 0.7){
      if(!is.null(data.T)){
        for (k in 1:length(data.T)) {
          if(tratype[[k]] == "heatmap-gradual"|tratype[[k]] =="heatmap-discrete"){
            tra_hmap_poslinhei[[k]] <- 0.7*tra_hmap_poslinhei[[k]]/allheight
          }
          heightTra[[k]] <- 0.7*heightTra[[k]]/allheight
        }
      }
      dis_Chr <- 0.7*dis_Chr/allheight
      if(!is.null(data.N)){
        for (k in 1:length(data.N)){
          labels_inf[k,3] <- 0.7* as.numeric(labels_inf[k,3])/allheight
        }
      }
      chr_height <- 0.7*chr_height/allheight
    }
  }else{
    if(allheight > 0.9){
      if(!is.null(data.T)){
        for (k in 1:length(data.T)) {
          if(tratype[[k]] == "heatmap-gradual"|tratype[[k]] =="heatmap-discrete"){
            tra_hmap_poslinhei[[k]] <- 0.9*tra_hmap_poslinhei[[k]]/allheight
          }
          heightTra[[k]] <- 0.9*heightTra[[k]]/allheight
        }
      }
      dis_Chr <- 0.9*dis_Chr/allheight
      if(!is.null(data.N)){
        for (k in 1:length(data.N)){
          labels_inf[k,3] <- 0.9*as.numeric(labels_inf[k,3])/allheight
        }
      }
      chr_height <- 0.9*chr_height/allheight
    }
  }
  
  
  
  
  
  if(!is.null(data.L)){
    if(ncol(data.L)==6 | ncol(data.L)==7){
      data.L1 <<- data.L[,1:3]
      data.L2 <<- data.L[,4:6]
      data.L1[,2] <- as.numeric(data.L1[,2])
      data.L1[,3] <- as.numeric(data.L1[,3])
      data.L2[,2] <- as.numeric(data.L2[,2])
      data.L2[,3] <- as.numeric(data.L2[,3])	  
      data.L1$num <<- 1:nrow(data.L1)
      data.L2$num <<- 1:nrow(data.L2)
      rownames(data.L1) <<- data.L1$num
      rownames(data.L2) <<- data.L2$num
    }
  }
  circlize_plot <- function(){
    data.C[,2] <- as.numeric(data.C[,2])
    data.C[,3] <- as.numeric(data.C[,3])
    circos.clear()
    
    if(!is.null(data.N)){
      c <- NULL
      for (k in 1:nrow(labels_inf)) {
        kkkk <- labels_inf[k,]
        kkkk[,c(1,3,5)] <- as.numeric(kkkk[,c(1,3,5)])
        c <- rbind(c,kkkk) 
      }
      labels_inf <- c
      if(0 %in% labels_inf[,1]){
        lsb_tra_index <- labels_inf[,1] %in% 0
        labels_inf_chr <- labels_inf[lsb_tra_index,]
        labels_inf_tra <- labels_inf[!lsb_tra_index,]
        data.CN <- data.N[[as.numeric(labels_inf_chr[[5]])]]
      }else{
        labels_inf_tra <- labels_inf
        labels_inf_chr <- NULL
      }
      lab_inf <- (1:length(data.T)) %in% labels_inf_tra[,1]
    }else{
      labels_inf_chr <- NULL
    }
    
    ## *** The gap width ***
    if(1 %in% unlist(tra_yaxis)){
      repnumgap <- round(length(unique(data.C[,1]))/length(gap.width))+1
      gap.width <- rep(gap.width, repnumgap)[1:length(unique(data.C[,1]))]
      gap.width <- as.numeric(gap.width)
      if(gap.width[length(gap.width)] < 6){
        gap.width[length(gap.width)] <- 6
      }
    }else{
      repnumgap <- round(length(unique(data.C[,1]))/length(gap.width))+1
      gap.width <- rep(gap.width, repnumgap)[1:length(unique(data.C[,1]))]
      gap.width <- as.numeric(gap.width)
    }
    
    
    
    colorChr <- gsub('\\"',"",colorChr)
    colorChr <- gsub("0x","#", colorChr)        
    repnumcol <- round(length(unique(data.C[,1]))/length(colorChr))+1
    colorChr <- rep(colorChr, repnumcol)[1:length(unique(data.C[,1]))]
    ## *** The gap width ***
    
    rotation <- gap.width[length(gap.width)]/2
    if(outAxis == 1 && labelChr == 1){
      plotTypes <- c("labels","axis")
    }else if(outAxis == 1 && labelChr == 2){
      plotTypes <- "axis"
    }else if(outAxis == 2 && labelChr == 1){
      plotTypes <- "labels"
    }else{
      plotTypes <- NULL
    }
    if(sam_datype == "a"){ # upload
      if(datatypeChr == "1"){ # general
        if(trackChr == "track"){ #Chromosome band show
          #trackChr : "Show" = "track", "Hide" = ""
          plotcircos(data.C , colorChr = colorChr , plotTypes = plotTypes , chr_height = chr_height , dis_Chr = dis_Chr , labels_inf_chr = labels_inf_chr , data.CN = data.CN , rotation = rotation , gap.width = gap.width , outAxis_size = outAxis_size , labelChr_size = labelChr_size)
        }else if(trackChr!="track"){
          
          plotcircos.notrack(data.C , plotTypes = plotTypes , labels_inf_chr = labels_inf_chr , data.CN = data.CN , rotation = rotation , gap.width = gap.width , outAxis_size = outAxis_size , labelChr_size = labelChr_size)
        }
      }else if(datatypeChr == "2"){# cytoband
        
        plotcircos.cyto(data.C , plotTypes=plotTypes , chr_height = chr_height , dis_Chr = dis_Chr , labels_inf_chr = labels_inf_chr , data.CN = data.CN , rotation = rotation , gap.width = gap.width ,  outAxis_size = outAxis_size , labelChr_size = labelChr_size)
        
      }
    }else if(sam_datype == "b"){ #sample
      if(sam_chr_type == "1"){
        plotcircos(data.C , colorChr = colorChr , plotTypes = plotTypes , chr_height = chr_height , dis_Chr = dis_Chr , labels_inf_chr = labels_inf_chr , data.CN = data.CN , rotation = rotation , gap.width = gap.width , outAxis_size = outAxis_size , labelChr_size = labelChr_size)
      }else{
        plotcircos.cyto(data.C , plotTypes = plotTypes , chr_height = chr_height , dis_Chr = dis_Chr , labels_inf_chr = labels_inf_chr , data.CN = data.CN , rotation = rotation , gap.width = gap.width ,  outAxis_size = outAxis_size , labelChr_size = labelChr_size)
        
      }
    }
    if(!is.null(unlist(data.T))){
      for(i in 1:length(data.T)){
        
        if(!is.null(data.N)){
          if(lab_inf[i]){
            labels_inf <- labels_inf_tra[labels_inf_tra[,1] %in% i,]
            data.NN <- data.N[[as.numeric(labels_inf[[5]])]]
            labelsize <- ((((as.numeric(labels_inf[[3]]))*4)/5)/max(strwidth(data.NN[,4])))-0.15
            lab_height <- as.numeric(labels_inf[[3]])
          }
        }else{
          lab_inf <- rep(0, times=length(data.T))
        }
        data.TT <- data.T[[i]]
        data.TT_old <- data.T[[i]]
        tktype <- tratype[[i]]
        data.TT[,2] <- as.numeric(data.TT[,2])
        data.TT[,3] <- as.numeric(data.TT[,3])
        ## *** The fill color for track ***
        data.TT$num <- 1:nrow(data.TT)
        if(tktype!="rect-discrete" && tktype!="rect-gradual" && tktype!="heatmap-gradual" && tktype!="heatmap-discrete"  && tktype!="ideogram"){
          data.TTC <- NULL
          coltypeTrack <- tra_coltype[[i]]
          if(coltypeTrack == 2){
            tkcolor <- tra_colcol[[i]]
            tkcolor <- gsub("\\s","",strsplit(tkcolor,",")[[1]])
            tkcolor <- gsub('\\"',"",tkcolor)
            tkcolor <- gsub("0x","#", tkcolor)
          }else if((coltypeTrack==3 && ("color" %in% colnames(data.TT))) | (coltypeTrack==3 && ncol(data.T[[i]])==4 && colnames(data.TT)[4]=="stack")){
            tkcolor <- tra_colorcus[[i]]
            tkcolor <- unlist(strsplit(tkcolor,";"))
            tkcolor <- data.frame(id=tkcolor,stringsAsFactors=F)
            tkcolor$group <- gsub("\\:.*","",tkcolor$id)
            tkcolor$cols <- gsub(".*\\:","",tkcolor$id)
            tkcolor$group <- gsub(" ","",tkcolor$group)
            tkcolor$cols <- gsub(" ","",tkcolor$cols)
            colname <- colnames(data.TT)
            if("color" %in% colnames(data.TT)){
              data.TTC <- merge(data.TT,tkcolor,by.x="color",by.y="group",all.x=T)
            }else if(colnames(data.TT)[4]=="stack"){
              data.TTC <- merge(data.TT,tkcolor,by.x="stack",by.y="group",all.x=T)
            }
            data.TTC <- data.TTC[c(colname,"cols")]
            data.TTC$cols[is.na(data.TTC$cols)] <- "grey"
            tkcolor <- unique(data.TTC$cols)
            data.TT <- data.TT[,1:4]
          }else if(coltypeTrack==1 && ("color" %in% colnames(data.TT))){
            selcols <- c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", "navy", "seagreen", "maroon", "burlywood3", "magenta2")
            tkcolor <- sample(selcols,length(unique(data.TT$color)))
            tkcolor <- data.frame(group=unique(data.TT$color),cols=tkcolor,stringsAsFactors=F)
            colname <- colnames(data.TT)
            data.TTC <- merge(data.TT,tkcolor,by.x="color",by.y="group",all.x=T)
            data.TTC <- data.TTC[c(colname,"cols")]
            data.TTC$cols[is.na(data.TTC$cols)] <- "grey"
            tkcolor <- unique(data.TTC$cols)
            data.TT <- data.TT[,1:4]
          }else if(coltypeTrack==1 && ncol(data.T[[i]])==4 && colnames(data.TT)[4]=="stack"){
            selcols <- c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", "navy", "seagreen", "maroon", "burlywood3", "magenta2")
            tkcolor <- sample(selcols,length(unique(data.TT$stack)))
            tkcolor <- data.frame(group=unique(data.TT$stack),cols=tkcolor,stringsAsFactors=F)
            colname <- colnames(data.TT)
            data.TTC <- merge(data.TT,tkcolor,by.x="stack",by.y="group",all.x=T)
            data.TTC <- data.TTC[c(colname,"cols")]
            data.TTC$cols[is.na(data.TTC$cols)] <- "grey"
            tkcolor <- unique(data.TTC$cols)
            data.TT <- data.TT[,1:4]
          }else{
            selcols <- c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", "navy", "seagreen", "maroon", "burlywood3", "magenta2")
            tkcolor <- sample(selcols,ncol(data.T[[i]])-3)
          }
          if(!is.null(data.TTC)){
            data.TTC <- data.TTC[order(data.TTC$num),]
            rownames(data.TTC) <- NULL
            data.TTC$num <- NULL
          }
          data.TT$num <- NULL
          if(c(ncol(data.TT)==5 | ncol(data.TT)==6 | ncol(data.TT)==7) && ("color" %in% colnames(data.TT))){
            data.TT <- data.TT[,1:4]
          }else if(c(ncol(data.TT)==5 | ncol(data.TT)==6 | ncol(data.TT)==7) && ("pch" %in% colnames(data.TT)) && !("color" %in% colnames(data.TT))){
            data.TT <- data.TT[,1:4]
            tkcolor <- tkcolor[1]
          }
        }
        ## *** The backgroud color for track ***
        tkbgcol <- tra_bgcol[[i]]
        if(!is.null(tkbgcol)){
          tkbgcol <- gsub("\\s","",strsplit(tkbgcol,",")[[1]])
          tkbgcol <- gsub('\\"',"",tkbgcol)
          tkbgcol <- gsub("0x","#", tkbgcol)
          repnumcol <- round(length(unique(data.C[,1]))/length(tkbgcol))+1
          tkbgcol <- rep(tkbgcol, repnumcol)[1:length(unique(data.C[,1]))]
        }
        ## *** The track margin ***
        tkmargin <- tra_Margin[[i]]
        tkmargin <- as.numeric(tkmargin)
        ## *** The track height ***
        tkheight <- heightTra[[i]]
        if(!is.null(tkheight)){
          tkheight <- as.numeric(tkheight)
        }
        ## *** The y coordinates of baselines ***
        tklinecoord <- tra_baseline[[i]]
        if(!is.null(tklinecoord)){
          tklinecoord <- as.numeric(unlist(strsplit(tklinecoord,",")))
        }else{
          tklinecoord <- c(0.25,0.75)
        }
        ## *** The baselines color ***
        tklinecolor <- tra_colorline[[i]]
        if(!is.null(tklinecolor)){
          tklinecolor <<- gsub('\\"',"",tklinecolor)
          tklinecolor <<- gsub("0x","#", tklinecolor)
          tklinecolor <- unlist(strsplit(tklinecolor,","))
          tklinecolor <- rep(tklinecolor, length(tklinecoord))[1:length(tklinecoord)]
        }
        if(ncol(data.T[[i]])==4 && colnames(data.T[[i]])[4]=="stack"){
          tklinecol <<- gsub('\\"',"",tklinecolor)
          tklinecol <<- gsub("0x","#", tklinecol)
          tklinecol <- unlist(strsplit(tklinecol,","))
          tklinecol <- rep(tklinecol, length(unique(data.T[[i]][,4])))[1:length(unique(data.T[[i]][,4]))]
        }
        ## *** Add connection ***
        if(!is.null(tra_hmap_poslines[[i]])){
          if(tra_hmap_poslines[[i]] == "1"){
            heightlines <<- tra_hmap_poslinhei[[i]]
          }
        }
        ## *** Add border ***
        tkborder <- tra_border[[i]]
        
        if(!is.null(tra_hmap_cellbord[[i]])){
          if(tra_hmap_cellbord[[i]] == "add"){
            tkbordercol <<- tra_hmap_cellbord_col[[i]]
          }else{
            tkbordercol <<- NA
          }
        }
        
        if(!is.null(tra_hmap_heatmapcol[[i]])){
          if(tra_hmap_heatmapcol[[i]] == 1){ #heatmap color type, 1 is "typical"
            hmapcols <<- gsub('\\"',"",tra_hmap_typcolhmap[[i]])
          }else{
            hmapcols <<- heatmapcols[[i]]
          }
          hmapcols <<- unlist(strsplit(hmapcols,"\\."))
        }
        #
        ##
        ## *** The bar direction ***
        tkbardir <- tra_bar_direction[[i]]
        if(!is.null(tkbardir)){
          if(tkbardir==2){
            tkbarvalue <- tra_bar_Boundary[[i]]
            tkbarcol1 <- tra_bar_coldir1[[i]]
            tkbarcol2 <- tra_bar_coldir2[[i]]
            tktransparency <- tra_transparency[[i]]
            tkbarcol1 <- adjustcolor(tkbarcol1, alpha.f = tktransparency)
            tkbarcol2 <- adjustcolor(tkbarcol2, alpha.f = tktransparency)
          }
        }
        
        if(tktype == "heatmap-discrete"){
          tra_heatcol_dis_tr <- tra_heatcol_dis[[i]]
          #tra_heatcol_dis,tra_heat_heatcoldiscus
          
          data.TT_hat_dis <- data.TT
          for(li in 4:length(data.TT)){
            data.TT_hat_dis[,li] <- as.numeric(as.factor(data.TT[,li]))
          }
          if(tra_heatcol_dis_tr == 2){
            heat_dis_cols <- tra_heat_heatcoldiscus[[i]]
            heat_dis_cols <- unlist(strsplit(heat_dis_cols,";"))
            heat_dis_cols <- data.frame(id=heat_dis_cols,stringsAsFactors=F)
            heat_dis_cols$group <- gsub("\\:.*","",heat_dis_cols$id)
            heat_dis_cols$cols <- gsub(".*\\:","",heat_dis_cols$id)
            heat_dis_cols$group <- gsub(" ","",heat_dis_cols$group)
            heat_dis_cols$cols <- gsub(" ","",heat_dis_cols$cols)
            heat_dis_cols[,2] <- as.numeric(as.factor(heat_dis_cols[,2]))
            data_TT_col <- apply(data.TT_hat_dis[,4:length(data.TT_hat_dis)], MARGIN = c(1,2), function(x){
              if(length(which(heat_dis_cols[,2] == x)) != 0){
                heat_dis_cols[which(heat_dis_cols[,2] == x),3]
              }else{
                "#FFFFFF"
              }
            })
          }else{
            cols <- c(brewer.pal(11,'Set3'),brewer.pal(9,'Set1')[c(-1,-3,-6)],brewer.pal(8,'Dark2'),"chartreuse","aquamarine","cornflowerblue","blue","cyan","bisque1","darkorchid2","firebrick1","gold1","magenta1","olivedrab1","navy","maroon1","tan","yellow3","black","bisque4","seagreen3","plum2","yellow1","springgreen","slateblue1","lightsteelblue1","lightseagreen","limegreen")
            selcol <- sample(cols,length(unique(unlist(data.TT_hat_dis[,4:length(data.TT_hat_dis)]))),replace = TRUE)
            data_ht_col <- data.TT_hat_dis
            for(ki in 4:length(data.TT_hat_dis)){
              data_ht_col[,ki] <- selcol[data.TT_hat_dis[,ki]]
            }
            data_TT_col <- data_ht_col[,4:length(data_ht_col)]
          }
        }
        if(tktype == "rect-gradual" | tktype == "rect-discrete"){
          ## *** The data color ***
          tkrectcol <-  tktype
          ## *** Select color ***
          selrectcol <- tra_rect_rectcol[[i]]
          if(tkrectcol=="rect-gradual"){
            rectcol <- tra_trct_colrect[[i]]
            if(rectcol=="blue"){
              rectcols <<- c("#EDEDFD","#6969F5","#00008B")
            }else if(rectcol=="red"){
              rectcols <<- c("#FDEDED","#F56969","#8B0000")
            }else if(rectcol=="green"){
              rectcols <<- c("#EDFBED","#69E169","#008B00")
            }else if(rectcol=="cyan"){
              rectcols <<- c("#EDFBFB","#69E1E1","#008B8B")
            }else if(rectcol=="purple"){
              rectcols <<- c("#F6F0FB","#B27FE1","#551A8B")
            }else if(rectcol=="pink"){
              rectcols <<- c("#FBEEF5","#E172AE","#8B1076")
            }else if(rectcol=="orange"){
              rectcols <<- c("#FDF5ED","#F5AE69","#8B4500")
            }else if(rectcol=="yellow"){
              rectcols <<- c("#FDFDED","#EFEF1A","#8B8B00")
            }else if(rectcol=="navy"){
              rectcols <<- c("#EDEDF6","#7272B8","#000080")
            }else if(rectcol=="seagreen"){
              rectcols <<- c("#F2FBF6","#4EEE94","#2E8B57")
            }else if(rectcol=="maroon"){
              rectcols <<- c("#FFF4FB","#FF69C7","#8B1C62")
            }else if(rectcol=="olivedrab"){
              rectcols <<- c("#FBFFF4","#C6FF52","#698B22")
            }else if(rectcol=="gold"){
              rectcols <<- c("#FFFCF1","#FFDD28","#8B7500")
            }else if(rectcol=="lightblue"){
              rectcols <<- c("#EFF5F7","#AFCDD7","#68838B")
            }else if(rectcol=="navy.yellow"){
              rectcols <<- c("#000080","#7B7B41","#FFFF00")
            }else if(rectcol=="purple.seagreen"){
              rectcols <<- c("#551A8B","#548994","#54FF9F")
            }else if(rectcol=="navy.orange"){
              rectcols <<- c("#000080","#7B5041","#FFA500")
            }else if(rectcol=="navy.cyan"){
              rectcols <<- c("#000080","#007BBD","#00FFFF")
            }else if(rectcol=="blue.red"){
              rectcols <<- c("#0000FF","#730083","#EE0000")
            }else if(rectcol=="green.red"){
              rectcols <<- c("#00EE00","#757800","#EE0000")
            }
          }else if(tkrectcol== "rect-discrete" && selrectcol==2){
            rectcols <- tra_rect_rectcoldis[[i]]
            data.TT[,4] <- rectcols
          }else if(tkrectcol== "rect-discrete" && selrectcol==3){
            rectcols <- tra_rect_rectcoldiscus[[i]]
            rectcols <- unlist(strsplit(rectcols,";"))
            rectcols <- data.frame(id=rectcols,stringsAsFactors=F)
            rectcols$group <- gsub("\\:.*","",rectcols$id)
            rectcols$cols <- gsub(".*\\:","",rectcols$id)
            rectcols$group <- gsub(" ","",rectcols$group)
            rectcols$cols <- gsub(" ","",rectcols$cols)
            colname <- colnames(data.TT)[1:3]
            data.TT <- merge(data.TT,rectcols,by.x=colnames(data.TT)[4],by.y="group",all.x=T)
            data.TT <- data.TT[c(colname,"cols")]
          }
        }
        ## *** The transparency of color ***
        
        tktransparency <- tra_transparency[[i]]
        if((tktype!="rect-gradual" && tktype!="rect-discrete" && tktype!="heatmap-gradual" && tktype!="heatmap-discrete" && tktype!="ideogram") | (tktype=="line" && tra_line_fillarea[[i]]!="add")){
          tkcolor <- adjustcolor(tkcolor, alpha.f = tktransparency)
        }
        # data.TTT <- data.T[[i]]
        # data.TTT$id <- paste(data.TTT[,1],data.TTT[,2],data.TTT[,3],sep="")
        # data.TTT$num <- 1:nrow(data.TTT)
        ## *** The track border
        if(!is.null(tkborder)){
          if(tkborder=="add"){
            tkborder <- "grey"
          }else{
            tkborder <- NA
          }
        }
        ## *** The symbol type & point size***
        if(tktype == "point" & !("pch" %in% colnames(data.TT))){
          if(!is.null(tra_poipch[[i]])){
            symboltype <- tra_poipch[[i]]
            if(!is.null(symboltype)){
              symboltype <- as.numeric(unlist(strsplit(symboltype,",")))
              symboltype <- rep(symboltype, length(unique(data.T[[i]][,4])))[1:length(unique(data.T[[i]][,4]))]
            }
          }
        }
        if(tktype == "point" & !("cex" %in% colnames(data.TT))){
          if(!is.null(tra_poi_poisiz[[i]])){
            pointsize <- as.numeric(tra_poi_poisiz[[i]])
          }
        }
        columns <- c(1:ncol(data.TT))[-c(1:3)]
        if(tktype == "line"){
          ## *** Fill the area ***
          if(tra_line_fillarea[[i]]!="add"){
            area <- FALSE
            borderset <- NA
            lwdnum <- 1
          }else if(tra_line_fillarea[[i]]=="add" && tra_line_selrea[[i]]==1){
            area <- TRUE
            lwdnum <- 0.2
          }else if(tra_line_fillarea[[i]]=="add" && tra_line_selrea[[i]]==2){
            area <- TRUE
            borderset <- NA
            if(nchar(tra_bar_borderarea[[i]])!=0){
              borderset <- adjustcolor(tra_bar_borderarea[[i]],alpha.f = tktransparency)
            }
            lwdnum <- 0.2
          }
          if(lab_inf[i]){
            if(labels_inf[[2]]=="outside"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = lab_height/4, labels_height = (lab_height/4)*3 , cex =labelsize , line_col = labels_inf[[4]] , col = labels_inf[[4]] , padding = 0 , track.margin = c(0,0), side = "outside")
            }
          }
          if(ncol(data.T[[i]])==4 && colnames(data.T[[i]])[4]=="stack"){
            bed_list <- lapply(unique(data.T[[i]][,4]),function(x){
              if(coltypeTrack==2){
                data.TT[data.TT[,4] %in% x,1:3]
              }else{
                data.TTC[data.TTC[,4] %in% x,1:3]
              }
            })
            circos.genomicTrackPlotRegion(bed_list, stack = TRUE, track.height = tkheight, track.margin = c(tkmargin,0),bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region, value, ...){
              ii = getI(...)
              if(coltypeTrack==1){
                circos.genomicLines(region, value, col=tkcolor[ii], lty=1, ...)
              }else if(coltypeTrack==2){
                circos.genomicLines(region, value, col=tkcolor[1], lty=1, ...)
              }else if(coltypeTrack==3){
                circos.genomicLines(region, value, col=tkcolor[ii], lty=1, ...)
              }
            })
          }else{
            data.TT[,ncol(data.TT)] <- as.numeric(data.TT[,ncol(data.TT)])
            circos.genomicTrackPlotRegion(data.TT, track.height = tkheight, track.margin = c(tkmargin,0) , bg.col = tkbgcol , bg.border = tkborder, panel.fun = function(region,value,...){
              if(nchar(tklinecolor[1])!=0){
                xlim <- get.cell.meta.data("xlim")
                ylim <- get.cell.meta.data("ylim")
                for(k in 1:length(tklinecoord)){
                  y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                  circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                }
              }
              if((coltypeTrack==1 && !("color" %in% colnames(data.T[[i]]))) | coltypeTrack==2){
                if(length(columns)==1){
                  tkcolor <- tkcolor[1]
                }else{
                  tkcolor <- c(tkcolor,rep("grey",length(columns)))
                  tkcolor <- tkcolor[1:length(columns)]
                }
                if(tra_line_selrea[[i]]==1 | tra_line_fillarea[[i]]!="add"){
                  borderset <- adjustcolor(tkcolor,alpha.f = tktransparency)
                }
                circos.genomicLines(region, value, numeric.column=columns-3, col=borderset, area=area, border=tkcolor, lwd=lwdnum, lty=1, ...)
              }
            })
            takindx <- get.current.track.index()
            if(coltypeTrack==3 && ncol(data.TTC)>=6 && ("cols" %in% colnames(data.TTC))){
              data.TTC$id <- paste(data.TTC[,1],data.TTC[,2],data.TTC[,3],sep="")
              data.TTC$num <- 1:nrow(data.TTC)
              lapply(unique(data.TTC[,1]),function(x){
                circos.updatePlotRegion(sector.index = x, track.index=takindx , bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
                if(nchar(tklinecolor[1])!=0){
                  xlim <- get.cell.meta.data("xlim")
                  ylim <- get.cell.meta.data("ylim")
                  for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                  }
                }
                dat <- data.TTC[data.TTC[,1] %in% x,]
                lapply(unique(dat$cols),function(m){
                  datt <- dat[dat$cols %in% m,]
                  ind <- which(data.TTC$id %in% datt$id)
                  datt.fil <- na.omit(unique(data.TTC[ind,]))
                  datt.fil <- datt.fil[datt.fil[,1] %in% x,]
                  rownum <- datt.fil$num
                  rownumdif <- diff(rownum)
                  indx <- which(rownumdif != 1)
                  indx1 <- c(0,indx)
                  rownumdif1 <- c(1,rownumdif)
                  if(length(indx)==0){
                    rownumdif1 <- 1
                  }else{
                    for(k in 1:length(which(rownumdif!=1))){
                      rownumdif1[(indx1[k]+1):indx[k]] <- k
                      if(k==length(which(rownumdif!=1))){
                        rownumdif1[(indx[k]+1):length(rownumdif1)] <- k+1
                      }
                    }
                  }
                  datt.fil$indx <- rownumdif1
                  lapply(unique(rownumdif1),function(h){
                    datt.fill <- datt.fil[datt.fil$indx == h,]
                    minnum <- min(datt.fill$num)
                    if(sum(dat$num==(minnum-1)) != 0){
                      datt.fill <- rbind(datt.fill,as.character(c(dat[dat$num==(minnum-1),],h)))
                    }
                    
                    datt.fill[,2] <- as.numeric(datt.fill[,2])
                    datt.fill[,3] <- as.numeric(datt.fill[,3])
                    datt.fill[,4] <- as.numeric(datt.fill[,4])
                    datt.fill$indx <- as.numeric(datt.fill$indx)
                    datt.fill <- datt.fill[!is.na(datt.fill[,2]),]
                    datt.fill <- datt.fill[order(datt.fill[,2],datt.fill[,3]),]
                    if(tra_line_selrea[[i]]==1 | tra_line_fillarea[[i]]!="add"){
                      borderset <- adjustcolor(m,alpha.f = tktransparency)
                    }
                    circos.lines((datt.fill[,2]+datt.fill[,3])/2,datt.fill[,4], col=borderset, area=area, border=m, lwd=lwdnum, lty=1)
                  })
                })
              })
            }else if(coltypeTrack==1 && ("color" %in% colnames(data.T[[i]])) && ("cols" %in% colnames(data.TTC))){
              data.TTC$id <- paste(data.TTC[,1],data.TTC[,2],data.TTC[,3],sep="")
              data.TTC$num <- 1:nrow(data.TTC)
              lapply(unique(data.TTC[,1]),function(x){
                circos.updatePlotRegion(sector.index = x, track.index = takindx, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
                if(nchar(tklinecolor[1])!=0){
                  xlim <- get.cell.meta.data("xlim")
                  ylim <- get.cell.meta.data("ylim")
                  for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                  }
                }
                dat <- data.TTC[data.TTC[,1] %in% x,]
                lapply(unique(dat$cols),function(m){
                  datt <- dat[dat$cols %in% m,]
                  ind <- which(data.TTC$id %in% datt$id)
                  datt.fil <- na.omit(unique(data.TTC[ind,]))
                  datt.fil <- datt.fil[datt.fil[,1] %in% x,]
                  rownum <- datt.fil$num
                  rownumdif <- diff(rownum)
                  indx <- which(rownumdif != 1)
                  indx1 <- c(0,indx)
                  rownumdif1 <- c(1,rownumdif)
                  if(length(indx)==0){
                    rownumdif1 <- 1
                  }else{
                    for(k in 1:length(which(rownumdif!=1))){
                      rownumdif1[(indx1[k]+1):indx[k]] <- k
                      if(k==length(which(rownumdif!=1))){
                        rownumdif1[(indx[k]+1):length(rownumdif1)] <- k+1
                      }
                    }
                  }
                  datt.fil$indx <- rownumdif1
                  lapply(unique(rownumdif1),function(h){
                    datt.fill <- datt.fil[datt.fil$indx == h,]
                    minnum <- min(datt.fill$num)
                    if(sum(dat$num==(minnum-1)) != 0){
                      
                      datt.fill <- rbind(datt.fill,as.character(c(dat[dat$num==(minnum-1),],h)))
                      
                    }
                    
                    datt.fill[,2] <- as.numeric(datt.fill[,2])
                    datt.fill[,3] <- as.numeric(datt.fill[,3])
                    datt.fill[,4] <- as.numeric(datt.fill[,4])
                    datt.fill$indx <- as.numeric(datt.fill$indx)
                    datt.fill <- datt.fill[!is.na(datt.fill[,2]),]
                    datt.fill <- datt.fill[order(datt.fill[,2],datt.fill[,3]),]
                    if(tra_line_selrea[[i]]==1 | tra_line_fillarea[[i]]!="add"){
                      borderset <- adjustcolor(m,alpha.f = tktransparency)
                    }
                    circos.lines((datt.fill[,2]+datt.fill[,3])/2,datt.fill[,4], col=borderset, area=area, border=m, lwd=lwdnum, lty=1)
                  })
                })
              })
            }
            if(tra_yaxis[[i]]==1){
              circos.yaxis(
                side = "left",
                tick = TRUE,
                at = c(min(data.TT_old[,4]),max(data.TT_old[,4])),
                #at = c(as.numeric(sprintf("%0.2f",min(data.TT_old[,4]))),as.numeric(sprintf("%0.2f",max(data.TT_old[,4])))),
                sector.index = get.all.sector.index()[1],
                labels.cex = 0.5
              )
            }
          }
          
          
          
          
          
          
          
          if(lab_inf[i]){
            if(labels_inf[[2]]=="inside"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = lab_height/4, labels_height = (lab_height/4)*3 , cex =labelsize  , line_col = labels_inf[[4]] , col = labels_inf[[4]] , padding = 0 , track.margin = c(0,0), side = "inside")
            }
          }
        }else if(tktype == "point"){
          if(lab_inf[i]){
            if(labels_inf[[2]]=="outside"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = lab_height/4, labels_height = (lab_height/4)*3 , cex =labelsize  , line_col = labels_inf[[4]] , col = labels_inf[[4]] , padding = 0 , track.margin = c(0,0), side = "outside")
            }
          }
          if(ncol(data.T[[i]])==4 && colnames(data.T[[i]])[4]=="stack"){
            bed_list <- lapply(unique(data.T[[i]][,4]),function(x){
              if(coltypeTrack==2){
                data.TT[data.TT[,4] %in% x,1:3]
              }else{
                data.TTC[data.TTC[,4] %in% x,1:3]
              }
            })
            circos.genomicTrackPlotRegion(bed_list, stack = TRUE, track.height = tkheight, track.margin = c(tkmargin,0), bg.col = tkbgcol , bg.border = tkborder, panel.fun = function(region, value, ...){
              ii = getI(...)
              if(coltypeTrack==1){
                circos.lines(CELL_META$cell.xlim, c(ii, ii), lty = 2, col = tklinecol[ii])
                circos.genomicPoints(region, value, pch = symboltype[ii], cex = pointsize, col = tkcolor[ii],...)
              }else if(coltypeTrack==2){
                circos.lines(CELL_META$cell.xlim, c(ii, ii), lty = 2, col = tklinecol[ii])
                circos.genomicPoints(region, value, pch = symboltype[ii], cex = pointsize, col = tkcolor[1],...)
              }else if(coltypeTrack==3){
                circos.lines(CELL_META$cell.xlim, c(ii, ii), lty = 2, col = tklinecol[ii]) #  "#808080" "#808080"
                circos.genomicPoints(region, value, pch = symboltype[ii], cex = pointsize, col = tkcolor[ii],...)
              }
            })
          }else{
            data.TT[,ncol(data.TT)] <- as.numeric(data.TT[,ncol(data.TT)])
            circos.genomicTrack(data.TT , track.height = tkheight , track.margin = c(tkmargin,0) , bg.col = tkbgcol , bg.border = tkborder , panel.fun = function(region, value, ...) {
              if(nchar(tklinecolor[1])!=0){
                xlim <- get.cell.meta.data("xlim")
                ylim <- get.cell.meta.data("ylim")
                for(k in 1:length(tklinecoord)){
                  y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                  circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                }
              }
              if(!("cex" %in% colnames(data.T[[i]])) && !("pch" %in% colnames(data.T[[i]])) && ((coltypeTrack==1 && !("color" %in% colnames(data.T[[i]]))) | coltypeTrack==2)){
                if(length(columns)==1){
                  tkcolor <- tkcolor[1]
                }else{
                  tkcolor <- c(tkcolor,rep("grey",length(columns)))
                  tkcolor <- tkcolor[1:length(columns)]
                }
                circos.genomicPoints(region, value, numeric.column=columns-3, col= tkcolor, cex= pointsize, pch= symboltype[1], ...)
              }
            })
            takindx <- get.current.track.index()
            if(!("cex" %in% colnames(data.T[[i]]))){
              if(coltypeTrack==3 && ("pch" %in% colnames(data.TTC)) && ("cols" %in% colnames(data.TTC))){
                lapply(unique(data.TTC[,1]),function(x){
                  circos.updatePlotRegion(sector.index = x, track.index = takindx , bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
                  if(nchar(tklinecolor[1])!=0){
                    xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
                    for(k in 1:length(tklinecoord)){
                      y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                    }
                  }
                  dat <- data.TTC[data.TTC[,1] %in% x,]
                  circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(dat$cols,alpha.f = tktransparency), cex=0.6, pch=dat$pch)
                })
              }else if(coltypeTrack==3 && ncol(data.TTC)>=6 && !("pch" %in% colnames(data.TTC)) && ("cols" %in% colnames(data.TTC))){
                lapply(unique(data.TTC[,1]),function(x){
                  circos.updatePlotRegion(sector.index = x, track.index=takindx, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
                  if(nchar(tklinecolor[1])!=0){
                    xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
                    for(k in 1:length(tklinecoord)){
                      y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                    }
                  }
                  dat <- data.TTC[data.TTC[,1] %in% x,]
                  circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(dat$cols,alpha.f = tktransparency), cex=0.6, pch=16)
                })
              }else if(coltypeTrack!=3 && ("pch" %in% colnames(data.T[[i]])) && ("color" %in% colnames(data.T[[i]]))){
                lapply(unique(data.T[[i]][,1]),function(x){
                  circos.updatePlotRegion(sector.index = x, track.index=takindx, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
                  if(nchar(tklinecolor[1])!=0){
                    xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
                    for(k in 1:length(tklinecoord)){
                      y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                    }
                  }
                  dat <- data.T[[i]][data.T[[i]][,1] %in% x,]
                  if(coltypeTrack==1){
                    tkcols <- data.TTC$cols[data.TTC[,1] %in% x]
                    circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(tkcols,alpha.f = tktransparency), cex=0.6, pch=dat$pch)
                  }else{
                    circos.points((dat[,2]+dat[,3])/2,dat[,4], col=tkcolor[1], cex=0.6, pch=dat$pch)
                  }
                })
              }else if(("pch" %in% colnames(data.T[[i]])) && !("color" %in% colnames(data.T[[i]]))){
                lapply(unique(data.T[[i]][,1]),function(x){
                  
                  circos.updatePlotRegion(sector.index = x, track.index=takindx, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
                  
                  if(nchar(tklinecolor[1])!=0){
                    xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
                    for(k in 1:length(tklinecoord)){
                      y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                    }
                  }
                  dat <- data.T[[i]][data.T[[i]][,1] %in% x,]
                  circos.points((dat[,2]+dat[,3])/2,dat[,4], col=tkcolor, cex=0.6, pch=dat$pch)
                })
              }else if(coltypeTrack==1 && ("color" %in% colnames(data.T[[i]])) && !("pch" %in% colnames(data.TTC)) && ("cols" %in% colnames(data.TTC))){
                lapply(unique(data.TTC[,1]),function(x){
                  
                  circos.updatePlotRegion(sector.index = x, track.index=takindx, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
                  
                  if(nchar(tklinecolor[1])!=0){
                    xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
                    for(k in 1:length(tklinecoord)){
                      y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                    }
                  }
                  dat <- data.TTC[data.TTC[,1] %in% x,]
                  circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(dat$cols,alpha.f = tktransparency), cex=0.6, pch=16)
                })
              }
            }else if("cex" %in% colnames(data.T[[i]])){
              
              if(coltypeTrack==3 && ("pch" %in% colnames(data.TTC)) && ("cols" %in% colnames(data.TTC))){
                lapply(unique(data.TTC[,1]),function(x){
                  
                  circos.updatePlotRegion(sector.index = x, track.index=takindx, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
                  
                  if(nchar(tklinecolor[1])!=0){
                    xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
                    for(k in 1:length(tklinecoord)){
                      y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                    }
                  }
                  dat <- data.TTC[data.TTC[,1] %in% x,]
                  circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(dat$cols,alpha.f = tktransparency), cex=dat$cex, pch=dat$pch)
                })
              }else if(coltypeTrack==3 && ncol(data.TTC)>=6 && !("pch" %in% colnames(data.TTC)) && ("cols" %in% colnames(data.TTC))){
                lapply(unique(data.TTC[,1]),function(x){
                  
                  circos.updatePlotRegion(sector.index = x, track.index=takindx, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
                  
                  if(nchar(tklinecolor[1])!=0){
                    xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
                    for(k in 1:length(tklinecoord)){
                      y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                    }
                  }
                  dat <- data.TTC[data.TTC[,1] %in% x,]
                  circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(dat$cols,alpha.f = tktransparency), cex=dat$cex, pch=16)
                })
              }else if(coltypeTrack!=3 && ("pch" %in% colnames(data.T[[i]])) && ("color" %in% colnames(data.T[[i]]))){
                lapply(unique(data.T[[i]][,1]),function(x){
                  
                  circos.updatePlotRegion(sector.index = x, track.index=takindx, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
                  
                  if(nchar(tklinecolor[1])!=0){
                    xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
                    for(k in 1:length(tklinecoord)){
                      y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                    }
                  }
                  dat <- data.T[[i]][data.T[[i]][,1] %in% x,]
                  if(coltypeTrack==1){
                    tkcols <- data.TTC$cols[data.TTC[,1] %in% x]
                    circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(tkcols,alpha.f = tktransparency), cex=dat$cex, pch=dat$pch)
                  }else{
                    circos.points((dat[,2]+dat[,3])/2,dat[,4], col=tkcolor[1], cex=dat$cex, pch=dat$pch)
                  }
                })
              }else if(("pch" %in% colnames(data.T[[i]])) && !("color" %in% colnames(data.T[[i]]))){
                lapply(unique(data.T[[i]][,1]),function(x){
                  
                  circos.updatePlotRegion(sector.index = x, track.index=takindx, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
                  
                  if(nchar(tklinecolor[1])!=0){
                    xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
                    for(k in 1:length(tklinecoord)){
                      y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                    }
                  }
                  dat <- data.T[[i]][data.T[[i]][,1] %in% x,]
                  circos.points((dat[,2]+dat[,3])/2,dat[,4], col=tkcolor, cex=dat$cex, pch=dat$pch)
                })
              }else if(coltypeTrack==1 && ("color" %in% colnames(data.T[[i]])) && !("pch" %in% colnames(data.TTC)) && ("cols" %in% colnames(data.TTC))){
                lapply(unique(data.TTC[,1]),function(x){
                  
                  circos.updatePlotRegion(sector.index = x, track.index=takindx, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
                  
                  if(nchar(tklinecolor[1])!=0){
                    xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
                    for(k in 1:length(tklinecoord)){
                      y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                    }
                  }
                  dat <- data.TTC[data.TTC[,1] %in% x,]
                  circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(dat$cols,alpha.f = tktransparency), cex=dat$cex, pch=16)
                })
              }else if(!("color" %in% colnames(data.T[[i]])) && !("pch" %in% colnames(data.TTC))){
                lapply(unique(data.T[[i]][,1]),function(x){
                  
                  circos.updatePlotRegion(sector.index = x, track.index=takindx, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
                  
                  if(nchar(tklinecolor[1])!=0){
                    xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
                    for(k in 1:length(tklinecoord)){
                      y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                    }
                  }
                  dat <- data.T[[i]][data.T[[i]][,1] %in% x,]
                  circos.points((dat[,2]+dat[,3])/2,dat[,4], col=tkcolor[1], cex=dat$cex, pch=16)
                })
              }else if(coltypeTrack==2 && ("color" %in% colnames(data.T[[i]])) && !("pch" %in% colnames(data.T[[i]]))){
                lapply(data.T[[i]],function(x){
                  
                  circos.updatePlotRegion(sector.index = x, track.index=takindx, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
                  
                  if(nchar(tklinecolor[1])!=0){
                    xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
                    for(k in 1:length(tklinecoord)){
                      y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                    }
                  }
                  dat <- data.T[[i]][data.T[[i]][,1] %in% x,]
                  circos.points((dat[,2]+dat[,3])/2,dat[,4], col=tkcolor[1], cex=dat$cex, pch=16)
                })
              }
            }
            if(tra_yaxis[[i]]==1){
              circos.yaxis(
                side = "left",
                tick = TRUE,
                at = c(min(data.TT_old[,4]),max(data.TT_old[,4])),
                #at = c(as.numeric(sprintf("%0.2f",min(data_t[,4]))),as.numeric(sprintf("%0.2f",max(data_t[,4])))),
                sector.index = get.all.sector.index()[1],
                labels.cex = 0.5
              )
            }
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
          }
          if(lab_inf[i]){
            if(labels_inf[[2]]=="inside"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = lab_height/4, labels_height = (lab_height/4)*3 , cex =labelsize  , line_col = labels_inf[[4]] , col = labels_inf[[4]] , padding = 0 , track.margin = c(0,0), side = "inside")
            }
          }
        }else if(tktype == "rect-discrete" | tktype == "rect-gradual"){
          if(lab_inf[i]){
            if(labels_inf[[2]]=="outside"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = lab_height/4, labels_height = (lab_height/4)*3 , cex =labelsize  , line_col = labels_inf[[4]] , col = labels_inf[[4]] , padding = 0 , track.margin = c(0,0), side = "outside")
            }
          }
          if(tkrectcol== "rect-discrete"){
            if(selrectcol==1){
              data.TT[,4] <- as.numeric(as.factor(data.TT[,4]))
              cols <- c(brewer.pal(11,'Set3'),brewer.pal(9,'Set1')[c(-1,-3,-6)],brewer.pal(8,'Dark2'),"chartreuse","aquamarine","cornflowerblue","blue","cyan","bisque1","darkorchid2","firebrick1","gold1","magenta1","olivedrab1","navy","maroon1","tan","yellow3","black","bisque4","seagreen3","plum2","yellow1","springgreen","slateblue1","lightsteelblue1","lightseagreen","limegreen")
              selcol <- sample(cols,length(unique(data.TT[,4])),replace = TRUE)
              #tkcolor.export[[i]] <<- selcol
              data.TT[,4] <- selcol[data.TT[,4]]
              circos.genomicTrackPlotRegion(data.TT, ylim=c(0,1),track.height = tkheight, track.margin = c(tkmargin,0), bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
                circos.genomicRect(region, value, col=adjustcolor(value[[1]],alpha.f = tktransparency), border = NA, ...)
              })
            }else{
              circos.genomicTrackPlotRegion(data.TT, ylim=c(0,1),track.height = tkheight, track.margin = c(tkmargin,0), bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
                circos.genomicRect(region, value, col=adjustcolor(value[[1]],alpha.f = tktransparency), border = NA, ...)
              })
            }
          }else{
            f <- colorRamp2(breaks = c(min(data.TT[,4]), mean(data.TT[,4]), max(data.TT[,4])), colors = rectcols)
            circos.genomicTrackPlotRegion(data.TT, ylim=c(0,1),track.height = tkheight, track.margin = c(tkmargin,0), bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
              circos.genomicRect(region, value, col=adjustcolor(f(value[[1]]),alpha.f = tktransparency), border = NA, ...)
            })
          }
          if(lab_inf[i]){
            if(labels_inf[[2]]=="inside"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = lab_height/4, labels_height = (lab_height/4)*3 , cex =labelsize  , line_col = labels_inf[[4]] , col = labels_inf[[4]] , padding = 0 , track.margin = c(0,0), side = "inside")
            }
          }
        }else if(tktype == "heatmap-gradual"){
          data.TT$num <- NULL
          if(lab_inf[i]){
            if(labels_inf[[2]]=="outside"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = lab_height/4, labels_height = (lab_height/4)*3 , cex =labelsize  , line_col = labels_inf[[4]] , col = labels_inf[[4]] , padding = 0 , track.margin = c(0,0), side = "outside")
            }
          }
          break1 <- min(as.numeric(as.matrix(data.TT[,-c(1:3)])))
          break2 <- max(as.numeric(as.matrix(data.TT[,-c(1:3)])))
          midpoint <- (break1+break2)/2
          f <- colorRamp2(breaks = c(break1, midpoint, break2), colors = hmapcols)
          
          if(is.na(tkbordercol)){ #no cell borders
            if(tra_hmap_poslines[[i]] == "2"){ #no position lines
              circos.genomicHeatmap(
                data.TT, col = f, side = "inside" , heatmap_height = tkheight , numeric.column = 4:13 ,  track.margin = c(tkmargin,0) , connection_height = NULL
              )
            }else{
              circos.genomicHeatmap(
                data.TT, col = f, side = "inside" , heatmap_height = tkheight , numeric.column = 4:13 ,  track.margin = c(tkmargin,0) , connection_height = heightlines
              )
            }
          }else{
            if(tra_hmap_poslines[[i]] == "2"){
              circos.genomicHeatmap(
                data.TT, col = f, side = "inside" , heatmap_height = tkheight , numeric.column = 4:13 ,  track.margin = c(tkmargin,0) , connection_height = NULL , border = tkbordercol , border_lwd = 0.1
              )
            }else{
              circos.genomicHeatmap(
                data.TT, col = f, side = "inside" , heatmap_height = tkheight , numeric.column = 4:13 ,  track.margin = c(tkmargin,0) , connection_height = heightlines , border = tkbordercol , border_lwd = 0.1
              )
            }
          }
          if(lab_inf[i]){
            if(labels_inf[[2]]=="inside"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = lab_height/4, labels_height = (lab_height/4)*3 , cex =labelsize  , line_col = labels_inf[[4]] , col = labels_inf[[4]] , padding = 0 , track.margin = c(0,0), side = "inside")
            }
          }
        }else if(tktype == "heatmap-discrete"){
          data.TT$num <- NULL
          if(lab_inf[i] != 0){
            
            if(labels_inf[[2]]=="outside"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = lab_height/4, labels_height = (lab_height/4)*3 , cex =labelsize  , line_col = labels_inf[[4]] , col = labels_inf[[4]] , padding = 0 , track.margin = c(0,0), side = "outside")
            }
          }
          #no cell borders
          if(tra_hmap_poslines[[i]] == "2"){ #no position lines
            circos.genomicHeatmap(
              data.TT_hat_dis, col = data_TT_col, side = "inside" , heatmap_height = tkheight , numeric.column = 4:length(data.TT_hat_dis) ,  track.margin = c(tkmargin,0) , connection_height = NULL
            )
          }else{
            circos.genomicHeatmap(
              data.TT_hat_dis, col = data_TT_col, side = "inside" , heatmap_height = tkheight , numeric.column = 4:length(data.TT_hat_dis) ,  track.margin = c(tkmargin,0) , connection_height = heightlines
            )
          }
          
          #if(is.na(tkbordercol)){}
          # else{
          #   if(tra_hmap_poslines[[i]] == "2"){
          #     circos.genomicHeatmap(
          #       data.TT_hat_dis, col = data_TT_col, side = "inside" , heatmap_height = tkheight , numeric.column = 4:length(data.TT_hat_dis) ,  track.margin = c(tkmargin,0) , connection_height = NULL , border = tkbordercol , border_lwd = 0.1
          #     )
          #   }else{
          #     circos.genomicHeatmap(
          #       data.TT_hat_dis, col = data_TT_col, side = "inside" , heatmap_height = tkheight , numeric.column = 4:length(data.TT_hat_dis) ,  track.margin = c(tkmargin,0) , connection_height = heightlines , border = tkbordercol , border_lwd = 0.1
          #     )
          #   }
          # }
          data.TT <- data_TT_col
          if(lab_inf[i]){
            if(labels_inf[[2]]=="inside"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = lab_height/4, labels_height = (lab_height/4)*3 , cex =labelsize  , line_col = labels_inf[[4]] , col = labels_inf[[4]] , padding = 0 , track.margin = c(0,0), side = "inside")
            }
          }
        }else if(tktype == "ideogram"){
          if(lab_inf[i]){
            if(labels_inf[[2]]=="outside"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = lab_height/4, labels_height = (lab_height/4)*3 , cex =labelsize  , line_col = labels_inf[[4]] , col = labels_inf[[4]] , padding = 0 , track.margin = c(0,0), side = "outside")
            }
          }
          circos.genomicIdeogram(data.TT,track.height = tkheight, track.margin = c(tkmargin,0))
          if(lab_inf[i]){
            if(labels_inf[[2]]=="inside"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = lab_height/4, labels_height = (lab_height/4)*3 , cex =labelsize  , line_col = labels_inf[[4]] , col = labels_inf[[4]] , padding = 0 , track.margin = c(0,0), side = "inside")
            }
          }
        }else if(tktype == "bar"){
          if(lab_inf[i]){
            if(labels_inf[[2]]=="outside"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = lab_height/4, labels_height = (lab_height/4)*3 , cex =labelsize  , line_col = labels_inf[[4]] , col = labels_inf[[4]] , padding = 0 , track.margin = c(0,0), side = "outside")
            }
          }
          data.TT[,ncol(data.TT)] <- as.numeric(data.TT[,ncol(data.TT)])
          
          circos.genomicTrackPlotRegion(data.TT, track.height = tkheight, track.margin = c(tkmargin,0),bg.col = tkbgcol , bg.border = tkborder , panel.fun = function(region,value,...){
            if(!("color" %in% colnames(data.T[[i]])) && !("cols" %in% colnames(data.TTC))){
              if(length(columns)==1 && tkbardir==1){
                if(nchar(tklinecolor[1])!=0){
                  xlim <- get.cell.meta.data("xlim")
                  ylim <- get.cell.meta.data("ylim")
                  for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                  }
                }
                if(coltypeTrack==1){
                  circos.genomicRect(region, value, numeric.column=columns-3, ytop.column = 1, ybottom = min(data.TT[,4]), col = tkcolor, border = NA, ...)
                }
              }else if(length(columns)==1 && tkbardir==2){
                if(nchar(tklinecolor[1])!=0){
                  xlim <- get.cell.meta.data("xlim")
                  ylim <- get.cell.meta.data("ylim")
                  for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                  }
                }
                tkbarvalue <- as.numeric(tkbarvalue)
                indx <- value[,1] > tkbarvalue
                if(length(value[indx,])!=0 && length(value[!indx,])!=0){
                  circos.genomicRect(region[indx,], value[indx,], ytop.column = 1, ybottom = tkbarvalue, col=tkbarcol1, border = NA, ...)
                  circos.genomicRect(region[!indx,], value[!indx,], ytop.column = 1, ybottom =  tkbarvalue, col=tkbarcol2, border = NA, ...)
                }else if(length(value[indx,])!=0 && length(value[!indx,])==0){
                  circos.genomicRect(region[indx,], value[indx,], ytop.column = 1, ybottom = tkbarvalue, col=tkbarcol1, border = NA, ...)
                }else if(length(value[indx,])==0 && length(value[!indx,])!=0){
                  circos.genomicRect(region[!indx,], value[!indx,], ytop.column = 1, ybottom =  tkbarvalue, col=tkbarcol2, border = NA, ...)
                }
              }else if(length(columns)==2 && tkbardir==1){
                if(nchar(tklinecolor[1])!=0){
                  xlim <- get.cell.meta.data("xlim")
                  ylim <- get.cell.meta.data("ylim")
                  for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                  }
                }
                if(coltypeTrack!=3){
                  tkcolor <- c(tkcolor,rep("grey",length(columns)))
                  tkcolor <- tkcolor[1:length(columns)]
                  circos.genomicRect(region, value, ytop.column = 1, ybottom = min(c(data.TT[,4],data.TT[,5])), col=tkcolor[1], border = NA, ...)
                  circos.genomicRect(region, value, ytop.column = 2, ybottom = min(c(data.TT[,4],data.TT[,5])), col=tkcolor[2], border = NA, ...)
                }
              }else if(length(columns)==2 && tkbardir==2){
                if(nchar(tklinecolor[1])!=0){
                  xlim <- get.cell.meta.data("xlim")
                  ylim <- get.cell.meta.data("ylim")
                  for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                  }
                }
                circos.genomicRect(region, value, ytop.column = 1, ybottom = min(c(data.TT[,4],data.TT[,5])), col=tkbarcol1, border = NA, ...)
                circos.genomicRect(region, value, ytop.column = 2, ybottom = min(c(data.TT[,4],data.TT[,5])), col=tkbarcol2, border = NA, ...)
              }
            }
            if(length(columns)==1 && tkbardir==1 && coltypeTrack==2){
              if(nchar(tklinecolor[1])!=0){
                xlim <- get.cell.meta.data("xlim")
                ylim <- get.cell.meta.data("ylim")
                for(k in 1:length(tklinecoord)){
                  y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                  circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                }
              }
              circos.genomicRect(region, value, numeric.column=columns-3, ytop.column = 1, ybottom = min(data.TT[,4]), col=tkcolor[1], border = NA, ...)
            }
          })
          takindx <- get.current.track.index()
          if(length(columns)==1 && tkbardir==1 && coltypeTrack==3 && ncol(data.TTC)>=6 && ("cols" %in% colnames(data.TTC))){
            lapply(unique(data.TTC[,1]),function(x){
              circos.updatePlotRegion(sector.index = x, track.index=takindx, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
              if(nchar(tklinecolor[1])!=0){
                xlim <- get.cell.meta.data("xlim")
                ylim <- get.cell.meta.data("ylim")
                for(k in 1:length(tklinecoord)){
                  y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                  circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                }
              }
              dat <- data.TTC[data.TTC[,1] %in% x,]
              circos.rect(xleft=dat[,2], xright=dat[,3],ytop=dat[,4],ybottom=rep(get.cell.meta.data("ylim")[1],nrow(dat)), col=adjustcolor(dat$cols,alpha.f = tktransparency), border = NA)
            })
          }else if(length(columns)==1 && tkbardir==1 && coltypeTrack==1 && ncol(data.TTC)>=6 && ("cols" %in% colnames(data.TTC))){
            lapply(unique(data.TTC[,1]),function(x){
              circos.updatePlotRegion(sector.index = x, track.index=takindx, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
              if(nchar(tklinecolor[1])!=0){
                xlim <- get.cell.meta.data("xlim")
                ylim <- get.cell.meta.data("ylim")
                for(k in 1:length(tklinecoord)){
                  y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                  circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
                }
              }
              dat <- data.TTC[data.TTC[,1] %in% x,]
              circos.rect(xleft=dat[,2], xright=dat[,3],ytop=dat[,4],ybottom=rep(get.cell.meta.data("ylim")[1],nrow(dat)), col=adjustcolor(dat$cols,alpha.f = tktransparency), border = NA)
            })
          }
          if(tra_yaxis[[i]]==1){
            circos.yaxis(
              side = "left",
              tick = TRUE,
              at = c(min(data.TT_old[,4]),max(data.TT_old[,4])),
              sector.index = get.all.sector.index()[1],
              labels.cex = 0.5
            )
          }
          if(lab_inf[i]){
            if(labels_inf[[2]]=="inside"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = lab_height/4, labels_height = (lab_height/4)*3 , cex =labelsize  , line_col = labels_inf[[4]] , col = labels_inf[[4]] , padding = 0 , track.margin = c(0,0), side = "inside")
            }
          }
        }
        lgdplot[[i]] <<- legendplot(tktype = tktype,data.TT = data.TT,data.TT_old = data.TT_old,i=i,legendpos = legendpos)
        progress$set(value = i)
        
        
      }
      lgdplot_cache <- NULL
      if(!is.null(unlist(lgdplot))){
        for (k in 1:length(lgdplot)) {
          if(is.null(lgdplot[[k]])){
            lgdplot_cache <- c(lgdplot_cache,k)
          }
        }
        if(!is.null(lgdplot_cache)){
          lgdplot <<- lgdplot[-c(lgdplot_cache)]
        }
      }
      
      
      ##
      
    }
    
    if(!is.null(data.L)){
      rou <- get_most_inside_radius()
      rou <- rou[1]
      if(colformatLinks!=3){
        if(colorLinks==2){ #specific color
          splitcol <- ":" %in% unlist(strsplit(selcolorLinks,""))
          if(ncol(data.L)==7 && colnames(data.L)[7]=="color" && splitcol){
            data.L$num <- 1:nrow(data.L)
            selcolorLinks <- unlist(strsplit(selcolorLinks,";"))
            selcolorLinks <- data.frame(id=selcolorLinks,stringsAsFactors=F)
            selcolorLinks$group <- gsub("\\:.*","",selcolorLinks$id)
            selcolorLinks$cols <- gsub(".*\\:","",selcolorLinks$id)
            selcolorLinks$group <- gsub(" ","",selcolorLinks$group)
            selcolorLinks$cols <- gsub(" ","",selcolorLinks$cols)
            data.LC <- merge(data.L,selcolorLinks,by.x="color",by.y="group",all.x=T)
            data.LC$cols[is.na(data.LC$cols)] <- "grey"
            data.LC <- data.LC[order(data.LC$num),]
            data.LC$num <- NULL
            rownames(data.LC) <- NULL
            data.L$num <- NULL
            colLinks <- adjustcolor(data.LC$cols, alpha.f = transparencyLinks)
          }else if((ncol(data.L)==6 | ncol(data.L)==7) && !splitcol){
            colLinks <- adjustcolor(selcolorLinks[1], alpha.f = transparencyLinks)
          }
          data.L1 <- data.L[,c(1:3)]
          data.L2 <- data.L[,c(4:6)]
          circos.genomicLink(data.L1, data.L2, rou = rou, col = colLinks, border = NA , reduce_to_mid_line = midplot)
        }else{
          if(ncol(data.L)==7 && colnames(data.L)[7]=="color"){
            groupnum <- length(unique(data.L[,7]))
            linkscolor.export <<- rand_color(groupnum)
            randcolorLinks <- data.frame(group=unique(data.L[,7]), cols=linkscolor.export, stringsAsFactors=F)
            data.LC <- merge(data.L,randcolorLinks,by.x="color",by.y="group",all.x=T)
            colLinks <- adjustcolor(data.LC$cols, alpha.f = transparencyLinks)
          }
          data.L1 <- data.L1[,c(1:3)]
          data.L2 <- data.L2[,c(1:3)]
          if(ncol(data.L)==7 && colnames(data.L)[7]=="color"){
            circos.genomicLink(data.L1, data.L2, rou = rou, col = colLinks, border = NA , reduce_to_mid_line = midplot)
          }else{
            linkscolor.export <<- rand_color(nrow(data.L1), transparency = 1-transparencyLinks)
            circos.genomicLink(data.L1, data.L2, rou = rou, col = linkscolor.export, border = NA , reduce_to_mid_line = midplot)
          }
          
        }
      }else{
        if(ncol(data.L)==7 && colnames(data.L)[7]=="color"){
          break1 <- min(as.numeric(as.matrix(data.L[,-c(1:6)])))
          break2 <- max(as.numeric(as.matrix(data.L[,-c(1:6)])))
          midpoint <- (break1+break2)/2
          f <- colorRamp2(breaks = c(break1, midpoint, break2), colors = gracolinks)
          colLinks <- f(data.L$color)
        }else if((ncol(data.L)==6 | ncol(data.L)==7)){
          linkscolor.export <<- rand_color(1)
          colLinks <- linkscolor.export
        }
        
        if(!(ncol(data.L)==6)){
          data.L1 <- data.L[,c(1:3)]
          data.L2 <- data.L[,c(4:6)]
          circos.genomicLink(data.L1, data.L2, rou = rou, col = colLinks, border = NA , reduce_to_mid_line = midplot)
        }
        
      }
    }
    if(length(hlt_data) != 0){
      lapply(1:length(hlt_data[,1]), function(k){
        kdata <- hlt_data[k,]
        draw.sector(
          start.degree = circlize(as.numeric(kdata[2]), 0, sector.index = kdata[1], track.index = 1)[1],
          end.degree = circlize(as.numeric(kdata[3]), 0, sector.index = kdata[1], track.index = 1)[1],
          rou1 = 1,
          col =  kdata[4]
        )
        # draw.sector(
        #   start.degree = get.cell.meta.data("cell.start.degree", sector.index = kdata[1]),
        #   end.degree = get.cell.meta.data("cell.end.degree", sector.index =  kdata[2]),
        #   rou1 = get.cell.meta.data("cell.top.radius", track.index =  as.numeric(kdata[3])),
        #   rou2 = get.cell.meta.data("cell.bottom.radius", track.index =  as.numeric(kdata[4])),
        #   col =  kdata[4]
        # )
      })
    }
    circos.clear()
    
  }

  
  lgdplot <- list()
  if(addlegend == "yes"){
    if(legendpos == "Right"){
      ddd <- reactive({
        circle_size = unit(1, "snpc")
        pushViewport(
          viewport(
            x = 0, 
            y = 0.5,
            width = circle_size, 
            height = circle_size,
            just = c("left", "center")
          )
        )
        par(omi = gridOMI(),  new = TRUE)
        circlize_plot()
        upViewport()
        h <- dev.size()[2]
        draw(
          packLegend(
            list = lgdplot,
            max_height = unit(0.9*h,"inch")
          ),
          x = circle_size,
          just = "left"
        )
      })
    }else{
      ddd <- reactive({
        circle_size = unit(1, "snpc")
        pushViewport(
          viewport(
            x = 0.5,
            y = 1,
            width = circle_size, 
            height = circle_size,
            just = c("center", "top")
          )
        )
        par(omi = gridOMI(),  new = TRUE)
        circlize_plot()
        upViewport()
        h <- dev.size()[1]
        draw(
          packLegend(
            list = lgdplot,
            max_width = unit(0.9*h,"inch"),
            direction = "horizontal"
          ),
          y = unit(1,"npc")-circle_size,
          just = "top"
        )
      })
    }
  }else{
    ddd <<- reactive({
      circle_size = unit(1, "snpc")
      pushViewport(
        viewport(
          x = 0, 
          y = 0.5,
          width = circle_size, 
          height = circle_size,
          just = c("left", "center")
        )
      )
      par(omi = gridOMI(),  new = TRUE)
      circlize_plot()
      upViewport()
    })
  }
  output$circosfigure <- renderPlot({
    progress <<- Progress$new(session, min=1, max=length(data.T))
    on.exit(progress$close())
    progress$set(
      message = 'Calculation in progress',
      detail = 'This may take a while...'
    )
    grid.newpage()
    grid.draw(ddd())
    figurecp <<- recordPlot()
  },width = plotsize[1],height = plotsize[2])
}
  
  
  
  
  




