server <- function(input, output,session) {
  
  data.C <- NULL
  data.T <- NULL
  data.L <- NULL
  data.N <- NULL
  chrdatas <- NULL
  tradatas <- NULL
  lindatas <- NULL
  labdatas <- NULL
  alldatapath <- NULL
  sam_chotra <<- 0
  trafil <- NULL
  tradat_cir <- NULL
  tra_Margin <- NULL
  tra_cirpar_setcho <<- NULL
  hlt_data <<- NULL
  tra_inf <- 0
  datan_info <- 0
  datat_info <- 0
  datal_info <- 0
  colorTrack <<- c()
  colorcusTrack <<- c()
  tra_poi_poisiz <<- c()
  hltregion.List <<- list() 
  tra_type <<- list()
  hltTrack.List <<- list()
  hltdata.List <<- list()
  trasetout <<- list()
  dataview_export <<- NULL
  setlist <<- list()
  typlist <<- list()
  typlist_old <<- list()
  check_setlist <<- list()
  setlist_old <<- list()
  check_setlist_old <<- list(0)
  lab_setlist <<- list()
  lab_setlist_old <<- list()
  ####################  trasetting
  tra_bar_direction  <<- list()
  tra_bar_Boundary  <<- list() 
  tra_bar_coldir1  <<- list() 
  tra_bar_coldir2  <<- list() 
  tra_coltype  <<- list() 
  tra_colcol  <<- list() 
  tra_colorcus  <<- list() 
  tra_line_fillarea  <<- list() 
  tra_rect_rectcol  <<- list() 
  tra_rect_rectcoldis  <<- list() 
  tra_rect_rectcoldiscus  <<- list() 
  tra_trct_colrect  <<- list()
  rect_gra_lowCol  <<- list()
  rect_gra_midCol  <<- list()
  rect_gra_highCol  <<- list()
  tra_line_selrea  <<- list() 
  tra_bar_borderarea  <<- list() 
  tra_transparency  <<- list() 
  tra_poipch  <<- list() 
  tra_poi_poisiz  <<- list() 
  tra_baseline  <<- list() 
  tra_colorline  <<- list() 
  tra_bgcol  <<- list() 
  tra_hmap_heatmapcol  <<- list() 
  tra_hmap_typcolhmap  <<- list() 
  tra_hmap_lowColor  <<- list() 
  tra_hmap_midColor  <<- list() 
  tra_hmap_highColor  <<- list()
  tra_hmap_poslines  <<- list() 
  tra_hmap_poslinhei  <<- list()
  tra_heatcol_dis <<- list()
  tra_heat_heatcoldiscus <<- list()
  heightTra  <<- list() 
  Tra_margin  <<- list() 
  tra_hmap_cellbord  <<- list() 
  tra_hmap_cellbord_col  <<- list() 
  tra_border  <<- list() 
  tra_yaxis <<- list()
  ################### labsetting
  lab_pos <<- list()
  poslabels <<- list()
  lab_fontsize <<- list()
  lab_fontcol <<- list()
  ###################
  ###sample environment
  observeEvent(input$sam_dataplan,{
    sam_heatmapcols <<- list()
    sam_gracolinks <<- NULL
    name_data.C <<- NULL
    name_data.T <<- NULL
    name_data.N <<- NULL
    name_data.L <<- NULL
    sam_data.C <<- NULL
    sam_datatypeChr <<- NULL
    sam_labelChr <<- NULL
    sam_labelChr_size <<- 1.2
    sam_trackChr <<- NULL
    sam_colorChr <<- NULL
    sam_heightChr <<- NULL
    sam_outAxis <<- NULL
    sam_outAxis_size <<- 0.7
    sam_gapChr <<- NULL
    sam_distance_Chr <<- NULL
    sam_data.T <<- list()
    sam_tratype <<- NULL
    sam_trapos <<- NULL
    sam_tra_bar_direction <<- NULL
    sam_tra_bar_Boundary <<- NULL
    sam_tra_bar_coldir1 <<- NULL
    sam_tra_bar_coldir2 <<- NULL
    sam_tra_coltype <<- NULL
    sam_tra_colcol <<- NULL
    sam_tra_colorcus <<- NULL
    sam_tra_line_fillarea <<- NULL
    sam_tra_rect_rectcol <<- NULL
    sam_tra_rect_rectcoldis <<- NULL
    sam_tra_rect_rectcoldiscus <<- NULL
    sam_tra_trct_colrect <<- NULL
    sam_tra_line_selrea <<- NULL
    sam_tra_bar_borderarea <<- NULL
    sam_tra_transparency <<- NULL
    sam_tra_poipch <<- NULL
    sam_tra_poi_poisiz <<- NULL
    sam_tra_baseline <<- NULL
    sam_tra_colorline <<- NULL
    sam_tra_bgcol <<- NULL
    sam_tra_hmap_heatmapcol <<- NULL
    sam_tra_hmap_typcolhmap <<- NULL
    sam_tra_hmap_lowColor <<- NULL
    sam_tra_hmap_midColor <<- NULL
    sam_tra_hmap_highColor <<- NULL
    sam_tra_hmap_poslines <<- NULL
    sam_tra_hmap_poslinhei <<- NULL
    sam_tra_heatcol_dis <<- NULL
    sam_tra_heat_heatcoldiscus <<- NULL
    sam_heightTraus <<- NULL
    sam_Tra_margin <<- NULL
    sam_tra_hmap_cellbord <<- NULL
    sam_tra_hmap_cellbord_col <<- NULL
    sam_tra_border <<- NULL
    sam_tra_yaxis <<- NULL
    sam_lab_index <<- NULL
    sam_data.N <<- NULL
    sam_labpos <<- NULL
    sam_lab_fontsize <<- NULL
    sam_lab_fontcol <<- NULL
    sam_poslabels <<- NULL
    sam_labels_inf <<- NULL
    sam_data.L <<- NULL
    sam_colformatLinks <<- NULL
    sam_colorLinks <<- NULL
    sam_selcolorLinks <<- NULL
    sam_transparencyLinks <<- NULL
    sam_lowColinks <<- NULL
    sam_midColinks <<- NULL
    sam_highColinks <<- NULL
    sam_addlegend <<- NULL
    sam_legendpos <<- NULL
    sam_source_data <<- NULL
    sam_plotsize <<- NULL
    sam_hlt_data <<- NULL
    
    sam_dataplan <- input$sam_dataplan
    
    plotflash_report <<- 1
    letplotgo <<- 1
    if(sam_dataplan == 1){
      name_data.C <<- c("chromosome_general.csv")
      name_data.T <<- c("barplot.csv","chromosome_ideogram.csv","heatmap-gradual.csv","line.csv","point.csv","rect_discrete.csv")
      name_data.N <<- c("gene_label.csv","gene_label1.csv")
      name_data.L <<- c("links.csv")
	    sam_data.C <<- data.frame(read.csv("./www/example_data/chromosome/chromosome_general.csv",header = T),stringsAsFactors = F)
      sam_datatypeChr <<- "1"
      sam_labelChr <<- 1
      sam_labelChr_size <<- 1.2
      sam_trackChr <<- "track"
      sam_colorChr <<- "#00EAFF"
      sam_heightChr <<- 0.05
      sam_outAxis <<- 1
      sam_outAxis_size <<- 0.7
      sam_gapChr <<- 1
      sam_distance_Chr <<- 0.01
      data.T1 <<- data.frame(read.csv("./www/example_data/track/barplot.csv",header = T),stringsAsFactors = F)
      data.T2 <<- data.frame(read.csv("./www/example_data/track/chromosome_ideogram.csv",header = T),stringsAsFactors = F)
      data.T3 <<- data.frame(read.csv("./www/example_data/track/heatmap_gradual.csv",header = T),stringsAsFactors = F)
      data.T4 <<- data.frame(read.csv("./www/example_data/track/line.csv",header = T),stringsAsFactors = F)
      data.T5 <<- data.frame(read.csv("./www/example_data/track/point.csv",header = T),stringsAsFactors = F)
      data.T6 <<- data.frame(read.csv("./www/example_data/track/rect_discrete.csv",header = T),stringsAsFactors = F)
      sam_data.T <<- list(data.T1,data.T2,data.T3,data.T4,data.T5,data.T6)
      sam_tratype <<- c("bar","ideogram","heatmap-gradual","line","point","rect-discrete")
      sam_trapos <<- c(1,2,3,4,5,6)
      sam_tra_bar_direction <<- c(1,1,1,1,1,1)
      sam_tra_bar_Boundary <<- c(0,0,0,0,0,0)
      sam_tra_bar_coldir1 <<- c("#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000")
      sam_tra_bar_coldir2 <<- c("#00FFFF","#00FFFF","#00FFFF","#00FFFF","#00FFFF","#00FFFF")
      sam_tra_coltype <<- c(1,1,1,1,1,1)
      sam_tra_colcol <<- c("red,blue","red,blue","red,blue","red,blue","red,blue","red,blue")
      sam_tra_colorcus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan")
      sam_tra_line_fillarea <<- c("","","","","","")
      sam_tra_rect_rectcol <<- c("1","1","1","1","1","1")
      sam_tra_rect_rectcoldis <<- c("#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000")
      sam_tra_rect_rectcoldiscus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan")
      sam_tra_trct_colrect <<- c("blue","blue","blue","blue","blue","blue")
      sam_tra_line_selrea <<- c("1","1","1","1","1","1")
      sam_tra_bar_borderarea <<- c("#FFA500","#FFA500","#FFA500","#FFA500","#FFA500","#FFA500")
      sam_tra_transparency <<- c(1,1,1,1,1,1)
      sam_tra_poipch <<- c("16","16","16","16","16","16")
      sam_tra_poi_poisiz <<- c(0.6,0.6,0.6,0.6,0.6,0.6)
      sam_tra_baseline <<- c("0.25,0.75","0.25,0.75","0.25,0.75","0.25,0.75","0.25,0.75","0.25,0.75")
      sam_tra_colorline <<- c("#808080","#808080","#808080","#808080","#808080","#808080")
      sam_tra_bgcol <<- c("#F2F2F2","#F2F2F2","#F2F2F2","#F2F2F2","#F2F2F2","#F2F2F2")
      sam_tra_hmap_heatmapcol <<- c("1","1","1","1","1","1")
      sam_tra_hmap_typcolhmap <<- c("blue.white.red","blue.white.red","blue.white.red","blue.white.red","blue.white.red","blue.white.red")
      sam_tra_hmap_lowColor <<- c("#0016DB","#0016DB","#0016DB","#0016DB","#0016DB","#0016DB")
      sam_tra_hmap_midColor <<- c("#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF")
      sam_tra_hmap_highColor <<- c("#FFFF00","#FFFF00","#FFFF00","#FFFF00","#FFFF00","#FFFF00")
      sam_tra_hmap_poslines <<- c("2","2","2","2","2","2")
      sam_tra_hmap_poslinhei <<- c(0.06,0.06,0.06,0.06,0.06,0.06)
      sam_tra_heatcol_dis <<- c(1,1,1,1,1,1)
      sam_tra_heat_heatcoldiscus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan")
      sam_heightTraus <<- c(0.05,0.05,0.05,0.05,0.05,0.05)
      sam_Tra_margin <<- c(0.01,0.01,0.01,0.01,0.01,0.01)
      sam_tra_hmap_cellbord <<- c("","","","","","")
      sam_tra_hmap_cellbord_col <<- c("#000000","#000000","#000000","#000000","#000000","#000000")
      sam_tra_border <<- c("","","","","","")
      sam_tra_yaxis <<- c(2,2,2,2,2,2)
      sam_lab_index <<- c(0,3)
      data.N1 <<- data.frame(read.csv("./www/example_data/label/gene_label.csv",header = T),stringsAsFactors = F)
      data.N2 <<- data.frame(read.csv("./www/example_data/label/gene_label1.csv",header = T),stringsAsFactors = F)
      sam_data.N <<- list(data.N1,data.N2)
      sam_labpos <<- c(0,3)
	    sam_lab_fontsize <<- c(0.15,0.13)
      sam_lab_fontcol <<- c("#000000","#FF0000")
      sam_poslabels <<- c("inside","outside")
      sam_labels_inf <<- as.data.frame(cbind(sam_labpos,sam_poslabels,sam_lab_fontsize,sam_lab_fontcol,c(1:length(sam_data.N)),rep(1,length(sam_lab_fontcol)),rep(100,length(sam_lab_fontcol))))
      sam_data.L <<- data.frame(read.csv("./www/example_data/links/links.csv",header = T),stringsAsFactors = F)
      sam_colformatLinks <<- "1"
	    sam_colorLinks <<- "1"
      sam_selcolorLinks <<- "yellowgreen"
      sam_transparencyLinks <<- 0.5
      sam_lowColinks <<- "#0016DB"
      sam_midColinks <<- "#FFFFFF"
      sam_highColinks <<- "#FFFF00"
      sam_addlegend <<- "No"
      sam_legendpos <<- "Bottom"  
      sam_source_data <<- "b"
      sam_plotsize <<- c(750,750)
    }else if(sam_dataplan == 2){
      name_data.C <<- c("chromosome_general.csv")
      name_data.T <<- c("line.csv","point.csv","rect_discrete.csv")
      sam_data.C <<- data.frame(read.csv("./www/example_data/chromosome/chromosome_general.csv",header = T),stringsAsFactors = F)
      sam_datatypeChr <<- "1"
      sam_labelChr <<- 1
      sam_labelChr_size <<- 1.2
      sam_trackChr <<- "track"
      sam_colorChr <<- "#00EAFF"
      sam_heightChr <<- 0.05
      sam_outAxis <<- 1
      sam_outAxis_size <<- 0.7
      sam_gapChr <<- 1
      sam_distance_Chr <<- 0.01
      data.T4 <<- data.frame(read.csv("./www/example_data/track/line.csv",header = T),stringsAsFactors = F)
      data.T5 <<- data.frame(read.csv("./www/example_data/track/point.csv",header = T),stringsAsFactors = F)
      data.T6 <<- data.frame(read.csv("./www/example_data/track/rect_discrete.csv",header = T),stringsAsFactors = F)
      sam_data.T <<- list(data.T4,data.T5,data.T6)
      sam_trapos <<- c(1,2,3)
      sam_tratype <<- c("line","point","rect-discrete")
      sam_tra_bar_direction <<- c(1,1,1)
      sam_tra_bar_Boundary <<- c(0,0,0)
      sam_tra_bar_coldir1 <<- c("#FF0000","#FF0000","#FF0000")
      sam_tra_bar_coldir2 <<- c("#00FFFF","#00FFFF","#00FFFF")
      sam_tra_coltype <<- c(1,1,1)
      sam_tra_colcol <<- c("red,blue","red,blue","red,blue")
      sam_tra_colorcus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan")
      sam_tra_line_fillarea <<- c("","","")
      sam_tra_rect_rectcol <<- c("1","1","1")
      sam_tra_rect_rectcoldis <<- c("#FF0000","#FF0000","#FF0000")
      sam_tra_rect_rectcoldiscus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan")
      sam_tra_trct_colrect <<- c("blue","blue","blue")
      sam_tra_line_selrea <<- c("1","1","1")
      sam_tra_bar_borderarea <<- c("#FFA500","#FFA500","#FFA500")
      sam_tra_transparency <<- c(1,1,1)
      sam_tra_poipch <<- c("16","16","16")
      sam_tra_poi_poisiz <<- c(0.6,0.6,0.6)
      sam_tra_baseline <<- c("0.25,0.75","0.25,0.75","0.25,0.75")
      sam_tra_colorline <<- c("#808080","#808080","#808080")
      sam_tra_bgcol <<- c("#F2F2F2","#F2F2F2","#F2F2F2")
      sam_tra_hmap_heatmapcol <<- c("1","1","1")
      sam_tra_hmap_typcolhmap <<- c("blue.white.red","blue.white.red","blue.white.red")
      sam_tra_hmap_lowColor <<- c("#0016DB","#0016DB","#0016DB")
      sam_tra_hmap_midColor <<- c("#FFFFFF","#FFFFFF","#FFFFFF")
      sam_tra_hmap_highColor <<- c("#FFFF00","#FFFF00","#FFFF00")
      sam_tra_hmap_poslines <<- c("2","2","2")
      sam_tra_hmap_poslinhei <<- c(0.06,0.06,0.06)
      sam_tra_heatcol_dis <<- c(1,1,1)
      sam_tra_heat_heatcoldiscus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan")
      sam_heightTraus <<- c(0.2,0.2,0.05)
      sam_Tra_margin <<- c(0.03,0.03,0.03)
      sam_tra_hmap_cellbord <<- c("","","")
      sam_tra_hmap_cellbord_col <<- c("#000000","#000000","#000000")
      sam_tra_border <<- c("","","")
      sam_tra_yaxis <<- c(1,1,2)
      sam_data.N <<- NULL
      sam_data.L <<- NULL
      sam_addlegend <<- "No"
      sam_legendpos <<- "Bottom"  
      sam_source_data <<- "b"
      sam_plotsize <<- c(750,750)
    }else if(sam_dataplan == 3){
      name_data.C <<- c("chromosome_cytoband.csv")
      name_data.T <<- c("barplot.csv","barplot_bidirectional.csv","barplot_color.csv")
      sam_data.C <<- data.frame(read.csv("./www/example_data/chromosome/chromosome_cytoband.csv",header = T),stringsAsFactors = F)
      sam_heightChr <<- 0.05
      sam_datatypeChr <<- "2"
      sam_labelChr <<- 1
      sam_labelChr_size <<- 1.2
      sam_trackChr <<- "track"
      sam_colorChr <<- "#00EAFF"
      sam_outAxis <<- 1
      sam_outAxis_size <<- 0.7
      sam_gapChr <<- 1
      sam_distance_Chr <<- 0.01
      data.T1 <<- data.frame(read.csv("./www/example_data/track/barplot.csv",header = T),stringsAsFactors = F)
      data.T2 <<- data.frame(read.csv("./www/example_data/track/barplot_bidirectional.csv",header = T),stringsAsFactors = F)
      data.T3 <<- data.frame(read.csv("./www/example_data/track/barplot_color.csv",header = T),stringsAsFactors = F)
      sam_data.T <<- list(data.T1,data.T2,data.T3)
      sam_trapos <<- c(1,2,3)
      sam_tratype <<- c("bar","bar","bar")
      sam_tra_bar_direction <<- c(1,2,1)
      sam_tra_bar_Boundary <<- c(0,0,0)
      sam_tra_bar_coldir1 <<- c("#FF0000","#FF0000","#FF0000")
      sam_tra_bar_coldir2 <<- c("#00FFFF","#00FFFF","#00FFFF")
      sam_tra_coltype <<- c(1,1,3)
      sam_tra_colcol <<- c("red,blue","red,blue","red,blue,yellow")
      sam_tra_colorcus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan;d:yellow")
      sam_tra_line_fillarea <<- c("","","")
      sam_tra_rect_rectcol <<- c("1","1","1")
      sam_tra_rect_rectcoldis <<- c("#FF0000","#FF0000","#FF0000")
      sam_tra_rect_rectcoldiscus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan")
      sam_tra_trct_colrect <<- c("blue","blue","blue")
      sam_tra_line_selrea <<- c("1","1","1")
      sam_tra_bar_borderarea <<- c("#FFA500","#FFA500","#FFA500")
      sam_tra_transparency <<- c(1,1,1)
      sam_tra_poipch <<- c("16","16","16")
      sam_tra_poi_poisiz <<- c(0.6,0.6,0.6)
      sam_tra_baseline <<- c("0.25,0.75","0.25,0.75","0.25,0.75")
      sam_tra_colorline <<- c("#808080","#808080","#808080")
      sam_tra_bgcol <<- c("#F2F2F2","#F2F2F2","#F2F2F2")
      sam_tra_hmap_heatmapcol <<- c("1","1","1")
      sam_tra_hmap_typcolhmap <<- c("blue.white.red","blue.white.red","blue.white.red")
      sam_tra_hmap_lowColor <<- c("#0016DB","#0016DB","#0016DB")
      sam_tra_hmap_midColor <<- c("#FFFFFF","#FFFFFF","#FFFFFF")
      sam_tra_hmap_highColor <<- c("#FFFF00","#FFFF00","#FFFF00")
      sam_tra_hmap_poslines <<- c("2","2","2")
      sam_tra_hmap_poslinhei <<- c(0.06,0.06,0.06)
      sam_tra_heatcol_dis <<- c(1,1,1)
      sam_tra_heat_heatcoldiscus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan")
      sam_heightTraus <<- c(0.15,0.2,0.15)
      sam_Tra_margin <<- c(0.03,0.03,0.03)
      sam_tra_hmap_cellbord <<- c("","","")
      sam_tra_hmap_cellbord_col <<- c("#000000","#000000","#000000")
      sam_tra_border <<- c("","","")
      sam_tra_yaxis <<- c(1,1,1)
      sam_data.N <<- NULL
      sam_data.L <<- NULL
      sam_addlegend <<- "No"
      sam_legendpos <<- "Right"  
      sam_source_data <<- "b"
      sam_plotsize <<- c(750,750)
    }else if(sam_dataplan == 4){
      name_data.C <<- c("chromosome_general.csv")
      name_data.T <<- c("heatmap_discrete","heatmap_gradual")
      sam_data.C <<- data.frame(read.csv("./www/example_data/chromosome/chromosome_general.csv",header = T),stringsAsFactors = F)
      sam_datatypeChr <<- "1"
      sam_labelChr <<- 1
      sam_labelChr_size <<- 1.2
      sam_trackChr <<- "track"
      sam_colorChr <<- "#00EAFF"
      sam_heightChr <<- 0.05
      sam_outAxis <<- 1
      sam_outAxis_size <<- 0.7
      sam_gapChr <<- 1
      sam_distance_Chr <<- 0.01
      data.T1 <<- data.frame(read.csv("./www/example_data/track/heatmap_discrete.csv",header = T),stringsAsFactors = F)
      data.T2 <<- data.frame(read.csv("./www/example_data/track/heatmap_gradual.csv",header = T),stringsAsFactors = F)
      sam_data.T <<- list(data.T1,data.T2)
      sam_trapos <<- c(1,2)
      sam_tratype <<- c("heatmap-discrete","heatmap-gradual")
      sam_tra_bar_direction <<- c(1,1)
      sam_tra_bar_Boundary <<- c(0,0)
      sam_tra_bar_coldir1 <<- c("#FF0000","#FF0000")
      sam_tra_bar_coldir2 <<- c("#00FFFF","#00FFFF")
      sam_tra_coltype <<- c(1,1)
      sam_tra_colcol <<- c("red,blue","red,blue")
      sam_tra_colorcus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan")
      sam_tra_line_fillarea <<- c("","")
      sam_tra_rect_rectcol <<- c("1","1")
      sam_tra_rect_rectcoldis <<- c("#FF0000","#FF0000")
      sam_tra_rect_rectcoldiscus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan")
      sam_tra_trct_colrect <<- c("blue","blue")
      sam_tra_line_selrea <<- c("1","1")
      sam_tra_bar_borderarea <<- c("#FFA500","#FFA500")
      sam_tra_transparency <<- c(1,1)
      sam_tra_poipch <<- c("16","16")
      sam_tra_poi_poisiz <<- c(0.6,0.6)
      sam_tra_baseline <<- c("0.25,0.75","0.25,0.75")
      sam_tra_colorline <<- c("#808080","#808080")
      sam_tra_bgcol <<- c("#F2F2F2","#F2F2F2")
      sam_tra_hmap_heatmapcol <<- c("1","1")
      sam_tra_hmap_typcolhmap <<- c("blue.white.red","green.black.red")
      sam_tra_hmap_lowColor <<- c("#0016DB","#0016DB")
      sam_tra_hmap_midColor <<- c("#FFFFFF","#FFFFFF")
      sam_tra_hmap_highColor <<- c("#FFFF00","#FFFF00")
      sam_tra_hmap_poslines <<- c("1","1")
      sam_tra_hmap_poslinhei <<- c(0.06,0.06)
      sam_tra_heatcol_dis <<- c(1,1)
      sam_tra_heat_heatcoldiscus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan")
      sam_heightTraus <<- c(0.2,0.2)
      sam_Tra_margin <<- c(0.01,0.01)
      sam_tra_hmap_cellbord <<- c("add","")
      sam_tra_hmap_cellbord_col <<- c("#000000","#000000")
      sam_tra_border <<- c("","","")
      sam_tra_yaxis <<- c(2,2,2)
      sam_data.N <<- NULL
      sam_data.L <<- NULL
      sam_addlegend <<- "Yes"
      sam_legendpos <<- "Right"  
      sam_source_data <<- "b"
      sam_plotsize <<- c(850,750)
    }else if(sam_dataplan == 5){
      name_data.C <<- c("chromosome_cytoband.csv")
      name_data.T <<- c("line","line_color","line_multicolumn","stack_line")
      sam_data.C <<- data.frame(read.csv("./www/example_data/chromosome/chromosome_cytoband.csv",header = T),stringsAsFactors = F)
      sam_datatypeChr <<- "2"
      sam_labelChr <<- 1
      sam_labelChr_size <<- 1.2
      sam_trackChr <<- "track"
      sam_colorChr <<- "#00EAFF"
      sam_heightChr <<- 0.05
      sam_outAxis <<- 1
      sam_outAxis_size <<- 0.7
      sam_gapChr <<- 1
      sam_distance_Chr <<- 0.01
      data.T1 <<- data.frame(read.csv("./www/example_data/track/line.csv",header = T),stringsAsFactors = F)
      data.T2 <<- data.frame(read.csv("./www/example_data/track/line_color.csv",header = T),stringsAsFactors = F)
      data.T3 <<- data.frame(read.csv("./www/example_data/track/line_multicolumn.csv",header = T),stringsAsFactors = F)
      data.T4 <<- data.frame(read.csv("./www/example_data/track/stack_line.csv",header = T),stringsAsFactors = F)
      sam_data.T <<- list(data.T1,data.T2,data.T3,data.T4)
      sam_trapos <<- c(1,2,3,4)
      sam_tratype <<- c("line","line","line","stack-line")
      sam_tra_bar_direction <<- c(1,1,1,1)
      sam_tra_bar_Boundary <<- c(0,0,0,0)
      sam_tra_bar_coldir1 <<- c("#FF0000","#FF0000","#FF0000","#FF0000")
      sam_tra_bar_coldir2 <<- c("#00FFFF","#00FFFF","#00FFFF","#00FFFF")
      sam_tra_coltype <<- c(1,3,2,1)
      sam_tra_colcol <<- c("red,blue","red,blue","red,blue","red,blue")
      sam_tra_colorcus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan")
      sam_tra_line_fillarea <<- c("","","","")
      sam_tra_rect_rectcol <<- c("1","1","1","1")
      sam_tra_rect_rectcoldis <<- c("#FF0000","#FF0000","#FF0000","#FF0000")
      sam_tra_rect_rectcoldiscus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan")
      sam_tra_trct_colrect <<- c("blue","blue","blue","blue")
      sam_tra_line_selrea <<- c("1","1","1","1")
      sam_tra_bar_borderarea <<- c("#FFA500","#FFA500","#FFA500","#FFA500")
      sam_tra_transparency <<- c(1,1,1,1)
      sam_tra_poipch <<- c("16","16","16","16")
      sam_tra_poi_poisiz <<- c(0.6,0.5,0.4,0.35)
      sam_tra_baseline <<- c("0.25,0.75","0.25,0.75","0.25,0.75","0.25,0.75")
      sam_tra_colorline <<- c("#808080","#808080","#808080","#808080")
      sam_tra_bgcol <<- c("#F2F2F2","#F2F2F2","#F2F2F2","#F2F2F2")
      sam_tra_hmap_heatmapcol <<- c("1","1","1","1")
      sam_tra_hmap_typcolhmap <<- c("blue.white.red","green.black.red","blue.white.red","green.black.red")
      sam_tra_hmap_lowColor <<- c("#0016DB","#0016DB","#0016DB","#0016DB")
      sam_tra_hmap_midColor <<- c("#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF")
      sam_tra_hmap_highColor <<- c("#FFFF00","#FFFF00","#FFFF00","#FFFF00")
      sam_tra_hmap_poslines <<- c("1","1","1","1")
      sam_tra_hmap_poslinhei <<- c(0.06,0.06,0.06,0.06)
      sam_tra_heatcol_dis <<- c(1,1,1,1)
      sam_tra_heat_heatcoldiscus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan")
      sam_heightTraus <<- c(0.15,0.15,0.15,0.15)
      sam_Tra_margin <<- c(0.02,0.02,0.02,0.02)
      sam_tra_hmap_cellbord <<- c("","","","")
      sam_tra_hmap_cellbord_col <<- c("#000000","#000000","#000000","#000000")
      sam_tra_border <<- c("","","","")
      sam_tra_yaxis <<- c(1,1,1,2)
      sam_data.N <<- NULL
      sam_data.L <<- NULL
      sam_addlegend <<- "No"
      sam_legendpos <<- "Bottom"  
      sam_source_data <<- "b"
      sam_plotsize <<- c(750,750)
    }else if(sam_dataplan == 6){
      name_data.C <<- c("chromosome_general.csv")
      name_data.T <<- c("point.csv","point_cex.csv","point_color.csv","point_color_cex.csv","point_color_pch.csv","point_color_pch_cex.csv","point_multicolumn.csv","point_pch.csv","point_pch_cex.csv","stack_point.csv")
      name_data.N <<- NULL
      name_data.L <<- c("links_discrete_color.csv")
      sam_data.C <<- data.frame(read.csv("./www/example_data/chromosome/chromosome_general.csv",header = T),stringsAsFactors = F)
      sam_datatypeChr <<- "1"
      sam_labelChr <<- 1
      sam_labelChr_size <<- 1.2
      sam_trackChr <<- "track"
      sam_colorChr <<- "#00EAFF"
      sam_heightChr <<- 0.05
      sam_outAxis <<- 1
      sam_outAxis_size <<- 0.7
      sam_gapChr <<- 1
      sam_distance_Chr <<- 0.01
      data.T1 <<- data.frame(read.csv("./www/example_data/track/point.csv",header = T),stringsAsFactors = F)
      data.T2 <<- data.frame(read.csv("./www/example_data/track/point_cex.csv",header = T),stringsAsFactors = F)
      data.T3 <<- data.frame(read.csv("./www/example_data/track/point_color.csv",header = T),stringsAsFactors = F)
      data.T4 <<- data.frame(read.csv("./www/example_data/track/point_color_cex.csv",header = T),stringsAsFactors = F)
      data.T5 <<- data.frame(read.csv("./www/example_data/track/point_color_pch.csv",header = T),stringsAsFactors = F)
      data.T6 <<- data.frame(read.csv("./www/example_data/track/point_color_pch_cex.csv",header = T),stringsAsFactors = F)
      data.T7 <<- data.frame(read.csv("./www/example_data/track/point_multicolumn.csv",header = T),stringsAsFactors = F)
      data.T8 <<- data.frame(read.csv("./www/example_data/track/point_pch.csv",header = T),stringsAsFactors = F)
      data.T9 <<- data.frame(read.csv("./www/example_data/track/point_pch_cex.csv",header = T),stringsAsFactors = F)
      data.T10 <<- data.frame(read.csv("./www/example_data/track/stack_point.csv",header = T),stringsAsFactors = F)
      sam_data.T <<- list(data.T1,data.T2,data.T3,data.T4,data.T5,data.T6,data.T7,data.T8,data.T9,data.T10)
      sam_tratype <<- c("point","point","point","point","point","point","point","point","point","stack-point")
      sam_trapos <<- c(1,2,3,4,5,6,7,8,9,10)
      sam_tra_bar_direction <<- c(1,1,1,1,1,1,1,1,1,1)
      sam_tra_bar_Boundary <<- c(0,0,0,0,0,0,0,0,0,0)
      sam_tra_bar_coldir1 <<- c("#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000")
      sam_tra_bar_coldir2 <<- c("#00FFFF","#00FFFF","#00FFFF","#00FFFF","#00FFFF","#00FFFF","#00FFFF","#00FFFF","#00FFFF","#00FFFF")
      sam_tra_coltype <<- c(1,1,1,1,1,1,2,1,1,3)
      sam_tra_colcol <<- c("red,blue","red,blue","red,blue","red,blue","red,blue","red,blue","red,blue","red,blue","red,blue","red,blue")
      sam_tra_colorcus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan")
      sam_tra_line_fillarea <<- c("","","","","","","","","","")
      sam_tra_rect_rectcol <<- c("1","1","1","1","1","1","1","1","1","1")
      sam_tra_rect_rectcoldis <<- c("#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000")
      sam_tra_rect_rectcoldiscus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan")
      sam_tra_trct_colrect <<- c("blue","blue","blue","blue","blue","blue","blue","blue","blue","blue")
      sam_tra_line_selrea <<- c("1","1","1","1","1","1","1","1","1","1")
      sam_tra_bar_borderarea <<- c("#FFA500","#FFA500","#FFA500","#FFA500","#FFA500","#FFA500","#FFA500","#FFA500","#FFA500","#FFA500")
      sam_tra_transparency <<- c(1,1,1,1,1,1,1,1,1,1)
      sam_tra_poipch <<- c("16","16","16","16","16","16","16","16","16","16")
      sam_tra_poi_poisiz <<- c(0.6,0.6,0.6,0.6,0.6,0.6,0.6,0.6,0.6,0.6)
      sam_tra_baseline <<- c("0.25,0.75","0.25,0.75","0.25,0.75","0.25,0.75","0.25,0.75","0.25,0.75","0.25,0.75","0.25,0.75","0.25,0.75","0.25,0.75")
      sam_tra_colorline <<- c("#808080","#808080","#808080","#808080","#808080","#808080","#808080","#808080","#808080","#808080")
      sam_tra_bgcol <<- c("#F2F2F2","#F2F2F2","#F2F2F2","#F2F2F2","#F2F2F2","#F2F2F2","#F2F2F2","#F2F2F2","#F2F2F2","#F2F2F2")
      sam_tra_hmap_heatmapcol <<- c("1","1","1","1","1","1","1","1","1","1")
      sam_tra_hmap_typcolhmap <<- c("blue.white.red","blue.white.red","blue.white.red","blue.white.red","blue.white.red","blue.white.red","blue.white.red","blue.white.red","blue.white.red","blue.white.red")
      sam_tra_hmap_lowColor <<- c("#0016DB","#0016DB","#0016DB","#0016DB","#0016DB","#0016DB","#0016DB","#0016DB","#0016DB","#0016DB")
      sam_tra_hmap_midColor <<- c("#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF")
      sam_tra_hmap_highColor <<- c("#FFFF00","#FFFF00","#FFFF00","#FFFF00","#FFFF00","#FFFF00","#FFFF00","#FFFF00","#FFFF00","#FFFF00")
      sam_tra_hmap_poslines <<- c("2","2","2","2","2","2","2","2","2","2")
      sam_tra_hmap_poslinhei <<- c(0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06)
      sam_tra_heatcol_dis <<- c(1,1,1,1,1,1,1,1,1,1)
      sam_tra_heat_heatcoldiscus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan","a:red;b:blue;c:cyan")
      sam_heightTraus <<- c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05)
      sam_Tra_margin <<- c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01)
      sam_tra_hmap_cellbord <<- c("","","","","","","","","","")
      sam_tra_hmap_cellbord_col <<- c("#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000")
      sam_tra_border <<- c("","","","","","","","","","")
      sam_tra_yaxis <<- c(2,2,2,2,2,2,2,2,2,2)
      sam_lab_index <<- c(0,3)
      sam_data.N <<- NULL
      sam_labpos <<- c(0,3)
      sam_lab_fontsize <<- c(0.15,0.13)
      sam_lab_fontcol <<- c("#000000","#FF0000")
      sam_poslabels <<- c("inside","outside")
      sam_labels_inf <<- as.data.frame(cbind(sam_labpos,sam_poslabels,sam_lab_fontsize,sam_lab_fontcol,c(1:length(sam_data.N)),rep(1,length(sam_lab_fontcol)),rep(100,length(sam_lab_fontcol))))
      sam_data.L <<- data.frame(read.csv("./www/example_data/links/links_discrete_color.csv",header = T),stringsAsFactors = F)
      sam_colformatLinks <<- "2"
      sam_colorLinks <<- "2"
      sam_selcolorLinks <<- "a:red;b:blue"
      sam_transparencyLinks <<- 0.5
      sam_lowColinks <<- "#0016DB"
      sam_midColinks <<- "#FFFFFF"
      sam_highColinks <<- "#FFFF00"
      sam_addlegend <<- "No"
      sam_legendpos <<- "Bottom"  
      sam_source_data <<- "b"
      sam_plotsize <<- c(750,750)
    } else if (sam_dataplan == 7) {
      name_data.C <<- c("chromosome_general.csv")
      name_data.T <<- c("line_color.csv","chromosome_ideogram.csv")
      name_data.N <<- NULL
      name_data.L <<- c("links_discrete_color.csv")
      sam_data.C <<- data.frame(read.csv("./www/example_data/chromosome/chromosome_general.csv",header = T),stringsAsFactors = F)
      sam_datatypeChr <<- "1"
      sam_labelChr <<- 1
      sam_labelChr_size <<- 1.2
      sam_trackChr <<- "track"
      sam_colorChr <<- "#00EAFF"
      sam_heightChr <<- 0.02
      sam_outAxis <<- 1
      sam_outAxis_size <<- 0.7
      sam_gapChr <<- 2
      sam_distance_Chr <<- 0.01
      data.T1 <<- data.frame(read.csv("./www/example_data/track/line_color.csv",header = T),stringsAsFactors = F)
      data.T2 <<- data.frame(read.csv("./www/example_data/track/chromosome_ideogram.csv",header = T),stringsAsFactors = F)
      sam_data.T <<- list(data.T1,data.T2)
      sam_tratype <<- c("line","ideogram")
      sam_trapos <<- c(1,2)
      sam_tra_bar_direction <<- c(1,1)
      sam_tra_bar_Boundary <<- c(0,0)
      sam_tra_bar_coldir1 <<- c("#FF0000","#FF0000")
      sam_tra_bar_coldir2 <<- c("#00FFFF","#00FFFF")
      sam_tra_coltype <<- c(1,1)
      sam_tra_colcol <<- c("red,blue","red,blue")
      sam_tra_colorcus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan")
      sam_tra_line_fillarea <<- c("add","")
      sam_tra_rect_rectcol <<- c("1","1")
      sam_tra_rect_rectcoldis <<- c("#FF0000","#FF0000")
      sam_tra_rect_rectcoldiscus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan")
      sam_tra_trct_colrect <<- c("blue","blue")
      sam_tra_line_selrea <<- c("1","1")
      sam_tra_bar_borderarea <<- c("#FFA500","#FFA500")
      sam_tra_transparency <<- c(1,1)
      sam_tra_poipch <<- c("16","16")
      sam_tra_poi_poisiz <<- c(0.6,0.6)
      sam_tra_baseline <<- c("0.25,0.75","0.25,0.75")
      sam_tra_colorline <<- c("#808080","#808080")
      sam_tra_bgcol <<- c("#F2F2F2","#F2F2F2")
      sam_tra_hmap_heatmapcol <<- c("1","1")
      sam_tra_hmap_typcolhmap <<- c("blue.white.red","blue.white.red")
      sam_tra_hmap_lowColor <<- c("#0016DB","#0016DB")
      sam_tra_hmap_midColor <<- c("#FFFFFF","#FFFFFF")
      sam_tra_hmap_highColor <<- c("#FFFF00","#FFFF00")
      sam_tra_hmap_poslines <<- c("2","2")
      sam_tra_hmap_poslinhei <<- c(0.06,0.06)
      sam_tra_heatcol_dis <<- c(1,1)
      sam_tra_heat_heatcoldiscus <<- c("a:red;b:blue;c:cyan","a:red;b:blue;c:cyan")
      sam_heightTraus <<- c(0.3,0.05)
      sam_Tra_margin <<- c(0.01,0.01)
      sam_tra_hmap_cellbord <<- c("","")
      sam_tra_hmap_cellbord_col <<- c("#000000","#000000")
      sam_tra_border <<- c("","")
      sam_tra_yaxis <<- c(2,2)
      sam_lab_index <<- c(0,3)
      sam_data.N <<- NULL
      sam_labels_inf <<- NULL
      sam_data.L <<- data.frame(read.csv("./www/example_data/links/links_gradual_color.csv",header = T),stringsAsFactors = F)
      sam_colformatLinks <<- "3"
      sam_colorLinks <<- "2"
      sam_selcolorLinks <<- "a:red;b:blue"
      sam_transparencyLinks <<- 0.5
      sam_lowColinks <<- "#0016DB"
      sam_midColinks <<- "#FFFFFF"
      sam_highColinks <<- "#FFFF00"
      sam_addlegend <<- "No"
      sam_legendpos <<- "Bottom"  
      sam_source_data <<- "b"
      sam_plotsize <<- c(750,750)
    }else if(sam_dataplan == 8){
      name_data.C <<- c("chromosome_cytoband.csv")
      name_data.T <<- NULL
      name_data.N <<- NULL
      name_data.L <<- c("links_gradual_color.csv")
      sam_data.C <<- data.frame(read.csv("./www/example_data/chromosome/chromosome_cytoband.csv",header = T),stringsAsFactors = F)
      sam_datatypeChr <<- "2"
      sam_labelChr <<- 1
      sam_labelChr_size <<- 1.2
      sam_trackChr <<- "track"
      sam_colorChr <<- "#00EAFF"
      sam_heightChr <<- 0.1
      sam_outAxis <<- 1
      sam_outAxis_size <<- 0.7
      sam_gapChr <<- 2
      sam_distance_Chr <<- 0.03
      sam_data.T <<- NULL
     
      sam_data.N <<- NULL
      sam_data.L <<- data.frame(read.csv("./www/example_data/links/links_gradual_color.csv",header = T),stringsAsFactors = F)
      sam_colformatLinks <<- "3"
      sam_colorLinks <<- "2"
      sam_selcolorLinks <<- "a:red;b:blue"
      sam_transparencyLinks <<- 0.5
      sam_lowColinks <<- "#0016DB"
      sam_midColinks <<- "#FFFFFF"
      sam_highColinks <<- "#FFFF00"
      sam_addlegend <<- "No"
      sam_legendpos <<- "Bottom"  
      sam_source_data <<- "b"
      sam_plotsize <<- c(750,750)
    }else if(sam_dataplan == 9){
      name_data.C <<- c("chromosome_general.csv")
      name_data.T <<- c("heatmap_discrete","heatmap_gradual","rect_discrete","rect_gradual")
      sam_data.C <<- data.frame(read.csv("./www/example_data/chromosome/chromosome_general.csv",header = T),stringsAsFactors = F)
      sam_datatypeChr <<- "1"
      sam_labelChr <<- 1
      sam_labelChr_size <<- 1.2
      sam_trackChr <<- "track"
      sam_colorChr <<- "#00EAFF"
      sam_heightChr <<- 0.05
      sam_outAxis <<- 1
      sam_outAxis_size <<- 0.7
      sam_gapChr <<- 1
      sam_distance_Chr <<- 0.01
      data.T1 <<- data.frame(read.csv("./www/example_data/track/heatmap_discrete.csv",header = T),stringsAsFactors = F)
      data.T2 <<- data.frame(read.csv("./www/example_data/track/heatmap_gradual.csv",header = T),stringsAsFactors = F)
      data.T3 <<- data.frame(read.csv("./www/example_data/track/rect_discrete.csv",header = T),stringsAsFactors = F)
      data.T4 <<- data.frame(read.csv("./www/example_data/track/rect_gradual.csv",header = T),stringsAsFactors = F)
      sam_data.T <<- list(data.T1,data.T2,data.T3,data.T4)
      sam_trapos <<- c(1,2,3,4)
      sam_tratype <<- c("heatmap-discrete","heatmap-gradual","rect-discrete","rect-gradual")
      sam_tra_rect_rectcol <<- c("1","1","1","1")
      sam_tra_trct_colrect <<- c("blue","blue","blue","blue")
      sam_tra_transparency <<- c(1,1,1,1)
      sam_tra_bgcol <<- c("#F2F2F2","#F2F2F2","#F2F2F2","#F2F2F2")
      sam_tra_hmap_heatmapcol <<- c("1","1","1","1")
      sam_tra_hmap_typcolhmap <<- c("blue.white.red","green.black.red","blue.white.red","green.black.red")
      sam_tra_hmap_poslines <<- c("2","2","2","2")
      sam_tra_heatcol_dis <<- c(1,1,1,1)
      sam_heightTraus <<- c(0.2,0.2,0.1,0.1)
      sam_Tra_margin <<- c(0.01,0.01,0.01,0.01)
      sam_tra_hmap_cellbord <<- c("","","","")
      sam_tra_hmap_cellbord_col <<- c("#000000","#000000","#000000","#000000")
      sam_tra_border <<- c("","","","")
      sam_tra_yaxis <<- c(2,2,2,2)
      sam_data.N <<- NULL
      sam_data.L <<- NULL
      sam_addlegend <<- "Yes"
      sam_legendpos <<- "Bottom"  
      sam_source_data <<- "b"
      sam_plotsize <<- c(650,750)
    } else {
      name_data.C <<- c("chromosome_general.csv")
      name_data.T <<- c("heatmap_gradual")
      sam_data.C <<- data.frame(read.csv("./www/example_data/chromosome/chromosome_general.csv",header = T),stringsAsFactors = F)
      sam_datatypeChr <<- "1"
      sam_labelChr <<- 1
      sam_labelChr_size <<- 1.2
      sam_trackChr <<- "track"
      sam_colorChr <<- "#E0FFFF,#00FFFF,#7FFFD4,#66CDAA,#AFEEEE,#40E0D0,#48D1CC,#20B2AA,#87CEEB,#00BFFF,#1E90FF,#6495ED,#4682B4,#4169E1,#0000CD,#7B68EE,#9370DB,#F0FFF0,#F8F8FF,#FFC0CB,#7CFC00"
      sam_heightChr <<- 0.03
      sam_outAxis <<- 1
      sam_outAxis_size <<- 0.7
      sam_gapChr <<- 1
      sam_distance_Chr <<- 0.02
      data.T1 <<- data.frame(read.csv("./www/example_data/track/heatmap_gradual.csv",header = T),stringsAsFactors = F)
      sam_data.T <<- list(data.T1)
      sam_trapos <<- c(1)
      sam_tratype <<- c("heatmap-gradual")
      sam_tra_transparency <<- c(1)
      sam_tra_bgcol <<- c("#F2F2F2")
      sam_tra_hmap_heatmapcol <<- c("1")
      sam_tra_hmap_typcolhmap <<- c("purple.yellow.red")
      sam_tra_hmap_poslines <<- c("2")
      sam_heightTraus <<- c(0.7)
      sam_Tra_margin <<- c(0.01)
      sam_tra_hmap_cellbord <<- c("")
      sam_tra_hmap_cellbord_col <<- c("#000000")
      sam_tra_border <<- c("")
      sam_tra_yaxis <<- c(2)
      sam_data.N <<- NULL
      sam_data.L <<- NULL
      sam_addlegend <<- "No"
      sam_legendpos <<- "Bottom"  
      sam_source_data <<- "b"
      sam_plotsize <<- c(750,750)
    }
    
    output$viewchr_sam <- renderDT(
      DT::datatable(
        sam_data.C,
        extensions = "Buttons",
        options = list(
          dom = 'Bfrtip',
          buttons = list(
            'pageLength',
            'copy',
            list(extend = 'csv',filename = paste0("chrdata")),
            list(extend = 'excel',filename = paste0("chrdata"))
          ),
          scrollX = TRUE
        )
      ),
      server = FALSE
    )
    tra_len <- length(sam_data.T)
    if(tra_len >= 1){
      lapply(1:tra_len,function(x){
        output[[paste0("viewTra_sam",x)]] <<- renderDT(
          DT::datatable(
            sam_data.T[[x]],
            extensions = "Buttons",
            options = list(
              dom = 'Bfrtip',
              buttons = list(
                'pageLength',
                'copy',
                list(extend = 'csv',filename = paste0("tradata",x)),
                list(extend = 'excel',filename = paste0("tradata",x))
              ),
              scrollX = TRUE
            )
          ),
          server = FALSE
        )
      })
    }
    #
    #labels view
    lab_len <- length(sam_data.N)
    if(lab_len >= 1){
      lapply(1:lab_len,function(x){
        output[[paste0("viewLab_sam",x)]] <<- renderDT(
          DT::datatable(
            sam_data.N[[x]],
            extensions = "Buttons",
            options = list(
              dom = 'Bfrtip',
              buttons = list(
                'pageLength',
                'copy',
                list(extend = 'csv',filename = paste0("labdata",x)),
                list(extend = 'excel',filename = paste0("labdata",x))
              ),
              scrollX = TRUE
            )
          ),
          server = FALSE
        )
      })
    }
    #links view
    output$viewlink_sam <- renderDT(
      DT::datatable(
        sam_data.L,
        extensions = "Buttons",
        options = list(
          dom = 'Bfrtip',
          buttons = list(
            'pageLength',
            'copy',
            list(extend = 'csv',filename = paste0("lindata")),
            list(extend = 'excel',filename = paste0("lindata"))
          ),
          scrollX = TRUE
        )
      ),
      server = FALSE
    )
  })
  observeEvent(input$dataup_example_go,{
    sendSweetAlert(
      session = session,
      title = "",
      text = "Example data loaded successfully!",
      type = "success"
    )
    dataview_export <<- 1
    tra_len <- length(sam_data.T)
    lab_len <- length(sam_data.N)
    output$example_data_ui <<- renderUI({
      tagList(
        bs4Card(
          collapsible = FALSE,
          title = HTML('<i class="fa-solid fa-circle"></i> Chromosome data (used to define the chromosomes of a Circos plot)'),
          width = 12,
          fluidRow(
            column(
              6,
              tags$div(
                HTML(' <font color="#2196F3"><h4><i class="fa-solid fa-play"></i> File name</font>'),
                bs4Dash::tooltip(
                  actionButton(
                    inputId = "datvie_sam_chrfilenm", 
                    label="" , 
                    icon=icon("question"),
                    status="info",
                    size = "xs"
                  ),
                  title = "Filenames of the input datasets.",
                  placement = "bottom"
                )
              )
            ),
            column(
              6,
              tags$div(
                HTML('<font color="#2196F3"><h4><i class="fa-solid fa-play"></i> Chromosome data type</font>'),
                bs4Dash::tooltip(
                  actionButton(
                    inputId = "datvie_sam_chrtyp", 
                    label="" , 
                    icon=icon("question"),
                    status="info",
                    size = "xs"
                  ),
                  title = "Chromosomes data can be either general data with 3 columns or cytoband data with 5 columns. 
                     The first 3 columns of either type of data should be the chromosome ID,
                     the start and end coordinates of different genomic regions. See example data for more details.",
                  placement = "bottom"
                )
              )
            )
          ),
          fluidRow(
            column(
              6,
              fluidRow(
                column(
                  10,
                  h4(name_data.C)
                ),
                column(
                  2,
                  bs4Dash::tooltip(
                    actionBttn(
                      inputId = "view_chr_data_sam",
                      label = NULL,
                      style = "unite",
                      color = "success",
                      icon = icon("eye")
                    ),
                    title = "Click to view the dataset.",
                    placement = "bottom"
                  )
                )
              )
            ),
            column(
              6,
              fluidRow(
                column(
                  11,
                  pickerInput(
                    inputId = "sam_datatypeChr",
                    label = NULL,
                    choices = c("general data" = "1", "cytoband data" = "2"),
                    selected = sam_datatypeChr
                  )
                ),
                column(
                  1,
                  bs4Dash::tooltip(
                    actionBttn(
                      inputId = "sam_chr_setting",
                      label = NULL,
                      style = "unite",
                      color = "success",
                      icon = icon("gear")
                    ),
                    title = "Set parameters for chromosome data. NOTE: Changes will not be applied for example datasets.",
                    placement = "bottom"
                  )
                )
              )
            ),
            tags$head(tags$style(paste0("#jquidatvie_chrvie_sam .modal-dialog{ max-width:1200px}"))),
            jqui_draggable(
              bsModal(
                id = "jquidatvie_chrvie_sam",
                title =  NULL,
                trigger = "view_chr_data_sam",
                size = "large",
                DTOutput("viewchr_sam")
              )
            ),
            tags$head(tags$style("#jquicirpar_chrsetting_sam .modal-dialog{ width:600px}")),
            jqui_draggable(
              bsModal(
                id = "jquicirpar_chrsetting_sam",
                title = NULL,
                trigger = "sam_chr_setting",
                size = "large",
                conditionalPanel(
                  condition = "input.sam_datatypeChr == '1'",
                  pickerInput(
                    inputId = "sam_trackChr",
                    label = tags$div(
                      HTML('<font color="red"><h5><i class="fa-solid fa-play"></i><b> Display chromosome band?</b></font>'),
                      bs4Dash::tooltip(
                        actionButton(
                          inputId = "datvie_sam_trachr", 
                          label="" , 
                          icon=icon("question"),
                          status="info",
                          size = "xs"
                        ),
                        title = "Display or hide the chromosome band?",
                        placement = "bottom"
                      )
                    ),
                    choices = c("Yes" = "track", "No" = ""),
                    selected = sam_trackChr
                  ),
                  conditionalPanel(
                    condition = "input.sam_trackChr == 'track'",
                    textInput(
                      inputId = "sam_colorChr",
                      label = tags$div(
                        HTML('<font color="red"><h5><i class="fa-solid fa-play"></i><b> Color(s) for chromosome band</b></font>'),
                        bs4Dash::tooltip(
                          actionButton(
                            inputId = "datvie_sam_chrcol", 
                            label="" , 
                            icon=icon("question"),
                            status="info",
                            size = "xs"
                          ),
                          title = "Colors to be used for chromosome bands. Character vector of arbitrary length representing colors is accepted and adjusted automatically to the number of chromosomes. For example, 'grey' or 'grey,red,green,blue'. Hex color codes as '#FF0000' are also supported.",
                          placement = "bottom"
                        )
                      ),
                      value = sam_colorChr
                    )
                  ),
                  conditionalPanel(
                    condition = "input.sam_trackChr == 'track'",
                    numericInput(
                      inputId = "sam_heightChr",
                      label = tags$div(
                        HTML('<font color="red"><h5><i class="fa-solid fa-play"></i><b> Chromosome band height</b></font>'),
                        bs4Dash::tooltip(
                          actionButton(
                            inputId = "datvie_sam_heichr1", 
                            label="" , 
                            icon=icon("question"),
                            status="info",
                            size = "xs"
                          ),
                          title = "Height of the chromosome band, which should be greater than 0 and smaller than 0.9.",
                          placement = "bottom"
                        )
                      ),
                      value = sam_heightChr, 
                      min = 0.01, 
                      max = 0.5,
                      step = 0.01
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.sam_datatypeChr == '2'",
                  numericInput(
                    inputId = "sam_heightChr",
                    label = tags$div(
                      HTML('<font color="red"><h5><i class="fa-solid fa-play"></i><b> Chromosome band height</b></font>'),
                      bs4Dash::tooltip(
                        actionButton(
                          inputId = "datvie_sam_heichr2", 
                          label="" , 
                          icon=icon("question"),
                          status="info",
                          size = "xs"
                        ),
                        title = "Height of the chromosome band, which should be greater than 0 and smaller than 0.9.",
                        placement = "bottom"
                      )
                    ),
                    value = sam_heightChr, 
                    min = 0.01, 
                    max = 0.5,
                    step = 0.01
                  )
                ),
                
                pickerInput(
                  inputId = "sam_outAxis",
                  label = tags$div(
                    HTML('<font><h5><i class="fa-solid fa-play"></i><b> Display the axis for genomic position?</b></font>'),
                    bs4Dash::tooltip(
                      actionButton(
                        inputId = "datvie_sam_outaxis", 
                        label="" , 
                        icon=icon("question"),
                        status="info",
                        size = "xs"
                      ),
                      title = "Display or hide the axis for genomic position?",
                      placement = "bottom"
                    )
                  ),
                  choices = c("Yes" = "1", "No" = "2"),
                  selected = sam_outAxis
                ),
                conditionalPanel(
                  condition = "input.sam_outAxis == '1'",
                  numericInput(
                    inputId = "sam_outAxis_size",
                    label = tags$div(
                      HTML('<font><h5><i class="fa-solid fa-play"></i><b> Font size of the axis for genomic position</b></font>'),
                      bs4Dash::tooltip(
                        actionButton(
                          inputId = "datvie_tip_sam_outaxissize", 
                          label="" , 
                          icon=icon("question"),
                          status="info",
                          size = "xs"
                        ),
                        title = "Too large font size may cause problems.",
                        placement = "bottom"
                      )
                    ),
                    value = sam_outAxis_size,
                    min = 0.1,
                    max = 2, 
                    step = 0.1
                  )
                ),
                pickerInput(
                  inputId = "sam_labelChr",
                  label = tags$div(
                    HTML('<font color="red"><h5><i class="fa-solid fa-play"></i><b> Display chromosome IDs?</b></font>'),
                    bs4Dash::tooltip(
                      actionButton(
                        inputId = "datvie_sam_labelchr", 
                        label="" , 
                        icon=icon("question"),
                        status="info",
                        size = "xs"
                      ),
                      title = "Display or hide the chromosome IDs?",
                      placement = "bottom"
                    )
                  ),
                  choices = c("Yes" = "1", "No" = "2"),
                  selected = sam_labelChr
                ),
                conditionalPanel(
                  condition = "input.sam_labelChr == '1'",
                  numericInput(
                    inputId = "sam_labelChr_size",
                    label = tags$div(
                      HTML('<font color="red"><h5><i class="fa-solid fa-play"></i><b> Font size of the chromosome IDs</b></font>'),
                      bs4Dash::tooltip(
                        actionButton(
                          inputId = "datvie_sam_labsize_tip", 
                          label="" , 
                          icon=icon("question"),
                          status="info",
                          size = "xs"
                        ),
                        title = "Too large font size may cause problems.",
                        placement = "bottom"
                      )
                    ),
                    value = sam_labelChr_size,
                    min = 0.1,
                    max = 3, 
                    step = 0.1
                  ),
                  sliderTextInput(
                    inputId = "sam_outergap",
                    label = tags$div(
                      HTML('<font color="red"><h5><i class="fa-solid fa-play"></i><b> Distance between chromosome IDs and chromosome axis</b></font>'),
                      bs4Dash::tooltip(
                        actionButton(
                          inputId = "datvie_tip90", 
                          label="" , 
                          icon=icon("question"),
                          status="info",
                          size = "xs"
                        ),
                        title = "Distance between the chromosome IDs and the chromosome axis for genomic positions.",
                        placement = "bottom"
                      )
                    ),
                    choices = 0:200,
                    grid = FALSE
                  )
                ),
                textInput(
                  inputId = "sam_gapChr",
                  label = tags$div(
                    HTML('<font><h5><i class="fa-solid fa-play"></i><b> Distances between adjacent sectors</b></font>'),
                    bs4Dash::tooltip(
                      actionButton(
                        inputId = "datvie_tip_sam_gapchr", 
                        label="" , 
                        icon=icon("question"),
                        status="info",
                        size = "xs"
                      ),
                      title = "Distances between neighbouring sectors. Numeric vector of arbitrary length is accepted and adjusted automatically to the number of sectors. For example, '1' or '1,2,3,1'. The first value corresponds to the distance between the first and the second sector.",
                      placement = "bottom"
                    )
                  ),
                  value = sam_gapChr
                ),
                numericInput(
                  inputId = "sam_distance_Chr",
                  label = tags$div(
                    HTML('<font color="red"><h5><i class="fa-solid fa-play"></i><b> Distance to the next section (track, label data, or link data)</b></font>'),
                    bs4Dash::tooltip(
                      actionButton(
                        inputId = "datvie_tip_sam_dischr", 
                        label="" , 
                        icon=icon("question"),
                        status="info",
                        size = "xs"
                      ),
                      title = "This parameter can be used to tune the distance between adjacent tracks, or the distance between a track and a label data, or the distance between a track and a link data.",
                      placement = "bottom"
                    )
                  ),
                  value = sam_distance_Chr, 
                  min = 0, 
                  max = 0.1,
                  step = 0.01
                )
              )
            )
          )
        ),
        if(!is.null(sam_data.T)){
          bs4Card(
            collapsible = FALSE,
            title = HTML('<i class="fa-solid fa-circle"></i> Track data (to be displayed in different tracks of a Circos plot)'),
            width = 12,
            fluidRow(
              column(
                6,
                tags$div(
                  HTML(' <font color="#2196F3"><h4><i class="fa-solid fa-play"></i> File name</font>'),
                  bs4Dash::tooltip(
                    actionButton(
                      inputId = "datvie_tip_sam_chrnm", 
                      label="" , 
                      icon=icon("question"),
                      status="info",
                      size = "xs"
                    ),
                    title = "Filenames of the input datasets.",
                    placement = "bottom"
                  )
                )
              ),
              column(
                3,
                tags$div(
                  HTML(' <font color="#2196F3"><h4><i class="fa-solid fa-play"></i> Plot type</font>'),
                  bs4Dash::tooltip(
                    actionButton(
                      inputId = "datvie_tip_sam_plottype", 
                      label="" , 
                      icon=icon("question"),
                      status="info",
                      size = "xs"
                    ),
                    title = "The type of plot to create using data of the current track.",
                    placement = "bottom"
                  )
                )
              ),
              column(
                3,
                tags$div(
                  HTML(' <font color="#2196F3"><h4><i class="fa-solid fa-play"></i> Track index</font>'),
                  bs4Dash::tooltip(
                    actionButton(
                      inputId = "datvie_tip_sam_traidx", 
                      label="" , 
                      icon=icon("question"),
                      status="info",
                      size = "xs"
                    ),
                    title = "The index of the current track. Identical index is not allowed for different tracks.",
                    placement = "bottom"
                  )
                )
              )
            ),
            lapply(1:tra_len,function(x){
              fluidRow(
                column(
                  6,
                  fluidRow(
                    column(
                      10,
                      h4(paste0(name_data.T[x]))
                    ),
                    column(
                      2,
                      bs4Dash::tooltip(
                        actionBttn(
                          inputId = paste0("view_sam_tra_data",x),
                          label = NULL,
                          style = "unite",
                          color = "success",
                          icon = icon("eye")
                        ),
                        title = "Click to view the dataset.",
                        placement = "bottom"
                      )
                    )
                  )
                ),
                column(
                  3,
                  pickerInput(
                    inputId = paste0("sam_tratype",x),
                    label = NULL,
                    choices = c("point", "line", "bar", "rect-discrete", "rect-gradual" , "heatmap-discrete" , "heatmap-gradual", "ideogram","stack-point","stack-line"),
                    selected = sam_tratype[x]
                  )
                ),
                column(
                  3,
                  fluidRow(
                    column(
                      10,
                      pickerInput(
                        inputId = paste0("sam_trapos",x),
                        label = NULL,
                        choices = 1:tra_len,
                        selected = sam_trapos[x]
                      )
                    ),
                    column(
                      2,
                      bs4Dash::tooltip(
                        actionBttn(
                          inputId = paste0("sam_tra_setting",x),
                          label = NULL,
                          style = "unite",
                          color = "success",
                          icon = icon("gear")
                        ),
                        title = "Set parameters for this track. NOTE: Changes will not be applied for example datasets.",
                        placement = "bottom"
                      )
                    )
                  )
                  
                ),
                tags$head(tags$style(paste0("#jquidatvie_travie_sam",x," .modal-dialog{ max-width:1200px}"))),
                jqui_draggable(
                  bsModal(
                    id = paste0("jquidatvie_travie_sam",x),
                    title = NULL,
                    trigger = paste0("view_sam_tra_data",x),
                    size = "large",
                    DTOutput(paste0("viewTra_sam",x))
                  )
                ),
                tags$head(
                  tags$style(
                    paste0("#jquidatvie_trasetting_sam",x," .modal-dialog{ width:1200px}")
                  )
                ),
                jqui_draggable(
                  bsModal(
                    id = paste0("jquidatvie_trasetting_sam",x),
                    title = NULL,
                    trigger = paste0("sam_tra_setting",x),
                    size = "large",
                    tagList(
                      h4(paste0("Plot type of the current track: ",sam_tratype[x])),
                      if(sam_tratype[x] == "bar"){
                        tagList(
                          pickerInput(
                            inputId = paste0("sam_tra_bar_direction",x),
                            label = tags$div(
                              HTML('<font><h5><i class="fa-solid fa-play"></i><b> Bar direction</b></font>'),
                              bs4Dash::tooltip(
                                actionButton(
                                  inputId = paste0("datvie_tip_sam_bar_direction",x), 
                                  label="" , 
                                  icon=icon("question"),
                                  status="info",
                                  size = "xs"
                                ),
                                title = "Bars can be unidirectional or bidirectional. For bidirectional bars, the 4th column which 
                                contains the data values will be divided into two groups based on the boundary value.",
                                placement = "bottom"
                              )
                            ),
                            choices = c("Unidirectional" = "1", "Bidirectional" = "2"),
                            selected = sam_tra_bar_direction[x]
                          ),
                          conditionalPanel(
                            condition= paste0("input.sam_tra_bar_direction",x,"== '2'"),
                            fluidRow(
                              column(
                                width = 4,
                                numericInput(
                                  inputId = paste0("sam_tra_bar_Boundary",x),
                                  label = "Boundary value:",
                                  value=sam_tra_bar_Boundary[x],
                                  step=0.01
                                )
                              ),
                              column(4,
                                     colourInput(
                                       inputId = paste0("sam_tra_bar_coldir1",x),
                                       label = "Outward bar color:",
                                       value = sam_tra_bar_coldir1[x],
                                       returnName = TRUE
                                     )
                              ),
                              column(4,
                                     colourInput(
                                       inputId = paste0("sam_tra_bar_coldir2",x),
                                       label = "Inward bar color:",
                                       value = sam_tra_bar_coldir2[x],
                                       returnName = TRUE
                                     )
                              )
                            )
                          )
                        )
                      },
                      if(sam_tratype[x] != "rect-discrete" & sam_tratype[x] != "rect-gradual" & sam_tratype[x] != "heatmap-discrete" & sam_tratype[x] != "heatmap-gradual" & sam_tratype[x] != "ideogram"){
                        if(sam_tratype[x] != "bar"){
                          tagList(
                            pickerInput(
                              inputId = paste0("sam_tra_coltype",x),
                              label = tags$div(
                                HTML('<font><h5><i class="fa-solid fa-play"></i><b> Data color</b></font>'),
                                bs4Dash::tooltip(
                                  actionButton(
                                    inputId = paste0("datvie_tip_bar_sam_trycoltp",x), 
                                    label="" , 
                                    icon=icon("question"),
                                    status="info",
                                    size = "xs"
                                  ),
                                  title = "The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
                        To customize color for data with multiple columns, users should provide a character string representing one or multiple 
                        colors separated by commas. For example, 'red' or 'red,orange,blue'.
                        To customize color for data with multiple groups, the column indicating different groups should be named as 'color' or 'stack'.
                        Users should provide a character strings assigning colors to each group. 
                        For example, 'a:red;b:green;c:blue', in which 'a b c' represent different data groups. 
                        Color for data groups without assigned color would be set as 'grey'. 
                        Hex color codes as '#FF0000' are also supported. See example data for more details.",
                                  placement = "right"
                                )
                              ),
                              
                              choices = c("Random" = "1", "Custom for data with multi-columns" = "2", "Custom for data with multi-groups" = "3"),
                              selected = sam_tra_coltype[x]
                            ),
                            conditionalPanel(
                              condition = paste0("input.sam_tra_coltype",x,"== '2'"),
                              textInput(paste0("sam_tra_colcol",x), NULL, value=sam_tra_colcol[x])
                            ),
                            conditionalPanel(
                              condition = paste0("input.sam_tra_coltype",x," == '3'"),
                              textInput(paste0("sam_tra_colorcus",x), NULL, value=sam_tra_colorcus[x])
                            )
                          )
                        }else{
                          tagList(
                            conditionalPanel(
                              condition = paste0("input.sam_tra_bar_direction",x,"== '1'"),
                              pickerInput(
                                inputId = paste0("sam_tra_coltype",x),
                                label = tags$div(
                                  HTML('<font><h5><i class="fa-solid fa-play"></i><b> Data color</b></font>'),
                                  bs4Dash::tooltip(
                                    actionButton(
                                      inputId = paste0("datvie_tip_bar_sam_trycoltp2",x), 
                                      label="" , 
                                      icon=icon("question"),
                                      status="info",
                                      size = "xs"
                                    ),
                                    title = "The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
                        To customize color for data with multiple columns, users should provide a character string representing one or multiple 
                        colors separated by commas. For example, 'red' or 'red,orange,blue'.if there is only one column of value, only one color needs to be specified.
                        To customize color for data with multiple groups, the column indicating different groups should be named as 'color' or 'stack'.
                        Users should provide a character strings assigning colors to each group. 
                        For example, 'a:red;b:green;c:blue', in which 'a b c' represent different data groups. 
                        Color for data groups without assigned color would be set as 'grey'. 
                        Hex color codes as '#FF0000' are also supported. See example data for more details.",
                                    placement = "right"
                                  )
                                ),
                                choices = c("Random" = "1", "Specific color" = "2","Custom for data with multi-groups" = "3"),
                                selected = sam_tra_coltype[x]
                              ),
                              conditionalPanel(
                                condition = paste0("input.sam_tra_coltype",x,"== '2'"),
                                colourInput(
                                  inputId = paste0("sam_tra_colcol",x),
                                  label = NULL,
                                  value = sam_tra_colcol[x],
                                  returnName = TRUE
                                )
                              ),
                              conditionalPanel(
                                condition = paste0("input.sam_tra_coltype",x," == '3'"),
                                textInput(paste0("sam_tra_colorcus",x), NULL, value=sam_tra_colorcus[x])
                              )
                            )
                          )
                        }
                      },
                      if(sam_tratype[x] == "line" && !(ncol(sam_data.T[[x]])==4 && colnames(sam_data.T[[x]])[4]=="stack")){
                        pickerInput(
                          inputId = paste0("sam_tra_line_fillarea",x),
                          label = tags$div(
                            HTML('<font><h5><i class="fa-solid fa-play"></i><b> Fill area?</b></font>'),
                            bs4Dash::tooltip(
                              actionButton(
                                inputId = paste0("datvie_tip_bar_sam_fillarea",x), 
                                label="" , 
                                icon=icon("question"),
                                status="info",
                                size = "xs"
                              ),
                              title = "Fill the area below the lines.",
                              placement = "right"
                            )
                          ),
                          choices = c("Yes" = "add", "No" = ""),
                          selected=sam_tra_line_fillarea[x]
                        )
                      },
                      if(sam_tratype[x] == "rect-gradual"){
                        tagList(
                          pickerInput(
                            inputId = paste0("sam_tra_trct_colrect",x),
                            label = NULL,
                            choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", "navy", "seagreen", "maroon", "olivedrab", "gold", "lightblue", "navy.yellow", "purple.seagreen", "navy.orange", "navy.cyan", "blue.red", "green.red"),
                            selected = sam_tra_trct_colrect[x]
                          )
                        )
                      },
                      if(sam_tratype[x] == "rect-discrete"){
                        tagList(
                          pickerInput(
                            inputId = paste0("sam_tra_rect_rectcol",x),
                            label = tags$div(
                              HTML('<font><h5><i class="fa-solid fa-play"></i><b> Data color</b></font>'),
                              bs4Dash::tooltip(
                                actionButton(
                                  inputId = paste0("datvie_tip_rect_sam_rectcol",x), 
                                  label="" , 
                                  icon=icon("question"),
                                  status="info",
                                  size = "xs"
                                ),
                                title = "The color to be used to plot the data, which can be random assigned by the application or specified by the users.
                    If 'Specific' was chosen, all data will be filled by a specified color. 
                    If 'Custom' was chosen, the 4th column of the uploaded data should be a categorical character vector with no more than 50 groups.
                    Users should provide values as 'a:red;b:green;c:blue', in which 'a b c' represent different
                    data category indicated by the 4th column of the uploaded data. 
                    Color for data without customed color will be set to NULL. Hex color codes as '#FF0000' are also supported.",
                                placement = "right"
                              )
                            ),
                            choices = c("Random" = "1", "Specific" = "2", "Custom" = "3"),
                            selected = sam_tra_rect_rectcol[x]
                          ),
                          conditionalPanel(
                            condition = paste0("input.sam_tra_rect_rectcol",x,"== '2'"),
                            colourInput(
                              inputId = paste0("sam_tra_rect_rectcoldis",x),
                              label = NULL,
                              value = sam_tra_rect_rectcoldis[x],
                              returnName = TRUE
                            )
                          ),
                          conditionalPanel(
                            condition = paste0("input.sam_tra_rect_rectcol",x,"== '3'"),
                            textInput(paste0("sam_tra_rect_rectcoldiscus",x), NULL, value=sam_tra_rect_rectcoldiscus[x])
                          )
                        )
                      },
                      if(sam_tratype[x] == "line" & !(ncol(sam_data.T[[x]])==4 && colnames(sam_data.T[[x]])[4]=="stack")){
                        tagList(
                          conditionalPanel(
                            condition = paste0("input.sam_tra_line_fillarea",x,"== 'add'"),
                            pickerInput(
                              inputId = paste0("sam_tra_line_selrea",x),
                              label = tags$div(
                                HTML('<font><h5><i class="fa-solid fa-play"></i><b> Color for the filled area </b></font>'),
                                bs4Dash::tooltip(
                                  actionButton(
                                    inputId = paste0("datvie_tip_line_sam_selrea",x), 
                                    label="" , 
                                    icon=icon("question"),
                                    status="info",
                                    size = "xs"
                                  ),
                                  title = "Color used for the filled area, which can be identical with lines' color or specified by the users. If 'Custom' was chosen, all data will be filled by a color specified by the user.",
                                  placement = "right"
                                )
                              ),
                              choices =  c("Identical with lines" = "1", "Custom" = "2"),
                              selected=sam_tra_line_selrea[x]
                            ),
                            conditionalPanel(
                              condition = paste0("input.sam_tra_line_selrea",x,"== '2'"),
                              colourInput(
                                inputId = paste0("sam_tra_bar_borderarea",x),
                                label = NULL,
                                value = sam_tra_bar_borderarea[x],
                                returnName = TRUE
                              )
                            )
                          )
                        )
                      },
                      if(sam_tratype[x] != "heatmap-discrete" & sam_tratype[x] != "heatmap-gradual" & sam_tratype[x] != "ideogram"){
                        numericInput(
                          inputId = paste0("sam_tra_transparency",x),
                          label = tags$div(
                            HTML('<font><h5><i class="fa-solid fa-play"></i><b> Color transparency</b></font>'),
                            bs4Dash::tooltip(
                              actionButton(
                                inputId = paste0("datvie_tip_sam_tra_trans",x), 
                                label="" , 
                                icon=icon("question"),
                                status="info",
                                size = "xs"
                              ),
                              title = "A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.",
                              placement = "right"
                            )
                          ),
                          value=sam_tra_transparency[x],
                          min=0,
                          max=1,
                          step=0.1
                        )
                      },
                      if(sam_tratype[x] == "point" & !("pch" %in% colnames(sam_data.T[[x]]))){
                        textInput(
                          paste0("sam_tra_poipch",x),
                          label = tags$div(
                            HTML('<font><h5><i class="fa-solid fa-play"></i><b> Point shape</b></font>'),
                            bs4Dash::tooltip(
                              actionButton(
                                inputId = paste0("datvie_tip_sam_poi_pch",x), 
                                label="" , 
                                icon=icon("question"),
                                status="info",
                                size = "xs"
                              ),
                              title = "Symbols used for different points. Applicable value can be an integer in [0-25] or an integer vector of arbitrary length adjusted automatically to the number of data categories. Type ?pch in R console for more details.",
                              placement = "right"
                            )
                          ),
                          value= sam_tra_poipch[x]
                        )
                      },
                      if(tra_type[x] == "stack-point"){
                        tagList(
                          textInput(
                            inputId = paste0("sam_tra_poipch",x),
                            label = tags$div(
                              HTML('<font><h5><i class="fa-solid fa-play"></i><b> Point shape</b></font>'),
                              bs4Dash::tooltip(
                                actionButton(
                                  inputId = paste0("datvie_tip_sam_poi_pch",x), 
                                  label="" , 
                                  icon=icon("question"),
                                  status="info",
                                  size = "xs"
                                ),
                                title = "Symbols used for different points. Applicable value can be an integer in [0-25] or an integer vector of arbitrary length adjusted automatically to the number of data categories. Type ?pch in R console for more details.",
                                placement = "right"
                              )
                            ),
                            value= sam_tra_poipch[x]
                          ),
                          numericInput(
                            paste0("sam_tra_poi_poisiz",x),
                            label = tags$div(
                              HTML('<font><h5><i class="fa-solid fa-play"></i><b> Point size</b></font>')
                            ),
                            value=sam_tra_poi_poisiz[x],
                            min=0,
                            max=1.5,
                            step=0.1
                          )
                        )
                      },
                      if(sam_tratype[x] == "point" & !("cex" %in% colnames(sam_data.T[[x]]))){
                        numericInput(
                          paste0("sam_tra_poi_poisiz",x),
                          label = tags$div(
                            HTML('<font><h5><i class="fa-solid fa-play"></i><b> Point size</b></font>')
                          ),
                          value=sam_tra_poi_poisiz[x],
                          min=0,
                          max=1.5,
                          step=0.1
                        )
                      },
                      if(sam_tratype[x] != "rect-discrete" & sam_tratype[x] != "rect-gradual" & sam_tratype[x] != "heatmap-discrete" & sam_tratype[x] != "heatmap-gradual" & sam_tratype[x] != "ideogram" & !(ncol(sam_data.T[[x]])==4 && colnames(sam_data.T[[x]])[4]=="stack")){
                        textInput(
                          inputId = paste0("sam_tra_baseline",x),
                          label = tags$div(
                            HTML('<font><h5><i class="fa-solid fa-play"></i><b> Y-axis coordinates of baselines</b></font>'),
                            bs4Dash::tooltip(
                              actionButton(
                                inputId = paste0("datvie_tip_sam_baseline",x), 
                                label="" , 
                                icon=icon("question"),
                                status="info",
                                size = "xs"
                              ),
                              title = "Decimal numbers in [0, 1] to adjust the Y-axis coordinates of baselines. Numeric vector of arbitrary length is also accepted. For example, '0.5' or '0.25,0.5,0.75'.",
                              placement = "right"
                            )
                          ),
                          value=sam_tra_baseline[x]
                        )
                      },
                      if(sam_tratype[x] != "rect-discrete" & sam_tratype[x] != "rect-gradual" & sam_tratype[x] != "heatmap-discrete" & sam_tratype[x] != "heatmap-gradual" & sam_tratype[x] != "ideogram" & !(sam_tratype[x]=="line" && (ncol(sam_data.T[[x]])==4 && colnames(sam_data.T[[x]])[4]=="stack"))){
                        colourInput(
                          inputId = paste0("sam_tra_colorline",x),
                          label = tags$div(
                            HTML('<font><h5><i class="fa-solid fa-play"></i><b> Baselines color(s)</b></font>'),
                            bs4Dash::tooltip(
                              actionButton(
                                inputId = paste0("datvie_tip_sam_baselinecol",x), 
                                label="" , 
                                icon=icon("question"),
                                status="info",
                                size = "xs"
                              ),
                              title = "The color to be used for the baselines which can be NULL or a character vector of arbitrary length adjusted automatically to the number of baselines. For example, 'grey' or 'red,green'. Hex color codes as '#FF0000' are also supported.",
                              placement = "right"
                            )
                          ),
                          value = sam_tra_colorline[x],
                          returnName = TRUE
                        )
                      },
                      if(sam_tratype[x] != "heatmap-discrete" & sam_tratype[x] != "heatmap-gradual" & sam_tratype[x] != "ideogram"){
                        colourInput(
                          inputId = paste0("sam_tra_bgcol",x),
                          label = tags$div(
                            HTML('<font><h5><i class="fa-solid fa-play"></i><b> Background color(s)</b></font>'),
                            bs4Dash::tooltip(
                              actionButton(
                                inputId = paste0("datvie_tip_sam_bgcol",x), 
                                label="" , 
                                icon=icon("question"),
                                status="info",
                                size = "xs"
                              ),
                              title = "The color to be used for the background of the plot which can be null or a color vector of arbitrary length adjusted 
                  automatically to the number of sectors. For example, 'grey95' or 'grey95,grey,pink,yellow'. Hex color codes as '#FF0000' are also supported.",
                              placement = "right"
                            )
                          ),
                          value = sam_tra_bgcol[x],
                          returnName = TRUE
                        )
                      },
                      if(sam_tratype[x] == "heatmap-gradual"){
                        tagList(
                          pickerInput(
                            inputId = paste0("sam_tra_hmap_heatmapcol",x),
                            label = tags$div(
                              HTML('<font><h5><i class="fa-solid fa-play"></i><b> Colors</b></font>'),
                              bs4Dash::tooltip(
                                actionButton(
                                  inputId = paste0("datvie_tip_heat_sam_col",x), 
                                  label="" , 
                                  icon=icon("question"),
                                  status="info",
                                  size = "xs"
                                ),
                                title = "Colors to be used for the heatmap, which can be assigned by the application or specified by the users.",
                                placement = "right"
                              )
                            ),
                            choices = c("Typical" = "1", "Custom" = "2"),
                            selected = sam_tra_hmap_heatmapcol[x]
                          ),
                          conditionalPanel(
                            condition = paste0("input.sam_tra_hmap_heatmapcol",x,"== '1'"),
                            pickerInput(
                              inputId = paste0("sam_tra_hmap_typcolhmap",x),
                              label = NULL,
                              choices =  c("blue.white.red", "green.black.red", "green.yellow.red", "purple.yellow.red", "blue.green.red", "blue.yellow.green", "cyan.white.deeppink1"),
                              selected = sam_tra_hmap_typcolhmap[x]
                            )
                          ),
                          conditionalPanel(
                            condition = paste0("input.sam_tra_hmap_heatmapcol",x,"== '2'"),
                            fluidRow(
                              column(
                                width = 4,
                                colourInput(
                                  inputId = paste0("sam_tra_hmap_lowColor",x),
                                  label = "lowColor",
                                  value = sam_tra_hmap_lowColor[x],
                                  returnName = TRUE
                                )
                              ),
                              column(
                                width = 4,
                                colourInput(
                                  inputId = paste0("sam_tra_hmap_midColor",x),
                                  label = "midColor",
                                  value = sam_tra_hmap_midColor[x],
                                  returnName = TRUE
                                )
                              ),
                              column(
                                width = 4,
                                colourInput(
                                  inputId = paste0("sam_tra_hmap_highColor",x),
                                  label = "highColor",
                                  value = sam_tra_hmap_highColor[x],
                                  returnName = TRUE
                                )
                              )
                            )
                          ),
                          pickerInput(
                            inputId = paste0("sam_tra_hmap_poslines",x),
                            label = tags$div(
                              HTML('<font><h5><i class="fa-solid fa-play"></i><b> Add position lines?</b></font>'),
                              bs4Dash::tooltip(
                                actionButton(
                                  inputId = paste0("datvie_tip_heat_sam_posline",x), 
                                  label="" , 
                                  icon=icon("question"),
                                  status="info",
                                  size = "xs"
                                ),
                                title = "Add genomic position lines between tracks, which can be used to identify the correspondance between heatmaps and genomic regions.",
                                placement = "right"
                              )
                            ),
                            choices = c("Yes" = "1", "No" = "2"),
                            selected = sam_tra_hmap_poslines[x]
                          ),
                          conditionalPanel(
                            condition = paste0("input.sam_tra_hmap_poslines",x,"== '1'"),
                            numericInput(
                              inputId = paste0("sam_tra_hmap_poslinhei",x),
                              label = tags$div(
                                HTML('<font><h5><i class="fa-solid fa-play"></i><b> Position lines height</b></font>'),
                                bs4Dash::tooltip(
                                  actionButton(
                                    inputId = paste0("datvie_tip_heat_sam_poslinehei",x), 
                                    label="" , 
                                    icon=icon("question"),
                                    status="info",
                                    size = "xs"
                                  ),
                                  title = "Height of the position lines.",
                                  placement = "right"
                                )
                              ),
                              value=sam_tra_hmap_poslinhei[x],
                              min=0,
                              max=0.8,
                              step=0.01
                            )
                          )
                        )
                      },
                      if(sam_tratype[x] == "heatmap-discrete"){
                        tagList(
                          pickerInput(
                            inputId = paste0("sam_tra_heatcol_dis",x),
                            label = tags$div(
                              HTML('<font><h5><i class="fa-solid fa-play"></i><b> Data color</b></font>'),
                              bs4Dash::tooltip(
                                actionButton(
                                  inputId = paste0("datvie_tip_sam_heat_colorsel",x), 
                                  label="" , 
                                  icon=icon("question"),
                                  status="info",
                                  size = "xs"
                                ),
                                title = "The color to be used to plot the data, which can be random assigned by the application or specified by the users.
                    If 'Specific' was chosen, all data will be filled by a specified color. 
                    If 'Custom' was chosen, the 4th column of the uploaded data should be a categorical character vector with no more than 50 groups.
                    Users should provide values as 'a:red;b:green;c:blue', in which 'a b c' represent different
                    data category indicated by the 4th column of the uploaded data. 
                    Color for data without customed color will be set to NULL. Hex color codes as '#FF0000' are also supported.",
                                placement = "right"
                              )
                            ),
                            #label = "Data color",
                            choices = c("Random" = "1" , "Custom" = "2"),
                            selected = sam_tra_heatcol_dis[x]
                          ),
                          conditionalPanel(
                            condition = paste0("input.sam_tra_heatcol_dis",x,"== '2'"),
                            textInput(
                              inputId =  paste0("sam_tra_heat_heatcoldiscus",x), 
                              label =  NULL,
                              value = sam_tra_heat_heatcoldiscus[x])
                          ),
                          pickerInput(
                            inputId = paste0("sam_tra_hmap_poslines",x),
                            label = tags$div(
                              HTML('<font><h5><i class="fa-solid fa-play"></i><b> Add position lines?</b></font>'),
                              bs4Dash::tooltip(
                                actionButton(
                                  inputId = paste0("datvie_tip_sam_heat_posline2",x), 
                                  label="" , 
                                  icon=icon("question"),
                                  status="info",
                                  size = "xs"
                                ),
                                title = "Add genomic position lines between tracks, which can be used to identify the correspondance between heatmaps and genomic regions.",
                                placement = "right"
                              )
                            ),
                            choices = c("Yes" = "1", "No" = "2"),
                            selected = sam_tra_hmap_poslines[x]
                          ),
                          conditionalPanel(
                            condition = paste0("input.sam_tra_hmap_poslines",x,"== '1'"),
                            numericInput(
                              inputId = paste0("sam_tra_hmap_poslinhei",x),
                              label = tags$div(
                                HTML('<font><h5><i class="fa-solid fa-play"></i><b> Position lines height</b></font>'),
                                bs4Dash::tooltip(
                                  actionButton(
                                    inputId = paste0("datvie_tip_heat_sam_poslinehei2",x), 
                                    label="" , 
                                    icon=icon("question"),
                                    status="info",
                                    size = "xs"
                                  ),
                                  title = "Height of the position lines.",
                                  placement = "right"
                                )
                              ),
                              value=sam_tra_hmap_poslinhei[x],
                              min=0,
                              max=0.8,
                              step=0.01
                            )
                          )
                        )
                      },
                      numericInput(
                        inputId = paste0("sam_heightTraus",x),
                        label = tags$div(
                          HTML('<font><h5><i class="fa-solid fa-play"></i><b> Track height</b></font>'),
                          bs4Dash::tooltip(
                            actionButton(
                              inputId = paste0("datvie_tip_sam_trahei",x), 
                              label="" , 
                              icon=icon("question"),
                              status="info",
                              size = "xs"
                            ),
                            title = "Height of the current track.",
                            placement = "right"
                          )
                        ),
                        value = sam_heightTraus[x],
                        min=0.01,
                        max=0.8,
                        step=0.01
                      ),
                      if(x < tra_len){
                        numericInput(
                          inputId = paste0("sam_Tra_margin",x),
                          label = tags$div(
                            HTML('<font><h5><i class="fa-solid fa-play"></i><b> Distance to the next section (track, label data, or link data)</b></font>'),
                            bs4Dash::tooltip(
                              actionButton(
                                inputId = paste0("datvie_tip_sam_tramar",x), 
                                label="" , 
                                icon=icon("question"),
                                status="info",
                                size = "xs"
                              ),
                              title = "This parameter can also be used to tune the distance between a track and a label data, or the distance between a track and a link data.",
                              placement = "right"
                            )
                          ),
                          
                          value=sam_Tra_margin[x],
                          min=0,
                          max=0.1,
                          step=0.01
                        )
                      },
                      if(sam_tratype[x] == "heatmap-gradual"){
                        tagList(
                          pickerInput(
                            inputId = paste0("sam_tra_hmap_cellbord",x),
                            label = tags$div(
                              HTML('<font><h5><i class="fa-solid fa-play"></i><b> Add cell borders?</b></font>'),
                              bs4Dash::tooltip(
                                actionButton(
                                  inputId = paste0("datvie_tip_sam_heaat_cellbo",x), 
                                  label="" , 
                                  icon=icon("question"),
                                  status="info",
                                  size = "xs"
                                ),
                                title = "Add borders to the heatmap grids, which can be used to separate different cells from each other.",
                                placement = "right"
                              )
                            ),
                            choices = c("Yes" = "add", "No" = ""),
                            selected = sam_tra_hmap_cellbord[x]
                          ),
                          conditionalPanel(
                            condition = paste0("input.sam_tra_hmap_cellbord",x,"== 'add'"),
                            colourInput(
                              inputId = paste0("sam_tra_hmap_cellbord_col",x),
                              label = tags$div(
                                HTML('<font><h5><i class="fa-solid fa-play"></i><b> Color of cell borders?</b></font>'),
                                bs4Dash::tooltip(
                                  actionButton(
                                    inputId = paste0("datvie_tip_sam_heaat_bocol",x), 
                                    label="" , 
                                    icon=icon("question"),
                                    status="info",
                                    size = "xs"
                                  ),
                                  title = "The color to be used for the borders of heatmap grids. For example, 'white' or 'red'. Hex color codes as '#FF0000' are also supported.",
                                  placement = "right"
                                )
                              ),
                              value = sam_tra_hmap_cellbord_col[x],
                              returnName = TRUE
                            )
                          )
                        )
                      },
                      if(sam_tratype[x] != "heatmap-discrete" & sam_tratype[x] != "heatmap-gradual" & sam_tratype[x] != "ideogram"){
                        pickerInput(
                          inputId = paste0("sam_tra_border",x),
                          label = tags$div(
                            HTML('<font><h5><i class="fa-solid fa-play"></i><b> Add sector borders?</b></font>'),
                            bs4Dash::tooltip(
                              actionButton(
                                inputId = paste0("datvie_tip_sam_tra_bo",x), 
                                label="" , 
                                icon=icon("question"),
                                status="info",
                                size = "xs"
                              ),
                              title = "Add borders to each sector.",
                              placement = "right"
                            )
                          ),
                          choices = c("Yes" = "add", "No" = ""),
                          selected= sam_tra_border[x]
                        )
                        
                      },
                      if(sam_tratype[x] == "point"|sam_tratype[x] == "line"|sam_tratype[x] == "bar"){
                        pickerInput(
                          inputId = paste0("sam_tra_yaxis",x),
                          label = tags$div(
                            HTML('<font><h5><i class="fa-solid fa-play"></i><b> Add Y-axis?</b></font>'),
                            bs4Dash::tooltip(
                              actionButton(
                                inputId = paste0("datvie_tip_sam_tra_yax",x), 
                                label="" , 
                                icon=icon("question"),
                                status="info",
                                size = "xs"
                              ),
                              title = "Add Y-axis for all the tracks if applicable.",
                              placement = "right"
                            )
                          ),
                          choices = c("Yes" = "1", "No" = "2"),
                          selected= sam_tra_yaxis[x]
                        )
                      }
                      
                    )
                  )
                )
              )
            })
          )
        },
        if(!is.null(sam_data.N)){
          bs4Card(
            collapsible = FALSE,
            title = HTML('<i class="fa-solid fa-circle"></i> Label data (used to label elements in a track)'),
            width = 12,
            tagList(
              fluidRow(
                column(
                  6,
                  tags$div(
                    HTML('<font color="#2196F3"><h4><i class="fa-solid fa-play"></i> File name</font>'),
                    bs4Dash::tooltip(
                      actionButton(
                        inputId = "datvie_lab_tip1_sam", 
                        label="" , 
                        icon=icon("question"),
                        status="info",
                        size = "xs"
                      ),
                      title = "Filenames of the input datasets.",
                      placement = "bottom"
                    )
                  )
                ),
                column(
                  6,
                  tags$div(
                    HTML('<font color="#2196F3"><h4><i class="fa-solid fa-play"></i> Label index</font>'),
                    bs4Dash::tooltip(
                      actionButton(
                        inputId = "datvie_tip_sam_labidx", 
                        label="" , 
                        icon=icon("question"),
                        status="info",
                        size = "xs"
                      ),
                      title = "The track index for the label data. Elements in the selected track will be labeled by the label data.",
                      placement = "bottom"
                    )
                  )
                )
              ),
              lapply(1:lab_len,function(x){
                fluidRow(
                  column(
                    6,
                    fluidRow(
                      column(
                        10,
                        h4(paste0(name_data.N[x]))
                      ),
                      column(
                        2,
                        bs4Dash::tooltip(
                          actionBttn(
                            inputId = paste0("sam_view_lab_data",x),
                            label = NULL,
                            style = "unite",
                            color = "success",
                            icon = icon("eye")
                          ),
                          title = paste0("Click to view the dataset."),
                          placement = "bottom"
                        )
                      )
                    )
                  ),
                  column(
                    6,
                    fluidRow(
                      column(
                        11,
                        pickerInput(
                          inputId = paste0("labpos",x),
                          label = NULL,
                          choices = 0:tra_len,
                          selected = sam_labpos[x]
                        )
                      ),
                      
                      column(
                        1,
                        bs4Dash::tooltip(
                          actionBttn(
                            inputId = paste0("sam_lab_setting",x),
                            label = NULL,
                            style = "unite",
                            color = "success",
                            icon = icon("gear")
                          ),
                          title = paste0("Set parameters for the label data. NOTE: Changes will not be applied for example datasets."),
                          placement = "bottom"
                        )
                      )
                    )
                    
                  ),
                  tags$head(tags$style(paste0("#jquicirpar_labview_sam",x," .modal-dialog{ max-width:1200px}"))),
                  jqui_draggable(
                    bsModal(
                      id = paste0("jquicirpar_labview_sam",x),
                      title = NULL,
                      trigger = paste0("sam_view_lab_data",x),
                      size = "large",
                      DTOutput(paste0("viewLab_sam",x))
                    )
                  ),
                  tags$head(tags$style(paste0("#jquidatvie_labsetting_sam",x," .modal-dialog{ width:1200px}"))),
                  jqui_draggable(
                    bsModal(
                      id = paste0("jquidatvie_labsetting_sam",x),
                      title = NULL,
                      trigger = paste0("sam_lab_setting",x),
                      size = "large",
                      tagList(
                        numericInput(
                          inputId = paste0("sam_lab_fontsize",x),
                          label = tags$div(
                            HTML('<font><h5><i class="fa-solid fa-play"></i><b> Label data hight</b></font>'),
                            bs4Dash::tooltip(
                              actionButton(
                                inputId = paste0("datvie_tip_sam_lab_labhi",x), 
                                label="" , 
                                icon=icon("question"),
                                status="info",
                                size = "xs"
                              ),
                              title = "The height of the track occupied by the label.",
                              placement = "right"
                            )
                          ),
                          value= sam_lab_fontsize[x], 
                          min=0.1, 
                          max=0.8,
                          step=0.1
                        ),
                        sliderTextInput(
                          inputId = paste0("sam_lab_fontper",x),
                          label = tags$div(
                            HTML('<font><h5><i class="fa-solid fa-play"></i><b> Font size (%)</b></font>'),
                            bs4Dash::tooltip(
                              actionButton(
                                inputId = paste0("sam_datvie_tip_lab_adjustfontsize",x),
                                label="" ,
                                icon=icon("question"),
                                status="info",
                                size = "xs"
                              ),
                              title = "The font size is automatically adjusted to the height of the Label. Additionaly, you can zoom the font size using this slider.",
                              placement = "right"
                            )
                          ),
                          choices = c(10:200),
                          selected = 100
                        ),
                        colourInput(
                          inputId = paste0("sam_lab_fontcol",x),
                          label = tags$div(
                            HTML('<font><h5><i class="fa-solid fa-play"></i><b> Font color</b></font>'),
                            bs4Dash::tooltip(
                              actionButton(
                                inputId = paste0("datvie_tip_sam_lab_labcol",x), 
                                label="" , 
                                icon=icon("question"),
                                status="info",
                                size = "xs"
                              ),
                              title = "The color of the label text and the connection line.",
                              placement = "right"
                            )
                          ),
                          returnName = TRUE,
                          value = sam_lab_fontcol[x]
                        ),
                        pickerInput(
                          inputId = paste0("sam_poslabels",x),
                          label = tags$div(
                            HTML('<font><h5><i class="fa-solid fa-play"></i><b> Label Position</b></font>'),
                            bs4Dash::tooltip(
                              actionButton(
                                inputId = paste0("datvie_tip_sam_lab_labpos",x), 
                                label="" , 
                                icon=icon("question"),
                                status="info",
                                size = "xs"
                              ),
                              title = "Place the labels in the inner or the outer of the track?",
                              placement = "right"
                            )
                          ),
                          choices = c("Inward", "Outward"),
                          selected = sam_poslabels[x]
                        )
                      )
                    )
                  )
                )
              })
            )
            
            
          )
          
        },
        if(!is.null(sam_data.L)){
          bs4Card(
            collapsible = FALSE,
            title = HTML('<i class="fa-solid fa-circle"></i> Links data'),
            width = 12,
            tagList(
              fluidRow(
                column(
                  6,
                  tags$div(
                    HTML('<font color="#2196F3"><h4><i class="fa-solid fa-play"></i> File name</font>'),
                    bs4Dash::tooltip(
                      actionButton(
                        inputId = "datvie_link_tip1_sam", 
                        label="" , 
                        icon=icon("question"),
                        status="info",
                        size = "xs"
                      ),
                      title = "Filenames of the input datasets.",
                      placement = "bottom"
                    )
                  )
                ),
                column(
                  6,
                  tags$div(
                    HTML('<font color="#2196F3"><h4><i class="fa-solid fa-play"></i> Linkdata format</font>'),
                    bs4Dash::tooltip(
                      actionButton(
                        inputId = "datvie_link_tip2_sam", 
                        label="" , 
                        icon=icon("question"),
                        status="info",
                        size = "xs"
                      ),
                      title = "The format of links data specified by the user. For data with 6 columns, user should select 'Data without a color column'. 
	For data with 7 columns, user should select 'Data with multi-groups' if the 'color' column represents different categories indicated by a character string as 'a, b, c'. 
	For data with 7 columns, user should select 'Data with gradual values' if the 'color' column represents gradual values indicated by numbers as '1, 2, 3'.",
                      placement = "bottom"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  6,
                  fluidRow(
                    column(
                      10,
                      h4(paste0(name_data.L))
                    ),
                    column(
                      2,
                      bs4Dash::tooltip(
                        actionBttn(
                          inputId = "sam_view_lin_data",
                          label = NULL,
                          style = "unite",
                          color = "success",
                          icon = icon("eye")
                        ),
                        title = paste0("Click to view the dataset."),
                        placement = "bottom"
                      )
                    )
                  )
                ),
                column(
                  6,
                  fluidRow(
                    column(
                      11,
                      pickerInput(
                        inputId = "sam_colformatLinks",
                        label = NULL,
                        choices = c("Data without a 'color' column" = "1","Data with multi-groups" = "2", "Data with gradual values" = "3"),
                        selected = sam_colformatLinks
                      )
                    ),
                    column(
                      1,
                      bs4Dash::tooltip(
                        actionBttn(
                          inputId = "sam_linsetting",
                          label = NULL,
                          style = "unite",
                          color = "success",
                          icon = icon("gear")
                        ),
                        title = paste0("Set parameters for Links data. NOTE: Changes will not be applied for example datasets."),
                        placement = "bottom"
                      )
                    )
                  )
                ),
                tags$head(tags$style(paste0("#jquidatvie_linview_sam .modal-dialog{ max-width:1200px}"))),
                jqui_draggable(
                  bsModal(
                    id = "jquidatvie_linview_sam",
                    title = NULL,
                    trigger = "sam_view_lin_data",
                    size = "large",
                    DTOutput("viewlink_sam")
                  )
                ),
                tags$head(tags$style(paste0("#jquidatvie_linsetting_sam .modal-dialog{ width:1200px}"))),
                jqui_draggable(
                  bsModal(
                    id = "jquidatvie_linsetting_sam",
                    title = NULL,
                    trigger = "sam_linsetting",
                    size = "large",
                    conditionalPanel(
                      condition="input.sam_colformatLinks=='1' | input.sam_colformatLinks=='2'",
                      pickerInput(
                        inputId = "sam_colorLinks",
                        label = tags$div(
                          HTML('<font><h5><i class="fa-solid fa-play"></i><b> Data color</b></font>'),
                          bs4Dash::tooltip(
                            actionButton(
                              inputId = "datvie_link_sam_collink", 
                              label="" , 
                              icon=icon("question"),
                              status="info",
                              size = "xs"
                            ),
                            title = "Links are filled with colors random assigned by the system or colors specified by the user. For data with 6 columns, format of specified color as 'red' or 'green' is supported. For data with 7 columns, format of specified color as 'a:red;b:green;c:blue' is supported. 'a', 'b', 'c' represents different categories indicated by the 7th column. Color for data groups without assigned color would be set as 'grey'.",
                            placement = "bottom"
                          )
                        ),
                        choices = c("Random" = "1", "Specific" = "2"),
                        selected=sam_colorLinks
                      ),
                      conditionalPanel(
                        condition="input.sam_colorLinks==2",
                        colourInput(
                          inputId = "sam_selcolorLinks",
                          label = NULL,
                          value = sam_selcolorLinks
                        )
                      ),
                      numericInput(
                        inputId = "sam_transparencyLinks",
                        label = tags$div(
                          HTML('<font><h5><i class="fa-solid fa-play"></i><b> Color transparency</b></font>'),
                          bs4Dash::tooltip(
                            actionButton(
                              inputId = "datvie_link_tra_lintran", 
                              label="" , 
                              icon=icon("question"),
                              status="info",
                              size = "xs"
                            ),
                            title = "A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.",
                            placement = "bottom"
                          )
                        ),
                        value=sam_transparencyLinks,
                        min=0,
                        max=1,
                        step=0.1
                      )
                    ),
                    conditionalPanel(
                      condition="input.sam_colformatLinks==3",
                      tags$div(
                        HTML('<font><h5><i class="fa-solid fa-play"></i><b> Data color</b></font>'),
                        bs4Dash::tooltip(
                          actionButton(
                            inputId = "datvie_link_sam_tip5", 
                            label="" , 
                            icon=icon("question"),
                            status="info",
                            size = "small"
                          ),
                          title = "For data with a 'color' column indicated by numbers, links are filled with colors specified by the user."
                        )
                      ),
                      fluidRow(
                        column(
                          width = 4,
                          
                          colourInput(
                            "sam_lowColinks",
                            label = HTML('<p><font size="3"><i class="fa-solid fa-circle"></i><strong> Low Color</strong></font></p>'),
                            value = sam_lowColinks,
                            returnName = TRUE
                          )
                        ),
                        column(
                          4,
                          colourInput(
                            "sam_midColinks",
                            label = HTML('<p><font size="3"><i class="fa-solid fa-circle"></i><strong> Middle Color</strong></font></p>'),
                            value = sam_midColinks,
                            returnName = TRUE
                          )
                        ),
                        column(
                          4,
                          colourInput(
                            "sam_highColinks",
                            label = HTML('<p><font size="3"><i class="fa-solid fa-circle"></i><strong> High Color</strong></font></p>'),
                            value = sam_highColinks,
                            returnName = TRUE
                          )
                        )
                      ),
                      HTML('<br>')
                    )
                  )
                )
              )
            )
          )
          
        }
        
        
      )
      
    })
  })
  
  observeEvent(input$sam_dat_vie_ok, {
    sam_heatmapcols <<- lapply(1:length(sam_data.T), function(x){
      c(sam_tra_hmap_lowColor[[x]],sam_tra_hmap_midColor[[x]],sam_tra_hmap_highColor[[x]])
    })
    sam_gracolinks <<- c(input$sam_lowColinks,input$sam_midColinks,input$sam_highColinks)
    sam_colorChr_in <- gsub("\\s","",strsplit(sam_colorChr,",")[[1]])
    sam_pospos <- 1:length(sam_data.T)
    sam_midplot <- FALSE
    sam_outergap <- 0
    sam_trac_index <- "No"
    plotfig(input = input, output = output,session=session,data.C = sam_data.C , dis_Chr = sam_distance_Chr , data.T = sam_data.T , data.L = sam_data.L, data.N = sam_data.N , tra_Margin = sam_Tra_margin , labels_inf = sam_labels_inf , labelChr = sam_labelChr,colorChr=sam_colorChr_in , tra_hmap_typcolhmap = sam_tra_hmap_typcolhmap , tra_border = sam_tra_border , tra_yaxis = sam_tra_yaxis ,
            trackChr = sam_trackChr ,tratype = sam_tratype,source_data = sam_source_data,chr_height = sam_heightChr, heightTra = sam_heightTraus , datatypeChr = sam_datatypeChr, tra_poi_poisiz = sam_tra_poi_poisiz , heatmapcols = sam_heatmapcols , tra_bgcol = sam_tra_bgcol , gap.width = sam_gapChr ,
            tra_hmap_poslines = sam_tra_hmap_poslines , tra_hmap_poslinhei = sam_tra_hmap_poslinhei , tra_hmap_cellbord = sam_tra_hmap_cellbord , tra_hmap_cellbord_col = sam_tra_hmap_cellbord_col , tra_hmap_heatmapcol = sam_tra_hmap_heatmapcol , plotsize = sam_plotsize ,
            tra_rect_rectcol = sam_tra_rect_rectcol , tra_trct_colrect = sam_tra_trct_colrect , tra_rect_rectcoldis = sam_tra_rect_rectcoldis , tra_rect_rectcoldiscus = sam_tra_rect_rectcoldiscus , tra_transparency = sam_tra_transparency , tra_coltype = sam_tra_coltype , tra_colcol = sam_tra_colcol , tra_heatcol_dis = sam_tra_heatcol_dis , tra_heat_heatcoldiscus = sam_tra_heat_heatcoldiscus,
            tra_colorcus = sam_tra_colorcus , tra_line_fillarea = sam_tra_line_fillarea , tra_poipch = sam_tra_poipch , tra_colorline = sam_tra_colorline , tra_baseline = sam_tra_baseline , outAxis = sam_outAxis , fontSize = fontSize , outAxis_size = sam_outAxis_size , labelChr_size = sam_labelChr_size , tra_bar_direction = sam_tra_bar_direction ,
            tra_bar_Boundary = sam_tra_bar_Boundary , tra_bar_coldir1 = sam_tra_bar_coldir1 , tra_bar_coldir2 = sam_tra_bar_coldir2 , hltTrack.List = hltTrack.List , hltdata.List = hltdata.List , tra_line_selrea = sam_tra_line_selrea , tra_bar_borderarea = sam_tra_bar_borderarea , colformatLinks = sam_colformatLinks , colorLinks = sam_colorLinks , outergap = sam_outergap ,
            selcolorLinks = sam_selcolorLinks , transparencyhltLinks = transparencyhltLinks , gracolinks =  sam_gracolinks , transparencyLinks = sam_transparencyLinks , legendpos = sam_legendpos , addlegend = sam_addlegend , hlt_data = sam_hlt_data , midplot = sam_midplot , trapos = sam_pospos , trac_index = sam_trac_index)
    
    
    
  })
  ## *** Upload Data ***
  observeEvent(input$alldata, {
    ### alert box
    if(is.null(input$alldata)){
      sendSweetAlert(
        session = session,
        title = "",
        text = "The file connot be empty.",
        type = "error"
      )
    }else{
      sendSweetAlert(
        session = session,
        title = "",
        text = "Data uploaded successfully!",
        type = "success"
      )
    }
    ###
    file <- input$alldata
    filename <- file$name
    if(!is.null(file)){
      output$dataclassify <<- renderUI({
        tagList(
          tags$div(
            HTML('<font><h4><i class="fa-solid fa-play"></i> Step 2.1. Please distribute the uploaded datasets to appropriate data groups:</font>'),
            bs4Dash::tooltip(
              actionButton(
                inputId = "datup_tip3", 
                label="" , 
                icon=icon("question"),
                status="info",
                size = "xs"
              ),
              title = "Please drag the uploaded datasets from the 'Candidate area' to the box of 'Chromosome data' or 'Track data' or 'Label data' or 'Links data', and then move to Step 2.2.",
              placement = "bottom"
            )
          ),
          bucket_list(
            header = NULL,
            group_name = "bucket_list_group",
            orientation = "horizontal",
            add_rank_list(
              text = "Candidate area",
              labels = filename,
              input_id = "rank_list_1"
            ),
            add_rank_list(
              text = "* Chromosome data",
              labels = chrdatas,
              input_id = "chrdata"
            ),
            add_rank_list(
              text = "Track data",
              labels = tradatas,
              input_id = "tradata"
            ),
            add_rank_list(
              text = "Label data",
              labels = labdatas,
              input_id = "labdata"
            ),
            add_rank_list(
              text = "Links data",
              labels = lindatas,
              input_id = "lindata"
            ),
            add_rank_list(
              text = "Garbage",
              labels = NULL,
              input_id = "garbage"
            )
          )
        )
      })
    }
  })
  
  observeEvent(input$save1,{
    filename <<-NULL
    chrdatas <<- input$chrdata
    tradatas <<- input$tradata
    lindatas <<- input$lindata
    labdatas <<- input$labdata
    garbage <<- input$garbage
    alldatapath <<- unique(rbind(input$alldata,alldatapath))
    alldatapath <<- alldatapath[!duplicated(alldatapath$name),]
    
    
    
    lonalldat <<- c(length(chrdatas),length(tradatas),length(lindatas),length(labdatas))
    ### alert box
    if(lonalldat[1] > 1){
      sendSweetAlert(
        session = session,
        title = "",
        text = "Only a single chromosome dataset is allowed.",
        type = "error"
      )
    }else if(lonalldat[3] > 1){
      sendSweetAlert(
        session = session,
        title = "",
        text = "Only a single Links dataset is allowed.",
        type = "error"
      )
    }else{
      sendSweetAlert(
        session = session,
        title = NULL,
        text = tags$div(HTML("<div style='white-space: pre-wrap;'>Input datasets saved and you can add more datasets by repeating Step 2.<br/>You can also finish data uploading and move to Step 3.</div>")),
        type = "success"
      )
    }  
    ###
  })
  
  observeEvent(input$dataup_go,{
    dataview_export <<- NULL
    dataall <- c(chrdatas,tradatas,lindatas,labdatas)
    datarepet <- TRUE %in% duplicated(dataall)
    if(datarepet == TRUE){
      sendSweetAlert(
        session = session,
        title = "",
        text = "Duplicated datasets found. Please drag the duplicated datasets into the 'Garbage' area.",
        type = "error"
      )
    }else{
      if(lonalldat[1] == 0){
        sendSweetAlert(
          session = session,
          title = "",
          text = "Chromosome data can not be empty.",
          type = "error"
        )
      }else{
        chrfil <- alldatapath[which(alldatapath[1] == chrdatas[1]),4]
        if(!grepl("text",alldatapath[which(alldatapath[1] == chrdatas[1]),3])){
          sendSweetAlert(
            session = session,
            title = "",
            text = "Please check the format of the Chromosome data.",
            type = "error"
          )
        }else if(!all(grepl("text",alldatapath[which(alldatapath[1] == tradatas),3]))){
          sendSweetAlert(
            session = session,
            title = "",
            text = "Please check the format of the Track data.",
            type = "error"
          )
        }else if(!all(grepl("text",alldatapath[which(alldatapath[1] == labdatas),3]))){
          sendSweetAlert(
            session = session,
            title = "",
            text = "Please check the format of the Label data.",
            type = "error"
          )
        }else if(!all(grepl("text",alldatapath[which(alldatapath[1] == lindatas),3]))){
          sendSweetAlert(
            session = session,
            title = "",
            text = "Please check the format of the Link data.",
            type = "error"
          )
        }else if(length(lindatas) > 1){
          sendSweetAlert(
            session = session,
            title = "",
            text = "Only a single Links dataset is allowed.",
            type = "error"
          )
        }else if(length(chrdatas) != 1){
          sendSweetAlert(
            session = session,
            title = "",
            text = "Only a single Chromosome dataset is allowed.",
            type = "error"
          )
        }else{
          
          data.C <<- fread(chrfil,data.table = FALSE,stringsAsFactors = F)
          tra_a <- length(tradatas)
          if(tra_a >= 1) {
            trafil <- lapply(1:tra_a, function(x) {
              alldatapath[which(alldatapath[1] == tradatas[x]), 4]
            })
            trafil <- unlist(trafil)
            data.T <<- lapply(1:tra_a, function(x) {
              tra_b <- trafil[[x]]
              data.frame(fread(tra_b), stringsAsFactors = F)
            })
          } else{
            data.T <<- NULL
          }
          
          lin_a <- length(lindatas)
          linfil <- lapply(1:lin_a,function(x){
            alldatapath[which(alldatapath[1] == lindatas[x]),4]
          })
          linfil <- unlist(linfil)
          if(lin_a >= 1) {
            lin_b <- linfil[[1]]
            data.L <<- data.frame(fread(lin_b), stringsAsFactors = F)
          } else{
            data.L <<- NULL
          }
          #
          lab_a <- length(labdatas)
          labfil <- lapply(1:lab_a, function(x) {
            alldatapath[which(alldatapath[1] == labdatas[x]), 4]
          })
          labfil <- unlist(labfil)
          if (lab_a >= 1) {
            data.N <<- lapply(1:lab_a, function(x) {
              lab_b <- labfil[[x]]
              data.frame(fread(lab_b), stringsAsFactors = F)
            })
          } else{
            data.N <<- NULL
          }
          #
          unichr <- unique(data.C[,1])
          if(tra_a >= 1) {
            unitra <- unique(unlist(lapply(1:length(data.T), function(x) {
              data.T[[x]][, 1]
            })))
          } else{
            unitra <- NULL
          }
          if (lab_a >= 1) {
            unilab <- unique(unlist(lapply(1:length(data.N), function(x) {
              data.N[[x]][, 1]
            })))
          } else{
            unilab <- NULL
          }
          if (!is.null(data.L)) {
            unilin <- unique(data.L[, 1])
          } else{
            unilin <- NULL
          }
          uniall <- unique(c(unitra,unilab,unilin))
          
          alldatalist <- c(list(data.C),data.T,list(data.L),data.N)
          alldatalist <- alldatalist[!sapply(alldatalist, is.null)]
          names(alldatalist) <- dataall
          
          primessage <- sapply(alldatalist, function(x){
            
            if (!all(is.character(x[, 1]))) {
              return("The first column should be characters.")
            } else if (!all(is.numeric(x[, 3]), is.numeric(x[, 2]))) {
              return("The 2nd and 3rd columns of the data should be numeric.")
            } else if (!all(x[, 2] > 0)) {
              return("Values of the 'start' column should be larger than 0.")
            } else if (!all((x[, 3] > x[, 2]) == TRUE)) {
              return("Values of the 'end' column should be larger than the 'start' column.")
            } else{
              return("success")
            }
          })
          if(!all(primessage == "success")){
            wrongdata <- dataall[primessage != "success"]
            wrongtext <- primessage[primessage != "success"]
            
            wrong.message <- paste(wrongdata, wrongtext, sep=": ")
            wrong.message <- paste0(wrong.message, collapse = "<br/>")
            wrong.message <- paste0("<div style='white-space: pre-wrap;'>", wrong.message, "</div>")
            sendSweetAlert(
              session = session,
              title = NULL,
              text = tags$div(HTML(wrong.message)),
              type = "error"
            )
          }else if(!all(uniall %in% unichr)){
            sendSweetAlert(
              session = session,
              title = "Wrong data format!",
              text = "The 'chromosome' column of the chromosome data is inconsistent with the 'chromosome' column of other datasets.",
              type = "error"
            )
          }else if(!(ncol(data.C)==3 | (ncol(data.C)==5 && is.character(data.C[,4]) && is.character(data.C[,5])))){
            sendSweetAlert(
              session = session,
              title = "Wrong data format!",
              text = "Chromosome data should contain 3 (general type) or 5 columns (cytoband type). Please upload applicable chromosome data.",
              type = "error"
            )
          }else if(!(all(is.numeric(data.C[,2]),is.numeric(data.C[,3])))){
            sendSweetAlert(
              session = session,
              title = "Wrong data format!",
              text = "The 2nd and 3rd columns of the chromosome data should be numeric vectors.",
              type = "error"
            )
          }else if(sum(is.na(data.C[,2:3])) != 0){
            sendSweetAlert(
              session = session,
              title = "Wrong data format!",
              text = "Missing value found in the chromosome data.",
              type = "error"
            )
          }else{
            datat_info <- rep(0,length(data.T))
            datat_info_text <- rep(NA,length(data.T))
            if(length(data.T) != 0){
              for (tt in 1:length(data.T)) {
                data.tt <- data.T[[tt]]
                if(!(all(is.numeric(data.tt[,2]),is.numeric(data.tt[,3])))){
                  datat_info[tt] <- 1
                  datat_info_text[tt] <- "the 2nd and the 3rd columns should be numeric vectors."
                }else if(sum(is.na(data.C[,2:3])) != 0){
                  datat_info[tt] <- 1
                  datat_info_text[tt] <- "NULL values found in the 2nd or the 3rd column!"
                }else if(ncol(data.tt) <= 3){
                  datat_info[tt] <- 1
                  datat_info_text[tt] <- "all Track data is greater than three columns"
                }
              }
            }
            if(length(data.N) != 0){
              datan_info <- rep(0,length(data.N))
              datan_info_text <- rep(NA,length(data.N))
              for (nn in 1:length(data.N)) {
                data.nn <- data.N[[nn]]
                if(ncol(data.nn) == 4 | ncol(data.nn) == 5){
                  if(!(all(is.numeric(data.nn[,2]),is.numeric(data.nn[,3])))){
                    datan_info[nn] <- 1
                    datan_info_text[nn] <- "the 2nd and the 3rd columns should be numeric vectors."
                  }else if(sum(is.na(data.nn[,2:3])) != 0){
                    datan_info[nn] <- 1
                    datan_info_text[nn] <- "NULL values found in the 2nd or the 3rd column!"
                  }else if(!is.character(data.nn[,4])){
                    datan_info[nn] <- 1
                    datan_info_text[nn] <- "the fourth column should be character vectors."
                  }else if(ncol(data.nn) == 5){
                    if(is.character(data.nn[,5])){
                      color_label <- data.nn[,5]
                      if(any(color_label == "" | is.na(color_label))){
                        datan_info[nn] <- 1
                        datan_info_text[nn] <- "NULL values found in the fifth column!"
                      }else{
                        if(length(grep("#",color_label))!=0){
                          color_label16 <- color_label[grep("#",color_label)]
                          color_label7 <- color_label16[which(nchar(color_label16) == 7)]
                          color_label9 <- color_label16[which(nchar(color_label16) == 9)]
                          if(!all(nchar(color_label16) == 7| nchar(color_label16) == 9)){
                            datan_info[nn] <- 1
                            datan_info_text[nn] <- "wrong RGB error!"
                          }else if(!any(grepl("^#([0-9a-fA-F]{6})$",color_label7)) & length(color_label7) != 0){
                            datan_info[nn] <- 1
                            datan_info_text[nn] <- "wrong RGB error!"
                          }else if(!any(grepl("^#([0-9a-fA-F]{8})$",color_label9)) & length(color_label9) != 0){
                            datan_info[nn] <- 1
                            datan_info_text[nn] <- "wrong RGB error!"
                          }else if(!all(color_label[-grep("#",color_label)] %in% colors())){
                            datan_info[nn] <- 1
                            datan_info_text[nn] <- "wrong color input!"
                          }
                        }else{
                          if(!all(color_label %in% colors())){
                            datan_info[nn] <- 1
                            datan_info_text[nn] <- "wrong color input!"
                          }
                        }
                      }
                    }else{
                      datan_info[nn] <- 1
                      datan_info_text[nn] <- "the fifth column of data should be characters!"
                    }
                  }
                }else{
                  datan_info[nn] <- 1
                  datan_info_text[nn] <- "the input data should contain 4 or 5 columns."
                }
              }
            }
            if(!is.null(data.L)){
              if(ncol(data.L) != 6 & ncol(data.L) != 7){
                datal_info <- "Links data should contain 6 or 7 columns."
              }else if(!all(is.numeric(data.L[,2]),is.numeric(data.L[,3]),is.numeric(data.L[,5]),is.numeric(data.L[,6]))){
                datal_info <- "The 2nd, 3rd, 5th and 6th column of the links data should be numeric vectors."
              }
            }
            if(sum(datat_info)!=0){
              datat_info_wrong <- which(datat_info == 1)
              datat_info_text_print <- paste(paste0("Data ",datat_info_wrong), datat_info_text[datat_info_wrong], sep=": ")
              datat_info_text_print <- paste0(datat_info_text_print, collapse = "<br/>")
              datat_info_text_print <- paste0("<div style='white-space: pre-wrap;'>", datat_info_text_print, "</div>")
              sendSweetAlert(
                session = session,
                title = "Track data format error!",
                text = tags$div(HTML(datat_info_text_print)),
                type = "error"
              )
            }else if(sum(datan_info)!=0){
              datan_info_wrong <- which(datan_info == 1)
              datan_info_text_print <- paste(paste0("Data ",datan_info_wrong), datan_info_text[datan_info_wrong], sep=": ")
              datan_info_text_print <- paste0(datan_info_text_print, collapse = "<br/>")
              datan_info_text_print <- paste0("<div style='white-space: pre-wrap;'>", datan_info_text_print, "</div>")
              sendSweetAlert(
                session = session,
                title = "Label data format error!",
                text = tags$div(HTML(datan_info_text_print)),
                type = "error"
              )
            }else if(datal_info != 0){
              sendSweetAlert(
                session = session,
                title = "Link data format error!",
                text = datal_info,
                type = "error"
              )
            }else{
              sendSweetAlert(
                session = session,
                title = "",
                text = "Please go to the 'Circos Parameters' page to view the uploaded datasets and set parameters.",
                type = "success"
              )
              dataview_export <<- 1
            }
          }
        }
      }
    }
    
    
    #chromosome view
    output$viewChr <- renderDT(
      DT::datatable(
        data.C,
        extensions = "Buttons",
        options = list(
          dom = 'Bfrtip',
          buttons = list(
            'pageLength'
          ),
          scrollX = TRUE
        )
      )
    )
    #tracks view
    tra_len <- length(tradatas)
    if(tra_len >= 1){
      lapply(1:tra_len,function(x){
        output[[paste0("viewTra",x)]] <<- renderDT(
          DT::datatable(
            data.T[[x]],
            extensions = "Buttons",
            options = list(
              dom = 'Bfrtip',
              buttons = list(
                'pageLength'
              ),
              scrollX = TRUE
            )
          )
        )
      })
    }else if(tra_len < 1){
      data.T <<- NULL
      output$sortable_traview <<- NULL
    }
    #
    #labels view
    lab_len <- length(labdatas)
    if(lab_len >= 1){
      lapply(1:lab_len,function(x){
        output[[paste0("viewLab",x)]] <<- renderDT(
          DT::datatable(
            data.N[[x]],
            extensions = "Buttons",
            options = list(
              dom = 'Bfrtip',
              buttons = list(
                'pageLength'
              ),
              scrollX = TRUE
            )
          )
        )
      })
    }
    #
    
    #links view
    output$viewlink <- renderDT(
      DT::datatable(
        data.L,
        extensions = "Buttons",
        options = list(
          dom = 'Bfrtip',
          buttons = list(
            'pageLength'
          ),
          scrollX = TRUE
        )
      )
    )
    output$sortable_chr <<- renderUI({
      if(!is.null(chrdatas)){
        h4(paste0(chrdatas))
      }
    })
    
    output$sortable_track <<- renderUI({
      if(tra_len != 0){
        tagList(
          fluidRow(
            column(
              6,
              tags$div(
                HTML('<font color="#2196F3"><h4><i class="fa-solid fa-play"></i> File name</font>'),
                bs4Dash::tooltip(
                  actionButton(
                    inputId = "datvie_tra_tip1", 
                    label="" , 
                    icon=icon("question"),
                    status="info",
                    size = "xs"
                  ),
                  title = "Name of the uploaded files.",
                  placement = "bottom"
                )
              )
            ),
            column(
              3,
              tags$div(
                HTML(' <font color="#2196F3"><h4><i class="fa-solid fa-play"></i> Plot type</font>'),
                bs4Dash::tooltip(
                  actionButton(
                    inputId = "datvie_tra_tip2", 
                    label="" , 
                    icon=icon("question"),
                    status="info",
                    size = "xs"
                  ),
                  title = "The type of plot to create using data of the current track.",
                  placement = "bottom"
                )
              )
            ),
            column(
              3,
              tags$div(
                HTML(' <font color="#2196F3"><h4><i class="fa-solid fa-play"></i> Track index</font>'),
                bs4Dash::tooltip(
                  actionButton(
                    inputId = "datvie_tra_tip3", 
                    label="" , 
                    icon=icon("question"),
                    status="info",
                    size = "xs"
                  ),
                  title = "The index of the current track. Identical index is not allowed for different tracks.",
                  placement = "bottom"
                )
              )
            )
          ),
          lapply(1:tra_len,function(x){
            fluidRow(
              column(
                6,
                fluidRow(
                  column(
                    10,
                    h4(paste0(tradatas[x]))
                  ),
                  column(
                    2,
                    bs4Dash::tooltip(
                      actionBttn(
                        inputId = paste0("view_tra_data",x),
                        label = NULL,
                        style = "unite",
                        color = "success",
                        icon = icon("eye")
                      ),
                      title = "Click to view the dataset.",
                      placement = "bottom"
                    )
                  )
                )
              ),
              column(
                3,
                pickerInput(
                  inputId = paste0("tratype",x),
                  label = NULL, 
                  choices = c("point", "line", "bar", "rect-discrete", "rect-gradual" , "heatmap-discrete" , "heatmap-gradual", "ideogram","stack-point","stack-line")
                )
              ),
              column(
                3,
                fluidRow(
                  column(
                    10,
                    pickerInput(
                      inputId = paste0("trapos",x),
                      label = NULL,
                      choices = 1:tra_len,
                      selected = x
                    )
                  ),
                  column(
                    2,
                    bs4Dash::tooltip(
                      actionBttn(
                        inputId = paste0("tra_setting",x),
                        label = NULL,
                        style = "unite",
                        color = "success",
                        icon = icon("gear")
                      ),
                      title = paste0("Set parameters for this track."),
                      placement = "bottom"
                    )
                  )
                )
              ),
              tags$head(tags$style(paste0("#jquidatvie_travie",x," .modal-dialog{ max-width:1200px}"))),
              jqui_draggable(
                bsModal(
                  id = paste0("jquidatvie_travie",x),
                  title = NULL,
                  trigger = paste0("view_tra_data",x),
                  size = "large",
                  DTOutput(paste0("viewTra",x))
                )
              ),
              tags$head(tags$style(paste0("#jquidatvie_trasetting",x," .modal-dialog{ width:1200px}"))),
              jqui_draggable(
                bsModal(
                  id = paste0("jquidatvie_trasetting",x),
                  title = NULL,
                  trigger = paste0("tra_setting",x),
                  size = "large",
                  uiOutput(paste0("sortable_track_datvie",x))
                )
              )
            )
          })
        )
      }
    })
    ###
    output$sortable_label <<- renderUI({
      labdatas <- input$labdata
      tradatas <- input$tradata
      if(length(labdatas) != 0){
        tagList(
          fluidRow(
            column(
              6,
              tags$div(
                HTML('<font color="#2196F3"><h4><i class="fa-solid fa-play"></i> File name</font>'),
                bs4Dash::tooltip(
                  actionButton(
                    inputId = "datvie_lab_tip1", 
                    label="" , 
                    icon=icon("question"),
                    status="info",
                    size = "xs"
                  ),
                  title = "Name of the uploaded file.",
                  placement = "bottom"
                )
              )
            ),
            column(
              6,
              tags$div(
                HTML(' <font color="#2196F3"><h4><i class="fa-solid fa-play"></i> Label index</font>'),
                bs4Dash::tooltip(
                  actionButton(
                    inputId = "datvie_lab_tip2", 
                    label="" , 
                    icon=icon("question"),
                    status="info",
                    size = "xs"
                  ),
                  title = "The track index for the label data. Elements in the selected track will be labeled by the label data.",
                  placement = "bottom"
                )
              )
            )
          ),
          lapply(1:length(labdatas),function(x){
            fluidRow(
              column(
                6,
                fluidRow(
                  column(
                    10,
                    h4(paste0(labdatas[x]))
                  ),
                  column(
                    2,
                    bs4Dash::tooltip(
                      actionBttn(
                        inputId = paste0("view_lab_data",x),
                        label = NULL,
                        style = "unite",
                        color = "success",
                        icon = icon("eye")
                      ),
                      title = paste0("Click to view the dataset."),
                      placement = "bottom"
                    )
                  )
                )
              ),
              column(
                6,
                fluidRow(
                  column(
                    11,
                    pickerInput(
                      inputId = paste0("labpos",x),
                      label = NULL,
                      choices = 0:length(tradatas),
                      selected = x
                    )
                  ),
                  column(
                    1,
                    bs4Dash::tooltip(
                      actionBttn(
                        inputId = paste0("lab_setting",x),
                        label = NULL,
                        style = "unite",
                        color = "success",
                        icon = icon("gear")
                      ),
                      title = paste0("Set parameters for the label data."),
                      placement = "bottom"
                    )
                  )
                )
              ),
              tags$head(tags$style(paste0("#jquicirpar_labview",x," .modal-dialog{ max-width:1200px}"))),
              jqui_draggable(
                bsModal(
                  id = paste0("jquicirpar_labview",x),
                  title = NULL,
                  trigger = paste0("view_lab_data",x),
                  size = "large",
                  DTOutput(paste0("viewLab",x))
                )
              ),
              tags$head(tags$style(paste0("#jquidatvie_labsetting",x," .modal-dialog{ width:1200px}"))),
              jqui_draggable(
                bsModal(
                  id = paste0("jquidatvie_labsetting",x),
                  title = NULL,
                  trigger = paste0("lab_setting",x),
                  size = "large",
                  uiOutput(paste0("sortable_label_datvie",x))
                )
              )
            )
          })
        )
      }
    })
    output$sortable_link <<- renderUI({
      if(!is.null(data.L)){
        tagList(
          fluidRow(
            column(
              6,
              tags$div(
                HTML('<font color="#2196F3"><h4><i class="fa-solid fa-play"></i> File name</font>'),
                bs4Dash::tooltip(
                  actionButton(
                    inputId = "datvie_link_tip1", 
                    label="" , 
                    icon=icon("question"),
                    status="info",
                    size = "xs"
                  ),
                  title = "Name of the uploaded files.",
                  placement = "bottom"
                )
              )
            ),
            column(
              6,
              tags$div(
                HTML(' <font color="#2196F3"><h4><i class="fa-solid fa-play"></i> Data format</font>'),
                bs4Dash::tooltip(
                  actionButton(
                    inputId = "datvie_link_tip2", 
                    label="" , 
                    icon=icon("question"),
                    status="info",
                    size = "xs"
                  ),
                  title = "The format of links data specified by the user. For data with 6 columns, please select 'Data without a color column'. 
                  For data with 7 columns, please select 'Data with multi-groups' if the 'color' column represents different categories indicated by a character string as 'a, b, c'. 
                  For data with 7 columns, please select 'Data with gradual values' if the 'color' column represents gradual values indicated by numbers as '1, 2, 3'.",
                  placement = "bottom"
                )
              )
            )
          ),
          fluidRow(
            column(
              6,
              fluidRow(
                column(
                  10,
                  h4(paste0(lindatas))
                ),
                column(
                  2,
                  bs4Dash::tooltip(
                    actionBttn(
                      inputId = "view_lin_data",
                      label = NULL,
                      style = "unite",
                      color = "success",
                      icon = icon("eye")
                    ),
                    title = paste0("Click to view the dataset."),
                    placement = "bottom"
                  )
                )
              )
            ),
            column(
              6,
              fluidRow(
                column(
                  11,
                  pickerInput(
                    inputId = "colformatLinks",
                    label = NULL,
                    choices = c("Data without a 'color' column" = "1", "Data with multi-groups" = "2", "Data with gradual values" = "3"),
                    selected="1"
                  )
                ),
                column(
                  1,
                  bs4Dash::tooltip(
                    actionBttn(
                      inputId = "linsetting",
                      label = NULL,
                      style = "unite",
                      color = "success",
                      icon = icon("gear")
                    ),
                    title = paste0("Set parameter for Links data."),
                    placement = "bottom"
                  )
                )
              )
            ),
            tags$head(tags$style(paste0("#jquidatvie_linview .modal-dialog{ max-width:1200px}"))),
            jqui_draggable(
              bsModal(
                id = "jquidatvie_linview",
                title = NULL,
                trigger = "view_lin_data",
                size = "large",
                DTOutput("viewlink")
              )
            ),
            tags$head(tags$style(paste0("#jquidatvie_linsetting .modal-dialog{ width:1200px}"))),
            jqui_draggable(
              bsModal(
                id = "jquidatvie_linsetting",
                title = NULL,
                trigger = "linsetting",
                size = "large",
                pickerInput(
                  inputId = "midplot",
                  label = tags$div(
                    HTML('<font><h5><i class="fa-solid fa-play"></i><b> Use the middle points to draw the links</b></font>'),
                    bs4Dash::tooltip(
                      actionButton(
                        inputId = "datvie_link_tip0", 
                        label="" , 
                        icon=icon("question"),
                        status="info",
                        size = "xs"
                      ),
                      title = "Only use the middle points of two genomic regions to draw the link. This is often used for extremely small genomic regions in large genomes.",
                      placement = "bottom"
                    )
                  ),
                  choices = c("Yes" = TRUE,"No" = FALSE),
                  selected = FALSE
                ),
                conditionalPanel(
                  condition = "input.colformatLinks=='1'",
                  pickerInput(
                    inputId = "colorLinks1",
                    label = tags$div(
                      HTML('<font><h5><i class="fa-solid fa-play"></i><b> Data color</b></font>'),
                      bs4Dash::tooltip(
                        actionButton(
                          inputId = "datvie_link_tip3", 
                          label="" , 
                          icon=icon("question"),
                          status="info",
                          size = "xs"
                        ),
                        title = "Links are filled with colors random assigned by the system or colors specified by the user. For data with 6 columns, format of specified color as 'red' or 'green' is supported. For data with 7 columns, format of specified color as 'a:red;b:green;c:blue' is supported. 'a', 'b', 'c' represents different categories indicated by the 7th column. Color for data groups without assigned color would be set as 'grey'.",
                        placement = "bottom"
                      )
                    ),
                    c("Random" = "1", "Specific" = "2"),
                    selected="1"
                  ),
                  conditionalPanel(
                    condition="input.colorLinks1==2",
                    colourInput(
                      inputId = "selcolorLinks1",
                      label = NULL,
                      value = "yellowgreen",
                      returnName = TRUE
                    )
                  ),
                  numericInput(
                    inputId = "transparencyLinks",
                    label = tags$div(
                      HTML('<font><h5><i class="fa-solid fa-play"></i><b> Color transparency</b></font>'),
                      bs4Dash::tooltip(
                        actionButton(
                          inputId = "datvie_link_tip4", 
                          label="" , 
                          icon=icon("question"),
                          status="info",
                          size = "xs"
                        ),
                        title = "A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.",
                        placement = "bottom"
                      )
                    ),
                    value=0.5,
                    min=0,
                    max=1,
                    step=0.1
                  )
                ),
                conditionalPanel(
                  condition="input.colformatLinks=='2'",
                  pickerInput(
                    inputId = "colorLinks2",
                    label = tags$div(
                      HTML('<font><h5><i class="fa-solid fa-play"></i><b> Data color</b></font>'),
                      bs4Dash::tooltip(
                        actionButton(
                          inputId = "datvie_link_tip3", 
                          label="" , 
                          icon=icon("question"),
                          status="info",
                          size = "xs"
                        ),
                        title = "Links are filled with colors random assigned by the system or colors specified by the user. For data with 6 columns, format of specified color as 'red' or 'green' is supported. For data with 7 columns, format of specified color as 'a:red;b:green;c:blue' is supported. 'a', 'b', 'c' represents different categories indicated by the 7th column. Color for data groups without assigned color would be set as 'grey'.",
                        placement = "bottom"
                      )
                    ),
                    c("Random" = "1", "Custom" = "2"),
                    selected="1"
                  ),
                  conditionalPanel(
                    condition="input.colorLinks2=='2'",
                    textInput(
                      inputId = "selcolorLinks2",
                      label = NULL,
                      value="a:red;b:#00FF00;c:yellowgreen"
                    )
                  ),
                  numericInput(
                    inputId = "transparencyLinks",
                    label = tags$div(
                      HTML('<font><h5><i class="fa-solid fa-play"></i><b> Color transparency</b></font>'),
                      bs4Dash::tooltip(
                        actionButton(
                          inputId = "datvie_link_tip4", 
                          label="" , 
                          icon=icon("question"),
                          status="info",
                          size = "xs"
                        ),
                        title = "A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.",
                        placement = "bottom"
                      )
                    ),
                    value=0.5,
                    min=0,
                    max=1,
                    step=0.1
                  )
                ),
                conditionalPanel(
                  condition="input.colformatLinks==3",
                  tags$div(
                    HTML('<font><h5><i class="fa-solid fa-play"></i><b> Data color</b></font>'),
                    bs4Dash::tooltip(
                      actionButton(
                        inputId = "datvie_link_tip5", 
                        label="" , 
                        icon=icon("question"),
                        status="info",
                        size = "xs"
                      ),
                      title = "For data with a 'color' column, links are filled with colors specified by the user."
                    )
                  ),
                  fluidRow(
                    column(
                      width = 4,
                      colourInput(
                        inputId = "lowColinks",
                        label = HTML('<p><font size="3"><i class="fa-solid fa-circle"></i><strong> Low Color</strong></font></p>'),
                        value = "#0016DB",
                        returnName = TRUE
                      )
                    ),
                    column(
                      4,
                      colourInput(
                        inputId = "midColinks",
                        label = HTML('<p><font size="3"><i class="fa-solid fa-circle"></i><strong> Middle Color</strong></font></p>'),
                        value = "#FFFFFF",
                        returnName = TRUE
                      )
                    ),
                    column(
                      4,
                      colourInput(
                        inputId = "highColinks",
                        label = HTML('<p><font size="3"><i class="fa-solid fa-circle"></i><strong> High Color</strong></font></p>'),
                        value = "#FFFF00",
                        returnName = TRUE
                      )
                    )
                  )
                )
              )
            )
          )
        )
      }
    })
  })
  
  # whichchangechrtyp <<- NULL
  # toListentratyp <- reactive({
  #   lapply(1:length(tradatas), function(x){
  #     input[[paste0("tratype",x)]]
  #   })
  # })
  # observeEvent(toListentratyp(),priority = 1,{
  #   if(input$dataup_go > 0){
  #     lapply(1:length(tradatas), function(x){
  #       typlist[x] <<- input[[paste0("tratype",x)]]
  #     })
  #     x <- as.numeric(unlist(typlist) != unlist(typlist_old))
  #     #x <- which((unlist(typlist)- unlist(typlist_old))==1)
  #     whichchangechrtyp <<- which(x == 1)
  #     typlist_old <<- typlist
  #     print("start")
  #     print(tra_yaxis)
  # 
  #     tra_yaxis[whichchangechrtyp] <<- "2"
  #     print(tra_yaxis)
  # 
  #   }
  # })
  
  
  observeEvent(input$dataup_go,{
    #track
    tradatas <- input$tradata
    tra_bar_direction <<- lapply(1:length(tradatas), function(x){
      return("1")
    })
    tra_bar_Boundary <<- lapply(1:length(tradatas), function(x){
      return(0)
    })
    tra_bar_coldir1 <<- lapply(1:length(tradatas), function(x){
      return("#FF0000")
    })
    tra_bar_coldir2 <<- lapply(1:length(tradatas), function(x){
      return("#00FFFF")
    })
    tra_coltype <<- lapply(1:length(tradatas), function(x){
      return("1")
    })
    tra_colcol <<- lapply(1:length(tradatas), function(x){
      return("red,blue")
    })
    tra_colcol_spec <<- lapply(1:length(tradatas), function(x){
      return("red")
    })
    tra_colorcus <<- lapply(1:length(tradatas), function(x){
      return("a:red;b:blue;c:cyan")
    })
    tra_line_fillarea <<- lapply(1:length(tradatas), function(x){
      return("")
    })
    tra_rect_rectcol <<- lapply(1:length(tradatas), function(x){
      return("1")
    })
    tra_rect_rectcoldis <<- lapply(1:length(tradatas), function(x){
      return("#FF0000")
    })
    tra_rect_rectcoldiscus <<- lapply(1:length(tradatas), function(x){
      return("a:red;b:blue;c:cyan")
    })
    tra_trct_colrect <<- lapply(1:length(tradatas), function(x){
      return("blue")
    })
    rect_gra_lowCol <<- lapply(1:length(tradatas), function(x){
      return("#FF6666")
    })
    rect_gra_midCol <<- lapply(1:length(tradatas), function(x){
      return("yellow")
    })
    rect_gra_highCol <<- lapply(1:length(tradatas), function(x){
      return("#0066CC")
    })
    
    
    tra_line_selrea <<- lapply(1:length(tradatas), function(x){
      return("1")
    })
    tra_bar_borderarea <<- lapply(1:length(tradatas), function(x){
      return("#FFA500")
    })
    tra_transparency <<- lapply(1:length(tradatas), function(x){
      return(1)
    })
    tra_poipch <<- lapply(1:length(tradatas), function(x){
      return("16")
    })
    tra_poi_poisiz <<- lapply(1:length(tradatas), function(x){
      return(0.6)
    })
    tra_baseline <<- lapply(1:length(tradatas), function(x){
      return("0.25,0.75")
    })
    tra_colorline <<- lapply(1:length(tradatas), function(x){
      return("#808080")
    })
    tra_bgcol <<- lapply(1:length(tradatas), function(x){
      return("#F2F2F2")
    })
    tra_hmap_heatmapcol <<- lapply(1:length(tradatas), function(x){
      return("1")
    })
    tra_hmap_typcolhmap <<- lapply(1:length(tradatas), function(x){
      return("blue.white.red")
    })
    tra_hmap_lowColor <<- lapply(1:length(tradatas), function(x){
      return("#0016DB")
    })
    tra_hmap_midColor <<- lapply(1:length(tradatas), function(x){
      return("#FFFFFF")
    })
    tra_hmap_highColor <<- lapply(1:length(tradatas), function(x){
      return("#FFFF00")
    })
    tra_hmap_poslines <<- lapply(1:length(tradatas), function(x){
      return("2")
    })
    tra_hmap_poslinhei <<- lapply(1:length(tradatas), function(x){
      return(0.06)
    })
    tra_heatcol_dis <<- lapply(1:length(tradatas), function(x){
      return(1)
    })
    tra_heat_heatcoldiscus <<- lapply(1:length(tradatas), function(x){
      return("a:red;b:blue;c:cyan")
    })
    heightTra <<- lapply(1:length(tradatas), function(x){
      return(0.1)
    })
    Tra_margin <<- lapply(1:length(tradatas), function(x){
      return(0.01)
    })
    tra_hmap_cellbord <<- lapply(1:length(tradatas), function(x){
      return("")
    })
    tra_hmap_cellbord_col <<- lapply(1:length(tradatas), function(x){
      return("#000000")
    })
    tra_border <<- lapply(1:length(tradatas), function(x){
      return("")
    })
    tra_yaxis <<- lapply(1:length(tradatas), function(x){
      return("2")
    })
    #label
    labdatas <- input$labdata
    lab_fontsize <<- lapply(1:length(labdatas), function(x){
      return(0.15)
    })
    lab_fontcol <<- lapply(1:length(labdatas), function(x){
      return("#000000")
    })
    lab_fontper <<- lapply(1:length(labdatas), function(x){
      return(100)
    })
    lab_adjustfontsize <<- lapply(1:length(labdatas), function(x){
      return(2)
    })
    poslabels <<- lapply(1:length(labdatas), function(x){
      return("inside")
    })
  })
  observeEvent(input$dat_vie_ok,ignoreNULL = TRUE,ignoreInit = TRUE,priority = 3,{
    letplotgo <<- 0
    chr_type <- input$chr_type
    link_type <- input$colformatLinks
    
    
    
    if(!is.null(data.T)){
      tra_inf <- rep(0,length(data.T))
      tra_inf_word <- rep(NA,length(data.T))
      for (k in 1:length(data.T)) { #"point", "line", "bar", "rect-discrete", "rect-gradual" , "heatmap-discrete" , "heatmap-gradual", "ideogram","stack-point","stack-line"
        tratype <- input[[paste0("tratype",k)]]
        data_TT <- data.T[[k]]
        if(tratype == "ideogram"){
          if(ncol(data_TT) != 5){
            tra_inf[k] <- 1
            tra_inf_word[k] <- "The ideogram data should contain 5 columns."
          }else if(!all(is.character(data_TT[,4]),is.character(data_TT[,5]))){
            tra_inf[k] <- 1
            tra_inf_word[k] <- "The last 2 columns of the ideogram data should be characters." 
          }
        }else if(tratype == "stack-point" | tratype == "stack-line"){
          if(ncol(data_TT) != 4){
            tra_inf[k] <- 1
            tra_inf_word[k] <- "The input data for 'stack-point' or 'stack-line' can only contain 4 columns."
          }else if(!is.character(data_TT[,4])){
            tra_inf[k] <- 1
            tra_inf_word[k] <- "The 4th column of input data for 'stack-point' or 'stack-line' should be a character vector."
          }
        } else if ( tratype == "bar" ) {
          if (ncol(data_TT) == 4 | ncol(data_TT) == 5){
            if ("cex" %in% colnames(data_TT)){
              tra_inf[k] <- 1
              tra_inf_word[k] <- "Please select the 'point' plot type for the input data of this track." 
            } else if ("pch" %in% colnames(data_TT)){
              tra_inf[k] <- 1
              tra_inf_word[k] <- "Please select the 'point' plot type for the input data of this track." 
            } else if(!is.numeric(data_TT[,4])){
              tra_inf[k] <- 1
              tra_inf_word[k] <- "The 4th column of input data should be numeric."
            } else if(ncol(data_TT) == 5){
              if(!"color" %in% colnames(data_TT)){
                tra_inf[k] <- 1
                tra_inf_word[k] <- "The 5th column of input data should be explicitly named as 'color'." 
              }else{
                if(!all(is.character(data_TT[,"color"]))){
                  tra_inf[k] <- 1
                  tra_inf_word[k] <- "The 'color' column should be a character vector."
                }
              }
            }
          } else {
            tra_inf[k] <- 1
            tra_inf_word[k] <- "The input data to create 'bar' plot can only contain 4 or 5 columns." 
          }
        }else if(tratype == "line"){
          if ("cex" %in% colnames(data_TT)){
            tra_inf[k] <- 1
            tra_inf_word[k] <- "Please select the 'point' plot type for the input data of this track." 
          } else if ("pch" %in% colnames(data_TT)){
            tra_inf[k] <- 1
            tra_inf_word[k] <- "Please select the 'point' plot type for the input data of this track." 
          } else if ("color" %in% colnames(data_TT)){
            if(ncol(data_TT) != 5){
              tra_inf[k] <- 1
              tra_inf_word[k] <- "Input data with a 'color' column should only contain 5 columns." 
            } else {
              if (!is.numeric(data_TT[,4])) {
                tra_inf[k] <- 1
                tra_inf_word[k] <- "The 4th column of input data should be a numeric vector."
              } else if(!all(is.character(data_TT[,"color"]))){
                tra_inf[k] <- 1
                tra_inf_word[k] <- "The 'color' column should be a character vector."
              }
            }
          } else if (!"color" %in% colnames(data_TT)){
            if(!all(sapply(data_TT[,4:length(data_TT)], function(x){is.numeric(x)}))){
              tra_inf[k] <- 1
              tra_inf_word[k] <- "All columns starting from the 4th column should be a numeric vector."
            }
          }
        }else if(tratype == "point"){
          if("color" %in% colnames(data_TT) | "cex" %in% colnames(data_TT) | "pch" %in% colnames(data_TT)){
            if(!all(tail(names(data_TT),n = -4) %in% c("cex","pch","color"))){
              tra_inf[k] <- 1
              tra_inf_word[k] <- "Only a single data value column should be included in the input data."
            }else if("color" %in% colnames(data_TT)){
              if(!is.character(data_TT$color)){
                tra_inf[k] <- 1
                tra_inf_word[k] <- "The 'color' column should be a character vector."
              }
            }else if("cex" %in% colnames(data_TT)){
              if(!is.numeric(data_TT$cex)){
                tra_inf[k] <- 1
                tra_inf_word[k] <- "The 'cex' column should be a numeric vector." 
              }
            }else if("pch" %in% colnames(data_TT)){
              if(!all(data_TT$pch %in% 1:25)){
                tra_inf[k] <- 1
                tra_inf_word[k] <- "Pch values should be integers in 1-25. Please refer to the help manual for more details." 
              }
            }
          }else{
            if(!all(sapply(data_TT[,4:length(data_TT)], function(x){is.numeric(x)}))){
              tra_inf[k] <- 1
              tra_inf_word[k] <- "All columns starting from the 4th column should be a numeric vector."
            }
          }
        }else if(tratype == "rect-discrete"){
          if(ncol(data_TT) != 4){
            tra_inf[k] <- 1
            tra_inf_word[k] <- "The rect-discrete data should only contain 4 columns." 
          }else if(!is.character(data_TT[,4])){
            tra_inf[k] <- 1
            tra_inf_word[k] <- "The 4th column of the rect-discrete data should be a character vector." 
          }
        }else if(tratype == "rect-gradual"){
          if(ncol(data_TT) != 4){
            tra_inf[k] <- 1
            tra_inf_word[k] <- "The rect-gradual data should only contain 4 columns." 
          }else if(!is.numeric(data_TT[,4])){
            tra_inf[k] <- 1
            tra_inf_word[k] <- "The 4th column of the rect-gradual data should be a numeric vector."
          }
        }else if(tratype == "heatmap-discrete"){
          if(!all(sapply(data_TT[,4:length(data_TT)], function(x){is.character(x)}))){
            tra_inf[k] <- 1
            tra_inf_word[k] <- "All columns except for the 1-3 columns of the input data for heatmap-discrete should be characters."
          }
        }else if(tratype == "heatmap-gradual"){
          if(!all(sapply(data_TT[,4:length(data_TT)], function(x){is.numeric(x)}))){
            tra_inf[k] <- 1
            tra_inf_word[k] <- "All columns except for the 1-3 columns of the input data for heatmap-discrete should be characters."
          }
        }
      }
    }
    if(chr_type == 2 && ncol(data.C) != 5){
      sendSweetAlert(
        session = session,
        title = "",
        text = "Format of the cytoband chromosome data is incorrect.",
        type = "error"
      )
    } else if(sum(tra_inf) != 0){
      tra_inf_word <- na.omit(tra_inf_word)
      sendSweetAlert(
        session = session,
        title = paste0("Error in track index: ",paste(which(tra_inf==1),collapse = ",")), #paste0("Error in index: ",paste(which(tra_inf==1),collapse = ",")),
        text = paste(tra_inf_word,collapse = ";"),
        type = "error"
      )
    } else if(!is.null(data.L)){
      if(link_type == 1){
        if(ncol(data.L) != 6 & ncol(data.L) != 7){
          sendSweetAlert(
            session = session,
            title = "",
            text = "The input data to create links should contain 6 or 7 columns.",
            type = "error"
          )
        }else{
          letplotgo <<- 1
        }
      }else if(link_type == 2){
        if(ncol(data.L) != 7){
          sendSweetAlert(
            session = session,
            title = "",
            text = "The input data to create colored links should contain 7 columns.",
            type = "error"
          )
        }else{
          if(!is.character(data.L[,7])){
            sendSweetAlert(
              session = session,
              title = "",
              text = "The 7th column of links 'Data with multi-groups' should be characters.",
              type = "error"
            )
          }else{
            letplotgo <<- 1
          }
        }
      }else{
        if(ncol(data.L) != 7){
          sendSweetAlert(
            session = session,
            title = "",
            text = "The input data should contain 7 columns.",
            type = "error"
          )
        }else{
          if(!is.numeric(data.L[,7])){
            sendSweetAlert(
              session = session,
              title = "",
              text = "The 7th column of links 'Data with gradual values' should be a numeric vector.",
              type = "error"
            )
          }else{
            letplotgo <<- 1
          }
        }
      }
    }else{
      letplotgo <<- 1
    }
  })
  observeEvent(input$chr_setting,{
    chr_type <- input$chr_type
    if(chr_type == 2 && ncol(data.C) != 5){
      sendSweetAlert(
        session = session,
        title = "",
        text = "Format of the cytoband chromosome data is incorrect.",
        type = "error"
      )
    }
  })
  observeEvent(input$linsetting,{
    link_type <- input$colformatLinks
    if(!is.null(data.L)){
      if(link_type == 1){
        if(ncol(data.L) != 6 & ncol(data.L) != 7){
          sendSweetAlert(
            session = session,
            title = "",
            text = "Data without a 'color' column should contain at least 6 columns.",
            type = "error"
          )
        }
      }else if(link_type == 2){
        if(ncol(data.L) != 7){
          sendSweetAlert(
            session = session,
            title = "",
            text = "The input data should contain 7 columns.",
            type = "error"
          )
        }else{
          if(!is.character(data.L[,7])){
            sendSweetAlert(
              session = session,
              title = "",
              text = "The 7th column of Data with multi-groups should be characters.",
              type = "error"
            )
          }
        }
      }else{
        if(ncol(data.L) != 7){
          sendSweetAlert(
            session = session,
            title = "",
            text = "The input data should contain 7 columns.",
            type = "error"
          )
        }else{
          if(!is.numeric(data.L[,7])){
            sendSweetAlert(
              session = session,
              title = "",
              text = "The 7th column of 'Data with gradual values' should be a numeric vector.",
              type = "error"
            )
          }
        }
      }
    }
  })
  whichchangechrset <<- NULL
  toListentra <- reactive({
    lapply(1:length(tradatas), function(x){
      input[[paste0("tra_setting",x)]]
    })
  })
  observeEvent(toListentra(),priority = 1,{
    if(input$dataup_go > 0){
      tradatas <- input$tradata
      lapply(1:length(tradatas), function(x){
        setlist[x] <<- input[[paste0("tra_setting",x)]]
      })
      x <- which((unlist(setlist)- unlist(setlist_old))==1)
      whichchangechrset <<- x
      setlist_old <<- setlist
      
      
      if(!is.null(input[[paste0("tra_bar_direction",x)]])){
        tra_bar_direction[x] <<- input[[paste0("tra_bar_direction",x)]]
      }
      if(!is.null(input[[paste0("tra_bar_Boundary",x)]])){
        tra_bar_Boundary[x] <<- input[[paste0("tra_bar_Boundary",x)]]
      }
      if(!is.null(input[[paste0("tra_bar_coldir1",x)]])){
        tra_bar_coldir1[x] <<- input[[paste0("tra_bar_coldir1",x)]]
      }
      if(!is.null(input[[paste0("tra_bar_coldir2",x)]])){
        tra_bar_coldir2[x] <<- input[[paste0("tra_bar_coldir2",x)]]
      }
      if(!is.null(input[[paste0("tra_coltype",x)]])){
        tra_coltype[x] <<- input[[paste0("tra_coltype",x)]]
      }
      if(!is.null(input[[paste0("tra_colcol",x)]])){
        tra_colcol[x] <<- input[[paste0("tra_colcol",x)]]
      }
      if(!is.null(input[[paste0("tra_colcol_spec",x)]])){
        tra_colcol_spec[x] <<- input[[paste0("tra_colcol_spec",x)]]
      }
      if(!is.null(input[[paste0("tra_colorcus",x)]])){
        tra_colorcus[x] <<- input[[paste0("tra_colorcus",x)]]
      }
      if(!is.null(input[[paste0("tra_line_fillarea",x)]])){
        tra_line_fillarea[x] <<- input[[paste0("tra_line_fillarea",x)]]
      }
      if(!is.null(input[[paste0("tra_rect_rectcol",x)]])){
        tra_rect_rectcol[x] <<- input[[paste0("tra_rect_rectcol",x)]]
      }
      if(!is.null(input[[paste0("tra_rect_rectcoldis",x)]])){
        tra_rect_rectcoldis[x] <<- input[[paste0("tra_rect_rectcoldis",x)]]
      }
      if(!is.null(input[[paste0("tra_rect_rectcoldiscus",x)]])){
        tra_rect_rectcoldiscus[x] <<- input[[paste0("tra_rect_rectcoldiscus",x)]]
      }
      if(!is.null(input[[paste0("tra_trct_colrect",x)]])){
        tra_trct_colrect[x] <<- input[[paste0("tra_trct_colrect",x)]]
      }
      if(!is.null(input[[paste0("rect_gra_lowCol",x)]])){
        rect_gra_lowCol[x] <<- input[[paste0("rect_gra_lowCol",x)]]
      }
      if(!is.null(input[[paste0("rect_gra_midCol",x)]])){
        rect_gra_midCol[x] <<- input[[paste0("rect_gra_midCol",x)]]
      }
      if(!is.null(input[[paste0("rect_gra_highCol",x)]])){
        rect_gra_highCol[x] <<- input[[paste0("rect_gra_highCol",x)]]
      }
      
      if(!is.null(input[[paste0("tra_line_selrea",x)]])){
        tra_line_selrea[x] <<- input[[paste0("tra_line_selrea",x)]]
      }
      if(!is.null(input[[paste0("tra_bar_borderarea",x)]])){
        tra_bar_borderarea[x] <<- input[[paste0("tra_bar_borderarea",x)]]
      }
      if(!is.null(input[[paste0("tra_transparency",x)]])){
        tra_transparency[x] <<- input[[paste0("tra_transparency",x)]]
      }
      if(!is.null(input[[paste0("tra_poipch",x)]])){
        tra_poipch[x] <<- input[[paste0("tra_poipch",x)]]
      }
      if(!is.null(input[[paste0("tra_poi_poisiz",x)]])){
        tra_poi_poisiz[x] <<- input[[paste0("tra_poi_poisiz",x)]]
      }
      if(!is.null(input[[paste0("tra_baseline",x)]])){
        tra_baseline[x] <<- input[[paste0("tra_baseline",x)]]
      }
      if(!is.null(input[[paste0("tra_colorline",x)]])){
        tra_colorline[x] <<- input[[paste0("tra_colorline",x)]]
      }
      if(!is.null(input[[paste0("tra_bgcol",x)]])){
        tra_bgcol[x] <<- input[[paste0("tra_bgcol",x)]]
      }
      if(!is.null(input[[paste0("tra_hmap_heatmapcol",x)]])){
        tra_hmap_heatmapcol[x] <<- input[[paste0("tra_hmap_heatmapcol",x)]]
      }
      if(!is.null(input[[paste0("tra_hmap_typcolhmap",x)]])){
        tra_hmap_typcolhmap[x] <<- input[[paste0("tra_hmap_typcolhmap",x)]]
      }
      if(!is.null(input[[paste0("tra_hmap_lowColor",x)]])){
        tra_hmap_lowColor[x] <<- input[[paste0("tra_hmap_lowColor",x)]]
      }
      if(!is.null(input[[paste0("tra_hmap_midColor",x)]])){
        tra_hmap_midColor[x] <<- input[[paste0("tra_hmap_midColor",x)]]
      }
      if(!is.null(input[[paste0("tra_hmap_highColor",x)]])){
        tra_hmap_highColor[x] <<-input[[paste0("tra_hmap_highColor",x)]]
      }
      if(!is.null(input[[paste0("tra_hmap_poslines",x)]])){
        tra_hmap_poslines[x] <<- input[[paste0("tra_hmap_poslines",x)]]
      }
      if(!is.null(input[[paste0("tra_hmap_poslinhei",x)]])){
        tra_hmap_poslinhei[x] <<- input[[paste0("tra_hmap_poslinhei",x)]]
      }
      if(!is.null(input[[paste0("tra_heatcol_dis",x)]])){
        tra_heatcol_dis[x] <<- input[[paste0("tra_heatcol_dis",x)]]
      }
      if(!is.null(input[[paste0("tra_heat_heatcoldiscus",x)]])){
        tra_heat_heatcoldiscus[x] <<- input[[paste0("tra_heat_heatcoldiscus",x)]]
      }
      if(!is.null(input[[paste0("heightTra",x)]])){
        heightTra[x] <<- input[[paste0("heightTra",x)]]
      }
      if(!is.null(input[[paste0("Tra_margin",x)]])){
        Tra_margin[x] <<- input[[paste0("Tra_margin",x)]]
      }
      if(!is.null(input[[paste0("tra_hmap_cellbord",x)]])){
        tra_hmap_cellbord[x] <<- input[[paste0("tra_hmap_cellbord",x)]]
      }
      if(!is.null(input[[paste0("tra_hmap_cellbord_col",x)]])){
        tra_hmap_cellbord_col[x] <<- input[[paste0("tra_hmap_cellbord_col",x)]]
      }
      if(!is.null(input[[paste0("tra_border",x)]])){
        tra_border[x] <<- input[[paste0("tra_border",x)]]
      }
      if(!is.null(input[[paste0("tra_yaxis",x)]])){
        tra_yaxis[x] <<- input[[paste0("tra_yaxis",x)]]
      }
    }
  })
  observeEvent(toListentra(),priority = 0,{ #right bottom
    tradatas <- input$tradata
    tra_num <- length(tradatas)
    tra_pos <<- lapply(1:tra_num, function(x){
      input[[paste0("trapos",x)]]
    })
    if(tra_num != 0){
      tra_type <- lapply(1:tra_num, function(x){
        input[[paste0("tratype",x)]]
      })
    }
    
    if(length(whichchangechrset) != 0){
      tratype <- input[[paste0("tratype",whichchangechrset)]]
      data_TT <- data.T[[whichchangechrset]]
      tra_inf1 <- 0
      tra_inf1_word <- NULL
      if(tratype == "ideogram"){
        if(ncol(data_TT) != 5){
          tra_inf1 <- 1
          tra_inf1_word <- "The ideogram data should contain 5 columns." 
        }else if(!all(is.character(data_TT[,4]),is.character(data_TT[,5]))){
          tra_inf1 <- 1
          tra_inf1_word <- "The last 2 columns of the ideogram data should be characters." 
        }
      }else if(tratype == "stack-point" | tratype == "stack-line"){
        if(ncol(data_TT) != 4){
          tra_inf1 <- 1
          tra_inf1_word <- "The input data for 'stack-point' or 'stack-line' can only contain 4 columns."
        }else if(!is.character(data_TT[,4])){
          tra_inf1 <- 1
          tra_inf1_word <- "The 4th column of input data for 'stack-point' or 'stack-line' should be a character vector."
        }
      }else if ( tratype == "bar" ) {
        if (ncol(data_TT) == 4 | ncol(data_TT) == 5){
          if ("cex" %in% colnames(data_TT)){
            tra_inf1 <- 1
            tra_inf1_word <- "Please select the 'point' plot type for the input data of this track." 
          } else if ("pch" %in% colnames(data_TT)){
            tra_inf1 <- 1
            tra_inf1_word <- "Please select the 'point' plot type for the input data of this track." 
          }  else if(!is.numeric(data_TT[,4])){
            tra_inf1 <- 1
            tra_inf1_word <- "The 4th column of input data should be numeric."
          } else if(ncol(data_TT) == 5){
            if(!"color" %in% colnames(data_TT)){
              tra_inf1 <- 1
              tra_inf1_word <- "The 5th column of input data should be explicitly named as 'color'." 
            }else{
              if(!all(is.character(data_TT[,"color"]))){
                tra_inf1 <- 1
                tra_inf1_word <- "The 'color' column should be a character vector."
              }
            }
          }
        } else {
          tra_inf1 <- 1
          tra_inf1_word <- "The input data to create 'bar' plot can only contain 4 or 5 columns." 
        }
      } else if (tratype == "line"){
        if ("cex" %in% colnames(data_TT)){
          tra_inf1 <- 1
          tra_inf1_word <- "Please select the 'point' plot type for the input data of this track." 
        } else if ("pch" %in% colnames(data_TT)){
          tra_inf1 <- 1
          tra_inf1_word <- "Please select the 'point' plot type for the input data of this track." 
        }  else if ("color" %in% colnames(data_TT)){
          if(ncol(data_TT) != 5){
            tra_inf1 <- 1
            tra_inf1_word <- "Input data with a 'color' column should only contain 5 columns." 
          } else {
            if (!is.numeric(data_TT[,4])) {
              tra_inf1 <- 1
              tra_inf1_word <- "The 4th column of input data should be a numeric vector."
            } else if(!all(is.character(data_TT[,"color"]))){
              tra_inf1 <- 1
              tra_inf1_word <- "The 'color' column should be a character vector."
            }
          }
        } else if (!"color" %in% colnames(data_TT)){
          if(!all(sapply(data_TT[,4:length(data_TT)], function(x){is.numeric(x)}))){
            tra_inf1 <- 1
            tra_inf1_word <- "All columns starting from the 4th column should be a numeric vector." 
          }
        }
      } else if (tratype == "point"){
        if("color" %in% colnames(data_TT) | "cex" %in% colnames(data_TT) | "pch" %in% colnames(data_TT)){
          if(!all(tail(names(data_TT),n = -4) %in% c("cex","pch","color"))){
            tra_inf1 <- 1
            tra_inf1_word <- "Only a single data value column should be included in the input data."
          }else if("color" %in% colnames(data_TT)){
            if(!is.character(data_TT$color)){
              tra_inf1 <- 1
              tra_inf1_word <- "The 'color' column should be a character vector."
            }
          }else if("cex" %in% colnames(data_TT)){
            if(!is.numeric(data_TT$cex)){
              tra_inf1 <- 1
              tra_inf1_word <- "The 'cex' column should be a numeric vector." 
            }
          }else if("pch" %in% colnames(data_TT)){
            if(!all(data_TT$pch %in% 1:25)){
              tra_inf1 <- 1
              tra_inf1_word <- "Pch values should be integers in 1-25. Please refer to the help manual for more details." 
            }
          }
        }else{
          if(!all(sapply(data_TT[,4:length(data_TT)], function(x){is.numeric(x)}))){
            tra_inf1 <- 1
            tra_inf1_word <- "All columns starting from the 4th column should be a numeric vector."
          }
        }
      }else if(tratype == "rect-discrete"){
        if(ncol(data_TT) != 4){
          tra_inf1 <- 1
          tra_inf1_word <- "The rect-discrete data should only contain 4 columns." 
        }else if(!is.character(data_TT[,4])){
          tra_inf1 <- 1
          tra_inf1_word <- "The 4th column of the rect-discrete data should be a character vector." 
        }
      }else if(tratype == "rect-gradual"){
        if(ncol(data_TT) != 4){
          tra_inf1 <- 1
          tra_inf1_word <- "The rect-gradual data should only contain 4 columns." 
        }else if(!is.numeric(data_TT[,4])){
          tra_inf1 <- 1
          tra_inf1_word <- "The 4th column of the rect-gradual data should be a numeric vector." 
        }
      }else if(tratype == "heatmap-discrete"){
        if(!all(sapply(data_TT[,4:length(data_TT)], function(x){is.character(x)}))){
          tra_inf1 <- 1
          tra_inf1_word <- "All columns except for the 1-3 columns of the input data for heatmap-discrete should be characters."
        }
      }else if(tratype == "heatmap-gradual"){
        if(!all(sapply(data_TT[,4:length(data_TT)], function(x){is.numeric(x)}))){
          tra_inf1 <- 1
          tra_inf1_word <- "All columns except for the 1-3 columns of the input data for heatmap-discrete should be characters."
        }
      }
      if(tra_inf1 != 0){
        sendSweetAlert(
          session = session,
          title = "",
          text = paste(tra_inf1_word),
          type = "error"
        )
      }
      
      lapply(as.numeric(unlist(tra_pos)), function(x){
        output[[paste0("sortable_track_datvie",x)]] <<- renderUI({
          tagList(
            h4(paste0("Plot type of the current track: ",tra_type[[x]])),
            if(tra_type[[x]] == "bar"){
              tagList(
                pickerInput(
                  inputId = paste0("tra_bar_direction",x),
                  label = tags$div(
                    HTML('<font><h5><i class="fa-solid fa-play"></i><b> Bar direction</b></font>'),
                    bs4Dash::tooltip(
                      actionButton(
                        inputId = paste0("datvie_tip_bar_direction",x), 
                        label="" , 
                        icon=icon("question"),
                        status="info",
                        size = "xs"
                      ),
                      title = "Bars can be unidirectional or bidirectional. For bidirectional bars, the 4th column which 
                                contains the data values will be divided into two groups based on the boundary value.",
                      placement = "bottom"
                    )
                  ),
                  choices = c("Unidirectional" = "1", "Bidirectional" = "2"),
                  selected = tra_bar_direction[x]
                ),
                conditionalPanel(
                  condition= paste0("input.tra_bar_direction",x,"== '2'"),
                  fluidRow(
                    column(
                      width = 4,
                      numericInput(
                        inputId = paste0("tra_bar_Boundary",x),
                        label = "Boundary value:",
                        value=tra_bar_Boundary[x],
                        step=0.01
                      )
                    ),
                    column(
                      width = 4,
                      colourInput(
                        inputId = paste0("tra_bar_coldir1",x),
                        label = "Outer bar color:",
                        value = tra_bar_coldir1[x],
                        returnName = TRUE
                      )
                    ),
                    column(
                      width = 4,
                      colourInput(
                        inputId = paste0("tra_bar_coldir2",x),
                        label = "Inner bar color:",
                        value = tra_bar_coldir2[x],
                        returnName = TRUE
                      )
                    )
                  )
                )
              )
            },
            if(tra_type[[x]] == "stack-line" | tra_type[[x]] == "stack-point"){
              tagList(
                pickerInput(
                  inputId = paste0("tra_coltype",x),
                  label = tags$div(
                    HTML('<font><h5><i class="fa-solid fa-play"></i><b> Data color</b></font>'),
                    bs4Dash::tooltip(
                      actionButton(
                        inputId = paste0("datvie_tip_bar_trycoltp",x), 
                        label="" , 
                        icon=icon("question"),
                        status="info",
                        size = "xs"
                      ),
                      title = "The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
                      To customize color for data with multiple columns, users should provide a character string representing one or multiple 
                      colors separated by commas. For example, 'red' or 'red,orange,blue'.
                      To customize color for data with multiple groups, the column indicating different groups should be named as 'color' or 'stack'.
                      Users should provide a character strings assigning colors to each group. 
                      For example, 'a:red;b:green;c:blue', in which 'a b c' represent different data groups. 
                      Color for data groups without assigned color would be set as 'grey'. 
                      Hex color codes as '#FF0000' are also supported. See example data for more details.",
                      placement = "right"
                    )
                  ),
                  choices = c("Random" = "1", "Specific color" = "2" , "Custom for data with multi-groups" = "3"),
                  selected = tra_coltype[x]
                ),
                conditionalPanel(
                  condition = paste0("input.tra_coltype",x,"== '2'"),
                  colourInput(
                    inputId = paste0("tra_colcol_spec",x),
                    label = NULL,
                    value = tra_colcol_spec[x],
                    returnName = TRUE
                  )
                ),
                conditionalPanel(
                  condition = paste0("input.tra_coltype",x," == '3'"),
                  textInput(paste0("tra_colorcus",x), NULL, value=tra_colorcus[x])
                )
              )
            },
            if(tra_type[[x]] != "rect-discrete" & tra_type[[x]] != "rect-gradual" & tra_type[[x]] != "heatmap-discrete" & tra_type[[x]] != "heatmap-gradual" & tra_type[[x]] != "ideogram"& tra_type[[x]] != "stack-line"& tra_type[[x]] != "stack-point"){
              if(tra_type[[x]] != "bar"){
                tagList(
                  pickerInput(
                    inputId = paste0("tra_coltype",x),
                    label = tags$div(
                      HTML('<font><h5><i class="fa-solid fa-play"></i><b> Data color</b></font>'),
                      bs4Dash::tooltip(
                        actionButton(
                          inputId = paste0("datvie_tip_bar_trycoltp",x), 
                          label="" , 
                          icon=icon("question"),
                          status="info",
                          size = "xs"
                        ),
                        title = "The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
                      To customize color for data with multiple columns, users should provide a character string representing one or multiple 
                      colors separated by commas. For example, 'red' or 'red,orange,blue'.
                      To customize color for data with multiple groups, the column indicating different groups should be named as 'color' or 'stack'.
                      Users should provide a character strings assigning colors to each group. 
                      For example, 'a:red;b:green;c:blue', in which 'a b c' represent different data groups. 
                      Color for data groups without assigned color would be set as 'grey'. 
                      Hex color codes as '#FF0000' are also supported. See example data for more details.",
                        placement = "right"
                      )
                    ),
                    choices = c("Random" = "1", "Custom for data with multi-columns" = "2", "Custom for data with multi-groups" = "3"),
                    selected = tra_coltype[x]
                  ),
                  conditionalPanel(
                    condition = paste0("input.tra_coltype",x,"== '2'"),
                    textInput(paste0("tra_colcol",x), NULL, value=tra_colcol[x])
                  ),
                  conditionalPanel(
                    condition = paste0("input.tra_coltype",x," == '3'"),
                    textInput(paste0("tra_colorcus",x), NULL, value=tra_colorcus[x])
                  )
                )
              }else{
                tagList(
                  conditionalPanel(
                    condition = paste0("input.tra_bar_direction",x,"== '1'"),
                    pickerInput(
                      inputId = paste0("tra_coltype",x),
                      label = tags$div(
                        HTML('<font><h5><i class="fa-solid fa-play"></i><b> Data color</b></font>'),
                        bs4Dash::tooltip(
                          actionButton(
                            inputId = paste0("datvie_tip_bar_trycoltp2",x), 
                            label="" , 
                            icon=icon("question"),
                            status="info",
                            size = "xs"
                          ),
                          title = "The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
                        To customize color for data with multiple columns, users should provide a character string representing one or multiple 
                        colors separated by commas. For example, 'red' or 'red,orange,blue'.if there is only one column of value, only one color needs to be specified.
                        To customize color for data with multiple groups, the column indicating different groups should be named as 'color' or 'stack'.
                        Users should provide a character strings assigning colors to each group. 
                        For example, 'a:red;b:green;c:blue', in which 'a b c' represent different data groups. 
                        Color for data groups without assigned color would be set as 'grey'. 
                        Hex color codes as '#FF0000' are also supported. See example data for more details.",
                          placement = "right"
                        )
                      ),
                      choices = c("Random" = "1", "Specific color" = "2","Custom for data with multi-groups" = "3"),
                      selected = tra_coltype[x]
                    ),
                    conditionalPanel(
                      condition = paste0("input.tra_coltype",x,"== '2'"),
                      colourInput(
                        inputId = paste0("tra_colcol_spec",x),
                        label = NULL,
                        value = tra_colcol_spec[x],
                        returnName = TRUE
                      )
                    ),
                    conditionalPanel(
                      condition = paste0("input.tra_coltype",x," == '3'"),
                      textInput(paste0("tra_colorcus",x), NULL, value=tra_colorcus[x])
                    )
                  )
                )
              }
            },
            if(tra_type[[x]] == "line"){
              pickerInput(
                inputId = paste0("tra_line_fillarea",x),
                label = tags$div(
                  HTML('<font><h5><i class="fa-solid fa-play"></i><b> Fill area?</b></font>'),
                  bs4Dash::tooltip(
                    actionButton(
                      inputId = paste0("datvie_tip_bar_fillarea",x), 
                      label="" , 
                      icon=icon("question"),
                      status="info",
                      size = "xs"
                    ),
                    title = "Fill the area below the lines.",
                    placement = "right"
                  )
                ),
                choices = c("Yes" = "add", "No" = ""),
                selected=tra_line_fillarea[x]
              )
            },
            if(tra_type[[x]] == "rect-gradual"){
              tagList(
                pickerInput(
                  inputId = paste0("tra_rect_rectcol",x),
                  label = tags$div(
                    HTML('<font><h5><i class="fa-solid fa-play"></i><b> Data color</b></font>'),
                    bs4Dash::tooltip(
                      actionButton(
                        inputId = paste0("datvie_tip_rect_rectcol",x), 
                        label="" , 
                        icon=icon("question"),
                        status="info",
                        size = "xs"
                      ),
                      title = "The color used to plot the data, you can use the preset color, or you can customize the color.",
                      placement = "right"
                    )
                  ),
                  
                  choices = c("Presets" = "1", "Customize" = "2"),
                  selected = tra_rect_rectcol[x]
                ),
                conditionalPanel(
                  condition = paste0("input.tra_rect_rectcol",x," == '1'"),
                  pickerInput(
                    inputId = paste0("tra_trct_colrect",x),
                    label = tags$div(
                      HTML('<font><h5><i class="fa-solid fa-play"></i><b> Color presets</b></font>')
                    ),
                    choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", "navy", "seagreen", "maroon", "olivedrab", "gold", "lightblue", "navy.yellow", "purple.seagreen", "navy.orange", "navy.cyan", "blue.red", "green.red"),
                    selected = tra_trct_colrect[x]
                  )
                ),
                conditionalPanel(
                  condition = paste0("input.tra_rect_rectcol",x," == '2'"),
                  
                  fluidRow(
                    column(
                      width = 4,
                      colourInput(
                        inputId = paste0("rect_gra_lowCol",x),
                        label = HTML('<p><font size="3"><i class="fa-solid fa-circle"></i><strong> Low Color</strong></font></p>'),
                        value = rect_gra_lowCol[x],
                        returnName = TRUE
                      )
                    ),
                    column(
                      4,
                      colourInput(
                        inputId = paste0("rect_gra_midCol",x),
                        label = HTML('<p><font size="3"><i class="fa-solid fa-circle"></i><strong> Middle Color</strong></font></p>'),
                        value = rect_gra_midCol[x],
                        returnName = TRUE
                      )
                    ),
                    column(
                      4,
                      colourInput(
                        inputId = paste0("rect_gra_highCol",x),
                        label = HTML('<p><font size="3"><i class="fa-solid fa-circle"></i><strong> High Color</strong></font></p>'),
                        value = rect_gra_highCol[x],
                        returnName = TRUE
                      )
                    )
                  )
                  
                  
                  
                )
                
              )
            },
            if(tra_type[[x]] == "rect-discrete"){
              tagList(
                pickerInput(
                  inputId = paste0("tra_rect_rectcol",x),
                  label = tags$div(
                    HTML('<font><h5><i class="fa-solid fa-play"></i><b> Data color</b></font>'),
                    bs4Dash::tooltip(
                      actionButton(
                        inputId = paste0("datvie_tip_rect_rectcol",x), 
                        label="" , 
                        icon=icon("question"),
                        status="info",
                        size = "xs"
                      ),
                      title = "The color to be used to plot the data, which can be random assigned by the application or specified by the users.
                    If 'Specific' was chosen, all data will be filled by a specified color. 
                    If 'Custom' was chosen, the 4th column of the uploaded data should be a categorical character vector with no more than 50 groups.
                    Users should provide values as 'a:red;b:green;c:blue', in which 'a b c' represent different
                    data category indicated by the 4th column of the uploaded data. 
                    Color for data without customed color will be set to NULL. Hex color codes as '#FF0000' are also supported.",
                      placement = "right"
                    )
                  ),
                  
                  choices = c("Random" = "1", "Specific" = "2", "Custom" = "3"),
                  selected = tra_rect_rectcol[x]
                ),
                conditionalPanel(
                  condition = paste0("input.tra_rect_rectcol",x,"== '2'"),
                  colourInput(
                    inputId = paste0("tra_rect_rectcoldis",x),
                    label = NULL,
                    value = tra_rect_rectcoldis[x],
                    returnName = TRUE
                  )
                ),
                conditionalPanel(
                  condition = paste0("input.tra_rect_rectcol",x,"== '3'"),
                  textInput(paste0("tra_rect_rectcoldiscus",x), NULL, value=tra_rect_rectcoldiscus[x])
                )
              )
            },
            if(tra_type[[x]] == "line"){
              tagList(
                conditionalPanel(
                  condition = paste0("input.tra_line_fillarea",x,"== 'add'"),
                  pickerInput(
                    inputId = paste0("tra_line_selrea",x),
                    label = tags$div(
                      HTML('<font><h5><i class="fa-solid fa-play"></i><b> Area color</b></font>'),
                      bs4Dash::tooltip(
                        actionButton(
                          inputId = paste0("datvie_tip_line_selrea",x), 
                          label="" , 
                          icon=icon("question"),
                          status="info",
                          size = "xs"
                        ),
                        title = "Filled color to the area, which can be identical with lines color or specified by the users. If 'Specific' was chosen, all data will be filled by a specified color as 'orange'.",
                        placement = "right"
                      )
                    ),
                    choices =  c("Identical with lines" = "1", "Specific" = "2"),
                    selected=tra_line_selrea[x]
                  ),
                  conditionalPanel(
                    condition = paste0("input.tra_line_selrea",x,"== '2'"),
                    colourInput(
                      inputId = paste0("tra_bar_borderarea",x),
                      label = NULL,
                      value = tra_bar_borderarea[x],
                      returnName = TRUE
                    )
                  )
                )
              )
            },
            if(tra_type[[x]] != "heatmap-discrete" & tra_type[[x]] != "heatmap-gradual" & tra_type[[x]] != "ideogram"){
              numericInput(
                inputId = paste0("tra_transparency",x),
                label = tags$div(
                  HTML('<font><h5><i class="fa-solid fa-play"></i><b> Color transparency</b></font>'),
                  
                  bs4Dash::tooltip(
                    actionButton(
                      inputId = paste0("datvie_tip_tra_trans",x),
                      label="" ,
                      icon=icon("question"),
                      status="info",
                      size = "xs"
                    ),
                    title = "A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.",
                    placement = "right"
                  )
                ),
                value=tra_transparency[x],
                min=0,
                max=1,
                step=0.1
              )
            },
            if(tra_type[[x]] == "point" & !("pch" %in% colnames(data.T[[x]]))){
              textInput(
                paste0("tra_poipch",x),
                label = tags$div(
                  HTML('<font><h5><i class="fa-solid fa-play"></i><b> Point shape</b></font>'),
                  bs4Dash::tooltip(
                    actionButton(
                      inputId = paste0("datvie_tip_poi_pch",x), 
                      label="" , 
                      icon=icon("question"),
                      status="info",
                      size = "xs"
                    ),
                    title = "Symbols used for different points. Applicable value can be an integer in [0-25] or an integer vector of arbitrary length adjusted automatically to the number of data categories. Type ?pch in R console for more details.",
                    placement = "right"
                  )
                ),
                value= tra_poipch[x]
              )
            },
            if(tra_type[[x]] == "stack-point"){
              tagList(
                textInput(
                  paste0("tra_poipch",x),
                  label = tags$div(
                    HTML('<font><h5><i class="fa-solid fa-play"></i><b> Point shape</b></font>'),
                    bs4Dash::tooltip(
                      actionButton(
                        inputId = paste0("datvie_tip_poi_pch",x), 
                        label="" , 
                        icon=icon("question"),
                        status="info",
                        size = "xs"
                      ),
                      title = "Symbols used for different points. Applicable value can be an integer in [0-25] or an integer vector of arbitrary length adjusted automatically to the number of data categories. Type ?pch in R console for more details.",
                      placement = "right"
                    )
                  ),
                  value= tra_poipch[x]
                ),
                numericInput(
                  paste0("tra_poi_poisiz",x),
                  label = tags$div(
                    HTML('<font><h5><i class="fa-solid fa-play"></i><b> Point size</b></font>')
                  ),
                  value=tra_poi_poisiz[x],
                  min=0,
                  max=1.5,
                  step=0.1
                )
              )
            },
            if(tra_type[[x]] == "point" & !("cex" %in% colnames(data.T[[x]]))){
              numericInput(
                paste0("tra_poi_poisiz",x),
                label = tags$div(
                  HTML('<font><h5><i class="fa-solid fa-play"></i><b> Point size</b></font>')
                ),
                value=tra_poi_poisiz[x],
                min=0,
                max=1.5,
                step=0.1
              )
            },
            if(tra_type[[x]] != "rect-discrete" & tra_type[[x]] != "rect-gradual" & tra_type[[x]] != "heatmap-discrete" & tra_type[[x]] != "heatmap-gradual" & tra_type[[x]] != "ideogram" & tra_type[[x]] != "stack-line"){
              if(tra_type[[x]] == "stack-point"){
                tagList(
                  colourInput(
                    inputId = paste0("tra_colorline",x),
                    label = tags$div(
                      HTML('<font><h5><i class="fa-solid fa-play"></i><b> Baselines color(s)</b></font>'),
                      bs4Dash::tooltip(
                        actionButton(
                          inputId = paste0("datvie_tip_baselinecol",x), 
                          label="" , 
                          icon=icon("question"),
                          status="info",
                          size = "xs"
                        ),
                        title = "The color to be used for the baselines which can be null or a character vector of arbitrary length adjusted automatically to the number of baselines. For example, 'grey' or 'red,green'.Hex color codes as '#FF0000' are also supported.",
                        placement = "right"
                      )
                    ),
                    value = tra_colorline[x],
                    returnName = TRUE
                  )
                )
                
              }else{
                tagList(
                  textInput(
                    inputId = paste0("tra_baseline",x),
                    label = tags$div(
                      HTML('<font><h5><i class="fa-solid fa-play"></i><b> Y-axis coordinates of baselines</b></font>'),
                      bs4Dash::tooltip(
                        actionButton(
                          inputId = paste0("datvie_tip_baseline",x), 
                          label="" , 
                          icon=icon("question"),
                          status="info",
                          size = "xs"
                        ),
                        title = "Decimal numbers in [0, 1] to adjust the Y-axis coordinates of baselines. Numeric vector of arbitrary length is also accepted. For example, '0.5' or '0.25,0.5,0.75'.",
                        placement = "right"
                      )
                    ),
                    value=tra_baseline[x]
                  ),
                  colourInput(
                    inputId = paste0("tra_colorline",x),
                    label = tags$div(
                      HTML('<font><h5><i class="fa-solid fa-play"></i><b> Baselines color(s)</b></font>'),
                      bs4Dash::tooltip(
                        actionButton(
                          inputId = paste0("datvie_tip_baselinecol",x), 
                          label="" , 
                          icon=icon("question"),
                          status="info",
                          size = "xs"
                        ),
                        title = "The color to be used for the baselines which can be null or a character vector of arbitrary length adjusted automatically to the number of baselines. For example, 'grey' or 'red,green'.Hex color codes as '#FF0000' are also supported.",
                        placement = "right"
                      )
                    ),
                    value = tra_colorline[x],
                    returnName = TRUE
                  )
                )
              }
            },
            if(tra_type[[x]] != "heatmap-discrete" & tra_type[[x]] != "heatmap-gradual" & tra_type[[x]] != "ideogram"){
              colourInput(
                inputId = paste0("tra_bgcol",x),
                label = tags$div(
                  HTML('<font><h5><i class="fa-solid fa-play"></i><b> Background color(s)</b></font>'),
                  bs4Dash::tooltip(
                    actionButton(
                      inputId = paste0("datvie_tip_bgcol",x), 
                      label="" , 
                      icon=icon("question"),
                      status="info",
                      size = "xs"
                    ),
                    title = "The color to be used for the background of the plot which can be null or a color vector of arbitrary length adjusted 
                  automatically to the number of sectors. For example, 'grey95' or 'grey95,grey,pink,yellow'. Hex color codes as '#FF0000' are also supported.",
                    placement = "right"
                  )
                ),
                value = tra_bgcol[x],
                returnName = TRUE
              )
            },
            if(tra_type[[x]] == "heatmap-gradual"){
              tagList(
                pickerInput(
                  inputId = paste0("tra_hmap_heatmapcol",x),
                  label = tags$div(
                    HTML('<font><h5><i class="fa-solid fa-play"></i><b> Colors</b></font>'),
                    bs4Dash::tooltip(
                      actionButton(
                        inputId = paste0("datvie_tip_heat_col",x), 
                        label="" , 
                        icon=icon("question"),
                        status="info",
                        size = "xs"
                      ),
                      title = "Colors to be used for the heatmap, which can be assigned by the application or specified by the users.",
                      placement = "right"
                    )
                  ),
                  choices = c("Typical" = "1", "Custom" = "2"),
                  selected = tra_hmap_heatmapcol[x]
                ),
                conditionalPanel(
                  condition = paste0("input.tra_hmap_heatmapcol",x,"== '1'"),
                  pickerInput(
                    inputId = paste0("tra_hmap_typcolhmap",x),
                    label = NULL,
                    choices =  c("blue.white.red", "green.black.red", "green.yellow.red", "purple.yellow.red", "blue.green.red", "blue.yellow.green", "cyan.white.deeppink1"),
                    selected = tra_hmap_typcolhmap[x]
                  )
                ),
                conditionalPanel(
                  condition = paste0("input.tra_hmap_heatmapcol",x,"== '2'"),
                  fluidRow(
                    column(
                      width = 4,
                      colourInput(
                        inputId = paste0("tra_hmap_lowColor",x),
                        label = "lowColor",
                        value = tra_hmap_lowColor[x],
                        returnName = TRUE
                      )
                    ),
                    column(
                      width = 4,
                      colourInput(
                        inputId = paste0("tra_hmap_midColor",x),
                        label = "midColor",
                        value = tra_hmap_midColor[x],
                        returnName = TRUE
                      )
                    ),
                    column(
                      width = 4,
                      colourInput(
                        inputId = paste0("tra_hmap_highColor",x),
                        label = "highColor",
                        value = tra_hmap_highColor[x],
                        returnName = TRUE
                      )
                    )
                  )
                ),
                pickerInput(
                  inputId = paste0("tra_hmap_poslines",x),
                  label = tags$div(
                    HTML('<font><h5><i class="fa-solid fa-play"></i><b> Add position lines?</b></font>'),
                    bs4Dash::tooltip(
                      actionButton(
                        inputId = paste0("datvie_tip_heat_posline",x), 
                        label="" , 
                        icon=icon("question"),
                        status="info",
                        size = "xs"
                      ),
                      title = "Add genomic position lines between tracks, which can be used to identify the correspondance between heatmaps and genomic regions.",
                      placement = "right"
                    )
                  ),
                  choices = c("Yes" = "1", "No" = "2"),
                  selected = tra_hmap_poslines[x]
                ),
                conditionalPanel(
                  condition = paste0("input.tra_hmap_poslines",x,"== '1'"),
                  numericInput(
                    inputId = paste0("tra_hmap_poslinhei",x),
                    label = tags$div(
                      HTML('<font><h5><i class="fa-solid fa-play"></i><b> Position lines height:</b></font>'),
                      bs4Dash::tooltip(
                        actionButton(
                          inputId = paste0("datvie_tip_heat_poslinehei",x), 
                          label="" , 
                          icon=icon("question"),
                          status="info",
                          size = "xs"
                        ),
                        title = "Height of the position lines.",
                        placement = "right"
                      )
                    ),
                    value=tra_hmap_poslinhei[x],
                    min=0,
                    max=0.8,
                    step=0.01
                  )
                )
              )
            },
            if(tra_type[[x]] == "heatmap-discrete"){
              tagList(
                pickerInput(
                  inputId = paste0("tra_heatcol_dis",x),
                  label = tags$div(
                    HTML('<font><h5><i class="fa-solid fa-play"></i><b> Data color</b></font>')
                  ),
                  #label = HTML('<font><h5><i class="fa-solid fa-play"></i><b> Data color</b></font>'),
                  #label = "Data color",
                  choices = c("Random" = "1" , "Custom" = "2"),
                  selected = tra_heatcol_dis[x]
                ),
                conditionalPanel(
                  condition = paste0("input.tra_heatcol_dis",x,"== '2'"),
                  textInput(
                    inputId =  paste0("tra_heat_heatcoldiscus",x), 
                    label =  NULL,
                    value = tra_heat_heatcoldiscus[x])
                ),
                pickerInput(
                  inputId = paste0("tra_hmap_poslines",x),
                  label = tags$div(
                    HTML('<font><h5><i class="fa-solid fa-play"></i><b> Add position lines?</b></font>'),
                    bs4Dash::tooltip(
                      actionButton(
                        inputId = paste0("datvie_tip_heat_posline2",x), 
                        label="" , 
                        icon=icon("question"),
                        status="info",
                        size = "xs"
                      ),
                      title = "Add genomic position lines between tracks, which can be used to identify the correspondance between heatmaps and genomic regions.",
                      placement = "right"
                    )
                  ),
                  choices = c("Yes" = "1", "No" = "2"),
                  selected = tra_hmap_poslines[x]
                ),
                conditionalPanel(
                  condition = paste0("input.tra_hmap_poslines",x,"== '1'"),
                  numericInput(
                    inputId = paste0("tra_hmap_poslinhei",x),
                    label = tags$div(
                      HTML('<font><h5><i class="fa-solid fa-play"></i><b> Position lines height:</b></font>'),
                      bs4Dash::tooltip(
                        actionButton(
                          inputId = paste0("datvie_tip_heat_poslinehei2",x), 
                          label="" , 
                          icon=icon("question"),
                          status="info",
                          size = "xs"
                        ),
                        title = "Height of the position lines.",
                        placement = "right"
                      )
                    ),
                    value=tra_hmap_poslinhei[x],
                    min=0,
                    max=0.8,
                    step=0.01
                  )
                )
              )
            },
            numericInput(
              inputId = paste0("heightTra",x),
              label = tags$div(
                HTML('<font><h5><i class="fa-solid fa-play"></i><b> Track height:</b></font>'),
                bs4Dash::tooltip(
                  actionButton(
                    inputId = paste0("datvie_tip_trahei",x), 
                    label="" , 
                    icon=icon("question"),
                    status="info",
                    size = "xs"
                  ),
                  title = "Height of the track.",
                  placement = "right"
                )
              ),
              value = heightTra[x],
              min=0.01,
              max=0.8,
              step=0.01
            ),
            
            numericInput(
              inputId = paste0("Tra_margin",x),
              label = tags$div(
                HTML('<font><h5><i class="fa-solid fa-play"></i><b> Distance to the next section</b></font>'),
                bs4Dash::tooltip(
                  actionButton(
                    inputId = paste0("datvie_tip_tramar",x), 
                    label="" , 
                    icon=icon("question"),
                    status="info",
                    size = "xs"
                  ),
                  title = "This parameter can also be used to tune the distance between adjacent tracks, or the distance between a track and a label data, or the distance between a track and a link data.",
                  placement = "right"
                )
              ),
              value=Tra_margin[x],
              min=0,
              max=0.1,
              step=0.01
            ),
            
            if(tra_type[[x]] == "heatmap-gradual" | tra_type[[x]] == "heatmap-discrete"){
              tagList(
                pickerInput(
                  inputId = paste0("tra_hmap_cellbord",x),
                  label = tags$div(
                    HTML('<font><h5><i class="fa-solid fa-play"></i><b> Add cell borders?</b></font>'),
                    bs4Dash::tooltip(
                      actionButton(
                        inputId = paste0("datvie_tip_heaat_cellbo",x), 
                        label="" , 
                        icon=icon("question"),
                        status="info",
                        size = "xs"
                      ),
                      title = "Add borders to the heatmap grids, which can be used to separate different cells from each other.",
                      placement = "right"
                    )
                  ),
                  choices = c("Yes" = "add", "No" = ""),
                  selected = tra_hmap_cellbord[x]
                ),
                conditionalPanel(
                  condition = paste0("input.tra_hmap_cellbord",x,"== 'add'"),
                  colourInput(
                    inputId = paste0("tra_hmap_cellbord_col",x),
                    label = tags$div(
                      HTML('<font><h5><i class="fa-solid fa-play"></i><b> Color of cell borders?</b></font>'),
                      bs4Dash::tooltip(
                        actionButton(
                          inputId = paste0("datvie_tip_heaat_bocol",x), 
                          label="" , 
                          icon=icon("question"),
                          status="info",
                          size = "xs"
                        ),
                        title = "The color to be used for the borders of heatmap grids. For example, 'white' or 'red'. Hex color codes as '#FF0000' are also supported.",
                        placement = "right"
                      )
                    ),
                    value = tra_hmap_cellbord_col[x],
                    returnName = TRUE
                  )
                )
              )
            },
            if(tra_type[[x]] != "heatmap-discrete" & tra_type[[x]] != "heatmap-gradual" & tra_type[[x]] != "ideogram"){
              pickerInput(
                inputId = paste0("tra_border",x),
                label = tags$div(
                  HTML('<font><h5><i class="fa-solid fa-play"></i><b> Add sector borders?</b></font>'),
                  bs4Dash::tooltip(
                    actionButton(
                      inputId = paste0("datvie_tip_tra_bo",x), 
                      label="" , 
                      icon=icon("question"),
                      status="info",
                      size = "xs"
                    ),
                    title = "Add borders to each sector.",
                    placement = "right"
                  )
                ),
                choices = c("Yes" = "add", "No" = ""),
                selected= tra_border[x]
              )
            },
            if(tra_type[[x]] == "point"|tra_type[[x]] == "line"|tra_type[[x]] == "bar"){
              pickerInput(
                inputId = paste0("tra_yaxis",x),
                label = tags$div(
                  HTML('<font><h5><i class="fa-solid fa-play"></i><b> Add Y-axis?</b></font>'),
                  bs4Dash::tooltip(
                    actionButton(
                      inputId = paste0("datvie_tip_tra_yax",x), 
                      label="" , 
                      icon=icon("question"),
                      status="info",
                      size = "xs"
                    ),
                    title = "Add Y-axis for all the tracks if applicable.",
                    placement = "right"
                  )
                ),
                choices = c("Yes" = "1", "No" = "2"),
                selected= tra_yaxis[x]
              )
            }
          )
        })
      })
    }
  })
  
  toListenlab <- reactive({
    lapply(1:length(labdatas), function(x){
      input[[paste0("lab_setting",x)]]
    })
  })
  observeEvent(toListenlab(),{
    if(input$dataup_go > 0){
      labdatas <- input$labdata
      lapply(1:length(labdatas), function(x){
        lab_setlist[x] <<- input[[paste0("lab_setting",x)]]
      })
      x <- which((unlist(lab_setlist)- unlist(lab_setlist_old))==1)
      lab_setlist_old <<- lab_setlist
      
      if(!is.null(input[[paste0("lab_fontsize",x)]])){
        lab_fontsize[x] <<- input[[paste0("lab_fontsize",x)]]
      }
      if(!is.null(input[[paste0("lab_fontcol",x)]])){
        lab_fontcol[x] <<- input[[paste0("lab_fontcol",x)]]
      }
      if(!is.null(input[[paste0("lab_fontper",x)]])){
        lab_fontper[x] <<- input[[paste0("lab_fontper",x)]]
      }
      if(!is.null(input[[paste0("lab_adjustfontsize",x)]])){
        lab_adjustfontsize[x] <<- input[[paste0("lab_adjustfontsize",x)]]
      }
      
      if(!is.null(input[[paste0("poslabels",x)]])){
        poslabels[x] <<- input[[paste0("poslabels",x)]]
      }
    }
  })
  observeEvent(toListenlab(),{
    labdatas <- input$labdata
    lab_num <- length(labdatas)
    labpos <<- lapply(1:length(labdatas),function(x){
      input[[paste0("labpos",x)]]
    })
    lapply(1:length(labdatas), function(x){
      output[[paste0("sortable_label_datvie",x)]] <<- renderUI({
        if(ncol(data.N[[x]]) == 4){
          tagList(
            numericInput(
              inputId = paste0("lab_fontsize",x),
              label = tags$div(
                HTML('<font><h5><i class="fa-solid fa-play"></i><b> Label data hight</b></font>'),
                bs4Dash::tooltip(
                  actionButton(
                    inputId = paste0("datvie_tip_lab_labhi",x), 
                    label="" , 
                    icon=icon("question"),
                    status="info",
                    size = "xs"
                  ),
                  title = "Height of the track occupied by the label data.",
                  placement = "right"
                )
              ),
              value= lab_fontsize[x], 
              min=0.01, 
              max=0.8,
              step=0.01
            ),
            sliderTextInput(
              inputId = paste0("lab_fontper",x),
              label = tags$div(
                HTML('<font><h5><i class="fa-solid fa-play"></i><b> Font size</b></font>'),
                  bs4Dash::tooltip(
                    actionButton(
                      inputId = paste0("datvie_tip_lab_adjustfontsize",x),
                      label="" ,
                      icon=icon("question"),
                      status="info",
                      size = "xs"
                    ),
                    title = "The font size is automatically adjusted to the height of the Label. Additionaly, you can zoom the font size using this slider.",
                    placement = "right"
                  )
              ),
              choices = c(10:200),
              selected = lab_fontper[x]
            ),
            colourInput(
              inputId = paste0("lab_fontcol",x),
              label = tags$div(
                HTML('<font><h5><i class="fa-solid fa-play"></i><b> Font color</b></font>'),
                bs4Dash::tooltip(
                  actionButton(
                    inputId = paste0("datvie_tip_lab_labcol",x), 
                    label="" , 
                    icon=icon("question"),
                    status="info",
                    size = "xs"
                  ),
                  title = "Color of the label text and the connection line.",
                  placement = "right"
                )
              ),
              value = lab_fontcol[x],
              returnName = TRUE
            ),
            pickerInput(
              inputId = paste0("poslabels",x),
              label = tags$div(
                HTML('<font><h5><i class="fa-solid fa-play"></i><b> Label Position</b></font>'),
                bs4Dash::tooltip(
                  actionButton(
                    inputId = paste0("datvie_tip_lab_labpos",x), 
                    label="" , 
                    icon=icon("question"),
                    status="info",
                    size = "xs"
                  ),
                  title = "Place the labels in the inner or the outer of the specified track?",
                  placement = "right"
                )
              ),
              choices = c("Inward" = "inside", "Outward" = "outside"),
              selected = poslabels[x]
            )
          )
          
        }else{
          tagList(
            numericInput(
              inputId = paste0("lab_fontsize",x),
              label = tags$div(
                HTML('<font><h5><i class="fa-solid fa-play"></i><b> Label data hight</b></font>'),
                bs4Dash::tooltip(
                  actionButton(
                    inputId = paste0("datvie_tip_lab_labhi",x), 
                    label="" , 
                    icon=icon("question"),
                    status="info",
                    size = "xs"
                  ),
                  title = "Height of the track occupied by the label.",
                  placement = "right"
                )
              ),
              value= lab_fontsize[x], 
              min=0.01, 
              max=0.8,
              step=0.01
            ),
            sliderTextInput(
              inputId = paste0("lab_fontper",x),
              label = tags$div(
                HTML('<font><h5><i class="fa-solid fa-play"></i><b> Font size</b></font>'),
                bs4Dash::tooltip(
                  actionButton(
                    inputId = paste0("datvie_tip_lab_adjustfontsize",x),
                    label="" ,
                    icon=icon("question"),
                    status="info",
                    size = "xs"
                  ),
                  title = "The font size is automatically adjusted to the height of the Label. Additionaly, you can zoom the font size using this slider.",
                  placement = "right"
                )
              ),
              choices = c(10:200),
              selected = lab_fontper[x]
            ),
            
            
            
            pickerInput(
              inputId = paste0("poslabels",x),
              label = tags$div(
                HTML('<font><h5><i class="fa-solid fa-play"></i><b> Label Position</b></font>'),
                bs4Dash::tooltip(
                  actionButton(
                    inputId = paste0("datvie_tip_lab_labpos",x), 
                    label="" , 
                    icon=icon("question"),
                    status="info",
                    size = "xs"
                  ),
                  title = "Place the labels in the inner or the outer of the specified track?",
                  placement = "right"
                )
              ),
              choices = c("Inward" = "inside", "Outward" = "outside"),
              selected = poslabels[x]
            )
          )
          
        }
        
      })
    })
  })
  
  toListen3 <- reactive({
    list(input$dat_vie_ok,input$updateplot)
  })
  observeEvent(toListen3(),ignoreNULL = TRUE,ignoreInit = TRUE,priority = 2,{
    if(letplotgo == 1){
      lab_pos <- lapply(1:length(labdatas),function(x){
        input[[paste0("labpos",x)]]
      })
      tra_pos <- lapply(1:length(tradatas), function(x){
        input[[paste0("trapos",x)]]
      })
      if(sum(duplicated(unlist(tra_pos))) != 0){
        sendSweetAlert(
          session = session,
          title = "",
          text = "Duplicated track indexes are not allowed.",
          type = "error"
        )
        plotflash_report <<- 0
      }else if(sum(duplicated(unlist(lab_pos))) != 0){
        sendSweetAlert(
          session = session,
          title = "",
          text = "Duplicated label indexes are not allowed.",
          type = "error"
        )
        plotflash_report <<- 0
      }else{
        plotflash_report <<- 1
      }
    }else{
      plotflash_report <<- 0
    }
  })
  
  
  observeEvent(toListen3(),ignoreNULL = TRUE,ignoreInit = TRUE,priority = 0,{
    isolate({
      if(plotflash_report == 1){
        if(length(data.T) != 0){
          length_T <- length(data.T)
        }else{
          length_T <- 1
        }
        labelChr <<- input$labelChr
        trackChr <<- input$trackChr
        
        tradat_cir <- input$tradata
        lab_pos <<- lapply(1:length(labdatas),function(x){
          input[[paste0("labpos",x)]]
        })
        if(length(labdatas) != 0){
          
          lapply(1:length(labdatas),function(x){
            
            if(!is.null(input[[paste0("poslabels",x)]])){
              poslabels[x] <<- input[[paste0("poslabels",x)]]
            }
            if(!is.null(input[[paste0("lab_fontsize",x)]])){
              lab_fontsize[x] <<- input[[paste0("lab_fontsize",x)]]
            }
            if(!is.null(input[[paste0("lab_fontcol",x)]])){
              lab_fontcol[x] <<- input[[paste0("lab_fontcol",x)]]
            }
            if(!is.null(input[[paste0("lab_adjustfontsize",x)]])){
              lab_adjustfontsize[x] <<- input[[paste0("lab_adjustfontsize",x)]]
            }
            if(!is.null(input[[paste0("lab_fontper",x)]])){
              lab_fontper[x] <<- input[[paste0("lab_fontper",x)]]
            }
          })
          labels_inf <- as.data.frame(cbind(unlist(lab_pos),unlist(poslabels),unlist(lab_fontsize),unlist(lab_fontcol),c(1:length(data.N)),unlist(lab_adjustfontsize),unlist(lab_fontper)))
        }
        tra_num <- length(tradatas)
        tra_pos <<- lapply(1:tra_num, function(x){
          input[[paste0("trapos",x)]]
        })
        
        pospos <<- as.numeric(unlist(tra_pos))
        ordertrapos <- order(pospos)
        if(tra_num != 0){
          tra_type1 <- lapply(1:tra_num, function(x){
            input[[paste0("tratype",x)]]
          })
          tratype_cache <- tra_type1
          tra_type <<- lapply(1:length(tra_pos), function(x){
            tratype_cache[[ordertrapos[[x]]]]#order(pospos)
          })
        }
        
        lapply(1:length(tradatas), function(x){
          
          if(!is.null(input[[paste0("tra_bar_direction",x)]])){
            tra_bar_direction[pospos[x]] <<- input[[paste0("tra_bar_direction",x)]]
          }
          if(!is.null(input[[paste0("tra_bar_Boundary",x)]])){
            tra_bar_Boundary[pospos[x]] <<- input[[paste0("tra_bar_Boundary",x)]]
          }
          if(!is.null(input[[paste0("tra_bar_coldir1",x)]])){
            tra_bar_coldir1[pospos[x]] <<- input[[paste0("tra_bar_coldir1",x)]]
          }
          if(!is.null(input[[paste0("tra_bar_coldir2",x)]])){
            tra_bar_coldir2[pospos[x]] <<- input[[paste0("tra_bar_coldir2",x)]]
          }
          if(!is.null(input[[paste0("tra_coltype",x)]])){
            tra_coltype[pospos[x]] <<- input[[paste0("tra_coltype",x)]]
          }
          if(!is.null(input[[paste0("tra_colcol",x)]])){
            tra_colcol[pospos[x]] <<- input[[paste0("tra_colcol",x)]]
          }
          if(!is.null(input[[paste0("tra_colcol_spec",x)]])){
            tra_colcol_spec[pospos[x]] <<- input[[paste0("tra_colcol_spec",x)]]
          }
          if(!is.null(input[[paste0("tra_colorcus",x)]])){
            tra_colorcus[pospos[x]] <<- input[[paste0("tra_colorcus",x)]]
          }
          if(!is.null(input[[paste0("tra_line_fillarea",x)]])){
            tra_line_fillarea[pospos[x]] <<- input[[paste0("tra_line_fillarea",x)]]
          }
          if(!is.null(input[[paste0("tra_rect_rectcol",x)]])){
            tra_rect_rectcol[pospos[x]] <<- input[[paste0("tra_rect_rectcol",x)]]
          }
          if(!is.null(input[[paste0("tra_rect_rectcoldis",x)]])){
            tra_rect_rectcoldis[pospos[x]] <<- input[[paste0("tra_rect_rectcoldis",x)]]
          }
          if(!is.null(input[[paste0("tra_rect_rectcoldiscus",x)]])){
            tra_rect_rectcoldiscus[pospos[x]] <<- input[[paste0("tra_rect_rectcoldiscus",x)]]
          }
          if(!is.null(input[[paste0("tra_trct_colrect",x)]])){
            tra_trct_colrect[pospos[x]] <<- input[[paste0("tra_trct_colrect",x)]]
          }
          if(!is.null(input[[paste0("tra_line_selrea",x)]])){
            tra_line_selrea[pospos[x]] <<- input[[paste0("tra_line_selrea",x)]]
          }
          if(!is.null(input[[paste0("tra_bar_borderarea",x)]])){
            tra_bar_borderarea[pospos[x]] <<- input[[paste0("tra_bar_borderarea",x)]]
          }
          if(!is.null(input[[paste0("tra_transparency",x)]])){
            tra_transparency[pospos[x]] <<- input[[paste0("tra_transparency",x)]]
          }
          if(!is.null(input[[paste0("tra_poipch",x)]])){
            tra_poipch[pospos[x]] <<- input[[paste0("tra_poipch",x)]]
          }
          if(!is.null(input[[paste0("tra_poi_poisiz",x)]])){
            tra_poi_poisiz[pospos[x]] <<- input[[paste0("tra_poi_poisiz",x)]]
          }
          if(!is.null(input[[paste0("tra_baseline",x)]])){
            tra_baseline[pospos[x]] <<- input[[paste0("tra_baseline",x)]]
          }
          if(!is.null(input[[paste0("tra_colorline",x)]])){
            tra_colorline[pospos[x]] <<- input[[paste0("tra_colorline",x)]]
          }
          if(!is.null(input[[paste0("tra_bgcol",x)]])){
            tra_bgcol[pospos[x]] <<- input[[paste0("tra_bgcol",x)]]
          }
          if(!is.null(input[[paste0("tra_hmap_heatmapcol",x)]])){
            tra_hmap_heatmapcol[pospos[x]] <<- input[[paste0("tra_hmap_heatmapcol",x)]]
          }
          if(!is.null(input[[paste0("tra_hmap_typcolhmap",x)]])){
            tra_hmap_typcolhmap[pospos[x]] <<- input[[paste0("tra_hmap_typcolhmap",x)]]
          }
          if(!is.null(input[[paste0("tra_hmap_lowColor",x)]])){
            tra_hmap_lowColor[pospos[x]] <<- input[[paste0("tra_hmap_lowColor",x)]]
          }
          if(!is.null(input[[paste0("tra_hmap_midColor",x)]])){
            tra_hmap_midColor[pospos[x]] <<- input[[paste0("tra_hmap_midColor",x)]]
          }
          if(!is.null(input[[paste0("tra_hmap_highColor",x)]])){
            tra_hmap_highColor[pospos[x]] <<-input[[paste0("tra_hmap_highColor",x)]]
          }
          if(!is.null(input[[paste0("tra_hmap_poslines",x)]])){
            tra_hmap_poslines[pospos[x]] <<- input[[paste0("tra_hmap_poslines",x)]]
          }
          if(!is.null(input[[paste0("tra_hmap_poslinhei",x)]])){
            tra_hmap_poslinhei[pospos[x]] <<- input[[paste0("tra_hmap_poslinhei",x)]]
          }
          if(!is.null(input[[paste0("tra_heatcol_dis",x)]])){
            tra_heatcol_dis[pospos[x]] <<- input[[paste0("tra_heatcol_dis",x)]]
          }
          if(!is.null(input[[paste0("tra_heat_heatcoldiscus",x)]])){
            tra_heat_heatcoldiscus[pospos[x]] <<- input[[paste0("tra_heat_heatcoldiscus",x)]]
          }
          if(!is.null(input[[paste0("heightTra",x)]])){
            heightTra[pospos[x]] <<- input[[paste0("heightTra",x)]]
          }
          if(!is.null(input[[paste0("Tra_margin",x)]])){
            Tra_margin[pospos[x]] <<- input[[paste0("Tra_margin",x)]]
          }
          if(!is.null(input[[paste0("tra_hmap_cellbord",x)]])){
            tra_hmap_cellbord[pospos[x]] <<- input[[paste0("tra_hmap_cellbord",x)]]
          }
          if(!is.null(input[[paste0("tra_hmap_cellbord_col",x)]])){
            tra_hmap_cellbord_col[pospos[x]] <<- input[[paste0("tra_hmap_cellbord_col",x)]]
          }
          if(!is.null(input[[paste0("tra_border",x)]])){
            tra_border[pospos[x]] <<- input[[paste0("tra_border",x)]]
          }
          if(!is.null(input[[paste0("tra_yaxis",x)]])){
            tra_yaxis[pospos[x]] <<- input[[paste0("tra_yaxis",x)]]
          }
        })
        Tra_margin[(length(Tra_margin)+1)] <- 0
        
        #}
        
        dis_Chr <- input$distance_Chr
        if(input$chr_type=="1"){
          chr_height <- input$heightChr
        }else{
          chr_height <- input$heightChr_cyt
        }
        source_data <- input$datatype
        datatypeChr <- input$chr_type
        trac_index <- input$trac_index
        heatmapcols <<- lapply(1:length_T, function(x){
          c(tra_hmap_lowColor[[x]],tra_hmap_midColor[[x]],tra_hmap_highColor[[x]])
        })
        if(length(data.L)!= 0){
          colformatLinks <<- input$colformatLinks
          
          if(colformatLinks == 1){
            selcolorLinks <<- input$selcolorLinks1
            colorLinks <<- input$colorLinks1
          }else{
            selcolorLinks <<- input$selcolorLinks2
            colorLinks <<- input$colorLinks2
          }
          transparencyLinks <<- input$transparencyLinks
        }
        
        transparencyhltLinks <<- input$transparencyhltLinks
        gracolinks <<- c(input$lowColinks,input$midColinks,input$highColinks)
        outAxis_size <<- input$outAxis_size
        labelChr_size <<- input$labelChr_size
        outAxis <<- input$outAxis
        gap.width <<- gsub("\\s","",strsplit(input$gapChr,",")[[1]])
        colorChr <<- gsub("\\s","",strsplit(input$colorChr,",")[[1]])
        addlegend <- input$addlegend
        
        cexlabel <<- input$cexlabel
        legendpos <- input$legendpos
        midplot <- input$midplot
        plotsize <- input$plotmultiples
        if(input$addlegend == "Yes"){
          
          if(input$legendpos == "Right"){
            sizeplot <- c(850,750)
          }else{
            sizeplot <- c(650,750)
          }
        }else{
          sizeplot <- c(750,750)
        }
        sizeplot <- sizeplot*plotsize*0.01
        
        
      
        if(!is.null(data.T)){
          rect_gra_3Col <- matrix(nrow = length(tra_type),ncol = 3)
          tratypeunlist <- unlist(tra_type)
          tra_rect_rectcolunlist <- unlist(tra_rect_rectcol)
          for (l in 1:length(tra_type)) {
            if(!(tratypeunlist[l] == "point"|tratypeunlist[l] == "line"|tratypeunlist[l] == "bar")){
              tra_yaxis[l] <<-  "2"
            }
            if(tratypeunlist[l] == "rect-gradual" & tra_rect_rectcolunlist[l] == "2"){
              rect_gra_3Col[l,] <- c(rect_gra_lowCol[[l]],rect_gra_midCol[[l]],rect_gra_highCol[[l]])
            }
            
            
            
            
            
          }
          for(k in which(tratypeunlist=="bar" | tratypeunlist=="stack-point" | tratypeunlist=="stack-line")){
            if(tra_coltype[[k]] == "2"){
              tra_colcol[[k]] = tra_colcol_spec[[k]]
            }
          }
        }
        
        
        if(1 %in% unlist(tra_yaxis) | trac_index == "Yes"){
          if(ncol(data.C) == 3){
            data.CC <- rbind(data.C,c("add",1,round(0.025*sum(data.C[,3]))))
          }else{
            kk <- tapply(data.C[,3],data.C[,1],max)
            data.CC <- rbind(data.C,c("add",1,round(0.025*sum(kk)),"1","1"))
          }
        }else{
          data.CC <- data.C
        }
        
        outergap <- input$outergap
        
        plotfig(input = input, output = output,session=session,data.C = data.CC , colorChr = colorChr , dis_Chr = dis_Chr , data.T = data.T , data.L = data.L, data.N = data.N , tra_Margin = Tra_margin , labels_inf = labels_inf , labelChr = labelChr , tra_hmap_typcolhmap = tra_hmap_typcolhmap , tra_border = tra_border ,tra_yaxis = tra_yaxis,
                trackChr = trackChr ,tratype = tra_type,source_data = source_data,chr_height = chr_height,datatypeChr = datatypeChr , heightTra = heightTra , tra_poi_poisiz = tra_poi_poisiz , heatmapcols = heatmapcols , tra_bgcol = tra_bgcol , gap.width = gap.width ,
                tra_hmap_poslines = tra_hmap_poslines , tra_hmap_poslinhei = tra_hmap_poslinhei , tra_hmap_cellbord = tra_hmap_cellbord , tra_hmap_cellbord_col = tra_hmap_cellbord_col , tra_hmap_heatmapcol = tra_hmap_heatmapcol , plotsize = sizeplot , rect_gra_3Col = rect_gra_3Col,
                tra_rect_rectcol = tra_rect_rectcol , tra_trct_colrect = tra_trct_colrect , tra_rect_rectcoldis = tra_rect_rectcoldis , tra_rect_rectcoldiscus = tra_rect_rectcoldiscus , tra_transparency = tra_transparency , tra_coltype = tra_coltype , tra_colcol = tra_colcol , tra_heatcol_dis = tra_heatcol_dis , tra_heat_heatcoldiscus = tra_heat_heatcoldiscus,
                tra_colorcus = tra_colorcus , tra_line_fillarea = tra_line_fillarea , tra_poipch = tra_poipch , tra_colorline = tra_colorline , tra_baseline = tra_baseline , outAxis = outAxis , fontSize = fontSize , outAxis_size = outAxis_size , labelChr_size = labelChr_size , tra_bar_direction = tra_bar_direction ,
                tra_bar_Boundary = tra_bar_Boundary , tra_bar_coldir1 = tra_bar_coldir1 , tra_bar_coldir2 = tra_bar_coldir2 , hltTrack.List = hltTrack.List , hltdata.List = hltdata.List , tra_line_selrea = tra_line_selrea , tra_bar_borderarea = tra_bar_borderarea , colformatLinks = colformatLinks , colorLinks = colorLinks ,
                selcolorLinks = selcolorLinks , transparencyhltLinks = transparencyhltLinks , gracolinks =  gracolinks , transparencyLinks = transparencyLinks , legendpos = legendpos , addlegend = addlegend , hlt_data = hlt_data , midplot = midplot , trapos = pospos , trac_index = trac_index , outergap = outergap
        )
      }
    })
  })
  toListenpdf <- reactive({
    list(input$dat_vie_ok,input$updateplot,input$sam_dat_vie_ok)
  })
  observeEvent(toListenpdf(),ignoreNULL = TRUE,{
    plotsize <- input$plotmultiples
    if(input$addlegend == "Yes"){
      if(input$legendpos == "Right"){
        sizeplot <- c(850,750)
      }else{
        sizeplot <- c(650,750)
      }
    }else{
      sizeplot <- c(750,750)
    }
    sizeplot <- sizeplot*plotsize*0.01
    if(input$datatype == "a"){
      plotsizepdf <<- sizeplot
    }else if(input$datatype == "b"){
      plotsizepdf <<- sam_plotsize
    }
    
    output$sortable_plot <<- renderUI({
      tagList(
        div(
          style= paste0("height:",sizeplot[2],"px;width: ",sizeplot[1],"px;"),
          shinycssloaders::withSpinner(
            plotOutput("circosfigure"),
            type = 8,
            hide.ui = TRUE,
            image.height = "auto"
          )
        )
      )
    })
    
  })
 
  
  ##clear button
  observe({
    length_T <<- length(data.T)
    for (i in 1:length_T) {
      observeEvent(input[[paste0("clearText_button",i)]],{
        updateTextInput(
          inputId = paste0("hltData",i),
          session = session,
          value = ""
        )
      })
    }
  })
  
  observeEvent(input$dataup_go | input$dataup_example_go,{
    if(!is.null(dataview_export)){
      updatebs4TabItems(
        inputId = "sidebar",
        session = session,
        selected = "dat-vie"
      )
    }
  })
  #dat_vie_ok
  observeEvent(input$dat_vie_ok | input$sam_dat_vie_ok,ignoreNULL = TRUE,ignoreInit = TRUE,priority = 1,{
    if(plotflash_report == 1){
      updatebs4TabItems(
        inputId = "sidebar",
        session = session,
        selected = "cir-par"
      )
    }
    
  })
  observeEvent(input$go_cirplot,{
    if(!is.null(input$other_plot_data)){
      data_cirplot <- read.csv(file = input$other_plot_data$datapath,header = T)
      output$cirplotplot <- renderPlot({
        circos.clear()
        ifaxis <-  input$other_plot_axis
        ifunit <- input$other_plot_unit
        unit <- input$other_plot_unitun
        iflegend <- input$other_plot_legend
        legendpos <- input$other_plot_legendpos
        ifline <- input$other_plot_line
        category <- data_cirplot[,1]
        percent <- data_cirplot[,2]
        color <- data_cirplot[,3]
        breaks <- seq(0, 85, by = 5)
        if(ifunit == "yes"){
          labelss <- paste0(breaks, "%")
          labelsscex <- 0.6
        }else{
          labelss <- NULL
          labelsscex <- NULL
        }
        circos.par("start.degree" = 90, cell.padding = c(0, 0, 0, 0))
        circos.initialize("a", xlim = c(0, 100))
        circos.track(
          ylim = c(0.5, length(percent)+0.5), track.height = 0.8, 
          bg.border = NA,
          panel.fun = function(x, y) {
            xlim = CELL_META$xlim
            if(ifline == "yes"){
              circos.segments(rep(xlim[1], 9), 1:9,
                              rep(xlim[2], 9), 1:9,
                              col = "#CCCCCC")
            }
            circos.rect(rep(0, 9),
                        1:9 - 0.45,
                        percent,
                        1:9 + 0.45,
                        col = color,
                        border = "white")
            if(iflegend == "Yes"){
              if(legendpos == "on"){
                circos.text(
                  rep(xlim[1], 9),
                  1:9,
                  paste(category, " - ", percent, unit),
                  facing = "downward",
                  adj = c(1.05, 0.5),
                  cex = 0.8
                )
              }
            }
            if(ifaxis == "yes"){
              circos.axis(
                h = "top",
                major.at = breaks,
                labels = labelss,
                labels.cex = labelsscex
              )
            }
          })
        circos.clear()
      },height=750, width=750)
    }
  })
  observe({
    output$other_plot_data1 <- reactive({
      return(!is.null(input$other_plot_data))
    })
    outputOptions(output, "other_plot_data1", suspendWhenHidden = FALSE)
    output$alldata1 <- reactive({
      return(!is.null(input$alldata))
    })
    outputOptions(output, "alldata1", suspendWhenHidden = FALSE)
    output$chrdata1 <- reactive({
      if(input$dataup_go){
        return(!is.null(input$chrdata))
      }
    })
    outputOptions(output, "chrdata1", suspendWhenHidden = FALSE)
    
  })
  
  observeEvent(input$clearhlData,{
    hlt_data <<- NULL
    updateTextInput(
      inputId = paste0("hltData"),
      session = session,
      value = ""
    )
  })
  observeEvent(input$savehlData,{
    
    hltrans <- input$hltrans
    datahlt <- input$hltData
    if(nchar(datahlt) != 0){
      tmp <- matrix(strsplit(datahlt, "\n")[[1]])
      myColnames <- c("chr","starpos","endpos","color")
      hlt_data1 <- matrix(0, length(tmp), length(myColnames))
      #if()
      colnames(hlt_data1) <- myColnames
      for(p in 1:length(tmp)){
        myRow <- strsplit(tmp[p], ",")[[1]]
        if(length(myRow)==4){
          hlt_data1[p,] <- myRow
        }
      }
      colorcol <- hlt_data1[,4]
      if(any("0" %in% hlt_data1[,1])){
        sendSweetAlert(
          session = session,
          title = "",
          text = "Format of the input data is incorrect!",
          type = "error"
        ) 
      }else if(!all(unique(hlt_data1[,1]) %in% unique(data.C[,1]))){
        sendSweetAlert(
          session = session,
          title = "",
          text = "Format of the chromosome data is incorrect!",
          type = "error"
        )
      }else{
        if(length(grep("#",colorcol))!=0){
          colorcol16 <- colorcol[grep("#",colorcol)]
          colorcol7 <- colorcol16[which(nchar(colorcol16) == 7)]
          colorcol9 <- colorcol16[which(nchar(colorcol16) == 9)]
          if(!all(nchar(colorcol16) == 7| nchar(colorcol16) == 9)){
            sendSweetAlert(
              session = session,
              title = "",
              text = "Wrong RGB error!",
              type = "error"
            )
          }else if(!any(grepl("^#([0-9a-fA-F]{6})$",colorcol7)) & length(colorcol7) != 0){
            sendSweetAlert(
              session = session,
              title = "",
              text = "Wrong RGB error!",
              type = "error"
            )
          }else if(!any(grepl("^#([0-9a-fA-F]{8})$",colorcol9)) & length(colorcol9) != 0){
            sendSweetAlert(
              session = session,
              title = "",
              text = "Wrong RGB error!",
              type = "error"
            )
          }else if(!all(colorcol[-grep("#",colorcol)] %in% colors())){
            sendSweetAlert(
              session = session,
              title = "",
              text = "Wrong color input!",
              type = "error"
            )
          }else{
            sendSweetAlert(
              session = session,
              title = "",
              text = "Please click the 'Update!' button to update the Circos plot!",
              type = "success"
            )
            for (k in 1:nrow(hlt_data1)) {
              if(nchar(hlt_data1[k,4]) != 9){
                hlt_data1[k,4] <- adjustcolor(hlt_data1[k,4], alpha.f = hltrans/100)
              }
            }
            hlt_data <<- hlt_data1
          }
        }else{
          if(!all(colorcol %in% colors())){
            sendSweetAlert(
              session = session,
              title = "",
              text = "Wrong color input!",
              type = "error"
            )
          }else{
            sendSweetAlert(
              session = session,
              title = "",
              text = "Please click the 'Update!' button to update the Circos plot!",
              type = "success"
            )
            for (k in 1:nrow(hlt_data1)) {
              hlt_data1[k,4] <- adjustcolor(hlt_data1[k,4], alpha.f = hltrans/100)
            }
            hlt_data <<- hlt_data1
          }
        }
      }
    }else{
      sendSweetAlert(
        session = session,
        title = "note",
        text = "Empty input!",
        type = "info"
      )
    }
  })
  ## *** Download PDF file ***
  observe({
    output$shinyCircos.pdf <- downloadHandler(
      filename <- function(){ paste('shinyCircos.pdf') },
      content <- function(file){
        pdf(file, width = plotsizepdf[1]/72, height = plotsizepdf[2]/72)
        print(figurecp)
        dev.off()
      }, contentType = 'application/pdf')
  })
  ## *** Download SVG file ***
  observe({
    output$shinyCircos.svg <- downloadHandler(
      filename <- function(){ paste('shinyCircos.svg') },
      content <- function(file){
        svg(file, width = plotsizepdf[1]/72, height = plotsizepdf[2]/72)
        print(figurecp)
        dev.off()
      }, contentType = 'image/svg')
  })
  ## *** Download Chinese help file ***
  observe({
    output$Chinesehelpmanual <- downloadHandler(
      filename <- function(){paste('shinyCircos_User_Manual_Chinese.pdf')},
      content <- function(file){
        file.copy("www/shinyCircos-V2.0_User_Manual_Chinese.pdf",file)
      }, contentType = 'application/pdf'
    )
  })
  ## *** Download English help file ***
  observe({
    output$Englishhelpmanual <- downloadHandler(
      filename <- function(){paste('shinyCircos_User_Manual_English.pdf')},
      content <- function(file){
        file.copy("www/shinyCircos-V2.0_User_Manual_English.pdf",file)
      }, contentType = 'application/pdf'
    )
  })
  observeEvent(input$dataup_go,{
    output$plotbutton <- reactive({
      if(!is.null(dataview_export)){
        return("1")
      }else{
        return("0")
      }
    })
    outputOptions(output, "plotbutton", suspendWhenHidden = FALSE)
  })
  
}