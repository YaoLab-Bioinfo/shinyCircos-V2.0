#******#theme
# mytheme <- create_theme(
#   
#   
#   
#   
# )
# tags$head(tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
#           tags$style(type="text/css", "select { max-width: 200px; }"),
#           tags$style(type="text/css", "textarea { max-width: 185px; }"),
#           tags$style(type="text/css", ".jslider { max-width: 200px; }"),
#           tags$style(type='text/css', ".well { max-width: 330px; }"),
#           tags$style(type='text/css', ".span4 { max-width: 330px; }"),
#           tags$style(HTML(".shiny-output-error-validation {color: red;}"))
# ) 
useSweetAlert()
#******#headerbar
header <- bs4DashNavbar(
  
  #icon = icon("home",lib = "font-awesome")
  
)

#******#sidebar
sidebar <- bs4DashSidebar(
  width = "220px",
  collapsed = FALSE,
  skin = "light",
  bs4SidebarMenu(
    id = "sidebar",
    bs4SidebarMenuItem("shinyCircos2.0",tabName = "shi-cir", icon = icon("home",lib = "font-awesome"),selected = TRUE),
    br(),
    bs4SidebarMenuItem("Data Upload",tabName = "dat-upl", icon = icon("upload",lib = "font-awesome")),
    bs4SidebarMenuItem("Circos Parameters",tabName = "dat-vie", icon = icon("cogs",lib = "font-awesome")),
    bs4SidebarMenuItem("Circos Plot",tabName = "cir-par", icon = icon("image",lib = "font-awesome")),
    bs4SidebarMenuItem("Gallery",tabName = "gal", icon = icon("adjust",lib = "font-awesome"))
    # bs4SidebarMenuItem("Help",tabName = "help", icon = icon("question",lib = "font-awesome")),
    # bs4SidebarMenuItem("Other Plot",tabName = "other-plot", icon = icon("bong",lib = "font-awesome")),
    # bs4SidebarMenuItem("Test",tabName = "test")
  )
)
#******#main body
body <- bs4DashBody(
   bs4TabItems(
     bs4TabItem(
       tabName = "shi-cir",
       HTML('<p><font size="6">shinyCircos2.0: an R/Shiny application for interactive creation of Circos plot</font></p>'),
       HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:9px 9px; background-repeat: no-repeat; background-position: 0px 50%"><font size="4">Software references</font></li></ul></p>'),
       HTML('<p>1. R Development Core Team. <i><a href="http://www.r-project.org/" target="_blank">R</a>:  A Language and Environment for Statistical Computing.</i> R Foundation for Statistical Computing, Vienna (2016) <br>
				2. RStudio and Inc. <i><a href="http://www.rstudio.com/shiny/" target="_blank">shiny</a>: Web Application Framework for R.</i> R package version 1.0.0 (2016) <br>
				3. Gu, Z. <i><a href="http://cran.r-project.org/web/packages/circlize/index.html" target="_blank">circlize</a>: Circular Visualization.</i> R package version 0.4.15 (2017) <br>
				4. Neuwirth, E. <i><a href="http://cran.r-project.org/web/packages/RColorBrewer/index.html" target="_blank">RColorBrewer</a>: ColorBrewer palettes.</i> R package version 1.1-2 (2014) <br>
				5. Lawrence, M. <i><a href="http://bioconductor.org/packages/GenomicRanges/" target="_blank">GenomicRanges</a>: Representation and manipulation of genomic intervals and variables defined along a genome.</i> R package version 1.48.0 (2016) <br>
				6. Dowle, M. <i><a href="http://cran.r-project.org/web/packages/data.table/index.html" target="_blank">data.table</a>: Extension of Data.frame.</i> R package version 1.14.2 (2021) <br>
				7. Victor Perrier. <i><a href="https://dreamrs.github.io/shinyWidgets/index.html" target="_blank">shinyWidgets</a>: Extend widgets available in shiny.</i> R package version 0.6.4 (2022) <br>
				8. Eric Bailey. <i><a href="https://ebailey78.github.io/shinyBS/" target="_blank">shinyBS</a>: Add additional functionality and interactivity to your Shiny applications.</i> R package version 0.61 (2015) <br>
				9. RStudio and Inc. <i><a href="https://rstudio.github.io/DT/" target="_blank">DT</a>: An R interface to the DataTables library.</i> R package version 0.22 (2015) <br>
				10. Andrie de Vries. <i><a href="https://rstudio.github.io/sortable/" target="_blank">sortable</a>: Enables drag-and-drop behaviour in Shiny apps.</i> R package version 0.4.5 (2021) <br>
				11. Yang Tang. <i><a href="https://cran.r-project.org/web/packages/shinyjqui/index.html" target="_blank">shinyjqui</a>: easily add interactions and animation effects to a shiny app.</i> R package version 0.4.1 (2022) <br>
				12. Dean Attali. <i><a href="https://cran.r-project.org/web/packages/colourpicker/index.html" target="_blank">colourpicker</a>: A colour picker that can be used as an input in Shiny apps</i> R package version 1.1.1 (2021) <br>
				13. Paul Murrell. <i><a href="https://cran.r-project.org/web/packages/gridBase/index.html" target="_blank">gridBase</a>: Integration of base and grid graphics.</i> R package version 0.4-7 (2014) <br>
				14. Gu, Z. <i><a href="https://bioconductor.org/packages/release/bioc/html/ComplexHeatmap.html" target="_blank">ComplexHeatmap</a>:  efficient to visualize associations between different sources of data sets and reveal potential patterns.</i> R package version 2.10.0 (2015) <br>
				15. R Core Team and contributors worldwide. <i><a href="http://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/00Index.html" target="_blank">grDevices</a>: Graphics devices and support for base and grid graphics.</i> R package version 3.3.3 (2016) <br></p>'),
       HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:9px 9px; background-repeat: no-repeat; background-position: 0px 50%"><font size="4">Further references</font></li></ul></p>'),
       h6("This application was created by ", a("Wen Yao", href="https://www.researchgate.net/profile/Wen_Yao", target="_blank"),"and Yazhou Wang",
          ". Please send bugs and feature requests to Wen Yao (ywhzau at gmail.com) or Yazhou Wang (gentelmanwang at gmail.com). This application uses the ", 
          a("shiny package from RStudio", href="http://www.rstudio.com/shiny/", target="_blank"), "."),
       HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:9px 9px; background-repeat: no-repeat; background-position: 0px 50%"><font size="5">Please cite</font></li></ul></p>'),
       h4("1111")
       
     ),
     bs4TabItem(
       tabName = "dat-upl",
       tags$head(
         tags$style("
                 input[type='file'] {width:5em;}
                 .toggleButton {width:100%;}
                 .clearButton {float:right; font-size:12px;}
                 .fa-angle-down:before, .fa-angle-up:before {float:right;}
                 .popover{text-align:left;width:500px;background-color:#000000;}
                 .popover-title{color:#FFFFFF;font-size:16px;background-color:#000000;border-color:#000000;}
                 .jhr{display: inline; vertical-align: top; padding-left: 10px;}
                 #sidebarPanel_1 {width:25em;}
                 #mainPanel_1 {left:28em; position:absolute; min-width:27em;}"
         ),
         tags$style(HTML(".shiny-output-error-validation {color: red;}")),
         tags$style(
           HTML(".checkbox {margin: 0}
                 .checkbox p {margin: 0;}
                 .shiny-input-container {margin-bottom: 0;}
                 .navbar-default .navbar-brand {color: black; font-size:150%;}
                 .navbar-default .navbar-nav > li > a {color:black; font-size:120%;}"
           )
         ),
         tags$script(
           HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});')
         )
       ),
       
       width = 12,
       br(),
       pickerInput(
         inputId = "datatype",
         label =  tags$div(
           HTML(' <font><h4>Select the data source:</font>'),
           bs4Dash::tooltip(
             actionButton(
               inputId = "datup_tip1", 
               label="" , 
               icon = icon("question"),
               status="info",
               size = "xs"
             ),
             title = "'upload data'need you choose your own data and parameters,'sample data'is not required",
             placement = "bottom"
           )
         ),
         choices = c("upload data"="a", "sample data"="b")
       ),
       br(),
       
       #
       ###upload data
       #
       
       conditionalPanel(
         condition = "input.datatype=='a'",
         fileInput(
           inputId = "alldata",
           label = tags$div(
             HTML(' <font><h4>Upload all data</font>'),
             bs4Dash::tooltip(
               actionButton(
                 inputId = "datup_tip2", 
                 label="" , 
                 icon=icon("question"),
                 status="info",
                 size = "xs"
               ),
               title = "You can upload data in batches,but remember to save each time",
               placement = "top"
             )
           ),
           multiple = TRUE
         ),
         br(),
         br(),
         conditionalPanel(
           condition = "output.alldata1",
           uiOutput("dataclassify"),
           fluidRow(
             column(
               3,
               fluidRow(
                 column(
                   6,
                   bs4Dash::tooltip(
                     actionBttn(
                       inputId = "save1",
                       label = "Save data",
                       style = "unite",
                       color = "success",
                       icon = icon("save")
                     ),
                     title = "After clicking save data, you can continue to upload data",
                     placement = "top"
                   )
                 ),
                 column(
                   6,
                   conditionalPanel(
                     condition = "input.save1",
                     bs4Dash::tooltip(
                       actionBttn(
                         inputId = "dataup_go",
                         label = "SUBMIT!!",
                         style = "unite",
                         color = "success",
                         icon = icon("forward")
                       ),
                       title = "Click to go to the next page to set parameters and preview data",
                       placement = "bottom"
                     )
                   )
                 )
               )
             ),
             column(
               9
             )
           )
         )
       ),
       
       #
       ###sample data
       #
       conditionalPanel(
         condition = "input.datatype=='b'",
         
         tags$div(
           HTML(' <font><h4>Please select the sample data plan</font>'),
           bs4Dash::tooltip(
             actionButton(
               inputId = "datup_sam_tipplan", 
               label="" , 
               icon=icon("question"),
               status="info",
               size = "xs"
             ),
             title = "Different plans correspond to different data and parameters. In sample data mode, user adjustments to parameters will not be applied.",
             placement = "bottom"
           )
         ),
         pickerInput(
           inputId = "sam_dataplan",
           label = NULL, 
           choices = c(
             "Option 1"="1",
             "Option 2"="2",
             "Option 3"="3",
             "Option 4"="4",
             "Option 5"="5",
             "Option 6"="6",
             "Option 7"="7",
             "Option 8"="8",
             "Option 9"="9",
             "Option 10"="10"
           )
         ),
         br(),
         bs4Dash::tooltip(
           actionBttn(
             inputId = "dataup_example_go",
             label = "SUBMIT!!!",
             style = "unite",
             color = "success",
             icon = icon("forward")
           ),
           title = "Click to view data and parameters",
           placement = "bottom"
         )
       )
     ),
     bs4TabItem(
       tabName = "dat-vie",
       conditionalPanel(
         condition = "input.datatype == 'b'&& input.dataup_example_go",
         uiOutput("example_data_ui"),
         # conditionalPanel(
         #   condition = "input.dataup_example_go",
         #   actionBttn(
         #     inputId = "sam_dat_vie_ok",
         #     label = "SUBMIT!!",
         #     style = "unite",
         #     color = "success",
         #     icon = icon("forward")
         #   )
         # )
         actionBttn(
           inputId = "sam_dat_vie_ok",
           label = "SUBMIT!!",
           style = "unite",
           color = "success",
           icon = icon("forward")
         )
       ),
       conditionalPanel(
         condition = "input.datatype == 'a' && input.dataup_go",
         bs4Card(
           collapsible = FALSE,
           title = "Chromosome data",
           width = 12,
           conditionalPanel(
             condition = "output.chrdata1",
             fluidRow(
               column(
                 6,
                 tags$div(
                   HTML(' <font color="#2196F3"><h4>File name</font>'),
                   bs4Dash::tooltip(
                     actionButton(
                       inputId = "datvie_tip1", 
                       label="" , 
                       icon=icon("question"),
                       status="info",
                       size = "xs"
                     ),
                     title = "The filename when uploading the file",
                     placement = "bottom"
                   )
                 )
               ),
               column(
                 6,
                 tags$div(
                   HTML(' <font color="#2196F3"><h4>Chromosome type</font>'),
                   bs4Dash::tooltip(
                     actionButton(
                       inputId = "datvie_tip2", 
                       label="" , 
                       icon=icon("question"),
                       status="info",
                       size = "xs"
                     ),
                     title = "Chromosomes data can be either general data with three columns or cytoband data with five columns. 
                     The first three columns of either type of data should be the chromosome ID,
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
                     uiOutput("sortable_chr")
                   ),
                   column(
                     2,
                     bs4Dash::tooltip(
                       actionBttn(
                         inputId = "view_chr_data",
                         label = NULL,
                         style = "unite",
                         color = "success",
                         icon = icon("eye")
                       ),
                       title = "Click to preview chromosome data",
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
                       inputId = "chr_type",
                       label = NULL,
                       choices = c("general" = "1", "cytoband" = "2")
                     )
                   ),
                   column(
                     1,
                     bs4Dash::tooltip(
                       actionBttn(
                         inputId = "chr_setting",
                         label = NULL,
                         style = "unite",
                         color = "success",
                         icon = icon("cog")
                       ),
                       title = "Chromosome parameter settings",
                       placement = "bottom"
                     )
                   )
                 )
               ),
               tags$head(tags$style(paste0("#jquidatvie_chrvie .modal-dialog{ width:1200px}"))),
               jqui_draggable(
                 bsModal(
                   id = "jquidatvie_chrvie",
                   title = NULL,
                   trigger = "view_chr_data",
                   size = "large",
                   DTOutput("viewChr")
                 )
               ),
               tags$head(tags$style("#jquicirpar_chrsetting .modal-dialog{ width:600px}")),
               jqui_draggable(
                 bsModal(
                   id = "jquicirpar_chrsetting",
                   title = NULL,
                   trigger = "chr_setting",
                   size = "large",
                   conditionalPanel(
                     condition = "input.chr_type == '1'",
                     pickerInput(
                       inputId = "trackChr",
                       label = tags$div(
                         HTML(' <font><h5><b>Chromosome band</b></font>'),
                         bs4Dash::tooltip(
                           actionButton(
                             inputId = "datvie_tip3", 
                             label="" , 
                             icon=icon("question"),
                             status="info",
                             size = "xs"
                           ),
                           title = "whether to show chromosome band",
                           placement = "bottom"
                         )
                       ),
                       choices = c("Show" = "track", "Hide" = "")
                     ),
                     conditionalPanel(
                       condition = "input.trackChr == 'track'",
                       textInput(
                         inputId = "colorChr",
                         label = tags$div(
                           HTML(' <font><h5><b>Color(s)</b></font>'),
                           bs4Dash::tooltip(
                             actionButton(
                               inputId = "datvie_tip4", 
                               label="" , 
                               icon=icon("question"),
                               status="info",
                               size = "xs"
                             ),
                             title = "Colors to be used for each chromosome/sector. Character vector of arbitrary length representing colors is accepted and adjusted automatically to the number of sectors. For example, 'grey' or 'grey,red,green,blue'. Hex color codes as '#FF0000' are also supported.",
                             placement = "bottom"
                           )
                         ),
                         value="#00EAFF"
                       ),
                       numericInput(
                         inputId = "heightChr",
                         label = tags$div(
                           HTML(' <font><h5><b>Band height:</b></font>'),
                           bs4Dash::tooltip(
                             actionButton(
                               inputId = "datvie_tip5", 
                               label="" , 
                               icon=icon("question"),
                               status="info",
                               size = "xs"
                             ),
                             title = "Height of the chromosome band, which should be greater than 0 and smaller than 0.9.",
                             placement = "bottom"
                           )
                         ),
                         value=0.02, 
                         min=0.01, 
                         max=0.9,
                         step=0.01
                       )
                     )
                   ),
                   conditionalPanel(
                     condition = "input.chr_type == '2'",
                     numericInput(
                       inputId = "heightChr_cyt",
                       label = tags$div(
                         HTML(' <font><h5><b>Band height:</b></font>'),
                         bs4Dash::tooltip(
                           actionButton(
                             inputId = "datvie_tip6", 
                             label="" , 
                             icon=icon("question"),
                             status="info",
                             size = "xs"
                           ),
                           title = "Height of the chromosome band, which should be greater than 0 and smaller than 0.9.",
                           placement = "bottom"
                         )
                       ),
                       value=0.05, 
                       min=0.01, 
                       max=0.9,
                       step=0.01
                     )
                   ),
                   pickerInput(
                     inputId = "outAxis",
                     label = tags$div(
                       HTML(' <font><h5><b>Genomic position axis</b></font>'),
                       bs4Dash::tooltip(
                         actionButton(
                           inputId = "datvie_tip7", 
                           label="" , 
                           icon=icon("question"),
                           status="info",
                           size = "xs"
                         ),
                         title = "whether to display the genomic position axis",
                         placement = "bottom"
                       )
                     ),
                     choices = c("Show" = "1", "Hide" = "2"),
                     selected = "1"
                   ),
                   conditionalPanel(
                     condition = "input.outAxis == '1'",
                     numericInput(
                       inputId = "outAxis_size",
                       label = tags$div(
                         HTML(' <font><h5><b>Genome position axis font size</b></font>'),
                         bs4Dash::tooltip(
                           actionButton(
                             inputId = "datvie_tip11", 
                             label="" , 
                             icon=icon("question"),
                             status="info",
                             size = "xs"
                           ),
                           title = "The genome position axis font size,too large may cause some problems",
                           placement = "bottom"
                         )
                       ),
                       value=0.7,
                       min=0.1,
                       max=3, 
                       step=0.1
                     )
                   ),
                   pickerInput(
                     inputId = "labelChr",
                     label = tags$div(
                       HTML(' <font><h5><b>Chromosome IDs</b></font>'),
                       bs4Dash::tooltip(
                         actionButton(
                           inputId = "datvie_tip8", 
                           label="" , 
                           icon=icon("question"),
                           status="info",
                           size = "xs"
                         ),
                         title = "whether to display the genomic IDs",
                         placement = "bottom"
                       )
                     ),
                     choices = c("Show" = "1", "Hide" = "2")
                   ),
                   conditionalPanel(
                     condition = "input.labelChr == '1'",
                     numericInput(
                       inputId = "labelChr_size",
                       label = tags$div(
                         HTML(' <font><h5><b>Chromosome IDs font size</b></font>'),
                         bs4Dash::tooltip(
                           actionButton(
                             inputId = "datvie_tip9", 
                             label="" , 
                             icon=icon("question"),
                             status="info",
                             size = "xs"
                           ),
                           title = "The font size of chromosome ID, too large may cause some problems",
                           placement = "bottom"
                         )
                       ),
                       value=1.2,
                       min=0.1,
                       max=3, 
                       step=0.1
                     )
                   ),
                   textInput(
                     inputId = "gapChr",
                     label = tags$div(
                       HTML(' <font><h5><b>Gap width(s):</b></font>'),
                       bs4Dash::tooltip(
                         actionButton(
                           inputId = "datvie_tip12", 
                           label="" , 
                           icon=icon("question"),
                           status="info",
                           size = "xs"
                         ),
                         title = "Gaps between neighbouring sectors. Numeric vector of arbitrary length is accepted and adjusted automatically to the number of sectors. For example, '1' or '1,2,3,1'. The first value corresponds to the gap between the first and the second sector.",
                         placement = "bottom"
                       )
                     ),
                     value = "1"
                   ),
                   numericInput(
                     inputId = "distance_Chr",
                     label = tags$div(
                       HTML(' <font><h5><b>Distance to next part</b></font>'),
                       bs4Dash::tooltip(
                         actionButton(
                           inputId = "datvie_tip13", 
                           label="" , 
                           icon=icon("question"),
                           status="info",
                           size = "xs"
                         ),
                         title = "The next part can be 'label''track''link'",
                         placement = "bottom"
                       )
                     ),
                     value=0.01, 
                     min=0, 
                     max=0.1,
                     step=0.01
                   )
                 )
               )
             )
           )
         ),
         bs4Card(
           collapsible = FALSE,
           title = "Track data",
           width = 12,
           uiOutput("sortable_track")
         ),
         bs4Card(
           collapsible = FALSE,
           title = "Label data",
           width = 12,
           uiOutput("sortable_label")
         ),
         bs4Card(
           collapsible = FALSE,
           title = "Link data",
           width = 12,
           uiOutput("sortable_link")
         ),
         bs4Dash::tooltip(
           actionBttn(
             inputId = "dat_vie_ok",
             label = "SUBMIT!!",
             style = "unite",
             color = "success",
             icon = icon("forward")
           ),
           title = "Draw using the current parameters",
           placement = "bottom"
         )
       )
     ),
     bs4TabItem(
       tabName = "cir-par",
       conditionalPanel(
         condition = "input.datatype == 'a'",
         conditionalPanel(
           condition = "input.dat_vie_ok",
           fluidRow(
             column(
               width = 6,
               fluidRow(
                 
                 
                 column(
                   width = 4,
                   bs4Dash::tooltip(
                     actionBttn(
                       inputId = "cirpar_legendsetting",
                       label = "Legend",
                       style = "unite",
                       color = "success",
                       icon = icon("cog")
                     ),
                     title = "Legend settings",
                     placement = "bottom"
                   ),
                   tags$head(tags$style("#jquicirpar_lengendsetting .modal-dialog{ width:1200px}")),
                   jqui_draggable(
                     bsModal(
                       id = "jquicirpar_lengendsetting",
                       title = NULL,
                       trigger = "cirpar_legendsetting",
                       size = "large",
                       h3("Legend"),
                       hr(),
                       pickerInput(
                         inputId = "addlegend",
                         label = tags$div(
                           HTML(' <font><h5><b>Add legend</b></font>'),
                           bs4Dash::tooltip(
                             actionButton(
                               inputId = "cirplo_leg_tip1", 
                               label="" , 
                               icon=icon("question"),
                               status="info",
                               size = "xs"
                             ),
                             title = "Whether to add a legend",
                             placement = "bottom"
                           )
                         ),
                         choices = c("yes" , "no"),
                         selected = "no"
                       ),
                       conditionalPanel(
                         condition = "input.addlegend == 'yes'",
                         hr(),
                         conditionalPanel(
                           condition = "input.addlegend == 'yes'",
                           pickerInput(
                             inputId = "legendpos",
                             label = tags$div(
                               HTML(' <font><h5><b>Legend position</b></font>'),
                               bs4Dash::tooltip(
                                 actionButton(
                                   inputId = "cirplo_leg_tip2", 
                                   label="" , 
                                   icon=icon("question"),
                                   status="info",
                                   size = "xs"
                                 ),
                                 title = "Legend position, providing both right and bottom positions",
                                 placement = "bottom"
                               )
                             ),
                             choices = c("Right","Bottom"),
                             selected = "Right"
                           )
                         )
                       )
                     )
                   )
                 ),
                 column(
                   width = 4,
                   bs4Dash::tooltip(
                     actionBttn(
                       inputId = "cirpar_plotsetting",
                       label = "Plot and highlight",
                       style = "unite",
                       color = "success",
                       icon = icon("cog")
                     ),
                     title = "Image size.NOTE: width and height affect the display of the legend",
                     placement = "bottom"
                   ),
                   tags$head(tags$style("#jquicirpar_plotsetting .modal-dialog{ width:1200px}")),
                   jqui_draggable(
                     bsModal(
                       id = "jquicirpar_plotsetting",
                       title = NULL,
                       trigger = "cirpar_plotsetting",
                       size = "large",
                       h3("plot size"),
                       fluidRow(
                         column(
                           width = 6,
                           numericInput(
                             inputId = "plotsize_width",
                             label = "width",
                             value=750, 
                             step=1
                           )
                         ),
                         column(
                           width = 6,
                           numericInput(
                             inputId = "plotsize_height",
                             label = "height",
                             value=750, 
                             step=1
                           )
                         )
                       ),
                       hr(),
                       h3("highlignt"),
                       tags$div(
                         HTML(' <font><h4>Paste data below:</font>'),
                         bs4Dash::tooltip(
                           actionButton(
                             inputId = "cirplo_highlight", 
                             label="" , 
                             icon=icon("question"),
                             status="info",
                             size = "xs"
                           ),
                           title = "Each row should contain five components separated by commas including:start sector position, end sector position, start track position,end track position, color value with transparency. For example, 'chr1,1,10000,'#FF0000'.Don not forget transparency it is important.",
                           placement = "bottom"
                         )
                       ),
                       tags$textarea(
                         id = "hltData",
                         rows = 10,
                         cols = 100,
                         ""
                       ),
                       sliderTextInput(
                         inputId = "hltrans",
                         label = HTML(' <font><h4>Choose transparency(%)</font>'), 
                         choices = c(1:100),
                         selected = 25
                       ),
                       br(),
                       actionBttn(
                         inputId = "savehlData",
                         label = "SAVE",
                         style = "unite",
                         color = "success",
                         size = "sm"
                       ),
                       actionBttn(
                         inputId = "clearhlData",
                         label = "CLEAN",
                         style = "unite",
                         color = "danger",
                         size = "sm"
                       )
                     )
                   )
                 ),
                 column(
                   width = 4,
                   bs4Dash::tooltip(
                     actionBttn(
                       inputId = "updateplot",
                       label = "Update",
                       style = "unite",
                       color = "success"
                     ),
                     title = "Redraw using the current parameters",
                     placement = "bottom"
                   )
                 )
               )
             ),
             column(
               width = 6
             )
           )
         )
       ),
       conditionalPanel(
         condition = "output.circosfigure",
         downloadButton("shinyCircos.pdf", "Download pdf-file"),
         downloadButton("shinyCircos.svg", "Download svg-file")
       ),
       conditionalPanel(
         condition = "input.dat_vie_ok >= 1 | input.sam_dat_vie_ok >= 1",
         shinycssloaders::withSpinner(
           #plotOutput("circosfigure", height='100%', width='100%'),
           plotOutput("circosfigure"),
           type = 8,
           hide.ui = FALSE
         )
       )
     ),
     # bs4TabItem(
     #   tabName = "help",
     #   includeHTML("README.html")
     #   
     # ),
     bs4TabItem(
       tabName = "gal",
       includeHTML("www/Gallery.html")
       
     )
     # bs4TabItem(
     #   tabName = "test",
     #   actionBttn(
     #     inputId = "testbottom",
     #     label = "Update",
     #     style = "unite",
     #     color = "success"
     #   ),
     #   #plotOutput("testplot")
     #   shinycssloaders::withSpinner(
     #     plotOutput("testplot"),
     #     type = 8
     #     #hide.ui = FALSE
     #   )
     #   
     # )
   )
)


#bottom bar
foot <- bs4DashFooter()

ui <- bs4DashPage(header, sidebar, body,foot,controlbar = NULL,scrollToTop = TRUE,dark = NULL,title = "shinyCircos2.0")#,freshTheme = mytheme)