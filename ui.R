useSweetAlert()
#******#headerbar
header <- bs4DashNavbar(
)
#******#sidebar
sidebar <- bs4DashSidebar(
  width = "220px",
  collapsed = FALSE,
  skin = "light",
  tags$head(
    tags$script('<script src="https://kit.fontawesome.com/29eb425cd7.js" crossorigin="anonymous"></script>')
  ),
  bs4SidebarMenu(
    id = "sidebar",
    bs4SidebarMenuItem("shinyCircos-V2.0",tabName = "shi-cir", icon = icon("house",lib = "font-awesome"),selected = TRUE),
    br(),
    bs4SidebarMenuItem("Data Upload",tabName = "dat-upl", icon = icon("upload",lib = "font-awesome")),
    bs4SidebarMenuItem("Circos Parameters",tabName = "dat-vie", icon = icon("gears",lib = "font-awesome")),
    bs4SidebarMenuItem("Circos Plot",tabName = "cir-par", icon = icon("image",lib = "font-awesome")),
    #bs4SidebarMenuItem("Gallery",tabName = "gal", icon = icon("adjust",lib = "font-awesome")),
    bs4SidebarMenuItem("Gallery",tabName = "gal", icon = icon("circle-half-stroke",lib = "font-awesome")),
    bs4SidebarMenuItem("Help",tabName = "help", icon = icon("question",lib = "font-awesome")),
    bs4SidebarMenuItem("About",tabName = "about", icon = icon("info",lib = "font-awesome")),
    bs4SidebarMenuItem("Contact",tabName = "contact", icon = icon("compass"))
  )
)
#******#main body
body <- bs4DashBody(
  
   bs4TabItems(
     bs4TabItem(
       tabName = "shi-cir",
       h1("Welcome to shinyCircos-V2.0!"),
       br(),
       shiny::includeHTML("www/main.html")
     ),
     bs4TabItem(
       tabName = "dat-upl",
       tags$head(
         HTML('<script src="https://kit.fontawesome.com/29eb425cd7.js" crossorigin="anonymous"></script>'),
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
       br(),
       pickerInput(
         inputId = "datatype",
         label =  tags$div(
           style = "width:1200px",
           HTML('<font><h4><i class="fa-solid fa-play"></i> Step 1. Upload data or load example data?</font>'),
           bs4Dash::tooltip(
             actionButton(
               inputId = "datup_tip1", 
               label="" , 
               icon = icon("question"),
               status="info",
               size = "xs"
             ),
             title = "To 'upload data', you need to upload your own data from the local disk to the shinyCircos web server.",
             placement = "bottom"
           )
         ),
         choices = c("Upload data"="a", "Load example data"="b")
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
             style = "width:1200px",
             HTML('<font><h4><i class="fa-solid fa-play"></i> Step 2. Upload one or multiple input datasets:</font>'),
             bs4Dash::tooltip(
               actionButton(
                 inputId = "datup_tip2", 
                 label="" , 
                 icon=icon("question"),
                 status="info",
                 size = "xs"
               ),
               title = "You can upload multiple datasets at the same time. Remember to save the uploaded data timely.",
               
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
               11,
               fluidRow(
                 column(
                   5,
                   bs4Dash::tooltip(
                     actionBttn(
                       inputId = "save1",
                       label = "Step 2.2. Save uploaded data",
                       style = "unite",
                       color = "success",
                       icon = icon("floppy-disk",lib = "font-awesome")
                     ),
                     title = "You can upload multiple datasets separately. Remember to save the uploaded data timely.",
                     placement = "top"
                   )
                 ),
                 column(
                   7,
                   conditionalPanel(
                     condition = "input.save1",
                     bs4Dash::tooltip(
                       actionBttn(
                         inputId = "dataup_go",
                         label = "Step 3. Submit the uploaded datasets!",
                         style = "unite",
                         color = "success",
                         icon = icon("forward")
                       ),
                       title = "Click this button to submit the uploaded data and go to the next page for parameters setting.",
                       placement = "bottom"
                     )
                   )
                 )
               )
             ),
             column(
               1
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
           HTML('<font><h4><i class="fa-solid fa-play"></i> Step 2. Choose an example dataset:</font>'),
           bs4Dash::tooltip(
             actionButton(
               inputId = "datup_sam_tipplan", 
               label="" , 
               icon=icon("question"),
               status="info",
               size = "xs"
             ),
             title = "Different parameters were pre-setted for different example datasets, which can not be adjusted.",
             placement = "bottom"
           )
         ),
         pickerInput(
           inputId = "sam_dataplan",
           label = NULL, 
           choices = c(
             "Example dataset 1"="1",
             "Example dataset 2"="2",
             "Example dataset 3"="3",
             "Example dataset 4"="4",
             "Example dataset 5"="5",
             "Example dataset 6"="6",
             "Example dataset 7"="7",
             "Example dataset 8"="8",
             "Example dataset 9"="9",
             "Example dataset 10"="10"
           )
         ),
         br(),
         bs4Dash::tooltip(
           actionBttn(
             inputId = "dataup_example_go",
             label = "Step 3. Submit the example datasets!",
             style = "unite",
             color = "success",
             icon = icon("forward")
           ),
           title = "Click this button to submit the chosen example datasets and go to the next page to view the example data and the pre-set parameters.",
           placement = "bottom"
         )
       )
     ),
     bs4TabItem(
       tabName = "dat-vie",
       conditionalPanel(
         condition = "input.datatype == 'b'&& input.dataup_example_go",
         uiOutput("example_data_ui"),
         bs4Dash::tooltip(
           actionBttn(
             inputId = "sam_dat_vie_ok",
             label = "Step 4. Submit the plot parameters to make the Circos plot!",
             style = "unite",
             color = "success",
             icon = icon("forward")
           ),
           title = "Click this button to make a Circos plot using the currently set parameter values.",
           placement = "bottom"
         )
       ),
       conditionalPanel(
         condition = "input.datatype == 'a' && input.dataup_go",
         bs4Card(
           collapsible = FALSE,
           title = HTML('<i class="fa-solid fa-circle"></i> Chromosome data (used to define the chromosomes of a Circos plot)'),
           width = 12,
           conditionalPanel(
             condition = "output.chrdata1",
             fluidRow(
               column(
                 6,
                 tags$div(
                   HTML(' <font color="#2196F3"><h4><i class="fa-solid fa-play"></i> File name</font>'),
                   bs4Dash::tooltip(
                     actionButton(
                       inputId = "datvie_tip1", 
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
                   HTML(' <font color="#2196F3"><h4><i class="fa-solid fa-play"></i> Chromosome type</font>'),
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
                         icon = icon("gear",lib = "font-awesome")
                       ),
                       title = "Set parameters for chromosome data.",
                       placement = "bottom"
                     )
                   )
                 )
               ),
               tags$head(tags$style(paste0("#jquidatvie_chrvie .modal-dialog{ max-width:1200px}"))),
               jqui_draggable(
                 bsModal(
                   id = "jquidatvie_chrvie",
                   title = NULL,
                   trigger = "view_chr_data",
                   size = "large",
                   DTOutput("viewChr")
                 )
               ),
               tags$head(tags$style("#jquicirpar_chrsetting .modal-dialog{ width:1200px}")),
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
                         HTML('<font><h5><i class="fa-solid fa-play"></i><b> Chromosome band</b></font>'),
                         bs4Dash::tooltip(
                           actionButton(
                             inputId = "datvie_tip3", 
                             label="" , 
                             icon=icon("question"),
                             status="info",
                             size = "xs"
                           ),
                           title = "Display or hide the chromosome band?",
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
                           HTML('<font><h5><i class="fa-solid fa-play"></i><b> Color(s):</b></font>'),
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
                         value="grey"
                       ),
                       numericInput(
                         inputId = "heightChr",
                         label = tags$div(
                           HTML('<font><h5><i class="fa-solid fa-play"></i><b>Band height:</b></font>'),
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
                         HTML('<font><h5><i class="fa-solid fa-play"></i><b> Band height:</b></font>'),
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
                       HTML('<font><h5><i class="fa-solid fa-play"></i><b>Axis for genomic position</b></font>'),
                       bs4Dash::tooltip(
                         actionButton(
                           inputId = "datvie_tip7", 
                           label="" , 
                           icon=icon("question"),
                           status="info",
                           size = "xs"
                         ),
                         title = "Display or hide the axis for genomic position?",
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
                         HTML('<font><h5><i class="fa-solid fa-play"></i><b>Font size of the axis for genomic position</b></font>'),
                         bs4Dash::tooltip(
                           actionButton(
                             inputId = "datvie_tip11", 
                             label="" , 
                             icon=icon("question"),
                             status="info",
                             size = "xs"
                           ),
                           title = "Too large font size may cause problems.",
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
                       HTML('<font><h5><i class="fa-solid fa-play"></i><b>Chromosome IDs</b></font>'),
                       bs4Dash::tooltip(
                         actionButton(
                           inputId = "datvie_tip8", 
                           label="" , 
                           icon=icon("question"),
                           status="info",
                           size = "xs"
                         ),
                         title = "Display or hide the chromosome IDs?",
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
                         HTML('<font><h5><i class="fa-solid fa-play"></i><b>Font size of chromosome IDs</b></font>'),
                         bs4Dash::tooltip(
                           actionButton(
                             inputId = "datvie_tip9", 
                             label="" , 
                             icon=icon("question"),
                             status="info",
                             size = "xs"
                           ),
                           title = "Too large font size may cause problems.",
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
                       HTML('<font><h5><i class="fa-solid fa-play"></i><b>Distances between adjacent sectors</b></font>'),
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
                       HTML('<font><h5><i class="fa-solid fa-play"></i><b>Distance between adjacent tracks</b></font>'),
                       bs4Dash::tooltip(
                         actionButton(
                           inputId = "datvie_tip13", 
                           label="" , 
                           icon=icon("question"),
                           status="info",
                           size = "xs"
                         ),
                         title = "This parameter can also be used to tune the distance between a track and a label data, or the distance between a track and a link data.",
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
           title = HTML('<i class="fa-solid fa-circle"></i> Track data (to be displayed in different tracks of a Circos plot)'),
           width = 12,
           uiOutput("sortable_track")
         ),
         bs4Card(
           collapsible = FALSE,
           title = HTML('<i class="fa-solid fa-circle"></i> Label data (used to label elements in a track)'),
           width = 12,
           uiOutput("sortable_label")
         ),
         bs4Card(
           collapsible = FALSE,
           title = HTML('<i class="fa-solid fa-circle"></i> Link data (used to create links in a Circos plot)'),
           width = 12,
           uiOutput("sortable_link")
         ),
         bs4Dash::tooltip(
           actionBttn(
             inputId = "dat_vie_ok",
             label = "Step 4. Submit the plot parameters to make the Circos plot!",
             style = "unite",
             color = "success",
             icon = icon("forward")
           ),
           title = "Click this button to make a Circos plot using the currently set parameter values.",
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
                   actionBttn(
                     inputId = "cirpar_advancesetting",
                     label = "Advanced options",
                     style = "unite",
                     color = "success",
                     icon = icon("gears",lib = "font-awesome")
                   ),
                   tags$head(tags$style("#jquicirpar_advancesetting .modal-dialog{ max-width:800px}")),
                   jqui_draggable(
                     bsModal(
                       id = "jquicirpar_advancesetting",
                       title = NULL,
                       trigger = "cirpar_advancesetting",
                       size = "large",
                       fluidRow(
                         column(
                           width = 6
                         ),
                         column(
                           width = 6,
                           
                         )
                       ),
                       fluidRow(
                         column(
                           width = 6,
                           pickerInput(
                             inputId = "addlegend",
                             label = tags$div(
                               HTML('<font><h4><i class="fa-solid fa-play"></i> Add legend</font>'),
                               bs4Dash::tooltip(
                                 actionButton(
                                   inputId = "cirplo_leg_tip1",
                                   label="" ,
                                   icon=icon("question"),
                                   status="info",
                                   size = "xs"
                                 ),
                                 title = "Legend position",
                                 placement = "bottom"
                               )
                             ),
                             choices = c("Yes", "No"),
                             selected = "no"
                           )
                         ),
                         column(
                           width = 6,
                           conditionalPanel(
                             condition = "input.addlegend == 'Yes'",
                             pickerInput(
                               inputId = "legendpos",
                               label = tags$div(
                                 HTML('<font><h4><i class="fa-solid fa-play"></i> Legend position</font>'),
                                 bs4Dash::tooltip(
                                   actionButton(
                                     inputId = "cirplo_leg_tip2",
                                     label="" ,
                                     icon=icon("question"),
                                     status="info",
                                     size = "xs"
                                   ),
                                   title = "Place the legend at the right or the bottom of the plot?",
                                   placement = "bottom"
                                 )
                               ),
                               choices = c("Right","Bottom"),
                               selected = "Right"
                             )
                           )
                         )
                       ),
                       hr(),
                       sliderTextInput(
                         inputId = "plotmultiples",
                         label = tags$div(
                           HTML('<font><h4><i class="fa-solid fa-play"></i> Zoom the plot (%)</font>')
                         ),
                         choices = c(50:300),
                         selected = 100
                       ),
                       hr(),
                       tags$div(
                         HTML('<font><h4><i class="fa-solid fa-play"></i> Highlight genomic regions with input data pasted below:</font>'),
                         bs4Dash::tooltip(
                           actionButton(
                             inputId = "cirplo_highlight",
                             label="" ,
                             icon=icon("question"),
                             status="info",
                             size = "xs"
                           ),
                           title = "Each row should contain five values separated by commas indicating the start sector position, the end sector position, start track index, end track index, and the color. For example, 'chr1,1,10000,'#FF0000'.",
                           placement = "bottom"
                         )
                       ),
                       tags$textarea(
                         id = "hltData",
                         rows = 5,
                         cols = 60,
                         ""
                       ),
                       sliderTextInput(
                         inputId = "hltrans",
                         label = tags$div(
                           style = "with:1200px;",
                           HTML('<font><h4><i class="fa-solid fa-play"></i> Transparency (%)</font>'),
                         ),
                         choices = c(1:100),
                         selected = 25
                       ),
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
                       ),
                       hr(),
                       pickerInput(
                         inputId = "trac_index",
                         label = tags$div(
                           HTML('<font><h4><i class="fa-solid fa-play"></i> Add track index</font>'),
                           bs4Dash::tooltip(
                             actionButton(
                               inputId = "cirplo_leg_tip3",
                               label="" ,
                               icon=icon("question"),
                               status="info",
                               size = "xs"
                             ),
                             title = "Display the index of each track.",
                             placement = "bottom"
                           )
                         ),
                         choices = c("Yes", "No"),
                         selected = "No"
                       ),
                       hr(),
                       bs4Dash::tooltip(
                         actionBttn(
                           inputId = "updateplot",
                           label = "Update",
                           style = "unite",
                           color = "success"
                         ),
                         title = "Update the Circos plot using the current parameter values.",
                         placement = "bottom"
                       )
                       
                     )
                   )
                 ),
                 column(
                   width = 8,
                   conditionalPanel(
                     condition = "output.circosfigure",
                     fluidRow(
                       column(
                         width = 6,
                         downloadBttn(
                           outputId = "shinyCircos.pdf", 
                           label = "Download PDF-file",
                           style = "unite",
                           color = "success"
                         )
                       ),
                       column(
                         width = 6,
                         downloadBttn(
                           outputId = "shinyCircos.svg",
                           label = "Download SVG-file",
                           style = "unite",
                           color = "success"
                         )
                       )
                       
                     )
                     
                   )
                   
                 )
               )
             ),
             column(
               width = 6,
               
             )
           )
         )
       ),
       
       conditionalPanel(
         condition = "input.dat_vie_ok >= 1 | input.sam_dat_vie_ok >= 1",
         uiOutput("sortable_plot")
       )
     ),
     bs4TabItem(
       tabName = "gal",
       includeHTML("www/gallery.html")
     ),
     bs4TabItem(
       tabName = "help",
       HTML('<font><h3><i class="fa-solid fa-play"></i> Language</font>'),
       fluidRow(
         column(
           width = 4,
           pickerInput(
             inputId = "helplan",
             label = NULL,
             choices = c("English" = 1,"简体中文" = 2),
             selected = 1
           ),
         ),
         column(
           width = 8
         )
       ),
       hr(),
       conditionalPanel(
         condition = "input.helplan == '1'",
         includeHTML("www/help-English.html")
       ),
       conditionalPanel(
         condition = "input.helplan == '2'",
         includeHTML("www/help-Chinese.html")
       )
     ),
     bs4TabItem(
       tabName = "about",
       shiny::includeHTML("www/about.html")
     ),
     bs4TabItem(
       tabName = "contact",
       shiny::includeHTML("www/contact.html")
     )
   )
)


#bottom bar
foot <- bs4DashFooter(
  includeHTML("www/ffooter.html")
)

ui <- bs4DashPage(
  header, sidebar, body , footer = foot ,controlbar = NULL,scrollToTop = TRUE,dark = NULL,title = "Welcome to shinyCircos-V2.0"
)