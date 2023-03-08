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
    tags$script('<script src="https://kit.fontawesome.com/29eb425cd7.js" crossorigin="anonymous"></script>'),
    
    
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
    tags$style("
         .right-button-class:hover p{
          	display:block;
          	transform-origin: 100% 0%;
          	-webkit-animation: fadeIn 0.6s ease-in-out;
          	animation: fadeIn 0.6s ease-in-out;
          }
          .right-button-class p{
          	display: none;
          	text-align: left;
          	background-color: #1E2021;
          	padding: 19px;
          	width: 350px;
          	position: absolute;
          	border-radius: 3px;
          	box-shadow: 1px 1px 1px rgba(0, 0, 0, 0.2);
          	right: 45px;
          	top: 40px;
          	color: #FFF;
          	font-size: 15px;
          	line-height: 1.4;
          	z-index:3;
            word-wrap:break-word;
          }
          .right-button-class p:before{
          	position: relative;
          	content: '';
          	border: 6px solid transparent;
          	border-bottom-color: #1E2021;
          }
          
          .right-button-class p:after{
          	width:100%;
          	height:40px;
          	content:'';
          	position: relative;
          	top:0px;
          	left:0;
          }
                 "),
    tags$style("
         .eye-button-class:hover p{
          	display:block;
          	transform-origin: 100% 0%;
          	-webkit-animation: fadeIn 0.6s ease-in-out;
          	animation: fadeIn 0.6s ease-in-out;
          }
          .eye-button-class p{
          	display: none;
          	text-align: left;
          	background-color: #1E2021;
          	padding: 19px;
          	width: 250px;
          	position: absolute;
          	border-radius: 3px;
          	box-shadow: 1px 1px 1px rgba(0, 0, 0, 0.2);
          	right: -40px;
          	top: 40px;
          	color: #FFF;
          	font-size: 15px;
          	line-height: 1.4;
          	z-index:3;
            word-wrap:break-word;
          }
          .eye-button-class p:before{
          	position: relative;
          	content: '';
          	border: 6px solid transparent;
          	border-bottom-color: #1E2021;
          }
          
          .eye-button-class p:after{
          	width:100%;
          	height:40px;
          	content:'';
          	position: relative;
          	top:0px;
          	left:0;
          }
                 "),
    
    tags$style("
         .help-tip{
           position: relative;
           text-align: center;
        	background-color: #17A2B8;          
        	border-radius: 10%;
        	left: 16px;              
        	bottom: 0px;           
        	width: 18px;            
        	height: 25px;
        	font-size: 15px;        
        	line-height: 25px;     
        	cursor: default;
         }
         .help-tip:before{
        		content:'?';
          	font-weight: bold;
          	color:#FFFFFF;
         }
         .help-tip:hover p{
          	display:block;
          	transform-origin: 100% 0%;
          	-webkit-animation: fadeIn 0.6s ease-in-out;
          	animation: fadeIn 0.6s ease-in-out;
          }
          .help-tip p{
          	display: none;
          	text-align: left;           
          	background-color: #1E2021;
          	padding: 19px;              
          	width: 350px;  
          	position: absolute;
          	border-radius: 3px;
          	box-shadow: 1px 1px 1px rgba(0, 0, 0, 0.2);
          	right: -340px;        
          	top: 15px;                 
          	color: #FFF;
          	font-size: 15px;
          	line-height: 1.4;
          	z-index:3;
          }
          
          .help-tip p:before{ 
          	position: relative;
          	content: '';
          	border: 6px solid transparent;
          	border-bottom-color: #1E2021;
          }
          
          .help-tip p:after{ 
          	width:100%;
          	height:40px;
          	content:'';
          	position: relative;
          	top:0px;
          	left:0;
          }
        "),
    tags$style("
         .help-right-tip{
           position: relative;
           text-align: center;
        	background-color: #17A2B8;          
        	border-radius: 10%;
        	left: 16px;              
        	bottom: 0px;           
        	width: 18px;            
        	height: 25px;
        	font-size: 15px;        
        	line-height: 25px;     
        	cursor: default;
         }
         .help-right-tip:before{
        		content:'?';
          	font-weight: bold;
          	color:#FFFFFF;
         }
         .help-right-tip:hover p{
          	display:block;
          	transform-origin: 100% 0%;
          	-webkit-animation: fadeIn 0.6s ease-in-out;
          	animation: fadeIn 0.6s ease-in-out;
          }
          .help-right-tip p{	
          	display: none;
          	text-align: left;           
          	background-color: #1E2021;
          	padding: 19px;              
          	width: 350px;  
          	position: absolute;
          	border-radius: 3px;
          	box-shadow: 1px 1px 1px rgba(0, 0, 0, 0.2);
          	right: 10px;        
          	top: 15px;
          	color: #FFF;
          	font-size: 15px;
          	line-height: 1.4;
          	z-index:3;
          }
          
          .help-right-tip p:before{ 
          	position: relative;
          	content: '';
          	border: 6px solid transparent;
          	border-bottom-color: #1E2021;
          }
          
          .help-right-tip p:after{ 
          	width:100%;
          	height:40px;
          	content:'';
          	position: relative;
          	top:0px;
          	left:0;
          }
        "),
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
  bs4SidebarMenu(
    id = "sidebar",
    bs4SidebarMenuItem("shinyCircos-V2.0",tabName = "shi-cir", icon = icon("house",lib = "font-awesome"),selected = TRUE),
    br(),
    bs4SidebarMenuItem("Data Upload",tabName = "dat-upl", icon = icon("upload",lib = "font-awesome")),
    bs4SidebarMenuItem("Circos Parameters",tabName = "dat-vie", icon = icon("gears",lib = "font-awesome")),
    bs4SidebarMenuItem("Circos Plot",tabName = "cir-par", icon = icon("image",lib = "font-awesome")),
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
       br(),
       pickerInput(
         inputId = "datatype",
         label =  tags$div(
           style = "width:1200px",
           HTML("<table><tr>
         <td><div><font><h4><i class='fa-solid fa-play'></i> Step 1. Upload data or load example data?</font></div></td>
         <td><div class='help-tip'><p style='font-weight: lighter;'> To 'upload data', you need to upload your own data from the local disk to the shinyCircos web server.</p></div></td>
         </tr></table>")
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
             HTML("<table><tr>
         <td><div><font><h4><i class='fa-solid fa-play'></i> Step 2. Upload one or multiple input datasets:</font></div></td>
         <td><div class='help-tip'><p style='font-weight: lighter;'>You can upload multiple datasets at the same time. Remember to save the uploaded data timely.</p></div></td>
         </tr></table>")
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
                       title = "Click this button to submit the uploaded data and go to the 'Circos Parameters' page for parameters setting.",
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
           HTML("<table><tr>
         <td><div><font><h4><i class='fa-solid fa-play'></i> Step 2. Choose an example dataset:</font></div></td>
         <td><div class='help-tip'><p>Different parameters were pre-setted for different example datasets, which can not be adjusted.</p></div></td>
         </tr></table>")
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
           title = "Click this button to submit the chosen example datasets and go to the 'Circos Parameters' page to view the example data and the pre-set parameters.",
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
         condition = "output.plotbutton == '1' & input.datatype == 'a'",
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
                   HTML("<table><tr>
         <td><div><font color='#2196F3'><h4><i class='fa-solid fa-play'></i> File name</font></div></td>
         <td><div class='help-tip'><p>Name of the uploaded file.</p></div></td>
         </tr></table>")
                 )
               ),
               column(
                 6,
                 tags$div(
                   HTML("<table><tr>
         <td><div><font color='#2196F3'><h4><i class='fa-solid fa-play'></i> Chromosome data type</font></div></td>
         <td><div class='help-right-tip'><p>Chromosomes data can be either general data with 3 columns or cytoband data with 5 columns. 
         The first 3 columns of either type of data should be the chromosome ID,
         the start and end coordinates of different genomic regions. See example data for more details.</p></div></td>
         </tr></table>")
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
                     div(
                       class = "eye-button-class",
                       actionBttn(
                         inputId = "view_chr_data",
                         label = NULL,
                         style = "unite",
                         color = "success",
                         icon = icon("eye")
                       ),
                       p(
                         'Click to view the dataset.'
                       )
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
                       choices = c("general data" = "1", "cytoband data" = "2")
                     )
                   ),
                   column(
                     1,
                     div(
                       class = "right-button-class",
                       actionBttn(
                         inputId = "chr_setting",
                         label = NULL,
                         style = "unite",
                         color = "success",
                         icon = icon("gear",lib = "font-awesome")
                       ),
                       p(
                         'Set parameters for chromosome data.'
                       )
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
                         HTML("<table><tr>
         <td><div><font color='red'><h5><i class='fa-solid fa-play'></i><b> Display chromosome band?</b></font></div></td>
         <td><div class='help-tip'><p>Display or hide the chromosome band?</p></div></td>
         </tr></table>")
                       ),
                       choices = c("Yes" = "track", "No" = "")
                     ),
                     conditionalPanel(
                       condition = "input.trackChr == 'track'",
                       textInput(
                         inputId = "colorChr",
                         label = tags$div(
                           HTML("<table><tr>
         <td><div><font color='red'><h5><i class='fa-solid fa-play'></i><b> Color(s) for chromosome band</b></font></div></td>
         <td><div class='help-tip'><p>Colors to be used for chromosome bands. Character vector of arbitrary length representing colors is accepted and adjusted automatically to the number of chromosomes. For example, 'grey' or 'grey,red,green,blue'. Hex color codes as '#FF0000' are also supported.</p></div></td>
         </tr></table>")
                         ),
                         value="grey"
                       ),
                       numericInput(
                         inputId = "heightChr",
                         label = tags$div(
                           HTML("<table><tr>
         <td><div><font color='red'><h5><i class='fa-solid fa-play'></i><b> Chromosome band height</b></font></div></td>
         <td><div class='help-tip'><p>Height of the chromosome band, which should be greater than 0 and smaller than 0.9.</p></div></td>
         </tr></table>")
                         ),
                         value=0.02, 
                         min=0.01, 
                         max=0.5,
                         step=0.01
                       )
                     )
                   ),
                   conditionalPanel(
                     condition = "input.chr_type == '2'",
                     numericInput(
                       inputId = "heightChr_cyt",
                       label = tags$div(
                         HTML("<table><tr>
         <td><div><font color='red'><h5><i class='fa-solid fa-play'></i><b> Chromosome band height</b></font></div></td>
         <td><div class='help-tip'><p>Height of the chromosome band, which should be greater than 0 and smaller than 0.9.</p></div></td>
         </tr></table>")
                       ),
                       value=0.05, 
                       min=0.01, 
                       max=0.5,
                       step=0.01
                     )
                   ),
                   pickerInput(
                     inputId = "outAxis",
                     label = tags$div(
                       HTML("<table><tr>
         <td><div><font><h5><i class='fa-solid fa-play'></i><b> Display the axis for genomic position?</b></font></div></td>
         <td><div class='help-tip'><p>Display or hide the axis for genomic position?</p></div></td>
         </tr></table>")
                     ),
                     choices = c("Yes" = "1", "No" = "2"),
                     selected = "1"
                   ),
                   conditionalPanel(
                     condition = "input.outAxis == '1'",
                     numericInput(
                       inputId = "outAxis_size",
                       label = tags$div(
                         HTML("<table><tr>
         <td><div><font><h5><i class='fa-solid fa-play'></i><b> Font size of the axis for genomic position</b></font></div></td>
         <td><div class='help-tip'><p>Too large font size may cause problems.</p></div></td>
         </tr></table>")
                       ),
                       value=0.7,
                       min=0.1,
                       max=1.5, 
                       step=0.1
                     )
                   ),
                   pickerInput(
                     inputId = "labelChr",
                     label = tags$div(
                       HTML("<table><tr>
         <td><div><font color='red'><h5><i class='fa-solid fa-play'></i><b> Display chromosome IDs?</b></font></div></td>
         <td><div class='help-tip'><p>Display or hide the chromosome IDs?</p></div></td>
         </tr></table>")
                     ),
                     choices = c("Yes" = "1", "No" = "2")
                   ),
                   conditionalPanel(
                     condition = "input.labelChr == '1'",
                     numericInput(
                       inputId = "labelChr_size",
                       label = tags$div(
                         HTML("<table><tr>
         <td><div><font color='red'><h5><i class='fa-solid fa-play'></i><b> Font size of chromosome IDs</b></font></div></td>
         <td><div class='help-tip'><p>Too large font size may cause problems.</p></div></td>
         </tr></table>")
                       ),
                       value=1.2,
                       min=0.1,
                       max=2, 
                       step=0.1
                     ),
                     sliderTextInput(
                       inputId = "outergap",
                       label = tags$div(
                         HTML("<table><tr>
         <td><div><font color='red'><h5><i class='fa-solid fa-play'></i><b> Distance between chromosome IDs and chromosome axis</b></font></div></td>
         <td><div class='help-tip'><p>Distance between the chromosome IDs and the chromosome axis for genomic positions.</p></div></td>
         </tr></table>")
                       ),
                       choices = 0:100,
                       grid = FALSE
                     )
                   ),
                   textInput(
                     inputId = "gapChr",
                     label = tags$div(
                       HTML('<table><tr>
         <td><div><font><h5><i class="fa-solid fa-play"></i><b> Distances between adjacent sectors</b></font></div></td>
         <td><div class="help-tip"><p>Distances between neighbouring sectors. Numeric vector of arbitrary length is accepted and adjusted automatically to the number of sectors. For example, "1" or "1,2,3,1". The first value corresponds to the distance between the first and the second sector.</p></div></td>
         </tr></table>')
                     ),
                     value = "1"
                   ),
                   numericInput(
                     inputId = "distance_Chr",
                     label = tags$div(
                       HTML('<table><tr>
         <td><div><font color="red"><h5><i class="fa-solid fa-play"></i><b> Distance to the next section (track, label data, or link data)</b></font></div></td>
         <td><div class="help-tip"><p>This parameter can be used to tune the distance between adjacent tracks, or the distance between a track and a label data, or the distance between a track and a link data.</p></div></td>
         </tr></table>')
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
         uiOutput("sortable_track"),
         uiOutput("sortable_label"),
         uiOutput("sortable_link"),
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
                           width = 6,
                           pickerInput(
                             inputId = "addlegend",
                             label = tags$div(
                               HTML('<table><tr>
         <td><div>
         <font><h4><i class="fa-solid fa-play"></i> Add legend?</font>
         </div></td>
         <td><div class="help-tip"><p>
         Display or hide the figure legend?
         </p></div></td>
         </tr></table>')
                             ),
                             choices = c("Yes", "No"),
                             selected = "No"
                           )
                         ),
                         column(
                           width = 6,
                           conditionalPanel(
                             condition = "input.addlegend == 'Yes'",
                             pickerInput(
                               inputId = "legendpos",
                               label = tags$div(
                                 HTML('<table><tr>
         <td><div>
         <font><h4><i class="fa-solid fa-play"></i> Legend position</font>
         </div></td>
         <td><div class="help-tip"><p>
         Place the legend at the right or the bottom of the plot?
         </p></div></td>
         </tr></table>')
                               ),
                               choices = c("Right","Bottom"),
                               selected = "Right"
                             )
                           )
                         )
                       ),
                       hr(),
                       
                       fluidRow(
                         column(width=6,
                                sliderTextInput(
                                  inputId = "plotmultiples",
                                  label = tags$div(
                                    HTML('<font><h4><i class="fa-solid fa-play"></i> Zoom the plot (%)</font>')
                                  ),
                                  choices = c(50:300),
                                  selected = 100
                                )
                         ),
                         column(width=6,
                                pickerInput(
                                  inputId = "trac_index",
                                  label = tags$div(
                                    HTML('<table><tr>
         <td><div>
         <font><h4><i class="fa-solid fa-play"></i> Add track index?</font>
         </div></td>
         <td><div class="help-tip"><p>
         Display or hide the index of each track?
         </p></div></td>
         </tr></table>')
                                  ),
                                  choices = c("Yes", "No"),
                                  selected = "No"
                                )
                         )
                       ),
                       
                       hr(),
                       
                       fluidRow(
                         column(width=12,
                                tags$div(
                                  HTML('<font><h4><i class="fa-solid fa-play"></i> Highlight user-specified genomic regions</font>')
                                )
                         )
                       ),
                       
                       fluidRow(
                         column(
                           width = 6,
                           tags$div(
                             HTML('<table><tr>
         <td><div>
         <font><h5><i class="fa-solid fa-circle"></i> Paste input genomic regions below:</font>
         </div></td>
         <td><div class="help-tip"><p>
         Each row should contain 4 values separated by commas indicating the chr, start genomic position, the end genomic position, and the color. For example, "chr1,1,10000,#FF0000".
         </p></div></td>
         </tr></table>')
                           )
                         ),
                         column(
                           width = 6,
                           tags$div(
                             style = "with:1200px;",
                             HTML('<font><h5><i class="fa-solid fa-circle"></i> Color transparency (%)</font>'),
                             br()
                           )
                         )
                       ),
                       fluidRow(
                         column(
                           width = 6,
                           tags$textarea(
                             id = "hltData",
                             rows = 4,
                             cols = 40,
                             ""
                           )
                         ),
                         column(
                           width = 6,
                           sliderTextInput(
                             inputId = "hltrans",
                             label = NULL,
                             choices = c(1:100),
                             selected = 25
                           )
                         )
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
                       
                       bs4Dash::tooltip(
                         actionBttn(
                           inputId = "updateplot",
                           label = "Update!",
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
             choices = c("English" = 1, "简体中文" = 2),
             selected = 1
           )
         ),
         column(
           width = 8,
         )
       ),
       hr(),
       conditionalPanel(
         condition = "input.helplan == '1'",
         shiny::includeHTML("www/help-English.html")
       ),
       conditionalPanel(
         condition = "input.helplan == '2'",
         shiny::includeHTML("www/help-Chinese.html")
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

# 
# #bottom bar
foot <- bs4DashFooter(
  includeHTML("www/ffooter.html")
)

ui <- bs4DashPage(
  header, sidebar, body , footer = foot ,controlbar = NULL,scrollToTop = TRUE,dark = NULL,title = "Welcome to shinyCircos-V2.0"
)