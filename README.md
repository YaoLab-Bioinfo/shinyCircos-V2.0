#	shinyCircos-V2.0：an R/Shiny application for interactive creation of Circos plot
---
shinyCircos-V2.0 is the updated version of shinyCircos.
In shinyCircos-V2.0, we developed several advanced features, designed brand-new user interface, and fixed bugs detected in shinyCircos.

#	Welcome to join the telegram group

https://t.me/+NadFeZazBBc2Y2U1  

#	Use shinyCircos-V2.0 online

shinyCircos-V2.0 is deployed at <a href="https://venyao.xyz/shinyCircos/" target="_blank">https://venyao.xyz/shinyCircos/</a>, and <a href="https://asiawang.shinyapps.io/shinyCircos/" target="_blank">https://asiawang.shinyapps.io/shinyCircos/</a> for online use.  

#	Help manual and video tutorials

- Help manual
https://yaolab-bioinfo.gitbook.io/shinycircos/

- Video tutorials
1. Example Circos plot 1
	+ https://youtu.be/H0mrhO2vAX8
	+ https://www.163.com/v/video/VNNIPBAQI.html
	+ https://venyao.xyz/example_circos_plot_1.mp4
2. Example Circos plot 2
	+ https://youtu.be/4Ot8C2XfVy0
	+ https://www.163.com/v/video/VNNIRQH2K.html
	+ https://venyao.xyz/example_circos_plot_2.mp4


#	Launch shinyCircos-V2.0 directly from R and GitHub

User can choose to run shinyCircos-V2.0 installed locally for a more preferable experience.

**Step 1: Install R and RStudio**

Before running the app you will need to have R and RStudio installed (tested with R 3.3.3 and RStudio 1.0.143).  
Please check CRAN (<a href="https://cran.r-project.org/" target="_blank">https://cran.r-project.org/</a>) for the installation of R.  
Please check <a href="https://www.rstudio.com/" target="_blank">https://www.rstudio.com/</a> for the installation of RStudio.  

**Step 2: Install the R Shiny package and other packages required by shinyCircos-V2.0**

Start an R session using RStudio and run these lines:  
```
install.packages("shiny")
install.packages("circlize")
install.packages("bs4Dash")
install.packages("DT")
install.packages("RColorBrewer")
install.packages("shinyWidgets")
install.packages("data.table")
install.packages("shinyBS")
install.packages("sortable")
install.packages("shinyjqui")
install.packages("shinycssloaders")
install.packages("colourpicker")
install.packages("gridBase")
install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")
```

**Step 3: Start the app**  

Start an R session using RStudio and run these lines:  
```
shiny::runGitHub("shinyCircos-V2.0", "YaoLab-Bioinfo")  
```
This command will download the code of shinyCircos from GitHub to a temporary directory of your computer and then launch the shinyCircos app in the web browser. Once the web browser was closed, the downloaded code of shinyCircos would be deleted from your computer. Next time when you run this command in RStudio, it will download the source code of shinyCircos from GitHub to a temporary directory again. This process is frustrating since it takes some time to download the code of shinyCircos from GitHub.  

Users are suggested to download the source code of shinyCircos from GitHub to a fixed directory of your computer, such as 'E:\apps' on Windows. A zip file named "shinyCircos-V2.0-master.zip" will be downloaded to your computer. Move this file to "E:\apps" and unzip this file. Then a directory named "shinyCircos-V2.0-master" will be generated in "E:\apps". The scripts "server.R" and "ui.R" can be found in "E:\apps\shinyCircos-V2.0-master".

Then you can start the shinyCircos app by running these lines in RStudio.  
```
library(shiny)
runApp("E:/apps/shinyCircos-V2.0-master", launch.browser = TRUE)
```

