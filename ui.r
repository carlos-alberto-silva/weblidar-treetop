################################################################################
#               ___________________________________________________            #
#                                                                              #
#                               WEB - LiDAR :                                  #
#               Web Application to processing and visualization                #
#                             LiDAR data                                       #
#               ___________________________________________________            #
#                                                                              #
#                            Carlos Alberto Silva                              #
#                              Forest Engineer                                 #
#                            MSc Forest Resource                               #
#                         Carlos_engflorestal@outlook.com                      #
#                              US Forest Service                               #
#                     Rocky Mountain Reserach Station                          #
################################################################################

################################################################################
# Libraries
options(rgl.useNULL=TRUE) 
#library(shiny)
#library(shinyRGL)
#library(ShinyDash)
#library(shinyIncubator)

#require(devtools)
#require(shiny)
#install_github("wch/shiny-gridster")
library(shinyGridster)

#require(devtools)
#require(shiny)
#install_github("trestletech/ShinyDash")
library(ShinyDash)

################################################################################

################################################################################
# ui.r

headerPanel2<-function (title, windowTitle = title) 
{
  tagList(tags$head(tags$title(windowTitle)), div(style = "margin-left:0px;", h1(title)))
}



shinyUI(navbarPage(collapsable = FALSE, inverse = T, 
  div(style="text-align:left;margin-left: +10px;margin-top: -20px; color:gray", 
    headerPanel2("","Web-LiDAR forest inventory TreeTop application"),
    HTML("<a href='http://www.fs.fed.us/rmrs/'><img style='width: 45px;height: 55px;' 
         src='usfs1.png'/>")),#style="text-align: center;", 
  
    tabPanel(#style="background: transparent;",
      h4(style="text-align:center;margin-top: 10px; color:white",  HTML("Application")),
      includeCSS("www/style.css"),
      tags$style(type="text/css", paste0(".shiny-progress .progress-text {", 
                                         "background-color:yellow; color:black ; ",
                                         "position: absolute; right: 50px;border-radius: 5px;ont-size: 50px;",            
                                         "opacity: 1; height: 50px;font-size: 15px; width: 180px;}")),
      div(class = "container",style="border-radius: 5px;",
        style="border: 2px solid transparent;",style="margin-left:0px;",
        style="margin-top:0px;",style="background: transparent;",
        style="float: left; width: 1800px;height: 1000px;",
        div(class = "container",style="border-radius: 5px;",
          style="border: 1px solid #999;",style="margin-left:0px;",
          style="margin-top:0px;",style="background: #4D704D;",
          style="float: left; width: 300px;height: 1000px;",
          h4(style = "margin-top: 2px; ",style = "text-align:center;",
            style="color:white", HTML("SETTINGS MENU")),
          div(style="margin-top:-15px;color:gray",HTML("<hr />")),
          
            div(style = "margin-top: -15px;color:white",
              radioButtons("Mydata", "Input CHM file",
                list("Custom data" = "CD","Example data" = "ED"),inline = TRUE)),
          conditionalPanel(condition="input.Mydata=='CD'", 
            div(style="margin-left: 2px;margin-top:-15px;color:white;",
            fileInput('chm', '',accept=c('.asc','.tif','.img')))),      
          div(style = "color:white",uiOutput("HTtype")), 
          conditionalPanel(condition="input.HTtypeI=='slidebar'",      
            div(style = "color:white",uiOutput("HTsliderO"))),
          conditionalPanel(condition="input.HTtypeI=='numericbox'", 
            div(style = "color:white",uiOutput("HTboxO"))),      
          div(class="row-fluid", div(class="span6",
            style = "margin-left: 2px;text-align:center; width:120px; color:white",
            selectInput("ws", "FWS",
              choices = c("3x3","5x5","7x7","9x9","11x11","13x13"),selected="3x3"),
            div(class="span6",style = "margin-left: 126px;",
              style = "margin-top:-76px; width:120px;", 
              selectInput("Pallet", "CHM color",
                choices = c("Blues","BuGn","BuPu","GnBu","Greens","Greys",
                            "Oranges","OrRd","PuBu","PuBuGn","PuRd","Purples",
                            "RdPu","Reds","YlGn","YlGnBu",
                            "YlOrBr","YlOrRd","BlGrRed"),selected="Greys")))),
          div(style ="margin-left: 2px;color:white",
            radioButtons("radiustype", "Tree Crown Width (TCW) Estimation",
              list("Fixed Width" = "FR","Variable Width" = "VR"),inline = TRUE)),
       
          conditionalPanel(condition="input.radiustype=='VR'", 
             div(style ="margin-left: 0px;color:white",
               radioButtons("equation", " Equation: TCW = f(ht); ht= Height (m)",
                 list("Deciduous" = "DC","Pines" = "PI","Combined"="CB",
                      "Use custom polynomial"="YR"),inline = TRUE)),                    
            conditionalPanel(condition="input.equation=='YR'", 
              div(style = "margin-left: 2px;margin-top: -10px;color:white",
                HTML("|---Inter-----------ht------------ht^2---------ht^3-|")),                                
              div(class="row-fluid", 
                div(class="span6",style = "margin-left: 0px;color:white",
                  numericInput("Ang", "", "")), 
               div(class="span6",style = "margin-left: 71px;margin-top: -67px;color:white",
                 numericInput("ht1", "", "")),
               div(class="span6",style = "margin-left: 141px;margin-top: -67px;color:white",
                 numericInput("ht2", "", "")),
               div(class="span6",style = "margin-left: 211px;margin-top: -67px;color:white",
                 numericInput("ht3", "", ""))))), 
          conditionalPanel(condition="input.radiustype=='FR'", 
            div(style="margin-left: 2px;",numericInput("frv", "", 5))),                 
          div(style = "margin-left: 2px;color:white",
            checkboxInput("filter", "Smoothing CHM", value=F)),      
          conditionalPanel(condition="input.filter==true", 
            div(style = "margin-left:0px; width:100px;margin-top:0px;color:white",
              radioButtons("filtertype", "",
                list("Mean" = "Mean","Median" = "Median","Gaussian" = "Gaussian"))),
            conditionalPanel(condition="input.filtertype=='Mean'",
              div(style = "margin-left: 120px;text-align:center; width:130px;margin-top:-80px;color:white",
                selectInput("wsf", "Filter Windows Size",
                  choices = c("3x3","5x5","7x7"),selected="3x3"))),
            conditionalPanel(condition="input.filtertype=='Median'",
              div(style = "margin-left: 120px;text-align:center; width:130px;margin-top:-80px;color:white",
                selectInput("wsf", "Filter Windows Size",choices = c("3x3","5x5","7x7"),selected="3x3"))),
            conditionalPanel(condition="input.filtertype=='Gaussian'",
              div(style = "color:white;margin-top:-15px;margin-left: 2px;",
              sliderInput("Sigma","Gaussian Sigma ",0.1,3,1.5,step=0.01,format="#.##")))),          
          div(style = "color:white",
              radioButtons("plotCHM2d", "", list("CHM 2d" = "plotchm2d",
                                                   "Lorenz curve" = "plotlorenzcurve"),inline = TRUE),
            radioButtons("plotProfile", "", list("CHM profile" = "plotCHMProfile",
              "Ripley's K and L" = "plotRipley"),inline = TRUE),
              radioButtons("plot3Dradio", "", list("CHM 3d" = "plotCHM3D",
                                                   "Trees 3d" = "plot3Dtrees"),inline = TRUE)),
          div(style="margin-top:-15px;color:gray",HTML("<hr />")),             
          div(style="margin-left: 2px;margin-top:-15px;",
            actionButton("action_button","Run"),uiOutput("TreelistR")),
          div(style="margin-top:-15px;color:gray",HTML("<hr />")),      
          HTML("<img style='width: 240px;height:60px;margin-left: 3.5px;margin-top: -15px;border-radius: 5px;border: 1px solid #999;' src='RMRS_LOGO_SAI.jpg'/>"),
          div(style = "margin-top: 0px; width: 200px;height: 179px;", style="color:white", HTML("SILVA, C.A; HUDAK, A.T.; CROOKSTON, N. L.; VIERLING, L. A.; KLAUBERG C.A. and VALBUENA, R. (2019)"))
        ),
            
        div(class = "container",style="border-radius: 5px;",
          style="border: 2px solid transparent;",
          style="margin-right:0px;",style="margin-top:0px;",
          style="background: transparent;",
          style="float: right; width: 1370px;height: 1000px;",
          div(class = "container",style="border-radius: 5px;",
            style="border: 2px solid white;",
            style="margin-left:-105px;",style="margin-top:0px;",
            style="background: white;",
            style="float: left; width: 350px;height: 400px;",
              h4(style = "margin-top: -0.5px; width: 250px;",
                 style = "margin-bottom: 10px; width: 250px;",
                 style="text-align: center;",style="color:#003300", 
                 HTML("Summary of LiDAR metrics")),
            div(style = "margin-top: 0px;margin-left: 30px",tableOutput("summary")),
            uiOutput("downloadShpR"),
            uiOutput("downloadShpRXY")),                                                                                
          
          div(class = "container",style="border-radius: 5px;",
            style="border: 2px solid white;",style="margin-left:5px;",
            style="margin-top:0px;",style="background: white;",
            style="float: left; width:1000px;height: 400px;",
            h4(style = "margin-top: 0px; width: 1000px; ",
              style="text-align: center;",style="color:#003300", 
              HTML("Canopy Height Profile and Spatial Pattern")),
            div(style = "margin-left: 20px;",plotOutput("hist")),
            uiOutput("Profile")),     
          
          div(class = "container",style="border-radius: 5px;",
            style="border: 2px solid white;",style="margin-left:-105px;",
            style="margin-top:8px;",style="background: white;",
            style="float: left; width: 672px;height: 595px;",
              h4(style = "margin-top: 0px; width: 673px; ",
                style="text-align: center;",style="color:#003300", 
                HTML("Individual Trees Detected on CHM and Lorenz curve")),
              div(style = "margin-top: 0px;",plotOutput("CHMplot2D")),
            uiOutput("outCHMplot2D")),
                       
          
          div(class = "container",style="border-radius: 5px;",
            style="border: 2px solid white;margin-left:5px;",
            style="margin-right:0px;",
            style="margin-top:8px;",style="background:white;",
            style="float: left; width: 673px;height: 595px;",
            h4(style = "margin-top: 0px; width: 673px; ",
              style="text-align: center;",style="color:#003300", 
              HTML("Canopy Height Model and 3D trees")),
            rglwidget::rglwidgetOutput("PLOT3D",width = "650px", height = "550px"))
        )
      ) 
    ),         
    ################################################################################

    tabPanel(h4(style="text-align:center;margin-top: 10px; color:white", 
      HTML("About")),# style="background: transparent;",             
        gridster(tile.width = 250, tile.height = 250,              
          gridsterItem(col = 2, row = 2, size.x = 3, size.y = 2,style="background: white;width:845px;height: 560px",
                               
                               div(style="color:forestgreen",h4("Acknowledgement:")),
                               div(align="justify",style="width: 810px;color:gray",h5("Funding to support Carlos Silva's development of Web-LiDAR and its underlying functions was provided through Ph.D scholarship from National Counsel of Technological and Scientific Development - CNPq via the Brazilian Science Without Borders program and a grant (RC-2243) from the Department of Defense Strategic Environmental Research and Development Program: Patterns and processes: monitoring and understanding plant diversity in frequently burned longleaf pine landscapes. J. O'Brien, PI; R. Mitchell, A. Hudak, L. Dyer, Co-PIs.")),
                               div(align="justify",style="width: 810px;color:gray",h5("The LiDAR data provided as an example dataset is from a longleaf pine forest at Eglin AFB. It's collection was funded by a grant (11-2-1-11) from the Joint Fire Science Program: Data set for fuels, fire behavior, smoke, and fire effects model development and evaluation-the RxCADRE project. R. Ottmar, PI; multiple Co-Is.")),
                               div(style="color:forestgreen",h4("Objective:")),
                               div(align="justify",style="width: 810px;color:gray",h5("Web-LiDAR was developed to support lidar-based forest inventory and management at Eglin Air Force Base (AFB), Florida, USA. However, it has general applicability to other forests in other ecosystems, and we encourage users to test it broadly. (Contact us by email: Carlos_engflorestal@outlook.com)")),
                               div(style = "margin-top: -15px;", HTML("<hr />")),
             h4(style = "margin-top: -20px;",style = "margin-top: 5px; width: 810px;",
              style = "margin-bottom: 10px;",style="text-align: center;",
              style="color:forestgreen", 
              HTML('Tutorial - Web-LiDAR Forest Inventory: TreeTop Application')),
            div(style = "margin-top: -10px;",style="text-align: center;",
              HTML("<a href='Web-LiDAR_tutorial_CAS_treeTop.pdf'a/><img style='width: 300px;height:200px' src='tutorialPDF.jpg'/>"),
              HTML("<a href='http://youtu.be/QMr8fbFpETs'a/><img style='width: 300px;height:200px' src='tutorialYoutube.jpg'/>"))
        )),            
 gridster(tile.width = 250, tile.height = 250,
                  
                  
                  gridsterItem(col = 1, row = 1, size.x = 1, size.y = 1,style="background: white;",style="background: white;width:280px;height: 280px",
                               h4(style = "margin-top: -5px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align:center;",style="color:#003300", HTML("Carlos Alberto Silva")),
                               div(style="text-align: center;",HTML("<a href='http://www.fs.fed.us/research/'a/><img style='border-radius: 5px; width: 220px; height:220px' src='carlos.jpg'/>"))),
                  gridsterItem(col = 2, row = 1, size.x = 1, size.y = 1,style="background: white;width:280px;height: 280px",
                               h4(style = "margin-top: -5px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align: center;",style="color:#003300", HTML("Andrew T. Hudak")),
                               div(style="text-align: center;",HTML("<a href='http://www.fs.fed.us/research/people/profile.php?alias=ahudak'a/><img style='border-radius: 5px;width: 220px; height:220px' src='ahudak.jpg'/>"))),
                  gridsterItem(col = 3, row = 1, size.x = 1, size.y = 1,style="background: white;width:280px;height: 280px",
                               h4(style = "margin-top: -5px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align: center;",style="color:#003300", HTML("Nicholas L. Crookston")),
                               div(style="text-align: center;",HTML("<a href='http://www.fs.fed.us/research/people/profile.php?alias=ncrookston'a/><img style='border-radius: 5px; width: 220px; height:220px' src='nick.jpg'/>"))),
                  gridsterItem(col = 4, row = 1, size.x = 1, size.y = 1,style="background: white;width:280px;height: 280px",
                               h4(style = "margin-top: -5px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align: center;",style="color:#003300", HTML("Lee Vierling")),
                               div(style="text-align: center;",HTML("<a href='http://www.uidaho.edu/cnr/frfs/leevierling'a/><img style='border-radius: 5px; width: 220px; height:220px' src='leevierling.jpg'/>"))),
                  gridsterItem(col = 1, row = 2, size.x = 1, size.y = 1,style="background: white;width:280px;height: 280px",
                               h4(style = "margin-top: -5px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align: center;",style="color:#003300", HTML("Rocky Mountain Research Station - RMRS")),
                               div(style="text-align: center;",HTML("<a href='http://www.fs.fed.us/rmrs/'a/><img style='width: 200px; height:170px;margin-top: 5px;' src='rmrs.gif'/>"))),
                  gridsterItem(col = 1, row = 3, size.x = 1, size.y = 1,style="background: white;width:280px;height: 280px",
                               h4(style = "margin-top: -5px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align: center;",style="color:#003300", HTML("University of Idaho")),
                               div(style="text-align: center;",HTML("<a href='http://www.uidaho.edu/'a/><img style='width: 200px;height:200px;margin-top: 5px;'' src='University_of_Idaho.png'/>"))),
                  gridsterItem(col = 5, row = 1, size.x = 1, size.y = 1,style="background: white;width:280px;height: 280px",
                               h4(style = "margin-top: -5px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align: center;",style="color:#003300", HTML("Carine Klaubert")),
                              div(style="text-align: center;",HTML("<a href='https://www.researchgate.net/profile/Carine_Klauberg/'a/><img style='width: 220px;height:220px' src='carine_Klauberg.jpg'/>"))),
                  gridsterItem(col = 6, row = 1, size.x = 1, size.y = 1,style="background: white;width:280px;height: 280px",
                               h4(style = "margin-top: -5px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align: center;",style="color:#003300", HTML("Rubén Valbuena")),
                               div(style="text-align: center;",HTML("<a href='https://www.bangor.ac.uk/natural-sciences/staff/ruben-valbuena/en#publications/'a/><img style='width: 220px;height:220px' src='Valbuena.gif'/>"))),
                  gridsterItem(col = 6, row = 2, size.x = 1, size.y = 1,style="background: white;width:280px;height: 280px",
                              h4(style = "margin-top: -5px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align: center;",style="color:#003300", HTML("Bangor University, UK")),
                              div(style="text-align: center;",HTML("<a href='https://www.bangor.ac.uk/natural-sciences/'a/><img style='width: 220px;height:220px' src='Bangor.gif'/>"))),
                  gridsterItem(col = 6, row = 3, size.x = 1, size.y = 1,style="background: white;width:280px;height: 280px",
                              h4(style = "margin-top: -5px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align: center;",style="color:#003300", HTML("Universidade Federal de São João del Rei")),
                              div(style="text-align: center;",HTML("<a href='https://www.ufsj.edu.br/csl/'a/><img style='width: 220px;height:220px' src='UFSJ.png'/>"))),
                  gridsterItem(col = 5, row = 2, size.x = 1, size.y = 1,style="background: white;width:280px;height: 280px",
                               h4(style = "margin-top: -5px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align: center;",style="color:#003300", HTML("USDA Forest Service")),
                               div(style="text-align: center;",HTML("<a href='http://www.fs.fed.us/research/'a/><img style='width: 200px;height:200px;margin-top: 5px;'' src='usfs2.jpg'/>"))),
                  gridsterItem(col = 5, row = 3, size.x = 1, size.y = 1,style="background: white;width:280px;height: 280px",
                               h4(style = "margin-top: -5px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align: center;",style="color:#003300", HTML("Web-LiDAR forest inventory")),
                               div(style="text-align: center;",HTML("<a href='http://forest.moscowfsl.wsu.edu/'a/><img style='width: 250px;height:220px' src='WebLiDARicon.png'/>"))))),


    div(style="margin-right",title=h4(style="margin-right;margin-top: 10px;color:white", 
      textOutput("pageviews")))
  )
)
################################################################################
