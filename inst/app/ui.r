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
#options(rgl.useNULL=TRUE)
options(shiny.deprecation.messages=FALSE)

################################################################################

appDir <- file.path(path.package("treetop", quiet=TRUE),"app")

################################################################################
# ui.r

headerPanel2<-function (title, windowTitle = title)
{
  tagList(tags$head(tags$title(windowTitle),  tags$style(HTML("
    .shiny-output-error-validation {
    color: red;
    }
    "))), div(style = "margin-left:0px;", h1(title)))
}

checkjs <- 'function checkFileName(fieldObj) {
    var FileName  = fieldObj.value;
    var FileBase = FileName.split(/[\\\\/]/).pop();
    if (! FileBase.startsWith("hello")) {
        fieldObj.value = "";
        alert("File does not start with hello");
        return false;
    }
    return true;
}'

attrib_replace <- function(x, cond, ...) {
  if (all(names(cond) %in% names(x)) && identical(cond, x[names(cond)])) x <- c(x, list(...))
  if ("attribs" %in% names(x)) x$attribs <- attrib_replace(x$attribs, cond = cond, ...)
  if ("children" %in% names(x)) x$children <- lapply(x$children, function(ch) attrib_replace(ch, cond = cond, ...))
  x
}



shinyUI(navbarPage(collapsible = FALSE, inverse = T,
                   div(style="text-align:left;margin-left: +10px;margin-top: -20px; color:#444",
                       headerPanel2("","Web-LiDAR forest inventory TreeTop application"),
                       HTML("<a href='https://github.com/carlos-alberto-silva/weblidar-treetop'><img style='width: 80px;height: 80px;'
         src='serdp1.png'/>"),
                       conditionalPanel(
                         condition="($('html').hasClass('shiny-busy'))",
                         div(style="position: absolute; top: 8px; right: 16px; font-size: 18px",img(src="busy3.gif",width="60",height="60"))
                       )),#style="text-align: center;",


                   tabPanel(#style="background: transparent;",
                     h4(style="text-align:center;margin-top: 10px; color:white;background: transparent;",  HTML("Application")),
                     includeCSS(file.path(appDir,"/style.css")),
                     tags$style(type="text/css", paste0(".shiny-progress.progress-text {",
                                                        "background-color:yellow; color:black; ",
                                                        "position: absolute; right: 50px;border-radius: 5px;font-size: 50px;",
                                                        "opacity: 1; height: 50px;font-size: 15px; width: 180px;}")),
                     tags$head(tags$style(".shiny-notification {position: fixed; top: 0px ;left: 728px;font-size: 20px;
                                          width: 700px;height:100px;background-color:#FF8C00;color:white;border-radius: 12px;line-height: 1")),

                     div(class = "container",style="border-radius: 5px;",
                         style="border: 2px solid transparent;",style="margin-left:0px;",
                         style="margin-top:0px;",style="background: transparent;",
                         style="float: left; width: 1800px;height: 1000px;",
                         div(class = "container",style="border-radius: 5px;",
                             style="border: 1px solid #999;",style="margin-left:0px;",
                             style="margin-top:0px;",style="background: #808080;",
                             style="float: left; width: 300px;height: 1030px;",
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
                                                        style = "margin-left: 2px;text-align:center; width:120px; color:white;margin-top: -15px",
                                                        selectInput("fws", "FWS",
                                                                    choices = c("3x3","5x5","7x7","9x9","11x11","13x13","15x15","17x17"),selected="5x5"),
                                                        div(class="span6",style = "margin-left: 126px;",
                                                            style = "margin-top:-79px; width:120px;",
                                                            selectInput("Pallet", "CHM color",
                                                                        choices = c("Blues","BuGn","BuPu","GnBu","Greens","Greys",
                                                                                    "Oranges","OrRd","PuBu","PuBuGn","PuRd","Purples",
                                                                                    "RdPu","Reds","YlGn","YlGnBu",
                                                                                    "YlOrBr","YlOrRd","BlGrRed","Viridis"),selected="Viridis")),


                                                        div(style = "color:white",uiOutput("tiles"))

                             )),
                             div(class="row-fluid", div(class="span6",
                                                        style = "margin-left: 1px;margin-top:-10px;text-align:center; width:80px; color:white",
                                                        selectInput("CrownColor", "Crown Color",
                                                                    choices = c("white","green","blue","red","black","yellow",
                                                                                "orange","brown"),selected="black"),
                                                        div(class="span6",style = "margin-left: 82px;",
                                                            style = "margin-top:-99px; width:85px;",
                                                            selectInput("TopColor", "Tree Top Color",
                                                                        choices = c("white","gray","green","forestgreen","blue","red","black","yellow",
                                                                                    "orange","brown"),selected="white")),
                                                        div(class="span6",style = "margin-left: 170px;",
                                                            style = "margin-top:-98px; width:85px;",
                                                            selectInput("profColor", "CHM Profile Color",
                                                                        choices = c("white","green","forestgreen","gray","blue","red","black","yellow",
                                                                                    "orange","brown"),selected="green")))),

                             div(style ="margin-left: 2px;margin-top:-10px;color:white",
                                 radioButtons("radiustype", "Tree Crowns Delineation",
                                              list("CrownSeg" = "FR","CrownAllometry" = "VR"),inline = TRUE)),

                             conditionalPanel(condition="input.radiustype=='VR'",
                                              div(style ="margin-left: 0px;color:white",
                                                  radioButtons("equation", " Equation: TCW = f(ht); ht= Height (m)",
                                                               list("Deciduous" = "DC","Pines" = "PI","Combined"="CB",
                                                                    "Use custom polynomial"="YR"),inline = TRUE)),
                                              conditionalPanel(condition="input.equation=='YR'",
                                                               div(style = "margin-left: 2px;margin-top: -10px;color:white",
                                                                   HTML("|---Inter-----------ht------------ht^2--------ht^3-|")),
                                                               div(class="row-fluid",
                                                                   div(class="span6",style = "margin-left: 0px;color:white",
                                                                       numericInput("Ang", "", "")),
                                                                   div(class="span6",style = "margin-left: 71px;margin-top: -68px;color:white",
                                                                       numericInput("ht1", "", "")),
                                                                   div(class="span6",style = "margin-left: 141px;margin-top: -68px;color:white",
                                                                       numericInput("ht2", "", "")),
                                                                   div(class="span6",style = "margin-left: 211px;margin-top: -68.2px;color:white",
                                                                       numericInput("ht3", "", ""))))),
                             conditionalPanel(condition="input.radiustype=='FR'",
                                              div(style="margin-left: 2px;color:white;margin-top: -10px;",
                                                  sliderInput("maxcrown", "maxcrown",0,1,0.6,step=0.01,sep="#.##")),
                                              div(style="margin-left: 2px;color:white;margin-top: -15px;",
                                                  sliderInput("exclusion","exclusion",0,1,0.3,step=0.01,sep="#.##"))),

                             div(style = "margin-left: 2px;margin-top: -10px;color:white",
                                 checkboxInput("filter", "Smoothing CHM", value=TRUE)),
                             conditionalPanel(condition="input.filter==true",
                                              div(style = "margin-left:0px; width:100px;margin-top:-35px;color:white",
                                                  radioButtons("filtertype", "",
                                                               list("Mean" = "Mean","Median" = "Median","Gaussian" = "Gaussian"))),
                                              conditionalPanel(condition="input.filtertype=='Mean'",
                                                               div(style = "margin-left: 125px;text-align:center; width:130px;margin-top:-105px;color:white",
                                                                   selectInput("sws", "Filter Windows Size",
                                                                               choices = c("3x3","5x5","7x7","9x9","11x11","13x13","15x15","17x17"),selected="3x3"))),
                                              conditionalPanel(condition="input.filtertype=='Median'",
                                                               div(style = "margin-left: 125px;text-align:center; width:130px;margin-top:-105px;color:white",
                                                                   selectInput("sws", "Filter Windows Size",choices = c("3x3","5x5","7x7","9x9","11x11","13x13","15x15","17x17"),selected="3x3"))),
                                              conditionalPanel(condition="input.filtertype=='Gaussian'",
                                                               div(style = "color:white;margin-top:-15px;margin-left: 2px;",
                                                                   sliderInput("Sigma","Gaussian Sigma ",0.1,3,1.5,step=0.01,format="#.##")))),

                             div(style = "color:white;margin-top:-20px",
                                 radioButtons("plotCHM2d", "", list("CHM 2d" = "plotchm2d",
                                                                    "Lorenz curve" = "plotlorenzcurve"),inline = TRUE),
                                 div(style = "color:white;margin-top:-30px",
                                     radioButtons("plotProfile", "", list("CHM profile" = "plotCHMProfile",
                                                                          "Ripley's K and L" = "plotRipley"),inline = TRUE)),

                                 div(style = "color:white;margin-top:-30px",
                                     radioButtons("plot3Dradio", "", list("CHM 3d" = "plotCHM3D",
                                                                          "Trees 3d" = "plot3Dtrees"),inline = TRUE)),

                                 conditionalPanel(condition="input.plot3Dradio=='plot3Dtrees'",
                                                  div(style = "color:white;margin-top:-10px;width:130px",
                                                      selectInput("plotShape", "Shape",
                                                                  choices = c("cone","ellipsoid","halfellipsoid","paraboloid","cylinder"),selected="halfellipsoid"),
                                                      div(style = "color:white;margin-top:-78px;width:130px;margin-left: 145px",
                                                          selectInput("plotSurface", "Surface",
                                                                      choices = c("solid","mesh","lines"),selected="lines")),


                                                      #radioButtons("plotShape", "", list("Cone" = "plotCone",
                                                      #                                      "Ellipsoid" = "plotEllipsoid"),inline = TRUE)
                                                  ))

                             ),



                             div(style="margin-top:-15px;color:gray;",HTML("<hr />")),
                               div(style="margin-left: 2px;margin-top:-15px;",
                                 actionButton("action_button","Run"),
                                 div(style="margin-left:55px;margin-top: -34px;width:200px",
                                     actionButton('refresh', 'Refresh app'))
                             ),
                             div(style="margin-top:-15px;color:gray",HTML("<hr />")),
                             conditionalPanel(condition="input.filtertype!='Gaussian' & input.plot3Dradio!='plot3Dtrees'",#input.radiustype!='FR'",# & input.plot3Dradio!='plot3Dtrees'",
                                              #conditionalPanel(condition="input.radiustype=='VR'",
                                              HTML("<a href='https://www.serdp-estcp.org/'a/><img style='width: 240px;height:60px;margin-left: 3.5px;margin-top: -15px;border-radius: 5px;border: 1px solid #999;' src='serdp.jpg'/>"))#,
                             #div(style = "margin-top: 0px; width: 200px;height: 179px;", style="color:white", HTML("SILVA, C.A; HUDAK, A.T.; CROOKSTON, N. L.; VIERLING, L. A.; KLAUBERG, C.; and VALBUENA, R. (2019)"))
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
                                 h4(style = "margin-top: -0.5px; width: 300px;",
                                    style = "margin-bottom: 10px;",
                                    style="text-align: center;",style="color:#003300",
                                    HTML("Summary of crown heights or CHM")),
                                 div(style = "margin-top: 0px;margin-left: 30px",tableOutput("summary"),tableOutput("summary2"))),


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
                                 style="float: left; width: 672px;height: 620px;",
                                 h4(style = "margin-top: 0px; width: 673px; ",
                                    style="text-align: center;",style="color:#003300",
                                    HTML("Individual Trees Detected on CHM and Lorenz curve")),
                                 div(style = "margin-top: -10px;",plotOutput("CHMplot2D")),
                                 uiOutput("outCHMplot2D")),


                             div(class = "container",style="border-radius: 5px;",
                                 style="border: 2px solid white;margin-left:5px;",
                                 style="margin-right:0px;",
                                 style="margin-top:8px;",style="background:white;",
                                 style="float: left; width: 673px;height: 620px;",
                                 h4(style = "margin-top: 0px; width: 673px; ",
                                    style="text-align: center;",style="color:#003300",
                                    HTML("Canopy Height Model and 3D trees")),
                                 rglwidget::rglwidgetOutput("PLOT3D",width = "650px", height = "550px"),
                                 uiOutput("downloadShpR"),
                                 uiOutput("downloadShpRXY"),
                                 uiOutput("TreelistR"))
                         )
                     )
                   ),
                   ################################################################################

                   tabPanel(h4(style="text-align:center;margin-top: 10px; color:white",
                               HTML("About")),# style="background: transparent;",

                            div(class = "container",style="border-radius: 5px;",
                                style="border: 2px solid transparent;",style="margin-left:0px;",
                                style="margin-top:0px;",style="background: transparent;",
                                style="float: left; width: 1600px;height: 1000px;",


                                div(class = "container",style="border-radius: 5px;",
                                    style="border: 2px solid transparent;",style="margin-left:0px;",
                                    style="margin-top:5px;",style="background: white;",
                                    style="float: left; width: 1525px;height: 800px;",
                                    div(style="text-align: center;",HTML("<a href='https://github.com/carlos-alberto-silva/weblidar-treetop/'a/><img style='border-radius: 5px; width: 1510px; height:790px' src='legend.png'/>"))),



                                div(class = "container",style="border-radius: 5px;",
                                    style="border: 2px solid transparent;",style="margin-left:0px;",
                                    style="margin-top:5px;",style="background: white;",
                                    style="float: left; width: 250px;height: 260px;",
                                    h4(style = "margin-top: -3px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align:center;",style="color:#003300", HTML("Carlos Alberto Silva")),
                                    div(style="text-align: center;",HTML("<a href='https://www.researchgate.net/profile/Carlos_Silva37/'a/><img style='border-radius: 5px; width: 220px; height:220px' src='carlos.jpg'/>"))),

                                div(class = "container",style="border-radius: 5px;",
                                    style="border: 2px solid transparent;",style="margin-left:5px;",
                                    style="margin-top:5px;",style="background: white;",
                                    style="float: left; width: 250px;height: 260px;",
                                    h4(style = "margin-top: -3px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align:center;",style="color:#003300", HTML("Andrew T. Hudak")),
                                    div(style="text-align: center;",HTML("<a href='https://www.fs.fed.us/research/people/profile.php?alias=ahudak/'a/><img style='border-radius: 5px; width: 220px; height:220px' src='ahudak.jpg'/>"))),

                                div(class = "container",style="border-radius: 5px;",
                                    style="border: 2px solid transparent;",style="margin-left:5px;",
                                    style="margin-top:5px;",style="background: white;",
                                    style="float: left; width: 250px;height: 260px;",
                                    h4(style = "margin-top: -3px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align:center;",style="color:#003300", HTML("Lee A. Vierling")),
                                    div(style="text-align: center;",HTML("<a href='https://www.uidaho.edu/cnr/faculty/vierling-l/'a/><img style='border-radius: 5px; width: 220px; height:220px' src='leevierling.jpg'/>"))),

                                div(class = "container",style="border-radius: 5px;",
                                    style="border: 2px solid transparent;",style="margin-left:5px;",
                                    style="margin-top:5px;",style="background: white;",
                                    style="float: left; width: 250px;height: 260px;",
                                    h4(style = "margin-top: -3px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align:center;",style="color:#003300", HTML("Ruben Valbuena")),
                                    div(style="text-align: center;",HTML("<a href='https://www.bangor.ac.uk/natural-sciences/staff/ruben-valbuena/en/'a/><img style='border-radius: 5px; width: 220px; height:220px' src='valbuena.jpg'/>"))),

                                div(class = "container",style="border-radius: 5px;",
                                    style="border: 2px solid transparent;",style="margin-left:5px;",
                                    style="margin-top:5px;",style="background: white;",
                                    style="float: left; width: 250px;height: 260px;",
                                    h4(style = "margin-top: -3px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align:center;",style="color:#003300", HTML("Adrian Cardil")),
                                    div(style="text-align: center;",HTML("<a href='https://scholar.google.com/citations?user=KvIOmQkAAAAJ&hl=es/'a/><img style='border-radius: 5px; width: 220px; height:220px' src='adrian.jpg'/>"))),

                                div(class = "container",style="border-radius: 5px;",
                                    style="border: 2px solid transparent;",style="margin-left:5px;",
                                    style="margin-top:5px;",style="background: white;",
                                    style="float: left; width: 250px;height: 260px;",
                                    h4(style = "margin-top: -3px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align:center;",style="color:#003300", HTML("Midhun Mohan")),
                                    div(style="text-align: center;",HTML("<a href='https://scholar.google.com/citations?user=LQITQ3YAAAAJ&hl=en/'a/><img style='border-radius: 5px; width: 220px; height:220px' src='mikey.jpg'/>"))),


                                div(class = "container",style="border-radius: 5px;",
                                    style="border: 2px solid transparent;",style="margin-left:0px;",
                                    style="margin-top:5px;",style="background: white;",
                                    style="float: left; width: 250px;height: 260px;",
                                    h4(style = "margin-top: -3px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align:center;",style="color:#003300", HTML("Danilo R. A. de Almeida")),
                                    div(style="text-align: center;",HTML("<a href='http://esalqlastrop.com.br/capa.asp?e=56/'a/><img style='border-radius: 5px; width: 220px; height:220px' src='danilo.jpg'/>"))),

                                div(class = "container",style="border-radius: 5px;",
                                    style="border: 2px solid transparent;",style="margin-left:5px;",
                                    style="margin-top:5px;",style="background: white;",
                                    style="float: left; width: 250px;height: 260px;",
                                    h4(style = "margin-top: -3px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align:center;",style="color:#003300", HTML("Eben N. Broadbent")),
                                    div(style="text-align: center;",HTML("<a href='http://sfrc.ufl.edu/people/faculty/broadbent/'a/><img style='border-radius: 5px; width: 220px; height:220px' src='eben.jpg'/>"))),

                                div(class = "container",style="border-radius: 5px;",
                                    style="border: 2px solid transparent;",style="margin-left:5px;",
                                    style="margin-top:5px;",style="background: white;",
                                    style="float: left; width: 250px;height: 260px;",
                                    h4(style = "margin-top: -3px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align:center;",style="color:#003300", HTML("Angelica M. A. Zambrano")),
                                    div(style="text-align: center;",HTML("<a href='http://uftcd.org/people/core-faculty-staff/angelica-m-almeyda-zambrano/'a/><img style='border-radius: 5px; width: 220px; height:220px' src='Almeyda.jpg'/>"))),

                                div(class = "container",style="border-radius: 5px;",
                                    style="border: 2px solid transparent;",style="margin-left:5px;",
                                    style="margin-top:5px;",style="background: white;",
                                    style="float: left; width: 250px;height: 260px;",
                                    h4(style = "margin-top: -3px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align:center;",style="color:#003300", HTML("Ben Wilkinson")),
                                    div(style="text-align: center;",HTML("<a href='http://sfrc.ufl.edu/people/faculty/wilkinson/'a/><img style='border-radius: 5px; width: 220px; height:220px' src='ben.jpg'/>"))),

                                div(class = "container",style="border-radius: 5px;",
                                    style="border: 2px solid transparent;",style="margin-left:5px;",
                                    style="margin-top:5px;",style="background: white;",
                                    style="float: left; width: 250px;height: 260px;",
                                    h4(style = "margin-top: -3px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align:center;",style="color:#003300", HTML("Ajay Sharma")),
                                    div(style="text-align: center;",HTML("<a href='http://sfrc.ufl.edu/people/faculty/sharma-ajay/'a/><img style='border-radius: 5px; width: 220px; height:220px' src='ajay.jpg'/>"))),

                                div(class = "container",style="border-radius: 5px;",
                                    style="border: 2px solid transparent;",style="margin-left:5px;",
                                    style="margin-top:5px;",style="background: white;",
                                    style="float: left; width: 250px;height: 260px;",
                                    h4(style = "margin-top: -3px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align:center;",style="color:#003300", HTML("Jason B. Drake")),
                                    div(style="text-align: center;",HTML("<a href='https://www.linkedin.com/in/jason-drake-39839563/'a/><img style='border-radius: 5px; width: 220px; height:220px' src='jason_drake.jpg'/>"))),


                                div(class = "container",style="border-radius: 5px;",
                                    style="border: 2px solid transparent;",style="margin-left:0px;",
                                    style="margin-top:5px;",style="background: white;",
                                    style="float: left; width: 250px;height: 260px;",
                                    h4(style = "margin-top: -3px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align:center;",style="color:#003300", HTML("Paul B. Medley")),
                                    div(style="text-align: center;",HTML("<a href='https://www.linkedin.com/in/paul-medley-222957151/'a/><img style='border-radius: 5px; width: 220px; height:220px' src='paul.jpg'/>"))),

                                div(class = "container",style="border-radius: 5px;",
                                    style="border: 2px solid transparent;",style="margin-left:5px;",
                                    style="margin-top:5px;",style="background: white;",
                                    style="float: left; width: 250px;height: 260px;",
                                    h4(style = "margin-top: -3px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align:center;",style="color:#003300", HTML("Jason G. Vogel")),
                                    div(style="text-align: center;",HTML("<a href='http://sfrc.ufl.edu/people/faculty/jason_vogel/'a/><img style='border-radius: 5px; width: 220px; height:220px' src='jason_vogel.jpg'/>"))),

                                div(class = "container",style="border-radius: 5px;",
                                    style="border: 2px solid transparent;",style="margin-left:5px;",
                                    style="margin-top:5px;",style="background: white;",
                                    style="float: left; width: 250px;height: 260px;",
                                    h4(style = "margin-top: -3px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align:center;",style="color:#003300", HTML("Gabriel A. Prata")),
                                    div(style="text-align: center;",HTML("<a href='https://www.researchgate.net/profile/Gabriel_Prata/'a/><img style='border-radius: 5px; width: 220px; height:220px' src='gabriel.jpg'/>"))),

                                div(class = "container",style="border-radius: 5px;",
                                    style="border: 2px solid transparent;",style="margin-left:5px;",
                                    style="margin-top:5px;",style="background: white;",
                                    style="float: left; width: 250px;height: 260px;",
                                    h4(style = "margin-top: -3px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align:center;",style="color:#003300", HTML("Jeff Atkins")),
                                    div(style="text-align: center;",HTML("<a href='https://atkinsjeff.github.io/'a/><img style='border-radius: 5px; width: 220px; height:220px' src='jeff.jpg'/>"))),

                                div(class = "container",style="border-radius: 5px;",
                                    style="border: 2px solid transparent;",style="margin-left:5px;",
                                    style="margin-top:5px;",style="background: white;",
                                    style="float: left; width: 250px;height: 260px;",
                                    h4(style = "margin-top: -3px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align:center;",style="color:#003300", HTML("Caio Hamamura")),
                                    div(style="text-align: center;",HTML("<a href='https://github.com/caiohamamura/'a/><img style='border-radius: 5px; width: 220px; height:220px' src='caio.png'/>"))),


                                div(class = "container",style="border-radius: 5px;",
                                    style="border: 2px solid transparent;",style="margin-left:5px;",
                                    style="margin-top:5px;",style="background: white;",
                                    style="float: left; width: 250px;height: 260px;",
                                    h4(style = "margin-top: -3px; width: 250px;",style = "margin-bottom: 10px; width: 250px;",style="text-align:center;",style="color:#003300", HTML("Carine Klauberg")),
                                    div(style="text-align: center;",HTML("<a href='https://www.researchgate.net/profile/Carine_Klauberg/'a/><img style='border-radius: 5px; width: 220px; height:220px' src='carine_Klauberg.jpg'/>"))))),

                   div(style="margin-right",title=h4(style="margin-right;margin-top: 10px;color:white",
                                                     textOutput("pageviews")))

                   # from shinybusy package
                   #add_busy_spinner(spin = "fading-circle", position = "top-right", color = "#f37938", onstart = FALSE, height = "50px", width = "50px")
)
)
################################################################################
