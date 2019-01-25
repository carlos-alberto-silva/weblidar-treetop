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

# LM
options(rgl.useNULL=TRUE)
require(RColorBrewer)
require(spatstat)
require(raster)
require(sp)
require(geometry)
require(moments)
require(maptools)
require(plotrix)
require(rgdal)
require(rasterVis)
library(rgl)
library(shiny)
#library(shinyRGL)
#library(shinyGridster)
library(ShinyDash)
#library(shinyIncubator)
library(rglwidget)

################################################################################
################################################################################

################################################################################
# server.r
options(shiny.maxRequestSize= 50*1024^2)
options(shiny.deprecation.messages=FALSE)	
shinyServer(function(input, output, session) {


CHM3Dvis<-function(chm,colR=col.rev,xlab="UTM Easting",ylab="UTM Northing",zlab="Height (m)") {

  X <- xFromCol(chm)
  Y <- yFromRow(chm)
  Z <- t((getValues(chm, format='matrix')))
  
  X <- c(min(X),X,max(X))
  Y <- c(max(Y),Y,min(Y))

  Z<-cbind(rep(0,nrow(Z)),Z,rep(0,nrow(Z)))
  Z<-rbind(rep(0,ncol(Z)),Z,rep(0,ncol(Z)))
  
  zd<-as.data.frame(Z)
  
  Z[is.na(Z)]=min(Z, na.rm =TRUE)
  Z[is.na(Z)]=0
   myColorRamp <- function(colors, values) {
    v <- (values - min(values))/diff(range(values))
    x <- colorRamp(colors)(v)
     rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
  } 
  surface3d(X, Y, Z, col=myColorRamp(colR,Z))
}
cone3d <- function(base=c(0,0,0),tip=c(0,0,1),rad=1,n=30,draw.base=TRUE,qmesh=FALSE,
                   trans = par3d("userMatrix"), ...) {
  ax <- tip-base
  if (missing(trans) && !rgl.cur()) trans <- diag(4)
  ### is there a better way?
  if (ax[1]!=0) {
    p1 <- c(-ax[2]/ax[1],1,0)
    p1 <- p1/sqrt(sum(p1^2))
    if (p1[1]!=0) {
      p2 <- c(-p1[2]/p1[1],1,0)
      p2[3] <- -sum(p2*ax)
      p2 <- p2/sqrt(sum(p2^2))
    } else {
      p2 <- c(0,0,1)
    }
  } else if (ax[2]!=0) {
    p1 <- c(0,-ax[3]/ax[2],1)
    p1 <- p1/sqrt(sum(p1^2))
    if (p1[1]!=0) {
      p2 <- c(0,-p1[3]/p1[2],1)
      p2[3] <- -sum(p2*ax)
      p2 <- p2/sqrt(sum(p2^2))
    } else {
      p2 <- c(1,0,0)
    }
  } else {
    p1 <- c(0,1,0); p2 <- c(1,0,0)
  }
  degvec <- seq(0,2*pi,length=n+1)[-1]
  ecoord2 <- function(theta) {
    base+rad*(cos(theta)*p1+sin(theta)*p2)
  }
  i <- rbind(1:n,c(2:n,1),rep(n+1,n))
  v <- cbind(sapply(degvec,ecoord2),tip)
  if (qmesh) 
    ## minor kluge for quads -- draw tip twice
    i <- rbind(i,rep(n+1,n))
  if (draw.base) {
    v <- cbind(v,base)
    i.x <- rbind(c(2:n,1),1:n,rep(n+2,n))
    if (qmesh)  ## add base twice
      i.x <-  rbind(i.x,rep(n+2,n))
    i <- cbind(i,i.x)
  }
  if (qmesh) v <- rbind(v,rep(1,ncol(v))) ## homogeneous
  if (!qmesh)
    triangles3d(v[1,i],v[2,i],v[3,i],...)
  else
    return(rotate3d(qmesh3d(v,i,material=...), matrix=trans))
}     

output$summary <- renderTable({

  output$pageviews <-  renderText({
    if (!file.exists("pageviews.Rdata")) pageviews <- 0 else load(file="pageviews.Rdata")
    pageviews <- pageviews + 1
    save(pageviews,file="pageviews.Rdata")
    paste("Number of Visits:",pageviews)
  })

  
  if ((input$Mydata)=="ED") {
    
    chmR<- raster("Eglin_plot1.asc")
  chmR0<-chmR
  } else { 

    
inFile<-input$chm
if (is.null(inFile)){
return(NULL)}

chmR<-raster(inFile$datapath)
chmR0<-chmR}


withProgress(message = 'LiDAR data processing....', value = 0.1,detail = '
                  This may take a while......', {
  Sys.sleep(0.25)
  
isolate({  
  if (input$filter==TRUE) {
    if (input$filtertype=="Mean") {
      if ( input$wsf=="3x3"){
        fws=3 }
      if ( input$wsf=="5x5"){
        fws=5 }
      if ( input$wsf=="7x7"){
        fws=7 }
      
      wf<-matrix(c(rep(1,fws*fws)),nrow=fws,ncol=fws)
      chmR <- focal(chmR, w=wf, fun=mean)
    }
    
    if (input$filtertype=="Median") {
      if ( input$wsf=="3x3"){
        fws=3 }
      if ( input$wsf=="5x5"){
        fws=5 }
      if ( input$wsf=="7x7"){
        fws=7 }
      
      wf<-matrix(c(rep(1,fws*fws)),nrow=fws,ncol=fws)
      chmR <- focal(chmR, w=wf, fun=median)
    }
    
    if (input$filtertype=="Gaussian") {
      # Gaussian filter for square cells
      fgauss <- function(sigma, n=3) {
        m <- matrix(nc=n, nr=n)
        col <- rep(1:n, n)
        row <- rep(1:n, each=n)
        x <- col - ceiling(n/2)
        y <- row - ceiling(n/2)
        # according to http://en.wikipedia.org/wiki/Gaussian_filter
        m[cbind(row, col)] <- 1/(2*pi*sigma^2) * exp(-(x^2+y^2)/(2*sigma^2))
        # sum of weights should add up to 1  
        m / sum(m)
      }
      gf=fgauss(input$Sigma)
      chmR <- focal(chmR, w=gf)}
  }
  
  output$HTtype <- renderUI({
  div(style = "margin-left:2px; width:250px;margin-top:-5px; color:white",
    radioButtons("HTtypeI", "Tree Height Threshold",list("Slide bar" = "slidebar",
      "Numeric input" = "numericbox"),inline = TRUE))
  })  
  
  output$HTboxO <- renderUI({
    div(style="margin-left: 2px;margin-top:-10px;",numericInput("HTboxI", "", 1.37))
  })
  
  output$HTsliderO <- renderUI({
    min<-min(getValues(chmR),na.rm=TRUE)
    max<-round(max(getValues(chmR),na.rm=TRUE),digits=2)
    value<-min+1.37
    div(style = "color:white;margin-left: 2px;margin-top:-10px;",
      sliderInput("HTsliderI","",min,max,value,step=0.01,sep="#.##"))
    })
  
}) 


if (input$action_button == 0) 
  return()
isolate({
  # tree dectetion by local maximum - FWS
  
  isolate({  
    if ((input$HTtypeI)=="slidebar") {
      Htreshoud<-input$HTsliderI
      
    } else {Htreshoud<-input$HTboxI} 
  })

  if ((input$Mydata)=="ED") {
    
    chmASCII<- read.asciigrid("Eglin_plot1.asc")
  } else {   
    chmASCII <- readGDAL(inFile$datapath)
  }

  if ( input$ws=="3x3"){
    fws=3 }
  if ( input$ws=="5x5"){
    fws=5 }
  if ( input$ws=="7x7"){
    fws=7 }
  if ( input$ws=="9x9"){
    fws=9 }
  if ( input$ws=="11x11"){
      fws=11}
  if ( input$ws=="13x13"){
      fws=13}

  w<-matrix(c(rep(1,fws*fws)),nrow=fws,ncol=fws)

  f <- function(chmR) max(chmR) # , na.rm=TRUE
  localmax <- focal(chmR, fun=f, w=w, pad=TRUE, padValue=NA)
  r2 <- chmR==localmax
  maxXY <- xyFromCell(r2, Which(r2==1, cells=TRUE))
  projection_chmASCII<-projection(chmASCII)
  maxXYdf<-as.data.frame(maxXY)
  decTREE<-SpatialPoints(maxXYdf)
  projection(decTREE)<-projection_chmASCII
  treesOver<-over(decTREE,chmASCII)
  treeMer<-cbind(maxXYdf,treesOver)
 colnames(treeMer)<-c("x","y","z") 
  head(treeMer)
  tree<-subset(treeMer, treeMer$z >=Htreshoud)
  colnames(tree)<-c("x","y","z")
})


temp<-tree
for(i in 1:nrow(temp)) { 
  isolate({
    Sys.sleep(0.02)
      
    Ang<-as.numeric(input$Ang)
    ht1<-as.numeric(input$ht1)
    ht2<-as.numeric(input$ht2)
    ht3<-as.numeric(input$ht3)
    width<-as.numeric(input$frv)   
    # equation from Popescu and Wynne
    # http://asprs.org/a/publications/pers/2004journal/may/2004_may_589-604.pdf
    if ((input$radiustype)=="FR") {temp[i,4]<-width} 
    
    if ((input$radiustype)=="VR") {
      if ((input$equation)=="DC") {temp[i,4] <- (3.09632 + 0.00895*(temp[i,3]*temp[i,3]))} 
      if ((input$equation)=="PI") {temp[i,4] <- (3.75105 + 0.17919*(temp[i,3]) + 0.01241*(temp[i,3]*(temp[i,3])))} 
      if ((input$equation)=="CB") {temp[i,4]<-(2.51503+0.00901*(temp[i,3]*temp[i,3]))}
      if ((input$equation)=="YR") {temp[i,4]<-Ang+ht1*temp[i,3]+ht2*(temp[i,3]*temp[i,3]) + ht3*(temp[i,3]*temp[i,3]*temp[i,3])}
    } 
  })
}



output$hist <- renderPlot({
  par(mfrow=c(1,2), mar=c(4.5,4,2,5))
  chmASCII.df<-data.frame(chmASCII)
  isolate({
    chmASCII.df<-subset(chmASCII.df,chmASCII.df[,1]>=Htreshoud)
  })
  
  dens<-density(chmASCII.df[,1],adjust = 1.3, kernel = "gaussian")
  plot(dens$y,dens$x, col="black",xlab="Density",ylab="Height (m)",type="line",lwd="1",ylim=c(0,max(chmASCII.df[,1]*1.3))) 
  polygon(dens$y,dens$x, col="forestgreen", border="black")
  boxplot(chmASCII.df[,1],ylim=c(0,max(chmASCII.df[,1])*1.3),horizontal=F, col="forestgreen",ylab="Height (m)")
  },height = 360,width=850)

isolate({
  output$downloadHist <- downloadHandler(
    filename <- function() {
      paste("CHM profile ",input$chm, Sys.Date(),'.png',sep='') },
    content <- function(file) {
      png(file, width = 800, height = 600, units = "px", pointsize = 12,
          bg = "white", res = 100)
      
      par(mfrow=c(1,2), mar=c(4.5,4,2,5))
      chmASCII.df<-data.frame(chmASCII)
      isolate({
        chmASCII.df<-subset(chmASCII.df,chmASCII.df[,1]>=Htreshoud)})
      
      dens<-density(chmASCII.df[,1],adjust = 1.3, kernel = "gaussian")
      plot(dens$y,dens$x, col="black",xlab="Density",ylab="Height (m)",type="line",lwd="1",ylim=c(0,max(chmASCII.df[,1]*1.3))) 
      polygon(dens$y,dens$x, col="forestgreen", border="black")
      boxplot(chmASCII.df[,1],ylim=c(0,max(chmASCII.df[,1]*1.3)),horizontal=F, col="forestgreen",ylab="Height (m)")
      dev.off()},
    contentType = 'image/png'
  )})

isolate({
    
output$CHMplot2D <- renderPlot({
  chmASCII.df<-data.frame(chmASCII)
  colS<-input$Pallet
  
  if ( colS =="BlGrRed") {
  myColorRamp <- function(colors, values) {
    v <- (values - min(values))/diff(range(values))
    x <- colorRamp(colors)(v)
    
    head(x)
    rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
  }
  
  col.rev <- myColorRamp(c("blue","green","yellow","red"),0:255)

  
  } else {
  
  col<-brewer.pal(9,colS)
  col.rev<-rev(col) 
}
  tree.xy<-data.frame(tree[,1:2])
  tree.xy <- data.frame(na.omit(tree.xy))
  
  plot(chmR0,col=col.rev,axes = T, xlab="UTM Easting",ylab="UTM Northing") 
  points(tree.xy, pch=16, cex = 1.5, col="forestgreen",  type = "p") 
  for ( i in 1:length(temp[,1])) {
    col<-c(1:length(temp[,1]))
    draw.circle(x=temp[i,1],y=temp[i,2],(temp[i,4])/2,border=col[i],lty=1,lwd=1)
  } },height = 570,width=640) })

isolate({
output$downloadCHMprint <- downloadHandler(
  filename <- function() {
    paste("CHM",input$chm, Sys.Date(),'.png',sep='') },
  content <- function(file) {
    png(file, width = 600, height = 600, units = "px", pointsize = 12,
        bg = "white", res = 100)
    
    colS<-input$Pallet
      if ( colS =="BlGrRed") {
  
  myColorRamp <- function(colors, values) {
    v <- (values - min(values))/diff(range(values))
    x <- colorRamp(colors)(v)
    
    head(x)
    rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
  }
  
  col.rev <- myColorRamp(c("blue","green","yellow","red"),0:255)
  
  } else {
  
  col<-brewer.pal(9,colS)
  col.rev<-rev(col) 
}
    
    tree.xy<-data.frame(tree[,1:2])
    tree.xy <- data.frame(na.omit(tree.xy))
    
    plot(chmR0,col=col.rev,axes = T, xlab="UTM Easting",ylab="UTM Northing") 
    points(tree.xy, pch=16, cex = 1.5, col="forestgreen",  type = "p") 
    
    for ( i in 1:length(temp[,1])) {
      col<-c(1:length(temp[,1]))
      draw.circle(x=temp[i,1],y=temp[i,2],(temp[i,4])/2,border=col[i],lty=1,lwd=1)
    }
    dev.off()},
  contentType = 'image/png'
)})

AreaPolyTree<-pi*((temp[,4]/2)^2)
newDataTree<-cbind(temp,AreaPolyTree)
colnames(newDataTree)<-c("x","y","Height","CW", "CA")

output$downloadShpR <- renderUI({div(style="margin-left:0px;margin-top: -5px;width:300px",downloadButton('downloadShp', 'Tree Crown (.shp)')) })


createShp <- reactive({
  myXY<-temp
  if (is.null(myXY)){
    return(NULL)      
  } else {
    polys<-list()
    
    width<-NULL
    myXY<-temp
    myXY <- data.frame(na.omit(temp))

    for(i in 1:nrow(myXY)) { 
      
      width[i] = (myXY[i,4])/2
      discbuff<-disc(radius=width[i], centre=c(myXY$x[i], myXY$y[i])) 
      discpoly<-Polygon(rbind(cbind(discbuff$bdry[[1]]$x, 
                                    y=discbuff$bdry[[1]]$y), c(discbuff$bdry[[1]]$x[1], 
                                                               y=discbuff$bdry[[1]]$y[1]))) 
      polys<-c(polys, discpoly) 
    }
    
    spolys<-list() 
    
    for(i in 1:length(polys)) { 
      spolybuff<-Polygons(list(polys[[i]]), ID=row.names(myXY)[i]) 
      spolys<-c(spolys, spolybuff) 
    }
    
    polybuffs<-SpatialPolygons(spolys) 
    
    
    SHP = SpatialPolygonsDataFrame(polybuffs, 
      data=data.frame(x=myXY[,1], y=myXY[,2], 
        row.names=sapply(slot(polybuffs, 'polygons'), function(x) slot(x, 'ID'))))
    
    proj4string(SHP) <- projection_chmASCII
    SHP$Height<-newDataTree[,3]
    SHP$CW<-newDataTree[,4]
    SHP$CA<-newDataTree[,5]
    return(SHP)
  }
})


output$downloadShp <- downloadHandler(
  
  filename = 'TreeCrownExport.zip',
  content = function(file) {
    if (length(Sys.glob("TreeCrownExport.*"))>0){
      file.remove(Sys.glob("TreeCrownExport.*"))
    }

    
    setwd(tempdir())
    writeOGR(createShp(), dsn="TreeCrownExport.shp", layer="TreeCrownExport", driver="ESRI Shapefile")
    zip(zipfile='TreeCrownExport.zip', files=Sys.glob("TreeCrownExport.*"))
    file.copy("TreeCrownExport.zip", file)
    if (length(Sys.glob("TreeCrownExport.*"))>0){
      file.remove(Sys.glob("TreeCrownExport.*"))
    }
  }
)



##############################################
output$downloadShpRXY <- renderUI({
  div(style="margin-left:160px;margin-top: -31px;width:300px",
  downloadButton('downloadShpXY', 'Tree Location (.shp)')) })

createShpXY <- reactive({
 
 
  if (is.null(newDataTree)){
    return(NULL)      
  } else {
   
lots <- SpatialPointsDataFrame(coords= cbind(newDataTree[,1],newDataTree[,2],
    newDataTree[,3], newDataTree[,4],newDataTree[,5]), data = newDataTree)
    proj4string(lots) <- projection_chmASCII
    return(lots)
  }
})


output$downloadShpXY <- downloadHandler(
  
  filename = 'TreeLocationExport.zip',
  content = function(file) {
    if (length(Sys.glob("TreeLocationExport.*"))>0){
      file.remove(Sys.glob("TreeLocationExport.*"))
    }
   
    setwd(tempdir())
    writeOGR(createShpXY(), dsn="TreeLocationExport.shp", 
      layer="TreeLocationExport", driver="ESRI Shapefile")
    zip(zipfile='TreeLocationExport.zip', files=Sys.glob("TreeLocationExport.*"))
    file.copy("TreeLocationExport.zip", file)
    if (length(Sys.glob("TreeLocationExport.*"))>0){
      file.remove(Sys.glob("TreeLocationExport.*"))
    }
  }
)

#########################################


isolate({
  output$PLOT3D <- renderRglwidget({
	try(rgl.close())
	
    if ((input$plot3Dradio)=="plotCHM3D") {
      colS<-input$Pallet
 
   if (colS =="BlGrRed") {
   
   myPal<-c("blue","green","yellow","red")
  
		} else {
  
		myPal <- rev(brewer.pal(9, colS))
		}
      #myPal <- colorRampPalette(rev(brewer.pal(9, colS))) 
       
      
	  #plot3D(chmR0,col=myPal)#,lit=TRUE,specular="black")
	  CHM3Dvis(chm=chmR0,colR=myPal,xlab="UTM Easting",ylab="UTM Northing",zlab="Height (m)")
      axes3d(c("x-", "y-"), col="black")
      title3d(xlab = "UTM.Easting", ylab = "UTM.Northing")#, col="green")
      #planes3d(a=0,b=0,c=-1,d=0.0001,color= "gray",alpha=0.4)
      aspect3d(1,1,0.5)
	  widget <- rglwidget()
    } else {
	
	xl<-max(tree[,1])-tree[,1]
	yl<-max(tree[,2])-tree[,2]
	
	CBHp<-sample(0.4,0.7,length(xl))
	CL<-1-CBHp
      for(i in 1:nrow(tree)) {
        cone3d(base=c(tree[i,1],tree[i,2],tree[i,3]*0.6), 
          rad=(temp[i,4])/2,tip=c(tree[i,1],tree[i,2],tree[i,3]), 
          col="forestgreen",front="lines")        
		vec=rbind(c( tree[i,1],tree[i,2], 0 ), c(tree[i,1], tree[i,2],tree[i,3]))
        segments3d( vec, col=" brown", lwd=2 ) 
      }
       
      axes3d(c("x-", "y-"), col="black")
      title3d(xlab = "UTM.Easting", ylab = "UTM.Northing")#, col="forestgreen")
      planes3d(a=0,b=0,c=-1,d=0.0001,color="gray",alpha=0.4)
      aspect3d(1,1,0.3)
	widget <- rglwidget(scene3d())
	#if (interactive())
  	#widget
	# Save it to a file.  This requires pandoc
	#filename <- tempfile(fileext = ".html")
	#htmlwidgets::saveWidget(rglwidget(), filename)
	#browseURL(filename)
    } 
  })  

})

output$TreelistR <- renderUI({div(style="margin-left:55px;margin-top: -30px;width:200px",
  downloadButton('downloadTreesLoc', 'Tree location (.csv) ')) 
})


output$Profile <- renderUI({
  div(style = "margin-top: -70px;margin-left:390px",
  downloadButton('downloadTable', 'Download LiDAR metrics'),
  downloadButton('downloadHist', 'Download CHM Profile'),
  downloadButton('downloadCHMprint', 'Download CHM'))
})


isolate ({
  output$downloadTreesLoc <- downloadHandler(
    filename = function() {
      paste("Trees",input$chm, Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      Treerow<-c(1:length(temp[,3]))
      tree.csv<-data.frame(cbind(Treerow,newDataTree))
      colnames(tree.csv)<-c("Tree","x","y","Height","Crown Width","Crown Area")
      write.csv(tree.csv, file, row.names=FALSE)}
    
  ) }) 

isolate ({
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("LiDAR Tree metrics",input$chm, Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(LiDARsummary, file, row.names=FALSE)}
    
  ) })

print(tree)
print(temp)
isolate ({
  Hexp<-as.numeric(temp[,3])
  NameExp<-c("Number of trees","Hmax","Hmean","Hmin","Hmedian","Hvar","Hsd","Hcv","Hkurtosis","Hskewness")
  MetricsExp<-c(length(Hexp), round(max(Hexp),digits=2),round(mean(Hexp),digits=2),
                round(min(Hexp),digits=2),round(median(Hexp),digits=2),round(var(Hexp),digits=2),
                round(sd(Hexp),digits=2),round(cv(Hexp),digits=2),round(kurtosis(Hexp),digits=2),
                round(skewness(Hexp),digits=2))
  
  print(NameExp)
  print(MetricsExp)
  LiDARsummary<-as.data.frame(cbind(NameExp,MetricsExp))
  colnames(LiDARsummary)<-c("Parameters", "Value")
  LiDARsummary  
}) 


})

 
})
################################################################################


    
})
################################################################################
