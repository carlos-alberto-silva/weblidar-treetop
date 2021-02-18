################################################################################
#               ___________________________________________________            #
#                                Server.r                                      #
#                               Weblidar- treetop                              #
#               Web Application for processing and visualizing                 #
#                             LiDAR data using R and shiny                     #
#               ___________________________________________________            #
#                                                                              #
################################################################################

################################################################################
# Libraries
options(rgl.useNULL=TRUE)
# warning messages off
options(warn=-1)


################################################################################

################################################################################
# server.r
options(shiny.maxRequestSize= 500*1024^2)
options(shiny.deprecation.messages=FALSE)

shinyServer(function(input, output, session) {


  interpol<- function(input,col) {
    surf.3d <- t(convhulln(input,options = "QJ"))
    rgl.triangles(input[surf.3d,1],input[surf.3d,2],input[surf.3d,3],col=col,alpha = 1,
                  lit = TRUE,ambient = "black",specular = "white",emission = "black",shininess = 50.0,
                  smooth = TRUE, texture = NULL,front = "fill",back ="fill", box=F,axes = FALSE)}



  # kurtosis and skewness from moments package
  kurtosis<-function (x, na.rm = FALSE)
  {
    if (is.matrix(x))
      apply(x, 2, kurtosis, na.rm = na.rm)
    else if (is.vector(x)) {
      if (na.rm)
        x <- x[!is.na(x)]
      n <- length(x)
      n * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)
    }
    else if (is.data.frame(x))
      sapply(x, kurtosis, na.rm = na.rm)
    else kurtosis(as.vector(x), na.rm = na.rm)
  }

  skewness<-function (x, na.rm = FALSE)
  {
    if (is.matrix(x))
      apply(x, 2, skewness, na.rm = na.rm)
    else if (is.vector(x)) {
      if (na.rm)
        x <- x[!is.na(x)]
      n <- length(x)
      (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
    }
    else if (is.data.frame(x))
      sapply(x, skewness, na.rm = na.rm)
    else skewness(as.vector(x), na.rm = na.rm)
  }
   TreesModel<- function(crownshape=c("cone","ellipsoid","halfellipsoid","paraboloid","cylinder"),
                        nz=15, nalpha=15, CL=5, CR=5, HCB=10, x0=0, y0=0, z0=0, dbh = 0.3, crowncolor = "forestgreen",
                        stemcolor = "chocolate4", shape=1
  ){

    crownshape <- match.arg(crownshape)

    Ht<-HCB+CL

	if (shape==3) {

	  if (Ht<=5){
    z <- rep(seq(0,1,length=10),each=5)
    angs <- rep(seq(0,2*pi, length=10),5)} else {

      nz=10; nalpha=10
      z <- rep(seq(0,1,length=nz),each=nalpha)
      angs <- rep(seq(0,2*pi, length=nalpha),nz)

	}

	  z<-jitter(z,2)
	  angs<-jitter(angs,2)

	  } else {
      z <- rep(seq(0,1,length=nz),each=nalpha)
      angs <- rep(seq(0,2*pi, length=nalpha),nz)
    }


    if(crownshape == "cone")distfun <- (1-z)
    if(crownshape == "ellipsoid")distfun <- sqrt(1 - ((z-1/2)^2)/((1/2)^2))
    if(crownshape == "halfellipsoid")distfun <- sqrt(1 - z**2)
    if(crownshape == "paraboloid")distfun <- sqrt(1-z)
    if(crownshape == "cylinder")distfun <- 1
    H <- HCB + CL
    r <- CR
    x <- x0 + r*distfun*cos(angs)
    y <- y0 + r*distfun*sin(angs)
    z <- z0 + HCB + z*CL

    keep <- !duplicated(cbind(x,y,z))
    x <- x[keep]
    y <- y[keep]
    z <- z[keep]
    klj=matrix(cbind(x,y,z),ncol=3)

    if (shape==1){

    mMatrix<-matrix(NA,ncol=3)[-1,]

    for ( i in 1:nrow(klj)){
      ln=i+nz
      if ( ln >= nrow(klj)) { ln2=nrow(klj) } else { ln2= ln}
        mMatrix<-rbind(mMatrix,rbind(klj[i,],klj[ln2,])) }
        kljzbase=subset(klj,klj[,3]==z[2])
         kljzbaseNew<-matrix(NA,ncol=3)[-1,]
            for ( i in 1:nrow(kljzbase)){
               kljzbaseNew<-rbind(kljzbaseNew,rbind(kljzbase[i,],c(x0,y0,HCB)))

    }
    newList<-rbind(kljzbaseNew,mMatrix,klj)
    rgl::lines3d(newList, col=crowncolor, add=T,box=F)
    }

	if (shape==2){interpol(klj,col=crowncolor)}

    if (shape==3){

	    NewList<-matrix(ncol=3)[-1,]
 	      for ( k in 1:nrow(klj)){
 	        sk<-sample(c(0.25,-0.25,0.5,-0.5), 1)
		      NewList<-rbind(NewList,rbind(klj[k,],c(x0,y0,klj[k,3]+sk)))
	    }
	NewList<-jitter(NewList,15)
	head(NewList)
	if (Ht<=2){col=rep("green",nrow(NewList))}
	if (Ht> 2 & Ht<=5){col=sample(c("green3","green3","green"),nrow(NewList), TRUE)}
	if (Ht>5){col=c("darkgreen")}

	rgl::lines3d(NewList, col=col, add=T)
	}
 }




GiniCoeff <- function (x, finite.sample = TRUE, na.rm = TRUE){
  if (!na.rm && any(is.na(x)))
    return(NA_real_)
  x <- as.numeric(stats::na.omit(x))
  n <- length(x)
  x <- sort(x)
  G <- 2 * sum(x * 1L:n)/sum(x) - (n + 1L)
  if (finite.sample)
    GC <- G/(n - 1L)
  else GC <- G/n
  return(GC)
}

getExtension <- function(file){
  ex <- strsplit(basename(file), split="\\.")[[1]]
  return(ex[length(ex)])
}
####################################

output$summary <- renderTable({

  observeEvent(input$refresh, {
    if (input$refresh==1){
      session$reload()
      rm(list=ls())
  }
    })


  output$pageviews <-  renderText({
    if (!file.exists("pageviews.Rdata")) pageviews <- 0 else load(file="pageviews.Rdata")
    pageviews <- pageviews + 1
    save(pageviews,file="pageviews.Rdata")
    paste("Number of Visits:",pageviews)
  })


  if ((input$Mydata)=="ED") {

  chmR<- raster::raster(system.file('extdata', 'Eglin_plot1.asc', package='treetop'))

  chmR0<-chmR
  projecCHM<-raster::projection(chmR)
  detail<-""
  area_ha <- 2.5
  #browser()
  } else {


 inFile<-input$chm
 if (is.null(inFile)){
 return(NULL)}

 fileExt<-getExtension(inFile$name)

 if (!any(fileExt==c("tif","asc","img"))){

   withProgress(message = "Invalid file; Please upload a raster ('.tif', '.asc' or '.img') file!",
                value = 1,detail = "Treetop will be refreshed!",{
     Sys.sleep(10)
     #return(NULL)
     #validate("Invalid file; Please upload a raster (".tif", ".asc" or ".img") file!")
     session$reload()

   })
  }


 chmR<-raster(inFile$datapath)
 chmR[chmR[]<0]<-0
 projecCHM<-raster::projection(chmR)
 area_ha<-0
 reschmR<-raster::res(chmR)[1]
 newst<-extent(chmR)

 r1NaM <- is.na(as.matrix(chmR))
 colNotNA <- which(colSums(r1NaM) != nrow(chmR))
 rowNotNA <- which(rowSums(r1NaM) != ncol(chmR))

 exst <- extent(chmR, rowNotNA[1], rowNotNA[length(rowNotNA)],
                   colNotNA[1], colNotNA[length(colNotNA)])
 chmR<-crop(chmR,exst)
 chmR0<-chmR

  }


 isolate({
 area_ha <- (ncell(chmR)*raster::res(chmR)[1]^2)/1000


 if (area_ha > 1000){

   grid <- aggregate(chmR, fact=500/res(chmR))
   grid_spdf <- as(grid, "SpatialPolygonsDataFrame")
   grid_spdf<-grid_spdf[!is.na(grid_spdf@data[,1]),]

   #grid <- raster(extent(chmR)+c(-500,+500,-500,+500), resolution = 500)
   grid_spdf@data$id <- 1:nrow(grid_spdf@data)

   #div(style = "color:white",uiOutput("tiles"))
   output$tiles <- renderUI({

     div(style="margin-left:1px;margin-top: -20px;width:245px",
         selectizeInput("tiles_list", label="Select tiles", choices=c("All",grid_spdf@data$id), selected = "All", multiple = TRUE,
                        options = NULL))



     #div(style="margin-left: 2px;margin-top:-10px;",numericInput("HTboxI", "", 1.37))
   })


   #exst<-extent(chmR)
  #newst<-extent(mean(exst[1:2])-plotlength,mean(exst[1:2])+plotlength,mean(exst[3:4])-plotlength,mean(exst[3:4])+plotlength)
  #chmR<-crop(chmR,newst)

  #if (reschmR<0.5){
  #detail<-paste0("Note: the study area is larger then 3ha. We have resampled the file extent to 3ha using xmin: ",newst[1],"; xmax: ",newst[2],
  #"; ymin: ",newst[3],"; ymax: ",newst[4]," extent. Also, the grid cell size of the CHM file is smaller then 0.5m. We have resampled it to 0.5m.")
  #} else {
  #  detail<-paste0("Note: the study area is larger then 3ha. We have resampled the file extent to 3ha using xmin: ",newst[1],"; xmax: ",newst[2],
  #                 "; ymin: ",newst[3],"; ymax: ",newst[4]," extent.")}
  }
})


 isolate({


  output$HTtype <- renderUI({
  div(style = "margin-left:2px; width:250px;margin-top:-10px; color:white",
      tags$head(tags$style(HTML('#action_button{background-color:#78FF33}'))),
    radioButtons("HTtypeI", "Tree height threshold",list("Slide bar" = "slidebar",
      "Numeric input" = "numericbox"),inline = TRUE))
  })

  output$HTboxO <- renderUI({
    div(style="margin-left: 2px;margin-top:-10px;",numericInput("HTboxI", "", 1.37))
  })

  chm_hts<-chmR0[!is.na(chmR0)]

  output$HTsliderO <- renderUI({
    min<-min(chm_hts,na.rm=TRUE)
    max<-round(max(chm_hts,na.rm=TRUE),digits=2)
    value<-min+1.37
    div(style = "color:white;margin-left: 2px;margin-top:-10px;",
      sliderInput("HTsliderI","",min,max,value,step=0.01,sep="#.##"))
    })


  # plot CHM 2D or lorenzCurve
  isolate({
    output$CHMplot2D <- renderPlot({
        colS<-input$Pallet

        if  ( area_ha > 1000){
          message = "Displaying the uploaded CHM file. Note: the study area is larger then 1000ha. We will active the tile option."
          detail = "Please select tiles, parameters for processing and click on run!"
        } else{
             message = "Displaying the uploaded CHM file."
            detail = "Please select parameters for processing and click on run!"
          }

        withProgress(message = message,
                     value = 1,detail = detail, {
        Sys.sleep(2)

        myColorRamp <- function(colors, values) {
          v <- (values - min(values))/diff(range(values))
          x <- colorRamp(colors)(v)
          head(x)
          rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
        }

        if ( colS =="BlGrRed") {col.rev <- myColorRamp(c("blue","green","yellow","red"),0:255)}
        if ( colS =="Viridis") {col.rev <- myColorRamp(c("#440154FF","#482878FF","#3E4A89FF",
                                                         "#31688EFF","#26828EFF","#1F9E89FF",
                                                         "#35B779FF","#6DCD59FF","#B4DE2CFF",
                                                         "#FDE725FF"),0:255)}

        if ( !colS =="BlGrRed" & !colS =="Viridis") {
          col<-RColorBrewer::brewer.pal(9,colS)
          col.rev<-rev(col)
        }
        par(mar=c(4,4,2,2))

        if (area_ha < 1000){
          raster::plot(chmR0,col=col.rev,axes = T, xlab="",ylab="",useRaster=F, legend=F)
#browser()
          r.range <- c(min(chm_hts, na.rm=T), max(chm_hts, na.rm=T))
          plot(chmR0, legend.only=TRUE, col=col.rev,
               legend.width=1, legend.shrink=0.75,#smallplot=c(0.9,1, .3,.75),
               axis.args=list(at=seq(r.range[1], r.range[2], 2),
                              labels=seq(r.range[1], r.range[2], 2),
                              cex.axis=1.5),
               legend.args=list(text='Height (m)', side=4, font=2, line=2.5, cex=1.5))
        }else{
          raster::plot(grid_spdf, border="red", lwd=2, axes=F)
          raster::plot(chmR0,col=col.rev,axes = F, xlab="",ylab="",useRaster=F, add=T, legend=F)
          axis(1);axis(2)
          r.range <- c(minValue(chmR0), maxValue(chmR0))
          plot(chmR0, legend.only=TRUE, col=col.rev,
               legend.width=1, legend.shrink=0.75,#smallplot=c(0.9,1, .3,.75),
               axis.args=list(at=seq(r.range[1], r.range[2], 2),
                              labels=seq(r.range[1], r.range[2], 2),
                              cex.axis=1.5),
               legend.args=list(text='Height (m)', side=4, font=2, line=2.7, cex=1.5))

          raster::plot(grid_spdf, add=T, axes=F,border="red", lwd=2)
          points(coordinates(grid_spdf),labels=grid_spdf$id, cex = 8, pch=16, col="gray")
          text(coordinates(grid_spdf),labels=grid_spdf$id, cex = 1.5, col="black")
        }
    })},height = 600,width=650)
  })





  output$hist <- renderPlot({
      par(mfrow=c(1,2), mar=c(5,5,2,1))
      dens<-density(chm_hts,adjust = 1.3, kernel = "gaussian")
      #par(mfrow=c(1,3), mar=c(5,5,2,2))
      plot(dens$y,dens$x, cex.lab=2,col="black",xlab="Density",ylab="Height (m)",type="line",lwd="1",ylim=c(0,max(chm_hts*1.3)))
      polygon(dens$y,dens$x, col=input$profColor, border="black")
      legend("topright","CHM height distribution", bty="n", text.font=2, cex=1.5)
      #if (!is.null(input$HTsliderI)) {abline(v=input$HTsliderI, lwd=2, col="red")}
      #if (!is.null(input$HTboxI)) {abline(v=input$HTboxI, lwd=2, col="red")}
      boxplot(chm_hts, cex.lab=2, ylim=c(0,max(chm_hts)*1.3),horizontal=F, col=input$profColor,ylab="Height (m)")

  },height = 360,width=850)




  isolate({
    output$PLOT3D <- rgl::renderRglwidget({

      while (rgl::rgl.cur() > 0) { try(rgl::rgl.close())}

        colS<-input$Pallet
        myColorRamp <- function(colors, values) {
          v <- (values - min(values))/diff(range(values))
          x <- colorRamp(colors)(v)
          rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
        }

        if ( colS =="BlGrRed") {myPal <- myColorRamp(c("blue","green","yellow","red"),0:255)}
        if ( colS =="Viridis") {myPal <- myColorRamp(c("#440154FF","#482878FF","#3E4A89FF",
                                                       "#31688EFF","#26828EFF","#1F9E89FF",
                                                       "#35B779FF","#6DCD59FF","#B4DE2CFF",
                                                       "#FDE725FF"),0:255)}

        if ( !colS =="BlGrRed" & !colS =="Viridis") {
          col<-RColorBrewer::brewer.pal(9,colS)
          myPal<-rev(col)
        }

        while (rgl::rgl.cur() > 0) { try(rgl::rgl.close()) }

        rasterVis::plot3D(chmR0,col=myPal,xlab="",ylab="",zlab="Height (m)")
        #CHM3Dvis1<-CHM3Dvis(chm=chmR0,colR=myPal,xlab="",ylab="",zlab="Height (m)")
        #rm(CHM3Dvis1)
        rgl::axes3d(c("x-", "y-"), col="black")
        rgl::title3d(xlab = "UTM Easting", ylab = "UTM Northing")#, col="green")
        #rgl::aspect3d(1,1,0.5)
        rgl::rglwidget()

   })
  })

  isolate ({
    output$summary2 <- renderTable({

    NameExp<-c("Area (ha)","Hmax","Hmean","Hmin","Hmedian","Hvar","Hsd","Hcv","Hkurtosis","Hskewness")
    MetricsExp<-c(round(area_ha,digits=2),#length(chm_hts),
                  round(max(chm_hts),digits=2),
                  round(mean(chm_hts),digits=2),
                  round(min(chm_hts),digits=2),
                  round(median(chm_hts),digits=2),
                  round(var(chm_hts),digits=2),
                  round(sd(chm_hts),digits=2),
                  round(mean(chm_hts)/sd(chm_hts),digits=2),
                  round(kurtosis(chm_hts),digits=2),
                  round(skewness(chm_hts),digits=2))

    LiDARsummary0<-data.frame(cbind(NameExp,MetricsExp))
    colnames(LiDARsummary0)<-c("Parameters", "Value")
    #browser()
    #Sys.sleep(1.5)
    if(!exists("LiDARsummary")){
      LiDARsummary0
    }
   })

  })
 })


 if (input$action_button == 0)
   return()

 #withProgress(message = paste('LiDAR data processing.This may take a few seconds','The memory used is',round(mem_used()/1024^2), "Mb."), value = 0.1,detail = detail, {
 withProgress(message = 'LiDAR data processing. This may take a few minutes!', min = 0, max = 1, value = 0.1,detail = "", {

   #browser()
   isolate({
   if (area_ha > 1000){
    if (!any(input$tiles_list=="All") & !is.null(input$tiles_list)==TRUE){
     chmR <- crop(raster::mask(chmR, grid_spdf[input$tiles_list,]),grid_spdf[input$tiles_list,])
      }
   }


   })
   chmR0<-chmR


  # if (input$filtertype=="Mean") {chmR <- raster::focal(chmR, w=wf, fun=mean)}
   #if (input$filtertype=="meadian") {chmR <- raster::focal(chmR, w=wf, fun=median)}
  #if (input$filtertype=="Gaussian") {chmR <- raster::focal(chmR, w=gf)}

    #Sys.sleep(10)

 isolate({
  # tree dectetion by local maximum - FWS

  isolate({
    if ((input$HTtypeI)=="slidebar") {
      Htreshoud<-input$HTsliderI

    } else {Htreshoud<-input$HTboxI}
  })

   if (input$filter==TRUE) {
     if (input$filtertype=="Mean") {
       if ( input$sws=="3x3"){
         sws=3 }
       if ( input$sws=="5x5"){
         sws=5 }
       if ( input$sws=="7x7"){
         sws=7 }
       if ( input$sws=="9x9"){
         sws=9 }
       if ( input$sws=="11x11"){
         sws=11 }
       if ( input$sws=="13x13"){
         sws=13 }
       if ( input$sws=="15x15"){
         sws=15 }
       if ( input$sws=="17x17"){
         sws=17 }
       wf<-matrix(c(rep(1,sws*sws)),nrow=sws,ncol=sws)
       chmR <- raster::focal(chmR, w=wf, fun=mean)
     }

     if (input$filtertype=="Median") {
       if ( input$sws=="3x3"){
         sws=3 }
       if ( input$sws=="5x5"){
         sws=5 }
       if ( input$sws=="7x7"){
         sws=7 }
       if ( input$sws=="9x9"){
         sws=9 }
       if ( input$sws=="11x11"){
         sws=11 }
       if ( input$sws=="13x13"){
         sws=13 }
       if ( input$sws=="15x15"){
         sws=15 }
       if ( input$sws=="17x17"){
         sws=17 }
       wf<-matrix(c(rep(1,sws*sws)),nrow=sws,ncol=sws)
       chmR <- raster::focal(chmR, w=wf, fun=median)
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
       chmR <- raster::focal(chmR, w=gf)
     }
   }
  if ( input$fws=="3x3"){
    fws=3 }
  if ( input$fws=="5x5"){
    fws=5 }
  if ( input$fws=="7x7"){
    fws=7 }
  if ( input$fws=="9x9"){
    fws=9 }
  if ( input$fws=="11x11"){
      fws=11}
  if ( input$fws=="13x13"){
      fws=13}
  if ( input$fws=="15x15"){
    fws=15 }
  if ( input$fws=="17x17"){
    fws=17 }
  decTREE <- lidR::tree_detection(chmR, lidR::lmf(ws=fws))
  treeMer<-cbind(as.data.frame(decTREE@coords),as.numeric(paste0(decTREE@data[,2])))
  colnames(treeMer)<-c("x","y","Height")
   tree<-subset(treeMer, treeMer$Height >=Htreshoud)
  colnames(tree)<-c("x","y","Height")
  #if(exists("decTREE")){rm(decTREE)}
 })

   incProgress(0.2, message = paste("Total of",nrow(tree),"individual trees detected!"))
   Sys.sleep(0.5)


   incProgress(0.2, message = "Starting individual tree crown delineation")
   Sys.sleep(0.5)
 if ((input$radiustype)=="VR") {
 isolate({
    treelist_treetop<-tree
    Ang<-as.numeric(input$Ang)
    ht1<-as.numeric(input$ht1)
    ht2<-as.numeric(input$ht2)
    ht3<-as.numeric(input$ht3)
    width<-as.numeric(input$frv)
    # equation from Popescu and Wynne
    #Popescu, S. C., & Wynne, R. H. (2004). Seeing the trees in the forest: using lidar and multispectral data
    #fusion with local filtering and variable window size for estimating tree height. Photogrammetric
    #Engineering and Remote, 70(5), 589–604. Retrieved from
    #http://asprs.org/a/publications/pers/2004journal/may/2004_may_589-604.pdf

    if ((input$radiustype)=="VR") {
      if ((input$equation)=="DC") {treelist_treetop$CR <- 3.09632 + 0.00895*(tree[,3]*tree[,3])}
      if ((input$equation)=="PI") {treelist_treetop$CR <- 3.75105 + 0.17919*(tree[,3]) + 0.01241*(tree[,3]*(tree[,3]))}
      if ((input$equation)=="CB") {treelist_treetop$CR<-2.51503+0.00901*(tree[,3]*tree[,3])}
      if ((input$equation)=="YR") {treelist_treetop$CR<-Ang+ht1*tree[,3]+ht2*(tree[,3]*tree[,3]) + ht3*(tree[,3]*tree[,3]*tree[,3])}
    }

    treelist_treetop$CA<-pi*(treelist_treetop$CR/2)^2
    treelist_treetop$treeID<-1:nrow(treelist_treetop)
    treelist_treetopsdf<-sp::SpatialPointsDataFrame(treelist_treetop[,1:2],data=treelist_treetop)
    treelist_treetopsdf@data<-treelist_treetopsdf@data[,c("x","y","Height","CA","CR","treeID")]
    polybuffs<-rgeos::gBuffer(SpatialPoints(treelist_treetop[,1:2]), width=treelist_treetop$CR*2, byid=TRUE, id=treelist_treetop$treeID)
    polyCrown = SpatialPolygonsDataFrame(polybuffs,
                                   data=data.frame(treelist_treetop,
                                                   row.names=sapply(slot(polybuffs, 'polygons'), function(x) slot(x, 'ID'))))

    polyCrown<-polyCrown[!is.na(polyCrown@data$CA),]
    treelist_treetopsdf<-treelist_treetopsdf[!is.na(treelist_treetopsdf@data$CA),]

  })
 }

 if ((input$radiustype)=="FR") {
   #browser()
   treelist_treetop<-tree
   treelist_treetopsdf<-sp::SpatialPointsDataFrame(treelist_treetop[,1:2],data=treelist_treetop)
   treelist_treetopsdf@data$treeID<-1:nrow(treelist_treetop)
   crowns=lidR::silva2016(chmR0, treelist_treetopsdf,max_cr_factor = input$maxcrown,exclusion = input$exclusion,
                          ID = "treeID")()
   contour_sf <- sf::st_as_sf(stars::st_as_stars(crowns),as_points = FALSE, merge = TRUE)
   contour<-sf::as_Spatial(contour_sf)


   #contour = raster::rasterToPolygons(crowns, dissolve = TRUE)
   rm(crowns)
   colnames(contour@data)[1]<-"treeID"
   contour@data$CA<-raster::area(contour)
   contour@data$CR<-sqrt(contour@data$CA/pi)
   #contour@data$CRT<-CL/CH
   #contour@data$CFI<-CL/CW
   #contour@data$CTI<-CW/CL
   #contour@data$CSR<-CW/CH
   #contour <- contour[order(contour$treeID, contour$CA, decreasing=TRUE),]
   #contour <- contour[!duplicated(contour$treeID),]
   contour<-raster::intersect(contour,treelist_treetopsdf)
   polyCrown<-merge(contour,treelist_treetopsdf@data, by="treeID")
   polyCrown@data<-polyCrown@data[,c("x","y","Height","CA","CR","treeID")]
   polyCrown<-polyCrown[!is.na(polyCrown@data$CA),]
   treelist_treetopsdf<-merge(treelist_treetopsdf,contour@data, by="treeID")
   treelist_treetopsdf<-treelist_treetopsdf[!is.na(treelist_treetopsdf@data$CA),]
   treelist_treetopsdf@data<-treelist_treetopsdf@data[,c("x","y","Height","CA","CR","treeID")]
   treelist_treetop<-treelist_treetopsdf@data
 }

   incProgress(0.4, message = "Preparing for rendering...1%")
   Sys.sleep(0.5)

 output$hist <- renderPlot({
  if ((input$plotProfile)=="plotRipley") {
    S <- treelist_treetopsdf
    sp::proj4string(S) <- projecCHM
    SP <- as(S, "SpatialPoints")
    P  <- as(SP, "ppp")
    P <- spatstat::as.ppp(sp::coordinates(S), raster::extent(S)[])
    K <- spatstat::envelope(P, spatstat::Kest, nsim = 99, verbose = F)
    L <- spatstat::envelope(P, spatstat::Lest, nsim = 99, verbose = F)
    CE <- spatstat::clarkevans.test(P)
    par(mfrow = c(1, 3), mar = c(8, 5, 4, 3))
    plot(K, lwd=2,main="a) K",xlab = "r (m)",cex.lab=1.5)
    legend("bottomright", cex=1.2,legend=c("Clark Evans test", paste0("R=",round(CE[1][[1]],4))), bty="n")#paste0("p-value=",round(CE[2][[1]],4))), bty="n")
    plot(L, lwd=2,main="b) L",xlab = "r (m)",cex.lab=1.5)
    plot(L, . -r ~ r, ylab=expression(hat("L")), xlab = "r (m)", main="c) L", lwd=2,cex.lab=1.5)

  } else {
  par(mfrow=c(1,2), mar=c(4.5,4,2,5))
  chm_hts<-na.omit(chmR0[])
  isolate({
    chm_hts<-chm_hts[chm_hts>=Htreshoud]
  })
  dens<-density(chm_hts,adjust = 1.3, kernel = "gaussian")
  par(mfrow=c(1,3), mar=c(5,5,2,2))
  plot(dens$y,dens$x, cex.lab=2,col="black",xlab="Density",ylab="Height (m)",type="line",lwd="1",ylim=c(0,max(chm_hts*1.3)))
  polygon(dens$y,dens$x, col=input$profColor, border="black")
  legend("topright","Crown-level metrics", bty="n", text.font=2, cex=1.5)
  boxplot(chm_hts, cex.lab=2, ylim=c(0,max(chm_hts)*1.3),horizontal=F, col=input$profColor,ylab="Height (m)")
  boxplot(treelist_treetopsdf@data$CA,ylim=c(0,max(treelist_treetopsdf@data$CA)*1.3),cex.lab=2, horizontal=F, col=input$profColor,ylab="Crown Area (m2)")
   }
  },height = 360,width=850)


 isolate({
  output$downloadHist <- downloadHandler(
    filename <- function() {
      paste("CHM profile ",input$chm, Sys.Date(),'.png',sep='') },
    content <- function(file) {
      png(file, width = 800, height = 600, units = "px", pointsize = 12,
          bg = "white", res = 100)

      par(mfrow=c(1,2), mar=c(4.5,4,2,5))
      chm_hts<-na.omit(chmR0[])
      isolate({
        chm_hts<-chm_hts[chm_hts>=Htreshoud]
      })
      dens<-density(chm_hts,adjust = 1.3, kernel = "gaussian")
      par(mfrow=c(1,3), mar=c(5,5,2,2))
      plot(dens$y,dens$x, cex.lab=2,col="black",xlab="Density",ylab="Height (m)",type="line",lwd="1",ylim=c(0,max(chm_hts*1.3)))
      polygon(dens$y,dens$x, col=input$profColor, border="black")
      boxplot(chm_hts, cex.lab=2, ylim=c(0,max(chm_hts)*1.3),horizontal=F, col=input$profColor,ylab="Height (m)")
      boxplot(treelist_treetopsdf@data$CA,ylim=c(0,max(treelist_treetopsdf@data$CA)*1.3),cex.lab=2, horizontal=F, col=input$profColor,ylab="Crown Area (m2)")
      dev.off()},
    contentType = 'image/png'
  )})

 incProgress(0.4, message = "Preparing for rendering...20%")
 Sys.sleep(0.5)

# download Ripley's K and L figure
 isolate({
  output$downloadRipley <- downloadHandler(
    filename <- function() {
      paste("Ripley's_K_L",input$chm, Sys.Date(),'.png',sep='')},
    content <- function(file) {
      png(file, width = 1200, height = 600, units = "px", pointsize = 12,
          bg = "white", res = 100)
      S <- treelist_treetopsdf
      sp::proj4string(S) <- projecCHM
      SP <- as(S, "SpatialPoints")
      P  <- as(SP, "ppp")
      P <- spatstat::as.ppp(sp::coordinates(S), raster::extent(S)[])
      K <- spatstat::envelope(P, spatstat::Kest, nsim = 99, verbose = F)
      L <- spatstat::envelope(P, spatstat::Lest, nsim = 99, verbose = F)
      CE<-clarkevans.test(P,nsim = 99)
      par(mfrow = c(1, 3), mar = c(8, 5, 4, 3))
      plot(K, lwd=2,main="a) K",xlab = "r (m)",cex.lab=1.5)
      legend("bottomright", cex=1.2,legend=c("Clark Evans test", paste0("R=",round(CE[1][[1]],4))), bty="n")#,paste0("p-value=",round(CE[2][[1]],4))), bty="n")
      plot(L, lwd=2,main="b) L",xlab = "r (m)",cex.lab=1.5)
      plot(L, . -r ~ r, ylab=expression(hat("L")), xlab = "r (m)", main="c) L", lwd=2,cex.lab=1.5)
            dev.off()},
    contentType = 'image/png'
  )})

 incProgress(0.5, message = "Preparing for rendering...30%")
 Sys.sleep(0.5)

 isolate({
  output$downloadCHM2d <- downloadHandler(
    filename <- function() {
      paste("Lorenz_curve",input$chm, Sys.Date(),'.png',sep='')},
    content <- function(file) {
      png(file, width = 700, height = 600, units = "px", pointsize = 12,
          bg = "white", res = 100)

      size <- treelist_treetopsdf@data$Height
      L.mean <-  max(cumsum(sort(size[size>=mean(size)],T)/sum(size)))
      GC <- GiniCoeff(size)
      par(mar=c(4,4,2,2))
      plot(c(0,1), c(0,1), type="l", col="grey", ylim=(0:1), xlim=c(0,1), lty=1, lwd=3,xlab="", ylab="", axes=T)
      title(ylab=expression(paste("Accummulated proportion of tree heights (", italic(H), " ; m)" )),
            xlab=expression(paste("Accummulated proportion of number of trees ")),
            line=2.5,cex.lab=1)
      polygon(c(0,seq(0,1,length.out=1000)),
              c(0,cumsum(seq(100,2,length.out=1000)/sum(seq(100,2,length.out=1000)))),
              col="grey", lty=0)
      lines(c(.5,0),c(.5,1),lty=3,lwd=4,col="grey"); lines(c(.5,.4),c(.5,.6),lty=3,lwd=4,col="white")
      lines(seq(0,1,length.out=length(size)+1),
            c(0,cumsum(sort(size,T)/sum(size))),
            lty=1,lwd=2)
      points(length(size[size>=mean(size)])/length(size),
             L.mean,
             pch=10,cex=2.5,lwd=2)
      legend("bottomright", c("Lorenz curve",
                              expression(paste("mean ", italic(H), " (inflexion point)")),
                              "axis of symmetry",
                              "maximum entropy and",
                              "absolute equality lines"),
             col=c("black","black","grey",NA,NA),
             pch=c(NA,10,NA,NA,NA),
             lty=c("solid",NA,"dotted",NA,NA),
             lwd=c(2,2,3,NA,NA),
             pt.cex = c(NA,2,NA,NA,NA),
             fill=c(NA, NA, NA, "gray",NA),
             border=c("white", "white",NA, "gray",NA),
             x.intersp=c(.4,.4,.4,.001,.001),
             box.lwd = NA,
             bg="transparent")
      text(.5,.4, "Gini coefficient = ", pos = 4); text(.75,.4,format(GC,digits=2), pos = 4 )
      text(.5,.35, "Proportion above the mean = ", pos = 4); text(.9,.35,format(L.mean,digits=2), pos = 4 )
       dev.off()
      },
    contentType = 'image/png'
  )})

 incProgress(0.6, message = "Preparing for rendering...40%")
 Sys.sleep(0.5)

 # plot CHM 2D or lorenzCurve
 isolate({
 output$CHMplot2D <- renderPlot({
  if ((input$plotCHM2d)=="plotlorenzcurve") {
     size <- treelist_treetopsdf@data$Height
    L.mean <-  max(cumsum(sort(size[size>=mean(size)],T)/sum(size)))
    GC <- GiniCoeff(size)

    par(mar=c(6,4,2,2))
    plot(c(0,1), c(0,1), type="l", col="grey", ylim=(0:1), xlim=c(0,1), lty=1, lwd=3,xlab="", ylab="")
    title(ylab=expression(paste("Accummulated proportion of tree heights (", italic(H), " ; m)" )),
          xlab=expression(paste("Accummulated proportion of number of trees ")),
          line=2.5,cex.lab=1.5)
    polygon(c(0,seq(0,1,length.out=1000)),
            c(0,cumsum(seq(100,2,length.out=1000)/sum(seq(100,2,length.out=1000)))),
            col="grey", lty=0)
    lines(c(.5,0),c(.5,1),lty=3,lwd=4,col="grey"); lines(c(.5,.4),c(.5,.6),lty=3,lwd=4,col="white")
    lines(seq(0,1,length.out=length(size)+1),
          c(0,cumsum(sort(size,T)/sum(size))),
          lty=1,lwd=2)
    points(length(size[size>=mean(size)])/length(size),
           L.mean,
           pch=10,cex=2.5,lwd=2)
    legend("bottomright", c("Lorenz curve",
                            expression(paste("mean ", italic(H), " (inflexion point)")),
                            "axis of symmetry",
                            "maximum entropy and",
                            "absolute equality lines"),
                            col=c("black","black","grey",NA,NA),
                            pch=c(NA,10,NA,NA,NA),
                            lty=c("solid",NA,"dotted",NA,NA),
                            lwd=c(2,2,3,NA,NA),
                            pt.cex = c(NA,2,NA,NA,NA),
                            fill=c(NA, NA, NA, "gray",NA),
                            border=c("white", "white",NA, "gray",NA),
                            x.intersp=c(.4,.4,.4,.001,.001),
                            box.lwd = NA,
                            bg="transparent")
    text(.5,.4, "Gini coefficient = ", pos = 4); text(.75,.4,format(GC,digits=2), pos = 4 )
    text(.5,.35, "Proportion above the mean = ", pos = 4); text(.9,.35,format(L.mean,digits=2), pos = 4 )
       } else {

  colS<-input$Pallet

  myColorRamp <- function(colors, values) {
    v <- (values - min(values))/diff(range(values))
    x <- colorRamp(colors)(v)
    head(x)
    rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
  }

  if ( colS =="BlGrRed") {col.rev <- myColorRamp(c("blue","green","yellow","red"),0:255)}
  if ( colS =="Viridis") {col.rev <- myColorRamp(c("#440154FF","#482878FF","#3E4A89FF",
                                                   "#31688EFF","#26828EFF","#1F9E89FF",
                                                   "#35B779FF","#6DCD59FF","#B4DE2CFF",
                                                   "#FDE725FF"),0:255)}

  if ( !colS =="BlGrRed" & !colS =="Viridis") {
  col<-RColorBrewer::brewer.pal(9,colS)
  col.rev<-rev(col)
  }
  tree.xy<-data.frame(tree[,1:2])
  tree.xy <- data.frame(na.omit(tree.xy))
  plot(chmR0,col=col.rev,axes = T, xlab="",ylab="",useRaster=F)
  points(tree.xy, pch=16, cex = 1.5, col=input$TopColor,  type = "p")
  plot(polyCrown, add=T, border=input$CrownColor, lwd=2)
   }
  },height = 600,width=600)
})

 incProgress(0.7, message = "Preparing for rendering...50%")
 Sys.sleep(0.5)

 # download CHM 2d
 isolate({
 output$downloadCHMprint <- downloadHandler(
  filename <- function() {
    paste("CHM",input$chm, Sys.Date(),'.png',sep='') },
  content <- function(file) {
    png(file, width = 600, height = 600, units = "px", pointsize = 12,
        bg = "white", res = 100)

    colS<-input$Pallet
    myColorRamp <- function(colors, values) {
      v <- (values - min(values))/diff(range(values))
      x <- colorRamp(colors)(v)
      head(x)
      rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
    }

    if ( colS =="BlGrRed") {col.rev <- myColorRamp(c("blue","green","yellow","red"),0:255)}
    if ( colS =="Viridis") {col.rev <- myColorRamp(c("#440154FF","#482878FF","#3E4A89FF",
                                                     "#31688EFF","#26828EFF","#1F9E89FF",
                                                     "#35B779FF","#6DCD59FF","#B4DE2CFF",
                                                     "#FDE725FF"),0:255)}

    if ( !colS =="BlGrRed" & !colS =="Viridis") {
      col<-RColorBrewer::brewer.pal(9,colS)
      col.rev<-rev(col)
    }
    tree.xy<-data.frame(tree[,1:2])
    tree.xy <- data.frame(na.omit(tree.xy))
    plot(chmR0,col=col.rev,axes = T, xlab="UTM Easting",ylab="UTM Northing")
    points(tree.xy, pch=16, cex = 1.5, col=input$TopColor,  type = "p")
    plot(polyCrown, add=T, border=input$CrownColor, lwd=2)
  dev.off()},
  contentType = 'image/png'
 )})

 if ((input$radiustype)=="FR") {
  output$downloadShpR <- renderUI({div(style="margin-left:0px;margin-top: 0px;width:300px",downloadButton('downloadShp', 'Tree Crown (.shp)')) })
  createShp <- reactive({
    if (is.null(treelist_treetop)){
      return(NULL)
    } else {

      SHP<-polyCrown
      return(SHP)
    }
  })

 } else {
 output$downloadShpR <- renderUI({div(style="margin-left:0px;margin-top: 0px;width:300px",downloadButton('downloadShp', 'Tree Crown (.shp)')) })
 createShp <- reactive({
  if (is.null(treelist_treetop)){
    return(NULL)
  } else {
    SHP<-polyCrown
    proj4string(SHP) <- projecCHM
    return(SHP)
  }
 })
 }

 incProgress(0.8, message = "Preparing for rendering...70%")
 Sys.sleep(0.5)

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
  div(style="margin-left:160px;margin-top: -33px;width:300px",
  downloadButton('downloadShpXY', 'Tree Location (.shp)')) })

 createShpXY <- reactive({
  if (is.null(treelist_treetopsdf@data)){
    return(NULL)
  } else {
   proj4string(treelist_treetopsdf) <- projecCHM
    return(treelist_treetopsdf)
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

 incProgress(0.9, message = "Preparing for rendering...90%")
 Sys.sleep(0.5)

 isolate({
   output$PLOT3D <- rgl::renderRglwidget({

     while (rgl::rgl.cur() > 0) { try(rgl::rgl.close())}

     if ((input$plot3Dradio)=="plotCHM3D") {
       colS<-input$Pallet
       myColorRamp <- function(colors, values) {
         v <- (values - min(values))/diff(range(values))
         x <- colorRamp(colors)(v)
         rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
       }

       if ( colS =="BlGrRed") {myPal <- myColorRamp(c("blue","green","yellow","red"),0:255)}
       if ( colS =="Viridis") {myPal <- myColorRamp(c("#440154FF","#482878FF","#3E4A89FF",
                                                        "#31688EFF","#26828EFF","#1F9E89FF",
                                                        "#35B779FF","#6DCD59FF","#B4DE2CFF",
                                                        "#FDE725FF"),0:255)}

       if ( !colS =="BlGrRed" & !colS =="Viridis") {
         col<-RColorBrewer::brewer.pal(9,colS)
         myPal<-rev(col)
       }

       while (rgl::rgl.cur() > 0) { try(rgl::rgl.close()) }

       rasterVis::plot3D(chmR0,col=myPal,xlab="",ylab="",zlab="Height (m)")
       #CHM3Dvis1<-CHM3Dvis(chm=chmR0,colR=myPal,xlab="",ylab="",zlab="Height (m)")
       #rm(CHM3Dvis1)
       rgl::axes3d(c("x-", "y-"), col="black")
       rgl::title3d(xlab = "UTM Easting", ylab = "UTM Northing")#, col="green")
       #rgl::aspect3d(1,1,0.5)
       rgl::rglwidget()

     } else {

           n<-nrow(treelist_treetopsdf@data)
           xl<-max(treelist_treetopsdf@data[,1])-treelist_treetopsdf@data[,1]
           yl<-max(treelist_treetopsdf@data[,2])-treelist_treetopsdf@data[,2]

           CBHp<-treelist_treetopsdf@data$Height*0.4
           CL<-treelist_treetopsdf@data$Height - CBHp

             if (input$plotSurface=="solid") { shape=2;}
             if (input$plotSurface=="mesh") { shape=1; }
             if (input$plotSurface=="lines") { shape=3}

           if (nrow(treelist_treetopsdf@data)>2000){

             detail2<-'Note: Number of trees is higher than 2000. This may take several minutes. It is recommended to use the line objects instead.'
           } else {
             detail2<-'This may take a few minutes......'

           }

           while (rgl::rgl.cur() > 0) { while (rgl::rgl.cur() > 0) { try(rgl::rgl.close()) } }
           withProgress(message = paste('Rendering',nrow(treelist_treetopsdf@data),"trees in 3D."), value = 0.1,detail = detail2, {
             Sys.sleep(10)

           for(i in 1:n) {

             rgl::bg3d(col = "white")
             vec=rbind(c(xl[i],yl[i], 0), c(xl[i], yl[i],treelist_treetopsdf@data$Height[i]))
             rgl::segments3d(vec, col=" brown",size=2)#, lwd=2)


             if (treelist_treetopsdf@data$Height[i]<=2){crowncolor="green"}
             if (treelist_treetopsdf@data$Height[i]> 2 & treelist_treetopsdf@data$Height[i]<=7){crowncolor="green3"}
             if (treelist_treetopsdf@data$Height[i]>7){crowncolor="darkgreen"}

             ptree<-TreesModel(crownshape = input$plotShape, nz=15, nalpha=15, CL = CL[i], CR =treelist_treetopsdf@data$CR[i],
                               HCB = CBHp[i], x0 =xl[i], y0 = yl[i], crowncolor = crowncolor, shape=shape)

             #browser()
             rm(ptree)
             incProgress(0.1, detail = paste("Ploting tree n:", i))

           }
           rgl::axes3d(c("x-", "y-"), col="black")
           rgl::title3d(xlab = "UTM Easting", ylab = "UTM Northing")#, col="forestgreen")
           rgl::planes3d(a=0,b=0,c=-1,d=0.0001,color="gray",alpha=0.4)
           #rgl::aspect3d(1,1,0.3)
           rgl::rglwidget()
         })
       }



   })

 })


 incProgress(0.9, message = "Preparing for rendering...99%")
 Sys.sleep(0.5)

 output$TreelistR <- renderUI({
  div(style="margin-left:332px;margin-top: -33px;width:300px",
  downloadButton('downloadTreesLoc', 'Tree location (.csv) '))
 })

 output$Profile <- renderUI({
  if ((input$plotProfile)=="plotRipley") {
    div(style = "margin-top: -70px;margin-left:50px",
        downloadButton('downloadRipley', "Download Ripley's K and L"))
  } else {
  div(style = "margin-top: -70px;margin-left:390px",
  downloadButton('downloadTable', 'Download LiDAR metrics'),
  downloadButton('downloadHist', 'Download CHM Profile'))
 }})


 output$outCHMplot2D <- renderUI({
  if ((input$plotCHM2d)=="plotlorenzcurve") {
    div(style = "margin-top: 165px;margin-left:450px",
        downloadButton('downloadCHM2d', "Download Lorenz Curve"))
  } else {
    div(style = "margin-top: 160px;margin-left:400px",
        downloadButton('downloadCHMprint', 'Download CHM'))
  }})

 isolate ({
  output$downloadTreesLoc <- downloadHandler(
    filename = function() {
      paste("Trees",input$chm, Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      tree.csv<-treelist_treetop
      write.csv(tree.csv, file, row.names=FALSE)}

  ) })

 isolate ({
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("LiDAR Crown-level metrics",input$chm, Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(LiDARsummary, file, row.names=FALSE)}

  ) })

 isolate ({
  NameExp<-c("Number of trees","Hmax","Hmean","Hmin","Hmedian","Hvar","Hsd","Hcv","Hkurtosis","Hskewness")
  MetricsExp<-c(length(treelist_treetopsdf@data$Height),
                round(max(treelist_treetopsdf@data$Height),digits=2),
                round(mean(treelist_treetopsdf@data$Height),digits=2),
                round(min(treelist_treetopsdf@data$Height),digits=2),
                round(median(treelist_treetopsdf@data$Height),digits=2),
                round(var(treelist_treetopsdf@data$Height),digits=2),
                round(sd(treelist_treetopsdf@data$Height),digits=2),
                round(cv(treelist_treetopsdf@data$Height),digits=2),
                round(kurtosis(treelist_treetopsdf@data$Height),digits=2),
                round(skewness(treelist_treetopsdf@data$Height),digits=2))

  LiDARsummary<-data.frame(cbind(NameExp,MetricsExp))
  colnames(LiDARsummary)<-c("Parameters", "Value")
  incProgress(0.99, message = "It is almost done. Stay there...!")
  Sys.sleep(1.5)
  LiDARsummary

 })
 })
#    },
#  error = function(e){
#  withProgress(message = 'An error occurred. The app will be reloaded shortly', value = 0.1,detail = '', {Sys.sleep(10)})
#    session$reload()
#  })

 })
################################################################################

})

################################################################################
