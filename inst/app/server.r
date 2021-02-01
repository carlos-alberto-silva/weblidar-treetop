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
require(rgl)
require(shiny)
require(rglwidget)
require(lidR)
require(rmarkdown)
require(pryr)
################################################################################
################################################################################

################################################################################
# server.r
options(shiny.maxRequestSize= 15*1024^2)
options(shiny.deprecation.messages=FALSE)

shinyServer(function(input, output, session) {


  interpol<- function(input,col) {
    surf.3d <- t(convhulln(input,options = "QJ"))
    rgl.triangles(input[surf.3d,1],input[surf.3d,2],input[surf.3d,3],col=col,alpha = 1,
                  lit = TRUE,ambient = "black",specular = "white",emission = "black",shininess = 50.0,
                  smooth = TRUE, texture = NULL,front = "fill",back ="fill",fog = F, box=F,axes = FALSE)}


   TreesModel<- function(crownshape=c("cone","ellipsoid","halfellipsoid","paraboloid","cylinder"),
                        nz=15, nalpha=15, CL=5, CW=5, HCB=10, x0=0, y0=0, z0=0, dbh = 0.3, crowncolor = "forestgreen",
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
    r <- CW/2
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
    lines3d(newList, col=crowncolor, add=T,box=F)
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
	if (Ht<=5){col=sample(c("green3","green3","darkgreen"),nrow(NewList), TRUE)}
	if (Ht>5){col="darkgreen"}

	lines3d(NewList, col=col, add=T)
	}
 }



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

####################################

output$summary <- renderTable({

  observeEvent(input$refresh, {
    if (input$refresh==1){
      session$reload()
  }
    })


 # tryCatch(
#    expr = {

  #    if (input$action_button == 1) {
   #     session$reload()}

      if(exists("chmR")){rm(chmR)}
      if(exists("chmR0")){rm(chmR0)}
      if(exists("decTREE")){rm(decTREE)}
      if(exists("r")){rm(r)}
      if(exists("treeMer")){rm(treeMer)}
      if(exists("exst")){rm(exst)}
      if(exists("tree")){rm(tree)}
      if(exists("temp")){rm(temp)}

  output$pageviews <-  renderText({
    if (!file.exists("pageviews.Rdata")) pageviews <- 0 else load(file="pageviews.Rdata")
    pageviews <- pageviews + 1
    save(pageviews,file="pageviews.Rdata")
    paste("Number of Visits:",pageviews)
  })


  if ((input$Mydata)=="ED") {

    chmR<- raster(system.file('extdata', 'Eglin_plot1.asc', package='treetop'))

  chmR0<-chmR
  projecCHM<-projection(chmR)
  detail<-""
  } else {


 inFile<-input$chm
 if (is.null(inFile)){
 return(NULL)}


 chmR<-raster(inFile$datapath)
 projecCHM<-projection(chmR)

 reschmR<-res(chmR)[1]
 #newst<-extent(chmR)


 #r1NaM <- is.na(as.matrix(chmR))
 #colNotNA <- which(colSums(r1NaM) != nrow(chmR))
 #rowNotNA <- which(rowSums(r1NaM) != ncol(chmR))

 #exst <- extent(chmR, rowNotNA[1], rowNotNA[length(rowNotNA)],
#                   colNotNA[1], colNotNA[length(colNotNA)])

 #chmR<-crop(chmR,exst)

 #if (reschmR<0.25){
 #  validate(
 #    need(reschmR==1, 'The spatial resolution of the CHM must to be 1m'))
  #stop()
 #  withProgress(message = "The grid cell size of the CHM file should be equal or higher then 1m", value = 0.1,detail = "The app will be restarted", {
 #    Sys.sleep(10)
 #  })
 #  session$reload()
 #}

 #plotlength<-86.60254

 area_ha <- (ncell(chmR)*reschmR^2)/10000

 if (area_ha > 3){
     withProgress(message = "Note: the study area is larger then 2ha. Please check the file extent or upload a smaller CHM file.", value = 0.1,detail = "The app will be restarted in a few seconds!", {
       Sys.sleep(10)
     })
     session$reload()

   #exst<-extent(chmR)
  #newst<-extent(mean(exst[1:2])-plotlength,mean(exst[1:2])+plotlength,mean(exst[3:4])-plotlength,mean(exst[3:4])+plotlength)
  #chmR<-crop(chmR,newst)

  #if (reschmR<0.5){
  #detail<-paste0("Note: the study area is larger then 3ha. We have resampled the file extent to 3ha using xmin: ",newst[1],"; xmax: ",newst[2],
  #"; ymin: ",newst[3],"; ymax: ",newst[4]," extent. Also, the grid cell size of the CHM file is smaller then 0.5m. We have resampled it to 0.5m.")
  #} else {
  #  detail<-paste0("Note: the study area is larger then 3ha. We have resampled the file extent to 3ha using xmin: ",newst[1],"; xmax: ",newst[2],
  #                 "; ymin: ",newst[3],"; ymax: ",newst[4]," extent.")}
  } else {

    if (reschmR<0.5){
    detail<-paste0("Please note: The the grid cell size of the CHM file is smaller then 0.5m. We have resampled it to 0.5m.")
    } else {detail<-""}
  }

 if (reschmR<0.5){
    #  validate(
    #    need(reschmR==1, 'The spatial resolution of the CHM must to be 1m'))
    #stop()
    #withProgress(message = "The grid cell size of the CHM file is smaller then 1m", value = 0.1,detail = ". It has been resampled to 1m now.", {
    #  Sys.sleep(10)
    #})

    rnull <- raster()
    extent(rnull) <- round(newst)
    res(rnull) <-0.5
    rnull2 <- setExtent(rnull, round(newst))
    chmR <- resample(chmR, rnull2, method='ngb')
    projection(chmR) <-projecCHM

 }
 chmR0<-chmR
 }


 isolate({
  if (input$filter==TRUE) {
    if (input$filtertype=="Mean") {
      if ( input$wsf=="3x3"){
        fws=3 }
      if ( input$wsf=="5x5"){
        fws=5 }
      if ( input$wsf=="7x7"){
        fws=7 }
      if ( input$wsf=="9x9"){
        fws=9 }
      if ( input$wsf=="11x11"){
        fws=11 }
      if ( input$wsf=="13x13"){
        fws=13 }
      if ( input$wsf=="15x15"){
        fws=15 }
      if ( input$wsf=="17x17"){
        fws=17 }
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
      if ( input$wsf=="9x9"){
        fws=9 }
      if ( input$wsf=="11x11"){
        fws=11 }
      if ( input$wsf=="13x13"){
        fws=13 }
      if ( input$wsf=="15x15"){
        fws=15 }
      if ( input$wsf=="17x17"){
        fws=17 }
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
 #withProgress(message = paste('LiDAR data processing.This may take a few seconds','The memory used is',round(mem_used()/1024^2), "Mb."), value = 0.1,detail = detail, {
 withProgress(message = 'LiDAR data processing. This may take a few seconds!', value = 0.1,detail = detail, {

    Sys.sleep(10)

 isolate({
  # tree dectetion by local maximum - FWS

  isolate({
    if ((input$HTtypeI)=="slidebar") {
      Htreshoud<-input$HTsliderI

    } else {Htreshoud<-input$HTboxI}
  })

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
  if ( input$ws=="15x15"){
    fws=15 }
  if ( input$ws=="17x17"){
    fws=17 }
  decTREE <<- lidR::tree_detection(chmR, lidR::lmf(ws=fws))
  treeMer<-cbind(as.data.frame(decTREE@coords),as.numeric(paste0(decTREE@data[,2])))
  colnames(treeMer)<-c("x","y","z")
   tree<-subset(treeMer, treeMer$z >=Htreshoud)
  colnames(tree)<-c("x","y","z")
  if(exists("decTREE")){rm(decTREE)}
 })


temp<-tree

 if ((input$radiustype)=="VR") {
 isolate({
    Ang<-as.numeric(input$Ang)
    ht1<-as.numeric(input$ht1)
    ht2<-as.numeric(input$ht2)
    ht3<-as.numeric(input$ht3)
    width<-as.numeric(input$frv)
    # equation from Popescu and Wynne
    # http://asprs.org/a/publications/pers/2004journal/may/2004_may_589-604.pdf

    if ((input$radiustype)=="VR") {
      if ((input$equation)=="DC") {temp[,4] <- (3.09632 + 0.00895*(temp[,3]*temp[,3]))}
      if ((input$equation)=="PI") {temp[,4] <- (3.75105 + 0.17919*(temp[,3]) + 0.01241*(temp[,3]*(temp[,3])))}
      if ((input$equation)=="CB") {temp[,4]<-(2.51503+0.00901*(temp[,3]*temp[,3]))}
      if ((input$equation)=="YR") {temp[,4]<-Ang+ht1*temp[,3]+ht2*(temp[,3]*temp[,3]) + ht3*(temp[,3]*temp[,3]*temp[,3])}
    }
    temp[,5]<-pi*(temp[,4]/2)^2
  })
 }

 if ((input$radiustype)=="FR") {
  tempsdf<-SpatialPointsDataFrame(temp[,1:2],temp)
  tempsdf@data$treeID<-1:nrow(temp)
  crowns=silva2016(chmR0, tempsdf, max_cr_factor = input$maxcrown, exclusion = input$exclusion,
                         ID = "treeID")()
  contour = rasterToPolygons(crowns, dissolve = TRUE)
  rm(crowns)
  colnames(contour@data)<-"treeID"
  contour@data$CA<-raster::area(contour)
  polyCrown<-merge(contour,tempsdf@data, by="treeID")
  pspdf<-merge(tempsdf,contour@data, by="treeID")
  temp<-data.frame(cbind(coordinates(pspdf),pspdf@data$z,
                         sqrt(as.numeric(paste0(pspdf@data$CA/pi)))*2,pspdf@data$CA))
 }

 temp<-data.frame(temp)
 colnames(temp)<-c("x","y","Height","CW","CA")

 output$hist <- renderPlot({
  if ((input$plotProfile)=="plotRipley") {
    S <- SpatialPointsDataFrame(coords= cbind(newDataTree[,1],newDataTree[,2]), data = newDataTree)
    proj4string(S) <- projecCHM
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
  chmR0.df<-as.data.frame(chmR0)
  isolate({
    chmR0.df<-subset(chmR0.df,chmR0.df[,1]>=Htreshoud)
  })
  dens<-density(chmR0.df[,1],adjust = 1.3, kernel = "gaussian")
  par(mfrow=c(1,3), mar=c(5,5,2,2))
  plot(dens$y,dens$x, cex.lab=2,col="black",xlab="Density",ylab="Height (m)",type="line",lwd="1",ylim=c(0,max(chmR0.df[,1]*1.3)))
  polygon(dens$y,dens$x, col=input$profColor, border="black")
  boxplot(chmR0.df[,1], cex.lab=2, ylim=c(0,max(chmR0.df[,1])*1.3),horizontal=F, col=input$profColor,ylab="Height (m)")
  ar<-pi*(temp[,4]/2)^2
  boxplot(ar,ylim=c(0,max(ar)*1.3),cex.lab=2, horizontal=F, col=input$profColor,ylab="Crown Area (m2)")
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
      chmR0.df<-as.data.frame(chmR0)
      isolate({
      chmR0.df<-subset(chmR0.df,chmR0.df[,1]>=Htreshoud)})
      dens<-density(chmR0.df[,1],adjust = 1.3, kernel = "gaussian")
      par(mfrow=c(1,3), mar=c(5,5,2,2))
      plot(dens$y,dens$x, cex.lab=2,col="black",xlab="Density",ylab="Height (m)",type="line",lwd="1",ylim=c(0,max(chmR0.df[,1]*1.3)))
      polygon(dens$y,dens$x, col=input$profColor, border="black")
      boxplot(chmR0.df[,1], cex.lab=2, ylim=c(0,max(chmR0.df[,1])*1.3),horizontal=F, col=input$profColor,ylab="Height (m)")
      ar<-pi*(temp[,4]/2)^2
      boxplot(ar,ylim=c(0,max(ar)*1.3),cex.lab=2, horizontal=F, col=input$profColor,ylab="Crown Area (m2)")
      dev.off()},
    contentType = 'image/png'
  )})


# download Ripley's K and L figure
 isolate({
  output$downloadRipley <- downloadHandler(
    filename <- function() {
      paste("Ripley's_K_L",input$chm, Sys.Date(),'.png',sep='')},
    content <- function(file) {
      png(file, width = 1200, height = 600, units = "px", pointsize = 12,
          bg = "white", res = 100)

      S <- SpatialPointsDataFrame(coords= cbind(newDataTree[,1],newDataTree[,2]), data = newDataTree)
      proj4string(S) <- projecCHM
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


 isolate({
  output$downloadCHM2d <- downloadHandler(
    filename <- function() {
      paste("Lorenz_curve",input$chm, Sys.Date(),'.png',sep='')},
    content <- function(file) {
      png(file, width = 700, height = 600, units = "px", pointsize = 12,
          bg = "white", res = 100)

      size <- newDataTree$Height
      L.mean <-  max(cumsum(sort(size[size>=mean(size)],T)/sum(size)))
      GC <- GiniCoeff(size)
      par(mar=c(8,4,2,10))
      plot(c(0,1), c(0,1), type="l", col="grey", ylim=(0:1), xlim=c(0,1), lty=1, lwd=3,xlab="", ylab="", axes=FALSE)
      title(ylab=expression(paste("Accummulated proportion of tree heights (", italic(H), " ; m)" )),
            xlab=expression(paste("Accummulated proportion of number of trees ")),
            line=2.5,cex.lab=1)
      polygon(c(0,seq(0,1,length.out=1000)),
              c(0,cumsum(seq(100,2,length.out=1000)/sum(seq(100,2,length.out=1000)))),
              col="grey", lty=0)
      lines(c(.5,0),c(.5,1),lty=3,lwd=4,col="grey"); lines(c(.5,.4),c(.5,.6),lty=3,lwd=4,col="white")
      lines(c(0,seq(0,1,length.out=length(size))),
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

 # plot CHM 2D or lorenzCurve
 isolate({
 output$CHMplot2D <- renderPlot({
  if ((input$plotCHM2d)=="plotlorenzcurve") {
     size <- newDataTree$Height
    L.mean <-  max(cumsum(sort(size[size>=mean(size)],T)/sum(size)))
    GC <- GiniCoeff(size)

    par(mar=c(8,4,2,10))
    plot(c(0,1), c(0,1), type="l", col="grey", ylim=(0:1), xlim=c(0,1), lty=1, lwd=3,xlab="", ylab="", box=FALSE)
    title(ylab=expression(paste("Accummulated proportion of tree heights (", italic(H), " ; m)" )),
          xlab=expression(paste("Accummulated proportion of number of trees ")),
          line=2.5,cex.lab=1)
    polygon(c(0,seq(0,1,length.out=1000)),
            c(0,cumsum(seq(100,2,length.out=1000)/sum(seq(100,2,length.out=1000)))),
            col="grey", lty=0)
    lines(c(.5,0),c(.5,1),lty=3,lwd=4,col="grey"); lines(c(.5,.4),c(.5,.6),lty=3,lwd=4,col="white")
    lines(c(0,seq(0,1,length.out=length(size))),
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

  chmR0.df<-as.data.frame(chmR0)
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
  plot(chmR0,col=col.rev,axes = T, xlab="",ylab="",useRaster=F)
  points(tree.xy, pch=16, cex = 1.5, col=input$TopColor,  type = "p")
  if ((input$radiustype)=="FR") {
    plot(contour, add=T, border=input$CrownColor, lwd=2)
     } else {
  for ( i in 1:length(temp[,1])) {
    col<-c(1:length(temp[,1]))
    draw.circle(x=temp[i,1],y=temp[i,2],(temp[i,4])/2,border=input$CrownColor,lty=1,lwd=1)
  }}}
  },height = 600,width=600)
})


 # download CHM 2d
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
    points(tree.xy, pch=16, cex = 1.5, col=input$TopColor,  type = "p")
    if ((input$radiustype)=="FR") {
      plot(contour, add=T, border=input$CrownColor, lwd=2)

    } else {
      for ( i in 1:length(temp[,1])) {
        col<-c(1:length(temp[,1]))
        draw.circle(x=temp[i,1],y=temp[i,2],(temp[i,4])/2,border=input$CrownColor,lty=1,lwd=1)
      }}
  dev.off()},
  contentType = 'image/png'
 )})

  newDataTree<-temp

 if ((input$radiustype)=="FR") {
  output$downloadShpR <- renderUI({div(style="margin-left:0px;margin-top: 0px;width:300px",downloadButton('downloadShp', 'Tree Crown (.shp)')) })
  createShp <- reactive({
    myXY<-temp
    if (is.null(myXY)){
      return(NULL)
    } else {

      SHP<-polyCrown
      SHP@data$CW<-sqrt(SHP@data$CA/pi)*2
      #SHP@data$CA<-pi*(SHP@data$CW/2)^2
      #print(SHP@data)
      return(SHP)
    }
  })

 } else {
 output$downloadShpR <- renderUI({div(style="margin-left:0px;margin-top: 0px;width:300px",downloadButton('downloadShp', 'Tree Crown (.shp)')) })
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

    proj4string(SHP) <- projecCHM
    SHP$Height<-newDataTree$Height
    SHP$CW<-newDataTree$CW
    SHP$CA<-newDataTree$CA
    return(SHP)
  }
 })
 }

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
  if (is.null(newDataTree)){
    return(NULL)
  } else {

 lots <- SpatialPointsDataFrame(coords= cbind(newDataTree[,1],newDataTree[,2],
    newDataTree$Height, newDataTree$CW,newDataTree$CA), data = newDataTree)
    proj4string(lots) <- projecCHM
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

     while (rgl.cur() > 0) { try(rgl.close())}

     if ((input$plot3Dradio)=="plotCHM3D") {
       colS<-input$Pallet
       if (colS =="BlGrRed") {
         myPal<-c("blue","green","yellow","red")
       } else {
         myPal <- rev(brewer.pal(9, colS))
       }
       while (rgl.cur() > 0) { try(rgl.close()) }

       CHM3Dvis1<-CHM3Dvis(chm=chmR0,colR=myPal,xlab="",ylab="",zlab="Height (m)")
       rm(CHM3Dvis1)
       axes3d(c("x-", "y-"), col="black")
       title3d(xlab = "UTM Easting", ylab = "UTM Northing")#, col="green")
       aspect3d(1,1,0.5)
       rglwidget()

     } else {


           xl<-max(tree[,1])-tree[,1]
           yl<-max(tree[,2])-tree[,2]

           CBHp<-tree[,3]*0.4
           CL<-tree[,3] - CBHp
           CW<-(temp[,4])/2


           if (nrow(tree) > 150) {

             if (input$plotSurface=="solid") { shape=3
             detail2<-'Note: The maximum number of trees allowed to plot in 3D using a solid surface is 150. The trees will renderized using the line-derived surface.'}
             if (input$plotSurface=="mesh") { shape=1; detail2<-''}
             if (input$plotSurface=="lines") { shape=3;detail2<-''}

           } else {

             if (input$plotSurface=="solid") { shape=2;detail2<-''}
             if (input$plotSurface=="mesh") { shape=1; detail2<-''}
             if (input$plotSurface=="lines") { shape=3;detail2<-''}

           }

           while (rgl.cur() > 0) { while (rgl.cur() > 0) { try(rgl.close()) } }
           withProgress(message = paste('Rendering',nrow(temp),"trees in 3D.",detail2), value = 0.1,detail = 'This may take a few seconds......', {
             Sys.sleep(10)

           for(i in 1:nrow(tree)) {
             bg3d(col = "white")
             ptree<-TreesModel(crownshape = input$plotShape, nz=15, nalpha=15, CL = CL[i], CW =CW[i]*2,
                               HCB = CBHp[i], x0 =xl[i], y0 = yl[i], crowncolor = "forestgreen", shape=shape)

             rm(ptree)
             incProgress(0.1, detail = paste("Ploting tree n:", i))
             vec=rbind(c(xl[i],yl[i], 0 ), c(xl[i], yl[i],tree[i,3]))
             segments3d(vec, col=" brown", lwd=2 )

           }
           axes3d(c("x-", "y-"), col="black")
           title3d(xlab = "UTM Easting", ylab = "UTM Northing")#, col="forestgreen")
           planes3d(a=0,b=0,c=-1,d=0.0001,color="gray",alpha=0.4)
           aspect3d(1,1,0.3)
           rglwidget()
         })
       }



   })

 })



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
    div(style = "margin-top: 150px;margin-left:400px",
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

 isolate ({
  Hexp<-as.numeric(paste0(temp[,3]))
  NameExp<-c("Number of trees","Hmax","Hmean","Hmin","Hmedian","Hvar","Hsd","Hcv","Hkurtosis","Hskewness")
  MetricsExp<-c(length(Hexp),
                round(max(Hexp),digits=2),
                round(mean(Hexp),digits=2),
                round(min(Hexp),digits=2),
                round(median(Hexp),digits=2),
                round(var(Hexp),digits=2),
                round(sd(Hexp),digits=2),
                round(cv(Hexp),digits=2),
                round(kurtosis(Hexp),digits=2),
                round(skewness(Hexp),digits=2))

  LiDARsummary<-data.frame(cbind(NameExp,MetricsExp))
  colnames(LiDARsummary)<-c("Parameters", "Value")
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
