![](https://github.com/carlos-alberto-silva/weblidar-treetop/blob/master/readme/wiki_page.png)<br/>

[![CRAN](https://www.r-pkg.org/badges/version/treetop)](https://cran.r-project.org/package=treetop)
![Github](https://img.shields.io/badge/Github-0.0.1-green.svg)
![licence](https://img.shields.io/badge/Licence-GPL--3-blue.svg) 
![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/treetop)


**Treetop: A Shiny-based Application for Extracting Forest Information from LiDAR data.**

Authors: Carlos Alberto Silva, Andrew T. Hudak, Lee A. Vierling,  Ruben Valbuena, Adrian Cardil, Midhun Mohan, Danilo Roberti Alves de Almeida, Eben N. Broadbent, Angelica M. Almeyda Zambrano, Ben Wilkinson, Ajay Sharma, Jason B. Drake, Paul B. Medley, Jason G. Vogel, Gabriel Atticciati Prata, Jeff Atkins, Caio Hamamura, Carine Klauberg.   

The treetop application provides options for i) detecting individual trees from LiDAR-derived canopy height models (CHM), ii) extract crown-level attributes (e.g. location, crown height and area), iii) assessing forest uniformidy and individual tree spatial distribution, iv) exporting extracted crown-level products, v) visualizing CHM and crown-level products in 2D and 3D.

# Getting Started


## Install R, Git and Rtools40

i) *R (>= 4.0.0)*: https://www.r-project.org/

ii) *Git*: https://git-scm.com/

iii) *Rtools40*: https://cran.r-project.org/bin/windows/Rtools/


## Treetop installation
```r
# The CRAN version:
install.packages("treetop")

# The development version:
#install.packages("remotes")
library(remotes)
install_git("https://github.com/carlos-alberto-silva/weblidar-treetop", dependencies = TRUE)

```    

## Loading and launching treetop application
```r
library(treetop)
launchApp(launch.browser = TRUE)

```
<img src="https://github.com/carlos-alberto-silva/weblidar-treetop/blob/master/readme/weblidar_treetop_app.gif">

# References
Chang, W., Cheng, J., Allaire, J. J., Xie, Y., & McPherson, J. (2021). shiny:
Web Application Framework for R. https://cran.r-project.org/web/packages/shiny/index.html

Leite, R.V.; Silva, C.A.; Mohan, M.; Cardil, A.; Almeida, D.R.A.d.; Carvalho, S.d.P.C.e; Jaafar, W.S.W.M.; Guerra-Hernández, J.; Weiskittel, A.; Hudak, A.T.; Broadbent, E.N.; Prata, G.; Valbuena, R.; Leite, H.G.; Taquetti, M.F.; Soares, A.A.V.; Scolforo, H.F.; Amaral, C.H.d.; Dalla Corte, A.P.; Klauberg, C. (2020). Individual Tree Attribute Estimation and Uniformity Assessment in Fast-Growing Eucalyptus spp. Forest Plantations Using Lidar and Linear Mixed-Effects Models. Remote Sens. 12, 3599. https://doi.org/10.3390/rs12213599

R Core Team. (2021). R: A Language and Environment for Statistical Computing; R Core Team: Vienna, Austria. https://www.r-project.org/

Silva, C. A., Hudak, A. T., Vierling, L. A., Loudermilk, E. L., O’Brien, J. J., Hiers, J. K., Khosravipour, A. (2016). Imputation of Individual Longleaf Pine (Pinus palustris Mill.) Tree Attributes from Field and LiDAR Data. Canadian Journal of Remote Sensing, 42(5), 554–573. doi:10.1080/07038992.2016.1196582

Roussel, J.R., Auty, D., Coops, N. C., Tompalski, P., Goodbody, T. R. H., Sánchez Meador, A., Bourdon, J.F., De Boissieu, F., Achim, A. (2020). lidR : An R package for analysis of Airborne Laser Scanning (ALS) data. Remote Sensing of Environment, 251 (August), 112061. <doi:10.1016/j.rse.2020.112061>.

# Acknowledgements
We gratefully acknowledge funding from the National Counsel of Technological and Scientific Development (CNPq) and Department of Defense Strategic Environmental Research and Development Program (SERDP), grant RC-2243, RC19-1064 and RC20-1346. 

# Reporting Issues 
Please report any issue regarding the Treetop app to Dr. *Carlos A. Silva* (carlos_engflorestal@outlook.com)

# Citing treetop application
Silva, C.A.; Hudak, A.T; Vierling, L.A.;  Valbuena, R.; Cardil, A.; Mohan, M.;  Almeida, D. A.;  Broadbent,E.N.; Zambrano,A. M. A.; Wilkinson, B., Sharma,A., Drake,J. B.;  Medley,P. B., Vogel, J. G.; Prata,G. A.; Atkins, J.; Hamamura,C.; Klauberg, C. 2021. TreeTop: A Shiny-based Application for Extracting Forest Information from LiDAR data for Ecologists and Conservationists. *Methods in Ecology and Evolution (In prep).*

Silva, C.A.; Hudak, A.T; Vierling, L.A.;  Valbuena, R.; Cardil, A.; Mohan, M.;  Almeida, D. A.;  Broadbent,E.N.; Zambrano,A. M. A.; Wilkinson, B., Sharma,A., Drake,J. B.;  Medley,P. B., Vogel, J. G.; Prata,G. A.; Atkins, J.; Hamamura,C.; Klauberg, C. Treetop: A Shiny-based Application for Extracting Forest Information from LiDAR data. Version 0.0.1, accessed on March. 13 2021, available at: https://CRAN.R-project.org/package=treetop

# Disclaimer
**Treetop has been developed using the *Shiny* (Chang et al. 2021) package in R (R Core Team 2021). It comes with no guarantee, expressed or implied, and the authors hold no responsibility for its use or reliability of its outputs.**

