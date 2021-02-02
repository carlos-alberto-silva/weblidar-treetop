![](https://github.com/carlos-alberto-silva/weblidar-treetop /blob/master/readme/wiki_page.png)<br/>

![Github](https://img.shields.io/badge/Github-0.0.1-green.svg)
![licence](https://img.shields.io/badge/Licence-GPL--3-blue.svg) 

**Treetop: A Shiny-based Application for Extracting Forest Information from LiDAR data for Ecologists and Conservationists.**

Authors: Carlos Alberto Silva, Andrew T. Hudak, Lee A. Vierling,  Ruben Valbuena, Adrian Cardil, Midhun Mohan, Danilo Roberti Alves de Almeida, Eben N. Broadbent, Angelica M. Almeyda Zambrano, Ben Wilkinson, Ajay Sharma, Jason B. Drake, Paul B. Medley, Jason G. Vogel, Gabriel Atticciati Prata,Jeff Atkins, Carine Klauberg.   

The treetop application provides options for i) detecting individual trees from lidar-derived canopy height models (CHM), ii) extract crown-level attributes (e.g. location, crown height and area), iii) assessing forest uniformidy and individual tree spatial distribution, iv) exporting extracted crown-level products, v) visualizing CHM and crown-level products in 2D and 3D.

# Getting Started

## Installation
```r

# Dependecies for the treetop package
packages = c("shiny","RColorBrewer","spatstat","raster","sp",
               "geometry","maptools","rgdal","rgl","lidR","pryr","devtools")

# Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# The development version:
devtools::install_git("https://github.com/carlos-alberto-silva/weblidar-treetop", dependencies = FALSE)

# loading treetop package
library(treetop)

```    

## Launch the treetop application
```r
treetop::launchApp()
```

# References
Silva, C. A., Hudak, A. T., Vierling, L. A., Loudermilk, E. L., O’Brien, J. J., Hiers, J. K., Khosravipour, A. (2016). Imputation of Individual Longleaf Pine (Pinus palustris Mill.) Tree Attributes from Field and LiDAR Data. Canadian Journal of Remote Sensing, 42(5), 554–573. doi:10.1080/07038992.2016.1196582

# Acknowledgements
We gratefully acknowledge funding from the National Counsel of Technological and Scientific Development (CNPq) and Department of Defense Strategic Environmental Research and Development Program, grant RC-2243. 

# Reporting Issues 
Please report any issue regarding the Treetop app to Dr. Silva (carlos_engflorestal@outlook.com)

# Citing treetop application
Silva et al. 2021. TreeTop: A Shiny-based Application for Extracting Forest Information from LiDAR data for Ecologists and Conservationists. Methods in Ecology and Evolution (In Review).

# Disclaimer
**Treetop has been developed using the Shiny package in R. It comes with no guarantee, expressed or implied, and the authors hold no responsibility for its use or reliability of its outputs.**

