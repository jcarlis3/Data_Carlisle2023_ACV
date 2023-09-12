# Overview
Supplemental data and code for Carlisle et al. 2023.

Carlisle, J.D., K.T. Smith, J.L. Beck, M.A. Murphy, and A.D. Chalfoun. 2023. Beyond overlap: considering habitat preference and fitness outcomes in the umbrella species concept. Animal Conservation. https://doi.org/10.1111/acv.12899



# Contents
## Data Products
- Raster (.img) of greater sage-grouse RSF values (model-predicted nest-site selection) as shown in Figure 2 (top) of the published paper.
- Raster (.img) of greater sage-grouse SPF values (model-predicted nest survival) as shown in Figure 2 (bottom) of the published paper.

## Input Data
- Spreadsheet (.csv) of greater sage-grouse nest data (see Data Sensitivity section below)
- Spreadsheet (.csv) of songbird nest data (*forthcoming*)
- Shapefile (.shp) of songbird nest-searching plots (*forthcoming*)
- Shapefile (.shp) of study area boundary (*forthcoming*)

## Analysis Code
- R script (.R) to predict nest-site selection and nest survival of greater sage-grouse using Random Forests
- R script (.R) to analyze songbird nest locations relative to greater sage-grouse RSF values (*forthcoming*)
- R script (.R) to analyze songbird nest survival relative to greater sage-grouse SPF values (*forthcoming*)

# Data Sensitivity
The location of greater sage-grouse nests is considered sensitive by the data
collectors, so geographic coordinates have been removed from the
dataset made publicly available here.

# License
See License_data.txt and License_code.txt for details.

- Data:  https://creativecommons.org/licenses/by/4.0/
- Code:  https://opensource.org/license/mit/
