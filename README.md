<h1>Remote Sensing and Environmental Modelling Challenge</h1>
 
Integration of remote sensing data into physically-based water balance modelling of forest stands  
Thomas Zieher, PhD  
Department of Natural Hazards, Austrian Research Center for Forests (BFW)
 
 
<h2>Objective of the assignment</h2>
The overall objective of this assignment is to parameterize and apply a physically-based soil-vegetation-atmosphere-transfer (SVAT) model and integrate remote sensing data for deriving input parameters (and eventually validation) of forest stands. We start with the model LWF-Brook90, parameterized in a remote office, relying only on publicly available data. In the field and with the help of remote sensing techniques and data, we start improving the parameterization of the model. In the end, we compare the results of the initial 'remote' setup and the results of the refined model runs.
 
Generally, the workflow comprises of the following steps:
1. Introduction to the model LWF-Brook90
2. Field work: collection of stand-specific input parameters (e.g. tree species, height, LAI), estimation of soil parameters (texture class, bulk density)
3. Model parameterization: integration of remote sensing for parameterization, application of the model tailored to the test site
4. Eventually: validation of model results with remote sensing data
 
With the model, we will generate time series of water balance components (e.g. soil evaporation, canopy transpiration, soil infiltration, surface runoff) on a daily resolution, which we can then use for deriving drought stress indicators. Based on the indicators, we will analyse a recent drought event in northern Italy, which occurred in 2022 (EC & JRC 2022). 

<h2>Background</h2>
Soil-vegetation-atmosphere transport (SVAT) models are useful tools for investigating the water balance in forest stands. Recent developments of the lumped SVAT model LWF-Brook90 (Fig. 1), based on the original Brook90 model (Federer et al. 2003), allow applications on the local to continental scale. In recent studies, the model LWF-Brook90 has been used for in-depth analyses of water fluxes in a forest stand compared to monitoring data (Schmidt-Walter et al. 2020), near-real-time soil moisture modelling in provincial forests in Germany (Vorobevskii et al. 2024) and analysing drought stress events in Switzerland (Meusburger et al. 2022). Furthermore, Meusburger et al. (2022) compared the modelling results with drought stress indicator maps based on Sentinel-2 imagery. A different version of the model was used to analyse the drought event in 2018 on a European scale (Vorobevskii et al. 2024b). The model LWF-Brook90 is very sensitive to vegetation parameters (Schmidt-Walter et al. 2020) and can be used to compare the water balance of different tree species. With an implemented snow model, it can be applied in Alpine environments.

[<img src="https://github.com/thomaszieher/earth_sensing_summer_school/blob/main/plots/model_schematics.png" width="400" />](https://github.com/thomaszieher/earth_sensing_summer_school/blob/main/plots/model_schematics.png)  
Figure 1: Conceptual sketch of the model LWF-Brook90 (own figure)

<h2>Materials</h2>
The model uses meteorological time series with daily resolution, soil input parameters and vegetation input parameters. We will derive meteorological input data from a local station and collect soil input data with a soil auger in the field. We will discuss the vegetation input parameters and integrate remote sensing data where possible.
 
The following software will be used and should be installed on the student’s computer prior to the Summer School:  
* R and RStudio
* The earth_sensing_summer_school repository (see instructions below)
* GIS software: preferably QGIS: https://qgis.org/en/site/forusers/download.html#  
 
<h2>Test site description</h2>
Our study site for field work will be at (south-oriented) forest stands located close to the venue and showing signs of drought stress. Eventually we will team up with Challenge A for including their results in the model parameterization. In case we cannot acquire data in the field we will use existing area-wide soil input data (Hengl et al. 2017, https://soilgrids.org) and published vegetation parameterizations. In this case, we will focus on the validation of the results with remote sensing data.

[<img src="https://github.com/thomaszieher/earth_sensing_summer_school/blob/main/plots/study_area.png" width="600" />](https://github.com/thomaszieher/earth_sensing_summer_school/blob/main/plots/study_area.png)  
Figure 2: study site (from Google Earth)

<h2>Methods</h2>

1. Meteorological input preparation: deriving and formatting of meteorological input time series  
2. Soil input preparation: soil testing in the field using a soil auger and simple tests based on field survey guidelines; as an alternative, the soil grids datasets published by Hengl et al. (2017) will be prepared  
3. Vegetation input preparation: starting with published vegetation parameterizations, we will refine selected parameters at the study site  
4. Analysis of the results: derivation of drought indicators, eventually comparison with indicators based on remote sensing  
 
[<img src="https://github.com/thomaszieher/earth_sensing_summer_school/blob/main/plots/ts_output.png" width="600" />](https://github.com/thomaszieher/earth_sensing_summer_school/blob/main/plots/ts_output.png)  
Figure 3: Example of resulting drought stress indicator time series in a daily resolution in a forest stand in Tyrol (Austria).

<h2>Assignment outputs</h2>

* Powerpoint presentation of final group presentation.  
* A written report, for which it is recommended to use the ISPRS Annals full-paper templates (in MS Word or LaTeX) from: 
https://www.isprs.org/documents/orangebook/app5.aspx

<h2>Installation instructions</h2>

1. Install R 4.5.1: https://cran.r-project.org
2. Install R Studio: https://posit.co/download/rstudio-desktop  
3. Install GIT: https://git-scm.com/downloads  
4. Clone repository: https://github.com/thomaszieher/earth_sensing_summer_school.git  
5. Restore environment with ```renv::restore()```

<h2>Acknowledgements</h2>

The methods and content of this assignment were elaborated within the WINALP-21 project, funded by the INTERREG VI-A program (grant number BA0100020).

[<img src="https://github.com/thomaszieher/earth_sensing_summer_school/blob/main/plots/project_logo.jpg" width="600" />](https://github.com/thomaszieher/earth_sensing_summer_school/blob/main/plots/project_logo.jpg)  

<h2>References</h2>

EC & JRC, 2022. Drought in northern Italy March 2022 – GDO analytical report, Publications Office of the European Union.  

Federer, C. A., Vörösmarty, C. and Fekete, B., 2003. Sensitivity of Annual Evaporation to Soil and Root Properties in Two Models of Contrasting Complexity, Journal of Hydrometeorology 4(6), pp. 1276-1290.  

Hengl, T., Mendes de Jesus, J., Heuvelink, G. B. M., Ruiperez Gonzalez, M., Kilibarda, M., Blagotić, A., Shangguan, W., Wright, M. N., Geng, X., Bauer-Marschallinger, B., Guevara, M. A., Vargas, R., MacMillan, R. A., Batjes, N. H., Leenaars, J. G. B., Ribeiro, E., Wheeler, I., Mantel, S. and Kempen, B., 2017. SoilGrids250m: Global gridded soil information based on machine learning, PLOS ONE 12(2), pp. 1-40.  

Meusburger, K., Trotsiuk, V., Schmidt-Walter, P., Baltensweiler, A., Brun, P., Bernhard, F., Gharun, M., Habel, R., Hagedorn, F., Köchli, R., Psomas, A., Puhlmann, H., Thimonier, A., Waldner, P., Zimmermann, S. and Walthert, L., 2022. Soil–plant interactions modulated water availability of Swiss forests during the 2015 and 2018 droughts, Global Change Biology 28(20), pp. 5928-5944.  

Schmidt-Walter, P., Trotsiuk, V., Meusburger, K., Zacios, M. and Meesenburg, H., 2020. Advancing simulations of water fluxes, soil moisture and drought stress by using the LWF-Brook90 hydrological model in R, Agricultural and Forest Meteorology 291, 108023.  

Vorobevskii, I., Luong, T. T., Kronenberg, R. and Petzold, R., 2024. High-resolution operational soil moisture monitoring for forests in central Germany, Hydrology and Earth System Sciences 28(15), pp. 3567-3595.  

Vorobevskii, I., Luong, T. T. and Kronenberg, R., 2024. Seasonal forecasting of local-scale soil moisture droughts with Global BROOK90: a case study of the European drought of 2018, Natural Hazards and Earth System Sciences 24(2), pp. 681-697.

