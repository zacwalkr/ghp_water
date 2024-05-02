# Spatial and temporal variation in water availability across high mountain gravelly pavement herbfields affects the distribution of hydrophilic plant species
Alexandra Blackburn-Smith, Ewen Silvester, Zac C. Walker, John W. Morgan 

## About
This repository contains the code and data associated with an unpublished manuscript. The paper assesses the relationship between plant species distribution in pavements and the spatial and temporal availability and composition of water. These analyses were done using R version 4.2.0.

## Abstract
Gravelly pavement herbfields are a groundwater-dependant vegetation type in the high mountains of south-eastern Australia. Water supply is hypothesised to be a crucial factor affecting the distribution of dominant plant species in the community. We quantified the relationship between plant species distribution in pavements and the spatial and temporal availability and composition of water. Water flow rates into pavements varied throughout the growing season, as did the lateral spread of water, and its permanency. The distribution of dominant pavement plant species was associated with moisture (i.e. frequency of microsite wetness). Oreobolus pumilio (Cyperaceae) was significantly negatively associated with persistent moisture while Psychrophilia introloba (Ranunculaceae) was significantly positively associated with more persistent moisture. Variation in moisture is clearly a potent force that structures pavement plant communities. The chemical composition of water changed while transiting through the pavements, with the loss of dissolved CO2 and an associated increase in pH likely due to strong CO2 evasion. More broadly, when compared with a range of hydrologic units across this landscape, the chemical composition of the pavement waters studied here was between that of groundwater and peatlands, corresponding to their physical position in the landscape.  These results have implications for the conservation of the gravelly pavement herbfield community. With climate change, reductions in water availability likely threatens the persistence of key hydrophilic species given their strong association with moisture. Reductions in areal extent, as well as re-organisation of the pavement plant community, could therefore be expected.

## Description of the data and file structure.
There are 4 data files and 1 R scripts in this repository.

The R script 'occu_analysis.R' uses the data file 'wetland_data.csv' to assess occupancy of four herbaceous gravelly herbfield pavement species along a microsite wetness gradient using glmm's. This code and data produce Fig 2 of the manuscript.
The data file 'gw_flow.csv' is used to produce the boxplots in Fig 1. The data file 'wq_across.csv' is used to produce the PCA biplot Fig 3 and boxplots in Fig S2. The data file 'wq_within.csv' is used to produce the PCA biplot Fig 4.

To add: GWB input file for Figure S1
