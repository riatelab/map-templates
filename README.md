# map-templates

R programmes to build map templates for Magrit (with boxes for overseas and outermost territories), with data. 

## Europe template
Map templates created with the sf, giscoR, mapinsetr R packages
Copyright ©EuroGeographics for the administrative boundaries ; (cc) RIATE for map template

NUTS2-3 Statistical attributes
NUTS2 and NUTS3 layers fed by GISCO attributes and Eurostat data (total area and total population, from eurostat R package)
- NUTS_ID : NUTS id code
- NAME_LATIN : NUTS name in local language, transliterated to Latin script
- URBN_TYPE : Urban-rural typology for NUTS3 regions (1: predominantly urban regio ; 2: intermediate region; 3 predominantly rural
- MOUNT_TYPE : Mountain typology for NUTS 3 region (1: "where more than 50 % of the surface is covered by topographic mountain areas" ; 2: "in which more than 50 % of the regional population lives in topographic mountain areas"; 
3: "where more than 50 % of the surface is covered by topographic mountain areas and where more than 50 % of the regional population lives in these mountain areas", 4: non-mountain region / other region, 0: no classification provided (e.g. in the case of NUTS 1 and NUTS 2 and non-EU countries)
- COAST_TYPE : Coastal typology for NUTS3 regions (1: coastal (on coast), 2: coastal (>= 50% of population living within 50km of the coastline), 3: non-coastal region}, 0: no classification provided (e.g. in the case of NUTS 1 and NUTS 2 regions).
- AREA_2021 : Total area in 2021 (or 2019-2020 if missing values, Eurostat table *reg_area3*)
- POP_2021 : Total population in 2021 (or 2019-2020 if missing values, Eurostat table *demo_r_pjanaggr3*)
- DENS_2021 : Population density (inh. per sq. km, POP_2021 / AREA_2021). 