# a5udes

![alt text](https://github.com/jmigueldelgado/a5udes/blob/master/screenshot.png?raw=true)

This R package in development will derive the topology of a dataset of reservoirs (down to 5000 m2) based on HydroSHEDS. The reservoirs used were obtained by filtering the global surface water explorer[1]. This is work in progress. The final result will be a directed graph in the form of a tidygraph and a set of functions to obtain the topology anywhere in the world.

This product _a5udes_ incorporates data from the HydroSHEDS database which is © World Wildlife Fund, Inc. (2006-2013) and has been used herein under license. WWF has not evaluated the data as altered and incorporated within  _a5udes_ and therefore gives no warranty regarding its accuracy, completeness, currency or suitability for any particular purpose. Portions of the HydroSHEDS database incorporate data which are the intellectual property rights of © USGS (2006-2008), NASA (2000-2005), ESRI (1992-1998), CIAT (2004-2006), UNEP-WCMC (1993), WWF (2004), Commonwealth of Australia (2007), and Her Royal Majesty and the British Crown and are used under license. The HydroSHEDS database and more information are available at http://www.hydrosheds.org.

conda env create -f a5udes.yml
devtools::install_github('jmigueldelgado/a5udes',ref='new_river_database')

[1] Jean-Francois Pekel, Andrew Cottam, Noel Gorelick, Alan S. Belward, High-resolution mapping of global surface water and its long-term changes. Nature 540, 418-422 (2016). (doi:10.1038/nature20584) 
