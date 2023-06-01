# Flow-demo
Demo program for analyzing and visualizing flow data

This code reads in data from USGS and uses supplemental data downloaded for the rain project.
The code will compile flow data into annual yield, and also performs the same process of identifying rain events in the dataset.

The code will also produce a few hydrographs of specific rain events or portions of rain events.
As with the rain project, a "rain event" is classified as rainfall that is preceded and followed by at least six hours of dry weather.
The hydrograph code is adapted from Chuliang Xiao, at https://rpubs.com/cxiao/hydrograph-ggplot2-plot

Example hydrographs:
![storm_1](https://github.com/justinkhiga/Flow-demo/assets/89282137/6fd46a14-2aac-427b-a480-e0c90cf409e1)
![biggeststorm](https://github.com/justinkhiga/Flow-demo/assets/89282137/d5c59c9c-693a-4216-b6e2-432a5076a27c)
![biggeststormzoom](https://github.com/justinkhiga/Flow-demo/assets/89282137/7c7ea1a7-db61-4ca2-99b2-a7765b851701)
