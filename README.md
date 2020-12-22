# Exercise on Stationarity and ARIMA modeling in R

The exercise aims to analyze four time series, two non-stationary and two stationary. Besided, it asks to pick a time series and to predict the next year value through the best ARIMA model.

The identified time series are:

• Dow Jones Industrial Average: it measures the stock performance of 30 large companies listed on stock exchanges in the United States,
• Southern Oscillation Index (SOI): a standardized index based on the observed sea level pressure differences between Tahiti and Darwin, Australia. The SOI is one measure of the large-scale fluctuations in air pressure occurring between the western and eastern tropical Pacific (i.e., the state of the Southern Oscillation) during El Niño and La Niña episodes. In general, smoothed time series of the SOI correspond very well with changes in ocean temperatures across the eastern tropical Pacific. Sustained negative values of the SOI below −7 often indicate El Niño episodes. These negative values are usually accompanied by sustained warming of the central and eastern tropical Pacific Ocean, a decrease in the strength of the Pacific Trade Winds, and a reduction in winter and spring rainfall over much of eastern Australia and the Top End. Sustainted positive values of the SOI above +7 are typical of a La Niña episode. They are associated with stronger Pacific trade winds and warmer sea temperatures to the north of Australia. Waters in the central and eastern tropical Pacific Ocean become cooler during this time. Together these give an increased probability that eastern and northern Australia will be wetter than normal.
• Global Temperatures Anomalies by GISTEMP: GISS Surface Temperature Analysis ver. 4 (GISTEMP v4) is an estimate of global surface temperature change. In the dataset, a multi-year smoothing is applied to fully remove the annual cycle and improve information content in temperature data.
• Global CO2 emissions: Carbon dioxide emissions are the primary driver of global climate change. CO2 emissions data is sourced from the Global Carbon Project and the Carbon Dioxide Information Analysis Centre (CDIAC). Data has been converted by Our World in Data from tonnes of carbon to tonnes of carbon dioxide using a conversion factor of 3.664.

The user can find the time series in the _Source_ folder, and the R code named _stationary-and-arima_.
