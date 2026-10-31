# Automobile Data Analysis and Visualization Dashboard

## Project Overview

This project focuses on analysing automobile data and understanding the relationship between vehicle characteristics and fuel efficiency using R Programming.

An interactive dashboard is developed using Shiny to display automobile statistics and visualizations.

## Objectives

- Analyse automobile data using R.
- Perform data manipulation using dplyr.
- Create visualizations using ggplot2.
- Compare city and highway mileage.
- Study the relationship between engine displacement and fuel efficiency.
- Develop an interactive dashboard.

## Technologies Used

- R Programming
- RStudio
- ggplot2
- dplyr
- Shiny
- Plotly
- DT
- bslib
- tidyr

## Dataset

The project uses the built-in `mpg` dataset from the ggplot2 package.

It contains 234 observations describing automobile characteristics and fuel economy.

## Dashboard Features

1. Total vehicle count.
2. Average city mileage.
3. Average highway mileage.
4. Manufacturer count.
5. Interactive manufacturer filters.
6. Vehicle-class filters.
7. Five data visualizations.
8. Automatic data insights.
9. Download filtered data as CSV.
10. Interactive data table.

## Visualizations

- Average Highway MPG by Vehicle Class – Bar Chart
- Highway Mileage Distribution – Histogram
- Engine Displacement vs Highway Mileage – Scatter Plot
- City vs Highway Mileage – Grouped Bar Chart
- Vehicle Category Distribution – Donut Chart

## How to Run the Project

1. Install R and RStudio.
2. Install the required packages

```r
install.packages(c(shiny, ggplot2, dplyr,
                   plotly, DT, bslib, tidyr))
```

3. Open `app.R` in RStudio.
4. Click the Run App button.

## Learning Outcomes

This project helped develop practical knowledge of data cleaning, data manipulation, data visualization and interactive dashboard development using R Programming.