# R package for whomap and bubble map
version 1.0

Draws choropleth and bubble maps of the world, based on the 2025 (latest) WHO shapefiles (without simplifications to be compliant to WHO legal requiremnets)
This package and functions of whomapper and bubblemapper are updated version of whomap package developed by Philippe Glaziou.


## Install:

remotes::install_github('yamanakatakuya/whomapper')


## Usage: 

whomapper <- function (df = data.frame(iso3 = NA, var = NA),
                    colours = NULL,
                    projection = "moll",
                    offset = 10.8,
                    low_col = '#BDD7E7',
                    high_col = '#08519C',
                    line_col = 'black',
                    line_width = 0.3,
                    map_title = "",
                    legend_title = "",
                    water_col = 'white',
                    na_label = 'No data',
                    na_col = 'white',
                    disclaimer = FALSE,
                    legend_pos = c(0.17,0.42)
)
    
df is a dataframe. It must contain a variable named "iso3" holding country ISO3 codes, and a second
categorical variable named "var". There should be no more than 6 categories (excluding "No data" and 
"Not applicable") for optimal display of the legend. The category labels should be short.

15 different map projections are allowed, by changing "projection" (default setting is "moll" Molweide Projection).
Option for projection: "eqc", "moll", "robin", "eck1", "eck2", "eck3", "eck4", "eck5", "eck6", "hammer", "goode", "sinu", "aitoff", "bonne +lat_1=45", "bonne +lat_1=90"

bubblemapper <- function (df = data.frame(iso3 = NA, size = NA),
                       projection = "moll",
                       offset = 10.8,
                       bubble_col = 'dodgerblue',
                       bubble_alpha = 0.4,
                       scale_breaks,
                       scale_limits,
                       scale_labels,
                       line_col = 'black',
                       line_width = 0.3,
                       map_title = "",
                       legend_title = "",
                       water_col = 'white',
                       na_label = 'No data',
                       na_col = 'white',
                       disclaimer = FALSE,
                       legend_pos = c(0.17,0.42)
)

df is a dataframe. It must contain a variable named "iso3" holding country ISO3 codes, and a second
numeric variable named "size".

Examples:

#### Univariate

brics <- data.frame(iso3=c('BRA','CHN','IND','RUS','ZAF'),
                    var=1)

Oceans and lakes in white, with Equirectangular projection

whomapper(brics, colour='red', legend_pos='none', water_col = 'white', projection = "eqc")

<img width="3000" height="1800" alt="p1" src="https://github.com/user-attachments/assets/5f54fbb2-999e-434c-873a-2874aebfdabc" />


Oceans and lakes in light blue, with Mollweide projection

whomapper(brics, colour='red', legend_pos='none', water_col = 'white', projection = "moll")

<img width="3000" height="1800" alt="p2" src="https://github.com/user-attachments/assets/9c7d1fd3-93a7-42f5-8d4e-b001b83e9d96" />


brics$var <- 1:5

whomap(brics, legend.title='BRICS', water.col = 'white')

![image](https://user-images.githubusercontent.com/233963/120228265-13798980-c24b-11eb-9ce6-7f62ae383fa7.png)


Recentered on the region Asia-Pacific, with the legend repositioned:

whomap(brics, legend.title = 'BRICS', legend.pos = c(0.7, 0.26), recentre = 163, water.col = 'white')

![image](https://user-images.githubusercontent.com/233963/119449970-98d4d980-bd33-11eb-89f3-24ca5c8be36f.png)


The above maps can be drawn using high-definition 2022 WHO shapefiles by passing a parameter "hidef = T" (default is F). The drawing in high-definition is considerably slower. The previous map in high-definition with the default colour for water bodies is shown below:

whomap(brics, legend.title = 'BRICS', legend.pos = c(0.7, 0.26), recentre = 163, hidef = TRUE)

![image](https://user-images.githubusercontent.com/233963/173190001-bcd107c7-17cc-483b-8419-968ccd06ec4c.png)



### Bivariate

World map also showing a secondary country marker denoting a second variable

whomap(brics, legend.title='BRICS', legend.pos=c(0.14, 0.34)) +
   add_marker('BRA', lab='Subnational\ndata')

![image](https://user-images.githubusercontent.com/233963/120228390-44f25500-c24b-11eb-848c-1673771848a2.png)



