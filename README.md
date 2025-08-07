# R package for whomap and bubble map
version 0.1.2

Draws choropleth and bubble maps of the world, based on the 2025 (latest) WHO shapefiles (without simplifications to be compliant to WHO legal requirements).
This package and functions of whomapper and bubblemapper are the updated version of whomap package developed by Philippe Glaziou.


## Install:

remotes::install_github('yamanakatakuya/whomapper')


## Usage: 

### whomapper

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

### bubblemapper

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

### map_builder

This is a script that has common codes i.e. map data transformation, WHO disclaimer, and common map layers and theme settings.


## Examples:

### Univariate

brics <- data.frame(iso3=c('BRA','CHN','IND','RUS','ZAF'),
                    var=1)

#### Oceans and lakes in white, with Equirectangular projection

whomapper(brics, colour='red', legend_pos='none', water_col = 'white', projection = "eqc")

<img width="3000" height="1800" alt="p1" src="https://github.com/user-attachments/assets/5f54fbb2-999e-434c-873a-2874aebfdabc" />


#### Oceans and lakes in light blue, with Mollweide projection

whomapper(brics, colour='red', legend_pos='none', water_col = 'white', projection = "moll")

<img width="3000" height="1800" alt="p2" src="https://github.com/user-attachments/assets/9c7d1fd3-93a7-42f5-8d4e-b001b83e9d96" />

### Categorical data
brics$var <- as.factor(1:5)

#### Oceans and lakes in white, with Eckert IV projection

whomapper(brics, legend_title='BRICS', water_col = 'white', projection = "eck4")

<img width="3000" height="1800" alt="p3" src="https://github.com/user-attachments/assets/e29f9fe8-6e85-4304-8be1-3ac190a57cbc" />


#### Recentered on the region Asia-Pacific, with the legend repositioned, with Robinson projection

whomapper(brics, legend_title = 'BRICS', legend_pos = c(0.7, 0.52), offset = 150, projection = "robin")

<img width="3000" height="1800" alt="p4" src="https://github.com/user-attachments/assets/d17be31a-0a5d-4fe1-8c5e-596e0e5718ab" />


### Bubble map, with Hammer projection
brics$size <-  c(1e4, 1e5, 3e5, 5e5, 1e6)

bubblemapper(brics,legend_title = "Size of value",
             bubble_col = "purple",
             scale_breaks = c(1e4, 2.5e5, 5e5, 1e6),
             scale_limits = c(1e4, 1e6),
             scale_labels = c("10 000","250 000","500 000","1 000 000"),
             legend_pos = c(0.17,0.54), projection = "hammer") 


<img width="3000" height="1800" alt="p5" src="https://github.com/user-attachments/assets/148f0f21-6efb-45d9-b5fa-d8b06fa40440" />



