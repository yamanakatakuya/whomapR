# R package for whomap and bubble map
version 0.1.6

Draws choropleth and bubble maps of the world, based on the 2025 (latest) WHO shapefiles (without simplifications to be compliant to WHO legal requirements).
This package and functions of whomapper and bubblemapper are the updated version of whomap package developed by Philippe Glaziou.


## Install:

remotes::install_github('yamanakatakuya/whomapper')


## Usage: 

### whomapper

whomapper <- function (X = data.frame(iso3 = NA, var = NA),
                    colours = NULL,
                    projection = "robin",
                    offset = NULL,
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
                    legend_pos = c(0.17,0.42),
                    zoom = 'Global'
)
    
X is a dataframe. It must contain a variable named "iso3" holding country ISO3 codes, and a second
categorical variable named "var". There should be no more than 6 categories (excluding "No data" and 
"Not applicable") for optimal display of the legend. The category labels should be short.

15 different map projections are allowed, by changing "projection" (default setting is "moll" Molweide Projection).
Options for projection: "eqc", "moll", "robin", "eck1", "eck2", "eck3", "eck4", "eck5", "eck6", "hammer", "goode", "sinu", "aitoff", "bonne +lat_1=45", "bonne +lat_1=90"

Zoom setting provides an option to produce WHO regional map.
Options for zoom: "Global", "AFR", "AMR", "EMR", "EUR", "SEA", "WPR"

### bubblemapper

bubblemapper <- function (X = data.frame(iso3 = NA, size = NA),
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

X is a dataframe. It must contain a variable named "iso3" holding country ISO3 codes, and a second
numeric variable named "size".

### add_marker

add_marker <- function(iso3 = NA_character_,
                       shape = 17,
                       col = 'red',
                       size = 3,
                       alpha = 1,
                       lab = '',
                       projection = "robin",
                       offset = 10.8) 

iso3 is a vector. It must contain a variable named "iso3" holding country ISO3 codes. This function is for adding the second layer of country markers on a map produced by whomapper() or bubblemapper().

### add_annotate

add_annotate <- function(
    geom,
    x = NULL, y = NULL,
    xend = NULL, yend = NULL,
    xmin = NULL, ymin = NULL,
    xmax = NULL, ymax = NULL,
    linewidth = NULL,
    size = NULL,
    col = NULL,
    lab = NULL,
    include_coord = FALSE,
    default_crs = 4326
) 

This function is for adding texts, shapes, segments etc using ggplot2::annotate (with CRS adjustment) on a map produced by whomapper() or bubblemapper().
When add_annotate is used multiple times, only the first annotate requires "include_coord = TRUE"."


### map_builder

This is a script that has common codes i.e. map data transformation, WHO disclaimer, and common map layers and theme settings, used both by whomapper.R and bubblemapper.R


## Examples:

### Univariate

brics <- data.frame(iso3=c('BRA','CHN','IND','RUS','ZAF'),
                    var=1)

#### Oceans and lakes in white, with Equirectangular projection

whomapper(brics, colour='red', legend_pos='none', water_col = 'white', projection = "eqc")

<img width="3000" height="1800" alt="p1" src="https://github.com/user-attachments/assets/5f54fbb2-999e-434c-873a-2874aebfdabc" />


#### Oceans and lakes in light blue, with Mollweide projection

whomapper(brics, colour='red', legend_pos='none', water_col = 'white', projection = "moll")

<img width="3000" height="1800" alt="p2" src="https://github.com/user-attachments/assets/19031a47-5a65-4c89-a61c-c2ea85a1c5ac" />


### Categorical data
brics$var <- as.factor(1:5)

#### Oceans and lakes in white, with Eckert IV projection

whomapper(brics, legend_title='BRICS', water_col = 'white', projection = "eck4")

<img width="3000" height="1800" alt="p3" src="https://github.com/user-attachments/assets/e29f9fe8-6e85-4304-8be1-3ac190a57cbc" />


#### Recentered on the region Asia-Pacific, with the legend repositioned, with Robinson projection

whomapper(brics, legend_title = 'BRICS', legend_pos = c(0.7, 0.52), offset = 150, projection = "robin")

<img width="3000" height="1800" alt="p4" src="https://github.com/user-attachments/assets/d17be31a-0a5d-4fe1-8c5e-596e0e5718ab" />


### Bubble map with add_annotate, with Hammer projection
brics$size <-  c(1e4, 1e5, 3e5, 5e5, 1e6)

bubblemapper(brics,legend_title = "Size of value",
             bubble_col = "purple",
             scale_breaks = c(1e4, 2.5e5, 5e5, 1e6),
             scale_limits = c(1e4, 1e6),
             scale_labels = c("10 000","250 000","500 000","1 000 000"),
             legend_pos = c(0.17,0.54), projection = "hammer") +
             
  add_annotate('text', lab='South Africa', x=50, y=-30, size=3, include_coord = TRUE) +
  add_annotate("segment",x=25, xend=45, y=-29, yend=-29) +
  
  add_annotate('text', lab='Russian Federation', x=150, y=83, size=3) +
  add_annotate("segment",x=89, xend=90, y=58, yend=82) 

<img width="3000" height="1800" alt="p5" src="https://github.com/user-attachments/assets/9fe47923-445a-4978-aa89-2bf4d62a8f8c" />

### WHO regional map (an example of the Western Pacific Region), with Robinson projection
wpr <- data.frame(iso3=c('AUS','CHN','PHL','IDN','VNM'),
                    var=as.factor(1:5))
whomapper(wpr, brewer.pal(5, "PuRd"), legend_title = 'BRICS', legend_pos = c(0.2, 0.4), zoom = "WPR")

<img width="3000" height="1800" alt="p6" src="https://github.com/user-attachments/assets/754808e8-5cfc-4af7-ad8a-fcfaa2947347" />




