# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Importing latest WHO geoJSON files (in ./shape folder)
# the latest WHO shape/geoJSON files are available at https://gis-who.hub.arcgis.com/
# Takuya Yamanaka, August 2025
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(ggpattern)
library(tidyverse)
library(ggplot2)
library(sf)
library(here)


load(here::here('data/disa_ac.rda'))
load(here::here('data/disa_nlake_nac.rda'))
load(here::here('data/disa_lake.rda'))
load(here::here('data/disb.rda'))
load(here::here('data/world.rda'))

load(here::here(paste0("./data/test_data", ".rda")))

# leftjoin a dataset
data <- world |>
  left_join(test_data, by = c("iso3"))

# 1. Ensure var is a factor with explicit NA
data$var <- forcats::fct_explicit_na(data$var, na_level = "No data")

# 2. Create one dummy sf object for both legend entries ---
dummy_legend <- st_sf(
  var = factor(
    c("No data", "Not applicable"),
    levels = c(levels(data$var),  "Not applicable")
  ),
  geometry = st_sfc(st_geometrycollection(), st_geometrycollection()),
  crs = st_crs(data)
)


legend.title <- "Status"
col <- c("royalblue4","dodgerblue3","steelblue1","darkviolet","maroon1","violet")
na_col <- 'white'
col2 <- c(col, na_col, 'grey60')

legend_pos <- c(0.2,0.40)


# plot the base world map
p <- ggplot() + 
  geom_sf(data=data,  col="black", aes(fill = var)) +
  # Dummy data for legend entries
  geom_sf(data = dummy_legend, aes(fill = var), show.legend = TRUE) +
  # legend
  ggplot2::scale_fill_manual(legend.title, values = col2) +
  guides(
    fill = guide_legend(override.aes = list(color = NA))  # remove outline in legend
  ) 


# Aksai Chin colour trick
# 1. Check China's value
china_status <- data$var[data$iso3 == "CHN"]
# 2. Assign names to color vector
names(col2) <- levels(data$var)
# 3. Get the color applied to China
china_color <- col2[as.character(china_status)]
print(china_color)


# plot AC layer and other layers
p <- p +
  # Stripe pattern for AC
  geom_sf_pattern(data = disa_ac,
                  fill = china_color,
                  col = "grey80",           # outline color
                  linewidth = 0.3,          # outline thickness
                  pattern = "stripe",
                  pattern_fill = "grey80",  # stripe color
                  pattern_colour = "grey80",
                  pattern_size = 0.015,       # stripe thickness
                  pattern_angle = 45,
                  pattern_density = 0.3,
                  pattern_spacing = 0.001) +
  # fill grey for other disputed areas
  geom_sf(data=disa_nlake_nac,  col="grey80", fill="grey80",
          linewidth = 0.1) +
  # fill white for lakes
  geom_sf(data=disa_lake,  col="black", fill="white",
          linewidth = 0.1) +
  # dashed lines for Sudan/South Sudan  boundaries
  geom_sf(data=disb_su,  col="grey50", fill="grey50",
          linewidth = 0.1,
          linetype = "dashed") +
  # solid black lines for Arunachal Pradesh
  geom_sf(data=disb_ar,  col="black", fill="grey50",
          linewidth = 0.1,
          linetype = "solid") +
  # solid lines for other boundaries
  geom_sf(data=disb_nsu,  col="grey50", fill="grey50",
          linewidth = 0.1,
          linetype = "solid") +
  
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank()) +
  theme(panel.grid = element_blank()) +
  theme(
    legend.key.size = unit(0.4, "cm"),
    legend.key = element_rect(fill = "white", color = "white"),
    legend.text = element_text(size = 6),
    legend.justification = c(0.5, 1),
    legend.title = element_text(size = 6),
    legend.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = legend_pos
  )

ggsave(here::here("./local/test_actualdata.png"), plot = p, 
       width = 10, height = 6, dpi = 1000)  


