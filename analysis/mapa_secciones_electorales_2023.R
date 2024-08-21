# -----------------------------------------------------------------------------
#                                   Dataton
# 
# Author: Roberto Gonzalez
# Date: August 20, 2024
# Objective: Map existence of pharmacies in Mexico
# -----------------------------------------------------------------------------

# Load auxiliary packages
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(readr, dplyr, magrittr, ggplot2, stringr, sf)

# Import data from DENUE
denue = read_csv("Dataton_src/DENUE/conjunto_de_datos/denue_inegi_46321-46531_.csv") |>
  mutate(nombre_act = str_to_lower(nombre_act)) |>
  filter(str_detect(nombre_act, "farm"))

# Import shapefile at the seccion level
Sys.setenv(OGR_GEOMETRY_ACCEPT_UNCLOSED_RING = "NO")
secciones = st_read("Dataton_src/MGS_SHAPEFILE_19OCT21/MGS_SHAPEFILE_19OCT21/SECCION.shp")

# Make the farmacias locations be coded in the same crs as the shapefile
farmacias = st_as_sf(denue, coords = c("longitud", "latitud"), crs = 4326) |>
  st_transform(st_crs(secciones))

# Map with a dot at each farmacia
ggplot()+
  geom_sf(data = secciones, fill = "white", color = "grey")+
  geom_sf(data = farmacias, color = "#750014", alpha = 0.2)+
  theme_minimal()

# Count the number of farmacias per seccion
farmacias_w_seccion = st_join(secciones, farmacias) |>
  group_by(SECCION) |>
  summarise(farm_num = n()) 

ggplot()+
  geom_sf(data = farmacias_w_seccion, aes(fill = farm_num), color = "white")+
  scale_fill_gradient(low = "white", high = "#750014", na.value = "white")+
  theme_minimal()
