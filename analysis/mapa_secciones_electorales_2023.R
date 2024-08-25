# -----------------------------------------------------------------------------
#                                   Dataton
# 
# Author: Roberto Gonzalez
# Date: August 20, 2024
# Objective: Map existence of pharmacies in Mexico
# -----------------------------------------------------------------------------

# Omit scientific notation
options(scipen = 99999)

# Load auxiliary packages
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(readr, dplyr, magrittr, ggplot2, stringr, sf, RColorBrewer)

# Import data from DENUE
denue = read_csv("Dataton_src/DENUE/conjunto_de_datos/denue_inegi_46321-46531_.csv") |>
  mutate(nombre_act = str_to_lower(nombre_act)) |>
  filter(str_detect(nombre_act, "farm"))

# Import data from the 2020 census for attributes
censo = read_csv("Dataton_src/Censo_2020/conjunto_de_datos/INE_SECCION_2020.csv") |>
  janitor::clean_names()

# Import shapefile at the seccion level
Sys.setenv(OGR_GEOMETRY_ACCEPT_UNCLOSED_RING = "NO")
secciones = st_read("Dataton_src/MGS_SHAPEFILE_19OCT21/MGS_SHAPEFILE_19OCT21/SECCION.shp")

# Add attributes (i.e. census variables) to each seccion electoral
secciones_full = left_join(secciones, censo, 
                            by = c("SECCION" = "seccion",
                                   "ENTIDAD" = "entidad",
                                   "MUNICIPIO" = "municipio")) |>
  mutate(area_seccion_m2 = st_area(geometry),
         area_seccion_km2 = area_seccion_m2/1000,
         pob60plus_per_km2 = p_60ymas/area_seccion_km2)

# Make the farmacias locations be coded in the same crs as the shapefile
farmacias = st_as_sf(denue, coords = c("longitud", "latitud"), crs = 4326) |>
  st_transform(st_crs(secciones_full))

ggplot()+
  geom_sf(data=secciones, color = "grey",fill = "white", size = 0.5)

# Map with a dot at each farmacia
ggplot()+
  geom_sf(data = secciones_full, fill = "white", color = "grey")+
  geom_sf(data = farmacias, color = "#750014", alpha = 0.2, size = 0.5)+
  theme_minimal()

# Count the number of farmacias per seccion
farmacias_w_seccion = st_join(secciones_full, farmacias) |>
  group_by(SECCION) |>
  summarise(farm_num = n(), 
            area_seccion_km2 = mean(area_seccion_km2),
            pob60plus_per_km2 = mean(pob60plus_per_km2)) |>
  filter(as.numeric(pob60plus_per_km2) <= 15) |>
  mutate(pharmacies_per_km2 = farm_num/area_seccion_km2,
         pharm_dens_cat = cut(as.numeric(pharmacies_per_km2), 
                              breaks = 3, 
                              labels = c("Low", "Medium", "High")),
         sixtyplus_dens_cat = cut(as.numeric(pob60plus_per_km2), 
                                  breaks = 3,
                                  labels = c("Low", "Medium", "High")))

ggplot()+
  geom_sf(data = farmacias_w_seccion, aes(fill = farm_num), color = "white")+
  scale_fill_gradient(low = "grey90", high = "#750014", na.value = "white")+
  theme_minimal()+
  labs(fill = "N. Farmacias")

# Map with bivariate color scale
bivariate_color_scale = c("Low.Low" = "white",
                           "Medium.Low" = "#e1edf3",
                           "High.Low" = "#b1d4e7",
                           "Low.Medium" = "#f3e1e1",
                           "Medium.Medium" = "#d2bfca",
                           "High.Medium" = "#b0a7b3",
                           "Low.High" = "#e7b1b1",
                           "Medium.High" = "#ca9fa0",
                           "High.High" = "#ad8f8f")

farmacias_w_seccion = farmacias_w_seccion |>
  mutate(bivariate_category = paste(pharm_dens_cat, sixtyplus_dens_cat, sep = "."),
         fillcolor = bivariate_color_scale[bivariate_category])

ggplot()+
  geom_sf(data = farmacias_w_seccion, aes(fill = fillcolor), color = "white")+
  theme_minimal()+
  labs(fill = "Farmacias x 60+")
