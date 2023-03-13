library(tidyverse)
library(stars)
library(terra)
library(tidyterra)
library(rnaturalearth)
library(rnaturalearthdata)
library(rhdf5)

# crs_guess <- '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs '
crs_guess <- '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
extent_guess <- c(49.635986, 60.109381, -121.69024, -77.457538)
res_guess <-  units::set_units(370.650173222222, m)
size_guess <- 3000

h5 <- 'path/to/data'

h5ls(h5)

x <- h5read(h5, name = '/HDFEOS/GRIDS/NPP_Grid_IMG_2D/XDim') %>%
  units::set_units(m)
# x_mat <- matrix(data = rep(x, size_guess), nrow = size_guess)

y <- h5read(h5, name = '/HDFEOS/GRIDS/NPP_Grid_IMG_2D/YDim') %>% 
  `-`(18000) %>% 
  units::set_units(m)
# y_mat <- matrix(data = rep(y, size_guess), nrow = size_guess, byrow = T)

world <- ne_countries(scale = "medium", returnclass = "sf")



st <- read_stars(
  h5,
  # RasterIO = list(
  #   nXSize = size_guess,
  #   nYSize = size_guess,
  #   nBufXSize = size_guess * res_guess,
  #   nBufYSize = size_guess * res_guess
  # ),
  # curvilinear = c('//HDFEOS/GRIDS/NPP_Grid_IMG_2D/XDim', '//HDFEOS/GRIDS/NPP_Grid_IMG_2D/YDim')
  # curvilinear = list(x = x_mat, y = y_mat)
  )

attr(st, 'dimensions')$x$offset <- min(x)
attr(st, 'dimensions')$y$offset <- max(y)
attr(st, 'dimensions')$x$delta <- res_guess
attr(st, 'dimensions')$y$delta <- -res_guess

st_crs(st) <- crs_guess

# st


# st %>% hist()

st %>% names()

# st %>% pull(5) %>% view()

# plot(st[5], axes = T)

ggplot() +
  geom_stars(data = st[3],
             show.legend = F) +
  geom_sf(data = world,
          colour = 'red',
          fill = '#00000000') +
  coord_sf(crs = crs_guess,
           xlim = range(x),
           ylim = range(y)) 
  

terra::describe(
  h5,
  meta = T,
  parse = T
)

ra <- terra::rast(
  h5
  )

# res(ra) <- rep(370.650173222222, 2)

crs(ra) <- crs_guess

ext(ra) <- extent_guess

plot(ra[[5]])

widen <- 1

ggplot() + 
  # geom_sf(data = world, fill = 'white') +
  geom_spatraster(data = ra[[5]]) +
  coord_sf(xlim = c(49.635986 - widen, 60.109381 + widen),
           ylim =  c(-121.69024 - widen, -77.457538 + widen)) +
  theme_bw() 
  
ra

tif_f <-  system.file("tif/L7_ETMs.tif", package = "stars")
tif <- read_stars(tif_f)
