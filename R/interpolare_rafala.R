library(raster)
library(rgdal)
library(climatetools)
library(classInt)
source("R/krige1_functii.R")

gfg
# citeste granitele
rou <- readOGR("shp", "romania")
proj4string(rou) <- CRS("+init=epsg:3844")


# citeste datele
dat <- read.csv("tab/rafale.csv")
# adauga coordonate
dat.co <- merge(dat, ws[,c("X", "Y","CODST")], by.x = "StID", by.y = "CODST")
coordinates(dat.co) <- ~X+Y
proj4string(dat.co) <- CRS("+init=epsg:3844")
plot(rou)
plot(dat.co, add = T, pch = 19)

# genereaza gridul pentru interpolare
grd <- expand.grid(x = seq(from = bbox(rou)[1,1], to = bbox(rou)[1,2], by = 1000),
                   y = seq(from = bbox(rou)[2,1], to = bbox(rou)[2,2], by = 1000),rbf = 0)
coordinates(grd) <- ~x + y
proj4string(grd) <- CRS("+init=epsg:3844")
gridded(grd) <- TRUE


# interpoleaza

rbf <- krige1(Raf_max ~ 1, dat.co, grd, model=v)
grd@data[,'rbf'] <- rbf
r <- raster(grd["rbf"])


# harta finala
r.geo <- projectRaster(r, crs = "+init=epsg:4326")
rou.geo <- spTransform(rou, CRS("+init=epsg:4326"))
r.geo <- mask(r.geo, rou.geo)
dat.co.geo <- spTransform(dat.co, CRS("+init=epsg:4326"))

brks <- classIntervals(dat.co$Raf_max, n = 5, style = "pretty")
nb <- length(brks$brks) - 1 
cols <- rev(topo.colors(nb))

png("png/Raf_max.png",width = 2000, height = 1480, units = "px", res = 200)
plot(r.geo, breaks = brks$brks, col = cols, lab.breaks = brks$brks)
plot(rou.geo, add = T)
text(coordinates(dat.co.geo)[,1],coordinates(dat.co.geo)[,2], dat.co.geo$Raf_max,
     col = "#636363")
grid()
dev.off()


