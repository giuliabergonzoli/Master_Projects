library(sp)
library(rgdal)
library(raster)
library(plotly)
library(akima)

# load Italy spatial data
italy <- getData('GADM', country = 'Italy', level = 1)
italy$NAME_1
lomb_emil_map <- italy[c(10,6),]  #Lombardy and Emil columns

# 820 points
grid <- makegrid(lomb_emil_map, cellsize = 0.08)

# 185 points
#grid <- makegrid(lomb_emil_map, cellsize = 0.17)

# grid is a data.frame. To change it to a spatial data set we have to
grid <- SpatialPoints(grid, proj4string = CRS(proj4string(lomb_emil_map)))
grid <- grid[lomb_emil_map,]

plot(lomb_emil_map)
plot(grid, pch=16, add = T, cex=0.5)
plot(coords_old, pch=1, add=T, cex=1, col=nugget_col)

new_nugget = grid@coords

##############
# 185 points
#mean_nugget = read.csv("resisuals_grid_200.csv", header=TRUE, dec = '.', sep = ',')
# 820 points
mean_nugget = read.csv("nugget_mean_new.csv", header=TRUE, dec = '.', sep = ',')

coords_old_data = read.csv("lat_long_to_python.csv", header=TRUE, dec = '.', sep = ';')
nugget_old = read.csv("mean_nugget_old.csv", header=TRUE, dec = '.', sep = ',')

coords_old_data = as.matrix(coords_old_data[,c(4,3)])
coords_old <- SpatialPoints(coords_old_data, proj4string = CRS(proj4string(lomb_emil_map)))

mean_nugget = mean_nugget[,2]
nugget_old = nugget_old[,2]

# convert SpatialPolygonsDataFrame object to data.frame
rwa2 <- fortify(lomb_emil_map)
class(rwa2)

df= data.frame(Lon=new_nugget[,1],Lat=new_nugget[,2],Spatial=mean_nugget)
df_2 = data.frame(Lon=coords_old_data[,1],Lat=coords_old_data[,2],Spatial=nugget_old)

rbPal <- colorRampPalette(c('blue','white','red'))
nugget_col <- rbPal(30)[as.numeric(cut(nugget_old,breaks = 30))]

scale_fill_gradient2(
  low = "blue",
  mid = "white",
  high = "red",
  midpoint = 0,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "fill"
)

scale_fill_continuous(name = "Spatial residuals",
                      low="blue",high="red")

# plot map and raindata  
ggplot() + 
  geom_polygon(data = rwa2, aes(x = long, y = lat, group = group),
               colour = "black", size = 0.8, fill = "white") +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = Spatial),alpha=0.6) +
  stat_contour(data = df, aes(x = Lon, y = Lat, z = Spatial)) +
  geom_point(data=df_2, aes(x=Lon, y=Lat),col=nugget_col,cex=4)+
  geom_point(data=df_2, aes(x=Lon, y=Lat),shape=1,cex=4)+
  ggtitle("Spatial residuals") +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill"
  )+
  theme_bw() +
  theme(plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20, vjust = -0.5),
        axis.title.y = element_text(size = 20, vjust = 0.2),
        legend.text = element_text(size = 10)) +
  coord_map()

######### write_tables ##########


library(geosphere)
nugget_new_cov = distm(new_nugget)
nugget__new_cov = nugget_new_cov/1000

write.table(nugget_new_cov,"grid_to_python.csv", sep = ";", dec = ".", row.names=FALSE)
write.table(new_nugget,"grid_lat_long_to_python.csv", sep = ";", dec = ".", row.names=FALSE)

#transform new_nugget in UTM coordinates
library(rgdal)
cord.dec = SpatialPoints(cbind(new_nugget[,1], -new_nugget[,2]), proj4string=CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:32748"))
cord.UTM@coords

write.table(cord.UTM@coords,"grid_UTMcoords_820.csv", sep = ";", dec = ".", row.names=FALSE)
