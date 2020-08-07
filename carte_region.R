### Carte de l'aire d'étude ####

library(sf)
library(raster)
library(RColorBrewer)
library(scales)
library(graphicsutils)

### DATA ####

ecoregion <- st_read("../Quebec_data/data/ecoregion_simple.gpkg")

ecoregion <- st_transform(ecoregion, 32188)
ecoregion$SOUS_DOM6 <- factor(ecoregion$SOUS_DOM6, c("Sugar maple-bitternut hickory",
                                                     "Sugar maple-basswood",
                                                     "Sugar maple-yellow birch",
                                                     "Balsam fir-yellow birch",
                                                     "Balsam fir-white birch",
                                                     "Spruce-moss"))

xy <- st_read("../Quebec_data/data/plot_xy32198_nov2019.gpkg") 
xy <- st_set_crs(xy, 32198)
xy <- st_transform(xy, 32188)


### GET NORTH AMERICA ####

# get canada boundary map
can <- raster::getData("GADM", country = "CAN", level = 1, path = "data/")
us <- raster::getData("GADM", country = "US", level = 0, path = "data/")

# convert to sf
can_st <- st_as_sf(can)
us_st <- st_as_sf(us)

can_prj <- st_transform(can_st, 32188)
us_prj <- st_transform(us_st, 32188)

can_simple_prj <- st_simplify(can_prj, dTolerance = 800, preserveTopology = F)
us_simple_prj <- st_simplify(us_prj, dTolerance = 500, preserveTopology = F)



# Subset Quebec

qc <- can_simple_prj %>% subset(NAME_1 %in% c("Québec"))
qc_neigh <- can_simple_prj %>% subset(NAME_1 %in% c("Ontario", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Manitoba"))


reg_name = c("Érablière à caryer", "Érablière à tilleul",
             "Érablière à bouleau jaune", "Sapinière à bouleau jaune", 
             "Sapinière à bouleau blanc", "Pessière")


col_reg <- brewer.pal(6, "Spectral")

col_reg = c("#6f739d", "#a7a980", "#fac679", "#f9a469", "#be6f67", "#ba6363" #"#993140"
            )
#"#96274c")

col_reg = c("#6f739d", "#1f678b", "#a7a980", "#fac679", "#f9a469", "#be6f67")


## Graticule
bb <- st_bbox(qc)
bb <- c(-600000, 4560000, 1550000,  6860000)
grat <- st_graticule(bb, crs = 32188, lon = seq(-100,-50, by = 5))
grat_x <- grat[grat$type == "E",]
grat_y <- grat[grat$type == "N",]

### Cartes de la région ####

png("images/carte_region.png", width = 5.8, height = 5.5, res = 400, units = "in")
par(mar = c(1,1.7,.5,1), oma = c(0,0,0,.1), xpd = F)
plot(st_geometry(qc), border = "grey65", lwd = .9, col = "grey95",
     xlim = c(-400000, 1490000), ylim = c(5300000, 5900000))

box2(1:4, col = "grey55")

par(xpd = F)
plot(st_geometry(us_simple_prj), add = T, border = "grey65", lwd = .65, col = "grey75")
par(xpd = F)
plot(st_geometry(qc_neigh), add = T, border = "grey55", lwd = .65, col = "grey95")
par(xpd = F)
plot(st_geometry(qc), border = "grey35", lwd = 1, add = T)

par(xpd = F)
plot(st_geometry(grat), add = T, col = alpha('grey35', .3), lwd = .6) 

plot(ecoregion["SOUS_DOM6"], add = T, border = "grey35", lwd = .5, pal =  alpha(rev(col_reg),.4))

axis(1, at = grat_x$x_start, labels = paste(abs(grat_x$degree),"°W"),
     cex.axis = .7, line = -1, tick = FALSE)
axis(2, at = grat_y$y_start, labels = paste(grat_y$degree,"°N"), 
     cex.axis = .7, las = 1, line = -1.6, tick = FALSE)

text(-3.5e5, 5.7e6, "Canada")
text(4e5, 4.94e6, "États-Unis")

legend("bottomright", legend = rev(reg_name), fill = (col_reg), 
       bg = "white", box.col = "grey55", border = (col_reg), cex = .8, y.intersp = .99)

dev.off()

### Cartes des placettes ####

png("images/carte_placette.png", width = 5.8, height = 4, res = 400, units = "in")
par(mar = c(1,1.7,.5,1), oma = c(0,0,0,.1), xpd = F)
plot(st_geometry(qc), border = "grey65", lwd = .9, col = "grey95",
     xlim = c(-400000, 1490000), ylim = c(5300000, 5900000))

box2(1:4, col = "grey55")

par(xpd = F)
plot(st_geometry(us_simple_prj), add = T, border = "grey65", lwd = .65, col = "grey75")
par(xpd = F)
plot(st_geometry(qc_neigh), add = T, border = "grey55", lwd = .65, col = "grey95")
par(xpd = F)
plot(st_geometry(qc), border = "grey35", lwd = 1, add = T)

par(xpd = F)
plot(st_geometry(grat), add = T, col = alpha('grey35', .3), lwd = .6) 

plot(ecoregion["SOUS_DOM6"], add = T, border = "grey35", lwd = .5, pal =  alpha(rev(col_reg),.4))

axis(1, at = grat_x$x_start, labels = paste(abs(grat_x$degree),"°W"),
     cex.axis = .7, line = -1, tick = FALSE)
axis(2, at = grat_y$y_start, labels = paste(grat_y$degree,"°N"), 
     cex.axis = .7, las = 1, line = -1.6, tick = FALSE)

#text(3.5e5, 5.8e6, "Québec")
text(-3.5e5, 5.7e6, "Canada")
text(4e5, 4.94e6, "États-Unis")

plot(st_geometry(xy), add = TRUE, pch = 16, col = "#ffffff", bg = "#ffffff", cex = .2)

legend("bottomright", legend = rev(reg_name), fill = (col_reg), 
       bg = "white", box.col = "grey55", border = (col_reg), cex = .8, y.intersp = .99)

dev.off()


### Carte du réchauffement climatique ####

qc_wgs <- can %>% subset(NAME_1 %in% c("Québec"))

retrieveClimateData(years = c(1950:1970, 2000:2018), info =  "bio")

## Create a list of raster
ls_tp <- list()
k <- 0
for (year in c(1950:1970, 2000:2018)) {
        k <- k + 1
        # get July max temperature 
        ra <- raster(paste0("climateData/", year, "/bio_01.asc"))
        ## crop
        ls_tp[[k]] <- rasterize(x = qc_wgs, y = crop(ra, qc_wgs@bbox), mask = T)
}

## get a raster stack
tp_st1 <- do.call(stack, ls_tp[1:21])
tp_st2 <- do.call(stack, ls_tp[22:40])

tp_st1 = projectRaster(tp_st1, crs = crs(qc))
tp_st2 = projectRaster(tp_st2, crs = crs(qc))

# Changement climatique (delta TP)
cc <- mean(tp_st2)/10 - mean(tp_st1)/10

# Couleur
p_temp <- colorRampPalette(rev(brewer.pal(9, "Spectral")))

br <- seq(.3, 1.4, by = .02)
colo <- p_temp(length(br))

lab <- br
lab[-seq(6,56,10)] <- ""


# BB
bb <- st_bbox(qc)
bb <- c(-600000, 4600000, 1850000,  7260000)
grat <- st_graticule(bb, crs = 32188, lon = seq(-100,-50, by = 5))
grat_x <- grat[grat$type == "E",]
grat_y <- grat[grat$type == "N",]


png("images/intro_carte_cc.png", width = 5.8, height = 5.7, res = 400, units = "in")
par(mar = c(1,1.7,.5,1), oma = c(0,0,0,.1), xpd = F)
plot(st_geometry(qc), border = "grey65", lwd = .9, col = "grey95",
     xlim = c(-400000, 1550000), ylim = c(4800000, 7000000))

par(xpd = F)
plot(st_geometry(us_simple_prj), add = T, border = "grey65", lwd = .65, col = "grey75")
par(xpd = F)
plot(st_geometry(qc_neigh), add = T, border = "grey55", lwd = .65, col = "grey95")

par(xpd = F)
plot(st_geometry(grat), add = T, col = alpha('grey35', .3), lwd = .6) 

plot(cc, col = colo, add = TRUE, legend = FALSE)

lg <- legend("topright", legend = c("",""), 
       col = "transparent", 
       title = "Réchauffement entre 1950-1970 et 2000-201", 
       cex = .85, title.col = "transparent", box.col = "white")
colorScale(6e5, 68.6e5, col = colo, labels = lab, 
           percx = .4, percy = .02, labels.cex = .6, 
           title = "Réchauffement entre 1950-1970 et 2000-2018\n(∆Température)", 
           title.cex = .8)

par(xpd = F)
plot(st_geometry(qc), border = "grey35", lwd = 1.4, add = T)

axis(1, at = grat_x$x_start, labels = paste(abs(grat_x$degree),"°W"),
     cex.axis = .7, line = -1, tick = FALSE)
axis(2, at = grat_y$y_start, labels = paste(grat_y$degree,"°N"), 
     cex.axis = .7, las = 1, line = -.8, tick = FALSE)

box2(1:4, col = "grey55")

dev.off()
