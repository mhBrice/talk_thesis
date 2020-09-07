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

ecoregion$SOUS_DOM3 <- factor(ecoregion$SOUS_DOM3, c("Boreal", "Mixed", "Hardwood"))

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

plot(ecoregion["SOUS_DOM6"], add = T, border = "grey35", lwd = .5, col = "transparent")

axis(1, at = grat_x$x_start, labels = paste(abs(grat_x$degree),"°W"),
     cex.axis = .7, line = -1, tick = FALSE)
axis(2, at = grat_y$y_start, labels = paste(grat_y$degree,"°N"), 
     cex.axis = .7, las = 1, line = -1.6, tick = FALSE)

text(-3.5e5, 5.7e6, "Canada")
text(4e5, 4.94e6, "États-Unis")

plot(st_geometry(xy), add = TRUE, pch = 21, col = alpha("black",.5), bg = alpha("black",.3), cex = .15)

# legend("bottomright", legend = rev(reg_name), fill = (col_reg), 
#        bg = "white", box.col = "grey55", border = (col_reg), cex = .8, y.intersp = .99)

dev.off()


### Carte des placettes utilisées dans le chapitre 3 ####

source("../recruitment/functions/my_maps.R")
sap_env <- readRDS("../recruitment/data/sap_env.RDS")

sap_sf <- sap_env %>% 
  st_as_sf() %>% 
  st_transform(32188)


xx = sap_sf %>% 
  group_by(ID_PE) %>%
  mutate(ACERUB = ifelse(any(ACERUB > 0),1,0),
         ACESAC = ifelse(any(ACESAC > 0),1,0),
         BETALL = ifelse(any(BETALL > 0),1,0),
         FAGGRA = ifelse(any(FAGGRA > 0),1,0)) %>% 
  slice(1)


reg25 <- ecoregion %>% filter(SOUS_DOM11 %in% c("2ouest","3ouest","4ouest", "5ouest"))
reg45 <- ecoregion %>% filter(SOUS_DOM11 %in% c("4ouest", "5ouest"))
reg5 <- ecoregion %>% filter(SOUS_DOM11 %in% c("5ouest"))
reg4 <- ecoregion %>% filter(SOUS_DOM11 %in% c("4ouest"))
reg3 <- ecoregion %>% filter(SOUS_DOM11 %in% c("3ouest"))
reg2 <- ecoregion %>% filter(SOUS_DOM11 %in% c("2ouest"))

col_reg <- brewer.pal(6,"Spectral")[c(1:3,5)]

reg_name <- reg_name[2:5]


bb <- st_bbox(qc)
bb <- c(-820000, 4050000, 1600000,  7050000)
grat <- st_graticule(bb, crs = 32188, lon = seq(-100,-50, by = 5))
grat_x <- grat[grat$type == "E",]
grat_y <- grat[grat$type == "N",]

# Context map

mat <- matrix(c(1,1,2,3,1,1,4,5), 2, byrow = T)

png("images/chap3_carte.png", width = 7.1, height = 3.3, res = 600, units = "in")
#quartz(width = 7.5, height = 4)
layout(mat, widths = c(.5,1))
par(mar = c(1,1.7,.5,1), oma = c(0,0,0,.1), xpd = F)
plot(st_geometry(qc), border = "grey65", lwd = .9, col = "grey95",
     xlim = c(-700000, 1450000), ylim = c(4750000, 6750000))

box2(1:4, col = "grey55")

par(xpd = F)
plot(st_geometry(us_simple_prj), add = T, border = "grey65", lwd = .65, col = "grey75")
par(xpd = F)
plot(st_geometry(qc_neigh), add = T, border = "grey55", lwd = .65, col = "grey95")
par(xpd = F)
plot(st_geometry(qc), border = "grey35", lwd = 1, add = T)

par(xpd = F)
plot(st_geometry(grat), add = T, col = alpha('grey35', .5), lwd = .6) 

plot(st_geometry(reg25), add = T, border = "grey35", lwd = .5, col = rev(col_reg))

axis(1, at = grat_x$x_start, labels = paste(abs(grat_x$degree),"°W"),
     cex.axis = .7, line = -1, tick = FALSE)
axis(2, at = grat_y$y_start, labels = paste(grat_y$degree,"°N"), 
     cex.axis = .7, las = 1, line = -0.9, tick = FALSE)


text(-5e5, 5.8e6, "Canada")
text(0, 4.6e6, "États-Unis")

# Legend
legend("bottomright", legend = rev(reg_name), fill = rev(col_reg), 
       bg = "white", box.col = "grey55", border = rev(col_reg), cex = .8)


## ACERUB
par(mar = c(.3,.5,.1,0))
my_maps(ecoregion = reg25, pts = subset(xx, ACERUB > 0)["ACERUB"],
        axis = 2, sp = spnames[1], reg = c("4ouest", "5ouest"))

## ACESAC
par(mar = c(.3,.5,.1,0))
my_maps(ecoregion = reg25, pts = subset(xx, ACESAC > 0)["ACESAC"],
        axis = NULL, sp = spnames[2], reg = c("4ouest"))

## BETALL
par(mar = c(.3,.5,.1,0))
my_maps(ecoregion = reg25, pts = subset(xx, BETALL > 0)["BETALL"],
        axis = 1:2, sp = spnames[3], reg = c("4ouest"))

## FAGGRA
par(mar = c(.3,.5,.1,0))
my_maps(ecoregion = reg25, pts = subset(xx, FAGGRA > 0)["FAGGRA"],
        axis = 1, sp = spnames[4], reg = c("3ouest"))



dev.off()



### Carte du réchauffement climatique ####

qc_wgs <- can %>% subset(NAME_1 %in% c("Québec"))

years <- c(1950:2018)
retrieveClimateData(years = years, info =  "bio")

## Create a list of raster
ls_tp <- list()
k <- 0
for (year in years) {
        k <- k + 1
        # get July max temperature 
        ra <- raster(paste0("climateData/", year, "/bio_01.asc"))
        ## crop
        ls_tp[[k]] <- rasterize(x = qc_wgs, y = crop(ra, qc_wgs@bbox), mask = T)
}

## get a raster stack
tp_st <- do.call(stack, ls_tp)
tp_st1 <- do.call(stack, ls_tp[1:21])
tp_st2 <- do.call(stack, ls_tp[51:69])

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


### Tendance climatique ####

tp_st_prj = projectRaster(tp_st, crs = crs(qc))
tp_st_merid <- rasterize(x = ecoregion, y = tp_st_prj, mask = T)

mean_TP <- cellStats(tp_st_merid, mean)

lm_TP <- lm(mean_TP/10 ~ years)
summary(lm_TP)



png("images/intro_clim_trend.png",
    width = 4, height = 3.5, res = 300, units = "in")
# quartz(width = 4, height = 3.5)
par(mar = c(3.5,4,.5,.5))

plot(mean_TP/10 ~ years, type = "l", las = 1,
     xlab = "", ylab = "", cex.axis=.8, xaxs = "i", yaxs = "i",
     col = "grey45", lwd = 1.2, axes = F, frame.plot = TRUE)
axis(2, cex.axis=.75, las = 1)
axis(1, cex.axis=.75)
abline(lm_TP, lwd = 1.2, col = "red3")
mtext(paste("Pente =", round(lm_TP$coef[2],3), "°C/année"),
      3, line = -1, at = 1960, adj = 0,
      cex = .85)
mtext("p-value < 0.001",
      3, line = -2, at = 1960, adj = 0,
      cex = .85)

mtext("Température annuelle moyenne\ndu Québec mériodional", 2, line = 2, cex = 0.8, font = 2)
mtext("Année", 1, line = 2.2, cex = 0.8, font = 2)


dev.off()


### Carte conclusion ####

png("images/conclu_carte2.png", width = 5, height = 3, res = 300, units = "in", bg = "transparent")
par(mar = c(.5,.5,.5,.5))

# plot(ecoregion["SOUS_DOM3"],  border = "grey35", lwd = .5, 
#      pal =  alpha(c("#158282", "#A1BD93","#D43650"),.3), main = NULL, 
#      key.pos = NULL)

plot(ecoregion["SOUS_DOM6"],  border = "grey35", lwd = .5, 
     pal =  alpha(c("#D43650","#D43650","#D43650", "#D43650","#A1BD93","#158282"),.3), main = NULL, 
     key.pos = NULL)

dev.off()
