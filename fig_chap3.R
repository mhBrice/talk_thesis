### Figures du chapitre 3 - Recrutement #####


library(countreg)
library(dplyr)

# Fonctions R

source('funs.R')

#source("../recruitment/functions/plot_shift.R")

#source('../recruitment/functions/plot_coef.R')

# Données

sap12 <- readRDS("../recruitment/data/sap12.RDS")

sap_histo <- sap12 %>% 
  filter(time == "1") %>% 
  arrange(ID_PE)

sap_recent <- sap12 %>% 
  filter(time == "2") %>%  
  arrange(ID_PE)

mhurd <- readRDS("../recruitment/res/mod_hurdle.RDS")


### 1.  Shift conceptuel ####


seqx <- seq(-12, 20, .01)
col1 <- "grey35"
col2 <- "red3"

png("images/chap3_shift_50.png", width = 6, height = 4, res = 300, units= "in")
par(mar = c(3,3,2,3))
plot0(c(-12, 20), c(0, .15))

lines(seqx, dnorm(seqx, 0, sd = 3.2), col = col1, lwd = 2)
text(-8, .07, "Distribution\nhistorique\n(1970-1981)", 
     cex = 1, col = col1, adj = .5, xpd = NA)

lines(seqx, dnorm(seqx, 6, sd = 3), col= col2, lwd = 2)
text(14, .07, "Distribution\ncontemporaine\n(2005-2018)", 
     cex = 1, col = col2, adj = .5, xpd = NA)

abline(v = 0, col = col1, lwd = 1.3)

abline(v = 6, col = col2, lwd = 1.3)

points(x = c(0,6), y = c(-0.01,-0.01), xpd = NA,
       pch = 21, col = c(col1, col2), bg = alpha(c(col1, col2), .2), 
       lwd = 1.5, cex = 1.2)

mtext("Latitude", 1, line = 1, font = 2)
mtext("Fréquence d'occurrence", 2, line = 1, font = 2)

arrows(x0 = 0, x1 = 6, y0 = .16, length = .1, lwd = 1.2, xpd = NA)
mtext("Déplacement de la médiane vers le nord", 3, line = 1)

dev.off()



### 2. Shift en latitude des 4 espèces ####


xlim <- c(2e+5, 6e+5)

br <- seq(1.8e+5, xlim[2], by = 20000)

line <- seq(xlim[1], xlim[2], by = 1e5)

### metres to degrees

x = st_set_crs(st_sfc(st_point(c(-206660, 200000)), 
                    st_point(c(-206660, 400000)),
                    st_point(c(-206660, 600000))), 32198)

st_transform(x, 4269)

latdeg <- c(45.76, 47.56, 49.37)



mySpecies <- c("ACERUB", "ACESAC", "BETALL", "FAGGRA")
spnames <- c("Acer rubrum", "Acer saccharum", "Betula alleghaniensis", "Fagus grandifolia")

## Peu ou pas - Historique seulement ####

png("images/chap3_lat_coupe01.png", width = 9, height = 3.7, res = 300, units ="in")
layout(matrix(c(1:4,5,5,5,5, 6,6,6,6), 3, byrow = T), heights = c(1, .15, .15))
par(mar = c(2.5,2,3.2,1), cex = .8)  
for(i in 1:4) {
  
  sp <- mySpecies[i]
  
  tmp1 <- sap_histo[which(sap_histo[,sp]>0),]
  tmp2 <- sap_recent[which(sap_recent[,sp]>0),]
  
  tmp1d <- tmp1$lat[which(tmp1$logging==0 & tmp1$natural==0)]
  tmp2d <- tmp2$lat[which(tmp2$logging==0 & tmp2$natural==0)]
  
  ymax = max(c(hist(tmp1d, breaks = br, plot = F)$counts, hist(tmp2d, breaks = br, plot = F)$counts))
  plot_shift(tmp1d, xlim = xlim, ylim = c(0, ymax),
             line = line,  br = br, unit = 1/1000, main = spnames[i])
  axis(2, cex = .9, line = -.5, las = 1, tick = FALSE)
  axis(1, tick = F, at = seq(2e5, xlim[2], by = 2e5), 
       mgp = c(3,.4,0), labels = seq(2e5, xlim[2], by = 2e5)/1000)
  axis(1, tick = F, at = seq(2e5, xlim[2], by = 2e5), 
       cex.axis = .85,
       mgp = c(3,1.3,0), labels = paste0("(",latdeg,"°)"))
  
}

par(mar = c(0,2,0,1))
plot0(text = "Latitude (km)", cex = 1.3)
arrows(x0 = -.6, x1 = .6, y0 = -.9, xpd = NA, lwd = 1.7, length = .1)

plot0()
legend("bottom", legend = c("Distribution historique", "Distribution contemporaine"), 
       fill = alpha(c("grey15", "red3"), .1), border = c("grey15", "red3"),
       cex = 1.3, bty = "n", horiz = T)

dev.off()


## Peu ou pas ####

png("images/chap3_lat_coupe0.png", width = 9, height = 3.7, res = 300, units ="in")
layout(matrix(c(1:4,5,5,5,5, 6,6,6,6), 3, byrow = T), heights = c(1, .15, .15))
par(mar = c(2.5,2,3.2,1), cex = .8)  
for(i in 1:4) {
  
  sp <- mySpecies[i]
  
  tmp1 <- sap_histo[which(sap_histo[,sp]>0),]
  tmp2 <- sap_recent[which(sap_recent[,sp]>0),]
  
  tmp1d <- tmp1$lat[which(tmp1$logging==0 & tmp1$natural==0)]
  tmp2d <- tmp2$lat[which(tmp2$logging==0 & tmp2$natural==0)]
  
  ymax = max(c(hist(tmp1d,breaks = br, plot = F)$counts, hist(tmp2d, breaks = br, plot = F)$counts))
  plot_shift(tmp1d, tmp2d,xlim = xlim, ylim = c(0, ymax),
             line = line,  br = br, unit = 1/1000, main = spnames[i])
  axis(2, cex = .9, line = -.5, las = 1, tick = FALSE)
  axis(1, tick = F, at = seq(2e5, xlim[2], by = 2e5), 
       mgp = c(3,.4,0), labels = seq(2e5, xlim[2], by = 2e5)/1000)
  axis(1, tick = F, at = seq(2e5, xlim[2], by = 2e5), 
       cex.axis = .85,
       mgp = c(3,1.3,0), labels = paste0("(",latdeg,"°)"))
  
}

par(mar = c(0,2,0,1))
plot0(text = "Latitude (km)", cex = 1.3)
arrows(x0 = -.6, x1 = .6, y0 = -.9, xpd = NA, lwd = 1.7, length = .1)

plot0()
legend("bottom", legend = c("Distribution historique", "Distribution contemporaine"), 
       fill = alpha(c("grey15", "red3"), .1), border = c("grey15", "red3"),
       cex = 1.3, bty = "n", horiz = T)

dev.off()


## Modérée ####

png("images/chap3_lat_coupe1.png",  width = 9, height = 3.7, res = 300, units ="in")
layout(matrix(c(1:4,5,5,5,5, 6,6,6,6), 3, byrow = T), heights = c(1, .15, .15))
par(mar = c(2.5,2,3.2,1), cex = .8)  
for(i in 1:4) {
  
  sp <- mySpecies[i]
  
  tmp1 <- sap_histo[which(sap_histo[,sp]>0),]
  tmp2 <- sap_recent[which(sap_recent[,sp]>0),]
  
  tmp1d <- tmp1$lat[which(tmp1$logging==1 & tmp1$natural==0)]
  tmp2d <- tmp2$lat[which(tmp2$logging==1 & tmp2$natural==0)]
  
  ymax = max(c(hist(tmp1d, breaks = br, plot = F)$counts, hist(tmp2d, breaks = br, plot = F)$counts))
  plot_shift(tmp1d, tmp2d, xlim = xlim, ylim = c(0, ymax),
             line = line,  br = br, unit = 1/1000, main = spnames[i])
  axis(2, cex = .9, line = -.5, las = 1, tick = FALSE)
  axis(1, tick = F, at = seq(2e5, xlim[2], by = 2e5), 
       mgp = c(3,.4,0), labels = seq(2e5, xlim[2], by = 2e5)/1000)
  axis(1, tick = F, at = seq(2e5, xlim[2], by = 2e5), 
       cex.axis = .85,
       mgp = c(3,1.3,0), labels = paste0("(",latdeg,"°)"))

}

par(mar = c(0,2,0,1))
plot0(text = "Latitude (km)", cex = 1.3)
arrows(x0 = -.6, x1 = .6, y0 = -.9, xpd = NA, lwd = 1.7, length = .1)

plot0()
legend("bottom", legend = c("Distribution historique", "Distribution contemporaine"), 
       fill = alpha(c("grey15", "red3"), .1), border = c("grey15", "red3"),
       cex = 1.3, bty = "n", horiz = T)
dev.off()


## Majeure ####

png("images/chap3_lat_coupe2.png", width = 9, height = 3.7, res = 300, units ="in")
layout(matrix(c(1:4,5,5,5,5, 6,6,6,6), 3, byrow = T), heights = c(1, .15, .15))
par(mar = c(2.5,2,3.2,1), cex = .8)  
for(i in 1:4) {
  
  sp <- mySpecies[i]
  
  tmp1 <- sap_histo[which(sap_histo[,sp]>0),]
  tmp2 <- sap_recent[which(sap_recent[,sp]>0),]
  
  tmp1d <- tmp1$lat[which(tmp1$logging==2 & tmp1$natural==0)]
  tmp2d <- tmp2$lat[which(tmp2$logging==2 & tmp2$natural==0)]
  
  ymax = max(c(hist(tmp1d, breaks = br, plot = F)$counts, hist(tmp2d, breaks = br, plot = F)$counts))
  plot_shift(tmp1d, tmp2d, xlim = xlim, ylim = c(0, ymax),
             line = line,  br = br, unit = 1/1000, main = spnames[i])
  axis(2, cex = .9, line = -.5, las = 1, tick = FALSE)
  axis(1, tick = F, at = seq(2e5, xlim[2], by = 2e5), 
       mgp = c(3,.4,0), labels = seq(2e5, xlim[2], by = 2e5)/1000)
  axis(1, tick = F, at = seq(2e5, xlim[2], by = 2e5), 
       cex.axis = .85,
       mgp = c(3,1.3,0), labels = paste0("(",latdeg,"°)"))
 
}

par(mar = c(0,2,0,1))
plot0(text = "Latitude (km)", cex = 1.3)
arrows(x0 = -.6, x1 = .6, y0 = -.9, xpd = NA, lwd = 1.7, length = .1)

plot0()
legend("bottom", legend = c("Distribution historique", "Distribution contemporaine"), 
       fill = alpha(c("grey15", "red3"), .1), border = c("grey15", "red3"),
       cex = 1.3, bty = "n", horiz = T)

dev.off()


### 2.1 Shift en latitude Acer rubrum ####


xlim <- c(2e+5, 6e+5)

br <- seq(1.8e+5, xlim[2], by = 20000)

line <- seq(xlim[1], xlim[2], by = 1e5)

### metres to degrees

x = st_set_crs(st_sfc(st_point(c(-206660, 200000)), 
                      st_point(c(-206660, 400000)),
                      st_point(c(-206660, 600000))), 32198)

st_transform(x, 4269)

latdeg <- c(45.76, 47.56, 49.37)



mySpecies <- "ACERUB"
spnames <- "Acer rubrum"

## Historique seulement ####

sp <- "ACERUB"
d_names <- c("Peu ou pas de coupe", "Coupe modérée", "Coupe majeure")

tmp1 <- sap_histo[which(sap_histo[,sp]>0),]
tmp2 <- sap_recent[which(sap_recent[,sp]>0),]



png("images/chap3_lat_AR1.png", width = 8, height = 3.7, res = 300, units ="in")

layout(matrix(c(1:3,4,4,4, 5,5,5), 3, byrow = T), heights = c(1, .15, .15))
par(mar = c(2.5,2,3.2,1), cex = .8)  

for(i in 1:3) { 
  tmp1d <- tmp1$lat[which(tmp1$logging==i-1 & tmp1$natural==0)]
  tmp2d <- tmp2$lat[which(tmp2$logging==i-1 & tmp2$natural==0)]
  
  ymax = max(c(hist(tmp1d, breaks = br, plot = F)$counts, hist(tmp2d, breaks = br, plot = F)$counts))
  plot_shift(tmp1d, xlim = xlim, ylim = c(0, ymax),
             line = line,  br = br, unit = 1/1000, main = d_names[i])
  axis(2, cex = .9, line = -.5, las = 1, tick = FALSE)
  axis(1, tick = F, at = seq(2e5, xlim[2], by = 2e5), 
       mgp = c(3,.4,0), labels = seq(2e5, xlim[2], by = 2e5)/1000)
  axis(1, tick = F, at = seq(2e5, xlim[2], by = 2e5), 
       cex.axis = .85,
       mgp = c(3,1.3,0), labels = paste0("(",latdeg,"°)"))
  }
  
par(mar = c(0,2,0,1))
plot0(text = "Latitude (km)", cex = 1.3)
arrows(x0 = -.6, x1 = .6, y0 = -.9, xpd = NA, lwd = 1.7, length = .1)

plot0()
legend("bottom", legend = c("Distribution historique", "Distribution contemporaine"), 
       fill = alpha(c("grey15", "red3"), .1), border = c("grey15", "red3"),
       cex = 1.3, bty = "n", horiz = T)
  
dev.off()


## AVANT-APRES ####

png("images/chap3_lat_AR2.png", width = 8, height = 3.7, res = 300, units ="in")

layout(matrix(c(1:3,4,4,4, 5,5,5), 3, byrow = T), heights = c(1, .15, .15))
par(mar = c(2.5,2,3.2,1), cex = .8)  

for(i in 1:3) { 
  tmp1d <- tmp1$lat[which(tmp1$logging==i-1 & tmp1$natural==0)]
  tmp2d <- tmp2$lat[which(tmp2$logging==i-1 & tmp2$natural==0)]
  
  ymax = max(c(hist(tmp1d, breaks = br, plot = F)$counts, hist(tmp2d, breaks = br, plot = F)$counts))
  plot_shift(tmp1d, tmp2d, xlim = xlim, ylim = c(0, ymax),
             line = line,  br = br, unit = 1/1000, main = d_names[i])
  axis(2, cex = .9, line = -.5, las = 1, tick = FALSE)
  axis(1, tick = F, at = seq(2e5, xlim[2], by = 2e5), 
       mgp = c(3,.4,0), labels = seq(2e5, xlim[2], by = 2e5)/1000)
  axis(1, tick = F, at = seq(2e5, xlim[2], by = 2e5), 
       cex.axis = .85,
       mgp = c(3,1.3,0), labels = paste0("(",latdeg,"°)"))
}


par(mar = c(0,2,0,1))
plot0(text = "Latitude (km)", cex = 1.3)
arrows(x0 = -.6, x1 = .6, y0 = -.9, xpd = NA, lwd = 1.7, length = .1)

plot0()
legend("bottom", legend = c("Distribution historique", "Distribution contemporaine"), 
       fill = alpha(c("grey15", "red3"), .1), border = c("grey15", "red3"),
       cex = 1.3, bty = "n", horiz = T)

dev.off()



### 3. Histogramme de recrutement - zero inflated ####

sap_env <- readRDS("../recruitment/data/sap_env.RDS")


png("images/chap3_recrue_hist.png", width = 4.5, height = 4, res = 300, units = "in")
par(mar = c(4.5,4,.5,.5))
hist(sap_env$ACERUB[which(sap_env$ecoreg3=="Mixed")], 
     breaks = 30, xlim = c(0, 30), col = "red3", border = "#9e0d0d",
     las = 1, main = "",
     ylab = "Fréquence", xlab = "", font.lab = 2,
     cex.axis = .8)

mtext("Nombre de recrues de Acer rubrum\ndans la sapinière à bouleau jaune", 1,  line = 3.2, adj = .55, font = 2)

dev.off()

### 4. Hurdle ####

## presenter les 4 espèces, juste zero part avec des bars foncé/pâle pour les variables.


r2z <- c(20.76, 45.37, 16.87, 40.43)
r2c <- c(2.86, 4.33, 3.39, 4.86)

allnames <- c("sTP", "sCMI", "slope_sTP", "slope_sCMI", 
              "phacid", "phbasic", 
              "drainagexeric",  "drainagehydric", 
              "VERSANTH", "VERSANTM",
              "A_BA_focal", "N_focal", "A_BA_boreal", 
              "logging1", "logging2", "natural1",
              "age_mean2")

labnames <- c("Température", "CMI", "Delta*Température", "Delta*CMI", 
              "pH~acide", "pH~basique", 
              "sol~xérique", "sol~hydrique", 
              "haut~de~pente", "milieu~de~pente",
              "conspécifique[local]", "conspécifique[voisin]", "boréal[local]",
              "coupe~modérée", "coupe~majeure",
              "perturbation~naturelle",
              "âge")


labnames <- parse(text = labnames)


gr = list(Climatique = 1:4, 
          "Topo-édaphique" = 5:10,
          Biotique = 11:13, 
          Perturbation = 14:17)

### Zero part ####

png("images/chap3_hurdleZ.png", width = 10, height = 5, res = 300, units = "in")

#quartz(width = 10, height = 5)
par(oma = c(1,0,0,.2))
layout(matrix(c(1:5), 1), widths = c(.9,1,1,1,1))

par(mar = c(3,0,3,0))

plot0(xlim = c(-1, 1), ylim = c(length(labnames), 1))
for(g in 1:length(gr)) {
  grl <- gr[[g]]
  text(-.9, mean(grl), labels = names(gr[g]), cex = 1.4, xpd = NA, srt = 90)
  arrows(y0 = min(grl)-.1, y1 = max(grl)+.1, x0 = -.7, 
         angle = 90, code = 1, 
         length = 0, col = "grey15", lwd = 1.5)
  
}

par(mar = c(3,1,3,0))
for(i in 1:4){

  xlabels <- ifelse(i==1, TRUE, FALSE)

  barplot_coef(mhurd[[i]], lab = labnames, gr = gr, type = "zero",
               xlabels = xlabels, ylab = FALSE, 
               allnames = allnames, 
               x_cex = 0.9, 
               pt_cex = 1.7, pt_lwd = 1.3,
               ci_lwd = 1.7)
  
  mtext(spnames[i], 3, line= 1.5, xpd = NA, font = 4)
  mtext(bquote("R"^2~.(r2z[i])*"%"), 3, line=0, xpd = NA, cex = .9)
}

mtext("Coefficient de pente", 1, outer = T, line = 0, at = .55)



dev.off()


### Count part #####

png("images/chap3_hurdleC.png", width = 10, height = 5, res = 300, units = "in")

#quartz(width = 10, height = 5)
par(oma = c(1,0,0,.2))
layout(matrix(c(1:5), 1), widths = c(.9,1,1,1,1))

par(mar = c(3,0,3,0))

plot0(xlim = c(-1, 1), ylim = c(length(labnames), 1))
for(g in 1:length(gr)) {
  grl <- gr[[g]]
  text(-.9, mean(grl), labels = names(gr[g]), cex = 1.4, xpd = NA, srt = 90)
  arrows(y0 = min(grl)-.1, y1 = max(grl)+.1, x0 = -.7, 
         angle = 90, code = 1, 
         length = 0, col = "grey15", lwd = 1.5)
  
}

par(mar = c(3,1,3,0))
for(i in 1:4){
  
  xlabels <- ifelse(i==1, TRUE, FALSE)
  
  barplot_coef(mhurd[[i]], lab = labnames, gr = gr, type = "count",
               xlabels = xlabels, ylab = FALSE, 
               allnames = allnames, 
               x_cex = 0.9, 
               pt_cex = 1.7, pt_lwd = 1.3,
               ci_lwd = 1.7)
  
  mtext(spnames[i], 3, line= 1.5, xpd = NA, font = 4)
  mtext(bquote("R"^2~.(r2c[i])*"%"), 3, line=0, xpd = NA, cex = .9)
}

mtext("Coefficient de pente", 1, outer = T, line = 0, at = .55)


dev.off()

