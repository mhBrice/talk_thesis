### Figures du chapitre 1 - Thermophilisation #####

library(zoo)
library(RColorBrewer)
library(scales)
library(graphicsutils)
library(gtools)
library(plotrix)
library(effects)
library(FD)

# Fonctions R

source('../thermophilization/functions/plot_beta.R')

source('../thermophilization/functions/varpart_fun.R')

source('../thermophilization/functions/trait_fun.R')

# Données

source("../thermophilization/functions/prep_data.R")

load("../thermophilization/ms/figures/result_reg.rda")

tree_trait <- readRDS("../thermophilization/data/tree_trait_sti.RDS")

sps_code <- read.csv2("data/ref_spCode.csv")


### 1. Stack plot de la diversité beta temporelle ####

# Rollmean

BCD_lat <- cbind.data.frame(xy, BCD,
                            BCD_boreal,
                            BCD_temperate,
                            BCD_pioneer,
                            disturb = BCDdf$disturb,
                            st_coordinates(xy)) %>%
  mutate(gains.rel = gains/tbi, losses.rel = losses/tbi) %>%
  arrange(Y)

k <- 500

BCD_lat1 <- BCD_lat %>% subset(disturb==0) %>% arrange(Y)
BCD_lat2 <- BCD_lat %>% subset(disturb==1) %>% arrange(Y)
BCD_lat3 <- BCD_lat %>% subset(disturb==2) %>% arrange(Y)


col_gr <- c("#08519C", "#C1D4E6", "#D97E21", "#F6DFC8", "#B22306", "#ECC8C1")

to_stack <- c("similarity", "b.gains", "b.losses",
              "p.gains", "p.losses",
              "t.gains",  "t.losses")

latlim = c(46,50)



### Peu ou pas de perturbations ####

png("images/chap1_stack_beta1.png", width = 7.5, height = 4.5, res = 300, units = "in")

layout(mat = matrix(c(1,2), 1), widths = c(1,.56))
par(mar = c(3.5,3.5,2.5,0), xpd = FALSE)
stack_plot(dat = BCD_lat1, stk = to_stack, index = "Y",
           lines = "tbi", col = col_gr, xlim = latlim,
           title = NULL, lgd = FALSE)
mtext(text = "Latitude", side = 1, line = 2.5, cex = 1.1, font = 2)
mtext(text = "Diversité ß temporelle", side = 2, line = 2.5, cex = 1.1, font = 2)
mtext(text = "Peu ou pas de perturbations", line = 1.4, font = 2, cex = 1.2)
mtext(paste(nrow(BCD_lat1), "placettes forestières"), side = 3, line = .4, cex = 1)

par(mar = c(3.5,0,2.5,0))
plot0()

legend("bottomleft",
       legend = c("Gain boréal", "Perte boréal",
                  "Gain pionnier", "Perte pionnier",
                  "Gain tempéré", "Perte tempéré"),
       fill = col_gr,
       border = "transparent", bty = "n", cex = 1)

text(.73, -.64, "Diversité ß",
     col = "grey15",  cex = 1)
text(x = .25, y = -.64, '}',  cex = 7.7, col = "grey15",
     family = 'Helvetica Neue UltraLight')

dev.off()

### Perturbations modérées ####

png("images/chap1_stack_beta2.png", width = 7.5, height = 4.5, res = 300, units = "in")

layout(mat = matrix(c(1,2), 1), widths = c(1,.56))
par(mar = c(3.5,3.5,2.5,0), xpd = FALSE)
stack_plot(dat = BCD_lat2, stk = to_stack, index = "Y",
           lines = "tbi", col = col_gr, xlim = latlim,
           title = NULL, lgd = FALSE)
mtext(text = "Latitude", side = 1, line = 2.5, cex = 1.1, font = 2)
mtext(text = "Diversité ß temporelle", side = 2, line = 2.5, cex = 1.1, font = 2)
mtext(text = "Perturbations modérées", line = 1.4, font = 2, cex = 1.2)
mtext(paste(nrow(BCD_lat2), "placettes forestières"), side = 3, line = .4, cex = 1)

par(mar = c(3.5,0,2.5,0))
plot0()

legend("bottomleft",
       legend = c("Gain boréal", "Perte boréal",
                  "Gain pionnier", "Perte pionnier",
                  "Gain tempéré", "Perte tempéré"),
       fill = col_gr,
       border = "transparent", bty = "n", cex = 1)

text(.73, -.64, "Diversité ß",
     col = "grey15",  cex = 1)
text(x = .26, y = -.64, '}',  cex = 7.7, col = "grey15",
     family = 'Helvetica Neue UltraLight')

dev.off()

### Perturbations majeures ####

png("images/chap1_stack_beta3.png", width = 7.5, height = 4.5, res = 300, units = "in")

layout(mat = matrix(c(1,2), 1), widths = c(1,.56))
par(mar = c(3.5,3.5,2.5,0), xpd = FALSE)
stack_plot(dat = BCD_lat3, stk = to_stack, index = "Y",
           lines = "tbi", col = col_gr, xlim = latlim,
           title = NULL, lgd = FALSE)
mtext(text = "Latitude", side = 1, line = 2.5, cex = 1.1, font = 2)
mtext(text = "Diversité ß temporelle", side = 2, line = 2.5, cex = 1.1, font = 2)
mtext(text = "Perturbations majeures", line = 1.4, font = 2, cex = 1.2)
mtext(paste(nrow(BCD_lat3), "placettes forestières"), side = 3, line = .4, cex = 1)

par(mar = c(3.5,0,2.5,0))
plot0()

legend("bottomleft",
       legend = c("Gain boréal", "Perte boréal",
                  "Gain pionnier", "Perte pionnier",
                  "Gain tempéré", "Perte tempéré"),
       fill = col_gr,
       border = "transparent", bty = "n", cex = 1)

text(.73, -.64, "Diversité ß",
     col = "grey15",  cex = 1)
text(x = .26, y = -.64, '}',  cex = 7.7, col = "grey15",
     family = 'Helvetica Neue UltraLight')

dev.off()


### 2. Partition de la variation ####

col_b <- "#1f678b"
col_c <- "#7d0030"
col_d <- "#ffba00"
col_frac <- c(col_b, col_c, col_d)

quartz(width = 4.6, height = 4)
png("images/chap1_varpart.png", width = 4.6, height = 4, res = 300, units = "in")
par(mar = c(1,1.5,1,1.3), font = 2)
varpart_plot(vp = vp_tbi$varpart, 
             labels = c("Conditions de base", "Changement climatique", "Perturbations"),
             pval = vp_tbi$pval, col_frac = alpha(col_frac, .3), 
             border = col_frac, lwd = 2)

dev.off()


### 3. Régression multiple ####

### labels

labels_sig <- c("Température", "Température^2", 
                "Précipitation", "Précipitation^2", "Delta*Temps",
                "Delta*Température", "Delta*Précipitation", 
                "Température~min", "Température~max","CMI~min",
                "Age",
                "Coupe~récente~modérée","Coupe~récente~majeure",
                "Coupe~historique~modérée", "Coupe~historique~majeure",
                "Naturelle~récente~modérée", "Naturelle~récente~majeure",
                "Naturelle~historique~modérée", "Naturelle~historique~majeure")
.expressions <- labels_sig
labs_expressions <- parse(text = .expressions)


x_lab <- barplot(tbi_reg$est[-20], plot = F)


# 
quartz(width = 5.2, height = 5.4)
png("images/chap1_reg_tbi.png", width = 5.2, height = 5.4, res = 300, units = "in")
layout(matrix(c(1, 2), 1), widths = c(.6, 1))

par(mar = c(2.5, 0, 1.5, 0), yaxs="i", oma = c(1, 1.2, 0, .3))

plot0(x = rep(1, length(x_lab)), y = x_lab,
      xlim = c(0, 1), ylim = c(max(x_lab) + .7, 0))

# Colored rectangles
rect(1.06, x_lab[1,]-.6, 10, x_lab[5,]+.6, 
     col = alpha(col_frac[1],.3), border = NA, xpd = NA)
rect(1.06, x_lab[6,]-.6, 10, x_lab[10,]+.6, 
     col = alpha(col_frac[2],.3), border = NA, xpd = NA)
rect(1.06, x_lab[11,]-.6, 10, x_lab[19,]+.6, 
     col = alpha(col_frac[3],.3), border = NA, xpd = NA)

# Coefs labels
text(1.05, x_lab, labs_expressions, xpd = NA, adj = 1, cex = .96)

par(mar = c(2.5,.8,1.5,.8))

coef_bp(coefs = tbi_reg$est[-20], se = tbi_reg$se[-20], 
        pstar = tbi_reg$pval[-20], axis_y = FALSE,
        at = c(-.5,0,.5,1,1.5),
        xlim = c(-.5,1.5), text_x = "", 
        title = "")

text(1.6, 22.3, labels = "Conditions de base", font = 2, cex = .95, xpd = NA, adj = 1)
text(1.6, 16.3, labels = "Changement climatique", font = 2, cex = .95, xpd = NA, adj = 1)
text(1.6, 10.3, labels = "Perturbations", font = 2, cex = .95, xpd = NA, adj = 1)

mtext("Coefficient de régression", 1, line = 2.2, cex = 1.1)
mtext("Effet sur la diversité ß", 3, line = .3, cex = 1.1)

dev.off()


### 3.1 Régression multiple - pertes et gains ####

### labels

labels_sig <- c("Age",
                "Coupe~récente[1]","Coupe~récente[2]",
                "Coupe~historique[1]", "Coupe~historique[2]",
                "Naturelle~récente[1]", "Naturelle~récente[2]",
                "Naturelle~historique[1]", "Naturelle~historique[2]")
.expressions <- labels_sig
labs_expressions <- parse(text = .expressions)

sel <- 11:19
x_lab <- barplot(gains_reg$est[sel], plot = F)


# 

png("images/chap1_reg_BC.png", width = 7, height = 3, res = 300, units = "in")
layout(matrix(c(1, 2, 3), 1), widths = c(.45, 1, 1))

par(mar = c(2.5, 0, 2, 0), yaxs="i", oma = c(1.1, 1.2, 0, .3))

plot0(x = rep(1, length(x_lab)), y = x_lab,
      xlim = c(0, 1), ylim = c(max(x_lab) + .7, 0))

# Colored rectangles

rect(1.06, x_lab[1,]-.6, 10, x_lab[9,]+.6, 
     col = alpha(col_frac[3],.3), border = NA, xpd = NA)

# Coefs labels
text(1, x_lab, labs_expressions, xpd = NA, adj = 1, cex = 1.3)

par(mar = c(2.5,.8,2,.8))

coef_bp(coefs = gains_reg$est[sel], se = gains_reg$se[sel], 
        pstar = NULL, axis_y = FALSE,
        xlim = c(-1,3.6), text_x = "", 
        title = "")


mtext("Effet sur les gains", 3, line = .3, cex = 1)

coef_bp(coefs = losses_reg$est[sel], se = losses_reg$se[sel], 
        pstar = NULL, axis_y = FALSE,
        xlim = c(-3,1.6), text_x = "", 
        title = "")
mtext("Effet sur les pertes", 3, line = .3, cex = 1)

mtext("Coefficient de régression", 1, line = 0, cex = 1, outer = T, adj=.6)

dev.off()



# coef_bp(coefs = gains_reg$est, se = gains_reg$se, pstar = gains_reg$pval, axis_y=F,
#         xlim = c(-1,3.5), title = "Gains")
# 
# 
# coef_bp(coefs = losses_reg$est, se = losses_reg$se, pstar = losses_reg$pval, axis_y=F,
#         xlim = c(-3,1.5), text_x = "", title = "Losses")


### 4. Thermophilisation ####


# Delta CAI
trait <- c("STI", "STq10", "STq90", "TolS")

CAIdiff <- cbind(plot_id = Comm_trait1$plot_id,
                 (Comm_trait2[,trait]-Comm_trait1[,trait])/BCDdf$time_interv*10,
                 BCD, BCD_boreal, BCD_temperate, BCD_pioneer,
                 disturb = BCDdf$disturb,
                 ecoreg5 = BCDdf$ecoreg5,
                 st_coordinates(xy))


### Anova thermophilisation vs perturbations ####

mod_aov <- aov(STI ~ disturb, data = CAIdiff)

eff_aov <- allEffects(mod = mod_aov)
eff_aov$disturb$lower

quartz(width = 5, height = 4)
png("images/chap1_thermo_aov2.png", width = 5, height = 4, res = 300, units = "in")
par(mar = c(3.5, 3.8, .5, .5))
plot(eff_aov$disturb$fit, type = "b", xlim = c(.7,3.3), ylim = c(0, .055), 
     pch = 19, cex = 1.5, col =  c("grey15", "red3", "grey15"),
     ann = FALSE, axes = FALSE, frame.plot = TRUE)
axis(2, las = 1, cex.axis = .8)
axis(1, at = 1:3, labels = c("Peu ou pas", "Modérée", "Majeure"), cex.axis = 1.3)
arrows(x0 = 1:3,  
       y0 = eff_aov$disturb$lower,
       y1 = eff_aov$disturb$upper,
       code = 0, lwd = 1.5, col = c("grey15", "red3", "grey15"))
mtext("Intensité de perturbations", 1, line = 2.4, font = 2, cex = 1.3)
mtext("Thermophilisation", 2, line = 2.5, font = 2, cex = 1.3)

#text(3, .05, labels = "P-value < 0.001", cex = .9)

dev.off()


### Thermophilisation vs latitude ####


CAIdiff1 <- CAIdiff %>% subset(disturb==0)
CAIdiff2 <- CAIdiff %>% subset(disturb==1)
CAIdiff3 <- CAIdiff %>% subset(disturb==2)


beta_lat1 <- arrange(CAIdiff1,Y) %>% dplyr::select(-ecoreg5, -disturb)
beta_lat2 <- arrange(CAIdiff2,Y) %>% dplyr::select(-ecoreg5, -disturb)
beta_lat3 <- arrange(CAIdiff3,Y) %>% dplyr::select(-ecoreg5, -disturb)

lat_ecotone <- quantile(st_coordinates(xy)[,2][BCDdf$ecoreg3=="Mixed"], c(.15, .85))

ctilim = c(-.1,.1)
latlim = c(46,50)



# quartz(width = 8, height = 3)
png("images/chap1_thermo_lat.png", width = 8, height = 3, res = 300, units = "in")
layout(matrix(1:5, 1), widths = c(0.22, 1, 1, 1, 0.08))

par(mar = c(2, 0, 2.2, 0), oma = c(1.5,0,0,0))
plot0()
mtext("Thermophilisation", side = 2, font = 2,
      line = -2, cex = .84, xpd = NA, col = "red3")
# mtext("Succession", side = 2, font = 2,
#       line = -2.5, cex= .84, col = "grey45")

par(mar = c(2, .7, 2.2, .9), xaxs = "i", yaxs = "i", xpd = FALSE)

### No disturbance
rollmean_plot(beta_lat1, var = c("STI", "TolS"), col = c("red3"),
              ylim = ctilim, xlim = latlim, labx = F, laby = F, means = F)
axis(2, at = c(-.1,0,.1), tick = FALSE, las = 1)
axis(1)
mtext("Peu ou pas de perturbations", 3, line = .5, font = 2, cex = .9)

rect(xleft = lat_ecotone[1], ybottom = -.1, 
     xright = lat_ecotone[2], ytop = .1, 
     border = alpha("grey",.5), col = alpha("grey",.15))

### Moderate disturbances
rollmean_plot(beta_lat2, var = c("STI", "TolS"), col = c("red3"),
              ylim = ctilim, xlim = latlim,
              labx = F, laby = F, means = F)
axis(1)
mtext("Perturbations modérées", 3, line = .5, font = 2, cex = .9)

rect(xleft = lat_ecotone[1], ybottom = -.1, 
     xright = lat_ecotone[2], ytop = .1, 
     border = alpha("grey",.5), col = alpha("grey",.15))

### Major disturbances
rollmean_plot(beta_lat3, var = c("STI", "TolS"), col = c("red3"),
              ylim = ctilim, xlim = latlim,
              labx = F, laby = F, means = F)
axis(1)
mtext("Perturbations majeures", 3, line = .5, font = 2, cex = .9)

rect(xleft = lat_ecotone[1], ybottom = -.1, 
     xright = lat_ecotone[2], ytop = .1, 
     border = alpha("grey",.5), col = alpha("grey",.15))

#
par(mar=c(2,0,2.2,0))
plot0(x = c(-1, 1), y = c(-1, 1))
text(-.2, .52, "+ sp 'chaudes'", srt = 270, cex = 1.2, font = 2, col = "red3", xpd=NA)
arrows(x0 = -1.5, y0 = .02, y1 = .98, angle = 15, length = .1, lwd = 2.1, col = "red3", xpd=NA)

text(-.2, -.5, "+ sp 'froides'", srt = 270, cex = 1.2, font = 2, col = "blue3", xpd=NA)
arrows(x0 = -1.5, y0 = -.02, y1 = -.98, angle = 15, length = .1, lwd = 2.1, col = "blue3", xpd = NA)

mtext("Latitude (°N)", 1, line = .5, outer = TRUE, cex = .9)

dev.off()


png("images/chap1_SM_thermo_lat.png", width = 8, height = 3, res = 300, units = "in")
layout(matrix(1:5, 1), widths = c(0.22, 1, 1, 1, 0.08))

par(mar = c(2, 0, 2.2, 0), oma = c(1.5,0,0,0))
plot0()
mtext("Thermophilisation", side = 2, font = 2,
      line = -1.3, cex = .84, xpd = NA, col = "red3")
mtext("Succession", side = 2, font = 2,
      line = -2.5, cex= .84, col = "grey45")

par(mar = c(2, .7, 2.2, .9), xaxs = "i", yaxs = "i", xpd = FALSE)

### No disturbance
rollmean_plot(beta_lat1, var = c("STI", "TolS"), col = c("red3", "grey45"),
              ylim = ctilim, xlim = latlim, labx = F, laby = F, means = F)
axis(2, at = c(-.1,0,.1), tick = FALSE, las = 1)
axis(1)
mtext("Peu ou pas de perturbations", 3, line = .5, font = 2, cex = .9)

rect(xleft = lat_ecotone[1], ybottom = -.1, 
     xright = lat_ecotone[2], ytop = .1, 
     border = alpha("grey",.5), col = alpha("grey",.15))

### Moderate disturbances
rollmean_plot(beta_lat2, var = c("STI", "TolS"), col = c("red3", "grey45"),
              ylim = ctilim, xlim = latlim,
              labx = F, laby = F, means = F)
axis(1)
mtext("Perturbations modérées", 3, line = .5, font = 2, cex = .9)

rect(xleft = lat_ecotone[1], ybottom = -.1, 
     xright = lat_ecotone[2], ytop = .1, 
     border = alpha("grey",.5), col = alpha("grey",.15))

### Major disturbances
rollmean_plot(beta_lat3, var = c("STI", "TolS"), col = c("red3", "grey45"),
              ylim = ctilim, xlim = latlim,
              labx = F, laby = F, means = F)
axis(1)
mtext("Perturbations majeures", 3, line = .5, font = 2, cex = .9)

rect(xleft = lat_ecotone[1], ybottom = -.1, 
     xright = lat_ecotone[2], ytop = .1, 
     border = alpha("grey",.5), col = alpha("grey",.15))

#
par(mar=c(2,0,2.2,0))
plot0(x = c(-1, 1), y = c(-1, 1))
text(-.2, .52, "+ sp 'chaudes'", srt = 270, cex = 1.2, font = 2, col = "red3", xpd=NA)
arrows(x0 = -1.5, y0 = .02, y1 = .98, angle = 15, length = .1, lwd = 2.1, col = "red3", xpd=NA)

text(-.2, -.5, "+ sp pionnières", srt = 270, cex = 1.2, font = 2, col = "grey45", xpd=NA)
arrows(x0 = -1.5, y0 = -.02, y1 = -.98, angle = 15, length = .1, lwd = 2.1, col = "grey45", xpd = NA)

mtext("Latitude (°N)", 1, line = .5, outer = TRUE, cex = .9)

dev.off()

### Contribution des espèces ####



tree_mat <- as.matrix(tree_trait[,c("STI")])
rownames(tree_mat) <- MySpecies


sp_contrib_cti <- sp_contrib_fun(mat1 = sp_mat1[,MySpecies],
                                 mat2 = sp_mat2[,MySpecies],
                                 trait = tree_mat)



delta_sp <- (sp_mat2[,MySpecies] - sp_mat1[,MySpecies])
mat_contrib <- matrix(nrow=2, ncol=ncol(sp_contrib_cti), dimnames = list(c("+","-"), colnames(sp_contrib_cti)))
for(i in colnames(mat_contrib)) {
  mat_contrib[1,i] <- sum(sp_contrib_cti[delta_sp[,i]>0,i], na.rm=T)/nrow(sp_contrib_cti)
  mat_contrib[2,i] <- sum(sp_contrib_cti[delta_sp[,i]<=0,i], na.rm=T)/nrow(sp_contrib_cti)
  mat_contrib[is.na(mat_contrib)] <- 0
}




# Species labels

sp_sel <- names(which(apply(abs(mat_contrib), 2, function(x) any(x > 0.001))))
spnames <- as.character(sps_code$complete.name[match(sp_sel,sps_code$spCode)])

mat_contrib <- mat_contrib[,sp_sel]
sp_ord <- order(mat_contrib[1,], decreasing = TRUE)
mat_contrib <- mat_contrib[,sp_ord]
spnames <- spnames[sp_ord]

bp <- barplot(mat_contrib[1,], width = .5, space = 1, plot = F)


png("images/chap1_spcontrib.png", width = 6.3, height = 5.5, res = 300, units = "in")
par(mar = c(1.8, 9, 3.3, 5))
plot0(x = rep(0, length(bp)), y = bp, xlim = c(-.1,.12), ylim = c(27.1,.4), yaxs = "i")

axis(1, cex.axis = 0.8, tick = FALSE, line = -.5)
axis(1, labels = F, tcl = -.25)
axis(3, cex.axis = 0.8, tick = FALSE, line = -.5)
axis(3, labels = F, tcl = -.25)
axis(2, at = bp[-c(1,26,27)], labels = spnames[-c(1,26,27)], las = 1, font = 3, cex.axis = 0.85,
     tick = FALSE, line = -.5)
axis(2, at = bp[c(1,26,27)], labels = spnames[c(1,26,27)], las = 1, font = 4, 
     tick = FALSE, line = -.5)
abline(v = 0)

barplot(mat_contrib[1,], col = "#1974b4", border = NA, 
        horiz = TRUE,
        axisnames = F, add = T, axes = F, width = .5, space = 1)
barplot(mat_contrib[2,], col = "#b41943", border = NA, 
        horiz = TRUE,
        axisnames = F, add = T, axes = F, width = .5, space = 1)


abline(v = 0)

mtext("Contribution des espèces à la thermophilisation", 3, line = 1.8, font = 2)

text(.05, 5, "Contribution par des gains", col = "#1974b4", cex = 1, adj = 0, xpd = NA)
text(.05, 7, "Contribution par des pertes", col = "#b41943",  cex = 1, adj = 0, xpd = NA)

dev.off()




