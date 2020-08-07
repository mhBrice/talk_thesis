
#### Perturbations ####

env <- readRDS("../transition/data/env_all.RDS") 


disturb_summ <- env %>% 
  group_by(ID_PE, ecoreg3) %>% 
  summarise("Minor" = all(natural==0 & logging==0),
            "Moderate natural" = any(natural==1),
            "Major natural" = any(natural==2),
            "Moderate logging" = any(logging==1),
            "Major logging" = any(logging==2)) 

disturb_summ <- aggregate(disturb_summ[,-c(1:2)], by=list(disturb_summ$ecoreg3), sum)



# log2 <- table(env$ecoreg3, env$ORIGINE, env$logging)[,,3][,c("CBT", "CPR", "CT")]
# nat2 <- table(env$ecoreg3, env$ORIGINE, env$natural)[,,3][,c("BR", "ES", "CHT","DT")]
# 
# log1 <- table(env$ecoreg3, env$PERTURB, env$logging)[,,2][,c("CAM","CB","CD","CDL","CE","CJ","CP","EPC")]
# nat1 <- table(env$ecoreg3, env$PERTURB, env$natural)[,,2][,c("BRP", "EL", "CHP", "VEP", "DP")]

ori <- table(env$ecoreg3, env$ORIGINE)

pert <- table(env$ecoreg3, env$PERTURB)

ori <- sort_col(ori)
pert <- sort_col(pert)

ori <- ori[,colSums(ori)>0]

ori <- cbind(ori, Autres = rowSums(ori[,6:8]))
ori <- ori[,-c(6:8)]
colnames(ori) <- c("Coupe totale", "Feu majeur", "Épidémie sévère", "Chablis total", "Dépérissement total", "Autres coupes majeures")

pert <- cbind(pert, Autres = rowSums(pert[,c(8:9,11:14)]))
pert <- pert[,-c(8:9,11:14)]
colnames(pert) <- c("Coupe partielle", "Épidémie légère", "Chablis patiel", "Éclaircie commerciale", "Dépérissement partiel", "Feu partiel", "Coupe de jardinage", "Verglas", "Autres")



disturb_types <- list("Peu ou pas" = disturb_summ[,2],
                      "Moderée" = pert,
                      "Majeure" = ori)


cols=c("#4d8585", gpuPalette('insileco', 13)[3:9], "#68566e")
cols=c("#3c784f", "#89a8ad", "#68566e", "#a83f02", "#3c7dba", "#4d8585", gpuPalette('insileco', 13)[4:6])
quartz()
par(mfrow = c(2,3))
for(i in 1:3) {
  my_waffle(x = pert[i,], nrows = 50, ncols = 60, cols = cols, lgd = F)  
  
  my_waffle(x = ori[i,], nrows = 50, ncols = 75, cols = gpuPalette('insileco', 13)[3:9], lgd = F)  
}

col1 <- gpuPalette('cisl', 6)[c(3:5,2,1,6)]
col2 <- rev(gpuPalette(1, 11))[-c(1:2)]

nreg <- c("Zone\nboréale", "Zone\nmixte", "Zone\ntempérée")

png("images/intro_perturbations.png", width = 8.5, height = 5.5, res = 300, units = "in")
#quartz(width = 8, height = 5.5)
par(oma = c(0,4,0,0))
layout(matrix(c(1:3,0,4,5), 2, byrow = T), heights = c(1,.5))
par(mar = c(3.5,1.2,2.7,.5))

barplot(disturb_summ[,2], col = "grey55", ylim = c(0, 3600), 
        names.arg = nreg, cex.axis = 1.1, cex.names = 1.2, las = 1, line=.5)
mtext("Peu ou pas perturbées", font = 2, line = 1.1)
mtext("Fréquence (Nombre d'observations)", 2, line = 3.8, cex = .95, xpd = NA)

barplot(t(pert), col = col2, ylim = c(0, 3600), cex.names = 1.2, 
        names.arg = nreg, axes = F, line=.5)
axis(2, labels = F)
mtext("Perturbations modérées", font = 2, line = 1.1)


barplot(t(ori), col = col1, ylim = c(0, 3600), cex.names = 1.2, 
       names.arg = nreg, axes = F, line=.5)
axis(2, labels = F)
mtext("Perturbations majeures", font = 2, line = 1.1)

# Légende
par(mar = c(0,1,0,1))
plot0()
legend("top", legend = rev(colnames(pert)), pt.bg = rev(col2), cex = 1.4, pt.cex = 3, pch = 22, bty = 'n')
plot0()
legend("top", legend = rev(colnames(ori)), pt.bg = rev(col1), cex = 1.4,  pt.cex = 3, pch = 22,bty = 'n')
dev.off()
