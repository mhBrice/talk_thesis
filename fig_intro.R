
#### Perturbations ####

env <- readRDS("../transition/data/env_all.RDS") 

env_data_oct2020 <- readRDS("~/Documents/GitHub/Quebec_data/data/env_data_oct2020.RDS")

env2 <- env_data_oct2020 %>% filter(ID_PE_MES %in% env$ID_PE_MES)

env$ecoreg3 <- factor(env$ecoreg3, levels = c("Boreal", "Mixed", "Hardwood"))

env2 = env2 %>% left_join(env[,c("ID_PE_MES", "ecoreg3")])
### Niveau de pertubation ####

disturb_summ <- env %>% 
  group_by(ID_PE, ecoreg3) %>% 
  summarise("Minor" = all(natural==0 & logging==0),
            #"Natural" = max(natural),
            #"Logging" = max(logging),
            d = max(natural, logging)) 

minor <- aggregate(disturb_summ$Minor, by=list(disturb_summ$ecoreg3), sum)
d1 <- aggregate(disturb_summ$d, by=list(disturb_summ$ecoreg3), function(x) sum(x==1))
d2 <- aggregate(disturb_summ$d, by=list(disturb_summ$ecoreg3), function(x) sum(x==2))

disturb_summ = as.matrix(cbind(minor[,2], d1[,2],d2[,2]))
rownames(disturb_summ) = c("Boréale", "Mixte", "Tempérée")

# lgd_col <- c("grey45", "#49735E", "#D38926")
lgd_col <- c("#49735E", "grey55", "grey25")
lgd_name <- c("Peu ou pas (<25%)", "Modérée (25-75%)", "Majeure (>75%)")

quartz(width = 4.5, height = 4.5)

png("images/intro_bp_perturb1.png", width = 4.5, height = 4.5, res = 300, units = "in")

par(mar=c(2,4,1,1))
barplot(t(disturb_summ), las = 1, cex.axis = .85, cex.names = 1.1,
        col = lgd_col)
mtext("Fréquence (Nombre de placettes/catégorie)", 2, line = 3, cex = 1)
legend("topright", legend = rev(lgd_name), pt.bg = rev(lgd_col), cex = 1.1, pt.cex = 2, pch = 22, bty = 'n')

dev.off()


##### Type de perturbations ####

ori <- table(env2$ecoreg3, env2$ORIGINE_ori)
log2 <- ori[,c("CT", "CPR", "CBA",  "CBT")]
nat2 <- ori[,c("BR", "ES", "CHT","DT")]

pert <- table(env2$ecoreg3, env2$PERTURB_ori)
log1 <- pert[,c("CP","EPC","CJ","CAM","CB","CD","CDL","CE")]
nat1 <- pert[,c("BRP", "EL", "CHP", "DP", "VEP")]



log2 <- cbind(log2, Autres = rowSums(log2[,-1]))
log2 <- log2[,-c(2:4)]
colnames(log2) <- c("Coupe totale", "Autres coupes")

colnames(nat2) <- c("Feu majeur", "Épidémie sévère", "Chablis total", "Dépérissement total")

log1 <- cbind(log1, Autres = rowSums(log1[,c(4:8)]))
log1 <- log1[,-c(4:8)]
colnames(log1) <- c("Coupe partielle", "Éclaircie commerciale", "Coupe de jardinage", "Autres coupes")

colnames(nat1) <- c("Feu partiel","Épidémie légère", "Chablis patiel", "Dépérissement partiel", "Verglas")


col1 <- gpuPalette('cisl', 6)[c(3:5,2,1,6)]
col2 <- (gpuPalette(1, 11))[-c(1:2)]

reg <- c("Boréale", "Mixte", "Tempérée")

png("images/intro_bp_perturb2.png", width = 8, height = 7.5, res= 300, units = 'in')
#quartz(width = 8, height = 7.5)
layout(matrix(c(1:8),2, byrow = T), widths = c(1,.6,1,.6))

par(mar=c(3,4,3,0), oma = c(0,2,0,0))
barplot(t(log1), col = col1, ylim = c(0,2100), las = 1, 
        main = "Coupe modérée", names.arg = reg, cex.names = 1.3, cex.main = 1.5)
par(mar=c(2,0,0,0))
plot0()
legend("bottomleft", 
       legend = rev(colnames(log1)), 
       pt.bg = rev(col1[1:4]), cex = 1.1, pt.cex = 2, pch = 22, bty = 'n', xpd = NA)

par(mar=c(3,4,3,0))
barplot(t(log2), col = col1, ylim = c(0,2100), las = 1, 
        main = "Coupe majeure", names.arg = reg, cex.names = 1.3, cex.main = 1.5)
par(mar=c(2,0,0,0))
plot0()
legend("bottomleft", 
       legend = rev(colnames(log2)), 
       pt.bg = rev(col1[1:2]), cex = 1.1, pt.cex = 2, pch = 22, bty = 'n', xpd = NA)

par(mar=c(2.5,4,3.5,0))
barplot(t(nat1), col = col2, ylim = c(0,2100), las = 1, xpd = NA, 
        main = "Perturbation naturelle modérée", names.arg = reg, cex.names = 1.3, cex.main = 1.5)
par(mar=c(2,0,0,0))
plot0()
legend("bottomleft", 
       legend = rev(colnames(nat1)), 
       pt.bg = rev(col2[1:5]), cex = 1.1, pt.cex = 2, pch = 22, bty = 'n', xpd = NA)

par(mar=c(2.5,4,3.5,0))
barplot(t(nat2), col = col2, ylim = c(0,2100), las = 1,  xpd = NA, 
        main = "Perturbation naturelle majeure", names.arg = reg, cex.names = 1.3, cex.main = 1.5)
par(mar=c(2,0,0,0))
plot0()
legend("bottomleft", 
       legend = rev(colnames(nat2)), 
       pt.bg = rev(col2[1:4]), cex = 1.1, pt.cex = 2, pch = 22, bty = 'n', xpd = NA)


mtext("Fréquence (Nombre de perturbations observées)", 2, line = 0, cex = 1, outer = T)

dev.off()



##### Niveaux de perturbations ####

ori <- table(env$ecoreg3, env$ORIGINE)
log2 <- ori[,c("CT", "CPR", "CBA",  "CBT")]
nat2 <- ori[,c("BR", "ES", "CHT","DT")]

pert <- table(env$ecoreg3, env$PERTURB)
log1 <- pert[,c("CP","EPC","CJ","CAM","CB","CD","CDL","CE")]
nat1 <- pert[,c("BRP", "EL", "CHP", "DP", "VEP")]



log2 <- rowSums(log2)
nat2 <- rowSums(nat2)

log1 <- rowSums(log1)
nat1 <- rowSums(nat1)

log = rbind(log1, log2)
nat = rbind(nat1, nat2)


col =c("grey55", "grey25")

reg <- c("Boréale", "Mixte", "Tempérée")


png("images/intro_bp_perturb3.png", width = 9, height = 4.5, res= 300, units = 'in')
#quartz(width = 9, height = 4.5)

par(mfrow = c(1,2))

par(mar=c(2,4,1,1))
barplot(log, col = col, ylim = c(0,4500), las = 1, 
        main = "", names.arg = reg, cex.names = 1.3, cex.main = 1.5)
mtext("Coupe", 3, line = -.7, cex = 1.5, font = 2)
mtext("Fréquence (Nombre de perturbations observées)", 2, line = 3, cex = 1)

par(mar=c(2,4,1,1))
barplot(nat, col = col, ylim = c(0,4500), las = 1, 
        main = "", names.arg = reg, cex.names = 1.3, cex.main = 1.5)
mtext("Perturbation naturelle", 3, line = -.7, cex = 1.5, font = 2)
legend("topright", inset = c(0, .2),
       legend = c("Majeure (>75%)", "Modérée (25-75%)"),
       pt.bg = rev(col), cex = 1.1, pt.cex = 2, pch = 22, bty = 'n', xpd = NA)

dev.off()









disturb_types <- list("Peu ou pas" = disturb_summ[,2],
                      "Moderée" = pert,
                      "Majeure" = ori)



col1 <- gpuPalette('cisl', 6)[c(3:5,2,1,6)]
col2 <- rev(gpuPalette(1, 11))[-c(1:2)]

nreg <- c("Zone\nboréale", "Zone\nmixte", "Zone\ntempérée")

png("images/intro_perturbations.png", width = 8.5, height = 5.5, res = 300, units = "in")
#quartz(width = 8, height = 5.5)
par(oma = c(0,4,0,0))
layout(matrix(c(1:3,0,4,5), 2, byrow = T), heights = c(1,.5))
par(mar = c(3.5,1.2,4,.5))

barplot(disturb_summ[,2], col = "grey55", ylim = c(0, 3600), 
        names.arg = nreg, cex.axis = 1.1, cex.names = 1.2, las = 1, line=.5)
mtext("Peu ou pas perturbées", font = 2, line = 2.5)
mtext("0-25% de la placette perturbée", line = 1.1, cex = .9)
mtext("Fréquence (Nombre d'observations)", 2, line = 3.8, cex = .95, xpd = NA)

barplot(t(pert), col = col2, ylim = c(0, 3600), cex.names = 1.2, 
        names.arg = nreg, axes = F, line=.5)
axis(2, labels = F)
mtext("Perturbations modérées", font = 2, line = 2.5)
mtext("25-75% de la placette perturbée", line = 1.1, cex = .9)

barplot(t(ori), col = col1, ylim = c(0, 3600), cex.names = 1.2, 
       names.arg = nreg, axes = F, line=.5)
axis(2, labels = F)
mtext("Perturbations majeures", font = 2, line = 2.5)
mtext("75-100% de la placette perturbée", line = 1.1, cex = .9)
# Légende
par(mar = c(0,1,0,1))
plot0()
legend("top", legend = rev(colnames(pert)), pt.bg = rev(col2), cex = 1.4, pt.cex = 3, pch = 22, bty = 'n')
plot0()
legend("top", legend = rev(colnames(ori)), pt.bg = rev(col1), cex = 1.4,  pt.cex = 3, pch = 22,bty = 'n')
dev.off()




##### Type de perturbations ####

ori <- table(env2$ecoreg3, env2$ORIGINE_ori)
log2 <- ori[,c("CBA",  "CDV", "CPH", "CPR", "CPT", "CRB", "CRS", "CS", "CT", "ETR", "RPS")]
nat2 <- ori[,c("BR", "ES", "CHT")]

pert <- table(env2$ecoreg3, env2$PERTURB_ori)
log1 <- pert[,c("CA", "CAM", "CB","CD","CDL","CE", "CEA", "CIP", "CJ", "CJG", "CJP", "CJT", "CP", 'CPC', "CPF", "CPI", "CPM", "CPS", "CPX", "CTR", "DEG", "DLD",  "EC", "ECE", "EPC", "ESI", "PCP")]
nat1 <- pert[,c("BRP", "EL", "CHP", "DP", "VEP")]



log2 <- cbind("Coupe totale" = log2[,9], 
              "CPRS" = log2[,4], 
              "Autres coupes" = rowSums(log2[,-c(4,9)]))

colnames(nat2) <- c("Feu majeur", "Épidémie sévère", "Chablis total")

log1 <- cbind("Coupe partielle" = log1[,13],
              "Éclaircie commerciale" = rowSums(log1[,23:25]),
              "Coupe de jardinage" = rowSums(log1[,c(9:12)]),
              "Autres coupes" = rowSums(log1[,-c(9:13,23:25)]))

colnames(nat1) <- c("Feu partiel","Épidémie légère", "Chablis patiel", "Dépérissement partiel", "Verglas")


col1 <- gpuPalette('cisl', 6)[c(3:5,2,1,6)]
col2 <- (gpuPalette(1, 11))[-c(1:2)]

reg <- c("Boréale", "Mixte", "Tempérée")

png("images/intro_bp_perturb2.png", width = 8, height = 7.5, res= 300, units = 'in')
#quartz(width = 8, height = 7.5)
layout(matrix(c(1:8),2, byrow = T), widths = c(1,.6,1,.6))

par(mar=c(3,4,3,0), oma = c(0,2,0,0))
barplot(t(log1), col = col1, ylim = c(0,3000), las = 1, 
        main = "Coupe modérée", names.arg = reg, cex.names = 1.3, cex.main = 1.5)
par(mar=c(2,0,0,0))
plot0()
legend("bottomleft", 
       legend = rev(colnames(log1)), 
       pt.bg = rev(col1[1:4]), cex = 1.1, pt.cex = 2, pch = 22, bty = 'n', xpd = NA)

par(mar=c(3,4,3,0))
barplot(t(log2), col = col1, ylim = c(0,3000), las = 1, 
        main = "Coupe majeure", names.arg = reg, cex.names = 1.3, cex.main = 1.5)
par(mar=c(2,0,0,0))
plot0()
legend("bottomleft", 
       legend = rev(colnames(log2)), 
       pt.bg = rev(col1[1:3]), cex = 1.1, pt.cex = 2, pch = 22, bty = 'n', xpd = NA)

par(mar=c(2.5,4,3.5,0))
barplot(t(nat1), col = col2, ylim = c(0,3000), las = 1, xpd = NA, 
        main = "Perturbation naturelle modérée", names.arg = reg, cex.names = 1.3, cex.main = 1.5)
par(mar=c(2,0,0,0))
plot0()
legend("bottomleft", 
       legend = rev(colnames(nat1)), 
       pt.bg = rev(col2[1:5]), cex = 1.1, pt.cex = 2, pch = 22, bty = 'n', xpd = NA)

par(mar=c(2.5,4,3.5,0))
barplot(t(nat2), col = col2, ylim = c(0,3000), las = 1,  xpd = NA, 
        main = "Perturbation naturelle majeure", names.arg = reg, cex.names = 1.3, cex.main = 1.5)
par(mar=c(2,0,0,0))
plot0()
legend("bottomleft", 
       legend = rev(colnames(nat2)), 
       pt.bg = rev(col2[1:3]), cex = 1.1, pt.cex = 2, pch = 22, bty = 'n', xpd = NA)


mtext("Fréquence (Nombre de perturbations observées)", 2, line = 0, cex = 1, outer = T)

dev.off()
