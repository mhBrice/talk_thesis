### Figures du chapitre 2 - Transitions ####


library(graphicsutils)
library(diagram)
library(dplyr)
library(msm)


# library(sf)
source("funs.R")

# Données

load("../transition/res/msm_all75.rda")
msm_glb <- msm_all75[["msm_glb"]]

states_ba <- readRDS("../transition/data/states_envba.RDS") 


env_sc <- scale(states_ba[,c("sTP", "sCMI", "DRAIN", "PH_HUMUS")])

states <- c("Boréal", "Mixte", "Pionnier", "Tempéré")

st_col <- c("#158282", "#A1BD93", "#FEAC19", "#D43650")

ll_mixed <- which(states_ba$ecoreg3=="Mixed")
mixed_mean <- as.list(apply(env_sc[ll_mixed,], 2, mean))
covar_log <- list(mixed_mean,
                  c(logging1 = 1, mixed_mean),
                  c(logging2 = 1, mixed_mean))




### 1. Diagramme de transition ####

trans_nb <- statetable.msm(states_num, plot_id, data = states_ba)
rownames(trans_nb) <- colnames(trans_nb) <- states
trans_perc <- t(round(trans_nb/rowSums(trans_nb)*100, 1))

arr_l <- trans_perc/40
diag(arr_l) <- 0
col_txt <- as.vector(ifelse(arr_l==0, "white", "black"))

pos.box <- cbind (c(0.5, 0.2, 0.8, 0.5),
                  c(0.8, 0.5, 0.5, 0.2))

quartz(width=5.5, height=3.8)
png("images/chap2_tr_obs.png", width=5.5, height=3.8, res = 300, units = "in")
par(mar=c(0,0,0,0))
pm <- plotmat(trans_perc, pos = pos.box, curve = 0.07, name = states, 
              lwd = 1.2, relsize = .9,
              box.cex = 1.2, cex.txt = 0, txt.col = "white",
              dtext = .35, txt.font = 2,
              box.lwd = 0.1, box.type = "rect", shadow.size = 0.005,
              box.prop = 0.35, box.size = 0.13, box.col = st_col,
              arr.length = arr_l, arr.width = arr_l,  arr.type ="triangle",
              arr.col = "grey40", arr.lcol = "grey40",
              arr.lwd = trans_perc/3, self.cex = .45, self.lwd = diag(trans_perc)/3,
              self.shifty = c(.07,0,0,-.07), self.shiftx = c(0,-.14,.14,0))
# self
pm$arr$TextY[1] <- pm$arr$TextY[1]+.058
pm$arr$TextX[6] <- pm$arr$TextX[6]-.058
pm$arr$TextX[11] <- pm$arr$TextX[11]+.058
pm$arr$TextY[16] <- pm$arr$TextY[16]-.058

pm$arr$TextX[8] <- pm$arr$TextX[8]-.03
pm$arr$TextY[8] <- pm$arr$TextY[8]-.01
pm$arr$TextX[9] <- pm$arr$TextX[9]+.03
pm$arr$TextY[9] <- pm$arr$TextY[9]+.01
pm$arr$TextY[3] <- pm$arr$TextY[3]-.005

text(pm$arr$TextX, pm$arr$TextY, pm$arr$Value, cex = .75, col = col_txt)
dev.off()


### 2. Probabilité de transition vs coupe ####

png("images/chap2_pmat_coupe0.png", width=5, height=3.5, res = 300, units = "in", bg = "transparent")
plot_pmat(mod = msm_glb, covariates = covar_log[[1]])
dev.off()

png("images/chap2_pmat_coupe1.png", width=5, height=3.5, res = 300, units = "in", bg = "transparent")
plot_pmat(mod = msm_glb, covariates = covar_log[[2]])
dev.off()

png("images/chap2_pmat_coupe2.png", width=5, height=3.5, res = 300, units = "in", bg = "transparent")
plot_pmat(mod = msm_glb, covariates = covar_log[[3]])
dev.off()

### 3. Équilibre ####

tp_grad <- seq(-1.9, 1.6, len = 50)

df <- expand.grid(sTP = tp_grad, logging = c(0,1, 2), 
                  sCMI = mixed_mean[[2]],
                  DRAIN = mixed_mean[[3]],
                  PH_HUMUS = mixed_mean[[4]])


sc_sTP <- c(attr(scale(states_ba$sTP), "scaled:center"),
            attr(scale(states_ba$sTP), "scaled:scale"))
x <- tp_grad * sc_sTP[2] + sc_sTP[1]

tp_mixed <- quantile(states_ba$sTP[states_ba$ecoreg3=="Mixed"], c(.2,.8))


qmats <- apply(df, 1, function(x)
  qmatrix.msm(msm_glb, covariates = as.list(x), ci = "none"))

SS <- apply(qmats, 2, function(x) steady_state(qmat = matrix(x, 4, 4)))

bb <- SS[1,]
tt <- SS[4,]


png("images/chap2_SS_coupe0.png", width=5.2, height=3.2, res = 300, units = "in")
par(oma = c(1,0,0,0))
plot_SS(logging = 0)
mtext("Nord", 1, outer = TRUE, adj = .1, cex = .9, font = 4, col = "grey35")
mtext("Sud", 1, outer = TRUE, adj = .73, cex = .9, font = 4, col = "grey35")
dev.off()

png("images/chap2_SS_coupe1.png", width=5.2, height=3.2, res = 300, units = "in")
par(oma = c(1,0,0,0))
plot_SS(logging = 0:1)
mtext("Nord", 1, outer = TRUE, adj = .1, cex = .9, font = 4, col = "grey35")
mtext("Sud", 1, outer = TRUE, adj = .73, cex = .9, font = 4, col = "grey35")
dev.off()

png("images/chap2_SS_coupe2.png", width=5.2, height=3.2, res = 300, units = "in")
par(oma = c(1,0,0,0))
plot_SS(logging = 0:2)
mtext("Nord", 1, outer = TRUE, adj = .1, cex = .9, font = 4, col = "grey35")
mtext("Sud", 1, outer = TRUE, adj = .73, cex = .9, font = 4, col = "grey35")
dev.off()


### 4. Résultats MSM ####

png("images/chap2_msm.png", width=8, height=7, res = 300, units = "in")
plot_risk(msm_glb)
dev.off()

### 5. Dynamique transitoire ####


init <- states_ba %>% filter(ecoreg3 == "Mixed") %>%
  group_by(ID_PE) %>% 
  arrange(year_measured) %>% 
  slice(1) 
init <- table(init$states_ba)
init <- init/sum(init)

dl <- list(list(mixed_mean, logging1 = 0), 
           list(mixed_mean, logging1 = 1), 
           list(mixed_mean, logging2 = 1))
qmats <- lapply(dl, function(x)
  qmatrix.msm(msm_glb, covariates = x, ci = "none"))

SS <- lapply(qmats, function(x) steady_state(qmat = x))
SS <- rbind(init, do.call(rbind, SS))


colss <- c("#2c7b8f", "#f1ba53","#E38451", "#b5305d")
lgd <- c("Proportion initiale réelle", 
         "Équilibre - Peu ou pas de coupe", 
         "Équilibre - Coupe modérée", 
         "Équilibre - Coupe majeure")


png("images/chap2_SS_bp.png", width = 5, height = 4, res = 300, units = "in")
barplot_index(index = SS, bars = 1:4, lgd = lgd, colss = colss, ylim = c(0,1),
              ylab = "Proportion des états")
dev.off()


## Dynamique transitoire
  

qmats <- lapply(covar_log, FUN = function(x) qmatrix.msm(msm_glb, covariates = x, ci = "none"))

### Steady state from qmat
eig <- lapply(qmats, function(x) eigen(t(x)))
pi <- lapply(eig, function(x) t(x$vectors[,which(abs(x$values) < 1e-12)]))
steady <- lapply(pi, function(x) x/sum(x))
steady <- lapply(steady, "colnames<-", states)

### Convergence to steady state - Damping ratio
lambda <- lapply(eig, function(x) sort(x$values, decreasing = TRUE)[2])

damping <- lapply(lambda, function(x) exp(abs(x)))

### Half life to steady state
halflife <- lapply(damping, function(x) log(90)/log(x)) 

### Sojourn time = turnover time from Hill

sojs <- lapply(covar_log, function(x)
  sojourn.msm(msm_glb, covariates = x, ci = "none")[[1]])
#sojs_contrib <- mapply("*", sojs, steady, SIMPLIFY = FALSE)

halflife <- unlist(halflife)
sojs <- do.call(rbind, sojs)

png("images/chap2_transient0.png", width = 7, height = 4, res = 300, units = "in")
layout(matrix(c(1,2), 1), widths = c(1,.4))
barplot_index(index = as.matrix(sojs), bars = 1, 
              ylab = "Temps de séjour (années)", ylim = c(0,200))
barplot_halflife(index = halflife, bars = 1, ylim = c(0, 850))
dev.off()


png("images/chap2_transient1.png", width = 7, height = 4, res = 300, units = "in")
layout(matrix(c(1,2), 1), widths = c(1,.4))
barplot_index(index = as.matrix(sojs), bars = 1:2, 
              ylab = "Temps de séjour (années)", ylim = c(0,200))
barplot_halflife(index = halflife, bars = 1:2, ylim = c(0, 850))
dev.off()


png("images/chap2_transient2.png", width = 7, height = 4, res = 300, units = "in")
layout(matrix(c(1,2), 1), widths = c(1,.4))
barplot_index(index = as.matrix(sojs), bars = 1:3, 
              ylab = "Temps de séjour (années)", ylim = c(0,200))
barplot_halflife(index = halflife, bars = 1:3, ylim = c(0, 850))
dev.off()


### Schéma

arr_l = matrix(rep(.3, 16),4)
diag(arr_l) <- 0

arr_l[1,4] = 0
arr_l[4,1] = 0

arr.lcol = matrix(rep("grey40", 16),4)
arr.lcol[1,4] = "transparent"
arr.lcol[4,1] = "transparent"

png("images/chap2_schema_tr.png", width=5, height=3.8, res = 300, units = "in", bg = "transparent")
par(mar=c(0,0,0,0))
plotmat(trans_perc, pos = pos.box, curve = 0.07, name = states, 
        lwd = 1.2, relsize = .9,
        box.cex = 1.3, cex.txt = 0, txt.col = "white",
        dtext = .35, txt.font = 2,
        box.lwd = 0.1, box.type = "rect", shadow.size = 0.005,
        box.prop = 0.35, box.size = 0.13, box.col = st_col,
        arr.length = arr_l, arr.width = arr_l,  arr.type ="triangle",
        arr.col = "grey40", arr.lcol = arr.lcol,
        arr.lwd = 3, self.cex = .45, self.lwd = 3,
        self.shifty = c(.07,0,0,-.07), self.shiftx = c(0,-.14,.14,0))
dev.off()
