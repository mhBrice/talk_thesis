### PLOT P MATRIX ####
plot_pmat <- function(mod, covariates = 0) {
  
  pmat <- t(pmatrix.msm(mod, t = 10, covariates = covariates, ci = "none"))
  class(pmat) <- "matrix"
  pmat <- round(pmat*100, 1)
  diag(pmat) <- 0
  
  pos.box <- cbind(c(0.5, 0.05, 0.95, 0.5),
                   c(0.95, 0.5, 0.5, 0.05))
  
  par(mar=c(.5,0.5,.5,.5))
  pm <- plotmat(pmat, pos = pos.box, curve = 0.07, name = states,
                lwd = 1.2, relsize = .9,
                box.cex = 1.2, cex.txt = 0, txt.col = "white",
                dtext = .35, txt.font = 2,
                box.lwd = 0.1, box.type = "rect", shadow.size = 0.005,
                box.prop = 0.35, box.size = 0.13, box.col = st_col,
                arr.length = pmat/80, arr.width = pmat/80,  arr.type ="triangle",
                arr.col ="grey40", arr.lcol = "grey40",
                arr.lwd = pmat/3)
  pm$arr$TextX[6] <- pm$arr$TextX[6]-.04
  pm$arr$TextX[7] <- pm$arr$TextX[7]+.03
  pm$arr$TextY[7] <- pm$arr$TextY[7]+.03
  
  text(pm$arr$TextX, pm$arr$TextY, pm$arr$Value, cex = .75)
}


#### function to plot risk ratio ####

plot_risk <- function(mod, 
                      varnames = c("Température", "CMI", 
                                   "Drainage", "pH", 
                                   "Naturelle 1", "Naturelle 2", 
                                   "Coupe 1", "Coupe 2")) {
  
  # Get estimates, CI and p-value
  HR <- hazard.msm(mod)
  
  n_var <- length(varnames)
  
  trans <- rownames(HR[[1]])
  
  trans_list <- list()
  
  for(i in 1:length(trans)) {
    coef <- lapply(HR, function(x) x[i,])
    coef <- do.call(rbind, coef)
    
    signif <- ifelse(coef[,2] <= 1 & coef[,3] <= 1 | coef[,2] >= 1 & coef[,3] >= 1, 1, 0)
    
    
    # Color
    col_pt <- rep(NA, n_var)
    
    col_pt[which(signif==1 & coef[,1]>1)] <- "dodgerblue4"
    col_pt[which(signif==0 & coef[,1]>1)] <- "#C3D3E2"
    col_pt[which(signif==1 & coef[,1]<1)] <- "#B41414"
    col_pt[which(signif==0 & coef[,1]<1)] <- "#ECC4C4"
    
    trans_list[[trans[i]]] <- cbind.data.frame(coef, signif, col_pt=col_pt)
  }
  
  # Layout
  mat <- matrix(c(0,1,2,0,
                  3,0,4,5,
                  6,7,0,8,
                  0,9,10,0),
                4, 4, byrow = T)
  diag(mat) <- 11:14
  mat <- rbind(mat, 15:18)
  
  layout(mat, heights = c(1,1,1,1,.7))  
  
  # Plot
  par(mar = c(.3,2.5,.3,.5), oma = c(0,2.6,1.6,0))
  for(i in trans) {
    
    tmp <- trans_list[[i]]

    labx <- "Hazard ratio"

    ylim <- range(lapply(trans_list, function(x) range(x[,1:3])))
    
    plot(tmp$HR, log = "y", ylim = ylim, col = "transparent", yaxs = "i",
         ann=F, xaxt="n", yaxt="n",  bty = "l", frame.plot = T)
    
    abline(h = 1, col = "grey65")
    
    # bars for confidence interval
    arrows(x0 = 1:n_var, 
           y0 = tmp$L,
           y1 = tmp$U, 
           angle = 90, code = 1, 
           length = 0, col = "grey45", lwd = 1.8, xpd = NA)
    
    # points
    points(tmp$HR, pch = 19, 
           col = as.character(tmp$col_pt), cex = 2)
    
    # axis
    axis(2, at=seq(0,100,1), tcl= -0.2, labels=F, col = "grey35")
    axis(2, at=c(.1,1,10,100), labels = c(.1,1,10,100), las = 1, cex.axis = 1.4)
  }
  
  # Labels
  par(mar=c(0.5,2.5,0.5,0.5))
  for(st in states) {
    plot0(text = st, fill = "grey95", font = 2, cex = 2.5)
  }
  
  
  par(mar = c(.5,2,.1,.5))
  for(i in 1:4) {
    plot0(x=1:n_var, y=rep(0,n_var))
    text(x=1:n_var, y=rep(1,n_var), labels = varnames, font = 2, cex = 1.6,
         srt = 90, xpd = NA, adj = 1) 
  }
  
  mtext(labx, 2,  line = 1, cex = 1.5, font = 2, las=0, outer=T)
  
  mtext("De", 3, adj = -0.01, line = -2, cex = 1.3, font = 2, outer=T)
  
  mtext("À", 3, adj = 0.06, line = 0, cex = 1.3, font = 2, outer=T)

}


#### steady state ####

steady_state <- function(pmat = NULL, qmat = NULL){
  
  if(!is.null(pmat)) {
    eig <- eigen(t(pmat))
    
    pi <- t(eig$vectors[,which(abs(eig$values-1) < 1e-12)])
  } 
  
  if(!is.null(qmat)) {
    eig <- eigen(t(qmat))
    
    pi <- t(eig$vectors[,which(abs(eig$values) < 1e-12)])
  }
  
  steady <- pi/sum(pi)
  colnames(steady) <- colnames(pmat)
  
  return(steady)
}

### FIND INTERSECT BETWEEN 2 CURVES ####
curve_intersect <- function(curve1, curve2) {
  
  colnames(curve1) <- colnames(curve2) <- c("x", "y")
  # Approximate the functional form of both curves
  curve1_f <- approxfun(curve1$x, curve1$y, rule = 2)
  curve2_f <- approxfun(curve2$x, curve2$y, rule = 2)
  
  # Calculate the intersection of curve 1 and curve 2 along the x-axis
  point_x <- uniroot(function(x) curve1_f(x) - curve2_f(x),
                     c(min(curve1$x), max(curve1$x)))$root
  
  # Find where point_x is in curve 2
  point_y <- curve2_f(point_x)
  
  return(list(x = point_x, y = point_y))
}

#### Plot steady state
plot_SS <- function(logging=0, lang = "fr") {
  layout(matrix(1:2, 1), widths = c(1,.4))
  par(mar = c(3,3.5,.7,0))
  
  col_bb <- "#158282"
  col_tt <- "#D43650"
  col_pts <- c("white", "grey", "black")
  
  if(lang == "fr") {
    lgd <- c("Peu ou pas", 
             "Modérée", 
             "Majeure")
    xlab <- "Température de la saison de croissance"
    ylab <- "Proportion d'états à l'équilibre"
    stB <- "Boréal"
    stT <- "Tempéré"
  } else {
    lgd <- c("Minor", 
             "Moderate", 
             "Major")
    xlab <- "Growing season temperature"
    ylab <- "Steady state proportion"
    stB <- "Boreal"
    stT <- "Temperate"
  }
 
  
  # Empty plot
  plot0(ylim = c(0,1), xlim = range(x), xaxs = "i", frame.plot = TRUE, yaxs = "i")
  polygon(x = c(tp_mixed, rev(tp_mixed)), y = c(0,0,1,1),
          col = alpha("grey", .2), border = NA)
  axis(1, cex.axis = .7, tick = F, line = -.3)
  axis(1, labels = F, tcl = -.3)
  axis(2, cex.axis = .7, las = 1, tick = F, line = -.3)
  axis(2, labels = F, tcl = -.3)
  
  mtext(xlab, 1, line = 1.8, cex = .9, font = 2)
  mtext(ylab, 2, line = 2, cex = .9, font = 2)
  
  text(10.7, .94, stB, col = col_bb, cex = 1, font = 2)
  text(13.8, .94, stT, col = col_tt, cex = 1, font = 2)
  
  
  for(i in 1:length(logging)) {
    ll <- which(df[,"logging"] == logging[i])
    lines(bb[ll] ~ x, col = col_bb, lwd = 2, lty = i)
    lines(tt[ll] ~ x, col = col_tt, lwd = 2, lty = i)
    
    # Intersect between boreal and mixed+temperate SS curves
    int <- curve_intersect(curve1 = cbind.data.frame(x, bb[ll]), 
                           curve2 = cbind.data.frame(x, tt[ll]))
    points(int$x, 1, xpd = NA, 
           pch = 21, col = "black", bg = col_pts[i], cex = 1.2, lwd = 2)
    
  }
  
  par(mar = c(3.5,0,.7,0))
  plot0()
  legend("left", legend = lgd[1:length(logging)],
         pch = 21, col = "black", pt.bg = col_pts[1:length(logging)], 
         lty = 1:length(logging), lwd = 1.8, seg.len = 2.5, x.intersp = .5,
         cex = .9, pt.cex = 1.2, pt.lwd = 1.8, xpd = NA, bty = "n")
}


  

### BARPLOT STEADY STATE & TRANSIENT ####


barplot_index <- function(index = NULL, bars = 1:3, ylim = NULL,
                          ylab = NULL, lgd = NULL,
                          colss = c("#f1ba53","#E38451", "#b5305d"),
                          lang = "fr") {
  par(mar = c(2,3.5,.7,.5))

  if(is.null(lgd)) {
    if(lang == "fr") {
      lgd <- c("Peu ou pas de coupe", 
               "Coupe modérée", 
               "Coupe majeure")
    } else {
      lgd <- c("Minor logging", 
               "Moderate logging", 
               "Major logging")
    }
  }

  
  if(is.null(ylim)) ylim <- range(index)
  
  bp <- barplot(as.matrix(index), beside = T, plot = F)
  
  plot0(xlim = range(bp), ylim = ylim, frame.plot = TRUE, yaxs = "i")
  axis(2, las = 1, cex.axis = .9)
  mtext(states, 1, at = bp[1,], line = .8, xpd = NA, adj = 0, cex = 1, font = 2)
  mtext(ylab, 2, line = 2.5, font = 2, cex = 1)
  
  lines(index[bars,] ~ bp[bars,], type = "h", lwd = 20, lend = 2, col = colss[bars])
  
  legend("topleft", legend = lgd[bars], cex = 1,
         pt.cex = 0, text.col = colss[bars], text.font = 2, 
         bty = "n", inset = c(.02,0))
}

barplot_halflife <- function(index = NULL, ylim = NULL, bars = 1:3,
                          colss = c("#f1ba53","#E38451", "#b5305d"),
                          lang = "fr") {
  par(mar = c(2,3.5,.7,.5))
  
  if(lang == "fr") {
    lgd <- c("Peu ou pas de coupe", 
             "Coupe modérée", 
             "Coupe majeure")
    ylab = "Temps de convergence (années)"
  } else {
    lgd <- c("Minor logging", 
             "Moderate logging", 
             "Major logging")
    ylab = "Convergence time (years)"
  }
  
  if(is.null(ylim)) ylim <- range(index)
  
  bp <- barplot(index, plot = FALSE)
  
  plot0(xlim = c(0.3, 3.6), ylim = ylim, frame.plot = TRUE, yaxs = "i")
  
  axis(2, las = 1, cex.axis = .9)

  mtext(ylab, 2, line = 2.5, font = 2, cex = 1)
  
  lines(index[bars] ~ bp[bars], type = "h", lwd = 20, lend = 2, col = colss[bars])

}


### PLOT RANGE SHIFT ####

plot_shift <- function(lat1, lat2 = NULL, xlim, ylim = NULL, line, br, unit = 1, digit = 1,
                       inset = c(0,-.17), alternative = "less",
                       main = NULL) {
  
  d1 <- hist(lat1, breaks = br, plot = FALSE)
  plot(d1,
       main = "", xlab = "", ylab = "", axes = FALSE, 
       xlim = xlim, ylim = ylim,
       col = alpha("grey15", .2), 
       border = "grey15", lwd = 1.6, xpd = NA)
  usr = par("usr")
  
  # Quantile
  q1 <- quantile(lat1, .5)
  
  points(x = q1, y = usr[3], 
         pch = 21, col = "grey15", bg = alpha("grey15", .2), 
         cex = 1.2, lwd = 1.5, xpd = NA)
  
  if(!is.null(lat2)) {
    d2 <- hist(lat2, breaks = br, plot = FALSE)
    plot(d2, col = alpha("red3", .1), 
         border = "red3", lwd = 1.6, add = T, xpd = NA)
    
    # Wilcoxon test
    wt <- wilcox.test(lat1, lat2, alternative = alternative)
    
    q2 <- quantile(lat2, .5)
    points(x = q2, y = usr[3], 
           pch = 21, col = "red3", bg = alpha("red3", .1),
           cex = 1.2, lwd = 1.5, xpd = NA)
    
    delta <- q2 - q1
    

    lgd = as.expression(bquote(Delta==.(myround(delta*unit, digit))~"km"))

    
    legend("top", legend = lgd, cex= 1.2, xjust = 0.5,
           col = "transparent",
           bty = "n", inset = inset, xpd = NA)
    
    mtext(gtools::stars.pval(wt$p.value), 
          3, line = -.8, adj = .5, cex = 1.3, font = 2)
    
    
    res <- cbind(wt$statistic, wt$p.value)
  }
  mtext(main, line = 2, cex = 1.1, font = 4)
  
  abline(v = line, col = alpha("grey75", .5), lwd = .7)
  abline(h = 0, col = "grey65")
  

}


### BARPLOT COEF HURDLE ####

barplot_coef <- function(mod, lab = NULL, 
         ylab = TRUE, xlabels = TRUE,
         type = c("zero", "count"),
         gr = NULL, 
         ylim = NULL, main = NULL, let = NULL,
         allnames, 
         x_cex = 0.8, 
         pt_cex = 1.4, pt_lwd = 1.1,
         ci_lwd = 1.5) {
  
  # Summary

  summ <- getSummary.hurdle(mod)
  
  varm <- c("(Intercept)","Log(theta)") 
  
  
  for(h in type) {
    summ_h <- summ$coef[,,h]
    
    summ_h <- subset(summ_h, !(rownames(summ_h) %in% varm))
    
    sp <- as.character(attr(terms(mod), "variables")[[2]])
    allnames <- gsub("focal", sp, allnames)
    d1  <- as.data.frame(matrix(NA, ncol = ncol(summ_h), 
                                nrow = length(allnames), 
                                dimnames = list(allnames, colnames(summ_h))))
    for(l in 1:length(allnames)) {
      
      tmp <- rownames(summ_h) %in% allnames[l]
      if(any(tmp)) {
        d1[l,]  <- summ_h[tmp,] 
      }
      
    }
    
    
    summ_h <- d1
    
    nvar <- nrow(summ_h)
    if(is.null(lab)) lab <- rownames(summ_h)
    
    # coefficients
    coef_h <- summ_h[, "est"]
    
    # conficence interval
    ci_h <- summ_h[,c("lwr", "upr")]
    ci_h <- ifelse(coef_h < 0, ci_h[,"upr"], ci_h[,"lwr"])
    
    # p-values
    p <- summ_h[,"p"]
    
    # Color
    col_h <- col_point(coef_h)
    bg_h <- col_h
    bg_h[which(p > .05)] <- alpha(bg_h[which(p > .05)],.2)
    
    # Limit
    xlim <- range(coef_h, na.rm = TRUE)
    
    bp <- barplot(coef_h, space = .5, plot = FALSE, horiz = TRUE)
    ylim <- c(max(bp),min(bp))
    bp <- barplot(coef_h, space = .5, plot = FALSE, horiz = TRUE, ylim =  ylim)
    
    # Plot
    plot(coef_h, bp, xlim = xlim, ylim = ylim, 
         col = "transparent", 
         ann=F, xaxt="n", yaxt="n", bty = "l", frame.plot = TRUE)
    usr <- par("usr")
    
    # Groups of variables
    if(!is.null(gr)){
      for(g in 1:length(gr)) {
        grl <- bp[gr[[g]]]
        if((g %% 2) == 0) alp <- .2 else alp <- 0
        polygon(y = c(min(grl)-.8, min(grl)-.8, max(grl)+.8,max(grl)+.8),
                x = c(usr[1:2], usr[2:1]), 
                col = alpha("grey75", alp), border = NA)
      }
    }
    abline(v = 0, col = "grey65")
    
    # Barplot
    barplot(coef_h, col = bg_h, border = col_h, space = .5, 
            add = T, axes = FALSE, horiz = TRUE, ylim = ylim)
    
    # bars for confidence interval
    arrows(y0 = bp, 
           x0 = ci_h,
           x1 = coef_h, 
           angle = 90, code = 1, 
           length = 0, col = "grey65", lwd = ci_lwd, xpd = NA)
    
    
    
    axis(2, at = bp, tcl = -0.2, labels = FALSE, col = "grey35")
    axis(1, tcl = -0.2, labels = FALSE, col = "grey35")
    axis(1, tick = FALSE, cex.axis = .9, las = 1)
    
    if(xlabels) 
      mtext(lab, 2, line = .8, at = bp, adj = 1, las = 2, cex = x_cex)
    
  }
  
  mtext(main, 3,
        adj = 0.95, cex = .85, font = 4, line = 1.3)
  
  
  
  if(ylab) mtext("Coefficient", 2, line = .5, cex = 1, font = 2, outer = T)
}



# function to color point
col_point <- function(coef) {
  col_pt <- coef
  
  col_pt[which(coef >= 0)] <- "#094987"
  col_pt[which(coef < 0)] <- "#b30c0c"
  
  col_pt
}


### Télécharger données climatiques ####

#' Retrieve raster of climate data from ftp://ftp.nrcan.gc.ca/pub/outgoing/NAM_grids
#'
#' @param years vector of selected years (numeric).
#' @param info a character string specifying the kind of data to be retrieved. One among "bio", "cmi", "mint", "maxt", "pcp", "sg". Default is set to "bio".
# # 300 arcsec = 10 squared kilometers. 60 arcsec = 2 squared kilometers
#' @param res resolution of the raster files to be downloaded in arcsec. Either 60 or 300, default is set to 300.
#' @param path folder where files will be stored.
#' @param keep_zip a logical. Should zip files be kept? Default is set to `FALSE`.

retrieveClimateData <- function(years = 1900:2015,
                                info =  c("bio", "cmi", "mint", "maxt", "pcp", "sg"), 
                                res = 300,
                                path = "climateData/", keep_zip = FALSE) {
  
  stopifnot(res %in% c(60, 300))
  dir.create(path, showWarnings = FALSE)
  #
  basurl <- "ftp://ftp.nrcan.gc.ca/pub/outgoing/NAM_grids/zipfiles"
  #
  info <- match.arg(info)
  beg <- paste0(basurl, res, "/")
  end <- paste0("_", res, "arcsec.zip")
  # year available: from 1900 to 2017
  for (year in years) {
    zout <- paste0("path", info, year, ".zip")
    print(paste0(beg, info, year, end))
    download.file(paste0(beg, info, year, end), destfile = zout)
    unzip(zout, exdir = "climateData")
    if (!keep_zip) unlink(zout)
  }
}
