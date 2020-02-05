
# function to plot coefficient ####
plot_coef <- function(mod, lab = NULL, ylab = T) {
  
  # Summary
  r2 <- round(r2(mod)[[2]]*100, 2)
  summ <- getSummary.hurdle(mod)
  
  varm <- c("(Intercept)","Log(theta)") 
  
  par(mfrow = c(1,2), mar = c(2,1.5,3,1), oma = c(2,4,1,0))
  for(h in c("zero", "count")) {
    summ_h <- summ$coef[,,h]
    
    summ_h <- subset(summ_h, !(rownames(summ_h) %in% varm))
    
    nvar <- nrow(summ_h)
    if(is.null(lab)) lab <- rownames(summ_h)
    
    # coefficients
    coef_h <- summ_h[, "est"]
    
    # conficence interval
    ci_h <- summ_h[,c("lwr", "upr")]
    
    # p-values
    p <- summ_h[,"p"]

    # Color
    col_h <- col_point(p, coef_h)
    
    # Plot
    plot(coef_h, nvar:1, xlim = range(ci_h, na.rm = T), col = "transparent", 
         ann=F, xaxt="n", yaxt="n",  bty = "l", frame.plot = T)
    box2(1:2, lwd = 1.5)
    usr <- par("usr")
    abline(v=0, col = "grey65", xpd = F)
    
    # bars for confidence interval
    arrows(y0 = nvar:1, 
           x0 = ci_h[,"lwr"],
           x1 = ci_h[,"upr"], 
           angle = 90, code = 1, 
           length = 0, col = "grey65", lwd = 2, xpd = NA)
    #points
    points(coef_h, nvar:1, pch = 21, bg = col_h, col = col_h, cex = 1.2)
    
    # axis
    if(h == "zero") {
      mtext(lab, 2, line = 1, at = nvar:1, adj = 1, las = 1, cex = 1)
      mtext("Zero hurdle model", 3, line = 1, font = 2, cex = 1.2)
    } else {
      mtext("Count model", 3, line = 1, font = 2, cex = 1.2)
    }
    axis(2, tcl= -0.2, labels=F, col = "grey35")
    axis(1, tcl= -0.2, labels=F, col = "grey35")
    axis(1, tick = FALSE, cex.axis = 1)
  }

  
  mtext(bquote("R"^2~.(r2)*"%"), 
        adj = 0.95, cex = 1.2, font = 3, line = -.5, outer = T)

  
  if(ylab) mtext("Coefficient", 1, line = .3, cex=1.2, font = 2, outer = T)
}

# function to color point
col_point <- function(pval, coef) {
  col_pt <- pval
  
  col_pt[which(pval<.05 & coef>=0)] <- "dodgerblue4"
  col_pt[which(pval>.05 & coef>=0)] <- "#C3D3E2"
  col_pt[which(pval<.05 & coef<0)] <- "#B41414"
  col_pt[which(pval>.05 & coef<0)] <- "#ECC4C4"
  col_pt
}
