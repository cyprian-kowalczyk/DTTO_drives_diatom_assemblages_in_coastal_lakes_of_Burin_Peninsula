strat.plot2 <- function (d, yvar = NULL, scale.percent = FALSE, graph.widths = 1, 
                         minmax = NULL, scale.minmax = TRUE, xLeft = 0.07, xRight = 1, 
                         yBottom = 0.07, yTop = 0.8, title = "", cex.title = 1.8, 
                         y.axis = TRUE, x.axis = TRUE, min.width = 5, ylim = NULL, 
                         y.rev = FALSE, y.tks = NULL, y.tks.labels = NULL, ylabel = "", 
                         cex.ylabel = 1, cex.yaxis = 0.8, xSpace = 0.01, x.pc.inc = 10, 
                         x.pc.lab = TRUE, x.pc.omit0 = TRUE, wa.order = "none", 
                         plot.line = TRUE, col.line = "black", lwd.line = 1, 
                         col.symb = "black", plot.bar = TRUE, lwd.bar = 1, col.bar = "grey", 
                         sep.bar = FALSE, bar.back = FALSE, plot.poly = FALSE, col.poly = "grey", 
                         col.poly.line = NA, lwd.poly = 1, plot.symb = FALSE, symb.pch = 19, 
                         symb.cex = 1, x.names = NULL, cex.xlabel = 1.1, srt.xlabel = 90, 
                         mgp = NULL, ylabPos = 2, cex.axis = 0.8, clust = NULL, clust.width = 0.1, 
                         orig.fig = NULL, exag = FALSE, exag.mult = 5, col.exag = "grey90", 
                         exag.alpha = 0.2, col.bg = NULL, fun1 = NULL, fun2 = NULL, 
                         add = FALSE, omitMissing = TRUE, ...) 
{
  errorMsg <- function(msg) {
    if (shiny_running()) {
      stop(msg)
    }
    else {
      stop(msg)
    }
  }
  d <- as.data.frame(d)
  fcall <- match.call(expand.dots = TRUE)
  if (!is.null(clust)) {
    if (class(clust)[1] != "chclust") 
      errorMsg("clust must be a chclust object")
  }
  if (!is.null(clust)) 
    xRight = xRight - clust.width
  if (is.null(yvar)) {
    yvar <- 1:nrow(d)
    if (is.null(ylim)) {
      ylim = c(0.5, nrow(d) + 0.5)
    }
  }
  else {
    if (!is.null(dim(yvar))) 
      yvar <- yvar[, 1, drop = TRUE]
  }
  if (is.null(x.names)) 
    x.names = colnames(d)
  if (is.null(ylim)) {
    ylim = range(yvar, na.rm = TRUE)
  }
  else {
    if (is.na(yvar[1])) 
      ylim[1] <- min(yvar, na.rm = TRUE)
    if (is.na(ylim[2])) 
      ylim[2] <- max(yvar, na.rm = TRUE)
  }
  oldfig = par("fig")
  oldmai <- par("mai")
  if (is.null(orig.fig)) {
    orig.fig = par("fig")
  }
  if (exag.mult < 1) 
    exag <- FALSE
  nsp <- ncol(d)
  nsam <- nrow(d)
  if (scale.percent == TRUE & length(x.pc.inc) > 1) {
    if (length(x.pc.inc) != nsp) 
      errorMsg("length of x.pc.inc should equal number of curves")
  }
  else {
    x.pc.inc <- rep(x.pc.inc[1], nsp)
  }
  if (!is.null(minmax)) {
    if (ncol(minmax) != 2) 
      errorMsg("minmax should have 2 columns")
    if (nrow(minmax) != nsp) 
      errorMsg("number of rows of minmax should equal number of curves")
  }
  par(mai = c(0, 0, 0, 0))
  if (length(graph.widths) == 1) 
    graph.widths <- rep(1, nsp)
  if (length(graph.widths) != nsp) 
    errorMsg("Length of graph.widths should equal number of curves")
  if (length(exag) == 1) 
    exag <- rep(exag[1], nsp)
  if (length(exag) != nsp) 
    errorMsg("Length of exag should equal number of curves")
  if (length(exag.mult) == 1) 
    exag.mult <- rep(exag.mult[1], nsp)
  if (length(exag.mult) != nsp) 
    errorMsg("Length of exag.mult should equal number of curves")
  if (length(col.exag) == 1) 
    col.exag <- rep(col.exag[1], nsp)
  if (length(col.exag) != nsp) 
    errorMsg("Length of col.exag should equal number of curves")
  if (!is.null(fun1)) {
    if (length(fun1) == 1) 
      fun1 <- lapply(1:nsp, function(x) fun1)
    if (length(fun1) != nsp) 
      errorMsg("Length of fun1 should equal number of curves")
  }
  if (!is.null(fun2)) {
    if (length(fun2) == 1) 
      fun2 <- lapply(1:nsp, function(x) fun2)
    if (length(fun2) != nsp) 
      errorMsg("Length of fun2 should equal number of curves")
  }
  if (length(x.axis) == 1) 
    x.axis <- rep(x.axis[1], nsp)
  if (length(x.axis) != nsp) 
    errorMsg("Length of x.axis should equal number of curves")
  cc.line <- rep(col.line, length.out = nsp)
  if (sep.bar) 
    cc.bar <- rep(col.bar, length.out = nsam)
  else cc.bar <- rep(col.bar, length.out = nsp)
  cc.poly <- rep(col.poly, length.out = nsp)
  cc.poly.line <- rep(col.poly.line, length.out = nsp)
  make.col <- function(x, alpha) {
    apply(col2rgb(x)/255, 2, function(x) rgb(x[1], x[2], 
                                             x[3], alpha))
  }
  if (col.exag[1] == "auto") 
    col.exag <- make.col(cc.poly, exag.alpha)
  inc <- 0.002
  if (wa.order == "topleft" || wa.order == "bottomleft") {
    colsum <- colSums(d, na.rm = TRUE)
    opt <- (t(d) %*% yvar)/colsum
    if ((wa.order == "topleft" & !y.rev) | (wa.order == 
                                            "bottomleft" & y.rev)) 
      opt.order <- rev(order(opt))
    else opt.order <- order(opt)
    d <- d[, opt.order]
    if (!is.null(minmax)) 
      minmax <- minmax[opt.order, ]
    if (!is.null(x.names)) 
      x.names <- x.names[opt.order]
    graph.widths <- graph.widths[opt.order]
    exag <- exag[opt.order]
    exag.mult <- exag.mult[opt.order]
    if (!is.null(fun1)) 
      fun1 <- fun1[opt.order]
    if (!is.null(fun2)) 
      fun2 <- fun2[opt.order]
    x.axis <- x.axis[opt.order]
    cc.poly <- cc.poly[opt.order]
    cc.poly.line <- cc.poly.line[opt.order]
    cc.line <- cc.line[opt.order]
    if (!sep.bar) 
      cc.bar <- cc.bar[opt.order]
  }
  if (scale.percent) {
    colM <- apply(d, 2, max, na.rm = TRUE)
    colM <- floor((colM + 5)/5) * 5
    colM[colM < min.width] <- min.width
    colM.sum <- sum(colM, na.rm = TRUE)
  }
  else {
    colM.sum <- sum(graph.widths, na.rm = TRUE)
    colM <- graph.widths
  }
  xLen <- xRight - xLeft
  xInc <- xLen - ((nsp + 1) * xSpace)
  inc <- xInc/colM.sum
  if (inc < 0) 
    errorMsg("Too many variables, curves will be too small.")
  x1 <- xLeft
  par(fig = figCnvt(orig.fig, c(x1, min(x1 + 0.4, 0.9), yBottom, 
                                yTop)), new = add)
  if (y.rev) {
    tmp <- ylim[1]
    ylim[1] <- ylim[2]
    ylim[2] <- tmp
  }
  plot(0, 0, cex = 0.5, xlim = c(0, 1), axes = FALSE, type = "n", 
       yaxs = "i", ylim = ylim, ...)
  usr1 <- par("usr")
  if (y.axis) {
    if (is.null(y.tks)) 
      y.tks <- axTicks(2)
    if (is.null(y.tks.labels)) 
      y.tks.labels <- y.override
    ax <- axis(side = 2, las = 1, at = y.tks, labels = as.character(y.tks.labels), 
               cex.axis = cex.yaxis, xpd = NA)
    x1 <- x1 + xSpace
    mtext(title, adj = 0, line = 5, cex = cex.title)
    mtext(ylabel, side = 2, line = ylabPos, cex = cex.ylabel)
  }
  ty <- ifelse(plot.line, "l", "n")
  tcll <- -0.3
  if ("tcl" %in% names(fcall)) 
    tcll <- eval(fcall$tcl)
  spc <- 0
  if ("las" %in% names(fcall)) {
    if ((eval(fcall$las)) == 2) 
      spc = 0.3
  }
  figs <- vector("list", length = nsp)
  usrs <- vector("list", length = nsp)
  for (i in 1:nsp) {
    y_var <- yvar
    x_var <- d[, i, drop = TRUE]
    nsam2 <- nsam
    if (omitMissing) {
      miss <- is.na(y_var) | is.na(x_var)
      nsam2 <- sum(!miss)
      if (nsam2 < nsam) {
        y_var <- y_var[!miss]
        x_var <- x_var[!miss]
        if (sep.bar) {
          cc.bar <- cc.bar[!miss]
        }
      }
    }
    par(new = TRUE)
    par(lend = "butt")
    if (scale.percent) {
      inc2 <- inc * colM[i]
      par(fig = figCnvt(orig.fig, c(x1, x1 + inc2, yBottom, 
                                    yTop)))
      plot(0, 0, cex = 0.5, xlim = c(0, colM[i]), axes = FALSE, 
           xaxs = "i", type = "n", yaxs = "i", 
           ylim = ylim, xlab = "", ylab = "", 
           ...)
      if (!is.null(col.bg)) 
        rect(par("usr")[1], ylim[1], par("usr")[2], 
             ylim[2], col = col.bg, border = NA)
      if (!is.null(fun1[i])) {
        fun1[[i]](x = x_var, y = y_var, i = i, nm = x.names[i])
      }
      if (plot.poly & exag[i]) {
        y <- c(y_var[1], y_var, y_var[nsam2])
        x2 <- c(0, x_var * exag.mult[i], 0)
        polygon(x2, y, col = col.exag[i], border = NA, 
                xpd = FALSE)
      }
      if (bar.back) {
        if (is.logical(plot.bar)) {
          if (plot.bar) {
            if (sep.bar) {
              segments(rep(0, nsam2), y_var, x_var, y_var, 
                       lwd = lwd.bar, col = cc.bar)
            }
            else {
              segments(rep(0, nsam2), y_var, x_var, y_var, 
                       lwd = lwd.bar, col = cc.bar[i])
            }
          }
        }
        else {
          if (plot.bar == "Full") {
            abline(h = y_var, col = cc.bar, lwd = lwd.bar)
          }
        }
      }
      if (plot.poly) {
        y <- c(y_var[1], y_var, y_var[nsam2])
        x <- c(0, x_var, 0)
        polygon(x, y, col = cc.poly[i], border = cc.poly.line[i], 
                lwd = lwd.poly)
      }
      if (!bar.back) {
        if (is.logical(plot.bar)) {
          if (plot.bar) {
            if (sep.bar) {
              segments(rep(0, nsam2), y_var, x_var, y_var, 
                       lwd = lwd.bar, col = cc.bar)
            }
            else {
              segments(rep(0, nsam2), y_var, x_var, y_var, 
                       lwd = lwd.bar, col = cc.bar[i])
            }
          }
        }
        else {
          if (plot.bar == "Full") {
            abline(h = y_var, col = cc.bar, lwd = lwd.bar)
          }
        }
      }
      lines(c(0, 0), c(min(y_var, na.rm = TRUE), max(y_var, 
                                                     na.rm = TRUE)), ...)
      if (ty == "l") 
        lines(x_var, y_var, col = cc.line[i], lwd = lwd.line)
      if (plot.symb) {
        points(x_var, y_var, pch = symb.pch, cex = symb.cex, 
               col = col.symb, xpd = FALSE)
      }
      if (!is.null(fun2[i])) {
        fun2[[i]](x = x_var, y = y_var, i = i, nm = x.names[i])
      }
      xlabb <- seq(0, colM[i], by = x.pc.inc[i])
      if (x.axis[i]) {
        if (x.pc.lab) {
          xlabbt <- as.character(xlabb)
          if (x.pc.omit0) 
            xlabbt[1] <- ""
          mgpX <- if (is.null(mgp)) {
            c(3, max(0, spc - tcll), 0.3)
          }
          else {
            mgp
          }
          axis(side = 1, at = xlabb, labels = xlabbt, 
               mgp = mgpX, cex.axis = cex.axis, ...)
        }
        else axis(side = 1, at = xlabb, labels = FALSE, 
                  mgp = mgpX, ...)
      }
      x1 <- x1 + inc2 + xSpace
    }
    else {
      inc2 <- inc * colM[i]
      par(fig = figCnvt(orig.fig, c(x1, min(1, x1 + inc2, 
                                            na.rm = TRUE), yBottom, yTop)))
      if (!is.null(minmax)) {
        plot(x_var, y_var, cex = 0.5, axes = FALSE, xaxs = "i", 
             type = "n", yaxs = "i", ylim = ylim, 
             xlim = c(minmax[i, 1], minmax[i, 2]), ...)
      }
      else {
        plot(x_var, y_var, cex = 0.5, axes = FALSE, xaxs = "i", 
             type = "n", yaxs = "i", ylim = ylim, 
             ...)
      }
      if (!is.null(col.bg)) 
        rect(par("usr")[1], par("usr")[3], 
             par("usr")[2], par("usr")[4], col = col.bg)
      tks <- axTicks(1)
      us <- par("usr")
      if (!is.null(fun1[i])) {
        fun1[[i]](x = x_var, y = y_var, i = i, nm = x.names[i])
      }
      if (plot.poly & exag[i]) {
        y <- c(y_var[1], y_var, y_var[nsam2])
        x2 <- c(us[1], x_var * exag.mult[i], us[1])
        polygon(x2, y, col = col.exag[i], border = NA)
      }
      if (bar.back) {
        if (is.logical(plot.bar)) {
          if (plot.bar) {
            if (sep.bar) {
              segments(rep(us[1], nsam2), y_var, x_var, 
                       y_var, lwd = lwd.bar, col = cc.bar)
            }
            else {
              segments(rep(us[1], nsam2), y_var, x_var, 
                       y_var, lwd = lwd.bar, col = cc.bar[i])
            }
          }
        }
        else {
          if (plot.bar == "Full") {
            abline(h = y_var, col = cc.bar, lwd = lwd.bar)
          }
        }
      }
      if (plot.poly) {
        y <- c(y_var[1], y_var, y_var[nsam2])
        x <- c(us[1], x_var, us[1])
        if (exag[i]) {
          x2 <- c(us[1], x_var * exag.mult[i], us[1])
          polygon(x2, y, col = col.exag[i], border = NA)
        }
        polygon(x, y, col = cc.poly[i], border = cc.poly.line[i], 
                lwd = lwd.poly)
      }
      if (!bar.back) {
        if (is.logical(plot.bar)) {
          if (plot.bar) {
            if (sep.bar) {
              segments(rep(us[1], nsam2), y_var, x_var, 
                       y_var, lwd = lwd.bar, col = cc.bar)
            }
            else {
              segments(rep(us[1], nsam2), y_var, x_var, 
                       y_var, lwd = lwd.bar, col = cc.bar[i])
            }
          }
        }
        else {
          if (plot.bar == "Full") {
            abline(h = y_var, col = cc.bar, lwd = lwd.bar)
          }
        }
      }
      lines(c(us[1], us[1]), c(min(y_var, na.rm = TRUE), 
                               max(y_var, na.rm = TRUE)), ...)
      if (ty == "l") 
        lines(x_var, y_var, col = cc.line[i], lwd = lwd.line)
      if (plot.symb) {
        points(x_var, y_var, pch = symb.pch, cex = symb.cex, 
               col = col.symb, xpd = FALSE)
      }
      if (!is.null(fun2[i])) {
        fun2[[i]](x = x_var, y = y_var, i = i, nm = x.names[i])
      }
      mgpX <- if (is.null(mgp)) {
        c(3, max(0, spc - tcll), 0.3)
      }
      else {
        mgp
      }
      if (x.axis[i]) {
        if (scale.minmax) {
          nn <- length(axTicks(1))
          tk <- c(axTicks(1)[1], axTicks(1)[nn])
          axis(side = 1, at = tk, labels = as.character(tk), 
               cex.axis = cex.axis, mgp = mgpX, ...)
        }
        else {
          axis(side = 1, cex.axis = cex.axis, mgp = mgpX, 
               ...)
        }
      }
      x1 <- x1 + inc2 + xSpace
    }
    usr2 <- par("usr")
    tks1 <- usr2[1]
    r <- abs((usr1[4] - usr1[3])) * 0.015
    pos <- usr1[4] + r
    if (y.rev) 
      pos <- usr1[4] - r
    if (!is.null(srt.xlabel)) {
      if (srt.xlabel < 90) 
        text(tks1[1], pos, labels = x.names[i], adj = c(0, 
                                                        0), srt = srt.xlabel, cex = cex.xlabel, xpd = NA)
      else text(tks1[1], pos, labels = x.names[i], adj = c(0, 
                                                           1), srt = srt.xlabel, cex = cex.xlabel, xpd = NA)
    }
    usrs[[i]] <- usr2
    figs[[i]] <- par("fig")
  }
  if (!is.null(clust)) {
    par(fig = figCnvt(orig.fig, c(x1, xRight + clust.width, 
                                  yBottom, yTop)))
    par(mar = c(0, 0, 0, 0))
    par(new = TRUE)
    if (y.rev) 
      xl <- rev(ylim)
    else xl <- ylim
    plot(clust, xvar = yvar, horiz = TRUE, x.rev = y.rev, 
         labels = rep("", length(yvar)), hang = -1, 
         mgp = mgpX, cex.axis = cex.axis, xlim = xl, yaxs = "i", 
         xpd = FALSE, ...)
  }
  par(mai = oldmai)
  oldfig[oldfig < 0] <- 0
  par(fig = oldfig)
  ll <- list(call = fcall, box = c(xLeft = xLeft, xRight = xRight, 
                                   yBottom = yBottom, yTop = yTop), usr = usr1, yvar = yvar, 
             ylim = ylim, y.rev = y.rev, figs = figs, usrs = usrs)
  invisible(ll)
}