.m.interaction.plot = function(x.factor, trace.factor, response, fun = mean, type = c("l", "p", "b"), legend = TRUE, trace.label = deparse(substitute(trace.factor)), 
    fixed = FALSE, xlab = deparse(substitute(x.factor)), ylab = ylabel, ylim = range(cells, na.rm = TRUE), lty = nc:1, col = 1, pch = c(1L:9, 0, letters), xpd = NULL, 
    leg.bg = par("bg"), leg.bty = "n", xtick = FALSE, xaxt = par("xaxt"), axes = TRUE, ...) {
    ylabel <- paste(deparse(substitute(fun)), "of ", deparse(substitute(response)))
    type <- match.arg(type)
    cells <- tapply(response, list(x.factor, trace.factor), fun)
    nr <- nrow(cells)
    nc <- ncol(cells)
    xvals <- 1L:nr
    xvals = as.numeric(rownames(cells))
    if (is.ordered(x.factor)) {
        wn <- getOption("warn")
        options(warn = -1)
        xnm <- as.numeric(levels(x.factor))
        options(warn = wn)
        if (!any(is.na(xnm))) 
            xvals <- xnm
    }
    xlabs <- rownames(cells)
    ylabs <- colnames(cells)
    nch <- max(sapply(ylabs, nchar, type = "width"))
    if (is.null(xlabs)) 
        xlabs <- as.character(xvals)
    if (is.null(ylabs)) 
        ylabs <- as.character(1L:nc)
    xlim <- range(xvals)
    xleg <- xlim[2L] + 0.05 * diff(xlim)
    xlim <- xlim + c(-0.2/nr, if (legend) 0.2 + 0.02 * nch else 0.2/nr) * diff(xlim)
    matplot(xvals, cells, ..., type = type, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, axes = axes, xaxt = "n", col = col, lty = lty, pch = pch)
    if (axes && xaxt != "n") {
        axisInt <- function(x, main, sub, lwd, bg, log, asp, ...) axis(1, x, ...)
        mgp. <- par("mgp")
        if (!xtick) 
            mgp.[2L] <- 0
        axisInt(1, at = xvals, labels = xlabs, tick = xtick, mgp = mgp., xaxt = xaxt, ...)
    }
    if (legend) {
        yrng <- diff(ylim)
        yleg <- ylim[2L] - 0.1 * yrng
        if (!is.null(xpd) || {
            xpd. <- par("xpd")
            !is.na(xpd.) && !xpd. && (xpd <- TRUE)
        }) {
            op <- par(xpd = xpd)
            on.exit(par(op))
        }
        text(xleg, ylim[2L] - 0.05 * yrng, paste("  ", trace.label), adj = 0)
        if (!fixed) {
            ord <- sort.list(cells[nr, ], decreasing = TRUE)
            ylabs <- ylabs[ord]
            lty <- lty[1 + (ord - 1)%%length(lty)]
            col <- col[1 + (ord - 1)%%length(col)]
            pch <- pch[ord]
        }
        legend(xleg, yleg, legend = ylabs, col = col, pch = if (type %in% c("p", "b")) 
            pch, lty = if (type %in% c("l", "b")) 
            lty, bty = leg.bty, bg = leg.bg)
    }
    invisible(xvals)
}
setGeneric("effectPlot", def = function(object, factors, fun = mean, response = NULL, single = FALSE, points = FALSE, classic = FALSE, axes = TRUE, lty, xlab, ylab, ###
    main, ylim, ...) standardGeneric("effectPlot"))                             
setMethod(effectPlot, signature(object = "facDesign"), function(object, factors, fun = mean, response = NULL, single = FALSE, points = FALSE, classic = FALSE, axes = TRUE, ###
    lty, xlab, ylab, main, ylim, ...) {
    oldMar = par("mar")
    oldOma = par("oma")
    oldMfrow = par("mfrow")
    oldMfcol = par("mfcol")
    on.exit(par(mar = oldMar, oma = oldOma, mfrow = oldMfrow, mfcol = oldMfcol))
    if(is.null(response)==FALSE)                                                ###
    {                                                                           ###
     temp=response(object)[response]                                            ###
     response(object)=temp                                                      ###
    }                                                                           ###
    ylabmiss = FALSE
    xlabmiss = FALSE
    mainmiss = FALSE
    ylimmiss = FALSE
    if (missing(ylim)) 
        ylimmiss = TRUE
    if (missing(lty)) 
        lty = 1
    X = cube(object)
    Y = as.data.frame(object@response[1:nrow(X), ])
    names(Y) = names(response(object))
    if (!missing(factors)) 
        k = length(factors)
    else #(missing(factors))                                                    ###
    {
        k = ncol(X)
        factors = names(X)
    }
    numCol = 1
    numRow = 1
    if (!single && missing(factors)) {                                          ###
        if (ncol(X) == 2) {
            numCol = 2
            numRow = 1
        }
        if (ncol(X) > 2) {
            numCol = 2
            numRow = 2
        }
    }
    if (!single && !missing(factors)) {                                         ###
        if (length(factors) == 2) {                                             ###
            numCol = 2                                                          ###
            numRow = 1                                                          ###
        }                                                                       ###
        if (length(factors) == 3) {                                             ###
            numCol = 3                                                          ###
            numRow = 1                                                          ###
        }                                                                       ###
        if (length(factors) == 4) {                                             ###
            numCol = 2                                                          ###
            numRow = 2                                                          ###
        }                                                                       ###
        if (length(factors) == 5) {                                             ###
            numCol = 3                                                          ###
            numRow = 2                                                          ###
        }                                                                       ###
        if (length(factors) == 6) {                                             ###
            numCol = 3                                                          ###
            numRow = 2                                                          ###
        }                                                                       ###
        if (length(factors) > 6) {                                              ###
            numRow = ceiling(sqrt(length(factors)))                             ###
            numCol = ceiling(sqrt(length(factors)))                             ###
        }                                                                       ###
    }                                                                           ###
    if (classic) {
        numCol = ncol(X)
        numRow = 1
    }
    if (!single) 
        par(mfrow = c(numRow, numCol))
    nextResponse = FALSE
    for (j in 1:ncol(Y)) {
        counter = 0
        cells = numeric(0)
        for (i in 1:length(factors)) {
            cells = c(cells, as.vector(tapply(Y[, j], list(X[, factors[i]], rep(0, nrow(X))), fun)))
            if (points) 
                cells = range(Y)
        }
        if (nextResponse & !single) {
            dev.new()
            par(mfrow = c(numRow, numCol))
        }
        for (i in 1:length(factors)) {
            if ((counter != 0 & counter%%(numCol * numRow) == 0) & !single) {
                dev.new()
                par(mfrow = c(numRow, numCol))
            }
            if (missing(main)) {
                main = paste("Effect Plot for", names(Y)[j])
                mainmiss = TRUE
            }
            if (mainmiss) 
                main = paste("Effect Plot for", names(Y)[j])
            if (missing(xlab)) {
                xlab = factors[i]
                xlabmiss = TRUE
            }
            if (xlabmiss) {
                if (identical(" ", names(object)[[i]])) 
                  xlab = factors[i]
                else xlab = paste(factors[i], ": ", names(object)[[i]], sep = "")
            }
            if (missing(ylab)) {
                ylab = paste(deparse(substitute(fun)), "of ", names(Y)[j])
                ylabmiss = TRUE
            }
            if (ylabmiss) 
                ylab = paste(deparse(substitute(fun)), "of ", names(Y)[j])
            if (ylimmiss) 
                ylim = range(cells, na.rm = TRUE)
            if (classic & i == 1) {
                par(mar = c(5, 0, 0, 0) + 0.1)
                par(oma = c(-0.1, 4, 4, 1) + 0.1)
            }
            if (classic) {
                .m.interaction.plot(x.factor = X[, factors[i]], trace.factor = rep(0, nrow(X)), response = Y[, j], lty = lty, ylim = ylim, xlab = xlab, fun = fun, 
                  ylab = ylab, legend = FALSE, axes = FALSE, main = " ", ...)
                grid(NA, 2)
                axis(1, at = X[, factors[i]])
                if (i == 1) 
                  axis(2)
                box()
                title(main, outer = TRUE)
            }
            else {
                .m.interaction.plot(x.factor = X[, factors[i]], trace.factor = rep(0, nrow(X)), response = Y[, j], lty = lty, ylim = ylim, xlab = xlab, fun = fun, 
                  ylab = ylab, legend = FALSE, axes = axes, main = main, ...)
                grid(NA, 2)
            }
            if (points) 
                points(X[, factors[i]], Y[, j], ...)
            counter = counter + 1
        }
        nextResponse = TRUE
    }
})
setMethod(effectPlot, signature(object = "taguchiDesign"), function(object, factors, fun = mean, response = NULL, single = FALSE, points = FALSE, classic = FALSE,  ###
    axes = TRUE, lty, xlab, ylab, main, ylim, ...) {
    oldMar = par("mar")
    oldOma = par("oma")
    oldMfrow = par("mfrow")
    oldMfcol = par("mfcol")
    on.exit(par(mar = oldMar, oma = oldOma, mfrow = oldMfrow, mfcol = oldMfcol))
    if(is.null(response)==FALSE)                                                ###
    {                                                                           ###
     temp=response(object)[response]                                            ###
     response(object)=temp                                                      ###
    }                                                                           ###
    ylabmiss = FALSE
    xlabmiss = FALSE
    mainmiss = FALSE
    ylimmiss = FALSE
    if (missing(ylim)) 
        ylimmiss = TRUE
    if (missing(lty)) 
        lty = 1
    X = object@design
    Y = response(object)
    if (!missing(factors)) 
        k = length(factors)
    else #(missing(factors))                                                    ###
    {
        k = ncol(X)
        factors = names(X)
    }
    numCol = 1
    numRow = 1
    if (!single && missing(factors)) {                                          ###
        if (ncol(X) == 2) {
            numCol = 2
            numRow = 1
        }
        if (ncol(X) > 2) {
            numCol = 2
            numRow = 2
        }
    }
    if (!single && !missing(factors)) {                                         ###
        if (length(factors) == 2) {                                             ###
            numCol = 2                                                          ###
            numRow = 1                                                          ###
        }                                                                       ###
        if (length(factors) == 3) {                                             ###
            numCol = 3                                                          ###
            numRow = 1                                                          ###
        }                                                                       ###
        if (length(factors) == 4) {                                             ###
            numCol = 2                                                          ###
            numRow = 2                                                          ###
        }                                                                       ###
        if (length(factors) == 5) {                                             ###
            numCol = 3                                                          ###
            numRow = 2                                                          ###
        }                                                                       ###
        if (length(factors) == 6) {                                             ###
            numCol = 3                                                          ###
            numRow = 2                                                          ###
        }                                                                       ###
        if (length(factors) > 6) {                                              ###
            numRow = ceiling(sqrt(length(factors)))                             ###
            numCol = ceiling(sqrt(length(factors)))                             ###
        }                                                                       ###
    }                                                                           ###        
    if (classic) {
        numCol = ncol(X)
        numRow = 1
    }
    if (!single) 
        par(mfrow = c(numRow, numCol))
    nextResponse = FALSE
    for (j in 1:ncol(Y)) {
        counter = 0
        cells = numeric(0)
        for (i in 1:length(factors)) {
            cells = c(cells, as.vector(tapply(Y[, j], list(X[, factors[i]], rep(0, nrow(X))), fun)))
            if (points) 
                cells = range(Y)
        }
        if (nextResponse & !single) {
            dev.new()
            par(mfrow = c(numRow, numCol))
        }
        for (i in 1:length(factors)) {
            if ((counter != 0 & counter%%(numCol * numRow) == 0) & !single) {
                dev.new()
                par(mfrow = c(numRow, numCol))
            }
            if (missing(main)) {
                main = paste("Effect Plot for", names(Y)[j])
                mainmiss = TRUE
            }
            if (mainmiss) 
                main = paste("Effect Plot for", names(Y)[j])
            if (missing(xlab)) {
                xlab = factors[i]
                xlabmiss = TRUE
            }
            if (xlabmiss) {
                if (identical(" ", names(object)[[i]])) 
                  xlab = factors[i]
                else xlab = paste(factors[i], ": ", names(object)[[i]], sep = "")
            }
            if (missing(ylab)) {
                ylab = paste(deparse(substitute(fun)), "of ", names(Y)[j])
                ylabmiss = TRUE
            }
            if (ylabmiss) 
                ylab = paste(deparse(substitute(fun)), "of ", names(Y)[j])
            if (ylimmiss) 
                ylim = range(cells, na.rm = TRUE)
            if (classic & i == 1) {
                par(mar = c(5, 0, 0, 0) + 0.1)
                par(oma = c(-0.1, 4, 4, 1) + 0.1)
            }
            if (classic) {
                .m.interaction.plot(x.factor = X[, factors[i]], trace.factor = rep(0, nrow(X)), response = Y[, j], lty = lty, ylim = ylim, xlab = xlab, fun = fun, 
                  ylab = ylab, legend = FALSE, axes = FALSE, main = " ", ...)
                grid(NA, 2)
                axis(1, at = X[, factors[i]])
                if (i == 1) 
                  axis(2)
                box()
                title(main, outer = TRUE)
            }
            else {
                .m.interaction.plot(x.factor = X[, factors[i]], trace.factor = rep(0, nrow(X)), response = Y[, j], lty = lty, ylim = ylim, xlab = xlab, fun = fun, 
                  ylab = ylab, legend = FALSE, axes = axes, main = main, ...)
                grid(NA, 2)
            }
            if (points) 
                points(X[, factors[i]], Y[, j], ...)
            counter = counter + 1
        }
        nextResponse = TRUE
    }
}) 
