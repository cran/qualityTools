#test = expand.grid(c(-1,1), c(-1,1))
#test = cbind(test, y = rnorm(4))
#
#test = expand.grid(c(-1,0,1), c(-1,0,1))
#test = cbind(test, y = rnorm(9))
#names(test) = c("kurz", "wirklichlangerName", "y")
#
#with(test, interactionPlot(kurz, wirklichlangerName, y))
#interactionPlot(test[,1], test[,2], test[,3], fun = median)
#interactionPlot(test[,1], test[,2], test[,3], fun = mean)
#interactionPlot(test[,1], test[,2], test[,3], fun = max)
#
#
#test = expand.grid(c(-1,1), c(-1,1), c(-1,1))
#test = cbind(test, rnorm(8, mean = 10))
#names(test) = c("A", "B", "C", "y")
#
#
#test = fracDesign(k = 3)
#response(test) = rnorm(8)
#
#

#helper function that returns the position of the LETTER in the alphabet
.letterPos = function(LETTER)
{
  if(!(nchar(LETTER) == 1))
    stop("factor names should be single characters only")

  return((1:26)[LETTERS[1:26] == LETTER])
                                                                                                             
}


#.aip is a slightly altered version of R's interaction.plot
.testFun = function (x.factor, trace.factor, response, fun = mean, type = c("l",
    "p", "b"), legend = TRUE, trace.label = deparse(substitute(trace.factor)),
    fixed = FALSE, xlab = deparse(substitute(x.factor)), ylab = ylabel,
    ylim = range(cellNew, na.rm = TRUE), lty = nc:1, col = 1, pch = c(1L:9,
        0, letters), xpd = NULL, leg.bg = par("bg"), leg.bty = "o",
    xtick = FALSE, xaxt = par("xaxt"), axes = TRUE, title = "", ...)
{
    ylabel <- paste(deparse(substitute(fun)), "of ", deparse(substitute(response)))
    type <- match.arg(type)
    cellNew <- tapply(response, list(x.factor, trace.factor), fun)
    nr <- nrow(cellNew)
    nc <- ncol(cellNew)
    xvals <- 1L:nr
    if (is.ordered(x.factor)) {
        wn <- getOption("warn")
        options(warn = -1)
        xnm <- as.numeric(levels(x.factor))
        options(warn = wn)
        if (!any(is.na(xnm)))
            xvals <- xnm
    }
    xlabs <- rownames(cellNew)
    ylabs <- colnames(cellNew)
    nch <- max(sapply(ylabs, nchar, type = "width"))
    if (is.null(xlabs))
        xlabs <- as.character(xvals)
    if (is.null(ylabs))
        ylabs <- as.character(1L:nc)
    xlim <- range(xvals)
    xleg <- xlim[2L] + 0.05 * diff(xlim)
    xlim <- xlim + c(-0.2/nr, if (legend) 0.2 + 0.02 * nch else 0.2/nr) *
        diff(xlim)
    matplot(xvals, cellNew, ..., type = type, xlim = xlim, ylim = ylim,
        xlab = xlab, ylab = ylab, axes = axes, xaxt = "n", col = col,
        lty = lty, pch = pch)
    if (axes && xaxt != "n") {
        axisInt <- function(x, main, sub, lwd, bg, log, asp,
            ...) axis(1, x, ...)
        mgp. <- par("mgp")
        if (!xtick)
            mgp.[2L] <- 0
        axisInt(1, at = xvals, labels = xlabs, tick = xtick,
            mgp = mgp., xaxt = xaxt, ...)
    }
    if(legend)
    {
      legpretty = ylabs
      #make a nice character vector for the legend
    #  if(all(is.numeric(as.numeric(ylabs))))
#      {
#        leglevel = as.numeric(ylabs)
#        legpretty = as.character(abs(as.numeric(ylabs)))
#        temp = character(length(ylabs))
#        temp[leglevel > 0] = "+"
#        temp[leglevel < 0] = "-"
#        temp[leglevel == 0] = " "
#        legpretty = paste(temp, legpretty, sep = "")
#      }
      legend("topright", legend = legpretty, title = title, col = col, pch = if (type %in% c("p", "b")) pch, lty = if (type %in% c("l", "b")) lty, bty = leg.bty, bg = leg.bg, inset = 0.02)
    }
  return(list(xvals,xlabs))
}



#Not used anymore
##Eine fuer ein facDesignObject angepasste Version von interaction.plot welche die tickposition der x-Achsen und die zugehoerigen labels zurueckgibt
.testFun2 = function (x.factor, trace.factor, response, fun = mean, type = c("l",
    "p", "b"), legend = TRUE, trace.label = deparse(substitute(trace.factor)),
    fixed = FALSE, xlab = deparse(substitute(x.factor)), ylab = ylabel,
    ylim = range(cells, na.rm = TRUE), lty = nc:1, col = 1, pch = c(1:9,
        0, letters), xpd = NULL, leg.bg = par("bg"), leg.bty = "n",
    xtick = FALSE, xaxt = par("xaxt"), axes = TRUE, ...)
{
    ylabel <- paste(deparse(substitute(fun)), "of ", deparse(substitute(response)))
    type <- match.arg(type)

    cells <- tapply(response, list(x.factor, trace.factor), fun)
    nr <- nrow(cells)
    nc <- ncol(cells)
    xvals <- 1:nr

    if (is.ordered(x.factor)) {
        wn <- getOption("warn")
        options(warn = -1)
        xnm <- as.numeric(levels(x.factor))
        options(warn = wn)
        if (!any(is.na(xnm)))
            xvals <- xnm
    }


    ###need this one as a list
    xlabs <- rownames(cells)
    ylabs <- colnames(cells)


    nch <- max(sapply(ylabs, nchar, type = "width"))
    if (is.null(xlabs))
        xlabs <- as.character(xvals)
    if (is.null(ylabs))
        ylabs <- as.character(1:nc)
    xlim <- range(xvals)
    xleg <- xlim[2] + 0.05 * diff(xlim)
    xlim <- xlim + c(-0.2/nr, if (legend) 0.3 + 0.02 * nch else 0.3/nr) *
        diff(xlim)
    matplot(xvals, cells, ..., type = type, xlim = xlim, ylim = ylim,
        xlab = xlab, ylab = ylab, axes = axes, xaxt = "n", col = col,
        lty = lty, pch = pch)
    if (axes && xaxt != "n") {
        axisInt <- function(x, main, sub, lwd, bg, log, asp,
            ...) axis(1, x, ...)
        mgp. <- par("mgp")
        if (!xtick)
            mgp.[2] <- 0
        axisInt(1, at = xvals, labels = xlabs, tick = xtick,
            mgp = mgp., xaxt = xaxt, ...)
    }
    if (legend) {
        yrng <- diff(ylim)
        yleg <- ylim[2] - 0.1 * yrng
        if (!is.null(xpd) || {
            xpd. <- par("xpd")
            !is.na(xpd.) && !xpd. && (xpd <- TRUE)
        }) {
            op <- par(xpd = xpd)
            on.exit(par(op))
        }
        text(xleg - 0.05, ylim[2] - 0.05 * yrng, paste("  ", trace.label),
            adj = 0)
        if (!fixed) {
            ord <- sort.list(cells[nr, ], decreasing = TRUE)
            ylabs <- ylabs[ord]
            lty <- lty[1 + (ord - 1)%%length(lty)]
            col <- col[1 + (ord - 1)%%length(col)]
            pch <- pch[ord]
        }
        legend(xleg, yleg, legend = ylabs, col = col, pch = if (type %in%
            c("p", "b"))
            pch, lty = if (type %in% c("l", "b"))
            lty, bty = leg.bty, bg = leg.bg)
    }
    return(list(xvals,xlabs))
}


.interactionPlotOld = function (x.factor, trace.factor, response, fun = mean, type = c("l",
    "p", "b"), legend = TRUE, trace.label = deparse(substitute(trace.factor)),
    fixed = FALSE, xlab = deparse(substitute(x.factor)), ylab = ylabel,
    ylim = range(cells, na.rm = TRUE), lty = nc:1, col = 1, pch = c(1:9,
        0, letters), xpd = NULL, leg.bg = par("bg"), leg.bty = "n",
    xtick = FALSE, xaxt = par("xaxt"), axes = TRUE, ...)
{
    ylabel <- paste(deparse(substitute(fun)), "of ", deparse(substitute(response)))
    type <- match.arg(type)
    cells <- tapply(response, list(x.factor, trace.factor), fun)
    nr <- nrow(cells)
    nc <- ncol(cells)
    xvals <- 1:nr
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
        ylabs <- as.character(1:nc)
    xlim <- range(xvals)
    xleg <- xlim[2] + 0.05 * diff(xlim)
    xlim <- xlim + c(-0.2/nr, if (legend) 0.2 + 0.02 * nch else 0.2/nr) *
        diff(xlim)
    matplot(xvals, cells, ..., type = type, xlim = xlim, ylim = ylim,
        xlab = xlab, ylab = ylab, axes = axes, xaxt = "n", col = col,
        lty = lty, pch = pch)
    if (axes && xaxt != "n") {
        axisInt <- function(x, main, sub, lwd, bg, log, asp,
            ...) axis(1, x, ...)
        mgp. <- par("mgp")
        if (!xtick)
            mgp.[2] <- 0
        axisInt(1, at = xvals, labels = xlabs, tick = xtick,
            mgp = mgp., xaxt = xaxt, ...)
    }
    if (legend) {
        yrng <- diff(ylim)
        yleg <- ylim[2] - 0.1 * yrng
        if (!is.null(xpd) || {
            xpd. <- par("xpd")
            !is.na(xpd.) && !xpd. && (xpd <- TRUE)
        }) {
            op <- par(xpd = xpd)
            on.exit(par(op))
        }
        text(xleg, ylim[2] - 0.05 * yrng, paste("  ", trace.label),
            adj = 0)
        if (!fixed) {
            ord <- sort.list(cells[nr, ], decreasing = TRUE)
            ylabs <- ylabs[ord]
            lty <- lty[1 + (ord - 1)%%length(lty)]
            col <- col[1 + (ord - 1)%%length(col)]
            pch <- pch[ord]
        }
        legend(xleg, yleg, legend = ylabs, col = col, pch = if (type %in%
            c("p", "b"))
            pch, lty = if (type %in% c("l", "b"))
            lty, bty = leg.bty, bg = leg.bg)
    }
    invisible()
}



#fdo ist ein facDesignObject d.h. ein entsprechender Versuchsplan oder ein Vector
interactionPlot = function(fdo = NULL, y = NULL, response = NULL, fun = mean, main = paste("Interaction plot for ",deparse(substitute(fdo))),col = 1:2, ...)
{
  DB = FALSE
  
  parList = list(...)

  old.par <- par(no.readonly = TRUE) #store all par settings which could be changed.
#  on.exit(par(old.par))

  #no facDesign but only x, y and response are given --> take the R-implementation of the interaction.plot
  if(class(fdo) != "facDesign")
  {
    if(any(is.null(fdo), is.null(y), is.null(response)))
      stop("Factors or response are not given!")

    .interactionPlotOld(fdo, y, response, fun = fun, main = main, ...) #TODO: do this by masking with any

    return()
  }

  else
  {

  diagNames = character(0)  #holds the names that were plotted on the diag of the plot matrix

  x = cube(fdo)
  runIndex = order(runOrd(fdo))
  x = x[runIndex[1:nrow(x)],]
  y = response(fdo)[1:nrow(x),]

  numFac = ncol(x)  #Anzahl Faktoren

  combMat = combn(names(x), 2)  #alle 2er Kombinationen von Faktoren bilden


  #Spezialfall nur 2 Faktoren d.h. genau ein Plot wird erscheinen
  if(numFac == 2)
  {
    facName2 = combMat[1,1]
    facName1 = combMat[2,1]
    temp = with(cbind(y,x), .testFun(eval(parse(text = facName2)),eval(parse(text = facName1)), xlab = facName1, response = y, trace.label = facName1, ylim = range(y), axes = F, fun , title = facName1, col = col,...))
    tempList = parList
    tempList$col = 1
    tempList$lwd = 1
  
    tempList$side = c(2)
     do.call(axis, tempList)
    box()
    
    tempList$at = temp[[1]]
    tempList$labels = temp[[2]]
    tempList$side = c(1)
      do.call(axis, tempList)

    return()
  }


  #prepare the window
#  plot.new()
  par(mfrow = c(numFac, numFac))
  par(mar = c(0,0,0,0))         #no margins
  par(oma = c(0,0,8,8))         #but outermargins instead
  plot(1,1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")


  for(i in 1:ncol(combMat))
  {
   facName1 = combMat[1,i]
   facName2 = combMat[2,i]
   rowNum = .letterPos(facName1)
   colNum = .letterPos(facName2) #+1 da in der ersten Spalte der kodierte Name des Buchstabens steht

   if(DB)
   {
     print(numFac)
     print(rowNum)
     print(colNum)
     print(facName1)
     print(facName2)
     print(y)
     print(fun)
     cat(paste(i,"\t", c(rowNum, colNum)))
   }



   par(mfg = c(rowNum, colNum))

   # Rueckgabe der entsprechenden Werte
   temp = with(cbind(x,y), .testFun(eval(parse(text = facName2)),eval(parse(text = facName1)), response = y, trace.label = facName1, ylim = range(y), axes = F, fun = fun, title = facName1, col = col,...))
   if(colNum == numFac)
   {
    tempList = parList
    tempList$col = 1
    tempList$lwd = 1
    tempList$side = 4
    do.call(axis, tempList)
#    axis(4, ...)
   }

   if(rowNum == 1)  #put the axes on the upper outside only
   {
    tempList = parList
    tempList$col = 1
    tempList$lwd = 1
    tempList$side = 3
    tempList$at = temp[[1]]
    tempList$labels = temp[[2]]
    do.call(axis, tempList)
#    axis(3, at = temp[[1]], labels = temp[[2]], ...)
   }
    
   box(which = "plot")

   par(mfg = c(rowNum, rowNum))
   plot(c(-1,1), c(-1,1), type = "n", axes = F, xlab = "", ylab = "", main = "")
   text(0,0, facName1, cex = 4)
   diagNames = c(diagNames, facName1)
  }

  par(mfg = c(numFac, numFac))
  plot(c(-1,1), c(-1,1), type = "n", axes = F, xlab = "", ylab = "", main = "")
  text(0,0,setdiff(names(x), diagNames), cex = 4)  #plot (text) the remaining factor on the trace of the plot matrix

  title(main, outer = T, ...)
  }


  par(old.par)
  invisible()
}


class(interactionPlot) <- "invisible"

#Example
#check for compatibility with the old interaction.plot
#temp = expand.grid(c(-1,1), c(-1,1), c(-1,1))
#temp = cbind(temp, rnorm(8, mean = 10))
#names(temp) = c("A", "B", "C", "y")
#with(temp, interactionPlot(A , B ,  response = 1:8, main = "test"))
#     