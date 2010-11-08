setClass("gageRR", representation = representation(X = "data.frame", 
    ANOVA = "aov", RedANOVA = "aov", method = "character", Estimates = "list", 
    Varcomp = "list", Sigma = "numeric", GageName = "character", 
    GageTolerance = "numeric", DateOfStudy = "character", PersonResponsible = "character", 
    Comments = "character", b = "factor", a = "factor", y = "numeric", 
    facNames = "character"))
setMethod("show", signature(object = "gageRR"), function(object) {
    print(as.data.frame(object))
})
setMethod("[", signature(x = "gageRR", i = "ANY", 
    j = "ANY"), function(x, i, j) {
    x@X[i, j]
})
setMethod("summary", signature(object = "gageRR"), 
    function(object) {
        if (all(is.na(object@X$Measurement))) 
            return(print(as.data.frame(object)))
        return(gageRR(object))
    })
setMethod("response", "gageRR", function(object) {
    out = object@X$Measurement
    return(out)
})
setReplaceMethod("response", "gageRR", function(object, 
    value) {
    object@X$Measurement = value
    return(object)
})
setMethod("names", signature(x = "gageRR"), function(x) {
    return(names(as.data.frame(x)))
})
setMethod("as.data.frame", "gageRR", function(x, row.names = NULL, 
    optional = FALSE, ...) {
    return(x@X)
})
as.data.frame.gageRR = function(x, row.names = NULL, 
    optional = FALSE, ...) {
    return(x@X)
}
setGeneric("tolerance", function(x) standardGeneric("tolerance"))
setGeneric("tolerance<-", function(x, value) standardGeneric("tolerance<-"))
setMethod("tolerance", "gageRR", function(x) unlist(x@GageTolerance))
setReplaceMethod("tolerance", "gageRR", function(x, 
    value) {
    if (!is.numeric(value)) 
        stop(paste(deparse(substitute(value)), "needs to be numeric"))
    x@GageTolerance = value
    return(x)
})
setGeneric("sigma", function(x) standardGeneric("sigma"))
setGeneric("sigma<-", function(x, value) standardGeneric("sigma<-"))
setMethod("sigma", "gageRR", function(x) unlist(x@Sigma))
setReplaceMethod("sigma", "gageRR", function(x, value) {
    if (!is.numeric(value)) 
        stop(paste(deparse(substitute(value)), "needs to be numeric"))
    x@Sigma = value
    return(x)
})
.aip = function(x.factor, trace.factor, response, 
    fun = mean, type = c("l", "p", "b"), legend = TRUE, trace.label = deparse(substitute(trace.factor)), 
    fixed = FALSE, xlab = deparse(substitute(x.factor)), ylab = ylabel, 
    ylim = range(cellNew, na.rm = TRUE), lty = nc:1, col = 1, 
    pch = c(1L:9, 0, letters), xpd = NULL, leg.bg = par("bg"), 
    leg.bty = "o", xtick = FALSE, xaxt = par("xaxt"), axes = TRUE, 
    title = "", ...) {
    ylabel <- paste(deparse(substitute(fun)), "of ", deparse(substitute(response)))
    type <- match.arg(type)
    cellNew <- tapply(response, list(x.factor, trace.factor), 
        fun)
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
    if (legend) {
        legend("topright", legend = ylabs, title = title, col = col, 
            pch = if (type %in% c("p", "b")) 
                pch, lty = if (type %in% c("l", "b")) 
                lty, bty = leg.bty, bg = leg.bg, inset = 0.02)
    }
    invisible()
}
gageRRDesign = function(Operators = 3, Parts = 10, 
    Measurements = 3, method = "crossed", sigma = 6, randomize = TRUE) {
    method = method
    opvec = factor()
    partvec = factor()
    yName = aName = bName = abName = NA
    yName = "Measurement"
    aName = "Operator"
    bName = "Part"
    abName = "Operator:Part"
    Operators = unique(Operators)
    Parts = unique(Parts)
    if (!is.numeric(sigma)) 
        stop("sigma needs to be numeric")
    if (method != "nested" && method != "crossed") 
        warning("unknown method specified --> defaulting to \"method = crossed\"")
    if (!is.numeric(Measurements)) 
        stop("Number of Measurements per Part not specified!")
    else Measurements = round(Measurements[1])
    if (!is.numeric(Operators) && !is.character(Operators)) 
        stop("Operator needs to be numeric 'Operator = 3' or character 'Operator = c(\"A\",\"B\", \"C\"")
    if (is.numeric(Operators)) 
        opvec = factor(LETTERS[1:Operators[1]])
    if (is.character(Operators)) 
        opvec = factor(Operators)
    if (length(unique(opvec)) > 26) 
        stop("To many Operators!")
    if (length(unique(opvec)) < 2) 
        stop("To few Operators")
    if (!is.numeric(Parts) && !is.character(Parts)) 
        stop("Parts needs to be numeric 'Parts = 3' or character 'Parts = c(\"A\",\"B\", \"C\"")
    if (is.numeric(Parts)) 
        partvec = factor(LETTERS[1:Parts[1]])
    if (is.character(Parts)) 
        partvec = factor(Parts)
    if (length(unique(partvec)) > 26) 
        stop("To many Parts!")
    if (length(unique(partvec)) < 2) 
        stop("To few Parts")
    Measurement = rep(NA, (length(opvec) * length(partvec) * 
        Measurements))
    outFrame = data.frame()
    if (method == "crossed") {
        temp = expand.grid(opvec, partvec)
        o = rep(temp[, 1], Measurements)
        p = rep(temp[, 2], Measurements)
    }
    else {
        p = factor(sort(rep(1:(length(partvec) * length(opvec)), 
            Measurements)))
        o = sort(rep(opvec, length(partvec)/length(opvec)))
    }
    if (randomize) 
        outFrame = data.frame(StandardOrder = 1:length(Measurement), 
            RunOrder = sample(1:length(Measurement), length(Measurement)), 
            Operator = factor(o), Part = factor(p), Measurement)
    else outFrame = data.frame(StandardOrder = 1:length(Measurement), 
        RunOrder = 1:length(Measurement), Operator = factor(o), 
        Part = factor(p), Measurement)
    outFrame = outFrame[order(outFrame$RunOrder), ]
    gageRRObj = new("gageRR")
    gageRRObj@facNames = c(yName, aName, bName, abName)
    names(gageRRObj@facNames) = c("yName", "aName", "bName", 
        "abName")
    gageRRObj@Sigma = sigma
    gageRRObj@method = method
    gageRRObj@a = factor(o)
    gageRRObj@b = factor(p)
    gageRRObj@y = as.numeric(Measurement)
    gageRRObj@method = method
    gageRRObj@Sigma = sigma
    gageRRObj@X = outFrame
    return(gageRRObj)
}
gageRR = function(gdo, method = "crossed", sigma = 5.15, 
    alpha = 0.25, DM = NULL, HM = NULL, toler = NULL, plotit = F, 
    dig = 4, ...) {
    yName = names(gdo)[5]
    aName = names(gdo)[3]
    bName = names(gdo)[4]
    abName = paste(aName, ":", bName, sep = "")
    bTobName = paste(bName, "to", bName, sep = " ")
    a = gdo@X[, aName]
    b = gdo@X[, bName]
    y = gdo@X[, yName]
    nestedFormula = as.formula(paste(yName, "~", aName, "/", 
        bName))
    crossedFormula = as.formula(paste(yName, "~", aName, "*", 
        bName))
    reducedFormula = as.formula(paste(yName, "~", aName, "+", 
        bName))
    if (is.na(y) || !is.numeric(y)) 
        stop("Measurements need to be numeric")
    if (method %in% c("crossed", "nested")) 
        method = method
    else method = gdo@method
    if (method == "nested") {
        alevels = unique(a)
        for (i in 1:length(alevels)) {
            blevels = unique(b[a == alevels[i]])
            if (any((blevels %in% b[a != alevels[i]]) == TRUE)) 
                stop("Design is not nested in the form given\nI check if nested\nII check encoding")
        }
        numA <- nlevels(a[, drop = T])
        numB <- nlevels(b[, drop = T])
        numMPP <- length(a)/(numB)
        fit = aov(nestedFormula, data = gdo)
        meanSq <- anova(fit)[, 3]
        gdo@ANOVA = fit
        gdo@method = "nested"
        MSa = meanSq[1]
        MSab = meanSq[2]
        MSe = meanSq[3]
        Cerror = MSe
        Cb = (MSab - MSe)/numMPP
        Ca = (MSa - MSab)/(numB * numMPP)
        if (Ca < 0) 
            Ca = 0
        Cab = 0
        totalRR = Ca + Cab + Cerror
        repeatability = Cerror
        reproducibility = Ca
        bTob = Cb
        totalVar = Cb + Ca + Cab + Cerror
        estimates = list(Cb = Cb, Ca = Ca, Cab = Cab, Cerror = Cerror)
        varcomp = list(totalRR = totalRR, repeatability = repeatability, 
            reproducibility = reproducibility, bTob = bTob, totalVar = totalVar)
        gdo@Estimates = estimates
        gdo@Varcomp = varcomp
    }
    if (method == "crossed") {
        numA <- nlevels(a[, drop = T])
        numB <- nlevels(b[, drop = T])
        numMPP <- length(a)/(numA * numB)
        fit = aov(crossedFormula, data = gdo)
        model <- anova(fit)
        gdo@ANOVA = fit
        gdo@method = "crossed"
        MSb = MSa = MSab = MSe = 0
        if (bName %in% row.names(model)) 
            MSb = model[bName, "Mean Sq"]
        else warning(paste("missing factor", bName, "in model"))
        if (aName %in% row.names(model)) 
            MSa = model[aName, "Mean Sq"]
        else warning(paste("missing factor", aName, "in model"))
        if (abName %in% row.names(model)) 
            MSab = model[abName, "Mean Sq"]
        else warning(paste("missing interaction", abName, "in model"))
        if ("Residuals" %in% row.names(model)) 
            MSe = model["Residuals", "Mean Sq"]
        else warning("missing Residuals in model")
        Cb = Ca = Cab = Cerror = 0
        Cb = (MSb - MSab)/(numA * numMPP)
        Ca = (MSa - MSab)/(numB * numMPP)
        Cab = (MSab - MSe)/(numMPP)
        Cerror = (MSe)
        gdo@RedANOVA = gdo@ANOVA
        if ((Cab < 0) || (model[abName, "Pr(>F)"] >= alpha)) {
            redFit <- aov(reducedFormula, data = gdo)
            model <- anova(redFit)
            MSb = MSa = MSab = MSe = 0
            if (bName %in% row.names(model)) 
                MSb = model[bName, "Mean Sq"]
            else warning(paste("missing factor", bName, "in model"))
            if (aName %in% row.names(model)) 
                MSa = model[aName, "Mean Sq"]
            else warning(paste("missing factor", aName, "in model"))
            if ("Residuals" %in% row.names(model)) 
                MSe = model["Residuals", "Mean Sq"]
            else warning("missing Residuals in model")
            Cb = Ca = Cab = Cerror = 0
            Cb = (MSb - MSab)/(numA * numMPP)
            Ca = (MSa - MSab)/(numB * numMPP)
            Cab = 0
            Cerror = (MSe)
            gdo@RedANOVA = redFit
        }
        gdo@method = "crossed"
        totalRR = Ca + Cab + Cerror
        repeatability = Cerror
        reproducibility = Ca + Cab
        bTob = Cb
        totalVar = Cb + Ca + Cab + Cerror
        estimates = list(Cb = Cb, Ca = Ca, Cab = Cab, Cerror = Cerror)
        varcomp = list(totalRR = totalRR, repeatability = repeatability, 
            reproducibility = reproducibility, a = Ca, a_b = Cab, 
            bTob = bTob, totalVar = totalVar)
        gdo@Estimates = estimates
        gdo@Varcomp = varcomp
    }
    cat("\n")
    cat(paste("AnOVa Table - ", gdo@method, "Design\n"))
    print(summary(gdo@ANOVA))
    cat("\n")
    cat("----------\n")
    if (!identical(gdo@RedANOVA, gdo@ANOVA)) {
        cat(paste("AnOVa Table Without Interaction - ", gdo@method, 
            "Design\n"))
        print(summary(gdo@RedANOVA))
        cat("\n")
        cat("----------\n")
    }
    Source = names(gdo@Varcomp)
    Source[Source == "repeatability"] = " repeatability"
    Source[Source == "reproducibility"] = " reproducibility"
    Source[Source == "a_b"] = paste("  ", abName)
    Source[Source == "a"] = paste("  ", aName)
    Source[Source == "bTob"] = bTobName
    VarComp = round(as.numeric(gdo@Varcomp[c(1:length(gdo@Varcomp))]), 
        3)
    Contribution = round(as.numeric(gdo@Varcomp[c(1:length(gdo@Varcomp))])/as.numeric(gdo@Varcomp[length(gdo@Varcomp)]), 
        3)
    VarComp = t(data.frame(gdo@Varcomp))
    VarCompContrib = VarComp/gdo@Varcomp$totalVar
    Stdev = sqrt(VarComp)
    StudyVar = Stdev * gdo@Sigma
    StudyVarContrib = StudyVar/StudyVar["totalVar", ]
    SNR = NA
    ptRatio = NULL
    temp = NULL
    if ((length(gdo@GageTolerance) > 0) && (gdo@GageTolerance > 
        0)) {
        ptRatio = StudyVar/gdo@GageTolerance
        temp = data.frame(VarComp, VarCompContrib, Stdev, StudyVar, 
            StudyVarContrib, ptRatio)
        names(temp)[6] = c("P/T Ratio")
        row.names(temp) = c(Source)
        SNR = sqrt(2 * (temp[bTobName, "VarComp"]/temp["totalRR", 
            "VarComp"]))
    }
    else {
        temp = data.frame(VarComp, VarCompContrib, Stdev, StudyVar, 
            StudyVarContrib)
        row.names(temp) = c(Source)
    }
    cat("\n")
    cat("Gage R&R\n")
    tempout = temp
    print(format(tempout, digits = 3))
    cat("\n")
    cat("---\n")
    cat(" * Contrib equals Contribution in %\n")
    if (!is.na(SNR)) 
        cat(paste(" **Number of Distinct Categories (signal-to-noise-ratio) =", 
            floor(SNR), "\n"))
    cat("\n")
    invisible(gdo)
}
setGeneric("plot", function(x, y, ...) standardGeneric("plot"))
setMethod("plot", signature(x = "gageRR"), function(x, 
    y, main, xlab, ylab, col, lwd, fun = mean, ...) {
    horiz = FALSE
    parList = list(...)
    gdo = x
    yName = names(gdo)[5]
    aName = names(gdo)[3]
    bName = names(gdo)[4]
    abName = paste(aName, ":", bName, sep = "")
    if (missing(col)) 
        col = 1:length(unique(gdo[, 3]))
    if (missing(lwd)) 
        lwd = 1
    par(mfrow = c(2, 2))
    temp = NULL
    Source = names(gdo@Varcomp)
    VarComp = round(as.numeric(gdo@Varcomp[c(1:length(gdo@Varcomp))]), 
        3)
    Contribution = round(as.numeric(gdo@Varcomp[c(1:length(gdo@Varcomp))])/as.numeric(gdo@Varcomp[length(gdo@Varcomp)]), 
        3)
    VarComp = t(data.frame(gdo@Varcomp))
    VarCompContrib = VarComp/gdo@Varcomp$totalVar
    Stdev = sqrt(VarComp)
    StudyVar = Stdev * gdo@Sigma
    StudyVarContrib = StudyVar/StudyVar["totalVar", ]
    if ((length(gdo@GageTolerance) > 0) && (gdo@GageTolerance > 
        0)) {
        ptRatio = StudyVar/gdo@GageTolerance
        temp = data.frame(VarComp, VarCompContrib, Stdev, StudyVar, 
            StudyVarContrib, ptRatio)
        contribFrame = data.frame(VarCompContrib, StudyVarContrib, 
            ptRatio)
        names(temp)[6] = c("P/T Ratio")
        row.names(temp) = c(Source)
        SNR = sqrt(2 * (temp["bTob", "VarComp"]/temp["totalRR", 
            "VarComp"]))
    }
    else {
        temp = data.frame(VarComp, VarCompContrib, Stdev, StudyVar, 
            StudyVarContrib)
        contribFrame = data.frame(VarCompContrib, StudyVarContrib)
    }
    bTob = paste(bName, "To", bName, sep = "")
    Source[Source == "bTob"] = bTob
    row.names(contribFrame) = Source
    if (gdo@method == "crossed") 
        contribFrame = contribFrame[-match(c("totalVar", "a", 
            "a_b"), row.names(temp)), ]
    else contribFrame = contribFrame[-match(c("totalVar"), row.names(temp)), 
        ]
    numBars = ncol(contribFrame)
    ymax = max(max(contribFrame))
    main1 = NA
    if (missing(main) || is.na(main[1])) 
        main1 = "Components of Variation"
    else main1 = main[1]
    xlab1 = NA
    if (missing(xlab) || is.na(xlab[1])) 
        xlab1 = "component"
    else xlab1 = xlab[1]
    ylab1 = NA
    if (missing(ylab) || is.na(ylab[1])) 
        ylab1 = ""
    else ylab1 = ylab[1]
    argList = list(...)
    redList = argList[names(argList) != "cex"]
    mybp = do.call(barplot, c(list(t(contribFrame), xlab = xlab1, 
        ylab = ylab1, main = main1, names.arg = rep("", 4), axes = F, 
        beside = T, ylim = c(0, 1.3 * ymax), col = col[1:numBars]), 
        redList))
    axis(1, at = colMeans(mybp), labels = names(as.data.frame(t(contribFrame))), 
        ...)
    axis(2, ...)
    box()
    legend("topright", names(contribFrame), col = col[1:numBars], 
        pch = c(15, 15), horiz = horiz, inset = 0.02)
    if (gdo@method == "crossed") {
        main2 = NA
        if (missing(main) || is.na(main[2])) 
            main2 = paste(yName, "by", bName)
        else main2 = main[2]
        xlab2 = NA
        if (missing(xlab) || is.na(xlab[2])) 
            xlab2 = bName
        else xlab2 = xlab[2]
        ylab2 = NA
        if (missing(ylab) || is.na(ylab[2])) 
            ylab2 = yName
        else ylab2 = ylab[2]
        plot(as.numeric(gdo[, 4]), jitter(gdo[, 5]), axes = F, 
            ylim = c(range(gdo[, 5])[1], range(gdo[, 5])[2] + 
                diff(range(gdo[, 5])) * 0.3), xlab = xlab2, ylab = ylab2, 
            main = main2, ...)
        box()
        axeVals = axis(1, ...)
        axis(2, ...)
        mByPa = split(gdo[, 5], as.numeric(gdo[, 4]))
        lines(sort(as.numeric(gdo[, 4])), lapply(mByPa, mean)[sort(as.numeric(gdo[, 
            4]))], lwd = lwd)
        points(sort(as.numeric(gdo[, 4])), lapply(mByPa, mean)[sort(as.numeric(gdo[, 
            4]))], pch = 15, ...)
        main3 = NA
        if (missing(main) || is.na(main[3])) 
            main3 = paste(yName, "by", aName)
        else main3 = main[3]
        xlab3 = NA
        if (missing(xlab) || is.na(xlab[3])) 
            xlab3 = aName
        else xlab3 = xlab[3]
        ylab3 = NA
        if (missing(ylab) || is.na(ylab[3])) 
            ylab3 = yName
        else ylab3 = ylab[3]
        colVec = .mapping(gdo[, 3], sort(unique(gdo[, 3])), col[1:length(unique(gdo[, 
            3]))])
        plot(as.numeric(gdo[, 3]), jitter(gdo[, 5]), axes = F, 
            ylim = c(range(gdo[, 5])[1], range(gdo[, 5])[2] + 
                diff(range(gdo[, 5])) * 0.3), col = colVec, xlab = xlab3, 
            ylab = ylab3, main = main3, ...)
        axis(1, at = sort(unique(as.numeric(gdo[, 3]))), labels = sort(unique(gdo[, 
            3])), ...)
        axis(2, ...)
        box()
        mByOp = split(gdo[, 5], as.numeric(gdo[, 3]))
        lines(sort(as.numeric(factor(names(mByOp)))), lapply(mByOp, 
            mean)[sort(names(mByOp))], lwd = lwd)
        main4 = NA
        if (missing(main) || is.na(main[4])) 
            main4 = paste("Interaction", abName)
        else main4 = main[4]
        xlab4 = NA
        if (missing(xlab) || is.na(xlab[4])) 
            xlab4 = names(gdo)[4]
        else xlab4 = xlab[4]
        ylab4 = NA
        if (missing(ylab) || is.na(ylab[4])) 
            ylab4 = paste(as.character(body(match.fun(fun)))[2], 
                "of", names(gdo)[5])
        else ylab4 = ylab[4]
        .aip(gdo[, 4], gdo[, 3], response = gdo[, 5], xlab = xlab4, 
            ylab = ylab4, main = main4, col = col, type = "b", 
            title = names(gdo)[3], ...)
    }
    else {
        main2 = NA
        if (missing(main) || is.na(main[2])) 
            main2 = paste(yName, "By", bName, "Within", aName)
        else main2 = main[2]
        xlab2 = NA
        if (missing(xlab) || is.na(xlab[2])) 
            xlab2 = bName
        else xlab2 = xlab[2]
        ylab2 = NA
        if (missing(ylab) || is.na(ylab[2])) 
            ylab2 = yName
        else ylab2 = ylab[2]
        index = order(gdo[, 3])
        colVec = .mapping(gdo[, 3][index], sort(unique(gdo[, 
            3][index])), col[1:length(unique(gdo[, 3]))])
        plotIndex = sort(rep(1:length(unique(gdo[, 4])), table(gdo[, 
            4])[1]), decreasing = F)
        axisIndex = unique(gdo[, 4][index])
        plot(plotIndex, gdo[, 5][index], ylim = c(range(gdo[, 
            5])[1], range(gdo[, 5])[2] + diff(range(gdo[, 5])) * 
            0.3), col = colVec, axes = F, xlab = xlab2, ylab = ylab2, 
            main = main2, ...)
        box()
        axis(2, ...)
        axis(1, at = 1:length(unique(plotIndex)), labels = axisIndex, 
            ...)
        legend("topright", as.character(unique(gdo[, 3][index])), 
            title = aName, col = col[1:length(unique(gdo[, 3][index]))], 
            pch = 19)
        meanVec = numeric(length(axisIndex))
        for (i in 1:length(axisIndex)) {
            meanVec[i] = mean(gdo[, 5][gdo[, 4] == axisIndex[i]])
        }
        lines(1:length(meanVec), meanVec, col = "blue", lwd = lwd)
        points(1:length(meanVec), meanVec, col = "blue", pch = 19, 
            ...)
        main3 = NA
        if (missing(main) || is.na(main[3])) 
            main3 = paste(yName, "By", aName)
        else main3 = main[3]
        xlab3 = NA
        if (missing(xlab) || is.na(xlab[3])) 
            xlab3 = aName
        else xlab3 = xlab[3]
        ylab3 = NA
        if (missing(ylab) || is.na(ylab[3])) 
            ylab3 = yName
        else ylab3 = ylab[3]
        plot(as.numeric(gdo[, 3][index]), gdo[, 5][index], axes = F, 
            ylim = c(range(gdo[, 5])[1], range(gdo[, 5])[2] + 
                diff(range(gdo[, 5])) * 0.3), col = colVec, xlab = xlab3, 
            ylab = ylab3, main = main3, ...)
        axis(1, at = unique(as.numeric(gdo[, 3][index])), labels = unique(gdo[, 
            3][index]), ...)
        axis(2, ...)
        box()
        mByOp = split(gdo[, 5], as.numeric(gdo[, 3]))
        lines(sort(as.numeric(factor(names(mByOp)))), lapply(mByOp, 
            mean)[sort(names(mByOp))], lwd = lwd)
    }
}) 
