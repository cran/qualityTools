.c4 = function(n) {
    if (n > 1 && n < 342) 
        sqrt(2/(n - 1)) * (factorial(n/2 - 1)/factorial((n - 1)/2 - 1))
    else stop("n needs to be bigger than 1 and smaller than 342")
}
.sdSg = function(x, grouping = NULL, method = c("NOWEIGHT", "MVLUE", "RMSDF"), na.rm = TRUE, DB = TRUE) {
    if (!is.data.frame(x) && !is.vector(x) && is.numeric(x)) 
        stop("x needs to be either a data.frame or a vector and numeric")
    if (is.null(grouping)) 
        return(sd(x))
    else grouping = as.data.frame(grouping)
    group = unique(grouping)
    sdVec = numeric(length = length(group))
    for (i in 1:length(group)) {
        if (is.data.frame(x)) {
            temp = x[group[i] == grouping[, 1], ]
            sdVec[i] = sd(temp, na.rm = T)/.c4(length(temp[!is.na(temp)]))
        }
        if (is.vector(x)) {
            temp = x[group[i] == grouping[, 1]]
            sdVec[i] = sd(temp, na.rm = T)/.c4(length(temp[!is.na(temp)]))
        }
    }
    if (DB == TRUE) 
        print(paste("std.dev: ", mean(sdVec)))
    return((mean(sdVec)))
}
pcr = function(x, distribution = "normal", lsl, usl, target, boxcox = FALSE, lambda, main, xlim, ylim, grouping = NULL, 
    std.dev = NULL, conf.level = 0.9973002, start, lineWidth = 1, lineCol = "red", lineType = "solid", specCol = "red3", 
    specWidth = 1, cex.text = 2, cex.val = 1.5, cex.col = "darkgray", ...) {
    DB = FALSE
    data.name = deparse(substitute(x))
    require(MASS, quietly = TRUE)
    par.orig <- par(c("mar", "oma", "mfrow"))
    on.exit(par(par.orig))
    parList = list(...)
    if (is.null(parList[["col"]])) 
        parList$col = "lightblue"
    if (is.null(parList[["border"]])) 
        parList$border = 1
    if (is.null(parList[["lwd"]])) 
        parList$lwd = 1
    if (is.null(parList[["cex.axis"]])) 
        parList$cex.axis = 1.5
    if (missing(lsl)) 
        lsl = NULL
    if (missing(usl)) 
        usl = NULL
    if (missing(target)) 
        target = NULL
    if (missing(lambda)) 
        lambda = c(-5, 5)
    if (!is.numeric(lambda)) 
        stop("lambda needs to be numeric")
    paramsList = vector(mode = "list", length = 0)
    estimates = vector(mode = "list", length = 0)
    varName = deparse(substitute(x))
    dFun = NULL
    pFun = NULL
    qFun = NULL
    cp = NULL
    cpu = NULL
    cpl = NULL
    cpk = NULL
    ppt = NULL
    ppl = NULL
    ppu = NULL
    xVec = numeric(0)
    yVec = numeric(0)
    if (is.vector(x)) 
        x = as.data.frame(x)
    if (boxcox) {
        distribution = "normal"
        if (length(lambda) >= 2) {
            lambda = c(min(lambda), max(lambda))
            temp = boxcox(x[, 1] ~ 1, lambda = lambda, plotit = FALSE)
            i = order(temp$y, decreasing = TRUE)[1]
            lambda = temp$x[i]
        }
        x = as.data.frame(x[, 1]^lambda)
    }
    numObs = nrow(x)
    if (!is.null(grouping)) 
        if (is.vector(grouping)) 
            grouping = as.data.frame(grouping)
    center = mean(x)
    if (!is.null(x) & !is.null(grouping)) {
        if (nrow(x) != nrow(grouping)) 
            stop(paste("length of ", deparse(substitute(grouping)), " differs from length of ", varName))
    }
    if (missing(main)) 
        if (boxcox) 
            main = paste("Process Capability using box cox transformation for", varName)
        else main = paste("Process Capability using", as.character(distribution), "distribution for", varName)
    if (is.null(std.dev)) {
        if (is.null(grouping)) 
            std.dev = .sdSg(x)
        else std.dev = .sdSg(x, grouping)
    }
    if (conf.level < 0 | conf.level > 1) 
        stop("conf.level must be a value between 0 and 1")
    confHigh = conf.level + (1 - conf.level)/2
    confLow = 1 - conf.level - (1 - conf.level)/2
    if (DB) {
        print(paste("confHigh:", confHigh))
        print(paste("confLow:", confLow))
    }
    distWhichNeedParameters = c("weibull", "logistic", "gamma", "exponential", "f", "geometric", "chi-squared", "negative binomial", 
        "poisson")
    if (is.character(distribution)) {
        qFun = .charToDistFunc(distribution, type = "q")
        pFun = .charToDistFunc(distribution, type = "p")
        dFun = .charToDistFunc(distribution, type = "d")
        if (is.null(qFun) & is.null(pFun) & is.null(dFun)) 
            stop(paste(deparse(substitute(y)), "distribution could not be found!"))
    }
    if (TRUE) {
        if (DB) 
            print("TODO: Pass the estimated parameters correctly")
        fitList = vector(mode = "list", length = 0)
        fitList$x = x[, 1]
        fitList$densfun = distribution
        if (!missing(start)) 
            fitList$start = start
        fittedDistr = do.call(fitdistr, fitList)
        estimates = as.list(fittedDistr$estimate)
        paramsList = estimates
        if (DB) 
            print(paste("parameter: ", paramsList))
    }
    paramsList = c(paramsList, .lfkp(parList, formals(qFun)))
    if (distribution == "normal") {
        paramsList$sd = std.dev
    }
    if (boxcox) {
        if (!is.null(lsl)) 
            lsl = lsl^lambda
        if (!is.null(usl)) 
            usl = usl^lambda
        if (!is.null(target)) 
            target = target^lambda
    }
    if (is.null(lsl) && is.null(usl)) {
        if (distribution == "normal") {
            paramsList$mean = center
            paramsList$sd = std.dev
        }
        paramsList$p = confLow
        lsl = do.call(qFun, paramsList)
        paramsList$p = confHigh
        usl = do.call(qFun, paramsList)
    }
    if (identical(lsl, usl)) 
        stop("lsl == usl")
    if (!is.null(lsl) && !is.null(target) && target < lsl) 
        stop("target is less than lower specification limit")
    if (!is.null(usl) && !is.null(target) && target > usl) 
        stop("target is greater than upper specification limit")
    if (!is.null(lsl) && !is.null(usl)) 
        if (lsl > usl) {
            temp = lsl
            lsl = usl
            usl = temp
        }
    paramsList$p = c(confLow, 0.5, confHigh)
    qs = do.call(qFun, paramsList)
    paramsList = .lfkp(paramsList, formals(pFun))
    if (!is.null(lsl) && !is.null(usl)) 
        cp = (usl - lsl)/(qs[3] - qs[1])
    if (!is.null(usl)) {
        cpu = (usl - qs[2])/(qs[3] - qs[2])
        paramsList$q = usl
        ppu = 1 - do.call(pFun, paramsList)
    }
    if (!is.null(lsl)) {
        cpl = (qs[2] - lsl)/(qs[2] - qs[1])
        paramsList$q = lsl
        ppl = do.call(pFun, paramsList)
    }
    cpk = min(cpu, cpl)
    ppt = sum(ppl, ppu)
    if (DB == TRUE) {
        print(cp)
        print(cpk)
        print(cpu)
        print(cpl)
        print(ppu)
        print(ppl)
        print(ppt)
    }
    if (missing(xlim)) {
        xlim <- range(x[, 1], usl, lsl)
        xlim <- xlim + diff(xlim) * c(-0.2, 0.2)
    }
    xVec <- seq(min(xlim), max(xlim), length = 200)
    if (distribution == "normal") 
        yVec <- dnorm(xVec, center, std.dev)
    else {
        dParamsList = .lfkp(paramsList, formals(dFun))
        dParamsList$x = xVec
        yVec = do.call(dFun, dParamsList)
    }
    histObj <- hist(x[, 1], plot = FALSE)
    if (missing(ylim)) {
        ylim <- range(histObj$density, yVec)
        ylim <- ylim + diff(ylim) * c(0, 0.05)
    }
    par(mar = c(0, 0, 0, 0) + 0.1)
    par(oma = c(2, 4, 7, 4) + 0.1)
    layout(matrix(c(1, 1, 1, 2, 1, 1, 1, 3, 1, 1, 1, 4, 5, 5, 6, 7), nr = 4, byrow = TRUE))
    do.call(hist, c(list(x[, 1], freq = FALSE, xlim = xlim, ylim = ylim, main = ""), parList))
    abline(h = 0, col = "gray")
    tempList = parList
    tempList$col = 1
    tempList$border = NULL
    do.call(box, tempList)
    lines(xVec, yVec, lwd = lineWidth, col = lineCol, lty = lineType)
    abline(v = usl, col = specCol, lwd = specWidth, lty = 5)
    abline(v = lsl, col = specCol, lwd = specWidth, lty = 5)
    abline(v = target, col = specCol, lwd = specWidth, lty = 5)
    if (!is.null(lsl)) 
        axis(side = 3, at = lsl, labels = paste("LSL =", format(lsl, digits = 3)), col = specCol)
    if (!is.null(usl)) 
        axis(side = 3, at = usl, labels = paste("USL =", format(usl, digits = 3)), col = specCol)
    if (!is.null(lsl) && !is.null(usl)) 
        axis(side = 3, at = c(lsl, usl), labels = c(paste("LSL =", format(lsl, digits = 3)), paste("USL =", format(usl, 
            digits = 3))), col = specCol)
    if (!is.null(target)) 
        text(target, max(ylim), "TARGET", pos = 1, col = cex.col, cex = cex.text)
    title(main = main, outer = TRUE)
    plot(0:5, 0:5, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
    box()
    text(2.3, 1, expression(c[p]), pos = 2, cex = cex.val)
    if (is.null(cp)) 
        text(2, 1, paste("=", "*"), pos = 4, cex = cex.val)
    else text(2, 1, paste("=", round(cp, 2)), pos = 4, cex = cex.val)
    text(2.3, 2, expression(c[pk]), pos = 2, cex = cex.val)
    if (is.null(cpk)) 
        text(2, 2, paste("=", "*"), pos = 4, cex = cex.val)
    else text(2, 2, paste("=", round(cpk, 2)), pos = 4, cex = cex.val)
    text(2.3, 3, expression(c[pkL]), pos = 2, cex = cex.val)
    if (is.null(cpl)) 
        text(2, 3, paste("=", "*"), pos = 4, cex = cex.val)
    else text(2, 3, paste("=", round(cpl, 2)), pos = 4, cex = cex.val)
    text(2.3, 4, expression(c[pkU]), pos = 2, cex = cex.val)
    if (is.null(cpu)) 
        text(2, 4, paste("=", "*"), pos = 4, cex = cex.val)
    else text(2, 4, paste("=", round(cpu, 2)), pos = 4, cex = cex.val)
    index = 1:(length(estimates) + 3)
    plot(0:5, c(0:4, max(index) + 1), type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
    box()
    names(x) = data.name
    adTestStats = .myADTest(x, distribution)
    A = numeric()
    p = numeric()
    if (class(adTestStats) == "adtest") {
        A = adTestStats$statistic
        p = adTestStats$p.value
    }
    else {
        A = NA
        p = NA
    }
    text(2.3, rev(index)[2], "A", pos = 2, cex = cex.val)
    text(2, rev(index)[2], paste("=", format(A, digits = 3)), pos = 4, cex = cex.val)
    text(2.3, rev(index)[3], "p", pos = 2, cex = cex.val)
    text(2, rev(index)[3], paste("=", format(p, digits = 3)), pos = 4, cex = cex.val)
    text(2.3, rev(index)[1], "n", pos = 2, cex = cex.val)
    text(2, rev(index)[1], paste("=", numObs), pos = 4, cex = cex.val)
    j = 1
    for (i in 3:(3 + length(estimates) - 1)) {
        try(text(2.3, rev(index)[i + 1], names(estimates)[[j]], pos = 2, cex = cex.val), silent = TRUE)
        try(text(2, rev(index)[i + 1], paste("=", format(estimates[[j]], digits = 3)), pos = 4, cex = cex.val), silent = TRUE)
        j = j + 1
    }
    qqPlot(x[, 1], y = distribution, ylab = "", main = "", axes = F)
    axis(1)
    axis(4)
    box()
    par(mar = c(0, 0, 3, 2))
    plot(c(-1, 1), c(0.5, 5), type = "n", axes = FALSE)
    box()
    text(0, 4.5, "Expected Fraction Nonconforming", cex = cex.val)
    text(-1.05, 3, expression(p[t]), pos = 4, cex = cex.val)
    text(-1.05, 2, expression(p[L]), pos = 4, cex = cex.val)
    text(-1.05, 1, expression(p[U]), pos = 4, cex = cex.val)
    text(-0.9, 3, paste("=", format(ppt, digits = 6)), pos = 4, cex = cex.val)
    if (is.null(ppl)) 
        text(-0.9, 2, paste("= 0"), pos = 4, cex = cex.val)
    else text(-0.9, 2, paste("=", format(ppl, digits = 6)), pos = 4, cex = cex.val)
    if (is.null(ppu)) 
        text(-0.9, 1, paste("= 0"), pos = 4, cex = cex.val)
    else text(-0.9, 1, paste("=", format(ppu, digits = 6)), pos = 4, cex = cex.val)
    text(0.05, 3, expression(ppm), pos = 4, cex = cex.val)
    text(0.05, 2, expression(ppm), pos = 4, cex = cex.val)
    text(0.05, 1, expression(ppm), pos = 4, cex = cex.val)
    text(0.35, 3, paste("=", format(ppt * 1e+05, digits = 6)), pos = 4, cex = cex.val)
    if (is.null(ppl)) 
        text(0.35, 2, paste("= 0"), pos = 4, cex = cex.val)
    else text(0.35, 2, paste("=", format(ppl * 1e+05, digits = 6)), pos = 4, cex = cex.val)
    if (is.null(ppu)) 
        text(0.35, 1, paste("= 0"), pos = 4, cex = cex.val)
    else text(0.35, 1, paste("=", format(ppu * 1e+05, digits = 6)), pos = 4, cex = cex.val)
    par(mar = c(0, 0, 3, 0))
    plot(c(-1, 1), c(0.5, 5), type = "n", axes = FALSE)
    box()
    text(0, 4.5, "Observed", cex = cex.val)
    obsL = 0
    obsU = 0
    if (!is.null(lsl)) {
        obsL = (sum(x < lsl)/length(x)) * 1e+06
        text(-1, 2, paste("ppm =", obsL), pos = 4, cex = cex.val)
    }
    else text(-1, 2, paste("ppm =", 0), pos = 4, cex = cex.val)
    if (!is.null(usl)) {
        obsU = (sum(x > usl)/length(x)) * 1e+06
        text(-1, 1, paste("ppm =", obsU), pos = 4, cex = cex.val)
    }
    else text(-1, 1, paste("ppm =", 0), pos = 4, cex = cex.val)
    text(-1, 3, paste("ppm =", obsL + obsU), pos = 4, cex = cex.val)
    invisible(list(lambda = lambda, cp = cp, cpk = cpk, cpl = cpl, cpu = cpu, ppt = ppt, ppl = ppl, ppu = ppu, A = A, 
        usl = usl, lsl = lsl, target = target))
} 
