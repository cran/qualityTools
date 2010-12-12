.c4 = function(n) {
    if (n > 1 && n < 342) 
        sqrt(2/(n - 1)) * (factorial(n/2 - 1)/factorial((n - 
            1)/2 - 1))
    else stop("n needs to be bigger than 1 and smaller than 342")
}
.sdSg = function(x, grouping = NULL, method = c("NOWEIGHT", 
    "MVLUE", "RMSDF"), na.rm = TRUE, DB = TRUE) {
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
            print(sdVec[i])
            print(length(temp[!is.na(temp)]))
        }
        if (is.vector(x)) {
            temp = x[group[i] == grouping[, 1]]
            sdVec[i] = sd(temp, na.rm = T)/.c4(length(temp[!is.na(temp)]))
            print(sdVec[i])
            print(length(temp[!is.na(temp)]))
        }
    }
    if (DB == TRUE) 
        print(paste("std.dev: ", mean(sdVec)))
    return((mean(sdVec)))
}
.boxCoxTrans = function(x, lambda) {
    if (abs(lambda) <= 1e-06) 
        y = log(x, base = exp(1))
    else y = (x^lambda - 1)/(lambda * ((prod(x, na.rm = TRUE)^(1/length(na.omit(x))))^(lambda - 
        1)))
    return(y)
}
.bctSD = function(lambda, x) {
    if (abs(lambda) <= 1e-06) 
        y = log(x, base = exp(1))
    else y = (x^lambda - 1)/(lambda * ((prod(x, na.rm = TRUE)^(1/length(na.omit(x))))^(lambda - 
        1)))
    return(sd(y))
}
pcr = function(x, distribution = "normal", lsl, usl, 
    target, boxcox = FALSE, lambda, main, xlim, ylim, grouping = NULL, 
    std.dev = NULL, conf.level = 0.9973002, start, lineWidth = 2, 
    lineCol = "red", lineType = "solid", specCol = "black", specWidth = 2, 
    cex.text = 2, cex.val = 1.5, cex.col = "darkgray", ...) {
    DB = FALSE
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
    paramsList = vector(mode = "list", length = 0)
    estimates = vector(mode = "list", length = 0)
    varName = deparse(substitute(x))
    dFun = NULL
    pFun = NULL
    qFun = NULL
    xVec = numeric(0)
    yVec = numeric(0)
    if (is.vector(x)) 
        x = as.data.frame(x)
    if (boxcox) {
        if (missing(lambda)) 
            lambda = c(-5, 5)
        if (!is.numeric(lambda)) 
            stop("lambda needs to be numeric")
        if (length(lambda) >= 2) 
            lambda = c(min(lambda), max(lambda))
        if (length(lambda) >= 2) 
            opt = optimize(.bctSD, interval = lambda, x[, 1])
        x = as.data.frame(x[, 1]^lambda)
    }
    numObs = nrow(x)
    if (!is.null(grouping)) 
        if (is.vector(grouping)) 
            grouping = as.data.frame(grouping)
    center = mean(x)
    if (!is.null(x) & !is.null(grouping)) {
        if (nrow(x) != nrow(grouping)) 
            stop(paste("length of ", deparse(substitute(grouping)), 
                " differs from length of ", varName))
    }
    if (missing(main)) 
        main = paste("Process Capability using", as.character(distribution), 
            "distribution for\n", varName)
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
    distWhichNeedParameters = c("weibull", "logistic", "gamma", 
        "exponential", "f", "geometric", "chi-squared", "negative binomial", 
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
    if (missing(lsl) || !is.numeric(lsl) || lsl == usl) {
        warning("lower specification limit should be provided!")
        paramsList$p = confLow
        if (distribution == "normal") {
            paramsList$mean = center
            paramsList$sd = std.dev
        }
        lsl = do.call(qFun, paramsList)
    }
    if (missing(usl) || !is.numeric(usl) || lsl == usl) {
        warning("upper specification limit should be provided!")
        paramsList$p = confHigh
        if (distribution == "normal") {
            paramsList$mean = center
            paramsList$sd = std.dev
        }
        usl = do.call(qFun, paramsList)
    }
    if (missing(target)) 
        target = mean(c(usl, lsl))
    if (target < lsl | target > usl) 
        warning("target value is not within specification limits!")
    if (lsl > usl) {
        temp = lsl
        lsl = usl
        usl = temp
    }
    paramsList$p = c(confLow, 0.5, confHigh)
    qs = do.call(qFun, paramsList)
    cp = (usl - lsl)/(qs[3] - qs[1])
    cpu = (usl - qs[2])/(qs[3] - qs[2])
    cpl = (qs[2] - lsl)/(qs[2] - qs[1])
    cpk = min(cpu, cpl)
    if (DB == TRUE) {
        print(cp)
        print(cpk)
        print(cpu)
        print(cpl)
    }
    if (missing(xlim)) {
        xlim <- range(x[, 1], usl, lsl, target)
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
    par(oma = c(12, 4, 4, 5) + 0.1)
    layout(matrix(c(1, 1, 1, 2, 1, 1, 1, 3, 1, 1, 1, 4), nr = 3, 
        byrow = TRUE))
    do.call(hist, c(list(x[, 1], freq = FALSE, xlim = xlim, ylim = ylim, 
        main = ""), parList))
    abline(h = 0, col = "gray")
    tempList = parList
    tempList$col = 1
    tempList$border = NULL
    do.call(box, tempList)
    lines(xVec, yVec, lwd = lineWidth, col = lineCol, lty = lineType)
    abline(v = usl, col = specCol, lwd = specWidth)
    abline(v = lsl, col = specCol, lwd = specWidth)
    abline(v = target, col = specCol, lwd = specWidth)
    text(lsl, 0.9 * max(ylim), "LSL", pos = 2, col = cex.col, 
        cex = cex.text)
    text(usl, 0.9 * max(ylim), "USL", pos = 4, col = cex.col, 
        cex = cex.text)
    text(target, max(ylim), "TARGET", pos = 1, col = cex.col, 
        cex = cex.text)
    title(main = main, outer = TRUE)
    pos1 = 0.025
    pos2 = 0.6
    mtext(expression(bar(x)), at = pos1, side = 1, line = 5, 
        cex = 1.5, adj = 1, outer = TRUE)
    mtext(paste(" =", round(center, 3)), at = pos1, side = 1, 
        line = 5, cex = 1.5, adj = 0, outer = TRUE)
    mtext("s", at = pos1, side = 1, line = 7, cex = 1.5, adj = 1, 
        outer = TRUE)
    mtext(paste(" =", round(std.dev, 3)), at = pos1, side = 1, 
        line = 7, cex = 1.5, adj = 0, outer = TRUE)
    mtext("n", at = pos1, side = 1, line = 9, cex = 1.5, adj = 1, 
        outer = TRUE)
    mtext(paste(" =", numObs), at = pos1, side = 1, line = 9, 
        cex = 1.5, adj = 0, outer = TRUE)
    mtext("Nominal Value", at = pos2, side = 1, line = 5, cex = 1.5, 
        adj = 1, outer = TRUE)
    mtext(paste(" =", round(target, 3)), at = pos2, side = 1, 
        line = 5, cex = 1.5, adj = 0, outer = TRUE)
    mtext("USL", at = pos2, side = 1, line = 7, cex = 1.5, adj = 1, 
        outer = TRUE)
    mtext(paste(" =", round(usl, 3)), at = pos2, side = 1, line = 7, 
        cex = 1.5, adj = 0, outer = TRUE)
    mtext("LSL", at = pos2, side = 1, line = 9, cex = 1.5, adj = 1, 
        outer = TRUE)
    mtext(paste(" =", round(lsl, 3)), at = pos2, side = 1, line = 9, 
        cex = 1.5, adj = 0, outer = TRUE)
    plot(0:5, 0:5, type = "n", axes = FALSE, xlab = "", ylab = "", 
        main = "")
    box()
    text(2, 1, expression(c[p]), pos = 2, cex = cex.val)
    text(2, 1, paste("=", round(cp, 2)), pos = 4, cex = cex.val)
    text(2, 2, expression(c[pk]), pos = 2, cex = cex.val)
    text(2, 2, paste("=", round(cpk, 2)), pos = 4, cex = cex.val)
    text(2, 3, expression(c[pkL]), pos = 2, cex = cex.val)
    text(2, 3, paste("=", round(cpl, 2)), pos = 4, cex = cex.val)
    text(2, 4, expression(c[pkU]), pos = 2, cex = cex.val)
    text(2, 4, paste("=", round(cpu, 2)), pos = 4, cex = cex.val)
    index = 0:(length(estimates) + 3)
    plot(0:5, c(0:4, max(index)), type = "n", axes = FALSE, xlab = "", 
        ylab = "", main = "")
    box()
    adTestStats = .myADTest(x[, 1], distribution)
    print(adTestStats)
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
    text(2, rev(index)[2], "A", pos = 2, cex = cex.val)
    text(2, rev(index)[2], paste("=", round(A, 3)), pos = 4, 
        cex = cex.val)
    text(2, rev(index)[3], "p", pos = 2, cex = cex.val)
    text(2, rev(index)[3], paste("=", round(p, 3)), pos = 4, 
        cex = cex.val)
    j = 1
    for (i in 3:(3 + length(estimates) - 1)) {
        try(text(2, rev(index)[i + 1], names(estimates)[[j]], 
            pos = 2, cex = cex.val), silent = TRUE)
        try(text(2, rev(index)[i + 1], paste("=", round(estimates[[j]], 
            3)), pos = 4, cex = cex.val), silent = TRUE)
        j = j + 1
    }
    qqPlot(x[, 1], y = distribution, ylab = "", main = "", axes = F)
    axis(1)
    axis(4)
    box()
} 
