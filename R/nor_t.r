normalPlot = function(fdo, threeWay = FALSE, na.last = NA, alpha = 0.05, sig.col = c("red1", "red2", "red3"), 
    main, ylim, xlim, xlab, ylab, pch, col, border = "red", ...) {
    DB = FALSE
    require(MASS, quietly = TRUE)
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
    parList = list(...)
    params = list()
    if (length(sig.col) < 3) 
        sig.col = as.vector(matrix(sig.col, nrow = 1, ncol = 3))
    leg.col = vector()
    p.col = vector()
    leg.txt = vector()
    if (!(class(fdo) == "facDesign")) 
        stop(paste(deparse(substitute(fdo)), "is not an object of class facDesign"))
    if (missing(main)) 
        main = paste("Normal plot of", deparse(substitute(fdo)))
    if (missing(xlab)) 
        xlab = "Coefficients"
    if (missing(ylab)) 
        ylab = "Theoretical Quantiles"
    if (missing(pch)) 
        pch = 19
    if (missing(col)) 
        col = "black"
    for (j in 1:ncol(response(fdo))) {
        if (j > 1) 
            dev.new()
        form = paste("response(fdo)[,", j, "]~")
        for (i in 1:ncol(cube(fdo))) {
            form = paste(form, names(cube(fdo))[i], sep = "")
            if (i < ncol(cube(fdo))) 
                form = paste(form, "*", sep = "")
        }
        if (DB == TRUE) 
            print(paste("form:", form))
        lm.1 = lm(as.formula(form), data = as.data.frame(fdo))
        lm.1s = summary(lm.1)
        effect = coef(lm.1s)[row.names(coef(lm.1s)) != "(Intercept)", "t value"]
        if (all(is.na(effect))) 
            stop("effects could not be calculated")
        sig = summary(lm.1)$coefficients[, "Pr(>|t|)"][-pmatch("(Intercept)", names(coef(lm.1)))]
        df.resid = df.residual(lm.1)
        nc = nrow(centerCube(fdo))
        if (DB) {
            print(paste("effect:", effect))
            print(paste("df.resid:", df.resid))
            print(paste("nc:", nc))
            print(paste("sig:", sig))
            print(summary(lm.1))
        }
        tQ = ppoints(effect)
        index = order(effect)
        sQ = effect[index]
        sig = sig[index]
        if (df.resid > 0) {
            for (k in seq(along = sig)) {
                setted = FALSE
                if (abs(sig)[k] < 0.01) {
                  if (!setted) {
                    p.col[k] = sig.col[1]
                    leg.txt = c(leg.txt, "p < 0.01")
                    leg.col = c(leg.col, p.col)
                    setted = TRUE
                  }
                }
                if (abs(sig)[k] < 0.05) {
                  if (!setted) {
                    p.col[k] = sig.col[2]
                    leg.txt = c(leg.txt, "p < 0.05")
                    leg.col = c(leg.col, p.col)
                    setted = TRUE
                  }
                }
                if (abs(sig)[k] < 0.1) {
                  if (!setted) {
                    p.col[k] = sig.col[3]
                    leg.txt = c(leg.txt, "p < 0.1")
                    leg.col = c(leg.col, p.col)
                    setted = TRUE
                  }
                }
                if (abs(sig)[k] >= 0.1) {
                  if (!setted) {
                    p.col[k] = col
                    leg.txt = c(leg.txt, "p >= 0.1")
                    leg.col = c(leg.col, p.col)
                    setted = TRUE
                  }
                }
            }
            leg.txt = unique(leg.txt)
            leg.col = unique(leg.col)
        }
        mid = round(length(tQ)/2)
        last = length(tQ)
        params$p = ppoints(effect)
        estimates = fitdistr(effect, "normal")
        params$mean = estimates$estimate[["mean"]]
        params$sd = estimates$estimate[["sd"]]
        y = do.call(qnorm, params)
        if (missing(xlim)) 
            xlim = range(sQ)
        if (missing(ylim)) 
            ylim = range(y)
        params = .lfkp(parList, c(formals(plot.default), par()))
        params$x = sQ
        params$y = y
        params$xlab = xlab
        params$ylab = ylab
        params$main = main
        params$xlim = xlim
        params$ylim = ylim
        params$pch = pch
        params$col = p.col
        do.call(plot, params)
        xp = c(qnorm(0.1), qnorm(0.99))
        yp = c(qnorm(0.1, mean = estimates$estimate[["mean"]], sd = estimates$estimate[["sd"]]), qnorm(0.99, mean = estimates$estimate[["mean"]], 
            sd = estimates$estimate[["sd"]]))
        slope = (yp[2] - yp[1])/(xp[2] - xp[1])
        int = yp[1] - slope * xp[1]
        abline(a = int, b = slope, col = border)
        text(sQ[1:mid], y[1:mid], names(sQ)[1:mid], pos = 4)
        text(sQ[(mid + 1):last], y[(mid + 1):last], names(sQ)[(mid + 1):last], pos = 2)
        if (df.resid > 0) 
            legend("topleft", legend = leg.txt, col = leg.col, pch = pch, inset = 0.02)
    }
} 
