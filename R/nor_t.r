normalPlot = function(fdo, threeWay = FALSE, na.last = NA, alpha = 0.05, sig.col = c("red1", "red2", "red3"), 
    main, ylim, xlim, xlab, ylab, pch, ...) {
    DB = FALSE
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
    parList = list(...)
    sig.col = character()
    leg.col = character()
    leg.txt = "p > 0.1"
    if (!(class(fdo) == "facDesign")) 
        stop(paste(deparse(substitute(fdo)), "is not an object of class facDesign"))
    if (missing(main)) 
        main = paste("Half-Normal plot for effects of", deparse(substitute(fdo)))
    if (missing(xlab)) 
        xlab = "Effects"
    if (missing(ylab)) 
        ylab = "Theoretical Quantiles"
    if (missing(pch)) 
        pch = 19
    effect.list = vector(mode = "list", length = 0)
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
        coefs = coef(lm.1)[-pmatch("(Intercept)", names(coef(lm.1)))]
        effect = 2 * coefs
        if (all(is.na(effect))) 
            stop("effects could not be calculated")
        sig = summary(lm.1)$coefficients[, "Pr(>|t|)"][-pmatch("(Intercept)", names(coef(lm.1)))]
        effect.list[[j]] = effect
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
        sig.col = rep("black", length = length(effect))
        if (!(is.null(parList$col[1]) || is.na(parList$col[1]))) {
            if (is.numeric(parList$col[1])) 
                sig.col = rep(parList$col[1], length = length(effect))
            if (parList$col[1] %in% colours()) 
                sig.col = rep(parList$col[1], length = length(effect))
        }
        leg.col = sig.col[1]
        if (df.resid > 0) {
            p001 = TRUE
            p005 = TRUE
            p010 = TRUE
            for (k in seq(along = sig)) {
                if (abs(sig)[k] <= 0.01) {
                  sig.col[k] = "red1"
                  if (p001) {
                    leg.txt = c(leg.txt, "p < 0.01")
                    leg.col = c(leg.col, "red1")
                    p001 = FALSE
                  }
                  break
                }
                if (abs(sig)[k] <= 0.05) {
                  sig.col[k] = "red2"
                  if (p005) {
                    leg.txt = c(leg.txt, "p < 0.05")
                    leg.col = c(leg.col, "red2")
                    p005 = FALSE
                  }
                  break
                }
                if (abs(sig)[k] <= 0.1) {
                  sig.col[k] = "red3"
                  if (p010) {
                    leg.txt = c(leg.txt, "p < 0.05")
                    leg.col = c(leg.col, "red3")
                    p010 = FALSE
                  }
                  break
                }
            }
        }
        mid = round(length(tQ)/2)
        last = length(tQ)
        if (missing(xlim)) 
            xlim = range(sQ)
        if (missing(ylim)) 
            ylim = c(0, 1)
        params = .lfkp(parList, c(formals(plot.default), par()))
        params$x = sQ
        params$y = tQ
        params$xlab = xlab
        params$ylab = ylab
        params$main = main
        params$xlim = xlim
        params$ylim = ylim
        params$pch = pch
        params$col = sig.col
        do.call(plot, params)
        text(sQ[1:mid], tQ[1:mid], names(sQ)[1:mid], pos = 4)
        text(sQ[(mid + 1):last], tQ[(mid + 1):last], names(sQ)[(mid + 1):last], pos = 2)
        if (df.resid > 0) 
            legend("topleft", legend = leg.txt, col = leg.col, pch = pch, inset = 0.02)
        xq = quantile(effect, prob = c(0.25, 0.75))
        yq = c(0.25, 0.75)
    }
    if (DB) {
        print(paste("xq:", xq[1], xq[2]))
        print(paste("yq:", yq[1], yq[2]))
    }
    slope <- diff(yq)/diff(xq)
    int <- yq[1] - slope * xq[1]
    if (!is.infinite(slope)) {
        params = .lfkp(parList, c(formals(abline), list(lwd = 1, col = 1)))
        params$a = int
        params$b = slope
        if (!(is.null(params$col[2]) || is.na(params$col[2]))) 
            params$col = params$col[2]
        do.call(abline, params)
    }
} 
