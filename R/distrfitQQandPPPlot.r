#######!!!!!!!!!!!Makes use of MASS package fitdistr
require(MASS)
#x - dataset used
#name - name of the fitted distribution
#parameter - a named vector of paramaters for the fitted distribution
#sd - the estimated standard errors; see fitdistr in library(MASS)
setClass("distr", representation(x = "vector", name = "character", parameters = "numeric", sd = "numeric", n = "numeric", loglik = "numeric"))



#distr - a list of objects of type fitdistr
setClass("distrCollection", representation(distr = "list"))

#generische accessor Methoden 
setMethod("[",  signature(x = "distrCollection", i = "ANY"),
                function(x,i, drop = missing)
                {
                  x@distr[i]
                }
          )
#TODO:
#names-function for distrCollection
#param-function for distrCollection
          


#generic show method for distrCollection
setMethod("show", signature(object = "distrCollection"), function(object) {

  distrList = object@distr
  
  cat("\n")
  
  for(i in seq(along = distrList))
  {
    temp = distrList[[i]]
    cat("\n")
    cat("fitted distribution is", temp@name,":\n")
    print(temp@parameters)
    cat("\n")
  }
}
)

#generic summary method for distrCollection
setMethod("summary", signature(object = "distrCollection"), function(object)
{
  numDist = length(object@distr)
  gofMatrix = data.frame(matrix(nrow = numDist, ncol = 3))
  names(gofMatrix) = c("Distribution", "A", "p.value")


  cat("\n------ Fitted Distribution and estimated parameters ------\n")

  for(i in seq(along = object@distr))
  {
    distrObj = object@distr[[i]]
    x = distrObj@x
    distribution = distrObj@name
    parameters = distrObj@parameters
    
    statistic = NA
    p.value = NA
   
    
    temp = .myADTest(x, distribution)
    try(statistic <- as.numeric(temp$statistic), silent = TRUE)
    try(p.value <- as.numeric(temp$p.value), silent = TRUE)
    
    gofMatrix[i,]  = c(distribution, as.numeric(statistic), as.numeric(p.value)) 
    
    cat("\n")
    cat("fitted distribution is", distribution,":\n")
    print(parameters)    
  }
  
  cat("\n")
  cat("\n------ Goodness of Fit - Anderson Darling Test ------\n")
  cat("\n")
  gofMatrixPrint = gofMatrix
  gofMatrixPrint[,2] = signif(as.numeric(gofMatrixPrint[,2]),4)
  gofMatrixPrint[,3] = signif(as.numeric(gofMatrixPrint[,3]),4)
  print(gofMatrixPrint)
}
)






#Description: calculates the most likely parameters for each distribution given in which
#return: object of class distrCollection
#distribution - density function or known distribution
#DOC: for now uses fitdistr (library(MASS))
#TODO: incorporate the use of user-defined densities
distribution = function(x, distribution = "weibull", start, ...)
{
  if(!require(MASS))
    stop("Package MASS needs to be installed!")
    
  #lower case for a distribution specified as character
  if(is.character(distribution))
    distribution = tolower(distribution)  

  allDistr = c("beta", "cauchy", "chi-squared", "exponential", "f", "gamma", "geometric", "log-normal", "logistic", "negative binomial", "normal", "poisson", "t","weibull")

  #if the requested distribution is not supported default to fitting of the normal distribution
  if(distribution %in% allDistr) 
    distrVec = distribution
  else
    distrVec = c("normal")
  
  #all possible distributions
  if(identical(distribution, "all"))
    distrVec = allDistr
  
  #most relevant distributions for quality tasks :-)
  if(identical(distribution, "quality"))
    distrVec = c("normal", "log-normal", "exponential", "weibull")
  

  distrColl = new("distrCollection")  
  for(i in seq(along = distrVec))
  {
    fit = new("distr")

    #for now usage of fitdistr function in MASS
    #copy the fitdistr fields into an object of distr class
    temp = fitdistr(x, distrVec[i],...)
    fit@x = x
    fit@name = distrVec[i]
    fit@parameters = temp$estimate
    fit@sd = temp$sd
    fit@loglik = temp$loglik
    
    distrColl@distr[i] = fit
  }
  return(distrColl)
}



#generic plot method for a object of class distr
#setGeneric("plot", function(x, y,  main, xlab, xlim, ylim, ylab, line.col, line.width, box = TRUE,...) standardGeneric("plot"))
##setGeneric("plot")
setGeneric("plot", function(x,y, ...) standardGeneric("plot"))
setMethod("plot", signature(x = "distr"), function(x, y, main, xlab, xlim, ylim, ylab, line.col, line.width, box = TRUE, ...)
{   



    object = x
    xVals = object@x
    parameters = object@parameters
    
    lq = NULL
    uq = NULL
    y = NULL
    
    if(missing(line.col))
      line.col = "red"
    
    if(missing(line.width))
      line.width = 1
    
    if(missing(main))
      main = object@name
    
    if(missing(xlab))
      xlab = "x"
    
    if(missing(ylab))
      ylab = "Density"

    
    distr = object@name
    qFun = .charToDistFunc(distr, type = "q")
    dFun = .charToDistFunc(distr, type = "d")


    #Anderson Darling test statistics for the legend
    adTestStats = .myADTest(xVals, distr)
    print(adTestStats)
#    print(attributes(adTestStats))

    if(class(adTestStats) == "adtest")
    { 
      A = adTestStats$statistic
      p = adTestStats$p.value
    }
    else
    {
      A = NA
      p = NA
    }

    histObj = hist(xVals, plot = FALSE)  

    #Grenzen des plots sind durch oberes und unteres Quantil gegeben
    if(missing(xlim))
    {
      lq = do.call(qFun, c(list(0.0001), as.list(parameters)))
      uq = do.call(qFun, c(list(0.9999), as.list(parameters)))
      xlim = range(lq, uq, xVals)
    }

    #Calculate values of the corresponding density distribution
    xPoints = seq(xlim[1], xlim[2], length = 200)
    yPoints = do.call(dFun, c(list(xPoints), as.list(parameters)))
    
    if(missing(ylim))
    {
      ylim = range(0, histObj$density, yPoints)
    }
    
    hist(xVals, freq = FALSE, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, main = main,...)
    lines(xPoints, yPoints, col = line.col, lwd = line.width) 
    abline(h = 0)

    legend("topright", c(paste(c(names(parameters), "A", "p") , ": ", c(format(parameters, digits = 3),format(A,digits = 3),format(p, digits = 3))), sep = " "), inset = 0.02)
    
    if(box)
    {
      box()
    }
}
)


#test1 = distribution(1:10, "quality")
#test2 = distribution(rlnorm(100), "log-normal")
#singleDist1 = test1@distr[[2]]
#singleDist2 = test1@distr[[1]]
#plot(singleDist1)
#plot(singleDist2)



#calculate  xlim and ylim for a distrCollection
#.xylimits is used in the generic plot function for a distrCollection object
#not to be seen outside
.xylimits = function(distrCollection, lowerquantile = 0.001, upperquantile = 0.999)
{
 
  
  x = NULL  #holds xlim values
  y = NULL  #holds ylim values

  for(i in seq(along = distrCollection@distr))
  {
    
    object = distrCollection@distr[[i]]
    xValues = object@x
    parameters = object@parameters
    distr = object@name
    qFun = .charToDistFunc(distr, type = "q")
    dFun = .charToDistFunc(distr, type = "d")
    
    lq = do.call(qFun, c(list(lowerquantile), as.list(parameters)))
    uq = do.call(qFun, c(list(upperquantile), as.list(parameters)))
    x = range(xValues, x, lq, uq)
    
    
    histObj = hist(xValues, plot = FALSE)  
    xPoints = seq(x[1], x[2], length = 200)
    yPoints = do.call(dFun, c(list(xPoints), as.list(parameters)))
    y = range(y, 0, histObj$density, yPoints)
    
    
  }

  invisible(list(xlim = x , ylim = y))
}



#generic plot method for a object of class distrCollection
#calls the geneeric plot method for each distr object in distrCollection
#main, cex.lab, cex.axis, cex.main can be passed via ... 
#setGeneric("plot", function(x, y, xlab, ylab, xlim, ylim, line.col, line.width, ...) standardGeneric("plot"))
setGeneric("plot", function(x,y, ...) standardGeneric("plot"))
setMethod("plot", signature(x = "distrCollection"), function(x, y, xlab, ylab, xlim, ylim, line.col, line.width, ...)
{
  y = NULL
  object = x
  distrList = object@distr
  
  numDist = length(object@distr)
  numColWin = ceiling(numDist/2)
  
  if(missing(xlim))
    xlim = .xylimits(object)$xlim
  
  if(missing(ylim))
    ylim = .xylimits(object)$ylim
  
  if(missing(line.col))
    line.col = "red"
  
  if(missing(line.width))
    line.width = 1
    
  lapply(distrList, plot, xlim = xlim, ylim = ylim, line.col = line.col, line.width = line.width, ...)
  

  #Comment on how to 
  cat(paste("Total of", numDist, "plots created"))
  cat("\n")
  cat(paste("Use par(mfrow = c(2,",numColWin,") to see all of them!", sep = ""))
  cat("\n")
}
)

#test1 = distribution(rnorm(20, mean = 20, sd = 2), "quality")
#plot(test1)
#par(mfrow = c(2,2))
#plot(test1, line.col = "blue", xlab = "test", col = 2, cex.lab = 2, xlim = c(10,30))








#QQ-Plot für eine beliebige Verteilung
#TODO: calculate parameters using fitdistr if ... is empty and parameters are needed
#TODO: rename qqplot.quality into qqplot
#x - vector or a object of class distrCollection
#y - the name of the distribution (character)
#return: x-quantiles and y-quantiles
qqPlot = function(x, y, main, xlab, ylab, start, ...)
{
  DB = FALSE
  parList = list(...)
  
  if(is.null(parList[["col"]]))
    parList$col = 1:2
  
  if(is.null(parList[["pch"]]))
    parList$pch = 19

  if(is.null(parList[["lwd"]]))
    parList$lwd = 1

  if(is.null(parList[["cex"]]))
    parList$cex = 1
  

  if(!require(MASS))
    stop("Package MASS needs to be installed!")
    
  if(class(x) == "distrCollection")    
  {
    distList = x@distr
    for(i in 1:length(distList))
    {
      d = distList[[i]]
      do.call(qqPlot, c(list(x = d@x, y = d@name), parList))
    }
    invisible() 
  }

  if(missing(y))
    y = "normal"

  if(missing(main))
    main = paste("Q-Q Plot for", deparse(substitute(y)), "distribution")
  
  if(missing(ylab))
    ylab = paste("Quantiles for", deparse(substitute(x)))
  
  if(missing(xlab))
    xlab = paste("Quantiles from", deparse(substitute(y)),"distribution")
  
  if(is.numeric(y))
  {
   cat("\ncalling (original) qqplot from namespace stats!\n")
   return(stats::qqplot(x,y,...) )
  }

  qFun = NULL
  theoretical.quantiles = NULL
  
  xs = sort(x)
  distribution = tolower(y)

  distWhichNeedParameters = c("weibull", "logistic", "gamma", "exponential", "f", "geometric", "chi-squared", "negative binomial", "poisson")  
   
  if(is.character(distribution))
  {
   qFun = .charToDistFunc(distribution, type = "q")

   if(is.null(qFun))     
     stop(paste(deparse(substitute(y)), "distribution could not be found!"))
  }

  theoretical.probs = ppoints(xs)
  xq = NULL
  yq = quantile(xs,prob=c(0.25,0.75)) #Stichprobe

  
  dots <- list(...)
#  if (y %in% distWhichNeedParameters)
  if(TRUE)
  {
    if(DB)
      print("TODO: Pass the estimated parameters correctly")
    #falls ein Parameter mit übergeben wird z.B. df für die t-Verteilung
    fitList = .lfkp(parList, formals(qFun))
    fitList$x = xs; fitList$densfun = distribution
    if(!missing(start))
      fitList$start = start
      
    if(DB)
    {
      print(fitList)
      print("Ende")  
    }
    
    fittedDistr = do.call(fitdistr, fitList)
    
#    fittedDistr = fitdistr(xs, distribution)
    #nur die parameter behalten die qFun nehmen kann
    parameter = fittedDistr$estimate
    parameter = .lfkp(as.list(parameter), formals(qFun))
    
    #nur die parameter behalten die qFun nehmen kann
    params = .lfkp(parList, formals(qFun))
    
    #falls eigene Parameter spezifiziert wurden diese und nicht die Schaetzungen nehmen    
    parameter = .lfrm(as.list(parameter), params)
    parameter = c(parameter, params)

    theoretical.quantiles = do.call(qFun, c(list(c(theoretical.probs)), parameter))
    xq <- do.call(qFun, c(list(c(0.25, 0.75)), parameter))
    
    if(DB)
    {
      print(paste("parameter: ", parameter))
      print(xq)
    }

  }
  else
  {
    #nur die parameter behalten die qFun nehmen kann
    params = .lfkp(parList, formals(qFun))
    params$p = theoretical.probs
    theoretical.quantiles = do.call(qFun, params)
    params$p = c(0.25, 0.75)
    xq = do.call(qFun, params)
  }

  params = .lfkp(parList, c(formals(plot.default),par()))
  params$x = theoretical.quantiles; params$y = xs; params$xlab = xlab; params$ylab = ylab; params$main = main;
  if(!(is.null(params$col[1]) || is.na(params$col[1])))
      params$col = params$col[1]
      
  do.call(plot, params)
                                     
  if(DB)
  {
    print(paste("xq:", xq[1], xq[2]))
    print(paste("yq:", yq[1], yq[2]))
  }
  
  slope <- diff(yq)/diff(xq)            #Anstieg
  int <- xq[1] - slope*yq[1]            #Achsenabschnitt

  #calculate the slope of a qqline
  slope <- diff(yq)/diff(xq)
        int <- yq[1L] - slope * xq[1L]

  if(!is.infinite(slope))
  {
    params = .lfkp(parList, c(formals(abline), list(lwd = 1, col = 1)))
    params$a = int
    params$b = slope
    #if there's a second color specified take this second color for abline
    if(!(is.null(params$col[2]) || is.na(params$col[2])))
      params$col = params$col[2]
    
    do.call(abline, params) #Linie durch Anstieg und Achsenabschnitt
  }
  
  invisible(list(x = theoretical.quantiles, y = xs))
}

#qqPlot(1:10, "normal", col = c("red", "blue"), main = "test", cex  = 2, pch = 4, axes = TRUE, cex.lab = 3, xlab = "test")
#qqPlot(1:10, "exponential", col = "red", main = "test", cex  = 2, pch = 4, axes = TRUE, cex.lab = 3, xlab = "test")
#qqPlot(1:10, "log-normal")
#qqPlot(1:10, "logistic")
#qqPlot(1:10, "poisson")
#qqPlot(1:10, "weibull", DB = FALSE)
#qqPlot(1:10, 1:10)




############################################################           TODO           ####################################################################
##Draws a probability plot for a given distribution
##Should give the same conclusions as a QQ-Plot. Might be rather expected than a QQ-Plot
#TODO: check for negative values of x, otherwise FAILURE for distributions that do not have negative input values
#probs - which percentages should be shown on the y-axis
#grid - is a (probability)grid to be drawn
#TODO: stats - show stats i.e. parameters and distribution and ad.test statistics
#TODO: check ... to pass parameters of a self defined distribution function
ppPlot = function(x, distribution, probs, main, xlab, ylab, xlim, ylim, grid = TRUE, box = TRUE, stats = TRUE, start, ...)
{
  DB = TRUE

  if(!require(MASS))
    stop("Package MASS needs to be installed!")
    
  if(!(is.numeric(x) | (class(x) == "distrCollection")))
    stop(paste(deparse(substitute(x))," needs to be numeric or an object of class distrCollection"))
    
  parList = list(...)
  if(is.null(parList[["col"]]))
    parList$col = c("black", "red", "gray")
  
  if(is.null(parList[["pch"]]))
    parList$pch = 19

  if(is.null(parList[["lwd"]]))
    parList$lwd = 1

  if(is.null(parList[["cex"]]))
    parList$cex = 1
  
  if(DB)
    print(parList)

  qFun = NULL
  xq = NULL
  yq = NULL
  x1 = NULL
  
  
  if(missing(probs))
    probs = ppoints(11)
  else
    if(min(probs) <= 0 || max(probs) >= 1)
      stop("probs should be values within (0,1)!")

  probs = round(probs, 2)


  if(is.numeric(x))
  {  
    x1 <- sort(na.omit(x))
    
    if(missing(xlim))
      xlim = c(min(x1) - 0.1*diff(range(x1)), max(x1) + 0.1*diff(range(x1)))
  }
  
  if(missing(distribution))
    distribution = "normal"

  if(missing(ylim))
    ylim = NULL
  
  if(missing(main))
    main = paste("Probability Plot for", deparse(substitute(distribution)), "distribution")
  
  if(missing(xlab))
    xlab = deparse(substitute(x))
  
  if(missing(ylab))
    ylab = "Probability"

  if(class(x) == "distrCollection")    
  {
    distList = x@distr
    for(i in 1:length(distList))
    {
      d = distList[[i]]
      do.call(ppPlot, c(list(x = d@x, distribution = d@name), parList))
    }
    invisible() 
  }
 
  distWhichNeedParameters = c("weibull", "gamma", "logistic", "exponential", "f", "geometric", "chi-squared", "negative binomial", "poisson")  
   
  if(is.character(distribution))
  {
    qFun = .charToDistFunc(distribution, type = "q")
    pFun = .charToDistFunc(distribution, type = "p")

   if(is.null(qFun))     
     stop(paste(deparse(substitute(y)), "distribution could not be found!"))
  }
  
  dots <- list(...)
#  if (length(dots) == 0 && (distribution %in% distWhichNeedParameters))
#   TODO: obsolete?
#  if(distribution %in% distWhichNeedParameters)
  if(TRUE)
  {
    print("TODO: Pass the estimated parameters correctly")

# ##############################################
#
#    fitList = .lfkp(parList, formals(qFun))
##    fitList = vector(mode = "list", length = 0)
#    fitList$x = xs; fitList$densfun = distribution
#    if(!missing(start))
#      fitList$start = start
#      
#    if(DB)
#    {
#      print(fitList)
#      print("Ende")  
#    }
#    
#    fittedDistr = do.call(fitdistr, fitList)
#    
##    fittedDistr = fitdistr(xs, distribution)
#    #nur die parameter behalten die qFun nehmen kann
#    parameter = fittedDistr$estimate
#    parameter = .lfkp(as.list(parameter), formals(qFun))
#    print(parameter)
#    
#    #nur die parameter behalten die qFun nehmen kann
#    params = .lfkp(parList, formals(qFun))
#    print(params)
#    
#    #falls eigene Parameter spezifiziert wurden diese und nicht die Schaetzungen nehmen    
#    parameter = .lfrm(as.list(parameter), params)
#    print(parameter)
#
#  #################################################
#



    fitList = .lfkp(parList, formals(qFun))
    fitList$x = x1; fitList$densfun = distribution
    if(!missing(start))
      fitList$start = start
    
    fittedDistr = do.call(fitdistr, fitList)

    #    fittedDistr = fitdistr(xs, distribution)
    #nur die parameter behalten die qFun nehmen kann
    parameter = fittedDistr$estimate
    parameter = .lfkp(as.list(parameter), formals(qFun))
    
    #nur die parameter behalten die qFun nehmen kann
    params = .lfkp(parList, formals(qFun))
    print(params)
    
    #falls eigene Parameter spezifiziert wurden diese und nicht die Schaetzungen nehmen    
    parameter = .lfrm(as.list(parameter), params)
    print(parameter)
    
    parameter = c(parameter, params)
    print(parameter)


    if(DB)
    {
      print(qFun)
      print(as.list(parameter))
      print(list(probs))
    }
    
    y = do.call(qFun, c(list(ppoints(x1)), as.list(parameter)))
    print(range(y))
    axisAtY = do.call(qFun, c(list(probs), as.list(parameter)))  
    
    yq = do.call(qFun, c(list(c(0.25, 0.75)), as.list(parameter)))             #0.25 and 0.75 Quantiles for qqline
    xq = quantile(x1, probs = c(0.25, 0.75))
    
#    print("XQ")
#    print(xq)
#    print("YQ")
#    print(yq)
#

    if(DB)
    {
      print(paste("parameter: ", parameter))
      print(xq)
    }

  }
  else
  {
    #nur die parameter behalten die qFun nehmen kann
    params = .lfkp(parList, formals(qFun))
    
    params$p = ppoints(x1)
    y = do.call(qFun, params)
    
    params$p = probs
    axisAtY = do.call(qFun, params)

    params$p = c(0.25, 0.75)
    yq = do.call(qFun, params)
    xq = quantile(x1, probs = c(0.25, 0.75))             #0.25 and 0.75 Quantiles for qqline
    
  }
    
  #Plotting the given x and the y of the fitted distribution labeled as qFun(x) (see above)
  params = .lfkp(parList, c(formals(plot.default),par()))
  params$x = x1; params$y = y; params$xlab = xlab; params$ylab = ylab; params$main = main;
  params$axes = FALSE
  params$lwd = 1
  if(!(is.null(params$col[1]) || is.na(params$col[1])))
      params$col = params$col[1]
  
      
  do.call(plot, params)
  
  pParams = params  #for points function later
  
  params = .lfkp(parList, list(cex.main = 1, cex.axis = 1, cex.lab = 1))
  params$side = 1
  axisAtX = do.call(axis, params)
  params$side = 2; params$at = axisAtY; params$labels = probs; params$las = 2;
  do.call(axis, params)

  if(grid)
  {
    params = .lfkp(parList, c(formals(abline), list(lwd = 1, col = 1)))
    params$h = axisAtY
    params$v = axisAtX

    #if there's a third color specified take this third color for abline
    if(!(is.null(params$col[3]) || is.na(params$col[3])))
      params$col = params$col[3]
    else
      params$col = 1

    if(!(is.null(params$lwd[2]) || is.na(params$lwd[2])))
      params$lwd = params$lwd[2]
    else
      params$lwd = 1

    do.call(abline, params) #Linie durch Anstieg und Achsenabschnitt
  }
  
  pParams = .lfkp(pParams, list(x = 1, y = 1, col = 1, cex = 1))
  do.call(points, pParams)
  
  

  #calculate the slope of a qqline y = m*x + n; m = deltaY/deltaX
  slope <- diff(yq)/diff(xq)
  int <- yq[1] - slope * xq[1]


  if(!is.infinite(slope))  
  {
    params = .lfkp(parList, c(formals(abline), list(lwd = 1, col = 1)))
    params$a = int
    params$b = slope
    #if there's a second color specified take this second color for abline
    if(!(is.null(params$col[2]) || is.na(params$col[2])))
      params$col = params$col[2]
    
    do.call(abline, params) #Linie durch Anstieg und Achsenabschnitt
  }

  if(box)
    box()
  
  invisible(list(x = x, y = y, int = int, slope = slope))
}
#ppPlot(rnorm(50), "normal", col = c(1,2,3))
#
#ppPlot(rnorm(50, 30), "weibull", col = c("red", "blue"))
#
#example 2-13 von Statistical quality Control p. 74 Montgomery
#octane = c(86, 87, 87.2, 87.4, 87.8, 88.2, 88.9, 89.6, 89.7, 90)
#ppPlot(octane, distribution = "weibull",col = c("black", "red", "gray"), pch = 19, lwd = 2)
#ppPlot(octane, distribution = "exponential",col = c("black", "red", "gray"), pch = 19, lwd = 2)
#ppPlot(octane, distribution = "cauchy",col = c("black", "red", "gray"), pch = 19, lwd = 2)
#ppPlot(octane, distribution = "logistic",col = c("black", "red", "gray"), pch = 19, lwd = 2)
#ppPlot(octane, distribution = "log-normal",col = c("black", "red", "gray"), pch = 19, lwd = 2)
#ppPlot(octane, distribution = "normal",col = c("black", "red", "gray"), pch = 19, lwd = 2)
#ppPlot(octane, "t", df = length(octane) - 1, col = c("black", "red", "gray"), pch = 19, lwd = 2)
#

#not really working yet - best way to specifiy start values
#ppPlot(octane, distribution = "beta",col = c("black", "red", "gray"), pch = 19, lwd = 2, start = list(shape1 = 1, shape2 = 0.1))
#ppPlot(octane, distribution = "gamma",col = c("black", "red", "gray"), pch = 19, lwd = 2, start = list(shape = 1, rate = 0.1))
#fitdistr(octane, "beta", start = list(shape1 = 2, shape2 = 1))
#ppPlot(octane, "f", start = list(df1 = 2, df2 = 40))


#ppPlot(octane, "weibull", probs = ppoints(11))
#ppPlot(octane, "exponential", probs = ppoints(11))
#
##dataset from  Journal of Quality Technology 1990 pp. 105-110, Aluminium Contaminitation in ppm
#contam = c(30, 30, 60, 63, 70, 79, 87, 90, 101, 102, 115, 118, 119, 119, 120, 125, 140, 145, 172, 182, 183, 191, 222, 244, 291, 511)
#par(mfrow = c(3,2))
#x = rweibull(20, 8, 2)
#ppPlot(x, "log-normal")
#ppPlot(x, "normal")
#ppPlot(x, "exponential", DB = TRUE)
#ppPlot(x, "cauchy")
#ppPlot(x, "weibull")
#ppPlot(x, "logistic")        
#

#ppPlot(octane, "log-normal")
#ppPlot(octane, "normal")
#ppPlot(octane, "exponential", DB = TRUE)
#ppPlot(octane, "cauchy")
#ppPlot(octane, "weibull")        
#qqPlot(octane, "t", df = 40)
#

