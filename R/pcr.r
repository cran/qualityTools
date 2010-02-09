#As seen on http://www.jmu.edu/docs/sasdoc/sashtml/qc/chap12/sect19.htm
.c4 = function(n)
{
  if(n > 1 && n < 342)
    sqrt(2/(n-1))*(factorial(n/2 - 1) / factorial((n-1)/2 - 1))
  else
    stop("n needs to be bigger than 1 and smaller than 342")
}

#.c4(c(2,3,4,5))

###define standard deviation for subGroups
#Definitions taken from http://www.jmu.edu/docs/sasdoc/sashtml/qc/chap12/sect19.htm
#only for numeric data so far...
#only for x being a numeric vector or a data.frame
.sdSg = function(x, grouping = NULL, method = c("NOWEIGHT", "MVLUE", "RMSDF"), na.rm = TRUE, DB = TRUE)
{
  if(!is.data.frame(x) && !is.vector(x) && is.numeric(x))
    stop("x needs to be either a data.frame or a vector and numeric")

    
  if(is.null(grouping))
    return(sd(x))
  else
    grouping = as.data.frame(grouping)

#maximale Anzahl Beobachtungen in einer Gruppe ermitteln --> ni
  group = unique(grouping)
#  sgSize = max(diff(match(group, grouping)))
  
#  N = length(GROUP)
  

  
  #TODO: subgroups must be bigger than 2

  sdVec = numeric(length = length(group))
  for(i in 1:length(group))
  {
    if(is.data.frame(x))
    {
     temp = x[group[i] == grouping[,1], ]

     sdVec[i] = sd(temp, na.rm = T)/.c4(length(temp[!is.na(temp)]))
#          sdVec[i] = sd(temp, na.rm = T)
     print(sdVec[i])
      print(length(temp[!is.na(temp)]))
    }
    if(is.vector(x))
    {
     temp = x[group[i] == grouping[,1]]
     sdVec[i] = sd(temp, na.rm = T)/.c4(length(temp[!is.na(temp)]))
#          sdVec[i] = sd(temp, na.rm = T)
     print(sdVec[i])
    print(length(temp[!is.na(temp)]))
    }
  }

  if(DB == TRUE)
    print(paste("std.dev: ", mean(sdVec)))  
    
  return((mean(sdVec)))

}

#.sdSg(1:40,grouping = rep(1:5,8))
#x = 1:40
#grouping = rep(1:5, 8)
#.sdSg(as.data.frame(x), as.data.frame(grouping))


#This is the Box-Cox Transformation
.boxCoxTrans = function(x,lambda)
{
  if(abs(lambda) <= 1.e-6)
    y = log(x, base = exp(1))

  else
   y = (x^lambda - 1)/(lambda*((prod(x, na.rm = TRUE)^(1/length(na.omit(x))))^(lambda - 1)))

return(y)
}

#returns the standard deviation of the Box-Cox transformed values
.bctSD = function(lambda,x)
{
#  x = 1:10
  if(abs(lambda) <= 1.e-6)
    y = log(x, base = exp(1))

  else
   y = (x^lambda - 1)/(lambda*((prod(x, na.rm = TRUE)^(1/length(na.omit(x))))^(lambda - 1)))

return(sd(y))
}




#x - a single numeric vector
#x - a numeric data.frame
#start - starting parameters for fitdistr as list(if needed)
#grouping - if x is a numeric data.frame than grouping is expected to be a column that can be interpreted as 
#conf.level - used to set the number of sigmas to compute the process capability. Default is a value of 0.9973 which is the 6sigma area of a normal distribution
#distribution - the distribution function that is to be used for the calculation of the process.capability, normal ist implemented as default
#TODO: show an Anderson Darling Test for normality
#TODO: integrate a capability plot showing the process tolerance and specification limits of the process
#TODO: plot within a plot
#TODO: ... um z.B. breaks = "scott"  für hist einzugeben
#TOOD: define different methods for calculating the standard deviation
#TODO: cp - is interpreted as a potential process capability
#TODO: cpk - is interpreted as actual process capabitility
pcr = function(x, distribution = "normal", lsl, usl, target, boxcox = FALSE, lambda, main, xlim, ylim, grouping = NULL, std.dev = NULL,  conf.level = 0.9973002, start, lineWidth = 2, lineCol = "red", lineType = "solid", specCol = "black", specWidth = 2, cex.text = 2, cex.val = 1.5, cex.col = "darkgray", ...)
{
  DB = FALSE

  require(MASS)    

  par.orig <- par(c("mar", "oma", "mfrow"))
  on.exit(par(par.orig))
 
  parList = list(...)
  
  if(is.null(parList[["col"]]))
    parList$col = "lightblue"
  
  if(is.null(parList[["border"]]))
    parList$border = 1

  if(is.null(parList[["lwd"]]))
    parList$lwd = 1

  if(is.null(parList[["cex.axis"]]))
    parList$cex.axis = 1.5

  
  paramsList = vector(mode = "list", length = 0)  #keeps parameter estimates for distributions
  estimates = vector(mode = "list", length = 0)
  
  varName = deparse(substitute(x))
  
  dFun = NULL
  pFun = NULL
  qFun = NULL
                                                                                                          
  xVec = numeric(0)
  yVec = numeric(0)
  
  #make x and grouping a data.frame
  if(is.vector(x))
    x = as.data.frame(x)
    
  if(boxcox)
  {
    if(missing(lambda))
      lambda = c(-5,5)
    if(!is.numeric(lambda))
      stop("lambda needs to be numeric")
    if(length(lambda) >= 2)
      lambda = c(min(lambda), max(lambda))

    if(length(lambda) >= 2)
      opt = optimize(.bctSD, interval = lambda, x[,1])
    
    x = as.data.frame(x[,1]^lambda)     
  }
    
  numObs = nrow(x)  

  if(!is.null(grouping))
    if(is.vector(grouping))
      grouping = as.data.frame(grouping)

  center = mean(x)

  #check if grouping has the same length as the data contained in x
  if(!is.null(x) & !is.null(grouping))
  {
    if(nrow(x) != nrow(grouping))
      stop(paste("length of ",deparse(substitute(grouping)), " differs from length of ", varName)) 
  }
  
  if(missing(main))
    main = paste("Process Capability using", as.character(distribution),"distribution for\n", varName)
#  if(boxcox)
#    main = paste(main, "and lambda =", format(lambda,3))
  
  
  #if no standard deviation is given, calculate the standard deviation
  #Use only for normal distribution
  if(is.null(std.dev))
  {
    if(is.null(grouping))
     std.dev = .sdSg(x)
    else
     std.dev = .sdSg(x, grouping)
   }
   
   
   if (conf.level < 0 | conf.level > 1) 
        stop("conf.level must be a value between 0 and 1")
        
    confHigh = conf.level + (1 - conf.level)/2    
    confLow = 1-conf.level  - (1-conf.level)/2
    
    if(DB)
    {
      print(paste("confHigh:", confHigh))
      print(paste("confLow:", confLow))
    }

    ##TODO: obsolete???
    distWhichNeedParameters = c("weibull", "logistic", "gamma", "exponential", "f", "geometric", "chi-squared", "negative binomial", "poisson")  

    if(is.character(distribution))
    {
      qFun = .charToDistFunc(distribution, type = "q")
      pFun = .charToDistFunc(distribution, type = "p")
      dFun = .charToDistFunc(distribution, type = "d")
  
      if(is.null(qFun) & is.null(pFun) & is.null(dFun))     
       stop(paste(deparse(substitute(y)), "distribution could not be found!"))
    }
    
    
#      if(distribution %in% distWhichNeedParameters)
      if(TRUE)
      {
        if(DB)
          print("TODO: Pass the estimated parameters correctly")
          
        fitList = vector(mode = "list", length = 0)
        fitList$x = x[,1]; fitList$densfun = distribution
        if(!missing(start))
          fitList$start = start
        
        fittedDistr = do.call(fitdistr, fitList) 
        estimates = as.list(fittedDistr$estimate)
        paramsList = estimates                 

        if(DB)
          print(paste("parameter: ", paramsList))
      }
      
      #nur die parameter behalten die qFun nehmen kann
      paramsList = c(paramsList, .lfkp(parList, formals(qFun)))


      if(distribution == "normal" )
      {
        paramsList$sd = std.dev
      }
      
      
      if (missing(lsl) || !is.numeric(lsl) || lsl == usl) 
      {
         warning("lower specification limit should be provided!")
         paramsList$p = confLow
         if(distribution == "normal")
         {
            paramsList$mean = center
            paramsList$sd = std.dev
         }

         lsl = do.call(qFun, paramsList)
      }

      if (missing(usl) || !is.numeric(usl) || lsl == usl) 
      {
         warning("upper specification limit should be provided!")
         paramsList$p = confHigh
         if(distribution == "normal")
         {
            paramsList$mean = center
            paramsList$sd = std.dev
         }
         usl = do.call(qFun, paramsList)
      }
      
      #calculate target if missing
      if(missing(target))
            target = mean(c(usl, lsl))
      
      if(target < lsl | target > usl) 
            warning("target value is not within specification limits!")
      
      #correct switched specification limits
      if(lsl > usl)
      {
        temp = lsl
        lsl = usl
        usl = temp
      }

      #zu berechnende Quantile hinzufügen
      paramsList$p = c(confLow, 0.5, confHigh)      
      qs = do.call(qFun, paramsList)

      #use quantile method to calculate the process capability
      #see also: http://www.jmu.edu/docs/sasdoc/sashtml/qc/chap4/sect12.htm#tcaphdet1
      cp = (usl - lsl)/(qs[3] - qs[1])
      cpu = (usl - qs[2])/(qs[3] - qs[2])
      cpl = (qs[2] - lsl)/(qs[2] - qs[1])
      cpk = min(cpu, cpl)
      
      if(DB == TRUE)
      {
       print(cp)
       print(cpk)
       print(cpu)
       print(cpl)
      }


    if(missing(xlim))
    {      
      xlim <- range(x[,1], usl, lsl, target)
      xlim <- xlim + diff(xlim) * c(-0.2, 0.2)
    }

    xVec<- seq(min(xlim), max(xlim), length = 200)

    if(distribution == "normal")
      yVec <- dnorm(xVec, center, std.dev)
    else
    {
      dParamsList = .lfkp(paramsList, formals(dFun))
      dParamsList$x = xVec
      yVec = do.call(dFun, dParamsList)
    }

    histObj <- hist(x[,1], plot = FALSE)

    if(missing(ylim))
    {
      ylim <- range(histObj$density, yVec)
      ylim <- ylim + diff(ylim) * c(0, 0.05)
    }
    
    par(mar = c(0,0,0,0)+0.1)
    par(oma = c(12,4,4,5)+0.1)
    layout(matrix(c(1, 1, 1, 2,
                    1, 1, 1, 3,
                    1, 1, 1, 4), nr = 3, byrow = TRUE))
    do.call(hist, c(list(x[,1], freq = FALSE, xlim = xlim, ylim = ylim, main = ""), parList))
    abline(h = 0, col = "gray")
    
    tempList = parList
    tempList$col = 1
    tempList$border = NULL
    do.call(box, tempList)
    lines(xVec,yVec, lwd = lineWidth, col = lineCol, lty = lineType)
    abline(v = usl, col = specCol, lwd = specWidth)
    abline(v = lsl, col = specCol, lwd = specWidth)
    abline(v = target, col = specCol, lwd = specWidth)
    text(lsl, 0.9*max(ylim), "LSL", pos = 2, col = cex.col, cex = cex.text)
    text(usl, 0.9*max(ylim), "USL", pos = 4, col = cex.col, cex = cex.text)
    text(target, max(ylim), "TARGET", pos = 1, col = cex.col, cex = cex.text)

    title(main = main, outer = TRUE)
    
    pos1 = 0.025
    pos2 = 0.6
    mtext(expression(bar(x)), at = pos1, side = 1, line = 5, cex = 1.5, adj = 1, outer = TRUE)
    mtext(paste(" =", round(center,3)), at = pos1,  side = 1, line = 5, cex = 1.5, adj = 0, outer = TRUE)
    mtext("s",at = pos1, side = 1, line = 7, cex = 1.5, adj = 1, outer = TRUE)
    mtext(paste(" =", round(std.dev,3)), at = pos1,  side = 1, line = 7, cex = 1.5, adj = 0, outer = TRUE)
    mtext("n", at = pos1, side = 1, line = 9, cex = 1.5, adj = 1, outer = TRUE)
    mtext(paste(" =", numObs), at = pos1,  side = 1, line = 9, cex = 1.5, adj = 0, outer = TRUE) 
    mtext("Nominal Value", at = pos2, side = 1, line = 5, cex = 1.5, adj = 1, outer = TRUE)
    mtext(paste(" =", round(target,3)), at = pos2,  side = 1, line = 5, cex = 1.5, adj = 0, outer = TRUE) 
    mtext("USL",at = pos2, side = 1, line = 7, cex = 1.5, adj = 1, outer = TRUE)
    mtext(paste(" =", round(usl,3)), at = pos2,  side = 1, line = 7, cex = 1.5, adj = 0, outer = TRUE) 
    mtext("LSL", at = pos2, side = 1, line = 9, cex = 1.5, adj = 1, outer = TRUE)
    mtext(paste(" =", round(lsl,3)), at = pos2,  side = 1, line = 9, cex = 1.5, adj = 0, outer = TRUE) 

    #plot the process capability ratios
    plot(0:5, 0:5, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
    box()

    text(2, 1, expression(c[p]), pos = 2, cex = cex.val)
    text(2, 1, paste("=", round(cp,2)), pos = 4, cex = cex.val)
    text(2, 2, expression(c[pk]), pos = 2, cex = cex.val)
    text(2, 2, paste("=", round(cpk,2)), pos = 4, cex = cex.val)
    text(2, 3, expression(c[pkL]), pos = 2, cex = cex.val)
    text(2, 3, paste("=", round(cpl,2)), pos = 4, cex = cex.val)
    text(2, 4, expression(c[pkU]), pos = 2, cex = cex.val)
    text(2, 4, paste("=", round(cpu,2)), pos = 4, cex = cex.val)     

    #plot the distribution and the estimated parameters
    index = 0:(length(estimates)+3)
    plot(0:5, c(0:4,max(index)), type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
    box()
    
    #Anderson Darling test statistics for the legend
    adTestStats = .myADTest(x[,1], distribution)
    print(adTestStats)

    A = numeric()
    p = numeric()
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
    text(2, rev(index)[2], "A", pos = 2, cex = cex.val)
    text(2, rev(index)[2], paste("=", round(A,3)), pos = 4, cex = cex.val)
    text(2, rev(index)[3], "p", pos = 2, cex = cex.val)
    text(2, rev(index)[3], paste("=", round(p,3)), pos = 4, cex = cex.val)

    j = 1
    for(i in 3:(3+length(estimates)-1))
    {
      #print(paste("estimates:",estimates))
      try(text(2, rev(index)[i+1], names(estimates)[[j]], pos = 2, cex = cex.val), silent = TRUE)
      try(text(2, rev(index)[i+1], paste("=", round(estimates[[j]],3)), pos = 4, cex = cex.val), silent = TRUE)
      j = j+1
    }
    
    #include a qqplot for the specified distribution
    qqPlot(x[,1], y = distribution, ylab = "", main = "", axes = F)
    axis(1)
    axis(4)
    box()
    
}


#pcr(1:10, lsl = 10, usl = 20, distribution = "weibull")
#octane = c(86, 87, 87.2, 87.4, 87.8, 88.2, 88.9, 89.6, 89.7, 90)
#pcr(octane, distribution = "weibull", usl = 80, lsl = 90, cex.val = 1.5)
#pcr(octane, distribution = "exponential",usl = 80, lsl = 90, cex.val = 1.5)
#pcr(octane, distribution = "normal", usl = 80, lsl = 90, cex.val = 1.5)
#pcr(octane, distribution = "cauchy", usl = 80, lsl = 90, cex.val = 1.5)
#pcr(octane, distribution = "logistic", usl = 80, lsl = 90, cex.val = 1.5)
#pcr(octane, distribution = "log-normal", usl = 80, lsl = 90, cex.val = 1.5)
#
#
#pcr(round(octane), distribution = "negative binomial", cex.val = 1.5)
#pcr(round(octane), distribution = "poisson", cex.val = 1.5)
#
#pcr(octane, distribution = "beta", cex.val = 1.5)
#pcr(octane, distribution = "f", cex.val = 1.5)
#pcr(octane, distribution = "geometric", cex.val = 1.5)
#




#pcr(rnorm(40, 15,1), lsl = 10, usl = 20, target = 16)
#set.seed(543)
#pcr(rnorm(40, 15,1), lsl = 10, usl = 20, target = 16, cex.val = 1.5, distribution = "normal", lty = "solid")
#grouping = rep(1:5, 8)
#pcr(rnorm(40, 15,1), grouping = grouping,lsl = 10, usl = 20, target = 16, cex.val = 2)
#


















