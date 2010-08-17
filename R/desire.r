#######
#####Desire function for the facDesign Object#############
setClass(Class = "desirability", representation = representation(response = "character", low = "numeric", high = "numeric", target = "ANY", scale = "numeric", importance = "numeric")
        )

#return - an object of class desirability for a response
#constraints - gibt obere und untere Grenze des Versuchraumes an. Basierend darauf wird ein newData data.frame aufgebaut
desirability = function(response, low, high, target = "max", scale = c(1,1), importance = 1, constraints)
{
  if(low >= high)
    stop("the lower bound must be greater than the high bound!")
  
  if(any(scale <= 0)) #for a specific target scale consists of 1 value for lower side and 1 value for upper side
    stop("the scale parameter must be greater than zero!")
    
  if(!is.numeric(target) &  !identical(tolower(target), "min") & !identical(tolower(target), "max"))
    stop("target needs to be \"min\", \"max\" or a numeric value")
 
  return(new("desirability", response = deparse(substitute(response)), low = low, high = high, target = target, scale = scale, importance = importance))
}



#function to transform a desirability into a function
#each desire function has a lower bound (min) and an upper bound
#if no numerical target is given "maximization is expected" unless "minimization is specified"
#target - min, max or a numerical value
#scale - scaliing factors
#importance - importance factor (not yet implemented)
#TODO: check for qualitative factors from fdo - object
#@return - a desire function 
.desireFun = function(low, high, target = "max", scale = c(1,1), importance = 1)
{
  DB = FALSE
  
  if(low >= high)
    stop("the lower bound must be greater than the high bound!")
  if(any(scale <= 0)) #for a specific target scale consists of 1 value for lower side and 1 value for upper side
    stop("the scale parameter must be greater than zero!")

  if(is.numeric(target))
  {
    out = function(y)
      {
        if(DB)
          print("target")

        flush.console()      
        d = rep(0, length(y))
        d[y >= low & y <= target] = ((y[y >= low & y <= target] - low) / (target - low))^scale[1]
        d[y >= target & y <= high] = ((y[y >= target & y <= high] - high) / (target - high))^scale[2]
        return(d)
      }
    return(out)
  }
  
  if(identical(tolower(target), "min"))
  {
      out = function(y)
      { 
        if(DB)
          print("min")
        
        d = rep(0, length(y))
        d[y > high] = 0
        d[y < low] = 1
        d[y >= low & y <= high] = ((y[y >= low & y <= high] - high) / (low - high))^scale[1]
        return(d)
      }
      return(out)
  }
  
  if(identical(tolower(target), "max"))
  {
      out = function(y)
      { 
        if(DB)
          print("max")
          
        d = rep(0, length(y))             
        d[y < low] = 0
        d[y > high] = 1
        d[y >= low & y <= high] = ((y[y >= low & y <= high] - low) / (high - low))^scale[1]
        return(d)
      }
      return(out)
  }
}

#dTest = .desireFun(10, 20, 12)
#dTest(10)
#dTest = .desireFun(10, 20, target = "max")
#dTest(y)
#dTest = .desireFun(10, 20, target = "min")


#show Methode for an object of class desirability
setMethod("show", signature(object = "desirability"), function(object) {
  
  if(!is.numeric(object@target))
    cat("Target is to",paste(object@target, "imize", sep = ""),object@response,"\n")
  else
    cat("Target is ",object@target, " for",object@response,"\n")
  cat("lower Bound: ",object@low,"\n")
  cat("higher Bound: ",object@high,"\n")
  
  if(is.numeric(object@target))
    cat("Scale factor is: low =",object@scale[1],"and high =", object@scale[2], "\n")
  else
    if(identical("min", object@target) | identical("max", object@target))
      cat("Scale factor is: ", object@scale, "\n")

  cat("importance: ",object@importance,"\n")
  cat("\n")
  }
)


##plot method for an object of class desirability
##scale - parameter for teaching purposes only
##TODO: integrate the slot 'importance'
#setMethod("plot", signature(x = "desirability"), function(x, y = missing, scale, main, xlab, ylab, type, col, numPoints = 500, ...)
#setGeneric("plot", function(x, y,scale, main, xlab, ylab, type, col,...) standardGeneric("plot"))
#setGeneric("plot", useAsDefault = function(x,y,...)graphics::plot(x,y, ...))
setGeneric("plot", function(x,y, ...) standardGeneric("plot"))
setMethod("plot", signature(x = "desirability"), function(x, y, scale, main, xlab, ylab, type, col, numPoints = 500,...)
{


  xm1 = NULL
  xm2 = NULL
  ym  = NULL
  y = NULL
  
  if(missing(main))
    main = paste("Desirability function for",x@response)
  if(missing(xlab))
    xlab = x@response
  if(missing(ylab))
    ylab = "Desirability"
  if(missing(type))
    type = "l"
  if(missing(scale))
    scale = x@scale
  if(missing(col))
    col = 1:length(scale)
    
  dFun = .desireFun(x@low, x@high, x@target, x@scale, x@importance)
  
  xVals = seq(x@low - 0.04*diff(range(x@low, x@high)), x@high + 0.04*diff(range(x@low, x@high)), length  = numPoints)
  yVals = dFun(xVals)

  plot(xVals, yVals, main = main, xlab = xlab, ylab = ylab, type = type, col = col, ...)
  
  #coordinates for the scale parameter labels
  if(is.numeric(x@target))
  {
    xm1 = mean(c(par("usr")[1], x@target))
    xm2 = mean(c(par("usr")[2], x@target))

    ym1 = yVals[max((1:numPoints)[xVals <= xm1])]
    ym2 = yVals[max((1:numPoints)[xVals <= xm2])]
    
    text(xm1 + 0.025*diff(range(par("usr")[1:2])), ym1, paste("scale =",scale[1]), adj = c(0,0))
    text(xm2 - 0.025*diff(range(par("usr")[1:2])), ym2, paste("scale =",scale[2]), adj = c(1,1) )
  }
  else
  {
    xm1 = mean(par("usr")[c(1,2)])
    ym1 = yVals[max((1:numPoints)[xVals <= xm1])]
    if(identical(x@target, "max"))
      text(xm1 + 0.025*diff(range(par("usr")[1:2])), ym1 - 0.025*diff(range(par("usr")[3:4])), paste("scale =",scale[1]), adj = c(0,0))
    else
      text(xm1 + 0.025*diff(range(par("usr")[1:2])), ym1 + 0.025*diff(range(par("usr")[3:4])), paste("scale =",scale[1]), adj = c(0,1))
  }

  out = list(x = xVals, y = yVals)
  names(out) = c(x@response, "desirability")

  invisible(out)
}
)

#test2 = desirability(y, 10, 20, target = 17, scale = c(0.6, 2))
#plot(test2)
#test1 = desirability(y, 10, 20, target = "max", scale = c(0.3))
#plot(test1)
#test3 = desirability(y, 10, 20, target = "min", scale = c(7))
#plot(test3)
#
#test = desirability(y, 10 ,20)
#plot(test)
#
#test2 = desirability(y, 10 ,20, target = 12, scale = c(2,7))
#plot(test2, col = 3, cex.axis = 2, lwd = 2, main = "re")
#


#gibt die desirability Werte für eine bestimmte Zielgröße zurück data.frame Format
#if response is NULL return all
#desVal(fdo, response = NULL)
#fdo - objcet of class facDesign
#constraints - a list with constraints for each factor i.e. constraints = list(A = c(-1,4), B = c(-1,3) ....)
overall = function(fdo, steps = 20, constraints,...)
{
  DB = FALSE
 #take each lm model
 #generate the desirability functions
 #calculate the desirability values

 #store the constraints  
 cs = list()
 if(!missing(constraints))
   cs = constraints
 
 l = vector(mode = "list", length = 0)
 
 #TODO: discard constraints if not numeric
 
 fitList = fits(fdo)
 if(length(fitList) < 1)
  stop(paste("no fits found in fits(",deparse(substitute(fdo)),")"), sep = "")
 
 desList = desires(fdo) 
 if(length(desList) < 1)
    stop(paste("no desirabilities found in desires(",deparse(substitute(fdo)),")"), sep = "")

  #create newData
  X = cube(fdo)  #first part of model is always the response?!?
  newdata = NULL

  for(i in names(names(fdo)))
  {
    seqList = vector(mode = "list", length = 0)
    seqList[["length"]] = steps
    seqList[["from"]] = min(X[,i])
    seqList[["to"]] = max(X[,i])
    minC = NULL
    maxC = NULL
    if(!is.null(cs[[i]]))
    {
      if(length(cs[[i]]) < 2)
        stop("length of ", names(cs[i]),"=", cs[i], " < 2 in constraints") 
      minC = min(cs[[i]])
      if(!is.null(minC) & !is.na(minC))
        seqList[["from"]] = minC
      maxC = max(cs[[i]])
      if(!is.null(maxC) & !is.na(maxC))
        seqList[["to"]] = maxC
      
      if(maxC == minC)
        stop(paste("equal values in constraints ",names(cs[i]),"=", cs[i]))

    }
   l[[i]] = do.call(seq, seqList)
  }
  
  if(DB)
    print(l)
  
  newdata = expand.grid(l)
  names(newdata) = names(X)
  out = newdata

  yCharSet = intersect(names(desires(fdo)), names(fits(fdo))) 
  dFrame = data.frame(matrix(NA, nrow = nrow(newdata), ncol = length(yCharSet)+1 ))
  names(dFrame) = c(yCharSet, "overall")
  dFrame[, "overall"] = 1 #standardmaessig fuer apply
  

  
  for(y in yCharSet) #calculate the desirability for each response in yCharSet
  {
    obj = desList[[y]]
    dFun = .desireFun(obj@low, obj@high, obj@target, obj@scale, obj@importance)
    lm.y = fitList[[y]]
    
    yHat = predict(lm.y, newdata = newdata, ...)
    yDes = dFun(yHat)
    dFrame[,y] = yDes
#    yDesProd = yDesProd*yDes
    
    if(DB)
    {
     print(y)
     print(dFun)
     print(lm.y)
     print(dFrame)
    }
  }
#  overall = apply(dFrame[,-pmatch("overall", names(dFrame))], 1, prod)^(1/length(yCharSet))
  overall = apply(dFrame, 1, prod)^(1/length(yCharSet))
  dFrame[,"overall"] = overall  #1en ueberschreiben
  dFrame = cbind(out, dFrame)
  invisible(dFrame)
}



#helper function to calculate the desirabilities for a combination of factors
#factors - list i.e. list(A = 1, B = 0)
.desHelp = function(fdo, factors,...)
{
  if(length(factors) != length(names(fdo)))
    stop("not enough factors specified in factors")
  
  if(any(is.na(factors)))
    stop("factors contain NA")

  yCharSet = intersect(names(desires(fdo)), names(fits(fdo))) 
  
  desList = desires(fdo)
  fitList = fits(fdo)
  yDes = vector(mode = "list")  #holds the desirabilities
  
  
  for(y in yCharSet) #calculate the desirability for each response in yCharSet
  {
    obj = desList[[y]]
    dFun = .desireFun(obj@low, obj@high, obj@target, obj@scale, obj@importance)
    lm.y = fitList[[y]]
    
    yHat = predict(lm.y, newdata = data.frame(factors),...)
    yDes[[y]] = dFun(yHat)
  }
  
  return(yDes)
}


##testing the desirability approach with 2 factors and one response
#x = rsmDesign(k = 2, blocks = 2, alpha = "both")
#runif(1)
#response(x) = data.frame(y= rnorm(nrow(response(x)), 12, sd =  2), y2 =  -2*x[,4]^2 - x[,5]^2 + rnorm(14, 12))
#
#fits(x) = lm(y ~ A*B + I(A^2) + I(B^2), data = x)
#fits(x) = lm(y2 ~ A*B + I(A^2) + I(B^2), data = x)
#
#steps = 20
#temp = data.frame(expand.grid(seq(-1.4, 1.4, length = steps), seq(-1.4, 1.4, length = steps)))
#names(temp) = names(names(x))
#ynew = predict(fits(x)$y, newdata = temp)
#jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
#wireframe(ynew ~ temp$A*temp$B, drape = TRUE, col.regions = jet.colors(length(unique(ynew))))
#
#
#desires(x) = desirability(y, 6, 18, scale = c(1, 1), target = "max")
#desires(x) = desirability(y2, 6, 18, scale = c(1, 1), target = "min")
#dVals = overall(x, steps = 10, constraints = list(A = c(0,1), B = c(0.4, 0.5)))
##wireframe(dVals[,3] ~ dVals[,1]*dVals[,2], shade = TRUE, col.regions = jet.colors(length(unique(dVals[,3]))))
#contourplot(dVals[,3] ~ dVals[,1]*dVals[,2], shade = TRUE, col.regions = jet.colors(length(unique(ynew))), region = TRUE, cuts = 200,labels = FALSE , cex.col = "transparent", col = "transparent")
#



#x = facDesign(3, replicates = 2)
##response(x) = data.frame(y1 = rnorm(16, 4, 2), y2 = rnorm(16, 8, 2))
#response(x) = data.frame(y1 = rnorm(16, 10) + (x[,4] + 2*x[,4]^2 + 3*x[,4]*x[,6]), y2 = rnorm(16, 15) + jitter(x[,4] + 2*x[,6]^2 + 3*x[,4]*x[,5]))
#fits(x) = lm(y1 ~ A*B*B, data = x)
#fits(x) = lm(y2 ~ A*B*B, data = x)
#desires(x) = desirability(y1, 0, 40, scale = c(1,1), target = mean(response(x)[,1]))
#desires(x) = desirability(y2, 0, 40, scale = c(1,1), target = mean(response(x)[,2]))
#desires(x) = desirability(y1, -1, 3, scale = c(1,1), target = 2.8)
#desires(x) = desirability(y2, -3, 6, scale = c(1,1))
#test = overall(x)
#hist(test[,6])
#require(lattice)
#contourplot(test[,6] ~ test[,3]*test[,2] | test[,1])
#wireframe(test[,6] ~ test[,3]*test[,2] | test[,1], shade = T)
#wireframe(test[,6] ~ test[,3]*test[,2] | test[,1], drape = T)
#wireframe(test[,6] ~ test[,1]*test[,3] | test[,2], drape = T)
#



setClass(Class = "desOpt", representation = representation(facCoded = "list", facReal = "list", responses ="list", desirabilities = "list", overall = "numeric", all = "data.frame", fdo = "facDesign"))


#gibt den Versuchsplan als data.frame zurueck
#called by lm when assigning an object of class facDesign to data
#setGeneric("as.data.frame.desOpt", function(x, row.names = NULL, optional = FALSE,...) standardGeneric("as.data.frame.desOpt"))
#setMethod("as.data.frame.desOpt", "desOpt", function(x, row.names = NULL, optional = FALSE,...) {
as.data.frame.desOpt = function(x, row.names = NULL, optional = FALSE,...) {
  return(x@all)
}


setMethod("as.data.frame", "desOpt", function(x, row.names = NULL, optional = FALSE,...) {
  return(x@all)
}
)



#is a helper function
#takes a facDesign object and a list of constraints (i.e. list(A = c(0,1), B = v(-1,1)) and makes a valid list
.validizeConstraints = function(fdo, constraints)
{
  X = as.data.frame(fdo)

  #set default values for constraints
  csOut = vector(mode = "list")
  for(i in names(names(fdo)))
  {
   csOut[[i]] = c(min(X[,i]), max(X[,i]))
  }

  #return default constraints if missing
  if(missing(constraints))
    return(csOut)
  

  
  #clean up the given constraints
  cs2 = constraints[names(names(fdo))]
  cs2 = cs2[!unlist(lapply(cs2, is.null))]
  cs2 = cs2[(unlist(lapply(cs2, length)) == 2)]
  #substitute given constraints  
  csOut[names(cs2)] = cs2[names(cs2)]

  return(csOut)  
}

#.validizeConstraints(fdo, temp)
#.validizeConstraints(fdo, constraints = list(A = c(-1,1), B = NA, F = c(-1,1)))



#type - grid evaluating a grid with steps points, optim: using optim function to search for an optimum
#start - specify starting point for optim
optimum = function(fdo, constraints, steps = 25, type = "grid", start, ...)
{
  DB = FALSE
  if(missing(fdo))
    stop("missing fdo!")

  X = as.data.frame(fdo)
  numFac = length(names(fdo))

  #check for types
  if(!(type %in% c("grid", "optim")))
  {
    warning(paste("type =", deparse(substitute(type)), "not found --> using type = \"grid\""))
    type = "grid"
  }

  constraints = .validizeConstraints(fdo, constraints)
  
  #optim starts in the center of the constrained design
  if(missing(start))
    start =  as.numeric(lapply(constraints, mean))

  #constraints for optim
  lower = numeric(length(constraints))
  upper = numeric(length(constraints))

  #transform constrains to upper and lower for optim
  for(i in seq(along = constraints))
  {
    lower[i] = min(constraints[[i]])
    upper[i] = max(constraints[[i]])
  }
  
  

  if(DB)  
  {
    print(constraints)
    print(start)
  }

  desOpt = new("desOpt")
  desOpt@fdo = fdo

  facCoded = NA #best combination in coded factor units
  desirabilities = NA
  overall = NA

  setList = list()
  
  #######finding optimal factor combination
  #searching the whole grid
  if(type == "grid")
  {
    dVals = overall(fdo = fdo, constraints = constraints, steps = steps)
    
    #index factor settings for highest desirability
    index = order(dVals[,"overall"], decreasing = TRUE)[1]

    desOpt@all = dVals
    desOpt@facCoded = as.list(dVals[index,names(names(fdo))])

    desirabilities = as.list(dVals[index, names(response(fdo))])
    overall = dVals[index, "overall"]
  }
  #using optim to find the maximum desirability
  if(type == "optim")
  {
      #this is the function for optim
      optDes = function(vec)
      {
        DB = FALSE
        l = as.list(vec)
        names(l) = LETTERS[1:length(vec)]
        newdata = data.frame(l)

        if(DB)
          print(paste("newdata:", newdata))

       fitList = fits(fdo)
       if(length(fitList) < 1)
        stop(paste("no fits found in fits(",deparse(substitute(fdo)),")"), sep = "")

       desList = desires(fdo)
       if(length(desList) < 1)
          stop(paste("no desirabilities found in desires(",deparse(substitute(fdo)),")"), sep = "")

        yCharSet = intersect(names(desires(fdo)), names(fits(fdo)))
        dFrame = data.frame(matrix(NA, nrow = nrow(newdata), ncol = length(yCharSet)+1 ))
        names(dFrame) = c(yCharSet, "overall")
        dFrame[, "overall"] = -Inf #standardmaessig fuer apply

        for(y in yCharSet) #calculate the desirability for each response in yCharSet
        {
          obj = desList[[y]]
          dFun = .desireFun(obj@low, obj@high, obj@target, obj@scale, obj@importance)
          lm.y = fitList[[y]]

          yHat = predict(lm.y, newdata = newdata)
          yDes = dFun(yHat)
          dFrame[,y] = yDes
       }

        overall = apply(dFrame[,-ncol(dFrame)], 1, prod)^(1/length(yCharSet))
        dFrame[,"overall"] = overall  #1en ueberschreiben

        return(dFrame[,"overall"])
      }

  if(DB)
  {
    print(paste("lower:",lower))
    print(paste("upper:",upper))
  }
  
  #negative fnscale means maximization, method = "L-BFGS-B" allows for constraints
  temp = optim(par = start, optDes, method = "L-BFGS-B", lower = lower, upper = upper, control = list(fnscale = -1, maxit = 1000))

  #store the best setting
  facCoded = as.list(temp$par)
  names(facCoded) = names(names(fdo))
  desOpt@facCoded = facCoded
  
  #store the overall desirability
  overall = temp$value
  
  #store the desirabilities for the best settings of the factors
  desirabilities = .desHelp(fdo, desOpt@facCoded)
  }


  #calculate the settings of the real world factors
  for(i in names(desOpt@facCoded))
  {
    desOpt@facReal[[i]] = code2real(lows(fdo)[[i]], highs(fdo)[[i]], desOpt@facCoded[[i]])
  }
  desOpt@desirabilities = desirabilities
  desOpt@overall = overall
  newData = do.call(data.frame, desOpt@facCoded)

  #calculate the obtained responses at these settings
  for(i in names(desOpt@desirabilities))
  {
    desOpt@responses[[i]] = predict(fits(fdo)[[i]], newData)
  }
  return(desOpt)
}

#optimum(fdo, type = "optim", constraints = list(A = c(-1,1), B = c(-1,1)))


##show method for an object of class desOpt
setMethod("show", signature(object = "desOpt"), function(object)
{
  cat(paste("\ncomposite (overall) desirability:", format(object@overall, digits = 3)))  
  cat("\n")
  cat("\n")
  
  temp1 = do.call(data.frame, object@facCoded)
  temp2 = do.call(data.frame, object@facReal)
  facFrame = rbind(temp1, temp2)
  row.names(facFrame) = c("coded", "real")
  show(format(facFrame, digits = 3))
  
  temp1 = do.call(data.frame, object@responses)
  temp2 = do.call(data.frame, object@desirabilities)
  respDesFrame = rbind(temp1, temp2)
  row.names(respDesFrame) = c("Responses", "Desirabilities")
  cat("\n")
  show(format(respDesFrame,digits = 3))
}
)




######################################################Obsolete Part##########################################


##calculates the best settings for factors given desirabilities for responses
#optimum = function(fdo, constraints = list(), steps = 25, ...)
#{
#  if(missing(fdo))
#    stop("missing fdo!")
#  
#  desOpt = new("desOpt")
#  desOpt@fdo = fdo
#    
#  setList = list()
#  dVals = overall(fdo = fdo, constraints = constraints, steps = steps)
#  index = order(dVals[,"overall"], decreasing = TRUE)[1]
#  
#  desOpt@all = dVals
#  desOpt@facCoded = as.list(dVals[index,names(names(fdo))])
#  newData = do.call(data.frame, desOpt@facCoded)
#  print(newData)
#
#  #calculate the settings of the real world factors
#  for(i in names(desOpt@facCoded))
#  {
#    desOpt@facReal[[i]] = code2real(lows(fdo)[[i]], highs(fdo)[[i]], desOpt@facCoded[[i]])
#  } 
#  desOpt@desirabilities = as.list(dVals[index, names(response(fdo))])
#  desOpt@overall = dVals[index, "overall"]
#  
#  #calculate the obtained responses at these settings
#  for(i in names(desOpt@desirabilities))
#  {
#    print(i)
#    print(fits(fdo)[[i]])
#    desOpt@responses[[i]] = predict(fits(fdo)[[i]], newData)  
#  }
#  return(desOpt)
#}
#
