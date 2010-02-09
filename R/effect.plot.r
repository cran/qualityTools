###x and y are data.frames
###TODO: Zur Zeit werden die Mittelwerte der Daten selber genommen
###effectPlot = function(x,y, col = "red", pch = 15, showPoints = FALSE, DB = T)
#effectPlot gets a facDesignObject only
effectPlot = function(fdo, main = paste("Main Effect Plot for ", deparse(substitute(fdo))), showPoints = FALSE, ylab = NULL, pch = 15, ...)
{
  DB = FALSE
  
  old.par <- par(no.readonly = TRUE) #store all par settings which could be changed.
  on.exit(par(old.par))


  if(!identical(class(fdo)[1] ,"facDesign"))
    stop(paste(deparse(substitute(fdo)), " is not an object of class facDesign!"))
  if(!validObject(fdo))
    stop(paste(deparse(substitute(fdo)), " is not a valid object of class facDesign!"))  


  x = cube(fdo)
  runIndex = order(runOrd(fdo))
  x = x[runIndex[1:nrow(x)],]
  y = data.frame(response(fdo)[1:nrow(x),])
  names(y) = names(response(fdo))
  
  numEffects = ncol(x)

  if(DB)
    print(numEffects)

  for(j in seq(along = y))  #durch jede Spalte der Zielgroeßenmatrix gehen
  {

    if(j > 1) #create a new window if there is more than one response in y
    {
      windows()
    }


    xAxis = numeric(0)

    ##will hold the x and y Values for plotting    
    xVec = numeric(0)
    yVec = numeric(0)
    
    
    for(i in seq(along = x))
    {
     xVals = c(min(x[,i]), max(x[,i]))  #den kleinsten und groeßten Wert heraussuchen --> sollte -1 und 1 ergeben
     yVals = c(mean(y[(x[,i] == min(x[,i]) ),j]),mean(y[(x[,i] == max(x[,i])),j]))
     xAxis = c(xAxis,c((2*i)-1, 2*i))  #Wert fuer axis(1, at = xAxis...
     ylow = y[x[,i] == min(x[,i]),j]   #y-Werte fuer xmin
     yhigh = y[x[,i] == max(x[,i]),j] #y-Werte fuer xmax
  
     xVec = c(xVec, c((2*i)-1, 2*i))
     yVec = c(yVec, yVals)
  
    
     if(DB)
     {
      cat("xVals: ", xVals,"\n")     
      cat("xVals: ", xVals,"\n")
      cat("yVals: ", yVals,"\n")
     }
      #initialize the plot that will hold all other plots
      if(i == 1 && showPoints == T)
      {
         labelSpace = 2*strheight("M", units = "fig") * abs(diff(range(y[,j])))
         
         if(is.null(ylab))
           ylab = names(response(fdo))[j]
         plot(0,0, ylab = ylab , xlab = "", xlim = c(1-0.3, 2*numEffects+0.2), ylim = c(min(y), max(y) + 2*labelSpace), type = "n", axes = F, ...)
         abline(h = max(y) + labelSpace)
         box(which = "plot")
      }

      if(showPoints)
      {
        lines(c((2*i)-1, 2*i), yVals, lwd = 2)
        points(c((2*i)-1, 2*i), yVals, pch = 15, cex = 2)
        abline(v = 2*i+0.5)
        points(c(rep((2*i)-1,length(ylow)),rep(2*i,length(yhigh))),c(ylow, yhigh))
        text((2*i)-1.5,max(y) + 2*labelSpace,paste(names(x)[i],": ",names(fdo)[[i]], sep = ""), cex = 1.5, pos = 4) 
      }

    }
    
    if(showPoints == FALSE)##geplottet ohne die einzelnen Beobachtungen
    {
      labelSpace = 2*strheight("M", units = "fig") * abs(diff(range(yVec))) #oberer Rand um die Labels reinzusetzen

      if(is.null(ylab))
        ylab = names(response(fdo))[j]

      plot(xVec, yVec, xlim = c(min(xVec)-0.3, max(xVec)+0.2), ylab = ylab, xlab = "", ylim = c(min(yVec), max(yVec) + 2*labelSpace), type = "p", cex = 2, pch = 15, axes = F, ...)
      abline(h = max(yVec) + labelSpace)
      box(which = "plot")
#    } 

    xAxis = numeric(0)
    for(i in 1:numEffects)
    {
      pos = c((2*i)-1, 2*i)
      xAxis = c(xAxis, pos)
      lines(pos, yVec[pos], lwd = 2)
      
      if(i != numEffects)
      {
        abline(v = 2*i+0.5)
      }  
        
      text((2*i)-1.5,max(yVec) + 2*labelSpace,paste(names(x)[i],": ",names(fdo)[[i]], sep = ""), cex = 1.5, pos = 4) 
    }
    }
    axis(1, at = xAxis, labels = rep(c("-", "+"), numEffects), cex.axis = 2)
    axis(2, ...)
    
  }

    title(main)
    
    invisible()
}


class(effectPlot) <- "invisible"


###Example for effectPlot
#test = fracDesign(k = 4, gen = "D = ABC")
#response(test) = rnorm(8)
#names(test) = c("Druck", "Temperatur", "Katalysator", "Umdrehung")
#effectPlot(test)
#effectPlot(test, showPoints = F)
#

