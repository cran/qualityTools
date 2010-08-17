#TODO:
#if 0 degrees of freedom --> make a lenth's plot
#if(df.resid == 0) (pass lm.1 to lenth.plot) and make a nice lenth plot
paretoPlot = function(fdo, threeWay = FALSE, abs = TRUE, decreasing = TRUE, na.last = NA, alpha = 0.05, xlim, ylim, xlab, ylab, main, ...)
{
  DB = FALSE
  
  ylimMissing = FALSE
  
  if(missing(ylim))
    ylimMissing = TRUE
  
  if(missing(xlab))
    xlab = ""
  
  #place legend depending on the order of the bars
  location = "topright"
  if(decreasing == F)
  {
    location = "topleft"
  }
    

#  old.par <- par(no.readonly = TRUE) #store all par settings which could be changed.
#  on.exit(par(old.par))

  xVals = numeric(0)  #to hold the x-ticks of the barplot
  sig.neg = NULL
  sig.pos = NULL
  effect.list = vector("list")

  
  for(j in 1:ncol(response(fdo)))
  {

    par(mar = c(5.1, 4.1, 4.1, 4.1))      
    if(j > 1)
    {
      windows()
      par(mar = c(5.1, 4.1, 4.1, 4.1))
    }
      
  if(!any(is.na(response(fdo)[,j])))
  {
  
  if(missing(ylab))
    ylab = names(response(fdo))[j]
    
    
  
  
  #TODO check if names of response can stand on the left side -> Workaround --> save name for each column and go by column
  #form = paste(names(response(fdo)), "~")  #is problematic for an expresssion that can be evaluated
  form = paste("response(fdo)[,",j,"]~")


  for(i in 1:ncol(cube(fdo)))
  {
     form = paste(form, names(cube(fdo))[i], sep = "")

     if(i < ncol(cube(fdo)))
      form = paste(form, "*", sep = "")
  }

  if(DB == TRUE)
    print(form)
    
  lm.1 = lm(as.formula(form), data = as.data.frame(fdo))
  coefs = coef(lm.1)[-pmatch("(Intercept)", names(coef(lm.1)))]
  df.resid = df.residual(lm.1)
  num.c = nrow(centerCube(fdo))
  
  
  if(df.resid == 0) #barplot of effects significance lines by lenth analysis
  {                 #Desing and Analysis of experiments - Volume2 Advanced Experimental Design - Hinkelmann/Kempthorne
    effect = 2*coefs
    effect = effect[!is.na(effect)] #for fractional designs not all of the effects are calculated
    effect.list[[j]] = effect

    if(missing(main))
      main = "Lenth Plot of effects"    
    
   plt = TRUE
   limits = TRUE
   faclab = NULL
   m = length(effect) #2^(k-p) - 1 --> Anzahl der Effekte - 1 für Intercept
   d = m/3
   s0 = 1.5*median(abs(effect))
   rmedian = effect[abs(effect) < 2.5*s0] #restricted median
   PSE = 1.5*median(abs(rmedian))  #Pseudo standard error
   ME = qt(1 - alpha/2, d)*PSE   #margin of error
   Gamma = (1 + (1 - alpha)^(1/m))/2
   SME = qt(Gamma, d)*PSE

   n = length(effect)
   
   if(ylimMissing)
    if(abs)
     ylim <- (range(c(0, abs(effect), 1.3*ME)))*1.1
    else
     ylim <- (range(c(effect, -1.3*ME, 1.3*ME)))*1.1

   if(abs)
   {
     xVals = barplot(abs(effect), las = 2, main = main, xlab = xlab, ylim = ylim, ylab = ylab, ...)
     abline(h = ME, col = "red")
     abline(h = SME, col = "red")
     try(axis(4, at = ME, labels = round(ME,3), las = 2), silent = T)
     text(x = xVals[1], y = ME, "ME", pos = 3) 
     try(axis(4, at = SME, labels = round(SME,3), las = 2), silent = T)
     text(x = xVals[2], y = SME, "SME", pos = 3) 
   }
   else
   {
     xVals = barplot(effect, las = 2, main = main, xlab = xlab, ylim = ylim, ylab = ylab, ...)
     abline(h = c(-ME, ME), col = "red")
     abline(h = c(-SME, SME), col = "red")
     try(axis(4, at = c(-ME, ME), labels = round(c(-ME, ME),3), las = 2), silent = T)
     text(x = xVals[1], y = c(-ME,ME), "ME", pos = c(1,3)) 
     try(axis(4, at = c(-SME, SME), labels = round(c(-SME, SME),3), las = 2), silent = T)
     text(x = xVals[2], y = c(-SME, SME), "SME", pos = c(1,3))
   }
    
   if(length(xVals) >= 1)
      for(i in 1:length(xVals))
      {
       text(xVals[i], effect[i] + max(ylim)*sign(effect[i])*0.05, format(round(effect[i],3)))
      }
   
   
   
  if(DB)    
    print(paste("MSE:", ME, "SME:", SME))
    
    
  }
  else #barplot of standardized effects
  {
    if(missing(main))
      main = "Standardized main effects and interactions"
  
    effect = ((summary(lm.1)$coefficients[-pmatch("(Intercept)", names(coef(lm.1))),1]) / (summary(lm.1)$coefficients[-pmatch("(Intercept)", names(coef(lm.1))),2]))
    if(all(is.na(effect)))
      stop("effects could not be calculated")

    effect = effect[!is.na(effect)] #for fractional designs not all of the effects are calculated    
    effect.list[[j]] = effect
    
    
  if((df.resid) > 0)
  {
    sig.pos = -qt(alpha/2, df.resid)
    sig.neg = +qt(alpha/2, df.resid)
  }

  if(ylimMissing)
    if(abs)
    {
      tempVec = c(effect, sig.pos)
      tempVec = tempVec[!is.na(tempVec)]
      print(tempVec)
      ylim = c(0, 1.3*max(tempVec))    
    }
    else
    {
      tempVec1 = c(0,effect,sig.neg, sig.pos)
      tempVec1 = tempVec1[!is.na(tempVec1)]
      tempVec2 = c(abs(effect), sig.pos, sig.neg)
      tempVec2 = tempVec2[!is.na(tempVec2)]
      ylim = c(1.3*min(tempVec1), 1.3*max(tempVec2))
    }
  
  if(DB)
    print(paste("ylim:", ylim))
  

  effect = effect[order(abs(effect), na.last = TRUE, decreasing = decreasing)]
  effect = round(effect, 3)

  if(abs)
  {
    xVals = barplot(abs(effect), las = 2, main = main, xlab = xlab, ylim = ylim, ylab = ylab, ...)
    if(length(xVals) >= 1)
      for(i in 1:length(xVals))
      {
       text(xVals[i], abs(effect[i] + max(ylim)*sign(effect[i])*0.05), format(effect[i]))
      }
  }
  else
  {
    xVals = barplot(effect, las = 2, main = main, xlab = xlab, ylim = ylim, ylab = ylab, ...)
    if(length(xVals) >= 1)
      for(i in 1:length(xVals))
      {
       text(xVals[i], effect[i] + max(ylim)*sign(effect[i])*0.05, format(effect[i]))
      }
  }

  
 

  myDelta = diff(range(ylim))*0.02

  try(abline(h = sig.pos, col = "red"), silent = TRUE) #plot the significance line
  try(axis(4, at = c(sig.pos, sig.neg), labels = round(c(sig.pos, sig.neg),3), las = 2), silent = T)
  try(abline(h = sig.neg, col = "red"), silent = TRUE) #plot the significance line

  }

  legend(location, legend = names(fdo), pch = paste(names(names(fdo)), sep = ""), bg = "white", inset = 0.02)

  abline(h = 0)  
  box()
  
    
  }

  
  if(DB)
  {
    print(df.resid)
    print(num.c)
    print(effect)
  }

  
  }

  invisible(effect.list)
}


class(paretoPlot) <- "invisible"


#test = fracDesign(k = 4, replicates = 1, centerCube = 1)
#names(test) = c("Faktor 1", "Faktor 2", "Faktor 3", "Faktor 4")
#response(test) = data.frame(y1 = round(rnorm(17, 12, 3),5), y2 = round(rnorm(17,10, 0.01),3))
#paretoPlot(test, abs = T, DB = TRUE)
#paretoPlot(test, abs = F, DB = TRUE)
#
##paretoPlot(test, abs = T, alpha = NULL)
##



#t2 = fracDesign(k = 4, replicates = 1, centerCube = 0)
#response(t2) = rnorm(16)
#paretoPlot(t2, abs = T)
#paretoPlot(t2, abs = F)
#


#t3 = fracDesign(2, replicates = 2)
#response(t3) = 1:8
