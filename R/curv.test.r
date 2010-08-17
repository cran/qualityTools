#only if there's a factorial design with additional center Points
#this one is using analysis of variance --> p-Value for quadratic effect is being extracted
#regarding that only one quadratic effect will be fitted with centerCube Points
.curvTest = function(fdo, response, DB = FALSE)
{ 
  fullString = character(0) #holds the full model for a factorial design with additional center Points
  interString = character(0) #holds the interaction strings
  quadString = character(0) #holds the quadratic strings
  quadString2 = character(0) # holds the concatenated quadratic terms for pmatch use
  y = character(0)
  nameVec = names(names(fdo))
  pValue = NA               #
  

#  if(deparse(substitute(response)) %in% names(response(fdo)))
#  if(length(strsplit(deparse(substitute(response)), "\"")[[1]]) == 1)
  if(!(try(is.character(response), silent = TRUE) == TRUE))
  {
    y = deparse(substitute(response))
  }
  else
    if(response %in% names(response(fdo)))
      y = response
    

  if(length(nameVec) < 1)
  {
    cat("\n")
    invisible("curvTest: not enough names (factors)!")
  }
  
  if(nrow(centerCube(fdo)) <= 1)
  {
    cat("\n")
    invisible("curvTest: not enough centerPoints for a test for curvature")
  }
  
  if(nrow(star(fdo)) > 0)
  {
    cat("\n")
    invisible("curvTest: star portion exists; nothing to do")  
  }
  
  
  if(length(nameVec) == 1)
  {
    cat("\n")
    invisible(nameVec[1])
  }
  
  #paste together all single effects
  for(i in seq(along = nameVec))
  {
    if(i == 1)
    {
      fullString = nameVec[i]
      if(DB)
        print(fullString)
    }
      
    if(length(nameVec) >= 2)
    {
      fullString = paste(fullString,"+",nameVec[i+1])
    if(DB)
      print(fullString)
    }
      
    if((i+1) >= length(nameVec))
    {
      if(DB)
        print("break")
      break;
    }
  }
  

  #paste together all interactions up to a specific degree (usually two way interactions
  for(k in 2:length(nameVec))
  {
    if(DB)
      print(k)
    temp = combn(nameVec, k, simplify = TRUE) 
    if(DB)
      print(temp)
    
    for(i in 1:ncol(temp))
    {
      interString = character(0)
      for(j in 1:k)
      {
        if(j == 1)
          interString = temp[j,i]
        else   
         interString = paste(interString, ":", temp[j,i])
      }  
      fullString =  paste(fullString,"+", interString)
    }
  } 
#return(fullString)    

  #paste together all quadratic effects
  for(i in seq(along = nameVec))
  {
    if(i == 1)
    {
      quadString = paste("I(",nameVec[i],"^2)", sep = "")
      quadString2 = paste("I(",nameVec[i],"^2)", sep = "")
      if(DB)
          print(quadString)
    }
      
    if(length(nameVec) >= 2)
    {
      quadString = paste(quadString,"+I(",nameVec[i+1],"^2)", sep = "")
      quadString2 = c(quadString2,paste("I(",nameVec[i+1],"^2)", sep = ""))
      if(DB)
      {
        print(quadString2)
        print(quadString)
      }
    }
      
    if((i+1) >= length(nameVec))
    {
      if(DB)
        print("break")
      break;
    }
  }
  fullString = paste(fullString, "+", quadString)
  

  

fullString = paste(y,"~", fullString)
if(DB)
  print(fullString)

aov.1 = aov(formula = as.formula(fullString), data = fdo)
if(DB)
{
  print(summary(aov.1))
  print(pmatch(quadString2, row.names(summary(aov.1)[[1]])))
}

rows = pmatch(quadString2, row.names(summary(aov.1)[[1]]))
if(DB)
  print(rows)
rows = row.names(summary(aov.1)[[1]])[rows[!is.na(rows)]]
if(DB)
  print(rows)

cols = "Pr(>F)"
tempFrame = data.frame(summary(aov.1)[[1]][rows,cols])


#original
if(nrow(tempFrame) > 0)
{
  row.names(tempFrame) = rows
  names(tempFrame) = cols
  pValue = format(tempFrame[1,1], digits = 3)
}
  

#to be printed
out = paste("Test for Curvature:  p =", pValue) 
cat("\n")
cat(out)
cat("\n")
invisible(pValue)
}

##Example from Mongomery example S14-2
#fdo = facDesign(k = 2, centerCube = 5)
#fdo = randomize(fdo, 44, TRUE)
#response(fdo) = c(39.3, 40.9, 40.0, 41.5, 40.3, 40.5, 40.7,  40.2, 40.6)
#names(response(fdo)) = "yield"
#fits(fdo) = lm(yield ~ A*B + I(A^2) + I(B^2), data  = fdo)
#curvTest(fdo, yield)


##another example checking for 1
#fdo = facDesign(k = 2, centerCube = 2)
#fdo = randomize(fdo, 44, TRUE)
#response(fdo) = c(39.3, 40.9, 40.0, 41.5, 45.3, 46.5)
#names(response(fdo)) = "yield"
#curvTest(fdo, yield)
#fits(fdo) = lm(yield ~ A*B + I(A^2) + I(B^2), data  = fdo)
#



#displays a summary  and the regression equation in non codec form for each fit 
#NOTE: if there's no fit nothing will be displayed
summaryFits = function(fdo, lmFit = TRUE, curvTest = TRUE, origFit = TRUE)
{
  summaryList = vector(mode = "list", length = 0)
  #data.frame mit uncodierten Werten erstellen
  origFrame = as.data.frame(fdo)
    for(i in names(names(fdo)))
      origFrame[,i] = code2real(lows(fdo)[[i]], highs(fdo)[[i]], origFrame[ ,i])
  
#  print(origFrame)

  for(f in names(response(fdo)))
  {
    if(!is.null(fits(fdo)[[f]]))
    {
      cat(paste("----------- Summary for response '", f,"' -----------", sep = ""))      
      cat("\n")      
      print(summary(fits(fdo)[[f]]))
      cat("-----------")
      cat("\n")
      cat("\n")
      cat("Regression in non coded form:")
      cat("\n")      
      lm.f = (lm(formula(fits(fdo)[[f]]), data = origFrame))
      coefs = coefficients(lm.f)
  
      #transform the coefficients vector into a readable form
      coefsI = coefs[pmatch("(Intercept)", names(coefs))]    
      coefsW = coefs[-pmatch("(Intercept)", names(coefs))]  #remove Intercept    
      coefsW = coefsW[!is.na(coefsW)] #remove NA's
      
      temp = character(length(coefsW))
      temp[coefsW >= 0] = "+"
      temp[coefsW < 0] = "-"
      
      firstString = ""
      firstString = paste(firstString, format(coefsI, digits = 4))
      restString = paste(format(abs(coefsW), digits = 4), names(coefsW), sep = "*")
      restString = paste(temp, restString)
      restString = paste(restString, collapse = " ")
  
      fullString = paste(firstString, restString)
      fullString = paste(paste(f," ="), fullString)
  
      cat("\n")      
      cat(paste("  ",fullString))
      cat("\n")
      cat("\n")

     
      cat("-----------")
      cat("\n")
      .curvTest(fdo, f)
      cat("\n")
      cat("\n")
      
    }
    
  }
  invisible()
}

#lows(fdo) = c(20, 40)
#highs(fdo) = c(40, 60)
#summaryFits(fdo)
#






























