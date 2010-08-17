##Complete source for DAP Package
#doeFactor - Klasse
#low setting - realer Wert des Faktors auf der hohen Stufe - character da eine hohe Stufe z.B. auch "Bauteil B" sein kann. character koennen aber in numerics umgewandelt werden
#high setting - realer Wert des Faktors auf der niedrigen Stufe
#unit - die Eiheit des Faktors
#type - numerisch oder Faktor   --> dient zur richtigen Ausgabe der Faktoren fuer z.B. lm  i.e. as.factor oder as.numeric bei uebergabe an bestimmte Funktionen
#name - Realer Name des Faktors
setClass("doeFactor",  representation = representation(low = "ANY", high = "ANY", name = "character", unit = "character", type = "character"),
                  prototype = prototype(low = -1, high = 1, name = "", unit = "", type = "numeric"))




#accessor and replacement function low
setGeneric(".low", function(object) standardGeneric(".low"))
setGeneric(".low<-", function(x, value) standardGeneric(".low<-"))
setMethod(".low", "doeFactor", function(object) unlist(object@low))    #low(object) gibt den low Factor zurueck
setReplaceMethod(".low", "doeFactor", function(x, value)
{
  boolOld = is.numeric(.low(x))
 
  #set the value
  x@low <- value

  boolNew = is.numeric(.low(x))
  if(boolNew)
    x@type = "numeric"
  else
    x@type = "factor"

  if(boolOld != boolNew)
    print("Note: The types of the factors were changed!")

 return(x)

}
)

#accessor function replacement high
setGeneric(".high", function(object) standardGeneric(".high"))
setGeneric(".high<-", function(x, value) standardGeneric(".high<-"))
setMethod(".high", "doeFactor", function(object) unlist(object@high))    #low(object) gibt den low Factor zurueck
setReplaceMethod(".high", "doeFactor", function(x, value)
{
  boolOld = is.numeric(.high(x))
 
  #set the value
  x@high <- value

  boolNew = is.numeric(.high(x))
  if(boolNew)
    x@type = "numeric"
  else
    x@type = "factor"

  if(boolOld != boolNew)
    print("Note: The types of the factors were changed!")

 return(x)
}
)


code2real = function(low, high, codedValue)
{
 return((diff(c(low, high))/2)*codedValue + mean(c(low, high)))

}
code2real(10, 20, -1)

#accessor function for type of the factor
setGeneric(".type", function(object) standardGeneric(".type"))
setGeneric(".type<-", function(x, value) standardGeneric(".type<-"))
setMethod(".type", "doeFactor", function(object) object@type)
setReplaceMethod(".type", "doeFactor", function(x, value)
{
  x@type <- value
  x
}
)


#accessor function for the unit of the factor
setGeneric(".unit", function(object) standardGeneric(".unit"))
setGeneric(".unit<-", function(x, value) standardGeneric(".unit<-"))
setMethod(".unit", "doeFactor", function(object) object@unit)
setReplaceMethod(".unit", "doeFactor", function(x, value)
{
 x@unit <- value
 x
}
)


#Generische accessor function names
setMethod("names", "doeFactor", function(x)
{
  x@name
}
)

setReplaceMethod("names", "doeFactor", function(x, value)
{
 x@name <- value
 x
}
)


#Generische show function
setMethod("show", signature(object = "doeFactor"), function(object) {
  cat("Name: ",names(object),"\n")
  cat("low Setting: ",.low(object),"\n")
  cat("high setting: ",.high(object),"\n")
  cat("Unit: ",.unit(object),"\n")
  cat("type: ",.type(object),"\n")
  cat("\n")
  }
)

#HELPING FUNCTION
#nice printing for a doeFactor List
#TODO: get the names of the attributes as shown by the show method for the doeFactor Class for the rownames of the data.frame
.nfp = function(x)
{

  #will be obsolete if validObject is written for facDesign Class
  if(is.list(x) && length(x[[1]]) > 0)
  {
    numAttr = length(attributes(x[[1]]))
    .numFac = length(x)

    frameOut = data.frame(matrix(ncol = .numFac, nrow = (numAttr - 1)))
   for(i in 1:(numAttr-1))
   {
    #lapply(
    #cat("unit:"
    charVec = character(0)
    for(j in 1:.numFac)
    {
      charVec = c(charVec,names(attributes(x[[1]])[i]), "\t\t")
      frameOut[i,j] = attributes(x[[j]])[[i]]
    }
#    cat(fac.names(attributes(myList[[1]])[i]),": ",charVec,"\n")

   }

   names(frameOut) = names(x)
   #do not take the class Attribute
   rownames(frameOut) = names(attributes(x[[1]]))[1:(numAttr-1)]

  }
  else
  {
   stop("no list given or length of list < 1")
  }

  print(frameOut)

}


######################################Test of the doeFactor Class############################################
#test = new("doeFactor")
#high(test) = "20"
#low(test) = "10"
#try(validObject(test))
######################################End of the doeFactor Class############################################


######################################Start of the facDesign Class############################################
setClass(Class = "facDesign", representation = representation(name = "character", factors = "list", cube = "data.frame", star = "data.frame",
                      centerCube = "data.frame", centerStar = "data.frame", generator = "ANY", response = "data.frame", block = "data.frame", blockGen = "data.frame", runOrder = "data.frame", standardOrder = "data.frame", desireVal = "list", desirability = "list", fits = "list")
                      )
                      
                      
######Desire Integration#####
#Dies kommt an das Ende der Datei

setGeneric("fits", function(x) standardGeneric("fits"))
setGeneric("fits<-", function(x, value) standardGeneric("fits<-"))
setMethod("fits", "facDesign", function(x)
{
 return(x@fits)
}
)

setMethod("fits<-", "facDesign", function(x,value)
{
  if(!identical(class(value), "lm"))
    stop(paste(deparse(substitute(value)), "needs to an object of class lm"))
    
  if(!any(names(value$model)[1] == names(response(x))))
    stop(paste("fitted response", names(value$model)[1], "could not be found in", deparse(substitute(x))))

  listPos = length(x@fits) + 1  
  yName = names(value$model)[1]
  #falls model schon in der Liste --> ersetzen d.h. listPos anpassen
  isIn = (yName == names(x@fits))
  if(any(isIn))
    listPos = (1:length(names(x@fits)))[isIn]
  
  
  
  x@fits[[listPos]] = value
  names(x@fits)[listPos] = yName
    
  x
}
)


setGeneric("desires", function(x) standardGeneric("desires"))
setGeneric("desires<-", function(x, value) standardGeneric("desires<-"))
setMethod("desires", "facDesign", function(x)
{
 return(x@desirability)
}
)

setMethod("desires<-", "facDesign", function(x,value)
{
#  if(!any(names(value$model)[1] == names(response(x))))
#    stop(paste("fitted response", names(lm.1$model)[1], "could not be found in", deparse(substitute(x))))

  #value is a desirability object
  if(!any(value@response == names(response(x))))
    stop(paste(value@response, "is not a response!"))    
  
  
  

  listPos = length(x@desirability) + 1  
  yName = value@response
  #falls model schon in der Liste --> ersetzen d.h. listPos anpassen
  isIn = (yName == names(x@desirability))
  if(any(isIn))
    listPos = (1:length(names(x@desirability)))[isIn]
  
  x@desirability[[listPos]] = value
  names(x@desirability)[listPos] = yName
    
  x
}
)







######Desire Integration Ende#####
                      


#generische accessor Methoden fuer nrow und ncol
setMethod("nrow", "facDesign", function(x) nrow(as.data.frame(x)))
setMethod("ncol", "facDesign", function(x) ncol(as.data.frame(x)))

#Methode fuer Anzahl der Faktoren
.numFac = function(fdo)
{
 return(length(names(fdo)))

}


#TODO: helper function to clear all slots of the fdo object
setGeneric(".clear", function(x) standardGeneric(".clear"))
setMethod(".clear", "facDesign", function(x)
{
 x@standardOrder = data.frame()
 x@runOrder = data.frame()
 x@cube = data.frame()
 x@centerStar = data.frame()
 x@centerCube = data.frame()
 x@star = data.frame()
 x@block = data.frame()
 x@blockGen = data.frame()
 x@response = data.frame()
 return(x)
}

)



#generische accessor Methoden fuer [
#setMethod("[",  signature(x = "facDesign", i = "ANY", j = "ANY", drop = "missing"),
#                function(x,i,j, drop)
setMethod("[",  signature(x = "facDesign", i = "ANY", j = "ANY"),
                function(x,i,j)

                {
#                  iIntern =  order(runOrd(x))[i]
#                  return(as.data.frame(x)[iIntern,j])
                   return(as.data.frame(x)[i,j]) 
                }
          )


#sorts  a facDesign object after standardOrder or RunOrder
#erstmal nur um nach runOrder zu sortieren
.helpSort = function(fdo, runOrd = TRUE)
{
  oldIndex = standOrd(fdo)
  newIndex = order(runOrd(fdo))
  out = fdo
  cube(out) = oldIndex[newIndex[1:nrow(cube)]]
}          
          
setReplaceMethod("[", signature(x = "facDesign", i = "ANY", j = "ANY"),
                function(x, i, j, value)
                {
                  print(paste("setting values via [",i,",",j," ]  is not supported"))
                  print("Use response() to set the values of the response!")
                  return(x)
                
                  #if(class(value) == "fdo")
                  #  print("TODO: replace parts or whole design, if different designs --> Print a Hint!")
                  
                  if(class(value) == "data.frame")
                    if(dim(as.data.frame(x)) == dim(as.data.frame(value)))
                    {
                      #TODO: validObject
                      #.centerPoints should be zero
                      #starPoints should contain star points i.e. zeros and one alpha value per row
                      #cube Points should consist of  -1 an +1 only
                      #a response is likely to have more than 2 or 5 distinct values
                      #-1 und +1 koennen auch real kodierte Werte sein.
                      print("data.frame in einzelne Slots umkopieren")
                    }
                  
                  if(!(missing(i) & missing(j)))
                  {
                     print("not supported")                  
                  }
                      
                  if(missing(j))
                  {
                    print("ncol must be the same")
                    
                    #validObject(x)
                    return(x)
                  }

                  if(missing(i))
                  {
                    print("nrow must be the same")
                    
                    #validObject(x)
                    return(x)
                  }    
                  
                  #i and j are given
                  
                  
                 return(x)
                }
          )


#setzt die types der Faktoren eines facDesigns numeric/factor
setGeneric("types", function(x) standardGeneric("types"))
setGeneric("types<-", function(x, value) standardGeneric("types<-"))
setMethod("types", "facDesign", function(x)
{
 return(sapply(factors(x), .type))
}
)

setReplaceMethod("types", "facDesign", function(x,value)
{
  for(i in 1:length(x@factors))
  {
    if(!identical(value[i],"numeric") & !identical(value[i],"factor"))
      stop(paste(value[i],"\ttype of factor needs to be 'numeric' or 'factor'"))

    .type(x@factors[[i]]) = as.character(value[i])

  }

  x
}
)


#generische accessor Funktion fuer die units der Faktoren (z.B. mm oder ml)
#setGeneric("units", function(x) standardGeneric("units"))
setMethod("units", "facDesign", function(x)
{
 return(sapply(factors(x), .unit))
}
)
#setGeneric("units", function(x, value) standardGeneric("units"))
setMethod("units<-", "facDesign", function(x,value)
{
   for(i in 1:length(x@factors))
   if(length(value) > 1)
      .unit(x@factors[[i]]) = as.character(value[i])
    else
      .unit(x@factors[[i]]) = as.character(value[1])

  x
}
)


#setzt die high levels der Faktoren
setGeneric("highs", function(object) standardGeneric("highs"))
setGeneric("highs<-", function(object, value) standardGeneric("highs<-"))
setMethod("highs", "facDesign", function(object)
{
  
  listOut = vector(mode = "list")
  for(i in names(factors(object)))
  {
    listOut[i] = .high(object@factors[[i]])
  }
  return(listOut)
}
)

setReplaceMethod("highs", "facDesign", function(object,value)
{
  for(i in seq(along = object@factors))
  {
    .high(object@factors[[i]]) = value[i]
  }

  return(object)
}
)



#setzt die low levels der Faktoren
setGeneric("lows", function(object) standardGeneric("lows"))
setGeneric("lows<-", function(object, value) standardGeneric("lows<-"))
setMethod("lows", "facDesign", function(object)
{
  listOut = vector(mode = "list")
  for(i in names(factors(object)))
  {
    listOut[i] = .low(object@factors[[i]])
  }

  return(listOut)
}
)

setReplaceMethod("lows", "facDesign", function(object,value)
{
  for(i in seq(along = object@factors))
  {
    .low(object@factors[[i]]) = value[i]
  }

  return(object)
}
)



#setzt und gibt den cube des facDesign objects zurueck
#TODO: Falls es Werte fuer die Response gibt, diese mit zurueckgeben
setGeneric("cube", function(x) standardGeneric("cube"))
setMethod("cube", "facDesign", function(x)
{
  out = x@cube
  out
}
)
setGeneric("cube<-", function(x, value) standardGeneric("cube<-"))
setReplaceMethod("cube", "facDesign", function(x, value)
{
 x@cube <- value
 x
}
)

#setzt und gibt den star des facDesign objects zurueck
setGeneric("star", function(x) standardGeneric("star"))
setGeneric("star<-", function(x, value) standardGeneric("star<-"))
setMethod("star", "facDesign", function(x) x@star)    #low(object) gibt den low Factor zurueck
#setReplaceMethod("star", "facDesign", function(x, value)
#{
# x@star <- value
# x
#}
#)


setReplaceMethod("star", "facDesign", function(x, value)
{
  
  DB = FALSE
  
  if(!is.data.frame(value))
    stop("data.frame must be provided!")
  
  if(.numFac(x) != ncol(value))
    stop("number of columns not matching!")
    
  if(nrow(value) == 0)
  {
    return("TODO: remove star und Rest anpassen")
  }
  
  oldResponse = response(x)
  newDf = value
  oldDf = x@star
  
  numNewRow = nrow(newDf) - nrow(oldDf)
  oldOrd = standOrd(x)
  oldRunOrd = runOrd(x)
  len  = nrow(oldOrd)
  lenFirst = nrow(cube(x)) + nrow(centerCube(x))

  #standOrd(x) anpassen
  standOrd(x) = data.frame(StandOrd = 1:(len + numNewRow))
  
  newRunOrd = data.frame()
  #runOrd(x) anpassen
  if(numNewRow > 0)
  {
   newNums = data.frame(newNums = seq(max(oldRunOrd) + 1, max(oldRunOrd) + numNewRow, by = 1))  
   
   if(DB)
     print(newNums)
   
   names(newNums) = names(oldRunOrd)
   newRunOrd = data.frame(oldRunOrd[1:lenFirst,])
  
   if(DB) 
     print(newRunOrd)
     
   names(newRunOrd) = names(oldRunOrd)
   restFrame = data.frame(oldRunOrd[-c(1:lenFirst),])
   names(restFrame) = names(oldRunOrd)
#   newRunOrd = rbind(newRunOrd, newNums, oldRunOrd[-c(1:lenFirst),])
   newRunOrd = rbind(newRunOrd, newNums, restFrame)
   
   if(DB)
     print(newRunOrd)
   
   
  }
  else
  {
#    newRunOrd = data.frame(oldRunOrd[1:(lenFirst+nrow(newDf)),])
    newRunOrd = data.frame(oldRunOrd[1:(lenFirst+nrow(newDf)+nrow(centerStar(x))),])
    names(newRunOrd) = names(oldRunOrd)
  }
  
  runOrd(x) = newRunOrd

  #response(x) anpassen
  naFrame = as.data.frame(matrix(rep(NA, times = ncol(oldResponse)*nrow(newDf)), ncol = ncol(oldResponse)))
  names(naFrame) = names(oldResponse)

  newResponse = data.frame(oldResponse[1:lenFirst,])
  names(newResponse) = names(oldResponse)
  
  restFrame = data.frame(oldResponse[-c(1:(lenFirst + nrow(oldDf))),])
  names(restFrame) = names(oldResponse)
  
  newResponse = rbind(newResponse, naFrame, restFrame)
  response(x) = newResponse
  
  if(DB)
  {  
    print(newResponse)
    print("hinter response")
  }



  #blockGen anpassen
  oldBlockGen = blockGen(x)
  if(ncol(oldBlockGen) > 0)
  {
    if(DB)
      print("TODO: BlockGen anpassen!")
    
    newBlockGen = data.frame(oldBlockGen[1:lenFirst,])
    names(newBlockGen) = names(blockGen(x))
    naFrameGen = as.data.frame(matrix(rep(NA, times = ncol(blockGen(x))*nrow(newDf)), ncol = ncol(blockGen(x))))
    names(naFrameGen) = names(oldBlockGen)
    
    restBlockGen = data.frame(oldBlockGen[-c(1:(lenFirst + nrow(oldDf))),])
    names(restBlockGen) = names(oldBlockGen)
    
    newBlockGen = rbind(newBlockGen, naFrameGen, restBlockGen)

    if(DB)
      print(newBlockGen)
      
    blockGen(x) = newBlockGen
  }

  #Block anpassen
  oldBlock = block(x)
  newBlock = data.frame(oldBlock[1:lenFirst,])
  names(newBlock) = names(oldBlock)
  naFrame = as.data.frame(matrix(rep(max(newBlock) + 1, times = ncol(oldBlock)*nrow(newDf)), ncol = ncol(oldBlock)))
  names(naFrame) = names(oldBlock)
  
  restBlock = data.frame(oldBlock[-c(1:(lenFirst + nrow(oldDf))),])
  names(restBlock) = names(oldBlock)
  
  newBlock = rbind(newBlock, naFrame, restBlock)
  block(x) = newBlock
  
  #star speichern
  x@star <- newDf
  return(x)
}
)

#fdo = facDesign(k = 3)
#star(fdo) = data.frame(A = c(0,0), B = c(0,0), C = c(0,0))
#star(fdo) = data.frame(A = c(0,0,0,0,0), B = c(0,0,0,0,0), C = c(0,0,0,0,0))
#
#fdo = facDesign(k = 3)
#fdo = blocking(fdo, 4)
#centerCube(fdo) = data.frame(A = c(0,0), B = c(0,0), C = c(0,0))
#centerCube(fdo) = data.frame(A = c(0,0,0), B = c(0,0,0), C = c(0,0,0))
#temp = .starFrame(3, 2)
#names(temp) = names(cube(fdo))
#star(fdo) = temp





#setzt und gibt die .centerPoints des facDesign objects zurueck
setGeneric("centerCube", function(x) standardGeneric("centerCube"))
setGeneric("centerCube<-", function(x, value) standardGeneric("centerCube<-"))
setMethod("centerCube", "facDesign", function(x) x@centerCube)    #low(object) gibt den low Factor zurueck
setReplaceMethod("centerCube", "facDesign", function(x, value)
{
  DB = FALSE #Debugging Option
  
  if(!is.data.frame(value))
    stop("data.frame must be provided!")
  
  if(.numFac(x) != ncol(value))
    stop("number of columns not matching!")
    
  if(nrow(value) == 0)
  {
    return("TODO: remove CenterCube und Rest anpassen")
  }


  newDf = value
  lenCube = nrow(cube(x))
  oldDf = x@centerCube
  oldRunOrd = runOrd(x)
  oldResponse = response(x)

  blockValues = unique(block(x)[1:nrow(cube(x)),])
  numBlocks = length(blockValues)
#  if(numBlocks > 1)
#  {
#    if(nrow(star(x)) > 0)  
#      numBlocks = numBlocks - 1
#  }
#  
  if(numBlocks > 1)
    for(i in 1:(numBlocks-1))  
    {
      newDf = rbind(newDf, value)
    }
  
  if(DB)
    print(newDf)

  
  
    
  numNewRow = nrow(newDf) - nrow(oldDf)
  oldOrd = standOrd(x)
  len  = nrow(oldOrd)
  
  #standOrd(x) anpassen
  standOrd(x) = data.frame(StandOrd = 1:(len + numNewRow))
  
  #runOrd(x) anpassen
  newRunOrd = data.frame()
  if(numNewRow > 0)
  {
   newNums = data.frame(newNums = seq(max(oldRunOrd) + 1, max(oldRunOrd) + numNewRow, by = 1))  
   names(newNums) = names(oldRunOrd)
    
   if(DB)
   { 
     print("----")
     print(newNums)
   }
   
   newRunOrd = data.frame(oldRunOrd[1:lenCube,])
   names(newRunOrd) = names(oldRunOrd)
   restRunOrd = data.frame(oldRunOrd[-c(1:lenCube),])
   names(restRunOrd) = names(oldRunOrd)
   newRunOrd = rbind(newRunOrd, newNums, restRunOrd)
   
   if(DB)
   {
    print("----")
    print(oldRunOrd[-c(1:lenCube),])
    print("----")
    print(newRunOrd)
   }

   runOrd(x) = newRunOrd
  }  
  else
  {
    newRunOrd = data.frame(oldRunOrd[1:(lenCube + nrow(newDf)),])  
    names(newRunOrd) = names(oldRunOrd)
    
    restRunOrd = data.frame(oldRunOrd[-c(1:(lenCube + nrow(oldDf))),])
    names(restRunOrd) = names(oldRunOrd)
    newRunOrd = rbind(newRunOrd, restRunOrd)
    if(DB)
    {
      print("----")
      print(newRunOrd)    
    }
    runOrd(x) = newRunOrd
  }
  
  
  #response(x) anpassen
  naFrame = as.data.frame(matrix(rep(NA, times = ncol(oldResponse)*nrow(newDf)), ncol = ncol(oldResponse)))
  names(naFrame) = names(oldResponse)

  newResponse = data.frame(oldResponse[1:lenCube,])
  names(newResponse) = names(oldResponse)
  
  restResponse = data.frame(oldResponse[-c(1:(lenCube + nrow(oldDf))),])
  names(restResponse) = names(oldResponse)
  
  newResponse = rbind(newResponse, naFrame, restResponse) #TODO: Index 
  
  if(DB)
  {
    print("newResponse_____")
    print(newResponse)
  }
  
  response(x) = newResponse

  
  #TODO: BlockGen Spalte(n) anpassen
  oldBlockGen = blockGen(x)
  if(ncol(oldBlockGen) > 0)
  {
    if(DB)
      print("TODO: BlockGen Spalte(n) anpassen")
      
    newBlockGen = data.frame(oldBlockGen[1:lenCube,])
    names(newBlockGen) = names(blockGen(x))
    naFrameGen = as.data.frame(matrix(rep(NA, times = ncol(blockGen(x))*nrow(newDf)), ncol = ncol(blockGen(x))))
    names(naFrameGen) = names(oldBlockGen)
    restFrame = data.frame(oldBlockGen[-c(1:(lenCube + nrow(oldDf))),])
    names(restFrame) = names(blockGen(x))
    newBlockGen = rbind(newBlockGen, naFrameGen, restFrame )
    blockGen(x) = newBlockGen

    if(DB)
      print(newBlockGen)
  }
  
  #block anpassen
  oldBlock = block(x)
  newBlock = data.frame(oldBlock[1:lenCube,])
  names(newBlock) = names(block(x))
#  naFrame = as.data.frame(matrix(rep(NA, times = ncol(block(x))*nrow(newDf)), ncol = ncol(block(x))))
  naFrame = as.data.frame(matrix(rep(blockValues, times = nrow(newDf)/numBlocks), ncol = 1))
  
  restFrame = as.data.frame(oldBlock[-c(1:(lenCube + nrow(oldDf))),])
  names(restFrame) = names(block(x))
  
  if(DB)  
    print(naFrame)
  
  names(naFrame) = names(oldBlock)
  newBlock = rbind(newBlock, naFrame, restFrame )

  block(x) = newBlock

  #centerCube speichern  
  x@centerCube <- newDf
  return(x)
}
)

#fdo = facDesign(k = 3)
#fdo = blocking(fdo, 2)
#centerCube(fdo) = data.frame(A = c(0,0), B = c(0,0), C = c(0,0))
#centerCube(fdo) = data.frame(A = c(0,0,0), B = c(0,0,0), C = c(0,0,0))
#temp = .starFrame(3, 2)
#names(temp) = names(cube(fdo))
#star(fdo) = temp


#setzt und gibt die .centerPoints des facDesign objects zurueck
setGeneric("centerStar", function(x) standardGeneric("centerStar"))
setGeneric("centerStar<-", function(x, value) standardGeneric("centerStar<-"))
setMethod("centerStar", "facDesign", function(x) {x@centerStar
}
)

#setReplaceMethod("centerStar", "facDesign", function(x, value)
#{
# x@centerStar <- value
# x
#}
#)

setReplaceMethod("centerStar", "facDesign", function(x, value)
{
  DB = FALSE  #DEBUGGING option
  
  if(!is.data.frame(value))
    stop("data.frame must be provided!")
  
  if(.numFac(x) != ncol(value))
    stop("number of columns not matching!")
    
  if(nrow(value) == 0)
  {
    return("TODO: remove CenterCube und Rest anpassen")
  }

  newDf = value
  oldDf = x@centerStar
  numNewRow = nrow(newDf) - nrow(oldDf)
  
  oldResponse = response(x)
  lenRest = nrow(cube(x)) + nrow(centerCube(x)) + nrow(star(x))

  oldRunOrd = runOrd(x)
  oldOrd = standOrd(x)
  len  = nrow(oldOrd)
  
  #standOrd(x) anpassen
  standOrd(x) = data.frame(StandOrd = 1:(len + numNewRow))
  
  #runOrd(x) anpassen
  newRunOrd = data.frame(oldRunOrd[1:lenRest,]) #alles ausser centerStar
  names(newRunOrd) = names(oldRunOrd)

  if(numNewRow > 0)
  {
   newNums = data.frame(newNums = seq(max(oldRunOrd) + 1, max(oldRunOrd) + numNewRow, by = 1))
   names(newNums) = names(oldRunOrd)
   restFrame = data.frame(oldRunOrd[-c(1:lenRest),])
   names(restFrame) = names(oldRunOrd)
   newRunOrd = rbind(newRunOrd, newNums, restFrame)
   
   if(DB)
     print(newRunOrd)
     #print(newNums)
     
   runOrd(x) = newRunOrd
   
  }  
  else
  {
    newRunOrd = data.frame(oldRunOrd[1:(lenRest + nrow(newDf)),])
    names(newRunOrd) = names(oldRunOrd)
    runOrd(x) = newRunOrd
  }

   #print(newRunOrd)

  #response(x) anpassen
  naFrame = as.data.frame(matrix(rep(NA, times = ncol(oldResponse)*nrow(newDf)), ncol = ncol(oldResponse)))
  names(naFrame) = names(oldResponse)
  newResponse = data.frame(oldResponse[1:lenRest,])
  names(newResponse) = names(response(x))
  newResponse = rbind(newResponse, naFrame)
  
  if(DB)
    print(" vor centerStar response")
  
  response(x) = newResponse
  
  if(DB)
    print("hinter centerStar response")
  


  #BlockGen Spalte anpassen
  oldBlockGen = blockGen(x)
  if(ncol(oldBlockGen) > 0)
  {
    print("TODO: BlockGen Spalte(n) anpassen")
    newBlockGen = data.frame(oldBlockGen[1:lenRest,])
    names(newBlockGen) = names(blockGen(x))
    naFrameGen = as.data.frame(matrix(rep(NA, times = ncol(blockGen(x))*nrow(newDf)), ncol = ncol(block(x))))
    names(naFrameGen) = names(oldBlockGen)
    
    restBlockGen = data.frame(oldBlockGen[-c(1:(lenRest + nrow(oldDf))),])
    names(restBlockGen) = names(oldBlockGen)
    
    newBlockGen = rbind(newBlockGen, naFrameGen, restBlockGen)
    
    if(DB)
      print(newBlockGen)
      
    blockGen(x) = newBlockGen
  }

  #Block Spalte anpassen
  oldBlock = block(x)
  #print(oldBlock)

  newBlock = data.frame(oldBlock[1:lenRest,])
  names(newBlock) = names(block(x))
  #print(newBlock)
  
  naFrame = as.data.frame(matrix(rep(max(block(x)[1:nrow(cube(x)),]) + 1, times = ncol(block(x))*nrow(newDf)), ncol = ncol(block(x))))
  names(naFrame) = names(oldBlock)
  #print(naFrame)
  
  restBlock = data.frame(oldBlock[-c(1:(lenRest + nrow(oldDf))),])
  names(restBlock) = names(oldBlock)
  
  newBlock = rbind(newBlock, naFrame, restBlock)
  #print(newBlock)
  
  block(x) = newBlock
  
  #centerstar abspeichern
  x@centerStar <- newDf
  x
}
)

#fdo = facDesign(k = 3)
#fdo = blocking(fdo, 2)
#centerCube(fdo) = data.frame(A = c(0,0), B = c(0,0), C = c(0,0))
#centerCube(fdo) = data.frame(A = c(0,0,0), B = c(0,0,0), C = c(0,0,0))
#temp = .starFrame(3, 2)
#names(temp) = names(cube(fdo))
#star(fdo) = temp
#
#
#fdo = facDesign(k = 3, block = 4)
#centerStar(fdo) = data.frame(A = c(0,0,0,0), B = c(0,0,0,0), C = c(0,0,0,0))
#star(fdo) = data.frame(A = c(0,0,0,0), B = c(0,0,0,0), C = c(0,0,0,0))
#centerStar(fdo) = data.frame(A = c(0,0,0,0,0), B = c(0,0,0,0,0), C = c(0,0,0,0,0))






#setzt und gibt den response Teil des Versuchsplans zurueck
setGeneric("response", function(object){standardGeneric ("response")})    #low(object) gibt den low Factor zurueck
setGeneric("response<-", function(object, value){ standardGeneric("response<-")})
#setMethod("response", "facDesign", function(object){ return(object@response)})    #low(object) gibt den low Factor zurueck
setMethod("response", "facDesign", function(object)
                              {
                              iIntern =  order(runOrd(object))  
                              out = data.frame(object@response[iIntern,])
                              names(out) = names(object@response)
                              
                              return(out)
                              }
          )
setReplaceMethod("response", "facDesign", function(object, value)
{
 #TODO
 #print("setting the response via response")
 
 index = order(runOrd(object))

 if(!is.vector(value) && !is.data.frame(value))
  stop("vector or data.frame expected!")

 if(is.vector(value) && (is.numeric(value) || is.na(value)))
 {
 # print(nrow(object))
  if(nrow(response(object)) != length(value))
    stop(paste("Number of rows for Design does not equal length of vector ", nrow(object), " != ", length(value)," "))

#  if(ncol(response(object)) >1)
#    stop("More than one response --> use response(x)[ ] instead!")
    
  object@response <- data.frame(value)
  object@response[index,] <- value
  names(object@response) = deparse(substitute(value))   #TODO: check if deparse(substitute(value)) always works
  return(object)
 }

  if(is.data.frame(value))
  {
#    object@response[index, ] <- value
    object@response <- value
    object@response[index,] <- value
    object
  }
#
# check if the object remains valid
# cat("\nTODO: check validity of object!\n")
 return(object)
}
)

#Einfuegen der blockGen Spalten
#Block Spalte selbst wird extra eingefuegt
setGeneric("blockGen", function(object){standardGeneric ("blockGen")})    #low(object) gibt den low Factor zurueck
setGeneric("blockGen<-", function(object, value){ standardGeneric("blockGen<-")})
setMethod("blockGen", "facDesign", function(object){ return(object@blockGen)})    #low(object) gibt den low Factor zurueck
setReplaceMethod("blockGen", "facDesign", function(object, value)
{
 #TODO
 #print("setting the response via response")

 if(!is.vector(value) && !is.data.frame(value))
  stop("vector or data.frame expected!")

 if(is.vector(value) && (is.numeric(value) || is.na(value)))
 {
 # print(nrow(object))
  if(nrow(object) != length(value))
    stop(paste("Number of rows for Design does not equal length of vector ", nrow(object), " != ", length(value)," "))

  object@blockGen <- as.data.frame(value)
  names(blockGen(object)) = deparse(substitute(value))   #TODO: check if deparse(substitute(value)) always works
  object
 }

  if(is.data.frame(value))
  {
#    if(nrow(object) != nrow(value))
#      stop(paste("Number of rows for Design does not equal length of blockGen ", nrow(object), " != ", length(value)," "))

   object@blockGen <- value
   object
  }
#
# check if the object remains valid
# cat("\nTODO: check validity of object!\n")
 return(object)
}
)

setGeneric("block", function(object){standardGeneric ("block")})    #low(object) gibt den low Factor zurueck
setGeneric("block<-", function(object, value){ standardGeneric("block<-")})
setMethod("block", "facDesign", function(object){ return(object@block)})    #low(object) gibt den low Factor zurueck
setReplaceMethod("block", "facDesign", function(object, value)
{
 if(!is.vector(value) && !is.data.frame(value))
  stop("vector or data.frame expected!")

 if(is.vector(value) && (is.numeric(value) || is.na(value)))
 {

  if(nrow(object) != length(value))
    stop(paste("Number of rows for Design does not equal length of vector ", nrow(object), " != ", length(value)," "))

  object@block <- as.data.frame(value)
  names(block(object)) = deparse(substitute(value))   #TODO: check if deparse(substitute(value)) always works
  object
 }

  if(is.data.frame(value))
  {
#    if(nrow(object) != nrow(value))
#      stop(paste("Number of rows for Design does not equal length of blockGen ", nrow(object), " != ", length(value)," "))
#
   object@block <- value
   object
  }
 return(object)
}
)



#gibt die StandardOrder des facDesign objects zurueck
setGeneric("standOrd", function(x) standardGeneric("standOrd"))
setGeneric("standOrd<-", function(x, value) standardGeneric("standOrd<-"))
setMethod("standOrd", "facDesign", function(x) x@standardOrder)    #low(object) gibt den low Factor zurueck
setReplaceMethod("standOrd", "facDesign", function(x, value)
{
 x@standardOrder <- value
 x
}
)

#gibt die RunOrder des facDesign objects zurueck
setGeneric("runOrd", function(x) standardGeneric("runOrd"))
setGeneric("runOrd<-", function(x, value) standardGeneric("runOrd<-"))
setMethod("runOrd", "facDesign", function(x) x@runOrder)    #low(object) gibt den low Factor zurueck
setReplaceMethod("runOrd", "facDesign", function(x, value)
{
 x@runOrder <- value
 x
}
)

#gibt die .generators des facDesign objects zurueck
setGeneric(".generators", function(object) standardGeneric(".generators"))
setGeneric(".generators<-", function(x, value) standardGeneric(".generators<-"))
setMethod(".generators", "facDesign", function(object) object@generator)    #low(object) gibt den low Factor zurueck
setReplaceMethod(".generators", "facDesign", function(x, value)
{
 x@generator <- value
 x
}
)


#gibt die Faktoren eines facDesign objects zurueck
setGeneric("factors", function(x) standardGeneric("factors"))
setMethod("factors", "facDesign", function(x) x@factors)    #low(object) gibt den low Factor zurueck
setGeneric("factors<-", function(x, value) standardGeneric("factors<-"))
setReplaceMethod("factors", "facDesign", function(x, value)
{
 #TODO
 #x@cube <- value
 if(length(value) != dim(cube(x))[2])
   stop("\nNumber of factors doesn't match with number of columns for factorial Design\n")
 x@factors <- value
 x
}
)


#Generische names Funktion fuer ein facDesign object
setMethod("names", "facDesign", function(x)
{
  return(sapply(x@factors, names))
}
)

setReplaceMethod("names", "facDesign", function(x, value)
{
  for(i in 1:length(x@factors))
    names(x@factors[[i]]) = as.character(value[i])

  x
}
)


#Generische as.data.frame Funktion fuer ein facDesign object
setMethod("as.data.frame", "facDesign", function(x, row.names = NULL, optional = FALSE, ...) {

  if(!is.null(cube(x)))
  {
    frameOut = cube(x)
  }
  else
    return(NULL)

  if(!is.null(centerCube))
    frameOut = rbind(frameOut, centerCube(x))

  if(!is.null(star(x)))
    frameOut = rbind(frameOut, star(x))

  if(!is.null(centerStar(x)))
    frameOut = rbind(frameOut, centerStar(x))

  if(!is.null(factors(x)) && length(factors(x)) == dim(frameOut)[2])
  {
    names(frameOut) = as.character(names(names(x)))
  }
  if(nrow(blockGen(x)) > 0)
  {
    frameOut = cbind(blockGen(x), frameOut)
  }
  if(nrow(block(x)) > 0)
  {
   frameOut = cbind(block(x), frameOut)
  }
  
  if(nrow(runOrd(x)) > 0)
  {
   frameOut = cbind(runOrd(x), frameOut)
  }
  
  if(nrow(standOrd(x)) > 0)
  {
   frameOut = cbind(standOrd(x), frameOut)
  }
  
  #finally add the response part if possible
  if(nrow(frameOut) == nrow(response(x)))
    frameOut = cbind(frameOut, x@response)
  else
  {
    temp = as.data.frame(matrix(NA, nrow = nrow(frameOut), ncol = ncol(response(x))))
    names(temp) = names(response(x))
    frameOut = cbind(frameOut, temp)
  }



#  return(data.frame(frameOut))

  runIndex = order(runOrd(x)) 
  out = frameOut[runIndex,]
  
  return(out)

}
)


#gibt den Versuchsplan als data.frame zurueck
#called by lm when assigning an object of class facDesign to data
#setGeneric("as.data.frame.facDesign", function(x, row.names = NULL, optional = FALSE) standardGeneric("as.data.frame.facDesign"))
#setMethod("as.data.frame.facDesign", "facDesign", function(x, row.names = NULL, optional = FALSE) {
#
#  if(!is.null(cube(x)))
#  {
#    frameOut = cube(x)
#  }
#  else
#    return(NULL)
#
#  if(!is.null(centerCube))
#    frameOut = rbind(frameOut, centerCube(x))
#
#  if(!is.null(star(x)))
#    frameOut = rbind(frameOut, star(x))
#
#  if(!is.null(centerStar(x)))
#    frameOut = rbind(frameOut, centerStar(x))
#
#  if(!is.null(factors(x)) && length(factors(x)) == dim(frameOut)[2])
#  {
#    names(frameOut) = as.character(names(names(x)))
#  }
#
#  #finally add the response part if possible
#  if(nrow(frameOut) == nrow(response(x)))
#    frameOut = cbind(frameOut, response(x))
#  else
#  {
#    temp = as.data.frame(matrix(NA, nrow = nrow(frameOut), ncol = ncol(response(x))))
#    names(temp) = names(response(x))
#    frameOut = cbind(frameOut, temp)
#   }
#
#
#  #standardmaeßig nach RunOrder sortiert
#  runIndex = order(runOrd(object)) 
#  return(data.frame(frameOut)[runIndex,])
#}
#)

#setGeneric("as.data.frame.facDesign", function(x, row.names = NULL, optional = FALSE, ...) standardGeneric("as.data.frame.facDesign"))
#setMethod("as.data.frame.facDesign", "facDesign", function(x, row.names = NULL, optional = FALSE,...) {
as.data.frame.facDesign = function(x, row.names = NULL, optional = FALSE,...) {

  if(!is.null(cube(x)))
  {
    frameOut = cube(x)
  }
  else
    return(NULL)

  if(!is.null(centerCube))
    frameOut = rbind(frameOut, centerCube(x))

  if(!is.null(star(x)))
    frameOut = rbind(frameOut, star(x))

  if(!is.null(centerStar(x)))
    frameOut = rbind(frameOut, centerStar(x))

  if(!is.null(factors(x)) && length(factors(x)) == dim(frameOut)[2])
  {
    names(frameOut) = as.character(names(names(x)))
  }
  if(nrow(blockGen(x)) > 0)
  {
    frameOut = cbind(blockGen(x), frameOut)
  }
  if(nrow(block(x)) > 0)
  {
   frameOut = cbind(block(x), frameOut)
  }
  
  if(nrow(runOrd(x)) > 0)
  {
   frameOut = cbind(runOrd(x), frameOut)
  }
  
  if(nrow(standOrd(x)) > 0)
  {
   frameOut = cbind(standOrd(x), frameOut)
  }
  
  #finally add the response part if possible
  if(nrow(frameOut) == nrow(response(x)))
    frameOut = cbind(frameOut, x@response)
  else
  {
    temp = as.data.frame(matrix(NA, nrow = nrow(frameOut), ncol = ncol(response(x))))
    names(temp) = names(response(x))
    frameOut = cbind(frameOut, temp)
  }



#  return(data.frame(frameOut))

  runIndex = order(runOrd(x)) 
  out = frameOut[runIndex,]

  return(frameOut)
}





####same method as as.data.frame.facDesign
setMethod("as.data.frame", "facDesign", function(x, row.names = NULL, optional = FALSE) {

  if(!is.null(cube(x)))
  {
    frameOut = cube(x)
  }
  else
    return(NULL)

  if(!is.null(centerCube))
    frameOut = rbind(frameOut, centerCube(x))

  if(!is.null(star(x)))
    frameOut = rbind(frameOut, star(x))

  if(!is.null(centerStar(x)))
    frameOut = rbind(frameOut, centerStar(x))

  if(!is.null(factors(x)) && length(factors(x)) == dim(frameOut)[2])
  {
    names(frameOut) = as.character(names(names(x)))
  }
  if(nrow(blockGen(x)) > 0)
  {
    frameOut = cbind(blockGen(x), frameOut)
  }
  if(nrow(block(x)) > 0)
  {
   frameOut = cbind(block(x), frameOut)
  }
  
  if(nrow(runOrd(x)) > 0)
  {
   frameOut = cbind(runOrd(x), frameOut)
  }
  
  if(nrow(standOrd(x)) > 0)
  {
   frameOut = cbind(standOrd(x), frameOut)
  }
  
  #finally add the response part if possible
  if(nrow(frameOut) == nrow(response(x)))
    frameOut = cbind(frameOut, x@response)
  else
  {
    temp = as.data.frame(matrix(NA, nrow = nrow(frameOut), ncol = ncol(response(x))))
    names(temp) = names(response(x))
    frameOut = cbind(frameOut, temp)
  }



#  return(data.frame(frameOut))
  runIndex = order(runOrd(x)) 
  out = frameOut[runIndex,]
  return(out)
}
)







#Generische show Funktion fuer ein facDesign object
setMethod("show", signature(object = "facDesign"), function(object) {
  #standardmaeßig nach RunOrder sortiert
  runIndex = order(runOrd(object))
#  print(format(as.data.frame(object)[runIndex,], digits = 4))
 print(format(as.data.frame(object), digits = 4))
  invisible(as.data.frame(object))
  }
)


#Generische summary Funktion fuer ein facDesign object
setMethod("summary", signature(object = "facDesign"), function(object) {

  #TODO
  #if(validObject(object))

  doeFactors = factors(object)
  cat("Information about the factors:\n\n")
  .nfp(doeFactors)
  cat("\n-----------\n")


  #Wuerfel als Ausgangspunkt nehmen
#  frameOut = cube(object)
#
#  if(nrow(centerCube(object)) > 0)
#  {
#   frameOut = rbind(frameOut, centerCube(object))
#  }
#
#  if(nrow(star(object)) > 0)
#  {
#   frameOut = rbind(frameOut, star(object))
#  }
#
#  if(nrow(centerStar(object)) > 0)
#  {
#    frameOut = rbind(frameOut, centerStar(object))
#  }
#
#  names(frameOut) = names(names(object))
#  frameOut = cbind(runOrd(object),frameOut)     #Run Order anfuegen
#  frameOut = cbind(standOrd(object), frameOut)  #Standard order anfuegen
#  
#  runIndex = standOrd(object)[runOrd(object)[,1],]
#  rName = names(response(object))
#  rValue = data.frame(y = response(object)[runIndex,])
#  names(rValue) = rName
#  
#  frameOut = cbind(frameOut,rValue)
#
#  print(frameOut)

  print(as.data.frame(object))

  cat("\n---------\n\n")
  identity(object)
  cat("\n")

#  invisible(frameOut)
  invisible(as.data.frame(object))

  }
)


######################################End of the facDesign Class############################################




######################################Start of the fracDesign and helper functions############################################

#################identity.r####################
#TODO! Was wenn keine Identitaet gefunden werden kann
#TODO! identity fuer data.frames in denen auch Zielgoeßen mit drinnestehen
#identity for 2^k-p factorials with coding -1 nad 1 only
#
#ermittelt die Identitaet eines 2^k-p Versuchsplan
#@return list containing a vector of column indices, the names of the columns for the identity
.identityOld = function(x, DB = FALSE)
{
  identityList = vector(mode = "list", length = 0)  #speichert alle gefundenen Identitaeten in einer Liste

  varName = deparse(substitute(x))
  ident = numeric(0) #temporaerer Identitaetsvektor
  resolution = numeric(0)

  #nur den cube Teil des facDesign objects nehmen
  #etwas generischer: Spalten in denen kein low und high level der Faktoren vorkommen aussortieren
  #                   Zeilen aussortieren in denen mehr als high und low level der Faktoren vorkommen
  x = cube(x)

  #Spalten die andere Werte als -1 und 1 habe aussortieren!
  index = numeric(0)
  for(i in 1:(dim(x)[2]))
  {
      if(!(TRUE && (unique(x[,i]) %in% c(-1,1))))  #in Spalte i befinden sich andere Werte als -1 und 1
        index = c(index, i)
  }
  if(length(index) > 0)
  {
    x = x[,-index]
    cat("Column(s) ", index, " are discarded for analysis\n")
  }

 #Anzahl Spalten
  n = dim(x)[2]
  if(n <= 1)
    stop("Factorial Design contains only one row!")
 a = 0
 #alle Moeglichkeiten m Elemente aus n Elementen zu ziehen
 #werden miteinander multipliziert. Ergeben sich 1er oder -1er Spalten wurde eine Identitaet gefunden
 for(m in 2:n)
 {
  combMat = combn(1:n,m)

  #entsprechende Spalten multiplizieren
  for(i in 1:(dim(combMat)[2]))
  {
    ident = NULL
    temp = x[,combMat[,i]]          #combMat[,i] ist der Vektor der Spaltenindices
    colProd = apply(temp, 1, prod)

    #Identitaet ist gefunden wenn nur 1en oder -1en in der Spalte
    if(length(unique(colProd)) == 1)
    {
      if(DB)
      {
        cat("\n")
        cat("Identitaet gefunden\n")
      }
      if(sum(colProd) < 0)  #negative Identitaet
        ident = -combMat[,i]
      else
        ident = combMat[,i]

      a = a+1
      identityList[[a]] = ident   #Vektor mit den Spalten in die Liste speichern
      charIdentity = character(0)

      for(j in 1:length(ident))
        charIdentity = c(charIdentity,names(x)[ident[j]])

      names(identityList)[[a]] = paste(charIdentity, sep = "", collapse = "")


    }

    if(DB)
    {
      cat("ident: ", ident, "\n")
      print(combMat[,i])
      print(apply(temp, 1, prod))  #zeilenweise multiplizieren
    }
  }

 }

  cat("Defining relations:\n")
  if(length(identityList) > 0)
  {
    for(i in 1:length(identityList))
    {
      identLen = length((strsplit(names(identityList)[i], split = character(0))[[1]]))
      if(length(resolution) == 0 || identLen > resolution)
        resolution = c(resolution,identLen)

      cat("I = ",names(identityList)[i],"\t\tColumns:",identityList[[i]] ,"\n")
    }
    cat("\nResolution: ", as.character(as.roman(min(resolution))),"\n")
  }

  invisible(identityList)   #Defining relations in einer Liste wiedergeben
}


########calculation of the alias table and the identity of an object of class facDesign###########
#calculation is done as seen on http://www.weibull.com/DOEWeb/alias_matrix.htm
#fdo - an object of class facDesign
#assumes that the first 
aliasTable = function(fdo)
{
 X = unique(cube(fdo))
 N = nrow(X)
 k = log2(N)
 kPlusP = ncol(X)

 X1 = .helpAliasTable(fdo, k)
 X2 = .helpAliasTable(fdo, k = kPlusP)
 
 logVec = !(names(X2) %in% names(X1)) 
 X2 = X2[,logVec]
 
 X1 = as.matrix(X1)
 X2 = as.matrix(X2)

 #now calculate the alias table
 alias.matrix = solve(t(X1)%*%X1)%*%t(X1)%*%X2

 return(alias.matrix)
 
}

#fdo = fracDesign(k = 4, gen = "D = ABC")
#aliasTable(fdo)
#
#k - up to which column?
#fdo - object of class facDesign
.helpAliasTable = function(fdo, k)
{
 X = unique(cube(fdo))
 N = nrow(X)
# k = log2(N)
 columns = names(X[,1:k])
 
 X1 = matrix(1, nrow = N, ncol = 1)
 
 nameVec = c("Identity")

 for(i in 1:k)
 {  
   temp = combn(columns, i)
   for(j in 1:ncol(temp))
   {
    index = names(names(fdo)) %in% temp[,j]
    if(length((1:length(index))[index]) == 1)
    {
      X1 = cbind(X1,X[,index])
      nameVec = c(nameVec, temp[,j])
    }
    else
    {
      X1 = cbind(X1, apply(X[,index], 1, prod))
      nameVec = c(nameVec, paste(temp[,j], sep = "", collapse = "")) 
    }
   }
   X1 = data.frame(X1)
   names(X1) = nameVec
 }
 return(X1)
}
#.helpAliasTable(fdo, k = 3)


#ermittelt die Identitaet eines 2^k-p Versuchsplan
#@return list containing a vector of column indices, the names of the columns for the identity
#identity = function(fdo)
setGeneric("identity")
setMethod("identity", signature(x = "facDesign"), function(x)
{
  identity = character(0)
  identityList = vector(mode = "list", length = 0)
#  ident = numeric(0) #temporaerer Identitaetsvektor
  resolution = numeric(0)

  temp = NULL
  A = aliasTable(x)
                                                             #
  if(any(dim(A) == 0))
    return(identityList)  
 
 
  temp = as.matrix(A["Identity",])
  boolTemp = apply(temp, 2, as.logical)

  identity = row.names(temp)[boolTemp[,1]]
  
  if(length(identity) > 0)
  {
    charList = strsplit(toupper(identity), split = "")
    identityList = lapply(charList, match, LETTERS[1:26])
    names(identityList) = identity
  }

  cat("Defining relations:\n")
  if(length(identityList) > 0)
  {
    for(i in 1:length(identityList))
    {
      identLen = length((strsplit(names(identityList)[i], split = character(0))[[1]]))
      if(length(resolution) == 0 || identLen > resolution)
        resolution = c(resolution,identLen)

      cat("I = ",names(identityList)[i],"\t\tColumns:",identityList[[i]] ,"\n")
    }
    cat("\nResolution: ", as.character(as.roman(min(resolution))),"\n")
  }

  invisible(identityList)
}
)
#fdo = fracDesign(k = 4, gen = "D = ABC")
#identity(fdo)
#identity(fracDesign(k = 3))



#####################################################################################


#####################confounds.r############################
#experimental
#Aliasstruktur eines 2^k-p Versuchsplans berechnen
#Es werden die Identitaeten des Versuchsplans ermittelt. Fuer die (2^k) - 1 berechenbaren Effekte wird die Vermengung mit den Identitaeten bestimmt. Dazu wird die Schnittmenge
#der Faktornamen aus Identitaet und berechenbaren Effekt gebildet. Die Schnittmenge wird vom Generator abezogen. Zum Generator hinzugefuegt werden die Faktoren welche der Generator
#noch nicht enthaelt
#TODO: ALIAS Struktur nur bis zu den two way interactions berechnen z.B. oonfounds = function(x, DB = TRUE, full = FALSE) #full fuer vollstaendige ALIAS STRUKTUR
confounds = function(x, depth =  2)
{
  DB = FALSE
  
  varName = deparse(substitute(x));
  identityList = identity(x);

  x = cube(x)

  if(length(identityList) < 1)
  {
    print(paste(varName," contains no defining relations!"))
    invisible()
  }


  effect1 = numeric(0)  #temporaere Hilfsvariable
  effect2 = numeric(0)  #temporaere Hilfsvariable

  if(DB)
    identityList

  index = numeric(0)
  for(i in 1:(dim(x)[2]))
  {
      if(!(TRUE && (unique(x[,i]) %in% c(-1,1))))  #in Spalte i befinden sich andere Werte als -1 und 1
        index = c(index, i)
  }
  if(length(index) > 0)
    x = x[,-index]

    if(DB)
      cat("Column(s) ", index, " are discarded for analysis\n")


  n = dim(x)[2]  #Anzahl Spalten
  if(n <= 1)
    stop("Factorial Design contains only one row!")

  #alle Moeglichkeiten m Elemente aus n Elementen zu ziehen --> berechenbare Effekte
  for(j in 1:length(identityList))
  {
    ident = identityList[[j]]
    for(m in 1:n)
    {
      combMat = combn(1:n,m)

      #Alias fuer den jeweilig berechenbaren Effekt ermitteln
      for(i in 1:(dim(combMat)[2]))
      {
        isect = intersect(ident, combMat[,i])    #Schnittmenge
        conf = setdiff(ident, isect)     #Schnittmenge wird vom Generator abgezogen
        conf = sort(c(conf, setdiff(combMat[,i], isect)))   #was nicht im Generator vorhanden ist wird dazugepackt

        effect1 = c(effect1, paste(sort(names(x)[as.numeric((combMat[,i]))]), sep = "", collapse = ""))
        effect2 = c(effect2, paste(sort(names(x)[conf]), sep = "", collapse = ""))

        if(DB)
        {
          cat("Effect(s) ", as.numeric((combMat[,i]))," aliased with Effect(s)", conf,"\n")
          cat("Effect(s)", names(x)[as.numeric((combMat[,i]))]," aliased with Effects ", names(x)[conf],"\n")
        }

      }
    }
  }

  if(DB)
  {
    cat(effect1,"\n")
    cat(effect2,"\n")
  }

  #TODO Vergleich darf nicht ueber Zahlen laufen
  if(length(effect1) > 0)
  dupIndex = numeric(0)
    for(i in 1:length(effect1))
    {
      if(DB)
      {
        cat("i: ", i, "\tlength(effect1): ",length(effect1), "\n")
        cat("effect 1 : ", effect1,"\n")
      }
      if(i > length(effect1)) #effect Vektoren sind durch die Entahme doppelt auftretender Alias kuerzer als Laufinxdex
        break;

        index = (1:length(effect1))[effect2 == effect1[i]]

        if(DB)
          cat("index: ", index,"\n")

        dupIndex = numeric(0)
        for(j in index)
        {
          if(effect1[j] == effect2[i])
          {
            if(i != j)  #nur Duplikate entfernen
              dupIndex = c(dupIndex, j)
          }
        }

        if(length(dupIndex > 0))
        {
          effect1 = effect1[-dupIndex]
          effect2 = effect2[-dupIndex]
        }
    }

    cat("\nAlias Structure:\n")
    for(i in 1:length(effect1))
    {
      #show only effects up to order depth - default is 2
      if((length(strsplit(effect1[i], split=character(0))[[1]]) <= depth) && (length(strsplit(effect2[i], split=character(0))[[1]]) <= depth))# & (length(strsplit(effect2[i], split=character(0))[[1]]) <= depth))
        cat(effect1[i], "\tis confounded with\t", effect2[i], "\n")

      #show all confounded effects
      if(identical(depth, "all"))
        cat(effect1[i], "\tis confounded with\t", effect2[i], "\n")

    }
     invisible(effect1)
    #invisible(data.frame(effect = effect1, alias = effect2))


}


#######################fracDesign.r####################################
#TODO: Versuchsplanklasse erstellen
#Die am haeufigsten verwendeten fractional factorials auflisten S. 272 BHH2 ---> Referenz in der Dokumentation
#TODO: Identitaet speichern bzw. die Defining Relation
#TODO: Identitaet bzw. Defining Relation aus einem vorhandenenm Versuchsplan ermitteln
#       Identitaet kann gefunden werden in dem alle Kombinationen von Spalten multipliziert werden. Identitaet hat eine Plus Spalte S.241
#       Mit der Identitaet kann dann die komplette ALIASstruktur ermittelt werden
#TODO: Dokumentation und die Verwendung von BHH2 vermerken
#TODO. Berechnung der Resolution eines Versuchsplan
#       entspricht der Anzahl Letters in der Defining Relation
#TODO: Normal Plots von S.261 BHH2


#TODO: Vereinfachte MEPlota a la "Qualitaetsmanagement in der Automobilindustrie"
#TODO: Barplot mit Signifikanzlinie und Beschriftung der Faktoren a la "Qualitaetsmanagement in der Automobilindustrie"
#TODO: Faktorklasse schreiben fuer steepest Ascent, automatisierte Beschriftung der Faktoren etc.


#designs fractional 2^k factorials i.e. 2^k-p
#k is number of factors in 2^k-p different settings
#
#fracDesign = function(k = 3, charGen = c("D = ABC"), DB = FALSE)
fracDesign = function(k = 3, gen = NULL, replicates = 1, blocks = 1, centerCube = 0, random.seed = 1234)
{
  DB = FALSE

  if(!is.numeric(random.seed))
    stop("random.seed needs to be numeric")
    
  if(!is.numeric(blocks))
    stop("blocks needs to be numeric!")
    
  if(!is.numeric(replicates))
    stop("replicates needs to be numeric!")
  else
    if(replicates < 0)
      stop("replicates need to >= 0")

  #begin: taken from BHH2 package
  N <- 2^k
    X <- matrix(NA, nrow = N, ncol = k)
    for (j in 1:k) X[, j] <- rep(sort(rep(c(-1, 1), N/2^j)),
        2^(j - 1))
    X <- X[, ncol(X):1]
    if (is.null(gen))
    {
        X = as.data.frame(X)
        names(X) = LETTERS[1:k]
    }
   #end: taken from BHH2 package

  #add the specified number of replicates
  origX = X
  if(replicates > 1)
  {
    for(i in 2:replicates)
    {
     X = rbind(X, origX)
    }
  }

frameOut = data.frame(X)

if(DB)
  print("juhu")

if(!is.null(gen))
{
  listGen = vector("list", length(gen))   #zukuenftige Liste der Generatoren / Defining Relations
  .numFactors = numeric(0)                        #zukuenftige Liste in Zahlen umgesetzte Buchstaben
  charFactors = character(0)

  if(DB)
  {
  cat(paste("gen is: ", gen , "\n"));
  cat(paste("length of gen is: ", length(gen), "\n"));
  print(listGen)
  }

   temp = character(0)  #sammelt alle vorhandenen Character!
  #Erste Schleife
  #Eine Zuordnung zwischen character und numeric Darstellung der Faktoren erstellen
#  for(i in 1:length(gen))
  for(i in seq(along = gen))
  {
    if(DB)
      cat("gen[",i,"] = ",gen[i], "\n")

    if(!is.character(gen[i]))
      stop("Defining Relations should contain characters only!")

    chars = strsplit(gen[i], split=character(0))[[1]]   #einzelne Strings in characterVector herunterbrechen

    if(DB)
    {
      cat("\nchars: ")
      print(chars)
      cat("\n")
    }


    checkDupl = character(0)
    for(j in 1:length(chars))
    {
      #Buchstaben und Gleichheitzeichen sind erlaubt
      if(chars[j] %in% toupper(c(LETTERS[1:26], letters[1:26])))
      {
        if(chars[j] %in% checkDupl)    #kommt ein Buchstabe in der defining relation doppelt vor wird abgebrochen
          stop("Defining relations contain one or more duplicates!")

        checkDupl = c(checkDupl, chars[j])
        temp = c(temp, chars[j])
      }
    }


    if(DB)
    {
      cat("\ntemp: ")
      print(temp)
      cat("\n")
    }

  }

  temp = sort(unique(temp))
  numCharVec = 1:length(temp)   #beinhaltet die Zuordnung Buchstabe zu Spalte
  names(numCharVec) = temp

  if(DB)
  {
    cat("Zuordnung Buchstabe und Spalte:\n")
    print(numCharVec)
    cat("\n")

  }

  ####Zweite Schleife --> Defining Relations ermitteln
#  for(i in 1:length(gen))
  for(i in seq(along = gen))
  {

    if(DB)
      cat("gen[",i,"] = ",gen[i], "\n")

    if(!is.character(gen[i]))
      stop("Defining Relations should contain characters only!")

    chars = strsplit(gen[i], split=character(0))[[1]]   #einzelne Strings in characterVector herunterbrechen

    numVec = numeric(0)
    charVec = character(0)

    allowedChars = c(LETTERS[1:26], letters[1:26], "=")
    for(j in 1:length(chars))
    {
      #nur Buchstaben und Gleichheitzeichen sind erlaubt
      if(chars[j] %in% allowedChars)
      {
        if((chars[j] == "=") & (length(numVec) != 1))   #Gleichheitszeichen darf sich nur an der 2 Stelle befinden eines jeden Generators befinden
          stop("check position of \"=\" in generators!")

        if(chars[j] != "=")                             #
        {
          charVec = c(charVec, toupper(chars[j]))
          numVec = c(numVec, numCharVec[names(numCharVec) == toupper(chars[j])])     #Buchstabe einer Spalte zuordnen d.h. entsprechend numCharVec sortieren
        }
      }

    }

    if(DB)
    {
    cat("charVec for i = ", i ,": ", charVec,"\n")
    cat("numVec for i = ", i ,": ", numVec,"\n")
    }

    listGen[[i]] = numVec   #numVec ist jetzt in der Form z.B. (4,1,2,3) und wird in die Liste gepackt
                            #ffDesMatrix aus BHH2 kann jetzt aufgerufen werden.

    .numFactors =  c(.numFactors, numVec)            #numeric Factors i.e. 1,2,3...
    charFactors = c(charFactors, charVec)         #character Factors i.e. A, B, C, ...
  }

  if(DB)
    print("juhu")
  
  
  names(.numFactors) = charFactors                 #Zuordnung von numeric to character der Faktoren speichern

  if(length(unique(.numFactors)) > k)    #Anzahl unterschiedlicher Faktoren aus Generator kann k nicht ueberschreiten
    stop("number of distinct Factors in generators greater than k!")


  if(DB)
  {
    print(listGen)
    print(.numFactors)
    print(charFactors)
  }


#  for (i in 1:length(listGen))
  for(i in seq(along = listGen))
  {
        ind <- trunc(listGen[[i]])
        if (any(abs(ind) > k))
            stop(paste("generator:", paste(ind[1], "=", paste(ind[-1],
                collapse = "*")), "includes undefined columns"))
        x <- rep(sign(ind[1]), N)
        for (j in ind[-1]) x <- x * X[, j]
        X[, abs(ind[1])] <- x
  }


        X <- unique(X)

#  frameOut = as.data.frame(ffDesMatrix(k = k, gen = listGen))

  origX = X
  if(replicates > 1)
  {
    for(i in 2:replicates)
    {
     X = rbind(X, origX)
    }
  }
  frameOut = as.data.frame(X)
  names(frameOut) = names(numCharVec) #numCharVec ist passend sortiert

  #Muss theoretisch nicht explizit abgefragt werden --> NA Spalten koennen auch so abgefragt werden.
  if(k > length(temp))  #Es sind mehr Faktoren k angegeben als in den Defining Relations entnommen werden koennen
  {
    charsLeft = (LETTERS[1:26])[-match(charFactors, LETTERS[1:26])]
    naIndex = (1:k)[is.na(names(frameOut))]    #frameOut hat k Columns von denen der Index der NA-Spalten ermittelt wird
    names(frameOut)[naIndex] = charsLeft[1:length(naIndex)]
  }

  #! Uncomment if fracDesign is to be used without class facDesign
  #return(frameOut)

}

  #### Design wird in einem Objekt der Klasse facDesign untergebracht ####
  DesignOut = new("facDesign")
  
  #### Generator abspeichern
  DesignOut@generator = gen
  
  ####cube zuordnen
  cube(DesignOut) = frameOut
  
  #Erstellen einer Liste von Faktoren mit Namen, Einheiten und Kodierung
  listFac = vector("list", ncol(frameOut))

  for(i in seq(along = listFac))
    listFac[i] = new("doeFactor")

  names(listFac) = names(frameOut)
  factors(DesignOut) = listFac

  
  
  if(DB)
    print(frameOut)


  if(DB)
    print("yes")
    

  if(DB)
    print("aha")

  #add a reponse data.frame to the Design
  numRows = nrow(cube(DesignOut)) + nrow(star(DesignOut)) + nrow(centerStar(DesignOut)) + nrow(centerCube(DesignOut))
  
  if(DB)
  {
    print(numRows)
    print("response")
  }


  DesignOut@response = data.frame(y = rep(NA, numRows))

  if(DB)
    print("response")


  


  #add a column for standard order and run Order
  standardOrder = data.frame(matrix(data = 1:numRows, nrow = numRows, ncol = 1))
  names(standardOrder) = "StandOrder"
  standardOrder
  standOrd(DesignOut) = standardOrder
  
  if(DB)
    print("1")

  set.seed(random.seed)
  runOrder = as.data.frame(standardOrder[sample(1:numRows),])

  if(DB)
    print("2")
    
  names(runOrder) = "RunOrder"
  runOrd(DesignOut) = runOrder

  if(DB)
    print("3")

  
  if(blocks == 1)
  {
   Block = data.frame(Block = rep(1, times = numRows)) 
   block(DesignOut) = Block
  }
  
  #Hinzufuegen der .centerPoints
  if(centerCube >= 1)
  {
    temp = data.frame(matrix(rep(0,centerCube*k), ncol = k, nrow = centerCube))
    names(temp) = names(frameOut)
#      print("4")
    centerCube(DesignOut) = temp
 #     print("5")
  }
  
#  if(blocks == 1)
#  {
#   Block = data.frame(Block = rep(1, times = nrow(DesignOut))) 
#     print("6")
#   block(DesignOut) = Block
#     print("7")
#  }

  return(DesignOut)
}

#test = fracDesign(k = 4, gen = c("D = ABC", "A = CD"), replicates = 2)
##
#test = fracDesign(k = 6, gen = c("E = BC"))
#
#t2 = fracDesign(k = 3, replicates = 2)

facDesign = function(k = 3, replicates = 1, blocks = 1, centerCube = 0)
{
  frameOut = fracDesign(k = k, gen = NULL, replicates = replicates, blocks = 1, centerCube = centerCube)
  return(frameOut)
}







######################################End of the fracDesign and helper functions############################################

#print.invisible <- function(x)
#{
# print("")
#}
#

#class(fracDesign) <- "invisible"
#class(identity) <- "invisible"
#class(summary) <- "invisible"
#class(show) <- "invisible"
#class(response) <- "invisible"








