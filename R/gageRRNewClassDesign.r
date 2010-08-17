setClass("gageRR",  representation = representation(X = "data.frame", ANOVA = "aov", RedANOVA = "aov", method = "character", Estimates = "list", Varcomp = "list", Sigma = "numeric", GageName = "character", GageTolerance = "numeric", DateOfStudy = "character", PersonResponsible = "character", Comments = "character", b = "factor", a = "factor", y = "numeric", facNames = "character" ))

#show Funktion fuer Gage R&R Klasse 
#@return - data.frame
setMethod("show", signature(object = "gageRR"), function(object) {

  print(as.data.frame(object))
}
)


#indexing function for gage R&R class
setMethod("[", signature(x = "gageRR", i = "ANY", j = "ANY"), function(x, i, j)
{
 x@X[i,j]

}
)


#summary Funktion fuer Gage R&R Klasse
#@return - if measurements exist the same as gageRR (complete model summary) or data.frame
setMethod("summary", signature(object = "gageRR"), function(object) {

  if(all(is.na(object@X$Measurement)))
    return(print(as.data.frame(object)))
  
  return(gageRR(object))


}
)

#setGeneric("response", function(object){standardGeneric ("response")})    
#setGeneric("response<-", function(object, value){ standardGeneric("response<-")})
setMethod("response", "gageRR", function(object)
                              {
                              out = object@X$Measurement
                              return(out)
                              }
          )
setReplaceMethod("response", "gageRR", function(object, value)
{
  object@X$Measurement = value
  return(object)
}
)


#summary Funktion fuer Gage R&R Klasse
#@return - if measurements exist the same as gageRR (complete model summary) or data.frame
setMethod("names", signature(x = "gageRR"), function(x) {
    return(names(as.data.frame(x)))
}
)


#Generische as.data.frame Funktion fuer ein gageRRDesign object
setMethod("as.data.frame", "gageRR", function(x, row.names = NULL, optional = FALSE,...) {
  return(x@X)
}
)

#gibt den Versuchsplan als data.frame zurueck
#called by lm when assigning an object of class gageRRDesign to data
#setGeneric("as.data.frame.gageRR", function(x, row.names = NULL, optional = FALSE,...) standardGeneric("as.data.frame.gageRR"))
#setMethod("as.data.frame.gageRR", "gageRR", function(x, row.names = NULL, optional = FALSE,...) {
as.data.frame.gageRR = function(x, row.names = NULL, optional = FALSE,...) {
  return(x@X)
}

setGeneric("tolerance", function(x) standardGeneric("tolerance"))
setGeneric("tolerance<-", function(x, value) standardGeneric("tolerance<-"))
setMethod("tolerance", "gageRR", function(x) unlist(x@GageTolerance))    #low(object) gibt den low Factor zurueck
setReplaceMethod("tolerance", "gageRR", function(x, value)
{
  if(!is.numeric(value))
    stop(paste(deparse(substitute(value)),"needs to be numeric"))

  x@GageTolerance = value

  return(x)
}
)

setGeneric("sigma", function(x) standardGeneric("sigma"))
setGeneric("sigma<-", function(x, value) standardGeneric("sigma<-"))
setMethod("sigma", "gageRR", function(x) unlist(x@Sigma))    #low(object) gibt den low Factor zurueck
setReplaceMethod("sigma", "gageRR", function(x, value)
{
  if(!is.numeric(value))
    stop(paste(deparse(substitute(value)),"needs to be numeric"))

  x@Sigma = value

  return(x)
}
)




#.aip is a slightly altered version of R's interaction.plot
.aip = function (x.factor, trace.factor, response, fun = mean, type = c("l",
    "p", "b"), legend = TRUE, trace.label = deparse(substitute(trace.factor)),
    fixed = FALSE, xlab = deparse(substitute(x.factor)), ylab = ylabel,
    ylim = range(cellNew, na.rm = TRUE), lty = nc:1, col = 1, pch = c(1L:9,
        0, letters), xpd = NULL, leg.bg = par("bg"), leg.bty = "o",
    xtick = FALSE, xaxt = par("xaxt"), axes = TRUE, title = "", ...)
{
    ylabel <- paste(deparse(substitute(fun)), "of ", deparse(substitute(response)))
    type <- match.arg(type)
    cellNew <- tapply(response, list(x.factor, trace.factor), fun)
    nr <- nrow(cellNew)
    nc <- ncol(cellNew)
    xvals <- 1L:nr
    if (is.ordered(x.factor)) {
        wn <- getOption("warn")
        options(warn = -1)
        xnm <- as.numeric(levels(x.factor))
        options(warn = wn)
        if (!any(is.na(xnm)))
            xvals <- xnm
    }
    xlabs <- rownames(cellNew)
    ylabs <- colnames(cellNew)
    nch <- max(sapply(ylabs, nchar, type = "width"))
    if (is.null(xlabs))
        xlabs <- as.character(xvals)
    if (is.null(ylabs))
        ylabs <- as.character(1L:nc)
    xlim <- range(xvals)
    xleg <- xlim[2L] + 0.05 * diff(xlim)
    xlim <- xlim + c(-0.2/nr, if (legend) 0.2 + 0.02 * nch else 0.2/nr) *
        diff(xlim)
    matplot(xvals, cellNew, ..., type = type, xlim = xlim, ylim = ylim,
        xlab = xlab, ylab = ylab, axes = axes, xaxt = "n", col = col,
        lty = lty, pch = pch)
    if (axes && xaxt != "n") {
        axisInt <- function(x, main, sub, lwd, bg, log, asp,
            ...) axis(1, x, ...)
        mgp. <- par("mgp")
        if (!xtick)
            mgp.[2L] <- 0
        axisInt(1, at = xvals, labels = xlabs, tick = xtick,
            mgp = mgp., xaxt = xaxt, ...)
    }
    if(legend)
    {
      legend("topright", legend = ylabs, title = title, col = col, pch = if (type %in% c("p", "b")) pch, lty = if (type %in% c("l", "b")) lty, bty = leg.bty, bg = leg.bg, inset = 0.02)
    }
    invisible()
}





#Validität checken
##TODDO!
##Operator und Part müssen Faktoren sein
##Die Anzahl der Vorkommnisse muss so gewählt sein, dass eine Analyse möglich ist.
##Funktion für das gageRR-Paket
##Funktion die in Abhängigkeit von Anzahl Operator, Anzahl Teile, Anzahl Messwiederholungen
##einen Versuchsplan zurückgibt
##Dies ist ein BALANCED Design only
##angegeben werden können die Namen der Faktorstufen oder aber einfach die Anzahl
##Measurements gibt die Measurements per Part an
##Default ist ein Crossed Design, nested Design kann ebenfalls angegeben werden
gageRRDesign = function(Operators = 3, Parts = 10, Measurements = 3, method = "crossed", sigma = 6, randomize = TRUE)
{
  method  = method
  opvec   = factor()
  partvec = factor()
  
  
  yName = aName = bName = abName = NA
  yName = "Measurement"  #name of the Measurement
  aName = "Operator"  #name of Operator
  bName = "Part"  #name of Part
  abName = "Operator:Part" #names of Interaction Term
  
  
  Operators = unique(Operators)
  Parts = unique(Parts)

      if(!is.numeric(sigma))
        stop("sigma needs to be numeric")

      if(method != "nested" && method != "crossed")
        warning("unknown method specified --> defaulting to \"method = crossed\"")

      if(!is.numeric(Measurements))
        stop("Number of Measurements per Part not specified!")
      else
        Measurements = round(Measurements[1])

      #check and set Operators
      if(!is.numeric(Operators) && !is.character(Operators))
        stop("Operator needs to be numeric 'Operator = 3' or character 'Operator = c(\"A\",\"B\", \"C\"")

      if(is.numeric(Operators))
        opvec = factor(LETTERS[1:Operators[1]])

      if(is.character(Operators))
        opvec = factor(Operators)

      if(length(unique(opvec)) > 26)
        stop("To many Operators!")

      if(length(unique(opvec)) < 2)
        stop("To few Operators")

      #check and set Parts
      if(!is.numeric(Parts) && !is.character(Parts))
        stop("Parts needs to be numeric 'Parts = 3' or character 'Parts = c(\"A\",\"B\", \"C\"")

      if(is.numeric(Parts))
        partvec = factor(LETTERS[1:Parts[1]])

      if(is.character(Parts))
        partvec = factor(Parts)

      if(length(unique(partvec)) > 26)
        stop("To many Parts!")

      if(length(unique(partvec)) < 2)
        stop("To few Parts")


      #Anzahl Messungen
      Measurement = rep(NA,(length(opvec)*length(partvec)*Measurements))
      outFrame = data.frame()

       if(method == "crossed")
       {
        temp = expand.grid(opvec, partvec)
        o = rep(temp[,1], Measurements)
        p = rep(temp[,2], Measurements)
       }
       else  ##Nested Design --> Aus i Batches, werden j Parts entnommen die k mal vermessen werden
       {
        p  = factor(sort(rep(1:(length(partvec)*length(opvec)),Measurements)))
        o    = sort(rep(opvec, length(partvec)/length(opvec)))
       }
       
       if(randomize) 
         outFrame = data.frame(StandardOrder = 1:length(Measurement), RunOrder = sample(1:length(Measurement), length(Measurement)), Operator = factor(o), Part = factor(p), Measurement)
       else
         outFrame = data.frame(StandardOrder = 1:length(Measurement), RunOrder = 1:length(Measurement), Operator = factor(o), Part = factor(p), Measurement)          

       outFrame = outFrame[order(outFrame$RunOrder),]


  gageRRObj = new("gageRR")
  gageRRObj@facNames = c(yName, aName, bName, abName)
  names(gageRRObj@facNames) = c("yName", "aName", "bName", "abName")
  gageRRObj@Sigma = sigma
  gageRRObj@method = method
  gageRRObj@a = factor(o)
  gageRRObj@b = factor(p)
  gageRRObj@y = as.numeric(Measurement)

#  gageRRObj = new("gageRR")
  gageRRObj@method = method
  gageRRObj@Sigma = sigma
  gageRRObj@X = outFrame
  return(gageRRObj)
}


#temp = gageRRDesign(Operators = 3,3,3)
#temp@y = rnorm(27)
#temp@X$Measurement  = rnorm(27)
#temp = gageRR(temp)
#



#gdo - an object of class gageRRDesign
gageRR = function(gdo, method = "crossed", sigma=5.15, alpha = 0.25, DM=NULL ,HM=NULL, toler=NULL, plotit=F, dig=4, ...)
{
  #Namen erstellen
  yName = names(gdo)[5]  #name of the Measurement
  aName = names(gdo)[3]  #name of Operator
  bName = names(gdo)[4]  #name of Part
  abName = paste(aName, ":",bName, sep = "") #names of Interaction Term
  bTobName = paste(bName,"to",bName, sep = " ") #i.e. Part To Part

  ###Faktoren bilden
  a = gdo@X[,aName]
  b = gdo@X[,bName]
  y = gdo@X[,yName]


#  print(a)
#  print(b)
#  print(abName)
#  print(y)  
#  
  nestedFormula = as.formula(paste(yName, "~", aName, "/", bName))
  crossedFormula = as.formula(paste(yName, "~", aName, "*", bName))
  reducedFormula = as.formula(paste(yName, "~", aName, "+", bName))
  
#  print(nestedFormula)
#  print(crossedFormula)
  


  if(is.na(y) || !is.numeric(y))
    stop("Measurements need to be numeric")

  if(method %in% c("crossed", "nested"))
    method = method
  else
    method = gdo@method


      #Introduction to Statistical Quality Control 5e page 359 && Design of Experiments 4th ed. Montgomery
      #Estimators for the unknown variances i.e. Variance Components
      #all effects are random i.e. Standard Model for Gage RR
      #method = NESTED i.e y[ijk] = u + a[i] + b[j(i)] + e[ijk]      i.e. i is nested in j respectively a is nested in b
      #aov(Response ~ Operator/Part)
      if(method == "nested")
      {
         #check if the design is nested regarding the encoding
         alevels = unique(a)
         for(i in 1:length(alevels))
         {
          blevels = unique(b[a == alevels[i]])

          if(any((blevels %in% b[a != alevels[i]]) == TRUE))
            stop("Design is not nested in the form given\nI check if nested\nII check encoding")
         }



        numA<-nlevels(a[,drop=T])#Anzahl Operators
        numB<-nlevels(b[,drop=T]) #Anzahl Teile
        numMPP<-length(a)/(numB) #Anzahl Messungen pro Teil, Parts werden nicht mermahls vermessen da nested, dementsprechend um 1/AnzahlOperator korrigieren

        #fit = aov(y ~ a/b)
        fit = aov(nestedFormula, data = gdo)
        meanSq<-anova(fit)[,3]

        gdo@ANOVA = fit
        gdo@method = "nested"

        MSa  = meanSq[1]  #Operator
        MSab  = meanSq[2]   #Part nested within Operator
        MSe   = meanSq[3]   #Error i.e. Repeatability

        Cerror  = MSe
        Cb   = (MSab - MSe)/numMPP   ##Anzahl der Freiheitsgrade muss noch durch 3
        Ca     = (MSa - MSab)/ (numB*numMPP)

        if(Ca < 0)
          Ca = 0

        Cab     = 0


        totalRR = Ca + Cab + Cerror
        repeatability = Cerror
        reproducibility = Ca
        bTob = Cb
        totalVar = Cb + Ca + Cab + Cerror

        #Liste mit Namen und Werten
        estimates = list(Cb = Cb, Ca = Ca, Cab = Cab , Cerror= Cerror)
        varcomp  = list(totalRR = totalRR, repeatability = repeatability, reproducibility = reproducibility, bTob = bTob, totalVar = totalVar)

        #contribution ist einfach nur getteilt durch totalVar
#        contribution = c(totalRR/totalVar, repeatability/totalVar, reproducibility/totalVar, partTopart/totalVar, totalVar/totalVar)

        gdo@Estimates = estimates
        gdo@Varcomp = varcomp

      }


        #Estimators for the unknown variances i.e. Variance Components
        #all effects are random i.e. Standard Model for Gage RR
        #method = CROSSED i.e. y[ijk] = u + a[i] + ß[j] +aß[ij] + e[ijk]
        #Mean squared Errors
      if(method == "crossed")
      {
        numA<-nlevels(a[,drop=T])#i.e. a
        numB<-nlevels(b[,drop=T]) #i.e. b
        numMPP<-length(a)/(numA*numB) #i.e. n

        #fit<-aov(y ~ a*b)
        fit = aov(crossedFormula, data = gdo)
        
        model <- anova(fit)
        gdo@ANOVA = fit
        gdo@method = "crossed"

        #Ueberfluessige Checks - urspruenglich etwas generischer geplant
        MSb = MSa = MSab = MSe = 0
        if(bName %in% row.names(model))
          MSb   = model[bName,"Mean Sq"]
        else
          warning(paste("missing factor", bName,"in model"))
        if(aName %in% row.names(model))
          MSa   = model[aName,"Mean Sq"]
        else
          warning(paste("missing factor",aName,"in model"))
        if(abName %in% row.names(model))
          MSab  = model[abName,"Mean Sq"]
        else
          warning(paste("missing interaction",abName ,"in model"))
        if("Residuals" %in% row.names(model))
          MSe  = model["Residuals","Mean Sq"]
        else
          warning("missing Residuals in model")


        #TODO: Für den Fall, dass die Wechselwirkung irrelevant ist besteht die Reproducibility aus dem Sigma_a allein
        #C is sigmaSquared
        #Calculation for both factors random only
        Cb = Ca = Cab = Cerror = 0
        Cb = (MSb - MSab)/(numA*numMPP)
        Ca   = (MSa - MSab)/(numB*numMPP)
        Cab   = (MSab - MSe)/(numMPP)
        Cerror= (MSe)

        gdo@RedANOVA = gdo@ANOVA#reduziertes Modell ist zunächst auch das Standardmodell
        #TODO: Wie steht die Interaction im Modell
        #Wechselwirkung ist irrelevant da kleiner 0 oder nicht signifikant --> Modell ohne Wechselwirkung anpassen
#        print(model[abName,])
#        print(Cab)
        if((Cab < 0) || (model[abName,"Pr(>F)"] >= alpha))
        {
          redFit<-aov(reducedFormula, data = gdo)
          model <- anova(redFit)

        MSb = MSa = MSab = MSe = 0
        if(bName %in% row.names(model))
          MSb   = model[bName,"Mean Sq"]
        else
          warning(paste("missing factor",bName,"in model"))
        if(aName %in% row.names(model))
          MSa   = model[aName,"Mean Sq"]
        else
          warning(paste("missing factor", aName, "in model"))
#        if(abName %in% row.names(model))
#          MSab  = model[abName,"Mean Sq"]
#        else
#          warning(paste("missing interaction" , abName,"in model"))
        if("Residuals" %in% row.names(model))
          MSe  = model["Residuals","Mean Sq"]
        else
          warning("missing Residuals in model")


        #TODO: Für den Fall, dass die Wechselwirkung irrelevant ist besteht die Reproducibility aus dem SigmaOperator allein
        Cb = Ca = Cab = Cerror = 0
        Cb = (MSb - MSab)/(numA*numMPP)
        Ca   = (MSa - MSab)/(numB*numMPP)
        Cab = 0
        Cerror= (MSe)

        gdo@RedANOVA = redFit
        }


        gdo@method = "crossed"

        totalRR = Ca + Cab + Cerror
        repeatability = Cerror
        reproducibility = Ca + Cab
        bTob = Cb
        totalVar = Cb + Ca + Cab + Cerror

        #Liste mit Namen und Werten
        estimates = list(Cb = Cb, Ca = Ca, Cab = Cab, Cerror = Cerror)
        varcomp  = list(totalRR = totalRR, repeatability = repeatability, reproducibility = reproducibility, a = Ca, a_b = Cab,  bTob = bTob, totalVar = totalVar)

        gdo@Estimates = estimates
        gdo@Varcomp = varcomp
      }
        
        
        
  cat("\n")
  cat(paste("AnOVa Table - ", gdo@method,"Design\n"))
  print(summary(gdo@ANOVA))
  cat("\n")
  cat("----------\n")

  if(!identical(gdo@RedANOVA, gdo@ANOVA))
  { cat(paste("AnOVa Table Without Interaction - ", gdo@method,"Design\n"))
    print(summary(gdo@RedANOVA))
    cat("\n")
    cat("----------\n")
  }

        Source = names(gdo@Varcomp)
        Source[Source == "repeatability"] = " repeatability"
        Source[Source == "reproducibility"] = " reproducibility"
        Source[Source == "a_b"] = paste("  ",abName)
        Source[Source == "a"] = paste("  ",aName)
        Source[Source == "bTob"] = bTobName

        VarComp = round(as.numeric(gdo@Varcomp[c(1:length(gdo@Varcomp))]),3)
        Contribution = round(as.numeric(gdo@Varcomp[c(1:length(gdo@Varcomp))])/as.numeric(gdo@Varcomp[length(gdo@Varcomp)]),3)

        VarComp = t(data.frame(gdo@Varcomp))
        VarCompContrib = VarComp/gdo@Varcomp$totalVar
        Stdev = sqrt(VarComp)
        StudyVar = Stdev*gdo@Sigma
        StudyVarContrib = StudyVar/StudyVar["totalVar",]

        SNR = NA
        ptRatio = NULL
        temp = NULL

        #Falls ein Tolerance Limit angegeben ist P/T Ratio berechnen
        if((length(gdo@GageTolerance) > 0) && (gdo@GageTolerance > 0))
        {
          ptRatio = StudyVar / gdo@GageTolerance
          temp = data.frame(VarComp , VarCompContrib, Stdev, StudyVar, StudyVarContrib, ptRatio)
          names(temp)[6] = c("P/T Ratio")
          row.names(temp) = c(Source)
          #calculate the number of distinct categories that can be reliably distinguished
          #sqrt(2*(ProcessVariance/MeasurementVariance))
          SNR = sqrt(2*(temp[bTobName, "VarComp"]/temp["totalRR", "VarComp"]))
        }
        else
        {
          temp = data.frame(VarComp , VarCompContrib, Stdev, StudyVar, StudyVarContrib)
          row.names(temp) = c(Source)
        }
        
        cat("\n")
        cat("Gage R&R\n")
        tempout = temp
      #  for (i in 1:ncol(temp))
#        {
#         tempout[,i] = signif(temp[,i], 3)
#        }
#        print(tempout)
        print(format(tempout, digits = 3))

        cat("\n")
        cat("---\n")
        cat(" * Contrib equals Contribution in %\n")

        if(!is.na(SNR))
          cat(paste(" **Number of Distinct Categories (signal-to-noise-ratio) =", floor(SNR),"\n"))
          cat("\n")

    invisible(gdo)
}

#Examples
#temp = gageRRDesign(Operators = 3,3,3)
#temp@y = rnorm(27)
#temp@X$Measurement  = rnorm(27, 20)
#temp@GageTolerance = 100
#gageRR(temp)
#
#matthews7.5 = gageRRDesign(Operators = 4, Parts = 8, Measurements = 2, method = "crossed", randomize = FALSE)
#matthews7.5@X = matthews7.5@X[order(matthews7.5@X[,"Part"]),]
#matthews7.5@X[,"Measurement"] = c(65,62,64,71,68,63,65,69,60,56,60,65,63,56,61,66, 44, 38, 46, 50, 45,43,46,47, 75,68,73,78, 76,71,71,78,63,57,63,65,66,57,62,68,59,55,57,65,60,53,60,62, 81,79,78,90,83,77,82,85,42,32,44,39,42,37,42,41)
#matthews7.5@GageTolerance = 100
#gageRR(matthews7.5)




#TODO: which integrieren, also welche Grafik soll gezeigt werden
#TODO: Farbauswahl aus der Klasse nehmen
#pch, col und andere Größen übergeben können
setGeneric("plot", function(x,y, ...) standardGeneric("plot"))
setMethod("plot", signature(x = "gageRR"), function(x, y, main, xlab, ylab, col, lwd, fun = mean, ...)
{
  horiz = FALSE

  parList = list(...)
  
  gdo = x
  yName = names(gdo)[5]  #name of the Measurement
  aName = names(gdo)[3]  #name of the Operator
  bName = names(gdo)[4]  #name of the Part
  abName = paste(aName,":",bName, sep = "")  #name of interaction between part and OPERATOR
  
  if(missing(col))
    col = 1:length(unique(gdo[,3]))
  
  if(missing(lwd))
    lwd = 1
  
  
##############################
#  if(missing(lwd))
#  if(is.null(parList[["lwd"]]))
#    lwd = 1
#  else
#    lwd = parList[["lwd"]]
#  
##  if(missing(col) || length(col) < 3)
#  if(is.null(parList[["col"]]))
#    col = 1:length(unique(gdo[,3]))
#  else
#    col = parList[["col"]]
#
#  if(is.null(parList[["lwd"]]))
#    lwd = 1
#  else
#    lwd = parList[["lwd"]]
#################################

  par(mfrow = c(2,2))

 
        temp = NULL

        Source = names(gdo@Varcomp)
        VarComp = round(as.numeric(gdo@Varcomp[c(1:length(gdo@Varcomp))]),3)
        Contribution = round(as.numeric(gdo@Varcomp[c(1:length(gdo@Varcomp))])/as.numeric(gdo@Varcomp[length(gdo@Varcomp)]),3)

        VarComp = t(data.frame(gdo@Varcomp))
        VarCompContrib = VarComp/gdo@Varcomp$totalVar
        Stdev = sqrt(VarComp)
        StudyVar = Stdev*gdo@Sigma
        StudyVarContrib = StudyVar/StudyVar["totalVar",]
        
        if((length(gdo@GageTolerance) > 0) && (gdo@GageTolerance > 0))
        {
          ptRatio = StudyVar / gdo@GageTolerance
          temp = data.frame(VarComp , VarCompContrib, Stdev, StudyVar, StudyVarContrib, ptRatio)
          contribFrame = data.frame(VarCompContrib, StudyVarContrib, ptRatio)
          names(temp)[6] = c("P/T Ratio")
          
          row.names(temp) = c(Source)
          
          
          #calculate the number of distinct categories that can be reliably distinguished
          #sqrt(2*(ProcessVariance/MeasurementVariance))
          SNR = sqrt(2*(temp["bTob", "VarComp"]/temp["totalRR", "VarComp"]))
          
          
        }
        else
        {
        temp = data.frame(VarComp , VarCompContrib, Stdev, StudyVar, StudyVarContrib)
        contribFrame = data.frame(VarCompContrib, StudyVarContrib)
        }
        
        bTob = paste(bName, "To", bName, sep = "")
        Source[Source == "bTob"] = bTob
        row.names(contribFrame) = Source
        
        if(gdo@method == "crossed")
          contribFrame = contribFrame[-match(c("totalVar","a","a_b"), row.names(temp)),]
        else
          contribFrame = contribFrame[-match(c("totalVar"), row.names(temp)),]

         numBars = ncol(contribFrame)
         ymax = max(max(contribFrame))

        main1 = NA
        if(missing(main) || is.na(main[1])) 
          main1 = "Components of Variation"
        else
          main1 = main[1]
          
        xlab1 = NA
        if(missing(xlab) || is.na(xlab[1])) 
          xlab1 = "component"
        else
          xlab1 = xlab[1]

        ylab1 = NA
        if(missing(ylab) || is.na(ylab[1])) 
          ylab1 = ""
        else
          ylab1 = ylab[1]        



        argList = list(...)
        redList = argList[names(argList) != "cex"]
        mybp = do.call(barplot, c(list(t(contribFrame), xlab = xlab1, ylab = ylab1, main = main1, names.arg = rep("",4), axes = F, beside = T, ylim = c(0,1.3*ymax), col = col[1:numBars]),redList))


        axis(1, at = colMeans(mybp), labels = names(as.data.frame(t(contribFrame))), ...)
        axis(2, ...)
        box()
        
        legend("topright", names(contribFrame), col = col[1:numBars], pch = c(15, 15), horiz = horiz, inset = 0.02)
        

  if(gdo@method == "crossed")
  {
        main2 = NA
        if(missing(main) || is.na(main[2])) 
          main2 = paste(yName,"by", bName)
        else
          main2 = main[2]
     
        xlab2 = NA
        if(missing(xlab) || is.na(xlab[2])) 
          xlab2 = bName
        else
          xlab2 = xlab[2]

        ylab2 = NA
        if(missing(ylab) || is.na(ylab[2])) 
          ylab2 = yName
        else
          ylab2 = ylab[2]        
  
        plot(as.numeric(gdo[,4]), jitter(gdo[,5]), axes = F, ylim = c(range(gdo[,5])[1], range(gdo[,5])[2] + diff(range(gdo[,5]))*0.3 ), xlab = xlab2, ylab = ylab2, main = main2,...) 
        box()
        axeVals = axis(1, ...)
        axis(2, ...)
        mByPa = split(gdo[,5], as.numeric(gdo[,4]))
        lines(sort(as.numeric(gdo[,4])), lapply(mByPa, mean)[sort(as.numeric(gdo[,4]))], lwd = lwd)
        points(sort(as.numeric(gdo[,4])), lapply(mByPa, mean)[sort(as.numeric(gdo[,4]))], pch = 15, ...)
        


        main3 = NA
        if(missing(main) || is.na(main[3])) 
          main3 = paste(yName,"by", aName)
        else
          main3 = main[3]
          
        xlab3 = NA
        if(missing(xlab) || is.na(xlab[3])) 
          xlab3 = aName
        else
          xlab3 = xlab[3]

        ylab3 = NA
        if(missing(ylab) || is.na(ylab[3])) 
          ylab3 = yName
        else
          ylab3 = ylab[3]        

        colVec = .mapping(gdo[,3], sort(unique(gdo[,3])), col[1:length(unique(gdo[,3]))])
        
        plot(as.numeric(gdo[,3]), jitter(gdo[,5]), axes = F, ylim = c(range(gdo[,5])[1], range(gdo[,5])[2] + diff(range(gdo[,5]))*0.3 ), col = colVec, xlab = xlab3, ylab = ylab3, main = main3, ...)
        axis(1, at = sort(unique(as.numeric(gdo[,3]))), labels = sort(unique(gdo[,3])), ...)
        axis(2, ...)
        box()
        
        #Linie durch die Mittelwerte einzeichnen, Zur Sicherheit sortiert nach as.numeric(gdo@Operator)
        mByOp = split(gdo[,5], as.numeric(gdo[,3]))
        lines(sort(as.numeric(factor(names(mByOp)))), lapply(mByOp, mean)[sort(names(mByOp))], lwd = lwd)
        
        
        ####Interaction vs Operator Plot
        main4 = NA
            if(missing(main) || is.na(main[4])) 
              main4 = paste("Interaction", abName)
            else
              main4 = main[4]
            
            xlab4 = NA
            if(missing(xlab) || is.na(xlab[4])) 
              xlab4 = names(gdo)[4]
            else
              xlab4 = xlab[4]
    
            ylab4 = NA
            if(missing(ylab) || is.na(ylab[4])) 
              ylab4 = paste(as.character(body(match.fun(fun)))[2], "of", names(gdo)[5]) #TODO: extract name of function correctly
            else
              ylab4 = ylab[4]
              
        .aip(gdo[,4], gdo[,3], response = gdo[,5], xlab = xlab4, ylab = ylab4, main = main4, col = col, type = "b", title = names(gdo)[3],...)
   }
   else  #"nested"
   {
        
        main2 = NA
        if(missing(main) || is.na(main[2])) 
          main2 = paste(yName,"By", bName, "Within", aName)
        else
          main2 = main[2]
        
        xlab2 = NA
        if(missing(xlab) || is.na(xlab[2])) 
          xlab2 = bName
        else
          xlab2 = xlab[2]

        ylab2 = NA
        if(missing(ylab) || is.na(ylab[2])) 
          ylab2 = yName
        else
          ylab2 = ylab[2]
    
        index = order(gdo[,3])
        colVec = .mapping(gdo[,3][index], sort(unique(gdo[,3][index])), col[1:length(unique(gdo[,3]))])
        plotIndex = sort(rep(1:length(unique(gdo[,4])), table(gdo[,4])[1]), decreasing = F)
        axisIndex = unique(gdo[,4][index])
        plot(plotIndex , gdo[,5][index],ylim = c(range(gdo[,5])[1], range(gdo[,5])[2] + diff(range(gdo[,5]))*0.3 ), col = colVec, axes = F, xlab = xlab2, ylab = ylab2, main = main2, ...)
        box()
        axis(2, ...)
        axis(1, at = 1:length(unique(plotIndex)), labels = axisIndex, ...)
        legend("topright", as.character(unique(gdo[,3][index])), title = aName, col = col[1:length(unique(gdo[,3][index]))], pch = 19)


        meanVec = numeric(length(axisIndex))        
        for(i in 1:length(axisIndex))
        {
          meanVec[i] = mean(gdo[,5][gdo[,4] == axisIndex[i]])
        }
        lines(1:length(meanVec), meanVec, col = "blue", lwd = lwd)
        points(1:length(meanVec), meanVec, col = "blue", pch = 19, ...)



        main3 = NA
        if(missing(main) || is.na(main[3])) 
          main3 = paste(yName,"By", aName)
        else
          main3 = main[3]
        
        xlab3 = NA
        if(missing(xlab) || is.na(xlab[3])) 
          xlab3 = aName
        else
          xlab3 = xlab[3]

        ylab3 = NA
        if(missing(ylab) || is.na(ylab[3])) 
          ylab3 = yName
        else
          ylab3 = ylab[3]
          
        #leer plotten um dann mit points aufzufüllen
        plot(as.numeric(gdo[,3][index]), gdo[,5][index], axes = F, ylim = c(range(gdo[,5])[1], range(gdo[,5])[2] + diff(range(gdo[,5]))*0.3 ), col = colVec, xlab = xlab3, ylab = ylab3, main = main3,...)
        axis(1, at = unique(as.numeric(gdo[,3][index])), labels = unique(gdo[,3][index]), ...)
        axis(2, ...)
        box()
        
        #Linie durch die Mittelwerte einzeichnen, Zur Sicherheit sortiert nach as.numeric(gdo@Operator)
        mByOp = split(gdo[,5], as.numeric(gdo[,3]))
        lines(sort(as.numeric(factor(names(mByOp)))), lapply(mByOp, mean)[sort(names(mByOp))], lwd = lwd)
   }
}                                                       
)


#temp = gageRRDesign(Operators = 3,10,3)
#response(temp) = rnorm(90, mean = 10)
#gdo = gageRR(temp)
#









