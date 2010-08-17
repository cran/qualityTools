#Funktion für das Erstellen eines Paretocharts

#TODO: Löschen nach Abschluss
#weight ist ein Vektor der z.B. die Kosten für eine Entität beinhaltet. Vektor muss Namen für die Kosten haben um zu matchen
#weight ist ein data.frame --> umbasteln in ein Vektor
#weight wird in einer Einheit angegeben. Der Name der Einheit sollte entsprechen als ylab und row.name in der Matrix auftauchen
#Ausgabe der Tabelle 


paretoChart = function(x, weight, showTable = TRUE, las = 0, main, col, border, xlab, ylab = "Frequency", percentVec, ...)
{
  varName = deparse(substitute(x))  #der name von x für die Überschrift
  #get corporate style somehow!
  corp.col = "pink3"
  corp.border = "red3"


  if(!is.vector(x) & !is.data.frame(x) & !is.table(x))
    stop("x should be a vector, dataframe or a table")

  if(is.table(x))
  {
    xtable = x
  }

  if(is.vector(x))
  {
    if(!is.null(names(x)))
      xtable = as.table(x)      
    else
      xtable = table(x)

  }

  #validate the weight vector if it exists
  if(!missing(weight))
  {
    if(!is.numeric(weight))
      stop("weight must be numeric!")
    if(is.null(names(weight)))  
      stop("weight is missing names for matching!")
    else
    {
      if(FALSE %in% (sort(names(weight)) == sort(names(xtable))))
        stop("names of weight and table do not match!")
      else
      {
        for(i in 1:length(xtable))
        {
          xtable[i] = weight[names(weight) == names(xtable)[i]]* xtable[i]
        }
      }
    }
  }
  else
  {
    weight = FALSE
  } 
  



  if(missing(showTable))
    showTable = TRUE
  if(missing(xlab))
    xlab = ""
  if(missing(main))
    main = paste("Pareto Chart for ", varName)
  if(missing(col))
    col = corp.col
  if(missing(border))
    border = corp.border
  if(missing(percentVec))
    percentVec = seq(0,1, by = 0.25)

  
  call <- match.call(expand.dots = TRUE)
  #save old par settings

  if(length(xtable) > 1)
  {
    #Häufigkeiten berechnen und in Tabelle speichern
    ylim = c(min(xtable), max(xtable)*1.025)
    xtable = sort(xtable, decreasing = TRUE, na.last = TRUE)
    cumFreq = cumsum(xtable)
    sumFreq = sum(xtable)
    percentage = xtable/sum(xtable)*100
    cumPerc = cumFreq/sumFreq*100
    
    frameOut = data.frame(Frequency = as.numeric(xtable), Cum.Freq = cumFreq, Percentage = percentage, Cum.Perc = cumPerc)
    names(frameOut) = c(ylab, paste("Cum.",ylab), "Percentage", "Cum. Percentage")
    row.names(frameOut) = names(xtable)

    frameInt = as.data.frame(t(frameOut))
    names(frameInt) = rep(" ", dim(frameInt)[2])

    


    

      #oldpar <- par(no.readonly=TRUE)   #save old par values
      oldpar <- par(no.readonly = TRUE) # all par settings which
      on.exit(par(oldpar))
  
      lineHeight = par("csi") * par("lheight") * par("cex") #calculation of line height see help(par) --> lheight
#      print(lineHeight)
      
      #Platz für die Tabelle berechnen!
      tablespace = (2*4)*(lineHeight/par("fin")[2]) #2*4 --> 8 Zeilen Platz für 4 Zeilen der Tabelle
      
      #par("fin") muss abgefragt werden können
      plot.new()

            #waagerechte Labels
        if(las == 0 | las == 1)    
        {
            mymai = par("mai")
            mymai[1] =  max(strheight(names(xtable), unit = "inches"))*3
            mymai[4] = strheight("Cumulative Percentage", unit = "inches")*8
            mymai[2] =  max(strwidth(names(frameOut), unit  = "inches"))*1.2
            par(mai = mymai, new = TRUE) 
        }
    
        #senkrechte Labels   
        if(las == 2 | las == 3)
        {
            mymai = par("mai")
            mymai[1] =  max(strwidth(names(xtable), unit = "inches"))*1.4
            mymai[4] = strheight("Cumulative Percentage", unit = "inches")*8
            mymai[2] =  max(strwidth(names(frameOut), unit  = "inches"))*1.2
            par(mai = mymai, new = TRUE) 
        }

    #soll die Tabelle unterhalb der Grafik angezeigt werden, sonst nur auf Kommandozeile
    if(showTable)
    {   
        par(fig = c(0,1, tablespace,1))

        xValue = barplot(xtable, axes = FALSE, las = las, width = 1, space = 0.2, xlim = c(0.2, 1.2*length(xtable)),   main = main, ylim = c(0, sum(xtable) + 0.01*(sum(xtable))), ylab = ylab, xlab = xlab, col = col, border = border)
        axis(1, at = xValue, labels = names(xtable),las = las)
        axis(2)
        axis(4, at = percentVec*(sumFreq), labels = percentVec)
        mtext(4, text = "Cumulative Percentage", line = 3)
        lines(xValue, cumFreq, col = corp.border)
        points(xValue, cumFreq, col = corp.border, pch = 15)
    
        #TODO reload oldpar!
    
        par(fig = c(0,1, 0, tablespace), new =TRUE) 
        mymai[1] = 0
        mymai[3] = 0
        par(mai = mymai)
        plot(xValue, rep(1, length(xValue)),xlim = c(0.2, 1.2*length(xtable)), ylim = c(0, 5), axes = FALSE, ylab = "", type = "n")
        axis(2, pos = 0.2, at = 1:4, labels = rev(c(ylab, paste("Cum.", ylab), "Percentage", "Cum. Percentage")), tick = FALSE, las = 1)

        #Häufigkeiten und Prozentwerte in die Grafik schreiben
        numCol = dim(frameInt)[2]
        numRow = dim(frameInt)[1]
        for(i in 1:numCol)
        {
         for(j in 1:numRow)
         {
          text(xValue[i], numRow+1 - j, round(frameInt[j,i]), adj = c(1,0.5))
         }
        }
    }
    #keine Tabelle unterhalb der Grafik anzeigen
    else
    {
      mymai[2] = mymai[4]
      par(mai = mymai)
      xValue = barplot(xtable, axes = FALSE, las = las, width = 1, space = 0.2, xlim = c(0.2, 1.2*length(xtable)),   main = main, ylim = c(0, sum(xtable)+ 0.01*(sum(xtable))), ylab = ylab, xlab = xlab, col = col, border = border)
      axis(1, at = xValue, labels = names(xtable),las = las)
      axis(2)
      axis(4, at = percentVec*(sumFreq), labels = percentVec)
      mtext(4, text = "Cumulative Percentage", line = 3)
      lines(xValue, cumFreq, col = corp.border)
      points(xValue, cumFreq, col = corp.border, pch = 15)
    }
    
  }
  else
  {
    warning("data should have at least two categories!")
  }
  
  #formatierte Ausgabe des Datensatzes
  frameOut = frameInt
  for (i in 3:nrow(frameInt))
  {
    frameInt[i,] = sprintf("%.1f%%", frameInt[i,])
  } 
  cat(paste("Pareto Analysis for", varName,"\n"))
  cat("---\n")
  print(frameInt)
  cat("\n")

    
  frameOut;

}


###Testen der Funktion
#defect = c(23, 18, 11, 90, 75)
#names(defect) = c("Dichtung leckt", "Riss im Schlauch", "Kratzer", "Einschlüsse", "fehlende Mutter")
#paretoChart(as.table(defect))
#paretoChart(as.table(defect), las = 3)
#

####Datensatz aus Montgomery - Introduction to statistical quality control
#defect = c(36, 34, 29, 17, 13, 6, 6, 5, 4,3,3,2,2,2,2,1,1,1,1)
#names(defect) = c("Incorrect Dimensions", "Parts damaged", "Machining problems", "Masking insufficient", "Supplied rusted parts", "Adhesive failure", "Salt-spray failure", "Film on Parts", "Processing out of Order", "Unfinished fairing", "Wrong part issued", "Delaminated composite", "Misaligned weld", "Paint out of limits",  "Voids in casting", "Improper test procedure", "Paint damaged by etching", "Powdery alodine", "Primer cans damaged")
#paretoChart(as.table(defect), las = 3, main = "", col = rgb(154/255, 192/255, 255/255), border = rgb(154/255, 192/255, 255/255))
#
#
#
#
#
#paretoChart(sample(LETTERS[1:10],30, replace = T))
#
#weight = as.vector(defect)
#weight
#names(weight) = names(defect)
#paretoChart(as.table(defect), weight = weight,las = 3)
#

