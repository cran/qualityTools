dotPlot = function(x, group, xlim, ylim, col, xlab, ylab, pch, cex, breaks, stacked = TRUE, ...)
{
  DB = FALSE
  pch.size = "O"  #reguliert wie dicht die dots nebeneinander liegen
  grouped = TRUE  #gruppierte Darstellung?
  
  
  #erstelle eine Matrix in der die Punkte einsortiert werden
  #sortiere alle rows raus in denen keine Punkte enthalten sind

  #if grouped sortiere die weitere Gruppe ein

  #jede Spalte des data.frames ist ein x-Slot
  #jede Zeile ein y Slot

  parList = list(...)

  if(missing(xlab))
    xlab = deparse(substitute(x))
  
  x = x[!is.na(x)]

  if(missing(xlim))
    xlim = range(x)
    
  if(missing(ylim))
    ylim = c(0,1)

  
  if(missing(ylab))
    ylab = ""

  if(missing(cex))
    cex = 1
  
  if(missing(group))
    group = rep(1, length(x))

  if(length(unique(group)) == 1)  #keine Gruppe oder nur eine angegeben
    grouped = FALSE

  if(missing(pch) || length(unique(group)) > length(pch))
    pch = 1:length(unique(group))

  if(missing(col) || length(unique(group)) > length(col))
    col = 1:length(unique(group))

  if(missing(breaks))
  {
    plot(1,1, xlim = xlim, ylim = ylim, type = "n", axes = FALSE, cex = cex, xlab = xlab, ylab = ylab)
    slotSizeX = strwidth(pch.size, units = "user", cex = cex)
    
    if(DB)
      print(paste("slotSizeX:",slotSizeX))
    
    #calculation of the breaks for the histogram  
    span = diff(range(x))
    temp1 = ppoints(2*ceiling(span/slotSizeX))    
    temp2 = numeric(length(temp1) + 2)
    temp2[2:(length(temp1)+1)] = temp1
    temp2[1] = temp2[1] - 1.01*diff(c(temp1[1], temp1[2]))
    temp2[length(temp2)] = rev(temp1)[1] + 1.01*diff(c(temp1[1], temp1[2]))
    temp2 = temp2*span + min(x)
    temp = min(x) + ppoints(span/slotSizeX )*span

    breaks = numeric(length(temp) +2)
    breaks[2:(length(temp)+1)] = temp
    breaks[1] = temp[1] - diff(c(temp[1], temp[2]))*1.001
    breaks[length(breaks)] = rev(temp)[1] + diff(c(temp[1], temp[2]))*1.001
    
    breaks = temp2
  }
  
    #calculation of the y coordinates for plotting
    slotSizeY = strheight(pch.size, units = "user", cex = cex)
    
    if(DB)
      print(paste("slotSizeY:",slotSizeY))
      
    span = diff(ylim)
    temp1 = ppoints(2*ceiling(span/slotSizeY))    
    temp2 = numeric(length(temp1) + 2)
    temp2[2:(length(temp1)+1)] = temp1
    temp2[1] = temp2[1] - 1.01*diff(c(temp1[1], temp1[2]))
    temp2[length(temp2)] = rev(temp1)[1] + 1.01*diff(c(temp1[1], temp1[2]))
    yVec = temp2*span + min(ylim)

    if(yVec[1] < 0)
      yVec = yVec + abs(yVec[1])
    else
      yVec = yVec - yVec[1]
    
    if(DB)
      print(paste("temp2:", temp2))
    if(DB)    
      print(paste("breaks:", breaks))






  histObj = hist(x, breaks = breaks, right = FALSE, plot = FALSE)
  hMids = histObj$mids
  hCounts = histObj$counts
  hMids = histObj$mids
  mat = matrix(NA, nrow = length(x), ncol = length(hMids))  #matrix for pch values
  colMat = mat  #matrix for col values
  groupmat = mat  #fuer gruppierte dotplots
  numVec = 1:nrow(mat)
  cutOff = 1


  groupList = vector(mode = "list", length = length(unique(group))) #speichert die Matrix zu jeder Gruppe
  for(k in unique(group))
  {
    histObj = hist(x[group == k], breaks = breaks, plot = FALSE)
    hMids = histObj$mids
    hCounts = histObj$counts
    hMids = histObj$mids


    for(i in seq(along = hMids))
    {
      value = pch[k]
      colValue = col[k]
      from = 0

      from = numVec[is.na(mat[,i])][1]  #starting point setzen

      to = from

     if(hCounts[i] == 0)
       value = NA

     if(hCounts[i] >= 1)
      to = to + hCounts[i] - 1

     if(to > cutOff)
        cutOff = to

      if(DB)
      {
        print(paste("from:", from))
        print(paste("to:", to))
        print(paste("i:", i))
        print(paste("value:", value))
      }
      
       mat[from:to, i] = value
       colMat[from:to, i] = colValue
       

    }
    groupList[[k]] = groupmat
  }

  if(grouped && !stacked)
  {
    groupIndex = unique(group)
    par(mfrow = c(length(groupIndex), 1))
    for(i in groupIndex)
      dotPlot(x[group == i], xlim = xlim, breaks = breaks, cex = cex, xlab = xlab, ylab = ylab, col = col, pch = pch, ...)
  }
  else
  {
    mat = mat[1:cutOff,]

    if(!is.matrix(mat))
      mat = matrix(mat, nrow = 1)
    
    if(DB)  
      print(mat)

    plot(1,1, xlim = xlim, ylim = ylim, type = "n", cex = cex, xlab = xlab, ylab = ylab, ...)
    for(i in 1:nrow(mat))
    {
      x = hMids[!is.na(mat[i,])]
      y = rep(i*0.3, times = length(x))
      y = rep(yVec[i], times = length(x))
      col = colMat[i,!is.na(mat[i,])]
      pch = mat[i,!is.na(mat[i,])]
      points(x,y, col = col, pch = pch, cex = cex)
    }
  }

  if(DB)
    print(hMids)
  
  invisible(mat)

}

#x = rnorm(16)
#dotPlot(x, group = rep(1:2, 16))
#windows()
#x = rnorm(28)
#dotPlot(x, group = rep(1:2, 14), stacked = FALSE, pch = c(19, 20))
#dotPlot(1:10)
#x = rnorm(200)
#group = rep(1:2,100)
#


#dotPlot(x[group == 2])
#windows()
#dotPlot(x, pch = 19:20)
#hist(x)
#windows()
#dotPlot(x)
#







