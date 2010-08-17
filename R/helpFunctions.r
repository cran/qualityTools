##########help functions for package qualityTools / DAP ####################


#sort out elements wholeList that are in filterList
#used in qq.plot 
#.lfrm - list filter remove
.lfrm = function(wholeList, filterList)
{
   if(!is.list(wholeList))
    stop(paste(deparse(substitute(wholeList)), "is not a list!")) 

   if(length(wholeList) == 0)
    return(wholeList)
   
   if(!is.list(filterList))
    stop(paste(deparse(substitute(filterList)), "is not a list!")) 
    
   if(length(filterList) == 0)
    return(wholeList)
    
   logVec = lapply(names(wholeList), "%in%", names(filterList))
   filteredList = wholeList[!unlist(logVec)]

   return(filteredList)
}

#l1 = list(main = "nicht in l2 enthalten",col = "red", pch = 19)
#l2 = par()
#.lfrm(l1, l2)


#keep elements in wholeList that are in filterList
#used in qq.plot 
#lfrkp - list filter keep
.lfkp = function(wholeList, filterList)
{
   if(!is.list(wholeList))
    stop(paste(deparse(substitute(wholeList)), "is not a list!")) 
   
   if(length(wholeList) == 0)
    return(wholeList)
   
   if(!is.list(filterList))
    stop(paste(deparse(substitute(filterList)), "is not a list!")) 
   
   if(length(filterList) == 0)
    return(filterList)
    
   logVec = lapply(names(wholeList), "%in%", names(filterList))
   filteredList = wholeList[unlist(logVec)]

   return(filteredList)
}

#l1 = list(main = "nicht in l2 enthalten",col = "red", pch = 19)
#l2 = par()
#.lfkp(l1, l2)
#
#.lfkp(list(main = "test", p = 0.4), formals(qnorm))
