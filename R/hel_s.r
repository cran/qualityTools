.lfrm = function(wholeList, filterList) {
    if (!is.list(wholeList)) 
        stop(paste(deparse(substitute(wholeList)), "is not a list!"))
    if (length(wholeList) == 0) 
        return(wholeList)
    if (!is.list(filterList)) 
        stop(paste(deparse(substitute(filterList)), "is not a list!"))
    if (length(filterList) == 0) 
        return(wholeList)
    logVec = lapply(names(wholeList), "%in%", names(filterList))
    filteredList = wholeList[!unlist(logVec)]
    return(filteredList)
}
.lfkp = function(wholeList, filterList) {
    if (!is.list(wholeList)) 
        stop(paste(deparse(substitute(wholeList)), "is not a list!"))
    if (length(wholeList) == 0) 
        return(wholeList)
    if (!is.list(filterList)) 
        stop(paste(deparse(substitute(filterList)), "is not a list!"))
    if (length(filterList) == 0) 
        return(filterList)
    logVec = lapply(names(wholeList), "%in%", names(filterList))
    filteredList = wholeList[unlist(logVec)]
    return(filteredList)
} 
