#TODO: eine order bzw. sort Funktion fr fdo erstellen
#TODO: as.data.frame fr fdo muss Blocking Struktur enthalten. Standardmaeßig ein einziger Block!
#TODO:
#summary laeuft immer nach der RunOrder
# [ und ]
#StandardOrder und RunOrder, als auch Blocking-Struktur wird standardmaeßig in as.data.frame inteegriert stanardmaeßig ein einziger Block (B1) und eine Kodierung (Block)
#Es wird umsortiert --> mit StandardOrder und Anzahl replicates, centerCube, centerStar habe ich immer die Grundordnung
#Es wird ein response Wert veraendert --> check if numeric ansonsten kein Problem
#StandardOrder koennte auch einfach durch die row.names des data.frames gegeben sein
#Funktion star erstellt ein Sterndesign und loescht ein bereits vorhandenes
#star(fdo, add = TRUE) laesst das Hinzufgen einzelner Runs zu
#fdo = centerCube(fdo, num = 1, add = TRUE) laesst das Hinzufgen einzelner .centerPoints zu
#fdo = centerStar(fdo, num = 1, add = TRUE) laesst das Hinzufgen einzelner .centerPoints im Stern zu
#Blocking Scheme for full factorial Designs only

###########################################
#translates chars to numbers
.letToNum = function(charVec)
{
  charVec = toupper(charVec)
  numVec = numeric(length = length(charVec))
 
  if(!is.character(charVec))
    stop(".letToNum: characterVector needs to be provided!")
 
  alphabet = LETTERS[1:26]
 
  for(i in seq(along = charVec))
  {
   numVec[i] = match(charVec[i], alphabet)
  }
  
  return(numVec)
}

#.letToNum(c("a", "e")) 

#helper function
.letterIndex = function(char)
{
  if(char %in% LETTERS[1:26])
  {
      return((1:26)[LETTERS[1:26] == char])
  }
    stop("no valid LETTER specified!")
}

###helper function###
.isEven = function(x)
{
  if(x %% 2 > 0)
    return(FALSE)
  
  return(TRUE)
}

.isOdd = function(x)
{
  return(!.isEven(x))
}

#helper function
#.lociv = length of chars in vec
.lociv = function(charVec)
{
  lenVec = numeric(length = length(charVec))
  for(i in seq(along = charVec))
  {
   lenVec[i] = length(strsplit(charVec[i], split = "")[[1]])
  }

  return(lenVec)
}
#.lociv(c("A", "BFGDF"))


#helper function
.confoundings = function(blockGenVec, lSet, DB = FALSE)
{
  biVec = character(0)
  
  for(i in 2:length(blockGenVec))
  {
    mat = combn(blockGenVec, i)
    temp = apply(mat, 2, strsplit, split = "")
    comb = lapply(temp, unlist)
    comb = lapply(comb, c, lSet)
    
    if(DB)
    {
      print("here")
      print(comb)
    }
    
    combFreq = sapply(comb, table) %% 2
    combBool = !apply(combFreq, 2, as.logical)                       
    
    chars = row.names(combFreq)
    
    if(DB)
      print(combBool)
    
    biTemp = character(0)
    for(j in 1:ncol(combBool))
    {
      biTemp = c(biTemp,paste(chars[combBool[,j]], collapse = "")) 
    }                                                                         

    if(DB)
      print(biTemp)
 
    biVec = c(biVec, biTemp) 

  }
  return(c(blockGenVec,biVec)) #Block interactions vec
}

#.confoundings(blockGenVec)
#.confoundings(c("ABCD", "ABC",  "AB"), lSet)
#




###############################################################################################################################
##Blocking Arrangements from Myers, Montgomery, Cook -Response Surface Methodology- 2009 p. 129, table 3.20
.rsm = vector(mode = "list", length = 7)
.rsm[[1]] = list(k = 3, blocks = 2, gen = c("ABC"))
.rsm[[2]] = list(k = 3, blocks = 4, gen = c("AB", "AC"))
.rsm[[3]] = list(k = 4, blocks = 2, gen = c("ABCD"))
.rsm[[4]] = list(k = 4, blocks = 4, gen = c("ABC", "ACD"))
.rsm[[5]] = list(k = 4, blocks = 8, gen = c("AB", "BC", "CD"))
.rsm[[6]] = list(k = 5, blocks = 2, gen = c("ABCDE"))
.rsm[[7]] = list(k = 5, blocks = 4, gen = c("ABC", "CDE"))
.rsm[[8]] = list(k = 5, blocks = 8, gen = c("ABE", "BCE", "CDE"))
.rsm[[9]] = list(k = 5, blocks = 16, gen = c("AB", "AC", "CD", "DE"))
.rsm[[10]] = list(k = 6, blocks = 2, gen = c("ABCDEF"))
.rsm[[11]] = list(k = 6, blocks = 4, gen = c("ABCF", "CDEF"))
.rsm[[12]] = list(k = 6, blocks = 8, gen = c("ABEF", "ABCD", "ACE"))
.rsm[[13]] = list(k = 6, blocks = 16, gen = c("ABF", "ACF", "BDF", "DEF"))
.rsm[[14]] = list(k = 6, blocks = 32, gen = c("AB", "BC", "CD", "DE", "EF"))
.rsm[[15]] = list(k = 7, blocks = 2, gen = c("ABCDEFG"))
.rsm[[16]] = list(k = 7, blocks = 4, gen = c("ABCFG", "CDEFG"))
.rsm[[17]] = list(k = 7, blocks = 8, gen = c("ABC", "DEF", "AFG"))
.rsm[[18]] = list(k = 7, blocks = 16, gen = c("ABD", "EFG", "CDE", "ADG"))
.rsm[[19]] = list(k = 7, blocks = 32, gen = c("ABG", "BCG", "CDG", "DEG", "EFG"))
.rsm[[20]] = list(k = 7, blocks = 64, gen = c("AB", "BC", "CD", "DE", "EF", "FG"))


#@return - effects for blocking
#alternate name could be block for this function and blocks for accessor function of a facDesign object
#useTable - "rsm" use Suggested Blocking Arrangements Myers, Montgomery, Cook -Response Surface Methodology- 2009
#           "calc" calculate takes time
#           "qualityTools"  use precalculated tab          
.blockInteractions = function(fdo, blocks = 2, useTable = "rsm")
{
  DB = FALSE
  
  if(!(blocks %in% c(0,1,2,4,8,16,32,64)))
    stop("blocks needs to be a power of 2 up to 64!")
  
  gen = NULL
  
  if(blocks %in% c(0,1))
  {
    if(DB)
      print("TODO: Return the Identity as generator")
      
    return(gen)
  }
  
  if(length(useTable) > 0)
  {
    #calculate k number of factors
    if(!(nrow(unique(cube(fdo))) >= 2^.numFac(fdo)))
      stop("no blocking of a fractional factorial Design --> block on replicates instead!")
  
   if(identical(useTable, "rsm"))
   {
   
    for(i in seq(along = .rsm))
    {
     if(.rsm[[i]]$k == .numFac(fdo) & .rsm[[i]]$blocks == blocks)
        return(.rsm[[i]]$gen)
    }
   }
   return(gen)
  }

  bgaci = matrix(nrow = 0, ncol = blocks - 1)  #block generators and confounded interactions, N-1 effects can be calculated from N different combinations
  #bgaci = vector(mode = "list", length = nrow (unique(as.data.frame(fdo))))
    
  if(!is.numeric(blocks))
    stop("blocks must be an integer")

  numCol = log2(blocks) #number of columns needed for the blocking
  
  blockGen = character(3) #block generators
  
  lSet = names(names(fdo))
  
  #Fr die Blockin Spalten kommen nur Wechselwirkungen in Frage!!!!
  #Alle moeglichen Wechselwirkungen erstellen
  sSet = vector(mode = "list")
  for(i in length(lSet):2)
  {
   sSet = c(sSet,combn(lSet, i, simplify = FALSE))
  }
  


  #return highest interaction for blocking  
  if(blocks == 2)
  {
    index = order(sapply(sSet,length), decreasing = TRUE)[1]
    sSet = sapply(sSet, paste,  collapse = "")
    return(sSet[index])
  }

  sSet = sapply(sSet, paste,  collapse = "")

  if(DB)
    print(sSet)  

  
  possGen = combn(sSet, numCol, simplify = FALSE) #possible Block  Generators
  
  
  for(i in seq(along = possGen))
  {
    blockGenVec = unlist(possGen[[i]])

    if(DB)
      print(blockGenVec)
    
    #alle Kombinationen aus diesen BlockGenerators bilden
    if(DB)
      print(.confoundings(blockGenVec, lSet))
    #bgaci[[i]] = .confoundings(blockGenVec, lSet)
    newRow = .confoundings(blockGenVec, lSet)

    if(!any(newRow %in% c(lSet, "")))
      bgaci = rbind(bgaci,.confoundings(blockGenVec, lSet))
  }
  
  #experimental - find the best block generators
  mat = unique(t(apply(bgaci, 1, sort)))
  temp = t(apply(mat, 1, .lociv))
  temp = t(apply(temp, 1, sort))

  ref = temp[1,]
  index = 1
  for(i in 1:nrow(temp))
  {
    if(any((ref - temp[i,]) < 0))
    {
      ref = temp[i,]
      index = i
    }
  }
  for(i in 1:nrow(temp))
  {
    if(!(any(ref - temp[i,] > 0) | any(ref - temp[i,] < 0)))
    {
      index = c(index,i)
    }
  }
  
  #all interactions that are confounded with blocks
  temp = unique((mat[index,]))
  cat("\nSuggested Effects for Blocking:")
  cat("\n")
  cat(temp[1, 1:numCol])
  cat("\n")
  cat("\nInteractions Confounded with blocks:")
  cat("\n")
  cat(unique(temp[1,]))
  cat("\n")
  cat("\n Alternate Effects for Blocking:")
  cat(temp[c(-1), 1:numCol])
  cat("\n")
  
#  gen = temp[1:numCol]
  gen = temp[1,1:numCol]
  return(gen)
}

#fdo = facDesign(k = 4 , replicates = 2)
#tt = .blockInteractions(fdo, 4, DB = FALSE)

#calculates the BlockGenerator columns needed to make the block column
.blockGenCol = function(gen, fdo)
{
  DB = FALSE
  blockVec = NULL
  .blockCol = NULL

  genList =  gen
  genList = strsplit(genList, split = "")
  
  .fdo = fdo

  #Spalten auswaehlen fr Generator
  #Spalten multiplizieren
  for(i in seq(along = genList))
  {
    gen = genList[[i]]
    
    for(j in seq(along = gen))
    {
      genTemp = .fdo[, gen[j]]
      
      if(j == 1)
        blockVec = rep(1, length = length(genTemp))
      
      blockVec = blockVec*genTemp
        
      if(DB)
        print(blockVec)
    }
    
    if(i == 1)
      .blockCol = data.frame(B1 = blockVec)
    else
      .blockCol = cbind(.blockCol, blockVec)
  
  }
  
  #Bloecke beschriften
  names(.blockCol) = paste("B", 1:ncol(.blockCol), sep = "")
  return(.blockCol)
}

#fdo = fracDesign(k = 3)
#ttt = .blockGenCol("AB", fdo)



#takes .blockGenCol (i.e. columns chosen to generate blocking structure) and calculates the Block Column for the fdo object
#@return - block Column for object of class fdo
.blockCol = function(.blockGenCol)
{
  DB = FALSE 
  
  .blockCol = numeric(nrow(.blockGenCol))
  uniCol = unique(.blockGenCol)
  for(i in 1:nrow(uniCol))
  {
    if(ncol(uniCol) == 1)
      .blockCol[apply(t(as.data.frame(apply(.blockGenCol, 1, "==", uniCol[i,]))), 2, all)] = i
    else
      .blockCol[apply(apply(.blockGenCol, 1, "==", uniCol[i,]), 2, all)] = i
  }
  
  #jetzt die centerCube Points behandeln
  
 return(data.frame(Block = .blockCol))
}

#fdo = facDesign(k = 3 , replicates = 1)
#tt = .blockInteractions(fdo, 4, DB = FALSE)
#t1 = .blockGenCol(tt, fdo)
#blockGen(fdo) =  t1
#block(fdo) = .blockCol(t1)
#


#function to randomize the runOrder within blocks
#TODO: in facDesign integrieren
#so - if TRUE no randomization will take place but standardOrder (i.e. so)
randomize = function(fdo, random.seed, so = FALSE)
{
  if(missing(random.seed))
    set.seed(93275938)
  else
    set.seed(random.seed)
  
    
  j = 1
  temp = runOrd(fdo)
  for(i in sort(unique(block(fdo)[,1])))
  {
    # match(block(fdo)[,1],sort(block(fdo)[,1])[1])
    pos = !is.na(match(block(fdo)[,1],i))  #Position
    count = sum(as.numeric(pos))  #Anzahl gefundener Elemente aufaddieren
    if(so)
    {
       temp[pos,1] = j:(j + (count-1))
    }
    else
    {
      temp[pos,1] = sample(j:(j + (count-1)), count)  #run Order aufsteigend innerhalb der Bloecke randomisieren    
    }
    j = j + count 
  }

  runOrd(fdo) = temp
  
  #return the randomized factorial design
  return(fdo)
}

#fdo = facDesign(k = 3 , replicates = 1)
#randomize(fdo)
#


###block a 2^k factorial design
#BoR - Block on Replicates Flag
blocking = function(fdo, blocks, BoR = FALSE, random.seed, useTable = "rsm", gen)
{
  if(missing(random.seed))
  {
    runif(1)  #if .Random.Seed does not exist anymore
    random.seed = .Random.seed[sample(1:626,1)]
  }
  
  if(missing(gen))
    gen = NULL

  #0 Bloecke sind technisch gesehen kein Block und somit ein Block
  if(blocks < 1)
    blocks = 1
  
  if(blocks == 1)
  {
   block(fdo)[,1] = 1
   fdo = randomize(fdo, random.seed = random.seed)
   return(fdo)
  }
  
  #eine spaeter angehaengter star oder centerStar part kriegt eine eigene block Nummer
  if(nrow(star(fdo)) > 0 | nrow(centerStar(fdo)) > 0)
  {
    if(blocks %in% c(2,3,5,9,17))
        blocks = blocks - 1
    else
      stop("Blocking not possible")
  }
  else
  {
    if(!(blocks %in% c(1,2,4,8,16,32,64,128)))
      stop("Blocking not possible")
  }

  #if no generators were specified calculate tmeh
  if(is.null(gen))
    gen = .blockInteractions(fdo, blocks, useTable)
  
  
  if(is.null(gen))
  {
    cat("\n")
    cat(paste("Blocking in", blocks, "blocks not possible!"))
    cat("\n")
    return(fdo)
  }
    
  .blockGenCol = .blockGenCol(gen, fdo)
  .blockCol = .blockCol(.blockGenCol)
  
  block(fdo) = .blockCol
  blockGen(fdo) = .blockGenCol
  
  #centerCubePoints werden blockmal eingehaengt  
  numCC = nrow(centerCube(fdo))
  if(numCC > 0)
  {
    #numCC mal pro Block anhaengen
    ccFrame = as.data.frame(matrix(0, nrow = numCC, ncol = ncol(cube(fdo))))
    names(ccFrame) = names(names(fdo))
    centerCube(fdo) = ccFrame
  } 

  #innerhalb jedes Blocks randomisieren!
  fdo = randomize(fdo, random.seed = random.seed)

  return(fdo)
}

#fdo = facDesign(k = 3 , replicates = 1)
#centerCube(fdo) = data.frame(A = 0, B = 0, C = 0)
#blocking(fdo,2)
#
#fdo
#randomize(fdo)
#
#blocking(fdo,4)
#



################check the calculation of blocking arrangements#########################
##passt!
#fdo = facDesign(k = 3 ,replicates = 2)
#tt = .blockInteractions(fdo, 4)
#
##passt!
#fdo = facDesign(k = 4 , replicates = 2)
#tt = .blockInteractions(fdo, blocks = 8)
#
##passt!
#fdo = facDesign(k = 5 ,replicates = 2)
#ttt = blocking(fdo, 8)
#
##passt!
#fdo = facDesign(k = 5 ,replicates = 2)
#ttt = blocking(fdo, 4)
#
##passt!
#fdo = facDesign(k = 5 ,replicates = 2)
#ttt = blocking(fdo, 8)
#
##passt
#fdo = facDesign(k = 5 ,replicates = 2)
#ttt = blocking(fdo, 16)
#
#
####Use of Blocking in fdo - Objects###
##TODO: include in block function
#


