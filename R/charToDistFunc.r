#returns the corresponding quantile function for the character 'distribution'
#this is a helper function
#type - r, q, d, and p  
.charToDistFunc = function(distribution, type = "q")
{
   fun = NULL
 
   if(identical("beta",distribution))
    fun = eval(parse(text = paste(type, "beta", sep = "")))

   if(identical("cauchy",distribution))
     fun = eval(parse(text = paste(type, "cauchy", sep ="")))

   if(identical("chi-squared",distribution))
     fun = eval(parse(text = paste(type, "chisq", sep ="")))
     
   if(identical("exponential",distribution))
     fun = eval(parse(text = paste(type, "exp", sep ="")))
     
   if(identical("f",distribution))
     fun = eval(parse(text = paste(type, "f", sep ="")))
        
   if(identical("geometric",distribution))
     fun = eval(parse(text = paste(type, "geom", sep ="")))
   
   if(identical("log-normal",distribution))
     fun = eval(parse(text = paste(type, "lnorm", sep ="")))
        
   if(identical("logistic",distribution))
     fun = eval(parse(text = paste(type, "logis", sep ="")))
     
   if(identical("negative binomial",distribution))
     fun = eval(parse(text = paste(type, "nbinom", sep ="")))
     
   if(identical("normal",distribution))
     fun = eval(parse(text = paste(type, "norm", sep ="")))

   if(identical("poisson",distribution))
     fun = eval(parse(text = paste(type, "pois", sep ="")))

   if(identical("t",distribution))
     fun = eval(parse(text = paste(type, "t", sep ="")))
   
   if(identical("weibull",distribution))
     fun = eval(parse(text = paste(type, "weibull", sep ="")))
   
   if(identical("gamma",distribution))
    fun = eval(parse(text = paste(type, "gamma", sep = "")))

#   print(as.character(fun))
   return(fun)

}
