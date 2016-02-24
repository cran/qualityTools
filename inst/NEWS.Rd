\name{NEWS}
\title{NEWS file for the qualityTools package}

\section{Changes in version 0.87 (1.41)}{
  \itemize{
  
    \item 04.08.2011: fixed expected fraction nonconforming calculation (ppm) in method pcr
    corrected the calculation of standard deviation based on subgroups
    \item 27. Jul 2011: in vignette, corrected typo in formula for c_pu i.e. capability for process capability regarding upper specification limit
    \item 14. Jul 2011: included the calculation of lambda for a boxcox transformation in function pcr.
    Only specified specification limits are plotted.
    fraction nonconforming are calculated in percentages and ppm
    \item 13. Jul 2011: reference line for Quantil-Quantil and Probability Plots changed
    Anderson Darling test now reports the name of the dataset passed from method pcr
    Reference line of QQ-Plot within method pcr changed
    \item 12. Jul 2011: reference line for normalPlot changed
    Included color coding of sigfnifican effects
    \item 11. Jul 2011: added parameters xlim, ylim and border for qqPlot
    Added parameter border for ppPlot
    
    \item 21. Mar 2011: added missing documentation for S4 classes. 
    Included a visually more appealing DMAIC cycle in the vignette

    \item 26. Feb 2011: Updated the vignette to explain the usage and scope of
    the cg method.
    Updated the vignette to untangle the R-Code used into a file called RCode.R.
    All examples are now easily reproducible.

    \item 11 Feb 2011: gageRRDesign now support nested designs. gageRR can be
    called with method = "nested".
    number of distinctive categories (i.e. ndc) is being calculated within the
    gageRR method
    
    \item 10 Feb 2011: plot method for objects of class gageDesign now includes
    the plotting of Range and Average charts.

    \item 18 Jan 2011: included the gage capability method cg to calculate and
    visualize the cg and cgk index

    \item 10 Dec 2010: gageRR with no interaction term is now calculated
    correctly. Bug in gageRR was found by user.
    
    \item ----qualityTools version 1.50----

    \item included comparison plot (compPlot) for Measurement Systems Analysis i.e. Gage R\&R
    
    \item included average plot (averagePlot) for Measurement Systems Analysis i.e. Gage R\&R
        
    \item inclued whiskers plot (whiskersPlot) for Measurement Systems Analysis i.e. Gage R\&R
    
    \item inclued error plot (errorPlot) for Measurement Systems Analysis i.e. Gage R\&R

    
    \item ----qualityTools Version 1.52----
    
    \item Bootstrap-based Anderson-Darling Test for Univariate Distributions included in method adSim.
    
    \item QQ-Plot-, PP-Plot- and respectively Process Capability methods partly calculate confidence limits
    
    \item included 3 parameter weibull (weibull3), lognormal (lognormal3) and gamma (gamma3)
    
    \item Multi Vari Chart method rewritten
    
    \item normalPlot can handle non replicated designs
    
    \item smaller fixes/enhancements to rsmDesign, fracDesign and facDesign
    

    \item ----qualityTools Version 1.54----
    
    \item fixed a previously introduced error regarding the blocking of factorial designs. The randomized runOrder when creating factorial designs has unfortunately changed with this fix.
    
    \item in normalPlot the level of significance is additionaly represented by different values for pch
    
    \item added parameter in qqPlot and ppPlot for the color of the confidence bounds

    \item added Plackett Burman Designs (Screening)

    \item added snPlot for taguchi Designs
    
    \item smaller enhancements for the use of the qualityTools package in a Web Application http://webapps.r-qualityTools.org
    \item fixed a bug for displaying the desirability value when using a single response
    
    \item smaller changes to the vignettes
    
    
    
}}



