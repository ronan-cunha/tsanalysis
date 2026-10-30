isat_outlier<-function(y){
  
  misat <- gets::isat(y, 
                      iis = TRUE,
                      sis = TRUE,
                      tif = FALSE,
                      t.pval=0.01,
                print.searchinfo=FALSE)
  return(residuals(misat))
}