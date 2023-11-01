makeTemplateDisclaimerData <- function(SelectedTemplateDisclaimer, 
                                       Disclaimer){
  Disclaimer <- 
    merge(Disclaimer, 
          SelectedTemplateDisclaimer, 
          by.x = "OID", 
          by.y = "ParentDisclaimer", 
          all.x = TRUE)
  is_active <- vapply(Disclaimer$IsActive.x & 
                        Disclaimer$IsActive.y, 
                      isTRUE, 
                      logical(1))
  Disclaimer <- Disclaimer[is_active, ]
  Disclaimer <- Disclaimer[order(Disclaimer$Order), ]
  Disclaimer[c("Disclaimer")]
}
