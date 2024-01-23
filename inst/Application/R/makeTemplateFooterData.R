makeTemplateFooterData <- function(SelectedTemplateFooter, 
                                   Footer){
  Footer <- 
    merge(Footer, 
          SelectedTemplateFooter, 
          by.x = "OID", 
          by.y = "ParentFooter", 
          all.x = TRUE)
  is_active <- vapply(Footer$IsActive.x & 
                        Footer$IsActive.y, 
                      isTRUE, 
                      logical(1))
  Footer <- Footer[is_active, ]
  Footer <- Footer[order(Footer$Order), ]
  Footer[c("Footer")]
}
