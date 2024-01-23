makeTemplateSignatureData <- function(SelectedTemplateSignature, 
                                      Signature){
  Signature <- 
    merge(Signature, 
          SelectedTemplateSignature, 
          by.x = "OID", 
          by.y = "ParentRole", 
          all.x = TRUE)
  is_active <- vapply(Signature$IsActive.x & 
                        Signature$IsActive.y, 
                      isTRUE, 
                      logical(1))
  Signature <- Signature[is_active, ]
  Signature <- Signature[order(Signature$Order), ]
  Signature[c("RoleName")]
}
