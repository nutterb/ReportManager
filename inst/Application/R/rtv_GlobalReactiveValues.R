..rtv_currentUserOid <- function(rv_User){
  rv_User$User$OID[rv_User$User$LoginId %in% Sys.info()["login"]]
}

..rtv_currentUserRole <- function(current_user_oid){
  UserRole <- queryUserRole(user_oid = current_user_oid) 
  UserRole[UserRole$IsActive & 
             UserRole$IsActiveUser & 
             UserRole$IsActiveRole, ]
}

..rtv_userHasRole <- function(role, 
                              current_user_role){
  role %in% 
    current_user_role$RoleName[current_user_role$IsActiveRole]
}