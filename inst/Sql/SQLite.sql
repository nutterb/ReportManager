/* User Table  ***********************************************/

CREATE TABLE [User](
  OID INTEGER PRIMARY KEY, 
  LastName VARCHAR(50) NOT NULL, 
  FirstName VARCHAR(50) NOT NULL,
  LoginId VARCHAR(50) NOT NULL, 
  EmailAddress VARCHAR(100) NULL, 
  IsInternal BIT NOT NULL DEFAULT 1, 
  IsActive BIT NOT NULL DEFAULT 1
);

/* UserEvent Table *******************************************/

CREATE TABLE [UserEvent](
  OID INTEGER PRIMARY KEY, 
  ParentUser INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(200) NULL, 
  
  FOREIGN KEY (ParentUser) REFERENCES [User](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID), 
  CONSTRAINT chk_UserEventType CHECK (EventType IN ('SetInternalTrue', 
                                                          'SetInternalFalse', 
                                                          'EditEmailAddress', 
                                                          'EditLastName',
                                                          'EditFirstName',
                                                          'EditLoginId', 
                                                          'Deactivate', 
                                                          'Activate', 
                                                          'Add'))
);

/* Role Table ******************************************************/

CREATE TABLE [Role](
  OID INTEGER PRIMARY KEY, 
  RoleName VARCHAR(75) NOT NULL, 
  RoleDescription VARCHAR(250), 
  IsActive BIT NOT NULL DEFAULT 1
);

/* RoleEvent Table *************************************************/

CREATE TABLE [RoleEvent](
  OID INTEGER PRIMARY KEY, 
  ParentRole INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
  FOREIGN KEY (ParentRole) REFERENCES [Role](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID), 
  CONSTRAINT chk_RoleEventType CHECK (EventType IN ('EditRoleDescription',
                                                    'EditRoleName', 
                                                    'Deactivate', 
                                                    'Activate', 
                                                    'Add'))
);


/* UserRole Table ********************************************/

CREATE TABLE [UserRole] (
  OID INTEGER PRIMARY KEY, 
  ParentUser INT NOT NULL, 
  ParentRole INT NOT NULL, 
  IsActive BIT NOT NULL DEFAULT 0, 
  
  FOREIGN KEY (ParentUser) REFERENCES [User](OID),
  FOREIGN KEY (ParentRole) REFERENCES [Role](OID)
);

/* UserRoleEvent Table ***************************************/

CREATE TABLE [UserRoleEvent] (
  OID INTEGER PRIMARY KEY, 
  ParentUserRole INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
  FOREIGN KEY (ParentUserRole) REFERENCES [UserRole](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID),
  CONSTRAINT chk_UserRoleEventType CHECK (EventType IN ('Add', 
                                                        'Activate', 
                                                        'Deactivate'))
);
