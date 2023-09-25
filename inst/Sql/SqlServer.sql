/* User Table  ***********************************************/

CREATE TABLE dbo.[User](
  OID INT IDENTITY(1, 1) NOT NULL, 
  LastName VARCHAR(50) NOT NULL, 
  FirstName VARCHAR(50) NOT NULL,
  LoginId VARCHAR(50) NOT NULL, 
  EmailAddress VARCHAR(100) NULL, 
  IsInternal BIT NOT NULL DEFAULT 1, 
  IsActive BIT NOT NULL DEFAULT 1, 
  
  PRIMARY KEY (OID)
);

/* UserEvent Table *******************************************/

CREATE TABLE dbo.[UserEvent](
  OID INT IDENTITY(1, 1)  NOT NULL, 
  ParentUser INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(50) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(200) NULL, 
  
  PRIMARY KEY (OID), 
  FOREIGN KEY (ParentUser) REFERENCES dbo.[User](OID), 
  FOREIGN KEY (EventUser) REFERENCES dbo.[User](OID), 
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

CREATE TABLE dbo.[Role](
  OID INTEGER IDENTITY(1, 1) NOT NULL, 
  RoleName VARCHAR(75) NOT NULL, 
  RoleDescription VARCHAR(250), 
  IsActive BIT NOT NULL DEFAULT 1, 
  
  PRIMARY KEY (OID)
);

/* RoleEvent Table *************************************************/

CREATE TABLE dbo.[RoleEvent](
  OID INTEGER IDENTITY(1, 1) NOT NULL,
  ParentRole INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
  PRIMARY KEY (OID),
  FOREIGN KEY (ParentRole) REFERENCES dbo.[Role](OID), 
  FOREIGN KEY (EventUser) REFERENCES dbo.[User](OID), 
  CONSTRAINT chk_RoleEventType CHECK (EventType IN ('EditRoleDescription',
                                                          'EditRoleName', 
                                                          'Deactivate', 
                                                          'Activate', 
                                                          'Add'))
);

/* UserRole Table ********************************************/

CREATE TABLE dbo.[UserRole] (
  OID INTEGER IDENTITY(1, 1) NOT NULL, 
  ParentUser INT NOT NULL, 
  ParentRole INT NOT NULL, 
  IsActive BIT NOT NULL DEFAULT 0, 
  
  PRIMARY KEY (OID),
  FOREIGN KEY (ParentUser) REFERENCES dbo.[User](OID),
  FOREIGN KEY (ParentRole) REFERENCES dbo.[Role](OID)
);

/* UserRoleEvent Table ***************************************/

CREATE TABLE dbo.[UserRoleEvent] (
  OID INTEGER IDENTITY(1, 1) NOT NULL, 
  ParentUserRole INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
  PRIMARY KEY (OID),
  FOREIGN KEY (ParentUserRole) REFERENCES dbo.[UserRole](OID), 
  FOREIGN KEY (EventUser) REFERENCES dbo.[User](OID),
  CONSTRAINT chk_UserRoleEventType CHECK (EventType IN ('Add', 
                                                              'Activate', 
                                                              'Deactivate'))
);
