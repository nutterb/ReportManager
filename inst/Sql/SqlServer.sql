/* ReportUser Table  ***********************************************/

CREATE TABLE dbo.[ReportUser](
  OID INT IDENTITY(1, 1) NOT NULL, 
  LastName VARCHAR(50) NOT NULL, 
  FirstName VARCHAR(50) NOT NULL,
  LoginId VARCHAR(50) NOT NULL, 
  EmailAddress VARCHAR(100) NULL, 
  IsInternal BIT NOT NULL DEFAULT 1, 
  IsActive BIT NOT NULL DEFAULT 1, 
  
  PRIMARY KEY (OID)
);

/* ReportUserEvent Table *******************************************/

CREATE TABLE dbo.[ReportUserEvent](
  OID INT IDENTITY(1, 1)  NOT NULL, 
  ParentReportUser INT NOT NULL, 
  EventReportUser INT NOT NULL, 
  EventType VARCHAR(50) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(200) NULL, 
  
  PRIMARY KEY (OID), 
  FOREIGN KEY (ParentReportUser) REFERENCES dbo.[ReportUser](OID), 
  FOREIGN KEY (EventReportUser) REFERENCES dbo.[ReportUser](OID), 
  CONSTRAINT chk_ReportUserEventType CHECK (EventType IN ('SetInternalTrue', 
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
  EventReportUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
  PRIMARY KEY (OID),
  FOREIGN KEY (ParentRole) REFERENCES dbo.[Role](OID), 
  FOREIGN KEY (EventReportUser) REFERENCES dbo.[ReportUser](OID), 
  CONSTRAINT chk_RoleEventType CHECK (EventType IN ('EditRoleDescription',
                                                          'EditRoleName', 
                                                          'Deactivate', 
                                                          'Activate', 
                                                          'Add'))
);

/* ReportUserRole Table ********************************************/

CREATE TABLE dbo.[ReportUserRole] (
  OID INTEGER PRIMARY KEY, 
  ParentReportUser INT NOT NULL, 
  ParentRole INT NOT NULL, 
  IsActive BIT NOT NULL DEFAULT 0, 
  
  FOREIGN KEY (ParentReportUser) REFERENCES dbo.[ReportUser](OID),
  FOREIGN KEY (ParentRole) REFERENCES dbo.[Role](OID)
);

/* ReportUserRoleEvent Table ***************************************/

CREATE TABLE dbo.[ReportUserRoleEvent] (
  OID INTEGER PRIMARY KEY, 
  ParentReportUserRole INT NOT NULL, 
  EventReportUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  
  FOREIGN KEY (ParentReportUserRole) REFERENCES dbo.[ReportUserRole](OID), 
  FOREIGN KEY (EventReportUser) REFERENCES dbo.[ReportUser](OID),
  CONSTRAINT chk_ReportUserRoleEventType CHECK (EventType IN ('Add', 
                                                              'Activate', 
                                                              'Deactivate'))
);
