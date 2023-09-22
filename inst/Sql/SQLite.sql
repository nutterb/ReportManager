/* ReportUser Table  ***********************************************/

CREATE TABLE [ReportUser](
  OID INTEGER PRIMARY KEY, 
  LastName VARCHAR(50) NOT NULL, 
  FirstName VARCHAR(50) NOT NULL,
  LoginId VARCHAR(50) NOT NULL, 
  EmailAddress VARCHAR(100) NULL, 
  IsInternal BIT NOT NULL DEFAULT 1, 
  IsActive BIT NOT NULL DEFAULT 1
);

/* ReportUserEvent Table *******************************************/

CREATE TABLE [ReportUserEvent](
  OID INTEGER PRIMARY KEY, 
  ParentReportUser INT NOT NULL, 
  EventReportUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(200) NULL, 
  
  FOREIGN KEY (ParentReportUser) REFERENCES [ReportUser](OID), 
  FOREIGN KEY (EventReportUser) REFERENCES [ReportUser](OID), 
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
  EventReportUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
  FOREIGN KEY (ParentRole) REFERENCES [Role](OID), 
  FOREIGN KEY (EventReportUser) REFERENCES [ReportUser](OID), 
  CONSTRAINT chk_RoleEventType CHECK (EventType IN ('EditRoleDescription',
                                                    'EditRoleName', 
                                                    'Deactivate', 
                                                    'Activate', 
                                                    'Add'))
);


/* ReportUserRole Table ********************************************/

CREATE TABLE [ReportUserRole] (
  OID INTEGER PRIMARY KEY, 
  ParentReportUser INT NOT NULL, 
  ParentRole INT NOT NULL, 
  IsActive BIT NOT NULL DEFAULT 0, 
  
  FOREIGN KEY (ParentReportUser) REFERENCES [ReportUser](OID),
  FOREIGN KEY (ParentRole) REFERENCES [Role](OID)
);

/* ReportUserRoleEvent Table ***************************************/

CREATE TABLE [ReportUserRoleEvent] (
  OID INTEGER PRIMARY KEY, 
  ParentReportUserRole INT NOT NULL, 
  EventReportUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  
  FOREIGN KEY (ParentReportUserRole) REFERENCES [ReportUserRole](OID), 
  FOREIGN KEY (EventReportUser) REFERENCES [ReportUser](OID),
  CONSTRAINT chk_ReportUserRoleEventType CHECK (EventType IN ('Add', 
                                                              'Activate', 
                                                              'Deactivate'))
);
