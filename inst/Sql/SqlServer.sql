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

/* Schedule Table **************************************************/

CREATE TABLE dbo.[Schedule] (
  OID INTEGER IDENTITY(1, 1) NOT NULL,  
  ScheduleName VARCHAR(50) NOT NULL, 
  Frequency INT NOT NULL, 
  FrequencyUnit VARCHAR(10) NOT NULL, 
  OffsetOverlap INT NOT NULL, 
  OffsetOverlapUnit VARCHAR(10) NOT NULL, 
  IsActive BIT NOT NULL DEFAULT 0, 
  
  PRIMARY KEY (OID),
);

/* ScheduleEventTable **********************************************/

CREATE TABLE [ScheduleEvent](
  OID INTEGER IDENTITY(1, 1) NOT NULL, 
  ParentSchedule INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
  PRIMARY KEY (OID),
  FOREIGN KEY (ParentSchedule) REFERENCES [Schedule](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID), 
  CONSTRAINT chk_ScheuleEventType CHECK (EventType IN ('EditScheduleName',
                                                       'EditFrequency',
                                                       'EditOverlap',
                                                       'Deactivate', 
                                                       'Activate', 
                                                       'Add'))
);

/* DateReportingFormat Table ***************************************/

CREATE TABLE [DateReportingFormat](
  OID INTEGER IDENTITY(1, 1) NOT NULL, 
  [FormatName] VARCHAR(25) NOT NULL, 
  [Description] VARCHAR(50) NULL,
  [FormatCode] VARCHAR(25) NOT NULL,
  IncrementStart INT NOT NULL, 
  IncrementStartUnit VARCHAR(10) NOT NULL, 
  IncrementEnd INT NOT NULL, 
  IncrementEndUnit VARCHAR(10) NOT NULL, 
  IsActive BIT NOT NULL DEFAULT 0, 
  
  PRIMARY KEY (OID),
);

/* DateReportingFormatEvent Table **********************************/

CREATE TABLE [DateReportingFormatEvent](
  OID INTEGER IDENTITY(1, 1) NOT NULL, 
  ParentDateReportingFormat INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 

  PRIMARY KEY (OID),  
  FOREIGN KEY (ParentDateReportingFormat) REFERENCES [DateReportingFormat](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID), 
  CONSTRAINT chk_DateReportingFormatEventType CHECK (EventType IN ('EditFormatName',
                                                                   'EditFormatDescription',
                                                                   'EditFormatCode',
                                                                   'EditIncrementStart',
                                                                   'EditIncrementEnd',
                                                                   'Deactivate', 
                                                                   'Activate', 
                                                                   'Add'))
);

/* Disclaimer Table ************************************************/

CREATE TABLE [Disclaimer](
  OID INTEGER IDENTITY(1, 1) NOT NULL, 
  Disclaimer VARCHAR(2000) NOT NULL,
  IsActive BIT DEFAULT 0
  
  PRIMARY KEY (OID), 
);

/* DisclaimerEvent Table *******************************************/

CREATE TABLE [DisclaimerEventTable](
  OID INTEGER IDENTITY(1, 1) NOT NULL, 
  ParentDisclaimer INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
  
  PRIMARY KEY (OID), 
  FOREIGN KEY (ParentDisclaimer) REFERENCES [Disclaimer](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID), 
  CONSTRAINT chk_DisclaimerEventType CHECK (EventType IN ('EditDisclaimer',
                                                          'Deactivate', 
                                                          'Activate', 
                                                          'Add')
);

/* Footer Table ****************************************************/

CREATE TABLE [Footer](
  OID INTEGER IDENTITY(1, 1) NOT NULL,  
  Footer VARCHAR(200) NOT NULL,
  IsActive BIT DEFAULT 0
  
  PRIMARY KEY (OID), 
);

/* FooterEvent Table ***********************************************/

CREATE TABLE [FooterEventTable](
  OID INTEGER IDENTITY(1, 1) NOT NULL, 
  ParentFooter INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
  PRIMARY KEY (OID), 
  FOREIGN KEY (ParentFooter) REFERENCES [Footer](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID), 
  CONSTRAINT chk_FooterEventType CHECK (EventType IN ('EditFooter',
                                                      'Deactivate', 
                                                      'Activate', 
                                                      'Add')
);
