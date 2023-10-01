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


/* Schedule Table **************************************************/

CREATE TABLE [Schedule] (
  OID INTEGER PRIMARY KEY, 
  ScheduleName VARCHAR(50) NOT NULL, 
  Frequency INT NOT NULL, 
  FrequencyUnit VARCHAR(10) NOT NULL, 
  OffsetOverlap INT NOT NULL, 
  OffsetOverlapUnit VARCHAR(10) NOT NULL, 
  IsActive BIT NOT NULL DEFAULT 0
);

/* ScheduleEventTable **********************************************/

CREATE TABLE [ScheduleEvent](
  OID INTEGER PRIMARY KEY, 
  ParentSchedule INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
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
  OID INTEGER PRIMARY KEY, 
  [FormatName] VARCHAR(25) NOT NULL, 
  [Description] VARCHAR(50) NULL,
  [FormatCode] VARCHAR(25) NOT NULL,
  IncrementStart INT NOT NULL, 
  IncrementStartUnit VARCHAR(10) NOT NULL, 
  IncrementEnd INT NOT NULL, 
  IncrementEndUnit VARCHAR(10) NOT NULL, 
  IsActive BIT NOT NULL DEFAULT 0
);

/* DateReportingFormatEvent Table **********************************/

CREATE TABLE [DateReportingFormatEvent](
  OID INTEGER PRIMARY KEY, 
  ParentDateReportingFormat INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
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
  OID INTEGER PRIMARY KEY, 
  Disclaimer VARCHAR(2000) NOT NULL,
  IsActive BIT DEFAULT 0
);

/* DisclaimerEvent Table *******************************************/

CREATE TABLE [DisclaimerEvent](
  OID INTEGER PRIMARY KEY,
  ParentDisclaimer INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(2000) NULL, 
  
  FOREIGN KEY (ParentDisclaimer) REFERENCES [Disclaimer](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID), 
  CONSTRAINT chk_DisclaimerEventType CHECK (EventType IN ('EditDisclaimer',
                                                          'Deactivate', 
                                                          'Activate', 
                                                          'Add'))
);

/* Footer Table ****************************************************/

CREATE TABLE [Footer](
  OID INTEGER PRIMARY KEY, 
  Footer VARCHAR(200) NOT NULL,
  IsActive BIT DEFAULT 0
);

/* FooterEvent Table ***********************************************/

CREATE TABLE [FooterEvent](
  OID INTEGER PRIMARY KEY,
  ParentFooter INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
  FOREIGN KEY (ParentFooter) REFERENCES [Footer](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID), 
  CONSTRAINT chk_FooterEventType CHECK (EventType IN ('EditFooter',
                                                      'Deactivate', 
                                                      'Activate', 
                                                      'Add'))
);

/* FileArchive *****************************************************/

CREATE TABLE [FileArchive](
  OID INTEGER PRIMARY KEY, 
  ParentReportTemplate INT NULL, 
  ParentReportInstance INT NULL,
  Description VARCHAR(250) NULL,
  CreatedDateTime DATETIME NOT NULL,
  IsLogo BIT NOT NULL DEFAULT 0,
  FileName VARCHAR(250) NOT NULL, 
  FileExtension VARCHAR(15) NOT NULL,
  FileSize INT NOT NULL,
  FileContent BLOB NOT NULL
);

/* ReportTemplate **************************************************/

CREATE TABLE [ReportTemplate](
  OID INTEGER PRIMARY KEY, 
  [TemplateDirectory] VARCHAR(50) NOT NULL,
  [TemplateFile] VARCHAR(50) NOT NULL,
  [Title] VARCHAR(200) NOT NULL, 
  [TitleSize] VARCHAR(15) NOT NULL, 
  IsSignatureRequired BIT NOT NULL DEFAULT 0, 
  IsActive BIT NOT NULL DEFAULT 0, 
  LogoFileArchive INT NULL, 
  
  FOREIGN KEY (LogoFileArchive) REFERENCES [FileArchive](OID)
);

/* ReportTemplateEvent *********************************************/

CREATE TABLE [ReportTemplateEvent](
  OID INTEGER PRIMARY KEY, 
  ParentReportTemplate INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
  FOREIGN KEY (ParentReportTemplate) REFERENCES [ReportTemplate](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID), 
  CONSTRAINT chk_ReportTemplateEventType CHECK (EventType IN ('EditTemplateFolder',
                                                              'EditTemplateFile',
                                                              'EditTitle', 
                                                              'EditTitleSize',
                                                              'SetSignatureRequiredFalse',
                                                              'SetSignatureRequiredTrue',
                                                              'EditLogoFile',
                                                              'Deactivate', 
                                                              'Activate', 
                                                              'Add'))
);

/* ReportInstance **************************************************/

CREATE TABLE [ReportInstance](
  OID INTEGER PRIMARY KEY, 
  ParentReportTemplate INT NOT NULL, 
  StartDateTime DATETIME NOT NULL, 
  EndDateTime DATETIME NOT NULL, 
  IsScheduled BIT DEFAULT 0, 
  InstanceTitle VARCHAR(200), 
  
  FOREIGN KEY (ParentReportTemplate) REFERENCES [ReportTemplate](OID)
);

/* ReportInstanceEvent *********************************************/

CREATE TABLE [ReportInstanceEvent](
  OID INTEGER PRIMARY KEY, 
  ParentReportInstance INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
  FOREIGN KEY (ParentReportInstance) REFERENCES [ReportInstance](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID), 
  CONSTRAINT chk_ReportInstanceEventType CHECK (EventType IN ('EditStartTime',
                                                              'EditEndTime', 
                                                              'EditTitleSize',
                                                              'EditIsScheduled', 
                                                              'EditInstanceTitle',
                                                              'Add'))
); 

/* ReportTemplateDisclaimer ****************************************/

CREATE TABLE [ReportTemplateDisclaimer](
  OID INTEGER PRIMARY KEY, 
  ParentReportTemplate INT NOT NULL,
  ParentDisclaimer INT NOT NULL, 
  IsActive BIT DEFAULT 0, 
  
  FOREIGN KEY (ParentReportTemplate) REFERENCES [ReportTemplate](OID), 
  FOREIGN KEY (ParentDisclaimer) REFERENCES [Disclaimer](OID)
);

/* ReportTemplateDisclaimerEvent ***********************************/

CREATE TABLE [ReportTemplateDisclaimerEvent] (
  OID INTEGER PRIMARY KEY, 
  ParentReportTemplateDisclaimer INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
  FOREIGN KEY (ParentReportTemplateDisclaimer) REFERENCES [ReportTemplateDisclaimer](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID),
  CONSTRAINT chk_ReportTemplateDisclaimerEventType CHECK (EventType IN ('Add', 
                                                                        'Activate', 
                                                                        'Deactivate'))
);

/* ReportTemplateFooter ********************************************/

CREATE TABLE [ReportTemplateFooter](
  OID INTEGER PRIMARY KEY, 
  ParentReportTemplate INT NOT NULL,
  ParentFooter INT NOT NULL, 
  IsActive BIT DEFAULT 0, 
  
  FOREIGN KEY (ParentReportTemplate) REFERENCES [ReportTemplate](OID), 
  FOREIGN KEY (ParentFooter) REFERENCES [Footer](OID)
);

/* ReportTemplateFooterEvent ***************************************/

CREATE TABLE [ReportTemplateFooterEvent] (
  OID INTEGER PRIMARY KEY, 
  ParentReportTemplateFooter INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
  FOREIGN KEY (ParentReportTemplateFooter) REFERENCES [ReportTemplateFooter](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID),
  CONSTRAINT chk_ReportTemplateFooterEventType CHECK (EventType IN ('Add', 
                                                                        'Activate', 
                                                                        'Deactivate'))
);

/* ReportTemplateSchedule ******************************************/

CREATE TABLE [ReportTemplateSchedule](
  OID INTEGER PRIMARY KEY, 
  ParentReportTemplate INT NOT NULL,
  ParentSchedule INT NOT NULL,
  StartDateTime DATETIME NOT NULL,
  IsActive BIT DEFAULT 0, 
  
  FOREIGN KEY (ParentReportTemplate) REFERENCES [ReportTemplate](OID), 
  FOREIGN KEY (ParentSchedule) REFERENCES [Schedule](OID)
);

/* ReportTemplateScheduleEvent *************************************/

CREATE TABLE [ReportTemplateScheduleEvent] (
  OID INTEGER PRIMARY KEY, 
  ParentReportTemplateSchedule INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
  FOREIGN KEY (ParentReportTemplateSchedule) REFERENCES [ReportTemplateSchedule](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID),
  CONSTRAINT chk_ReportTemplateFooterEventType CHECK (EventType IN ('Add', 
                                                                    'Activate', 
                                                                    'Deactivate', 
                                                                    'EditStartDate'))
);
