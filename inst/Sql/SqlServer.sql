/* User Table  ***********************************************/

CREATE TABLE dbo.[User](
  [OID] INT IDENTITY(1, 1) NOT NULL, 
  [LastName] VARCHAR(50) NOT NULL, 
  [FirstName] VARCHAR(50) NOT NULL,
  [LoginId] VARCHAR(50) NOT NULL, 
  [EmailAddress] VARCHAR(100) NULL, 
  [IsInternal] BIT NOT NULL DEFAULT 1, 
  [IsActive] BIT NOT NULL DEFAULT 1, 
  
  PRIMARY KEY (OID)
);

/* UserEvent Table *******************************************/

CREATE TABLE dbo.[UserEvent](
  [OID] INT IDENTITY(1, 1)  NOT NULL, 
  [ParentUser] INT NOT NULL, 
  [EventUser] INT NOT NULL, 
  [EventType] VARCHAR(50) NOT NULL, 
  [EventDateTime] DATETIME NOT NULL, 
  [NewValue] VARCHAR(200) NULL, 
  
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
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [RoleName] VARCHAR(75) NOT NULL, 
  [RoleDescription] VARCHAR(250), 
  [IsActive] BIT NOT NULL DEFAULT 1, 
  
  PRIMARY KEY (OID)
);

/* RoleEvent Table *************************************************/

CREATE TABLE dbo.[RoleEvent](
  [OID] INTEGER IDENTITY(1, 1) NOT NULL,
  [ParentRole] INT NOT NULL, 
  [EventUser] INT NOT NULL, 
  [EventType] VARCHAR(25) NOT NULL, 
  [EventDateTime] DATETIME NOT NULL, 
  [NewValue] VARCHAR(250) NULL, 
  
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
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [ParentUser] INT NOT NULL, 
  [ParentRole] INT NOT NULL, 
  [IsActive] BIT NOT NULL DEFAULT 0, 
  
  PRIMARY KEY (OID),
  FOREIGN KEY (ParentUser) REFERENCES dbo.[User](OID),
  FOREIGN KEY (ParentRole) REFERENCES dbo.[Role](OID)
);

/* UserRoleEvent Table ***************************************/

CREATE TABLE dbo.[UserRoleEvent] (
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [ParentUserRole] INT NOT NULL, 
  [EventUser] INT NOT NULL, 
  [EventType] VARCHAR(25) NOT NULL, 
  [EventDateTime] DATETIME NOT NULL, 
  [NewValue] VARCHAR(250) NULL, 
  
  PRIMARY KEY (OID),
  FOREIGN KEY (ParentUserRole) REFERENCES dbo.[UserRole](OID), 
  FOREIGN KEY (EventUser) REFERENCES dbo.[User](OID),
  CONSTRAINT chk_UserRoleEventType CHECK (EventType IN ('Add', 
                                                        'Activate', 
                                                        'Deactivate'))
);

/* Schedule Table **************************************************/

CREATE TABLE dbo.[Schedule] (
  [OID] INTEGER IDENTITY(1, 1) NOT NULL,  
  [ScheduleName] VARCHAR(50) NOT NULL, 
  [Frequency] INT NOT NULL, 
  [FrequencyUnit] VARCHAR(10) NOT NULL, 
  [OffsetOverlap] INT NOT NULL, 
  [OffsetOverlapUnit] VARCHAR(10) NOT NULL, 
  [IsActive] BIT NOT NULL DEFAULT 0, 
  
  PRIMARY KEY (OID)
);

/* ScheduleEventTable **********************************************/

CREATE TABLE dbo.[ScheduleEvent](
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [ParentSchedule] INT NOT NULL, 
  [EventUser] INT NOT NULL, 
  [EventType] VARCHAR(25) NOT NULL, 
  [EventDateTime] DATETIME NOT NULL, 
  [NewValue] VARCHAR(250) NULL, 
  
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

CREATE TABLE dbo.[DateReportingFormat](
  OID INTEGER IDENTITY(1, 1) NOT NULL, 
  [FormatName] VARCHAR(25) NOT NULL, 
  [Description] VARCHAR(50) NULL,
  [FormatCode] VARCHAR(25) NOT NULL,
  [IncrementStart] INT NOT NULL, 
  [IncrementStartUnit] VARCHAR(10) NOT NULL, 
  [IncrementEnd] INT NOT NULL, 
  [IncrementEndUnit] VARCHAR(10) NOT NULL, 
  [IsActive] BIT NOT NULL DEFAULT 0, 
  
  PRIMARY KEY (OID)
);

/* DateReportingFormatEvent Table **********************************/

CREATE TABLE dbo.[DateReportingFormatEvent](
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [ParentDateReportingFormat] INT NOT NULL, 
  [EventUser] INT NOT NULL, 
  [EventType] VARCHAR(25) NOT NULL, 
  [EventDateTime] DATETIME NOT NULL, 
  [NewValue] VARCHAR(250) NULL, 

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

CREATE TABLE dbo.[Disclaimer](
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [Disclaimer] VARCHAR(2000) NOT NULL,
  [IsActive] BIT DEFAULT 0,
  
  PRIMARY KEY (OID)
);

/* DisclaimerEvent Table *******************************************/

CREATE TABLE dbo.[DisclaimerEvent](
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [ParentDisclaimer] INT NOT NULL, 
  [EventUser] INT NOT NULL, 
  [EventType] VARCHAR(25) NOT NULL, 
  [EventDateTime] DATETIME NOT NULL, 
  [NewValue] VARCHAR(2000) NULL, 
  
  
  PRIMARY KEY (OID), 
  FOREIGN KEY (ParentDisclaimer) REFERENCES [Disclaimer](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID), 
  CONSTRAINT chk_DisclaimerEventType CHECK (EventType IN ('EditDisclaimer',
                                                          'Deactivate', 
                                                          'Activate', 
                                                          'Add'))
);

/* Footer Table ****************************************************/

CREATE TABLE dbo.[Footer](
  [OID] INTEGER IDENTITY(1, 1) NOT NULL,  
  [Footer] VARCHAR(200) NOT NULL,
  [IsActive] BIT DEFAULT 0,
  
  PRIMARY KEY (OID) 
);

/* FooterEvent Table ***********************************************/

CREATE TABLE dbo.[FooterEvent](
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [ParentFooter] INT NOT NULL, 
  [EventUser] INT NOT NULL, 
  [EventType] VARCHAR(25) NOT NULL, 
  [EventDateTime] DATETIME NOT NULL, 
  [NewValue] VARCHAR(250) NULL, 
  
  PRIMARY KEY (OID), 
  FOREIGN KEY (ParentFooter) REFERENCES [Footer](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID), 
  CONSTRAINT chk_FooterEventType CHECK (EventType IN ('EditFooter',
                                                      'Deactivate', 
                                                      'Activate', 
                                                      'Add'))
);

/* FileArchive *****************************************************/

CREATE TABLE dbo.[FileArchive](
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [ParentReportTemplate] INT NULL, 
  [ParentReportInstance] INT NULL,
  [Description] VARCHAR(250) NULL,
  [CreatedDateTime] DATETIME NOT NULL,
  [IsLogo] BIT NOT NULL DEFAULT 0,
  [FileName] VARCHAR(250) NOT NULL, 
  [FileExtension] VARCHAR(15) NOT NULL,
  [FileSize] INT NOT NULL,
  [FileContent] VARBINARY(MAX) NOT NULL, 
  
  PRIMARY KEY (OID)
);

/* ReportTemplate **************************************************/

CREATE TABLE dbo.[ReportTemplate](
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [TemplateDirectory] VARCHAR(50) NOT NULL, 
  [TemplateFile] VARCHAR(50) NOT NULL,
  [Title] VARCHAR(200) NOT NULL, 
  [TitleSize] VARCHAR(15) NOT NULL, 
  [IsSignatureRequired] BIT NOT NULL DEFAULT 0, 
  [IsActive] BIT NOT NULL DEFAULT 0, 
  [LogoFileArchive] INT NULL, 
  
  PRIMARY KEY (OID), 
  FOREIGN KEY (LogoFileArchive) REFERENCES [FileArchive](OID)
);

/* ReportTemplateEvent *********************************************/

CREATE TABLE dbo.[ReportTemplateEvent](
  [OID] INTEGER IDENTITY(1, 1) NOT NULL,
  [ParentReportTemplate] INT NOT NULL, 
  [EventUser] INT NOT NULL, 
  [EventType] VARCHAR(25) NOT NULL, 
  [EventDateTime] DATETIME NOT NULL, 
  [NewValue] VARCHAR(250) NULL, 
  
  PRIMARY KEY (OID), 
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

CREATE TABLE dbo.[ReportInstance](
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [ParentReportTemplate] INT NOT NULL, 
  [StartDateTime] DATETIME NOT NULL, 
  [EndDateTime] DATETIME NOT NULL, 
  [IsScheduled] BIT DEFAULT 0, 
  [InstanceTitle] VARCHAR(200), 
  
  PRIMARY KEY (OID), 
  FOREIGN KEY (ParentReportTemplate) REFERENCES [ReportTemplate](OID)
);

/* ReportInstanceEvent *********************************************/

CREATE TABLE dbo.[ReportInstanceEvent](
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [ParentReportInstance] INT NOT NULL, 
  [EventUser] INT NOT NULL, 
  [EventType] VARCHAR(25) NOT NULL, 
  [EventDateTime] DATETIME NOT NULL, 
  [NewValue] VARCHAR(250) NULL, 
  
  PRIMARY KEY (OID), 
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

CREATE TABLE dbo.[ReportTemplateDisclaimer](
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [ParentReportTemplate] INT NOT NULL,
  [ParentDisclaimer] INT NOT NULL, 
  [IsActive] BIT DEFAULT 0, 
  [Order] INT,
  
  PRIMARY KEY (OID), 
  FOREIGN KEY (ParentReportTemplate) REFERENCES [ReportTemplate](OID), 
  FOREIGN KEY (ParentDisclaimer) REFERENCES [Disclaimer](OID)
);

/* ReportTemplateDisclaimerEvent ***********************************/

CREATE TABLE dbo.[ReportTemplateDisclaimerEvent] (
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [ParentReportTemplateDisclaimer] INT NOT NULL, 
  [EventUser] INT NOT NULL, 
  [EventType] VARCHAR(25) NOT NULL, 
  [EventDateTime] DATETIME NOT NULL, 
  [NewValue] VARCHAR(250) NULL, 
  
  PRIMARY KEY (OID), 
  FOREIGN KEY (ParentReportTemplateDisclaimer) REFERENCES [ReportTemplateDisclaimer](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID),
  CONSTRAINT chk_ReportTemplateDisclaimerEventType CHECK (EventType IN ('Add', 
                                                                        'Activate', 
                                                                        'Deactivate',
                                                                        'Reorder'))
);

/* ReportTemplateFooter ********************************************/

CREATE TABLE dbo.[ReportTemplateFooter](
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [ParentReportTemplate] INT NOT NULL,
  [ParentFooter] INT NOT NULL, 
  [IsActive] BIT DEFAULT 0, 
  [Order] INT,
  
  PRIMARY KEY (OID), 
  FOREIGN KEY (ParentReportTemplate) REFERENCES [ReportTemplate](OID), 
  FOREIGN KEY (ParentFooter) REFERENCES [Footer](OID)
);

/* ReportTemplateFooterEvent ***************************************/

CREATE TABLE dbo.[ReportTemplateFooterEvent] (
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [ParentReportTemplateFooter] INT NOT NULL, 
  [EventUser] INT NOT NULL, 
  [EventType] VARCHAR(25) NOT NULL, 
  [EventDateTime] DATETIME NOT NULL, 
  [NewValue] VARCHAR(250) NULL, 
  
  PRIMARY KEY (OID), 
  FOREIGN KEY (ParentReportTemplateFooter) REFERENCES [ReportTemplateFooter](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID),
  CONSTRAINT chk_ReportTemplateFooterEventType CHECK (EventType IN ('Add', 
                                                                    'Activate', 
                                                                    'Deactivate', 
                                                                    'Reorder'))
);

/* ReportTemplateSchedule ******************************************/

CREATE TABLE dbo.[ReportTemplateSchedule](
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [ParentReportTemplate] INT NOT NULL,
  [ParentSchedule] INT NOT NULL,
  [StartDateTime] DATETIME NOT NULL,
  [IsActive] BIT DEFAULT 0, 
  
  PRIMARY KEY (OID), 
  FOREIGN KEY (ParentReportTemplate) REFERENCES [ReportTemplate](OID), 
  FOREIGN KEY (ParentSchedule) REFERENCES [Schedule](OID)
);

/* ReportTemplateScheduleEvent *************************************/

CREATE TABLE dbo.[ReportTemplateScheduleEvent] (
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [ParentReportTemplateSchedule] INT NOT NULL, 
  [EventUser] INT NOT NULL, 
  [EventType] VARCHAR(25) NOT NULL, 
  [EventDateTime] DATETIME NOT NULL, 
  [NewValue] VARCHAR(250) NULL, 
  
  PRIMARY KEY (OID), 
  FOREIGN KEY (ParentReportTemplateSchedule) REFERENCES [ReportTemplateSchedule](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID),
  CONSTRAINT chk_ReportTemplateScheduleEventType CHECK (EventType IN ('Add', 
                                                                    'Activate', 
                                                                    'Deactivate', 
                                                                    'EditStartDate'))
);

/* ReportTemplateSignature *****************************************/

CREATE TABLE dbo.[ReportTemplateSignature](
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [ParentReportTemplate] INT NOT NULL,
  [ParentRole] INT NOT NULL,
  [ORDER] INT NOT NULL,
  [IsActive] BIT DEFAULT 0, 
  
  PRIMARY KEY (OID),
  FOREIGN KEY (ParentReportTemplate) REFERENCES [ReportTemplate](OID), 
  FOREIGN KEY (ParentRole) REFERENCES [Role](OID)
);

/* ReportTemplateSignatureEvent ************************************/

CREATE TABLE dbo.[ReportTemplateSignatureEvent] (
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [ParentReportTemplateSignature] INT NOT NULL, 
  [EventUser] INT NOT NULL, 
  [EventType] VARCHAR(25) NOT NULL, 
  [EventDateTime] DATETIME NOT NULL, 
  [NewValue] VARCHAR(250) NULL, 
  
  PRIMARY KEY (OID),
  FOREIGN KEY (ParentReportTemplateSignature) REFERENCES [ReportTemplateSignature](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID),
  CONSTRAINT chk_ReportTemplateSignatureEventType CHECK (EventType IN ('Add', 
                                                                    'Activate', 
                                                                    'Deactivate', 
                                                                    'Reorder'))
);

/* ReportTemplateDistribution **************************************/

CREATE TABLE dbo.[ReportTemplateDistribution](
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [ParentReportTemplate] INT NOT NULL,
  [ParentUser] INT NOT NULL,
  [ORDER] INT NOT NULL,
  [IsActive] BIT DEFAULT 0, 
  
  PRIMARY KEY (OID),
  FOREIGN KEY (ParentReportTemplate) REFERENCES [ReportTemplate](OID), 
  FOREIGN KEY (ParentUser) REFERENCES [User](OID)
);

/* ReportTemplateSignatureEvent ************************************/

CREATE TABLE dbo.[ReportTemplateDistributionEvent] (
  [OID] INTEGER IDENTITY(1, 1) NOT NULL, 
  [ParentReportTemplateDistribution] INT NOT NULL, 
  [EventUser] INT NOT NULL, 
  [EventType] VARCHAR(25) NOT NULL, 
  [EventDateTime] DATETIME NOT NULL, 
  [NewValue] VARCHAR(250) NULL, 
  
  PRIMARY KEY (OID),
  FOREIGN KEY (ParentReportTemplateDistribution) REFERENCES [ReportTemplateDistribution](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID),
  CONSTRAINT chk_ReportTemplateDistributionEventType CHECK (EventType IN ('Add', 
                                                                    'Activate', 
                                                                    'Deactivate', 
                                                                    'Reorder'))
);
