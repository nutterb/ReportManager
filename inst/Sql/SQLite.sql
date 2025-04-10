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

CREATE TABLE [UserSignature] (
  OID INTEGER PRIMARY KEY, 
  ParentUser INT NOT NULL, 
  FileName VARCHAR(250) NOT NULL, 
  FileExtension VARCHAR(15) NOT NULL,
  FileSize INT NOT NULL,
  FileContent BLOB NOT NULL, 
  
  FOREIGN KEY (ParentUser) REFERENCES [User](OID)
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
  IsActive BIT NOT NULL DEFAULT 0,
  [IsPeriodToDate] BIT NOT NULL DEFAULT 0
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
                                                       'Add', 
                                                       'SetIsPeriodToDateTrue', 
                                                       'SetIsPeriodToDateFalse'))
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
  [TemplateName] VARCHAR(50) NOT NULL,
  [TemplateDirectory] VARCHAR(50) NOT NULL,
  [TemplateFile] VARCHAR(50) NOT NULL,
  [Title] VARCHAR(200) NOT NULL, 
  [TitleSize] VARCHAR(15) NOT NULL,
  [IncludeTableOfContents] BIT NOT NULL, 
  [DefaultEmailText] VARCHAR(1000) NOT NULL,
  [IsSignatureRequired] BIT NOT NULL DEFAULT 0, 
  [IsActive] BIT NOT NULL DEFAULT 0, 
  [LogoFileArchive] INT NULL, 
  [DateReportingFormat] INT NOT NULL,
  [SupportingDataFile] VARCHAR(150) NOT NULL,
  [IsIncludeData] BIT NOT NULL,
  
  FOREIGN KEY (LogoFileArchive) REFERENCES [FileArchive](OID),
  FOREIGN KEY (DateReportingFormat) REFERENCES [DateReportingFormat](OID) 
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
  CONSTRAINT chk_ReportTemplateEventType CHECK (EventType IN ('EditIsIncludeData', 
                                                              'EditSupportingDataFile', 
                                                              'EditTemplateName',
                                                              'EditDateReportingFormat',
                                                              'EditTemplateFolder',
                                                              'EditTemplateFile',
                                                              'EditTitle', 
                                                              'EditTitleSize',
                                                              'EditDefaultEmailText',
                                                              'SetIncludeTocFalse', 
                                                              'SetIncludeTocTrue',
                                                              'SetSignatureRequiredFalse',
                                                              'SetSignatureRequiredTrue',
                                                              'EditLogoFile',
                                                              'Deactivate', 
                                                              'Activate', 
                                                              'Add'))
);

/* ReportTemplateDisclaimer ****************************************/

CREATE TABLE [ReportTemplateDisclaimer](
  OID INTEGER PRIMARY KEY, 
  ParentReportTemplate INT NOT NULL,
  ParentDisclaimer INT NOT NULL, 
  IsActive BIT DEFAULT 0, 
  [Order] INT,
  
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
                                                                        'Deactivate',
                                                                        'Reorder'))
);

/* ReportTemplateFooter ********************************************/

CREATE TABLE [ReportTemplateFooter](
  OID INTEGER PRIMARY KEY, 
  ParentReportTemplate INT NOT NULL,
  ParentFooter INT NOT NULL, 
  IsActive BIT DEFAULT 0, 
  [Order] INT,
  
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
                                                                    'Deactivate',
                                                                    'Reorder'))
);

/* ReportTemplateSchedule ******************************************/

CREATE TABLE [ReportTemplateSchedule](
  OID INTEGER PRIMARY KEY, 
  ParentReportTemplate INT NOT NULL,
  ParentSchedule INT NOT NULL,
  StartDateTime DATETIME NOT NULL,
  IsActive BIT DEFAULT 0, 
  IndexDateTime DATETIME NULL,
  
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
                                                                    'EditStartDate', 
                                                                    'EditIndexDate'))
);

/* ReportTemplateSignature *****************************************/

CREATE TABLE [ReportTemplateSignature](
  OID INTEGER PRIMARY KEY, 
  ParentReportTemplate INT NOT NULL,
  ParentRole INT NOT NULL,
  [Order] INT NOT NULL,
  IsActive BIT DEFAULT 0, 
  
  FOREIGN KEY (ParentReportTemplate) REFERENCES [ReportTemplate](OID), 
  FOREIGN KEY (ParentRole) REFERENCES [Role](OID)
);

/* ReportTemplateSignatureEvent ************************************/

CREATE TABLE [ReportTemplateSignatureEvent] (
  OID INTEGER PRIMARY KEY, 
  ParentReportTemplateSignature INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
  FOREIGN KEY (ParentReportTemplateSignature) REFERENCES [ReportTemplateSignature](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID),
  CONSTRAINT chk_ReportTemplateSignatureEventType CHECK (EventType IN ('Add', 
                                                                    'Activate', 
                                                                    'Deactivate', 
                                                                    'Reorder'))
);

/* ReportTemplateDistribution **************************************/

CREATE TABLE [ReportTemplateDistribution](
  OID INTEGER PRIMARY KEY, 
  ParentReportTemplate INT NOT NULL,
  [ParentUser] INT NULL,
  [ParentRole] INT NULL,
  IsActive BIT DEFAULT 0, 
  
  FOREIGN KEY (ParentReportTemplate) REFERENCES [ReportTemplate](OID), 
  FOREIGN KEY (ParentUser) REFERENCES [User](OID),
  FOREIGN KEY (ParentRole) REFERENCES [Role](OID)
);

/* ReportTemplateDistributionEvent ************************************/

CREATE TABLE [ReportTemplateDistributionEvent] (
  OID INTEGER PRIMARY KEY, 
  ParentReportTemplateDistribution INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
  FOREIGN KEY (ParentReportTemplateDistribution) REFERENCES [ReportTemplateDistribution](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID),
  CONSTRAINT chk_ReportTemplateDistributionEventType CHECK (EventType IN ('Add', 
                                                                    'Activate', 
                                                                    'Deactivate'))
);

/* ReportTemplatePermission ****************************************/

CREATE TABLE [ReportTemplatePermission](
  [OID] INTEGER PRIMARY KEY,
  [ParentReportTemplate] INT NOT NULL, 
  [ParentRole] INT NOT NULL, 
  [CanView] BIT DEFAULT 0, 
  [CanAddNotes] BIT DEFAULT 0, 
  [CanEditNarrative] BIT DEFAULT 0, 
  [CanSubmit] BIT DEFAULT 0, 
  [CanStartRevision] BIT DEFAULT 0, 
  [IsActive] BIT DEFAULT 0, 
  
  FOREIGN KEY (ParentReportTemplate) REFERENCES [ReportTemplate](OID), 
  FOREIGN KEY (ParentRole) REFERENCES [Role](OID)  
);

/* ReportTemplatePermissionEvent ***********************************/

CREATE TABLE [ReportTemplatePermissionEvent] (
  [OID] INTEGER PRIMARY KEY,
  [ParentReportTemplatePermission] INT NOT NULL, 
  [EventUser] INT NOT NULL, 
  [EventType] VARCHAR(25) NOT NULL, 
  [EventDateTime] DATETIME NOT NULL, 
  [NewValue] VARCHAR(250) NULL, 
  
  FOREIGN KEY (ParentReportTemplatePermission) REFERENCES [ReportTemplatePermission](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID),
  CONSTRAINT chk_ReportTemplatePermissionEventType CHECK (EventType IN ('Add', 
                                                                    'Activate', 
                                                                    'Deactivate', 
                                                                    'SetCanView', 
                                                                    'SetCanAddNotes', 
                                                                    'SetCanEditNarrative', 
                                                                    'SetCanSubmit', 
                                                                    'SetCanStartRevision'))
);

/* ReportInstance **************************************************/

CREATE TABLE [ReportInstance](
  OID INTEGER PRIMARY KEY, 
  ParentReportTemplate INT NOT NULL, 
  StartDateTime DATETIME NOT NULL, 
  EndDateTime DATETIME NOT NULL, 
  IsSignatureRequired BIT DEFAULT 0,
  IsScheduled BIT DEFAULT 0, 
  InstanceTitle VARCHAR(200), 
  IsSubmitted BIT DEFAULT 0,
  
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
                                                              'EditIsScheduled', 
                                                              'EditInstanceTitle',
                                                              'EditIsSignatureRequired',
                                                              'EditIsSubmitted',
                                                              'Add'))
); 

/* ReportInstanceNote **********************************************/
CREATE TABLE [ReportInstanceNote](
  [OID] INTEGER PRIMARY KEY,
  [ParentReportInstance] INT NOT NULL, 
  [ParentUser] INT NOT NULL, 
  [NoteDateTime] DATETIME NOT NULL,
  [Note] TEXT, 
  
  FOREIGN KEY (ParentReportInstance) REFERENCES [ReportInstance](OID),
  FOREIGN KEY (ParentUser) REFERENCES [User](OID)
);

/* ReportInstanceNarrative *****************************************/
CREATE TABLE [ReportInstanceNarrative](
  [OID] INTEGER PRIMARY KEY,
  [ParentReportInstance] INT NOT NULL, 
  [Narrative] TEXT, 
  
  FOREIGN KEY (ParentReportInstance) REFERENCES [ReportInstance](OID)
);

CREATE TABLE [ReportInstanceNarrativeEvent](
  [OID] INTEGER PRIMARY KEY, 
  [ParentReportInstanceNarrative] INT NOT NULL, 
  [EventUser] INT NOT NULL, 
  [EventDateTime] DATETIME NOT NULL, 
  [NewValue] TEXT, 
  
  FOREIGN KEY (ParentReportInstanceNarrative) REFERENCES [ReportInstanceNarrative](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID)
);


/* ReportInstanceSignature *****************************************/

CREATE TABLE [ReportInstanceSignature](
  [OID] INTEGER PRIMARY KEY, 
  [ParentReportInstance] INT NOT NULL, 
  [ParentReportTemplateSignature] INT NOT NULL, 
  [ParentUser] INT NOT NULL,
  [SignatureDateTime] DATETIME NOT NULL,
  [SignatureName] VARCHAR(200) NOT NULL, 
  [IsSigned] BIT NOT NULL,
  
  FOREIGN KEY (ParentReportInstance) REFERENCES [ReportInstance](OID),
  FOREIGN KEY (ParentReportTemplateSignature) REFERENCES [ReportTemplateSignature](OID),
  FOREIGN KEY (ParentUser) REFERENCES [User](OID)
);

CREATE TABLE [ReportInstanceGeneration](
  [OID] INTEGER PRIMARY KEY, 
  [ParentReportInstance] INT NOT NULL, 
  [ParentReportTemplate] INT NOT NULL,
  [StartDateTime] DATETIME NULL, 
  [EndDateTime] DATETIME NULL,
  [ReportFormat] VARCHAR(10) NOT NULL,
  [IncludeData] BIT NOT NULL, 
  [IsPreview] BIT NOT NULL,
  [IsDistributed] BIT NOT NULL,
  [IsArchived] BIT NOT NULL, 
  [IsSubmission] BIT NOT NULL,
  [PreviewDateTime] DATETIME NOT NULL, 
  [ParentUser] INT NOT NULL, 
  
  FOREIGN KEY (ParentReportInstance) REFERENCES [ReportInstance](OID),
  FOREIGN KEY (ParentReportTemplate) REFERENCES [ReportTemplate](OID),
  FOREIGN KEY (ParentUser) REFERENCES [User](OID), 
  CONSTRAINT chk_ReportInstanceGenerationReportFormat CHECK (ReportFormat IN ('shiny',
                                                                              'html',
                                                                              'pdf', 
                                                                              'preview')),
  CONSTRAINT chk_ReportInstanceGenerationAttribute CHECK
    ((IsPreview = 0 AND (IsDistributed = 1 OR IsArchived = 1 OR IsSubmission = 1)) OR
     (IsPreview = 1 AND (IsDistributed = 0 AND IsArchived = 0 AND IsSubmission = 0)))
);


CREATE TABLE [ReportInstanceGenerationRecipient](
  [OID] INTEGER PRIMARY KEY,
  [ParentReportInstanceGeneration] INT NOT NULL, 
  [ParentUser] INT NOT NULL,
  
  FOREIGN KEY (ParentReportInstanceGeneration) REFERENCES [ReportInstanceGeneration](OID),
  FOREIGN KEY (ParentUser) REFERENCES [User](OID)
);


CREATE TABLE [ReportInstanceDistribution](
  [OID] INTEGER PRIMARY KEY,
  [ParentReportInstance] INT NOT NULL, 
  [ParentUser] INT NULL, 
  [ParentRole] INT NULL, 
  [IsActive] BIT NOT NULL, 
  
  FOREIGN KEY (ParentReportInstance) REFERENCES [ReportInstance](OID),
  FOREIGN KEY (ParentUser) REFERENCES [User](OID),
  FOREIGN KEY (ParentRole) REFERENCES [Role](OID)
);

/* ReportInstanceDistributionEvent ************************************/

CREATE TABLE [ReportInstanceDistributionEvent] (
  OID INTEGER PRIMARY KEY, 
  ParentReportInstanceDistribution INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(25) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
  FOREIGN KEY (ParentReportInstanceDistribution) REFERENCES [ReportInstanceDistribution](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID),
  CONSTRAINT chk_ReportInstanceDistributionEventType CHECK (EventType IN ('Add', 
                                                                          'Activate', 
                                                                          'Deactivate'))
);

/* ReportInstanceRevision ******************************************/

CREATE TABLE [ReportInstanceRevision] (
  OID INTEGER PRIMARY KEY,
  ParentReportInstance INT NOT NULL, 
  ParentUser INT NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  Reason TEXT NOT NULL, 
  
  FOREIGN KEY (ParentReportInstance) REFERENCES [ReportInstance](OID), 
  FOREIGN KEY (ParentUser) REFERENCES [User](OID)
);

/* AutoDistribution ************************************************/

CREATE TABLE [AutoDistribution] (
  OID INTEGER PRIMARY KEY, 
  ParentReportTemplate INT NOT NULL, 
  StartDateTime DATETIME NOT NULL, 
  IsActive BIT NOT NULL, 
  DelayAfterInstanceEnd INT NOT NULL, 
  DelayUnits VARCHAR(50) NOT NULL, 
  CurrentOrLastInstance VARCHAR(20) NOT NULL, 
  IsAddToArchive BIT NOT NULL, 
  ReportFormat VARCHAR(10) NOT NULL,
  IsDistributeInternalOnly BIT NOT NULL, 
  IsEmbedHtml BIT NOT NULL, 
  
  FOREIGN KEY (ParentReportTemplate) REFERENCES [ReportTemplate](OID), 
  CONSTRAINT chk_AutoDistributionCurrentOrLast CHECK (CurrentOrLastInstance IN ('Current', 
                                                                                'LastCompleted'))
  
);

CREATE TABLE [AutoDistributionEvent](
  OID INTEGER PRIMARY KEY, 
  ParentAutoDistribution INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(30) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(250) NULL, 
  
  FOREIGN KEY (ParentAutoDistribution) REFERENCES [AutoDistribution](OID), 
  FOREIGN KEY (EventUser) REFERENCES [User](OID), 
  CONSTRAINT chk_AutoDistributionEventType CHECK (EventType IN ('Add', 
                                                                'Activate', 
                                                                'Deactivate', 
                                                                'EditStartDateTime', 
                                                                'EditDelayAfterInstanceEnd', 
                                                                'EditDelayUnits', 
                                                                'EditCurrentOrLastInstance', 
                                                                'EditIsAddToArchive', 
                                                                'EditReportFormat', 
                                                                'EditIsDistributeInternalOnly', 
                                                                'EditIsEmbedHtml'))
);

/* ReportInstanceAutoDistribution **********************************/

CREATE TABLE [ReportInstanceAutoDistribution] (
  OID INTEGER PRIMARY KEY,
  ParentReportInstance INT NOT NULL, 
  AutoDistributeDateTime DATETIME NOT NULL, 
  
  FOREIGN KEY (ParentReportInstance) REFERENCES [ReportInstance](OID)
);


/* Settings ********************************************************/

CREATE TABLE [ApplicationSetting] (
  OID INTEGER PRIMARY KEY, 
  SettingKey VARCHAR(50) NOT NULL, 
  SettingValue VARCHAR(500) NULL, 
  
  CONSTRAINT unq_SettingKey UNIQUE (SettingKey)
);

CREATE TABLE [ApplicationSettingEvent] (
  OID INTEGER PRIMARY KEY, 
  ParentApplicationSetting INT NOT NULL, 
  EventUser INT NOT NULL, 
  EventType VARCHAR(10) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(500) NULL, 
  
  FOREIGN KEY (ParentApplicationSetting) REFERENCES [ApplicationSetting](OID),
  CONSTRAINT chk_ApplicationSettingEventType CHECK (EventType IN ('EditKey', 
                                                                  'EditValue'))
);
