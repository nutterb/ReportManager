/* ReportUser Table  ***********************************************/

CREATE TABLE dbo.ReportUser(
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

CREATE TABLE dbo.ReportUserEvent(
  OID INT IDENTITY(1, 1)  NOT NULL, 
  ParentReportUser INT NOT NULL, 
  EventReportUser INT NOT NULL, 
  EventType VARCHAR(50) NOT NULL, 
  EventDateTime DATETIME NOT NULL, 
  NewValue VARCHAR(200) NULL, 
  
  PRIMARY KEY (OID), 
  FOREIGN KEY (ParentReportUser) REFERENCES ReportUser(OID), 
  FOREIGN KEY (EventReportUser) REFERENCES ReportUser(OID), 
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
