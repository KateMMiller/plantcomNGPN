-- Path and file names to make sure are correct --
DECLARE @DBSUFFIX NVARCHAR(MAX) = 'FFI_RA_'; -- database name sans park code 
DECLARE @PATHBAK NVARCHAR(MAX) = 'C:\temp\'; -- path where .bak lives (best if not on OneDrive)
DECLARE	@PATHSSMS NVARCHAR(MAX) = 'C:\Program Files\Microsoft SQL Server\MSSQL15.SQLEXPRESS\MSSQL\DATA\'; -- path where SSMS puts restored db- may change with new versions;

-- Create table of id and park codes (change park list below if not all parks)
IF EXISTS(SELECT * FROM ParkList) DROP TABLE ParkList;
CREATE TABLE ParkList(id INT, PARK NVARCHAR(MAX))
INSERT INTO ParkList(id, park)
VALUES (1, 'AGFO'), (2, 'BADL'), (3, 'DETO'), (4, 'FOLA'), (5, 'FOUS'), (6, 'JECA'), (7, 'KNRI'), 
       (8, 'MNRR'), (9, 'MORU'), (10, 'SCBL'), (11, 'THRO'), (12,'WICA');

-- Set up variables for while loop
DECLARE @ParkCnt INT;
SELECT @ParkCnt = COUNT(*) FROM Parklist; -- max index
DECLARE @i INT; -- index for loop
DECLARE @PARK NVARCHAR(MAX);
DECLARE @DBNAME NVARCHAR(MAX);
SET @i = 1
-- Begin for loop
WHILE @i <= @ParkCnt
  BEGIN
  -- Variables to declare and modify for different database name and file locations
  SELECT @PARK = park FROM ParkList WHERE id = @i
  SET @DBNAME = @DBSUFFIX + @PARK

  -- Variables based off three lines above
  DECLARE @DBFULLPATH NVARCHAR(MAX) = @PATHBAK + @DBNAME + '.bak'; -- full .bak DB path and file name
  DECLARE @DBSSMS NVARCHAR(MAX) = @PATHSSMS + @DBNAME + '.mdf'; -- for restored DB 
  DECLARE @LOGSSMS NVARCHAR(MAX) = @PATHSSMS + @DBNAME + '.ldf'; -- for restored DB
  DECLARE @DBLOG NVARCHAR(MAX) = @DBNAME + '_log';

  DECLARE @SQL NVARCHAR(MAX);
   -- Run remaining query to restore database
  USE[master]

  SET @SQL = 
    ' RESTORE DATABASE ['+@DBNAME+'] FROM DISK = '''+@DBFULLPATH+''' WITH FILE = 1, MOVE '''+@DBNAME+''' TO '''+@DBSSMS+''',
      MOVE '''+@DBLOG+''' TO '''+@LOGSSMS+''',
      NOUNLOAD, STATS = 5'
  EXEC(@SQL)
  SET @i = @i + 1
END
GO