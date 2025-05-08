-- Variables to declare and modify for different database name and file locations
DECLARE @DBNAME NVARCHAR(MAX) = 'FFI_RA_AGFO';
DECLARE @FilePathOrig NVARCHAR(MAX) = 'C:\temp\';
DECLARE	@FilePathNew NVARCHAR(MAX) = 'C:\Program Files\Microsoft SQL Server\MSSQL15.SQLEXPRESS\MSSQL\DATA\';
DECLARE @SQL NVARCHAR(MAX);

-- Run remaining query to restore database
--USE [master]

DECLARE @DATA NVARCHAR(MAX) = @FilePathNew + @DBNAME + '.mdf';
DECLARE @LOG NVARCHAR(MAX) = @FilePathNew + @DBNAME + '.ldf';
DECLARE @DBFULLPATH NVARCHAR(MAX) = @FilePathOrig + @DBNAME + '.bak';
DECLARE @DBLOG NVARCHAR(MAX) = @DBNAME + '_log';

USE[master]

SET @SQL =
  ' RESTORE DATABASE ['+@DBNAME+'] FROM DISK = '''+@DBFULLPATH+''' WITH FILE = 1, MOVE '''+@DBNAME+''' TO '''+@DATA+''',
    MOVE '''+@DBLOG+''' TO '''+@LOG+''',
    NOUNLOAD, STATS = 5'
EXEC(@SQL)
GO

