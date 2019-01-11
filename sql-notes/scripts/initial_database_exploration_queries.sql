/* Print all tables contained in a database
   Reference: https://stackoverflow.com/questions/175415/how-do-i-get-list-of-all-tables-in-a-database-using-tsql	
   */
SELECT * FROM DB2018.INFORMATION_SCHEMA.TABLES WHERE TABLE_TYPE='BASE TABLE';
SELECT * FROM AdventureWorks2008R2.INFORMATION_SCHEMA.TABLES WHERE TABLE_TYPE='BASE TABLE';

/* Print structure of a table*/
select * from INFORMATION_SCHEMA.COLUMNS
where TABLE_NAME='ProductInventory';

/* Print total number of columns in a table.  */
select COUNT(*) from INFORMATION_SCHEMA.COLUMNS where TABLE_NAME='ProductInventory';
