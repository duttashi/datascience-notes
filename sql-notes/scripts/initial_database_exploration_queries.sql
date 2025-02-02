select current_date;
select * from city;
select * from payment;
select distinct customer_id, payment_id, amount from payment where amount between 2.99 and 3.99 limit 50;
select distinct customer_id, payment_id, amount from payment order by payment_id asc;
select distinct release_year from film;
select distinct film_id, title, release_year, special_features, rental_rate from film where release_year!=2001 limit 100;

/* SQL commands specific to PostgresSQL */

-- show column names from table
-- Reference: https://dba.stackexchange.com/questions/22362/how-do-i-list-all-columns-for-a-specified-table
select * from information_schema.columns where table_schema='public'
and table_name = 'city';

-- show all tables in the database
-- Reference: https://stackoverflow.com/questions/769683/show-tables-in-postgresql
select * from pg_catalog.pg_tables;

-- Find the shortest and longest city name  as well as their respective lengths (i.e.: number of characters in the name)
select city, char_length(city) from city ORDER BY length(city), city ASC LIMIT 10;
select city, char_length(city) from city ORDER BY length(city), city DESC LIMIT 10;

-- Fetch clause
select film_id, title from film order by title fetch FIRST 10 rows only;

-- Like Operator and wildcards for filering
/*
To use the wildcard, what you'll do is add a percent sign before, after,
or in the middle of what you're searching for.
It's important to note that the wildcards will not match null values.
Again, remember nulls are really no value in the column.
You wouldn't be able to use a wildcard in those cases.

There are two wildcards used in conjunction with the LIKE operator:

% - The percent sign represents zero, one, or multiple characters
_ - The underscore represents a single character

*/
-- Examples
-- Reference: https://www.w3schools.com/sql/sql_like.asp
select film_id, title from film where title LIKE 'Ala%';
-- Finds any values that have "or" in any position
select film_id, title from film where title LIKE '%ash%';

/* ORDER BY clause

ORDER BY allows us to sort data by particular columns.
Now there are a few rules when using ORDER BY.
One is that it can take multiple column names. You can order by one column or can order by all the columns, and so it goes in the fashion that you want it to add them in.
If you're doing multiple columns, you just want to make sure you're adding a comma after that.
The other thing is you can actually do is sort by a column that you didn't retrieve.
So it may not be in your select statement but you can still use the column to sort your data which is really helpful.
The last rule is that ORDER BY must always be the last clause in the select statement.
Just kind of on the finishing touches, always round it up with the adding the ORDER BY at the end.
*/
-- Examples
select film_id, title from film where title LIKE '%ash%' ORDER BY title ASC;
select film_id, title from film where title LIKE 'W%y' ORDER BY title DESC; -- the wildcard should be read as "find any values that start with W and ends with y".

/* Math Operations


*/

-- show all column names in the payment table
select * from information_schema.columns where table_schema='public'and table_name = 'payment';

-- show columns where payment is less than 100$
select max(amount) from payment;
-- So the minimum amount value is 0 and the max amount is 11.99
select payment_date, customer_id, rental_id, amount from payment where amount between 10.99 and 11.99;
