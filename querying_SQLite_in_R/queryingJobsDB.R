# Import libraries
library(RSQLite)
library(DBI)
library(tibble)

# Connect with the database
jobs.db <- dbConnect(SQLite(), "db/jobs.db")
  
  # List all tables in the .db
  tables <- dbListTables(jobs.db)
  tables
  
  # Write a query as a character vector, also known as a string in Python
  query <- "SELECT * from recent_grads"
  query
  
  # Send the query to the database and execute it
  result <- dbGetQuery(jobs.db, query)
  tibble(result)

  # Exercise 
  major_query <- "SELECT Major FROM recent_grads"
  majors <- dbGetQuery(jobs.db, major_query)
  tibble(majors)

    
# Another workflow to use if the database is too large for the memory: separate dbGetQuery() in two actions

  # dbSendQuery() is like dbGetQuery(), but only executes the query
  majorCategory_query <- "SELECT Major_category FROM recent_grads"
  send_query_db <- dbSendQuery(jobs.db, majorCategory_query)
  
  # dbFentch() show the results
  majorCategory <- dbFetch(send_query_db, n = 10)
  tibble(majorCategory)

  # Exercise
  major_and_category_query <- "SELECT Major, Major_category FROM recent_grads"
  send_query_db <- dbSendQuery(jobs.db, major_and_category_query)
  major_and_category <- dbFetch(send_query_db, n = 5)
  tibble(major_and_category)

# Close the original connection to the database and clear the result
major_query <- "SELECT Major FROM recent_grads ORDER BY Major DESC"
send_query_db <- dbSendQuery(jobs.db, major_query)
reverse_alphabetical <- dbFetch(send_query_db)
tibble(reverse_alphabetical)
dbClearResult(send_query_db)
dbDisconnect(jobs.db)

