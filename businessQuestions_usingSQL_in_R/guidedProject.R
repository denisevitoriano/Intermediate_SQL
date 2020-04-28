---
title: "Answering Business Questions using SQL: Guided Project Solutions"
couse: "Intermediate SQL in R by Dataquest"
author: "Denise Vitoriano"
---

# Import libraries
library(RSQLite)
library(DBI)
library(tidyverse)
library(ggplot2)
library(bbplot)

# 1. Helper functions ----

# Create 2 helper functions*
    # *Helper function: function that performs a small operation in the context of a greater process

# run_query(): verify if the database connection is open or not (state of the database)

db <- "db/chinook.db"

run_query <- function(q) {
    conn <- dbConnect(SQLite(), db)
    result <- dbGetQuery(conn, q)
    dbDisconnect(conn)
    return(result)
}

# show_tables(): return a list of all tables and views in the database
show_tables <- function() {
    q = "SELECT name, type FROM sqlite_master WHERE type IN ('table', 'view')"
    return(run_query(q))
}

show_tables()


# 2. Connect with the database ----
conn <- dbConnect(SQLite(), db)

# List all tables in the .db
album_tbl <- run_query("SELECT * FROM album") %>% tibble()
customer_tbl <- run_query("SELECT * FROM customer") %>%  tibble()
genre_tbl <- run_query("SELECT * FROM genre") %>%  tibble()
invoice_tbl <- run_query("SELECT * FROM invoice") %>%  tibble()
invoice_line_tbl <- run_query("SELECT * FROM invoice_line") %>%  tibble()
track_tbl <- run_query("SELECT * FROM track") %>%  tibble()


# 3. Task 1 ----
# The Chinook record store has just signed a deal with a new record label, 
# and you're in charge of choosing the first three albums to be added to the store. 
# There are four albums to choose from, and all four are by artists who don't have any tracks 
# in the store right now. Below is the list of artist names and the genre of music they produce:

artist_name <- c("Regal", "Red Tone", "Meteor and the Girls", "Slim Jim Bites")
genre <- c("Hip-Hop", "Punk", "Pop", "Blues")
new_albums_tbl <- tibble(artist_name, genre)

# Genres which sell the most in the USA

track_purchased <- "
WITH
    track_purchased ASteleg
        (
         SELECT
            t.name track,
            il.unit_price,
            il.quantity,
            CAST(il.unit_price AS FLOAT) * il.quantity total_price,
            g.name genre
         FROM invoice i
         LEFT JOIN invoice_line il ON i.invoice_id = il.invoice_id
         LEFT JOIN track t ON il.track_id = t.track_id
         LEFT JOIN genre g ON t.genre_id = g.genre_id
         WHERE i.billing_country = 'USA'
         ),
    genre_purchased AS
        (
         SELECT
            genre,
            COUNT(genre) track_quantity,
            SUM(total_price) genre_price
            FROM track_purchased
            GROUP BY genre
        )
SELECT
    genre,
    track_quantity,
    SUM(track_quantity) OVER () total_tracks,
    ROUND((CAST(track_quantity AS FLOAT) / SUM(track_quantity) OVER ()), 4) quantity_by_total, 
    genre_price,
    SUM(genre_price) OVER () total_price,
    ROUND((genre_price / SUM(genre_price) OVER ()), 4) price_by_total
FROM genre_purchased
GROUP BY genre
ORDER BY 4 DESC
LIMIT 10
;
"
genre_purchased_tbl <- run_query(track_purchased) %>%  tibble()

# 3.1. Visualization ----

genre_purchased_plot <- genre_purchased_tbl %>%
    select(genre, track_quantity, quantity_by_total) %>%
    ggplot(aes(x = reorder(genre, track_quantity), 
               y = track_quantity,
               fill = quantity_by_total)) +
    geom_bar(stat = "identity", 
             position = "identity",
             fill = "#1380A1") +
    scale_y_continuous(limits = c(0, 600),
                       breaks = seq(0, 600, by = 100)) +
    coord_flip() +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    bbc_style() +
    labs(title = "The most sold genres in the USA",
         subtitle = "Top 10 genres by quantity of tracks") +
    theme(panel.grid.major.x = element_line(color = "#cbcbcb"), 
          panel.grid.major.y = element_blank()) +
    geom_label(aes(label = scales::percent(quantity_by_total, accuracy = 0.1)),
               hjust = 0, 
               vjust = 0.5, 
               colour = "#222222", 
               fill = NA, 
               label.size = NA,
               family = "Helvetica",
               size = 5)

# Saving plot

finalise_plot(plot_name = genre_purchased_plot, 
              source = "Source: Chinook record store database.",
              save_filepath = "img/genre_sales.png")

# Based on the tracks and genres which sell the most in the USA, we recommend that Chinook store should purchase albums from these three artists and genres:
# Red Stone - Punk;
# Slim Jim Bites - Blues;
# Meteor and the Girls - Pop.
# However, rock sells the majority of tracks. So to expand our sales, we should ask the record label if they have any up-and-coming rock bands.


# 4. Task 2 ----

# You have been asked to analyze the purchases of customers belonging to each employee to see 
# if any sales support agent is performing either better or worse than the others.

employee_sales <- "
WITH 
    employee_sales_by_year AS
        (
         SELECT 
            strftime('%Y', i.invoice_date) year,
            COUNT(i.invoice_id) cust_inv,
            SUM(i.total) emp_sales,
            e.first_name || ' ' || e.last_name employee,
            e.title,
            e.hire_date
        FROM employee e
        LEFT JOIN customer c ON e.employee_id = c.support_rep_id
        LEFT JOIN invoice i ON c.customer_id = i.customer_id
        GROUP BY year, employee
        HAVING e.title LIKE '%Support%'
        ),
    invoice_by_year AS
        (
         SELECT 
            strftime('%Y', invoice_date) invoice_year,
            COUNT(invoice_id) year_inv,
            SUM(total) year_sales
         FROM invoice
         GROUP BY invoice_year
        )
SELECT
    esby.year,
    iby.year_sales,
    esby.emp_sales,
    iby.year_inv,
    esby.cust_inv,
    esby.emp_sales/esby.cust_inv emp_sales_by_cust_inv,
    iby.year_sales/iby.year_inv year_sales_by_year_inv,
    esby.emp_sales/iby.year_inv emp_sales_by_year_inv,
    (esby.emp_sales/iby.year_sales) * 100 emp_sales_rate_by_year_sales,
    esby.employee,
    esby.title,
    esby.hire_date
FROM employee_sales_by_year esby
LEFT JOIN invoice_by_year iby ON esby.year = iby.invoice_year
;
"
employee_sales_tbl <- run_query(employee_sales) %>% tibble()


# 4.1. Visualization - line chart ----

employee_sales_line_chart <- employee_sales_tbl %>%
    ggplot(aes(x = as.numeric(year), 
               y = emp_sales_rate_by_year_sales,
               colour = employee)) +
    geom_point(size = 3.5) +
    geom_line(size = 1.5) +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    scale_colour_manual(values = c("#FAAB18", "#1380A1", "#3BB273")) +
    scale_y_continuous(limits = c(0, 50),
                       breaks = seq(0, 50, by = 10),
                       labels = c("0%","10%", "20%", "30%", "40%", "50%")) +
    bbc_style() +
    labs(title = "Employee perfomance",
         subtitle = "% of year sales by invoices") +
    theme(legend.position = "right",
          axis.ticks.x = element_line(colour = "#333333"), 
          axis.ticks.length =  unit(0.26, "cm"))
    

# Saving plot

finalise_plot(plot_name = employee_sales_line_chart,
              source = "Source: Chinook record store database.",
              save_filepath = "img/employee_perfomance_linechart.png")


# 4.2. Visualization - bar chart ----

employee_sales_bar_chart <- employee_sales_tbl %>%
    ggplot(aes(x = as.numeric(year), 
               y = emp_sales,
               fill = employee)) +
    geom_bar(stat = "identity", 
             position = "dodge") +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    scale_fill_manual(values = c("#FAAB18", "#1380A1", "#3BB273", "#FF595E")) +
    scale_y_continuous(limits = c(0, 600),
                       breaks = seq(0, 600, by = 100)) +
    bbc_style() +
    labs(title = "Employee perfomance",
         subtitle = "Sales ($) by year") +
    theme(legend.position = "top",
          legend.justification = "left") +
    geom_text(aes(label = scales::percent(emp_sales_rate_by_year_sales/100, accuracy = 0.1)),
              position = position_dodge(width = 0.9),
              vjust = -0.25,
              colour = "#222222",
              family = "Helvetica",
              size = 4)

# Saving plot

finalise_plot(plot_name = employee_sales_bar_chart,
              source = "Source: Chinook record store database.",
              save_filepath = "img/employee_perfomance_barchart.png")



# Jane Peacock has the highest perfomance sales by year among the others. 
# Although she was the worst in 2019, her performance sales were greater than Margaret Park and Steve Johnson
# in 2017, 2018 and 2020. However, it is important to mention that Jane is the oldest employee at company, and
# probably because of that there is a 10% difference between her sales and the others in 2017. 
# Taking a look from 2018 to 2020 their perfomance were preaty similar.


# 5. Task 3 ----

# Analyze the sales data for customers from each different country. 
# You have been given guidance to use the country value from the customers table, 
# and ignore the country from the billing address in the invoice table.

country_purchases <- "
WITH
    country_purchases AS
        (
         SELECT
            CASE
                WHEN (SELECT COUNT(DISTINCT c.customer_id) FROM customer) = 1 THEN 'Other'
                ELSE c.country
                END AS country,
            COUNT(DISTINCT c.customer_id) cust_quant,
            COUNT(i.invoice_id) inv_quant,
            SUM(i.total) purchases,
            SUM(i.total)/COUNT(DISTINCT c.customer_id) purc_by_cust,
            SUM(i.total)/COUNT(i.invoice_id) purc_by_inv,
            CASE
                WHEN COUNT(DISTINCT c.customer_id) <= 1 THEN 1
                ELSE 0
                END AS sort
         FROM customer c
         LEFT JOIN invoice i ON c.customer_id = i.customer_id
         GROUP BY c.country
        ),
    other_country_purchases AS
        (
         SELECT 
             country,
             SUM(cust_quant),
             SUM(inv_quant),
             SUM(purchases),
             SUM(purchases)/SUM(cust_quant),
             SUM(purchases)/SUM(inv_quant),
             sort
         FROM country_purchases
         GROUP BY country
         HAVING country = 'Other'
        )

SELECT * FROM country_purchases
WHERE sort = 0

UNION

SELECT * FROM other_country_purchases
ORDER BY sort, purchases DESC
;
"
country_purchases_tbl <- run_query(country_purchases) %>%  tibble()


# 5.1. Visualization 1 ----
country_purchases_plot <- country_purchases_tbl %>% 
    ggplot(aes(x = reorder(country, purchases), 
               y = purchases,
               fill = purchases)) +
    geom_bar(stat = "identity", 
             position = "identity",
             fill = "#028090") +
    scale_y_continuous(limits = c(0, 1200),
                       breaks = seq(0, 1200, by = 200)) +
    coord_flip() +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    bbc_style() +
    labs(title = "Sales by country",
         subtitle = "In dollar (US$)") +
    theme(panel.grid.major.x = element_line(color = "#cbcbcb"), 
          panel.grid.major.y = element_blank()) 

# Saving plot

finalise_plot(plot_name = country_purchases_plot, 
              source = "Source: Chinook record store database.",
              save_filepath = "img/country_purchases.png")

# According to this analysis, we can see that the USA is the country that sold the most. 
# However, we should also take into account the average of total sales by number of customers and 
# how much each one is willing to spend by order.


# 5.2. Visualization 2 ----

avg_cust_purchases_plot <- country_purchases_tbl %>% 
    ggplot(aes(x = reorder(country, purc_by_cust), 
               y = purc_by_cust,
               fill = purc_by_cust)) +
    geom_bar(stat = "identity", 
             position = "identity",
             fill = "#e63946") +
    scale_y_continuous(limits = c(0, 150),
                       breaks = seq(0, 140, by = 20)) +
    coord_flip() +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    bbc_style() +
    labs(title = "Average sales by customer",
         subtitle = "In dollar (US$)") +
    theme(panel.grid.major.x = element_line(color = "#cbcbcb"), 
          panel.grid.major.y = element_blank()) 

# Saving plot

finalise_plot(plot_name = avg_cust_purchases_plot, 
              source = "Source: Chinook record store database.",
              save_filepath = "img/avg_cust_purchases.png")


# 5.3 Visualization 3 ----

avg_purchases_by_inv_plot <- country_purchases_tbl %>% 
    ggplot(aes(x = reorder(country, purc_by_inv), 
               y = purc_by_inv,
               fill = purc_by_inv)) +
    geom_bar(stat = "identity", 
             position = "identity",
             fill = "#f4a261") +
    scale_y_continuous(limits = c(0, 10),
                       breaks = seq(0, 10, by = 2)) +
    coord_flip() +
    geom_hline(yintercept = 0,
               size = 1,
               colour = "#333333") +
    bbc_style() +
    labs(title = "Average sales by invoice",
         subtitle = "In dollar (US$)") +
    theme(panel.grid.major.x = element_line(color = "#cbcbcb"), 
          panel.grid.major.y = element_blank()) 

# Saving plot

finalise_plot(plot_name = avg_purchases_by_inv_plot, 
              source = "Source: Chinook record store database.",
              save_filepath = "img/avg_purchases_by_inv.png")