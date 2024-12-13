library(shiny)
library(bslib)
library(future)
library(promises)
library(pool)

require(RSQLite)

future::plan("multisession")

pool <- pool::dbPool(
  drv = RSQlite::SQLite(),
  db = "app.db"
)

pool::dbWriteTable(pool, "mtcars", mtcars, overwrite = TRUE)
