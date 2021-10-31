#' dbInterface
#'
#' @description An R6 class used to interface with a database. Provides
#' wrapper functions for common database operations, such as querying, creating
#' tables, appending and writing to tables,
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom DBI Id dbConnect dbDisconnect
#' @importFrom DBI dbGetQuery dbExecute dbCreateTable dbWriteTable
#' @importFrom R6 R6Class
#' @importFrom stringi stri_replace_all
#'
#' @noRd
dbInterface <- R6::R6Class(
  classname = "dbInterface",

  private = list(
    connection = NA,
    driver = NA,
    host = NA_character_,
    port = NA_integer_,
    user = NA_character_,
    pass = NA_character_,
    db   = NA_character_,

    # optional fields for convenience
    schema = NA_character_,
    table = NA_character_
  ),

  public = list(

    ## class methods ----

    #' initialize
    #'
    #' @param drv A database connection driver.
    #' @param host A hostname to connect to.
    #' @param port A port to connect to.
    #' @param user A database username.
    #' @param pass A password to use to authenticate the user.
    #' @param db A database name to connect to.
    #' @param schema An (optional) schema name. Required for `query_self()`.
    #' @param table  An (optional) table name. Required for `query_self()`.
    #'
    #' @return self
    #'
    initialize = function(drv, host, port, user, pass, db,
                          schema = NA_character_, table = NA_character_) {
      self$set("driver", drv)
      self$set("host", host)
      self$set("port", port)
      self$set("user", user)
      self$set("pass", pass)
      self$set("db", db)

      self$set("schema", schema)
      self$set("table", table)

      tryCatch(
        expr = {
          self$connect()
          self$disconnect()
        },
        error = function(e) {
          message(e)
          stop("could not initialize a db connection")
        }
      )

      invisible(self)
    },

    #' get
    #'
    #' @param var A variable name to get.
    #'
    #' @return The private variable value.
    #'
    get = function(var) {
      return(private[[var]])
    },

    #' set
    #'
    #' @param var A variable name to set.
    #' @param val A variable value to set.
    #'
    #' @return self
    #'
    set = function(var, val) {
      private[[var]] <- val
      invisible(self)
    },

    ## connection ----

    #' connect
    #'
    #' @return self
    #'
    connect = function() {
      self$set(
        "connection",
        dbConnect(
          drv  = self$get("driver"),
          host = self$get("host"),
          port = self$get("port"),
          user = self$get("user"),
          password = self$get("pass"),
          dbname = self$get("db")
        )
      )
      invisible(self)
    },

    #' disconnect
    #'
    #' @return self
    #'
    disconnect = function() {
      dbDisconnect(self$get("connection"))
      invisible(self)
    },

    ## interaction ----

    #' generic
    #'
    #' @param fn A function to call.
    #' @param params A list of named parameters to pass to `fn`.
    #'
    #' @return A result of the called function.
    #'
    generic = function(fn, params) {
      self$connect()
      res <- do.call(fn, params)
      self$disconnect()
      return(res)
    },

    #' query
    #'
    #' @param sql A SQL query string, or local file path to a SQL query.
    #'
    #' @return The results of the query.
    #'
    query = function(sql) {
      if (file.exists(sql)) s = self$onelineq(sql) else s = self$lineclean(sql)
      self$generic(
        fn = dbGetQuery,
        list(
          conn = self$get("connection"),
          statement = s
        )
      )
    },

    #' query_self
    #'
    #' @description Class variables `schema` and `table` must be set (not NA).
    #'
    #' @return The results of a select * query from the class var `schema.table`.
    #'
    query_self = function() {
      if (is.na(self$get("schema"))) stop("class schema must be defined")
      if (is.na(self$get("table"))) stop("class table must be defined")

      self$generic(
        fn = dbGetQuery,
        list(
          conn = self$get("connection"),
          statement = self$lineclean(
            paste0("select * from ", self$get("schema"), ".", self$get("table"))
          )
        )
      )
    },

    #' query_self_param
    #'
    #' @description A side-effect is the `schema` and `table` inputs are set
    #' as class variables for future use.
    #'
    #' @param schema A schema name.
    #' @param table A table name.
    #'
    #' @return The results of a select * query from the class var `schema.table`.
    #'
    query_self_param = function(schema, table) {
      self$set("schema", schema)
      self$set("table", table)
      self$query_self()
    },

    #' query_self_param_clear
    #'
    #' @description A wrapper for `query_param()` which resets the class
    #' variables `schema` and `table` after querying.
    #'
    #' @param schema A schema name.
    #' @param table A table name.
    #'
    #' @return The results of a select * query from the class var `schema.table`.
    #'
    query_self_param_clear = function(schema, table) {
      res <- self$query_self_param(schema, table)
      self$set("schema", NA_character_)
      self$set("table", NA_character_)
      return(res)
    },

    #' execute
    #'
    #' @param sql A SQL query string, or local file path to a SQL query.
    #'
    #' @return The results of the execute.
    #'
    execute = function(sql) {
      if (file.exists(sql)) s = self$onelineq(sql) else s = self$lineclean(sql)
      self$generic(
        fn = dbExecute,
        params = list(
          conn = self$get("connection"),
          statement = s,
        )
      )
    },

    #' create
    #'
    #' @param schema A schema name to create the table in. Defaults to "public".
    #' @param table A table name to create.
    #' @param fields A named vector: names are column names, values are types.
    #'
    create = function(schema = "public", table, fields) {
      self$generic(
        fn = dbCreateTable,
        params = list(
          conn = self$get("connection"),
          name = Id(schema = schema, table = table),
          fields = fields
        )
      )
    },

    #' append
    #'
    #' @param schema A schema name to reference. Default is "public".
    #' @param table A table name to append to.
    #' @param data A data.frame to append to the table.
    #'
    append = function(schema = "public", table, data) {
      self$generic(
        fn = dbWriteTable,
        params = list(
          conn = self$get("connection"),
          name = Id(schema = schema, table = table),
          value = data,
          append = TRUE
        )
      )
    },

    #' write
    #'
    #' @param schema A schema name to reference. Default is "public".
    #' @param table A table name to write to.
    #' @param data A data frame to write to table.
    #'
    write = function(schema = "public", table, data) {
      self$generic(
        fn = dbWriteTable,
        params = list(
          conn = self$get("connection"),
          name = Id(schema = schema, table = table),
          value = data,
          overwrite = TRUE
        )
      )
    },

    ## class utils ----

    #' lineclean
    #'
    #' Cleans a string of whitespace, particularly of:
    #'  * tabs
    #'  * leading and trailing whitespace
    #'  * collapses multiple spaces
    #'  * destroys single line SQL comments
    #'
    #' @param x A string to remove various whitespace from.
    #'
    #' @returns A cleaned string.
    #'
    lineclean = function(x) {
      return(
        stri_replace_all(
          str = x,
          regex = c("\t+", "^\\s+", "\\s+$", "[ ]+", "^[-]+.*$"),
          replacement = " ",
          vectorize_all = FALSE
        )
      )
    },

    #' onelineq
    #'
    #' Parses a SQL file into a valid query string.
    #'
    #' @param file A SQL file on disk to parse into a single line string.
    #'
    #' @return A cleaned SQL query from file.
    #'
    onelineq = function(file) {
      return(
        paste(
          unlist(
            Filter(function(x) x != "", lapply(readLines(file), self$lineclean))
          ),
          collapse = " "
        )
      )
    },

    ## init utils ----
    #' create_if_not_exist
    #'
    #' Creates a table `kpthor.events` if it doesn't exist in the connected
    #' database.
    #'
    #' @return A return status code from the dbExecute.
    #'
    #' @importFrom DBI dbExecute
    create_if_not_exist = function() {
      query <- "create table if not exists kpthor.events (
        date date,
        time int,
        minute int,
        pet text,
        action text,
        value text
      );"
      self$generic(
        fn = dbExecute,
        params = list(
          conn = self$get("connection"),
          statement = self$lineclean(query)
        )
      )
    }

  )
)
