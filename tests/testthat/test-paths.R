test_that("lv_path prefers the environment variable over the default", {
  withr::local_envvar(LIPDVERSE_DATABASE = "/tmp/somewhere")
  expect_equal(lv_path("database"), "/tmp/somewhere")
})

test_that("lv_path expands ~ and errors helpfully when required and absent", {
  withr::local_envvar(LIPDVERSE_DATABASE = "~/definitely/not/here")
  expect_match(lv_path("database"), "^/")
  expect_error(lv_path("database", must_exist = TRUE), class = "lv_error_path")
})

test_that("an empty environment variable is an error, not a silent default", {
  # A variable that is set but blank almost always means a misconfigured
  # wrapper script. Falling back to the default would silently point the
  # pipeline at the real database.
  withr::local_envvar(LIPDVERSE_QCSTORE = " ")
  expect_error(lv_path("qcstore"), "set but empty")
})

test_that("an unset variable does use the default", {
  withr::local_envvar(LIPDVERSE_QCSTORE = NULL)
  expect_match(lv_path("qcstore"), "lipdverse-qcstore$")
})

test_that("lv_secret reads from the environment as JSON", {
  withr::local_envvar(LIPDVERSE_SECRET_MYSQL = '{"user":"u","password":"p"}')
  s <- lv_secret("mysql")
  expect_equal(s$user, "u")
  expect_equal(s$password, "p")
})

test_that("lv_secret falls back to the state directory", {
  st <- withr::local_tempdir()
  withr::local_envvar(LIPDVERSE_STATE = st, LIPDVERSE_SECRET_MYSQL = "")
  dir.create(file.path(st, "secrets"))
  jsonlite::write_json(list(user = "fromfile"), file.path(st, "secrets", "mysql.json"),
                       auto_unbox = TRUE)
  expect_equal(lv_secret("mysql")$user, "fromfile")
})

test_that("a missing secret errors unless optional", {
  withr::local_envvar(LIPDVERSE_STATE = withr::local_tempdir(), LIPDVERSE_SECRET_NOPE = "")
  expect_error(lv_secret("nope"), class = "lv_error_secret")
  expect_null(lv_secret("nope", required = FALSE))
})
