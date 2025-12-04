testthat::test_that(
  desc = "Able to authenticate from saved RDS token",
  code = {
    testthat::skip_on_cran()

    # set up token for authentication from environmental variable/github secret
    if (!is.na(Sys.getenv("RDROP2_TOKEN_B64", unset = NA_character_))) {
      token_b64 <- Sys.getenv("RDROP2_TOKEN_B64", unset = NA_character_)
      if (nzchar(token_b64)) {
        token_file <- normalizePath(file.path(tempdir(), "token.rds"), mustWork = FALSE)
        raw <- tryCatch(openssl::base64_decode(token_b64), error = function(e) NULL)
        if (!is.null(raw)) {
          writeBin(raw, token_file)
          options(rdrop2.token_file = token_file)
        }
      }
      testthat::expect_true(inherits(rdrop2::drop_auth(rdstoken = token_file), "Token2.0"))
    } else {
      warning("token.rds not available")
    }

    # read cached token and check its class
    #testthat::expect_true(inherits(rdrop2::drop_auth(rdstoken = "token.rds"), "Token2.0"))
})

testthat::test_that(
  desc = "Account information works correctly",
  code = {
    testthat::skip_on_cran()
    acc_info <- rdrop2::drop_acc()

    # expect list with sublists
    testthat::expect_true(is.list(acc_info))
    testthat::expect_true(is.list(acc_info$name))
})
