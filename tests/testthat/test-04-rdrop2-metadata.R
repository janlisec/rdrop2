testthat::test_that(
  desc = "Able to retrieve metadata for file in multiple ways",
  code = {
    testthat::skip_on_cran()

    # upload new file to root
    tmp_file <- normalizePath(file.path(tempdir(), traceless("test-drop-get-metadata.csv")), mustWork = FALSE)
    write.csv(mtcars, tmp_file)
    testthat::expect_message(rdrop2::drop_upload(tmp_file), "uploaded")

    # lookup by path
    metadata <- rdrop2::drop_get_metadata(basename(tmp_file))

    testthat::expect_true(is.list(metadata))
    testthat::expect_equal(metadata$.tag, "file")

    # lookup by id
    testthat::expect_identical(metadata, rdrop2::drop_get_metadata(metadata$id))

    # lookup by revision
    testthat::expect_identical(metadata, rdrop2::drop_get_metadata(paste0("rev:", metadata$rev)))

    # delete
    rdrop2::drop_delete(basename(tmp_file))

    # get deleted metadata
    deleted_metadata <- rdrop2::drop_get_metadata(basename(tmp_file), include_deleted = TRUE)

    testthat::expect_true(is.list(deleted_metadata))
    testthat::expect_equal(deleted_metadata$.tag, "deleted")
    testthat::expect_identical(
      metadata[c("name", "path_lower", "path_display")],
      deleted_metadata[c("name", "path_lower", "path_display")]
    )

    # cleanup
    force_unlink(tmp_file)
})
