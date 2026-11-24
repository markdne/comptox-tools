# Small binary fingerprint matrix shared by the tests
make_fingerprints <- function(n_rows = 60L) {
  withr::local_seed(11)

  fingerprints <- matrix(
    stats::rbinom(n_rows * 20L, 1, 0.3),
    nrow = n_rows,
    ncol = 20L
  )
  # Jaccard distance is undefined for all-zero rows
  fingerprints[rowSums(fingerprints) == 0, 1] <- 1
  rownames(fingerprints) <- sprintf("chem_%02d", seq_len(n_rows))

  return(fingerprints)
}

# Skip tests that need umap-learn >= 0.6 in the reticulate environment
skip_if_no_umap_learn <- function() {
  testthat::skip_if_not(
    reticulate::py_module_available("umap"),
    "umap-learn is not available"
  )

  installed_version <- as.character(reticulate::import("umap")$`__version__`)
  installed_numeric <- sub("^([0-9]+(\\.[0-9]+)*).*$", "\\1", installed_version)
  testthat::skip_if(
    numeric_version(installed_numeric) < numeric_version("0.6.0"),
    "umap-learn < 0.6.0"
  )
}


## Input validation (no Python needed) ----

test_that("run_umap() rejects invalid inputs", {
  fingerprints <- make_fingerprints()

  expect_error(run_umap(as.data.frame(fingerprints)), "numeric matrix")
  expect_error(run_umap(fingerprints, method = "uwot"))
  expect_error(run_umap(fingerprints, nearest_neighbour_backend = "annoy"))
  expect_error(run_umap(fingerprints, optimizer = "sgd"))
  expect_error(run_umap(fingerprints, legacy_compatibility_layout = "yes"), "TRUE")
  expect_error(run_umap(fingerprints, legacy_compatibility_layout = NA), "TRUE")
  expect_error(run_umap(fingerprints, random_state = 1.5), "whole number")
  expect_error(run_umap(fingerprints, n_jobs = 0), "n_jobs")
  expect_error(run_umap(fingerprints, n_jobs = 2.5), "n_jobs")
  expect_error(run_umap(fingerprints, init = "umap"), "init")
  expect_error(run_umap(fingerprints, init = matrix(0, nrow = 3, ncol = 2)), "init")
})

test_that("run_umap() refuses modern optimisers with the legacy pipeline", {
  fingerprints <- make_fingerprints()

  expect_error(run_umap(fingerprints, optimizer = "adam"), "new pipeline")
  expect_error(run_umap(fingerprints, optimizer = "momentum"), "new pipeline")
})

test_that("run_umap() limits naive initialisation to methods it supports", {
  fingerprints <- make_fingerprints()

  expect_error(
    run_umap(fingerprints, method = "naive", init = "recursive"),
    "naive"
  )
})


## Pure-R implementation ----

test_that("run_umap() runs the naive method and warns about ignored settings", {
  fingerprints <- make_fingerprints()

  naive_result <- run_umap(fingerprints, method = "naive", metric = "euclidean")

  expect_s3_class(naive_result, "umap")
  expect_equal(dim(naive_result$layout), c(nrow(fingerprints), 2L))

  expect_warning(
    run_umap(
      fingerprints,
      method                      = "naive",
      metric                      = "euclidean",
      legacy_compatibility_layout = FALSE,
      n_jobs                      = 2
    ),
    "ignored"
  )
})


## umap-learn checks (Python mocked) ----

test_that("check_umap_learn() explains a missing or outdated umap-learn", {
  local_mocked_bindings(
    py_module_available = function(...) FALSE,
    .package = "reticulate"
  )
  expect_error(check_umap_learn(), "not available")

  local_mocked_bindings(
    py_module_available = function(...) TRUE,
    import              = function(...) list(`__version__` = "0.5.12"),
    .package            = "reticulate"
  )
  expect_error(check_umap_learn(), "0.5.12")
})


## umap-learn pipelines (needs Python) ----

test_that("the legacy pipeline returns a reproducible umap object", {
  skip_if_no_umap_learn()
  fingerprints <- make_fingerprints()

  first_run <- run_umap(fingerprints)
  second_run <- run_umap(fingerprints)

  expect_s3_class(first_run, "umap")
  expect_equal(dim(first_run$layout), c(nrow(fingerprints), 2L))
  expect_identical(rownames(first_run$layout), rownames(fingerprints))
  expect_identical(first_run$layout, second_run$layout)
  expect_true(first_run$config$compatibility_layout)
  expect_identical(first_run$config$optimizer, "compatibility")
  expect_identical(first_run$config$n_jobs, 1L)
})

test_that("the new pipeline is reproducible on all cores and differs from legacy", {
  skip_if_no_umap_learn()
  fingerprints <- make_fingerprints()

  legacy_result <- run_umap(fingerprints)
  first_run <- run_umap(fingerprints, legacy_compatibility_layout = FALSE)
  second_run <- run_umap(fingerprints, legacy_compatibility_layout = FALSE)

  expect_identical(first_run$layout, second_run$layout)
  expect_false(isTRUE(all.equal(first_run$layout, legacy_result$layout)))
  expect_identical(first_run$config$optimizer, "adam")
  expect_identical(first_run$config$n_jobs, -1L)
})

test_that("legacy-only conflicts warn and fall back", {
  skip_if_no_umap_learn()
  fingerprints <- make_fingerprints()

  expect_warning(
    rinnd_result <- run_umap(fingerprints, nearest_neighbour_backend = "rinnd"),
    "PyNNDescent"
  )
  expect_identical(rinnd_result$config$nearest_neighbor_backend, "pynndescent")

  expect_warning(
    jobs_result <- run_umap(fingerprints, n_jobs = 4),
    "one core"
  )
  expect_identical(jobs_result$config$n_jobs, 1L)
})

test_that("RINND can be chosen for the new pipeline", {
  skip_if_no_umap_learn()
  skip_if_not(reticulate::py_module_available("rinnd"), "rinnd is not installed")
  fingerprints <- make_fingerprints()

  rinnd_result <- run_umap(
    fingerprints,
    legacy_compatibility_layout = FALSE,
    nearest_neighbour_backend   = "rinnd"
  )

  expect_equal(dim(rinnd_result$layout), c(nrow(fingerprints), 2L))
})

test_that("umap-learn results work with predict() and umap_layout()", {
  skip_if_no_umap_learn()
  fingerprints <- make_fingerprints()

  new_result <- run_umap(fingerprints, legacy_compatibility_layout = FALSE)

  expect_equal(dim(stats::predict(new_result, fingerprints[1:5, ])), c(5L, 2L))
  expect_equal(
    nrow(umap_layout(
      new_result,
      n_neighbours = 15,
      source_info  = rep("test", nrow(fingerprints))
    )),
    nrow(fingerprints)
  )
})
