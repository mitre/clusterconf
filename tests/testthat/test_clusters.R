
test_that("get_cluster_name works with spaces" , {
  expect_equal(get_cluster_name("abc def ghi"), "abcdefghi")
})

test_that("get_cluster_name works with caps" , {
  expect_equal(get_cluster_name("ABC"), "abc")
})

test_that("get_cluster_name works with no caps" , {
  expect_equal(get_cluster_name("abc"), "abc")
})

test_that("get_config_pkg_prepend works", {
  expect_equal(clusterconf:::get_config_pkg_prepend(),
               "clusterconf.")
})

test_that("get_cluster_package_name works", {
  expect_equal(get_cluster_package_name("ABC DEF GHI"),
               "clusterconf.abcdefghi")
})

test_that("list_available_clusters works with no installed clusterconf packages", {
  with_mock(
    installed.packages = function() { matrix("abc") }
  )

  expect_equal(list_available_clusters(installed_only = TRUE), character(0L))  
})

test_that("list_available_clusters works with some installed clusterconf packages", {
  test_clusters <- c("abc", "def")
  conf_packages <- paste0("clusterconf.", test_clusters)
  
  with_mock(
    installed.packages = function() {
      return(matrix(c("xyz", conf_packages)))
    },
    available_clusters <- list_available_clusters(installed_only = TRUE)
  )
  
  expect_equal(available_clusters, test_clusters)  
})

test_that("list_available_clusters works with no available clusterconf packages", {
  with_mock(
    installed.packages = function() { matrix("abc") },
    available.packages = function() { matrix("xyz") },
    available_clusters <- list_available_clusters(installed_only = FALSE)
  )
  
  expect_equal(available_clusters, character(0L))
})

test_that("list_available_clusters works with some available clusterconf packages", {
  test_clusters <- c("abc", "def")
  conf_packages <- paste0("clusterconf.", test_clusters)
  
  with_mock(
    installed.packages = function() { matrix("abc") },
    available.packages = function() { 
      matrix(c("xyz", conf_packages)) 
    },
    available_clusters <- list_available_clusters(installed_only = FALSE)
  )

  expect_equal(available_clusters, test_clusters)
})

test_that("list_available_clusters doesn't match string in middle of package name", {
  with_mock(
    installed.packages = function() { matrix("abc.clusterconf") },
    available.packages = function() { matrix("xyz.clusterconf") },
    available_clusters <- list_available_clusters(installed_only = FALSE)
  )
  
  expect_equal(available_clusters, character(0L))
})

test_that("is_cluster_known correctly determines a known cluster", {
  with_mock(
    installed.packages = function() { matrix("clusterconf.abc") },
    expect_equal(is_cluster_known("abc"), TRUE)    
  )
})

test_that("is_cluster_known notifies about uninstalled, but available, package", {
  with_mock(
    installed.packages = function() { matrix("def") },
    available.packages = function() { matrix("clusterconf.abc") },
    expect_message(is_cluster_known("abc", install = "no"), "is not installed"),
    expect_false(is_cluster_known("abc"))
  )
})

test_that("is_cluster_known issues warning for unavailable package", {
  with_mock(
    installed.packages = function() { matrix("abc") },
    available.packages = function() { matrix("def") },
    expect_warning(is_cluster_known("xyz"), "no configuration package was found"),
    expect_false(is_cluster_known("xyz"))
  )
})

test_that("is_cluster_known issues error for unavailable package with stopifnot", {
  with_mock(
    installed.packages = function() { matrix("abc") },
    available.packages = function() { matrix("def") },
    expect_error(is_cluster_known("xyz", stopifnot = TRUE), "no configuration package was found"),
    expect_false(is_cluster_known("xyz"))
  )
})

