#' Check to see if cluster has known configurations
#' 
#' This function will check to see if configurations for the named cluster exist. In
#' the even that configurations are available in a configured repository but not installed
#' locally then the user will be prompted to install the configurations.
#' 
#' @param cluster_name Character. The name of the cluster. Capitolization and spacing
#'   is ignored and does not matter (e.g., "My Cluster" is equivalent to "mycluster")
#' @param install Character. Should be one of \code{"ask"}, \code{"yes"}, or \code{"no"},
#'   case insensitive. First letter abbreviations are suficient.
#' @param stopifnot Logical. If \code{TRUE} then in the event a cluster is not known
#'   an error will result. Otherwise that case will trigger a warning and a \code{FALSE}
#'   return value.
#' @export
is_cluster_known <- function(cluster_name, install="ask", stopifnot=FALSE) {
  
  install <- tolower(install)
  config_pkg <-get_cluster_package_name(cluster_name)
  if (config_pkg %in% installed.packages()[, 1]) {
    return(TRUE)
  } else if (config_pkg %in% list_available_clusters()) {
    message("The cluster configuration package '", config_pkg, "', corresponding to the '",
            config_pkg, "' cluster is not installed. It is however a cluster with known configuration. ")
    if (grepl("^a", install))
      response <- readline("Would you like to install the configurations now? [y/n]: ")
    else
      response <- "no"
    if (grepl("^y", install) || grepl("^y", tolower(response))) {
      install.packages(config_pkg)
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    if (stopifnot)
      f <- stop
    else
      f <- warning
    f(paste0("The cluster configuration package '", config_pkg, "', corresponding to the '",
             cluster_name, "' cluster is not installed. Moreover no configuration package was found ", 
             "at any of the currently configured repositories. ",
             "Use the function 'list_available_clusters()' to show cluster configuration packages ",
             "that are locally installed as well as those available in the repositories that ",
             "your R session knows about (listed using 'getOption(\"repos\")'). ",
             "Likely issues include a typo (or otherwise incorrect cluster specification name) ",
             "or else perhaps the requisite package repository is not configured."))
    return(FALSE)
  }
}

#' @rdname is_cluster_known
#' @param installed_only Logical indicator of whether to only look at installed 
#'   packages rather than available packages.  This can save time, especially in
#'   the first call from a new session, relative to looking in all available
#'   packages.  Default FALSE.
#' @export
list_available_clusters <- function(installed_only=FALSE) {
  pattern <- paste0("^", get_config_pkg_prepend())
  
  if (installed_only) {
    ap <- installed.packages()[, 1]
  } else {
    ap <- c(available.packages()[, 1], installed.packages()[, 1])
  }
  config_pkgs <- ap[grepl(pattern, ap)]
  cluster_names <- gsub(pattern, "", config_pkgs)
  names(cluster_names) <- NULL
  return(unique(cluster_names))
}

#' @rdname is_cluster_known
#' @export
get_cluster_name <- function(cluster_name) {
  return(tolower(gsub(" ", "", cluster_name)))
}

#' @rdname is_cluster_known
#' @export
get_cluster_package_name <- function(cluster_name) {
  return(paste0(get_config_pkg_prepend(), get_cluster_name(cluster_name)))
}

get_config_pkg_prepend <- function() {
  return("clusterconf.")
}