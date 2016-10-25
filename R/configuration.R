#' Get cluster configurations
#' 
#' Find and read the cluster configuration file for a named cluster.
#' 
#' @param cluster_name Character. The name of the cluster. Capitolization and spacing
#'   is ignored and does not matter (e.g., "My Cluster" is equivalent to "mycluster")
#' @param scope Character. The scope of the configurations of interest. The configuration
#'   YAML may be organized according to 
#' @importFrom config get
#' @export
get_cluster_configs <- function(cluster_name, scope) {
  configs <- config::get(get_yaml_configuraions_path(cluster_name))
  
  if (!missing(scope)) {
    if (!(scope %in% names(configs))) {
      warning(paste0("Cannot return configurations for scope: '", scope, "'. ",
                     "Valid scopes are: '", paste0(names(configs), collapse="', '"),
                     "'. Returning NULL."))
      configs <- NULL
    } else {
      configs <- configs[[scope]]
    }
  }
  return(configs)
}

#' @rdname get_cluster_configs
#' @export
get_yaml_configuraions_path <- function(cluster_name) {
  pkg_path <- find.package(get_cluster_package_name(cluster_name))
  if (!dir.exists(file.path(pkg_path, "configs")))
    pkg_path <- file.path(pkg_path, "inst")
  
  path <- file.path(pkg_path, "configs")
  fname <- dir(path, pattern = "yaml$", include.dirs = FALSE)[1]
  return(file.path(path, fname))
}

#' Get java classpath
#' 
#' Some cluster configuration packages contain java dependencies. This utility will
#' use the jar names from the configuration YAML and find the full file paths to the
#' stated dependencies.
#' 
#' @param cluster_name Character. The name of the cluster. Capitolization and spacing
#'   is ignored and does not matter (e.g., "My Cluster" is equivalent to "mycluster")
#' @param jars Character. A vector of jar files to include on the classpath. These should
#'   be listed using the full file name, including the ".jar" extension at the end.
#' @export
get_java_classpath <- function(cluster_name, jars) {
  java_path <- get_java_dependencies_path(cluster_name)
  files <- dir(java_path, include.dirs=FALSE)
  id <- jars %in% files
  if (any(!id))
    warning(paste0("Unable to locate dependencies: '", paste0(jars[!id], collapse="', '"), "'"))
  return(file.path(java_path, jars[id]))
}

#' @rdname get_java_classpath
#' @export
get_java_dependencies_path <- function(cluster_name) {
  pkg_path <- find.package(get_cluster_package_name(cluster_name))
  if (!dir.exists(file.path(pkg_path, "java")))
    pkg_path <- file.path(pkg_path, "inst")
  
  return(file.path(pkg_path, "java"))
}


get_cluster_name <- function(cluster_name) {
  return(tolower(gsub(" ", "", cluster_name)))
}

get_cluster_package_name <- function(cluster_name) {
  return(paste0("clusterconf.", get_cluster_name(cluster_name)))
}