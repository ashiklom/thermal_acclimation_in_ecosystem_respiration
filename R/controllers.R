# Where the targets pipeline runs its tasks.
#
# On the YCRC Bouchet cluster (a hostname ending in ycrc.yale.edu) every crew
# worker is its own Slurm job; anywhere else workers are local R processes.
# Run size is set through environment variables (see submit.sh):
#   TAS_WORKERS          maximum number of workers (default: 4 local, 60 Slurm)
#   TAS_SLURM_PARTITION  Slurm partition for workers (default: day)
#   TAS_SLURM_MINUTES    wall time of each worker job (default: 1380)
#   TAS_SLURM_MEMORY_GB  memory of each worker job (default: 24)
# A worker has 4 CPUs: one brm() fit samples 4 chains in parallel.

#' Whether this process runs on YCRC Bouchet
#'
#' @return TRUE if the hostname ends in ycrc.yale.edu.
#' @noRd
on_bouchet <- function() grepl("ycrc\\.yale\\.edu$", Sys.info()[["nodename"]])

#' The crew controller of the targets pipeline
#'
#' On YCRC Bouchet a Slurm controller (every worker its own job with 4 CPUs, logging to
#' _logs/), anywhere else local R processes; sized by the environment variables in the file
#' header.
#'
#' @return A crew controller.
#' @noRd
pipeline_controller <- function() {
  if (on_bouchet()) {
    crew.cluster::crew_controller_slurm(
      name = "bouchet",
      workers = as.integer(Sys.getenv("TAS_WORKERS", "60")),
      host = controller_host(),
      seconds_idle = 300,
      tls = crew::crew_tls(mode = "none"),
      options_cluster = crew.cluster::crew_options_slurm(
        # One BLAS/OpenMP thread per process: the node's other cores belong to
        # other jobs, and multithreaded BLAS in 4 forked chains oversubscribes.
        script_lines = c("export OPENBLAS_NUM_THREADS=1", "export OMP_NUM_THREADS=1"),
        log_output = file.path(normalizePath("_logs"), "crew-%A.out"),
        log_error = file.path(normalizePath("_logs"), "crew-%A.err"),
        memory_gigabytes_required = as.numeric(Sys.getenv("TAS_SLURM_MEMORY_GB", "24")),
        cpus_per_task = 4,
        time_minutes = as.integer(Sys.getenv("TAS_SLURM_MINUTES", "1380")),
        partition = Sys.getenv("TAS_SLURM_PARTITION", "day")
      )
    )
  } else {
    crew::crew_controller_local(name = "local", workers = as.integer(Sys.getenv("TAS_WORKERS", "4")), seconds_idle = 60)
  }
}

#' The address workers use to reach this process
#'
#' The IP address workers on other nodes use to reach this process: the first IPv4 address that
#' is neither loopback nor link-local (crew's default can pick an address the compute nodes
#' cannot reach). Stops if there is none.
#'
#' @return The IPv4 address (character).
#' @noRd
controller_host <- function() {
  ips <- unlist(nanonext::ip_addr())
  usable <- ips[!grepl("^(127\\.|169\\.254\\.|::1|fe80:)", ips) & grepl("^[0-9.]+$", ips)]
  if (length(usable) == 0) stop("no usable IPv4 address for the crew controller: ", paste(ips, collapse = ", "))
  unname(usable[[1]])
}
