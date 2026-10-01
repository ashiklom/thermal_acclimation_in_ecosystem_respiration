# The crew controller `_targets.R` runs under.
#
# Local: 8 workers, each fit running `N_CORES` chains. Slurm (YCRC): one job
# per worker, sized by `THERMAL_SLURM_WORKERS`/`THERMAL_SLURM_MINUTES`, which
# `submit.sh` sets. Every setting below was learned from a failed run; see
# docs/running-on-ycrc.md before changing one.
pipeline_controller <- function(host = system2("hostname", stdout = TRUE)) {
  if (!grepl("ycrc.yale.edu", host, fixed = TRUE)) {
    return(crew::crew_controller_local(workers = 8))
  }

  # Absolute, because a worker inherits its working directory from `sbatch`.
  log_dir <- file.path(getwd(), "_logs")
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  crew.cluster::crew_controller_slurm(
    workers = as.integer(Sys.getenv("THERMAL_SLURM_WORKERS", "20")),
    host = controller_host(),
    seconds_idle = 600,
    # TLS was implicated in workers dying early on this cluster.
    tls = crew::crew_tls(mode = "none"),
    options_cluster = crew.cluster::crew_options_slurm(
      # Keep at or above the controller's own --time in submit.sh.
      time_minutes = as.integer(Sys.getenv("THERMAL_SLURM_MINUTES", "1425")),
      # One task with N_CORES CPUs, so brms's chains share one node.
      n_tasks = 1,
      cpus_per_task = N_CORES,
      # `%A_%a`: crew.cluster submits each batch of workers as a job array.
      log_output = file.path(log_dir, "crew-%A_%a.out"),
      log_error = file.path(log_dir, "crew-%A_%a.err"),
      memory_gigabytes_required = 32,
      # pthreads OpenBLAS sizes its pool from the node's cores, not the job's;
      # uncapped, crowded workers abort on start. Stan does the arithmetic.
      script_lines = c(
        "export OPENBLAS_NUM_THREADS=1",
        "export OMP_NUM_THREADS=1"
      ),
      verbose = TRUE
    )
  )
}

# The address workers dial back to. crew's default is the first of
# `nanonext::ip_addr()`, and some YCRC nodes list a link-local (169.254.x.x)
# management interface first, which no other node can reach: with the
# controller on one, every worker off its node timed out dialling it. The
# `cluster` interface is the one the node's hostname resolves to.
controller_host <- function(ips = nanonext::ip_addr()) {
  routable <- ips[!startsWith(ips, "127.") & !startsWith(ips, "169.254.")]
  if (!length(routable)) stop("No routable IPv4 address for the crew controller.")
  if ("cluster" %in% names(routable)) routable[["cluster"]] else unname(routable[[1]])
}
