# Running the pipeline on YCRC

`submit.sh` runs the full grid:
every site × every recipe × both models at the manuscript's sampler settings.
The job it submits is only the controller.
`crew_controller_slurm()` in `pipeline_controller()` (R/controllers.R)
launches the workers as their own Slurm jobs,
and `pipeline_controller()` picks the Slurm controller
whenever the host name contains `ycrc.yale.edu`.
Anywhere else it uses a local controller with 8 workers.

```bash
sbatch submit.sh                  # the full grid
scripts/scan-and-run.sh           # or: scan providers, submit only if something changed
```

The run is configured by environment variables that `submit.sh` exports.
`THERMAL_SITES`, `THERMAL_RECIPES`, `THERMAL_MODELS` and `THERMAL_FIT`
are covered in [docs/recipes.md](recipes.md).
Two more size the cluster:

| variable | `submit.sh` | default | meaning |
|---|---|---|---|
| `THERMAL_SLURM_WORKERS` | 192 | 20 | maximum concurrent worker jobs |
| `THERMAL_SLURM_MINUTES` | 1425 | 1425 | wall time per worker job |

Keep `THERMAL_SLURM_MINUTES` at or above the controller's own `--time`
(23:45 = 1425 min).
All the workers launch at the start of the run.
If their limit is shorter than the controller's,
every one of them is killed at the same moment, mid-fit,
and crew then resubmits the whole fleet at once to a busy partition.

## Worker settings, and why each one is there

Each of these was learned from a failed or degraded full run.

- **`OPENBLAS_NUM_THREADS=1`, `OMP_NUM_THREADS=1`**,
  set in the worker job script (`script_lines`)
  and in `submit.sh` for the controller.
  - This R links pthreads OpenBLAS (`libopenblasp-r0.3.33.so`),
    which sizes its thread pool from the node's core count and ignores the cgroup.
    A worker holding 4 CPUs on a 96-core node tried to start about 96 BLAS threads.
  - On 2026-09-22, every worker that landed on a crowded node aborted (exit 134)
    about a second after starting, at around 120 MB RSS.
    That was 24 of 24 on a1130u31n04 and 16 of 16 on a1132u35n03.
    Workers on nodes that drew only one or two survived.
  - The cap costs nothing: Stan does the arithmetic,
    and `backend = "cmdstanr"` runs each chain as its own single-threaded process.
  - `pipeline_controller()` sets these for the workers explicitly
    rather than relying on `sbatch` to pass the environment on,
    since whether it does is a site setting.
- **`n_tasks = 1, cpus_per_task = N_CORES`**, not `n_tasks = 4`.
  That is `--ntasks=1 --cpus-per-task=4`.
  - `--ntasks=4` asks for four CPUs with no locality constraint.
    On the first full run only 26 of 68 workers got all four on one node.
  - brms runs its `N_CORES` chains as local processes inside a single R session,
    so the others were sampling four chains
    on the one or two CPUs they had on that node.
- **`memory_gigabytes_required = 32`.**
  - Under the partition default of 20 GB,
    the largest worker of the first run peaked at 20,971 MB,
    flat against the ceiling,
    and several were OUT_OF_MEMORY-killed mid-fit.
  - 32 GB is that observed peak
    plus room for the sites that never got far enough to report one.
- **Worker logs in `_logs/crew-%A_%a.{out,err}`.**
  - crew.cluster sends them to /dev/null by default.
  - On 2026-09-22 the pipeline lost 87 of its 96 workers in the first four seconds,
    then sat for 22 hours with nothing completed,
    because crew only rescales when a task completes.
    The cause had to be reconstructed from `sacct` placement data.
  - The path is absolute
    because a worker inherits its working directory from wherever `sbatch` ran.
- **`tls = crew_tls(mode = "none")`**: plain TCP between controller and workers.
  - On an earlier run on this cluster,
    crew's default self-signed TLS was implicated
    in workers being terminated far too early.
    The problem sits somewhere low in the nanonext/mbedtls layer,
    and "turning it off made it stop" is as far as it was pinned down.
  - If TLS goes back on, watch for workers dying early,
    not for anything that looks like a certificate error.
  - The trade-off:
    task payloads (flux-tower data) cross the internal HPC network unencrypted.
- **`host = controller_host()`**: the address workers dial back to.
  - crew's default is the first of `nanonext::ip_addr()`.
    Some nodes (c1104u05n02, for one)
    list a link-local USB management interface (169.254.1.2) first.
  - On 2026-10-01 the controller landed on such a node:
    the 3 workers on its own node connected,
    and all 60 elsewhere exited after 13 s
    with `dial ... Timed out` in their `.err` logs.
  - `controller_host()` skips loopback and link-local addresses
    and prefers the `cluster` interface,
    the one the node's hostname resolves to.
- **`seconds_idle = 600`.**
  Idle workers are handed back during the tail of the run,
  when fewer tasks remain than workers.
  crew relaunches them on demand.

## Sizing

These figures come from the 108 fits the first full run completed.

- **Mean fit time: 3914 s.**
  - By model, `direct` averaged 4077 s and `total` 3177 s.
    The completed sample was 60 % `total`.
  - The mean is weighted by the recipe × model grid rather than by that sample.
- **Remaining work:** 1530 fits, or about 1660 core-hours.
- **96 workers** × 4 CPUs is 384 CPUs, about 3 % of the `day` partition.
  That clears the remaining work inside one 23:45 window.

Packing is not the problem to fix.
The `day` partition ranges from 48-core/368 GB nodes to 192-core/1.5 TB ones,
so 24 workers landing together on a 96-core node is Slurm doing its job.
The 32 GB request would not have spread them out on a node that size.
What killed them was the BLAS threads.

## Local worker count

With 18 cores and the development sample, a benchmark of `crew_controller_local()`:

| workers | load avg | sum of target times | wall |
|---|---|---|---|
| 10 | 60 | 223.0 min | 49.9 min |
| 4 | 13 | 154.0 min | 59.8 min |

- With 4 workers each fit ran 31 % faster, but the whole run took 20 % longer.
  Twelve targets over 4 workers is three scheduling waves,
  while 10 workers ran nearly all of them at once.
- Much of a target's life is serial R work,
  so sizing workers at cores / `N_CORES` leaves the machine idle.
- 8 is the midpoint and has **not** been benchmarked.
