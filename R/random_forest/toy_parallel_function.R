#in code.R, an example that uses parallel.  Use this to get started with local testing:
myfun <- function(n) 
{
  ## '8' this is the size of the cluster.  This line will take a  
  ## second or so to boot the cluster.
  cl <- parallel::makePSOCKcluster(8)
  ## always shut down the cluster even on failure.  This line is very
  ## important!
  on.exit(parallel::stopCluster(cl))
  ## A silly function to parallelise.  In real uses you may have to
  ## get use some clusterEvalQ and clusterExport commands here too.
  ## Test locally.
  f <- function(i) 
  {
    c(i, Sys.getpid())
  }
  x <- seq_len(n)
  simplify2array(parallel::clusterApply(cl, x, f))
}
