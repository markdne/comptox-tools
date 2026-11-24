.onLoad <- function(libname, pkgname) {
  # umap-learn used by run_umap(). 0.6.0 adds the new layout pipeline and keeps
  # the legacy (0.5.x) pipeline, so earlier embeddings can still be reproduced.
  # RINND is optional: users who want it call
  # reticulate::py_require("rinnd>=0.3.1") before Python starts.
  reticulate::py_require("umap-learn==0.6.0")
}
