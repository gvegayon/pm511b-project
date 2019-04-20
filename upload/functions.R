# This function computes pvalues
glm_pval <- function(m, as_significant = TRUE, level  = .05) {
  
  betas <- stats::coef(m)
  vmat  <- stats::vcov(m)
  
  ans <- rbind(
    t      = 2*stats::pt(-abs(betas/sqrt(diag(vmat))), df = m$df.residual),
    nromal = 2*stats::pnorm(-abs(betas/sqrt(diag(vmat))))
  )
  
  if (as_significant) {
    # Computing aucs in observed data
    aucs <- pROC::ci.auc(
      response  = as.factor(m$y),
      predictor = m$fitted.values
    )
    
    # Doing the same in the cross-validation data
    pr <- predict(m, newdata = dat1)
    aucs1 <- pROC::ci.auc(
      response  = as.factor(dat1$died),
      predictor = pr
    )
    
    tibble::tibble(
      significant   = all(ans["t", ] < level),
      aic           = stats::AIC(m),
      auc           = aucs[2],
      auc_lower     = aucs[1],
      auc_upper     = aucs[3],
      auc_out       = aucs1[2],
      auc_lower_out = aucs1[1],
      auc_upper_out = aucs1[3]
    )
  }
  else
    ans
  
}

#' @param vars_pool Character vector. List of variables that can be used
#' in the model.
#' @param baseline_models Character vector. Current list of Models to be used.
add_variable <- function(
  vars_pool,
  baseline_models = NULL,
  nmodels         = 100L,
  mc.cores        = 4L
) {
  
  # Combinations of variables
  if (length(baseline_models)) {
    
    vars_next <- expand.grid(var = vars_pool, model = baseline_models) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        var   = as.character(var),
        model = as.character(model)
      )
    
    vars_next <- paste0(vars_next$model, " + ", vars_next$var)
    
  } else {
    
    vars_next <- t(combn(vars_pool, 2L))
    vars_next <- sprintf("died ~ %s + %s", vars_next[,1], vars_next[,2])
    
  }
  
  # Making sure we are not duplicating models/variables
  message("Checking duplicated vars/models (", length(vars_next), ") ...", appendLF = FALSE)
  vars_next <- gsub(".+[~]\\s*", "", vars_next)
  vars_next <- lapply(vars_next, strsplit, split = "\\s*[+]\\s*")
  vars_next <- lapply(vars_next, "[[", 1)
  vars_next <- lapply(vars_next, trimws)
  vars_next <- lapply(vars_next, sort)
  vars_next <- lapply(vars_next, unique)
  vars_next <- sapply(vars_next, paste0, collapse = " + ")
  vars_next <- unique(sprintf("died ~ %s", vars_next))
  message("Final size of models to estimate ", length(vars_next))
  
  # First with 2, so that way we can reasses the models
  formulas <- sapply(vars_next, as.formula)
  
  # Running the models
  message("Estimating ", length(formulas), " models in parallel...", appendLF = FALSE)
  models <- parallel::mclapply(formulas, function(fm) {
    glm_pval(glm(fm, data = dat0, family = binomial("logit") ))
  }, mc.cores = 4L) %>% dplyr::bind_rows()
  message(" done.")
  
  
  # Pick the top 100
  models_top_100 <- models %>%
    mutate(
      index = 1:n(),
      model = vars_next
    ) %>%
    # filter(significant == TRUE) %>%
    arrange(desc(auc)) %>%
    head(nmodels)
  
  # Listing next top variables
  structure(
    models_top_100,
    nmodels = length(formulas)
  )
}