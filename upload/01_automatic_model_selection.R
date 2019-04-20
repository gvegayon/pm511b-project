library(tidyr)
library(dplyr)
library(magrittr)

source("functions.R")
dat <- readRDS("model_data.rds")

vars <- colnames(dat)
vars <- setdiff(vars, c("sid", "race", "val", "asaps", "died"))

# Creating the models
formulas <- lapply(sprintf("died ~ %s", vars), as.formula)

# estimating the models
models   <- vector("list", length(formulas))
dat0 <- dat %>% filter(val == 0)
dat1 <- dat %>% filter(val == 1)
for (m in 1:length(formulas)) {
  models[[m]] <- glm(formulas[[m]], data = dat0, family = binomial("logit") )
  if ((!m %% 10))
    message("Iteration ", m, " out of ", length(models))
}

# Checking which ones where significant. For this we will use as criteria that
# all the pvalues in the model are significant.
significant <- vector("logical", length(formulas))



# Fixing the NAs
significant <- lapply(models, glm_pval, as_significant = TRUE) %>%
  bind_rows %>%
  mutate(significant = coalesce(significant, FALSE)) %>%
  extract2("significant")



model_with_2 <- add_variable(vars[significant])
model_with_3 <- add_variable(vars[significant], model_with_2$model)
model_with_4 <- add_variable(vars[significant], model_with_3$model)
model_with_5 <- add_variable(vars[significant], model_with_4$model)
model_with_6 <- add_variable(vars[significant], model_with_5$model)
model_with_7 <- add_variable(vars[significant], model_with_6$model)
model_with_8 <- add_variable(vars[significant], model_with_7$model)
model_with_9 <- add_variable(vars[significant], model_with_8$model)

models <- mget(ls(pattern = "model_with_"))
for (i in seq_along(models)) {
  models[[i]]$nmodels <- attr(models[[i]], "nmodels")
  models[[i]]$nvars   <- as.integer(gsub(".+with[_]", "",names(models)[i]))
}
models <- bind_rows(models) %>%
  arrange(desc(auc))

# Saving the last one
saveRDS(models, "automatic_model_selection.rds")
