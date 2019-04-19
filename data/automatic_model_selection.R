library(tidyr)
library(dplyr)
library(magrittr)

source("functions.R")
dat <- readRDS("data/model_data.rds")

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
saveRDS(models, "data/automatic_model_selection.rds")

# Now, adding the variable for the out of sample -------------------------------

# Mapping categories
# 1: "Healthy"
# 2: "Mild systemic disease"
# 3: "Severe systemic disease"
# 4: "Severe systemic disease that is a constant threat to life"
# 5: "Not expected to survive without operation"
# asaps_levels <- c("Healthy",
# "Mild sys dis",
# "Sev sys dis",
# "Sev sys dis const threat to life",
# "Not expected to surv w o operation")
# 
# dat$asaps2 <- asaps_levels[dat$asaps]
# dat$asaps2 <- factor(dat$asaps2, levels = asaps_levels)

dat <- dat %>%
  mutate(asaps = as.factor(asaps)) %>%
  model.matrix(~ asaps, data = .) %>%
  as_tibble() %>%
  select(-1) %>%
  set_colnames(tolower(colnames(.))) %>%
  set_colnames(gsub("^asaps", "asaps_", colnames(.))) %>%
  set_colnames(gsub("([-]|\\s+)", "_", colnames(.))) %>%
  bind_cols(dat, .) %>%
  # select(-asaps2) %>%
  mutate_at(vars(starts_with("asaps_")), as.integer)



# Which variables we need to create the interactions
vars <- colnames(dat) %>%
  extract(!grepl("__", .)) %>%
  setdiff(c("sid", "race", "val", "asaps", "died"))

# Creating interactions
vars <- expand.grid(vars[grepl("asaps", vars)], vars, stringsAsFactors = FALSE)
vars <- t(apply(vars, 1, sort)) %>% unique
vars <- vars[vars[,1] != vars[,2],]
for (i in 1:nrow(vars)) {
  dat[[paste0(vars[i, 1], "__", vars[i, 2])]] <- 
    dat[[vars[i, 1]]] * dat[[vars[i, 2]]]
}

vars <- paste0(vars[,1], "__", vars[,2])


models$model
