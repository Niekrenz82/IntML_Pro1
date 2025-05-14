
# Define your encoding pipe
pipe_encode = po("encode",
                 method         = "treatment",
                 affect_columns = selector_type("factor")
)

# Helper to wrap any learner in your pipe
make_graph_learner = function(learner) {
  gl = pipe_encode %>>% learner
  GraphLearner$new(gl)
}

# Common tuning components
resampling_inner   = rsmp("cv", folds = 2)
measure_mse        = msr("regr.mse")
tuner_rs           = tnr("random_search")
terminator_evals = trm("evals", n_evals = 50)
terminator_evals_small = trm("evals", n_evals = 20)

# 4) Auto‐tuners with the pipe in front

# 4a) Ranger, include tuning space in the learner using lts
base_ranger = lts(lrn("regr.ranger"))
gl_ranger   = make_graph_learner(base_ranger)
at_ranger   = AutoTuner$new(
  learner      = gl_ranger,
  resampling   = resampling_inner,
  measure      = measure_mse,
  tuner        = tuner_rs,
  terminator   = terminator_evals_small
)

# 4b) XGBoost, include tuning space in the learner using lts
base_xgb = lts(lrn("regr.xgboost"))
gl_xgb   = make_graph_learner(base_xgb)
at_xgb   = AutoTuner$new(
  learner      = gl_xgb,
  resampling   = resampling_inner,
  measure      = measure_mse,
  tuner        = tuner_rs,
  terminator   = terminator_evals_small       # pulls the xgboost‐space, namespaced
)

# 4c) lightgbm, include tuning space in the learner using lts
base_lgbm = mlr3extralearners::lrn("regr.lightgbm",
                                   learning_rate = to_tune(0.005, 0.1, logscale = TRUE),
                                   num_leaves = to_tune(10, 512),
                                   max_depth = to_tune(3, 15),
                                   feature_fraction = to_tune(0.6, 1.0),
                                   bagging_fraction = to_tune(0.6, 1.0))
gl_lgbm   = make_graph_learner(base_lgbm)
at_lgbm   = AutoTuner$new(
  learner      = gl_lgbm,
  resampling   = resampling_inner,
  measure      = measure_mse,
  tuner        = tuner_rs,
  terminator   = terminator_evals        # pulls the xgboost‐space, namespaced
)

# 5) Other “plain” learners also piped

# 5a) Lasso
gl_lasso = make_graph_learner(
  lrn("regr.glmnet", id = "lasso", alpha = 1)
)

# 5b) Ridge
gl_ridge = make_graph_learner(
  lrn("regr.glmnet", id = "ridge", alpha = 0)
)

# 5c) Featureless baseline
gl_featless = make_graph_learner(lrn("regr.featureless"))

