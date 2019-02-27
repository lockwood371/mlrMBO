AcquisitionFunctionCB = R6Class("AcquisitionFunctionCB",
  inherit = AcquisitionFunction,
  public = list(
    # public members

    lambda = numeric(1L),
    model = NULL,

    # constructor
    initialize = function(lambda = 2, attributes = FALSE) {
      self$lambda = assert_number(lambda, lower = 0)
      super$initialize(id = "CB", name = "Confidence Bound", requirements = "se", opt_direction = "objective", attributes = attributes)
    },

    # public methods
    prepare = function(opt.state) {
      opt.problem = getOptStateOptProblem(opt.state)
      fun = getOptProblemFun(opt.problem)
      self$opt_direction = ifelse(shouldBeMinimized(fun), "minimize", "maximize")

      self$model = getOptStateModels(opt.state)[[1]]
    },

    evaluate_all = function(x) {
      p = predict(self$model, newdata = x)$data
      res = p$response + self$mult_inv_max * cb.lambda * p$se
      data.frame(acq = res, se = p$se, mean = p$response, lambda = self$lambda)
    }
  )
)
