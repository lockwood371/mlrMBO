AcquisitionFunctionEI = R6Class("AcquisitionFunctionEI",
  inherit = AcquisitionFunction,
  public = list(
    # public members

    se_threshold = numeric(1L),
    model = NULL,
    y_min = numeric(1L),

    # constructor
    initialize = function(se.threshold = 1e-6, attributes = FALSE) {
      self$se_threshold = assert_number(se.threshold, lower = 1e-20)
      super$initialize(id = "EI", name = "Expected Improvement", requirements = "se", opt_direction = "maximize", attributes = attributes)
    },

    # public methods
    prepare = function(opt.state) {
      opt.problem = getOptStateOptProblem(opt.state)
      control = getOptProblemControl(opt.problem)
      assertString(control$y.name)
      y = design[, control$y.name]
      assertNumeric(y, any.missing = FALSE)
      self$model = getOptStateModels(opt.state)[[1]]


    },

    evaluate_all = function(x) {
      p = predict(self$model, newdata = x)$data
      res = p$response + self$mult_inv_max * cb.lambda * p$se
      data.frame(acq = res, se = p$se, mean = p$response, lambda = self$lambda)
    }
  )
)
