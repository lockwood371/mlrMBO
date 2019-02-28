AcquisitionFunctionMR = R6Class("AcquisitionFunctionMR",
  inherit = AcquisitionFunction,
  public = list(

    # public members

    model = NULL,

    # constructor
    initialize = function(attributes = FALSE) {
      super$initialize(id = "MR", name = "Mean Response", requirements = character(0L), opt_direction = "objective", attributes = attributes)
    },

    # public methods
    prepare = function(opt.state) {
      self$model = getOptStateModels(opt.state)[[1]]
    },

    evaluate_all = function(x) {
      p = predict(self$model, newdata = x)$data$response
      data.frame(acq = p)
    }
  )
)
