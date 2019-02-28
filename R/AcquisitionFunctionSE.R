AcquisitionFunctionSE = R6Class("AcquisitionFunctionSE",
  inherit = AcquisitionFunction,
  public = list(

    # public members

    model = NULL,

    # constructor
    initialize = function(attributes = FALSE) {
      super$initialize(id = "SE", name = "Standard Error", requirements = "se", opt_direction = "maximize", attributes = attributes)
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
