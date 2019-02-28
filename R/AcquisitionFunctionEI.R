AcquisitionFunctionEI = R6Class("AcquisitionFunctionEI",
  inherit = AcquisitionFunction,
  public = list(
    # public members

    se_threshold = numeric(1L),
    model = NULL,
    y_min = numeric(1L),
    mult_max_to_min_fun = 1,

    # constructor
    initialize = function(se.threshold = 1e-6, attributes = FALSE) {
      self$se_threshold = assert_number(se.threshold, lower = 1e-20)
      super$initialize(id = "EI", name = "Expected Improvement", requirements = "se", opt_direction = "maximize", attributes = attributes)
    },

    # public methods
    prepare = function(opt.state) {
      opt.problem = getOptStateOptProblem(opt.state)
      control = getOptProblemControl(opt.problem)
      design = getOptStateDesigns(opt.state)[[1]]

      self$model = getOptStateModels(opt.state)[[1]]

      assertString(control$y.name)
      self$mult_max_to_min_fun = if (shouldBeMinized(getOptProblemFun(opt.problem))) 1 else -1
      self$y_min = min(self$mult_max_to_min_fun * design[, control$y.name])
      self$model = getOptStateModels(opt.state)[[1]]
    },

    evaluate_all = function(x) {
      p = predict(self$model, newdata = x)$data
      p.mu = self$mult_max_to_min_fun * p$response
      p.se = p$se
      d = self$y_min - p.mu
      xcr = d / p.se
      xcr.prob = pnorm(xcr)
      xcr.dens = dnorm(xcr)
      ei = d * xcr.prob + p.se * xcr.dens
      ei = ifelse(p.se < se.threshold, 0, ei)
      data.frame(acq = ei, se = p.se, mean = p.mu)
    }
  )
)
