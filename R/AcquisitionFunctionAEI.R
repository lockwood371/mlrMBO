AcquisitionFunctionAEI = R6Class("AcquisitionFunctionAEI",
  inherit = AcquisitionFunction,
  public = list(
    # public members

    aei_use_nugget = logical(1L),
    se_threshold = numeric(1L),
    model = NULL,
    effective_best_configuration = NULL,
    mult_max_to_min_fun = 1,
    pure_noise_var = numeric(1L)

    # constructor
    initialize = function(aei.use.nugget = FALSE, se.threshold = 1e-6, attributes = FALSE) {
      self$aei_use_nugget = assert_flag(aei.use.nugget)
      self$se_threshold = assert_number(se.threshold, lower = 1e-20)
      super$initialize(id = "AEI", name = "Augmented Expected Improvement", requirements = "se", opt_direction = "maximize", attributes = attributes)
    },

    # public methods
    prepare = function(opt.state) {
      opt.problem = getOptStateOptProblem(opt.state)
      control = getOptProblemControl(opt.problem)
      design = getOptStateDesigns(opt.state)[[1]]

      self$model = getOptStateModels(opt.state)[[1]]
      self$effective_best_configuration = getEffectiveBestPoint(design = design, model = self$model, par.set = getOptProblemParSet(par.set), control = control)
      self$mult_max_to_min_fun = if (shouldBeMinized(getOptProblemFun(opt.problem))) 1 else -1

      # noise estimation
      if (self$aei_use_nugget) {
        self$pure_noise_var = getLearnerModel(model, TRUE)@covariance@nugget
      } else {
        self$pure_noise_var = estimateResidualVariance(model, data = design, target = control$y.name)
      }
    },

    evaluate_all = function(x) {
      ebs = self$effective_best_configuration
      p = predict(model, newdata = x)$data
      p.mu = self$mult_max_to_min_fun * p$response
      p.se = p$se

      d = ebs$mu - p.mu
      xcr = d / p.se
      xcr.prob = pnorm(xcr)
      xcr.dens = dnorm(xcr)

      tau = sqrt(self$pure_noise_var)
      res = ifelse(p.se < self$se_threshold, 0,
        (d * xcr.prob + p.se * xcr.dens) * (1 - tau / sqrt(tau^2 + p.se^2)))

      data.table(acq = res, se = p$se, mean = p$response, tau = tau)
    }
  )
)
