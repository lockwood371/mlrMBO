AcquisitionFunction = R6Class("AcquisitionFunction",
  public = list(
    # public members
    id = character(1L),
    name = character(1L),
    requirements = NULL,
    opt_direction = character(1L), #should the acq fun be maximized or minimized or objective if the direction is the same as the direction of the objective function
    attributes = logical(1L),

    # constructor
    initialize = function(id, name, requirements, opt_direction, attributes = FALSE) {
      self$id = assert_string(id)
      self$character = assert_string(name)
      self$requirements = assert_character(requirements)
      self$opt_direction = assert_choice(opt_direction, c("minimize", "maximize", "objective"))
      self$attributes = assert_flag(attributes)
    },

    # public methods

    # prepare the AcquisitionFunction for the actual optimization stage
    prepare = function(opt.state) {
      stop("not implemented")
    },

    # returns all Acquisition Function Values
    # x - data.frame
    # res - data.frame with at least $acq column
    evaluate_all = function(x) {
      stop("not implemented")
    },

    # x - see evaluate_all
    # res - numeric(nrow(x))
    evaluate = function(x, attributes = self$attributes) {
      res_all = self$evaluate_all(x)
      res = res_all$acq
      if (attributes) {
        res_all$acq = NULL
        res = setAttribute(res, "crit.components", res_all)
      }
      return(res)
    },

    # see evaluate
    # res - converted to min problem
    evaluate_as_minimization = function(x) {
      self$mult_inv_max * self$evaluate(x)
    }
  ),

  # active bindings
  active = list(
    # multiplier to invert maximization to minimization
    mult_inv_max = function() if (self$opt_direction == "minimize") 1 else -1
  )
)
