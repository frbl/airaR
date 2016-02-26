#' Facade for the vars package.
#'
#' @field bootstrap_iterations the number of bootstrap iterations to do for determining the significance of the effects
#' @field horizon the number of steps to look in the future
#' @field var_model the var model to perform the calculations on
#' @field orthogonalize use orthogonalized IRF
#' @importFrom vars VAR irf Bcoef Phi
#' @importFrom methods setRefClass
#' @export VarsFunctions
#' @exportClass VarsFunctions
VarsFunctions <- setRefClass('VarsFunctions',
  fields = c(
    "bootstrap_iterations",
    "horizon",
    "var_model",
    "orthogonalize"
  ),
  methods = list(
    initialize = function(bootstrap_iterations, horizon, var_model, orthogonalize, reverse_order) {
      # In order to allow for a reverse ordering in variables, we have to override a function in the vars package.
      # This is done here. Note the fact that the reverse order var is stored in a global variable. This is due to
      # The fact that we are not calling the function directly, and thus cannot pass an argument to it.
      assign('reverse_order', reverse_order, envir= .GlobalEnv)
      override_function("Psi.varest","vars",myPsi.varest)

      callSuper(bootstrap_iterations= bootstrap_iterations, horizon = horizon,
                var_model = var_model, orthogonalize = orthogonalize)

    },
    bootstrapped_irf = function(from, to) {
        vars::irf(var_model, impulse=from,
                 response = to, n.ahead = horizon, cumulative= FALSE,
                 boot=TRUE, ci=.95, runs = bootstrap_iterations, ortho = orthogonalize)
    },
    irf = function(from, to) {
        vars::irf(var_model, impulse=from,
                  response = to, n.ahead = horizon,
                  cumulative= FALSE, ortho = orthogonalize, boot= FALSE)
    }
  )
)
