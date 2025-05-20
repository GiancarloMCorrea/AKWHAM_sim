estimate_vb_growth <- function(age, length) {
  library(TMB)
  
  # Create a TMB data and parameters template
  tmb_data <- list(
    age = age,
    length = length,
    N = length(age)
  )
  
  tmb_parameters <- list(
    Linf = mean(length), # As a start value, mean of lengths
    K = 0.2,             Initial guess for K
    t0 = 0               # Initial guess for t0
  )
  
  # Vonalanffy growth model in C++ (TMB uses C++ code for fast computation)
  vb_growth_cpp <-  #include <MB.hpp>
    template<class Type>
    Type objective_function<Type>::operator() () {
      DATA_VECTOR(age);
      DATA_VECTOR(length);
      PARAMETER(Linf);
      PARAMETER(K);
      PARAMETER(t0);
      
      int N = age.size();
      Type nll = 0; // Negative log-likelihood
      
      for (int i = 0; i < N; i++) {
        Type expected_length = Linf * (1 - exp(-K * (age[i] - t0)));
        nll -= dnorm(length[i], expected_length, Type(1), true);
      }
      return nll;
    }
  "
  
  # Write the C++ code to a temporary file
  model_file <- tempfile(pattern = "vb_growth", file = ".cpp")
  writeLines(vb_growth_cpp, con = model_file)
  
  # Compile the model
  compile(model_file)
  dyn.load(dynlib(gsub("\\.cpp$", "", model_file)))
  
  # Create an objective function for optimization
  obj <- MakeADFun = tmb_data, parameters = tmb_parameters, DLL = gsub("\\.cpp$", "", model_file))
  
  # Optimize the negative log-likelihood
  opt <- nlminb(start = obj$par, obj = obj$fn, gr = obj$gr)
  
  # Extract parameter estimates
  estimates <- opt$par
  
  # Return the estimated parameters as a named vector
  return(estimates)
}

