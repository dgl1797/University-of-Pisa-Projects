export MoeliaAlgorithm
"""
  Module exporting APIs for MAT struct
"""
module MoeliaAlgorithm

  using MoeLia 

  """
    API to validate problem's critera defined in `MoeliaProblemTypes.MPT` object\n
    **IMPORTANT:**\n
    to correctly exploit validate_criteria your criteria function should always require population before 
    iteration\n
    **EXAMPLE OF CRITERIA FUNCTIONS**\n
    \tfunction valid_criteria1(population::AbstractArray, iteration::Int, other_params...)
    \t\t # code returning a boolean
    \tend # valid because population is requested before iteration and because returns a boolean
    \t
    \tfunction valid_criteria2(iteration::Int, other_params...)
    \t\t # code returning a boolean
    \tend # valid: population is not required but iteration is and the function returns a boolean
    \t
    \tfunction valid_criteria3(population::AbstractArray, other_params...)
    \t\t # code returning a boolean
    \tend # valid: iteration is not required but population is and the function returns a boolean
    \t
    \tfunction invalid_critera1(iteration::Int, population::AbstractArray, other_params...)
    \t\t # code returning a boolean
    \tend # invalid because population must be placed before iteration
    \t
    \tfunction invalid_criteria2(population::AbstractArray, other_params...)
    \t\t # code not returning a boolean
    \tend # invalid because the function must return a boolean
    **REQUIRES**\n
    \t@arg population::Matrix{Float64}
    \t@arg iteration::Int # counter of the current iteration for iteration-based stop criteria
    \t@arg problem::MoeliaProblemTypes.MPT
    **PRODUCES**\n
    \t@arg is_valid::Bool # whether the condition is valid or not
    \t
  """
  function validate_criteria(population::AbstractArray, iteration::Int, problem::MoeliaProblemTypes.MPT)::Bool
    if problem.params.crequires_it
      return (
        problem.params.crequires_pop 
          ? problem.params.criteria(population, iteration, problem.params.cparams...)
          : problem.params.criteria(iteration, problem.params.cparams...)
      )
    else
      return (
        problem.params.crequires_pop
          ? problem.params.criteria(population, problem.params.cparams...)
          : problem.params.criteria(problem.params.cparams...)
      )
    end
  end

  """
    Setter for populator function of `MoeliaAlgoTypes.MAT` object\n
    **REQUIRES**\n
    \t@arg algo::MoeliaAlgoTypes.MAT # to be modified in-place
    \t@arg populator_function::Function
    \t@arg populator_parameters...
    \t
  """
  function set_populator!(algo::MoeliaAlgoTypes.MAT, populator_function::Function, populator_parameters...)
    algo.population_initializer = populator_function
    algo.initializer_parameters = populator_parameters
  end

  """
    Setter for population size of `MoeliaAlgoTypes.MAT` object\n
    **REQUIRES**\n
    \t@arg algo::MoeliaAlgoTypes.MAT # to be modified in-place
    \t@arg new_size::Int
    **THROWS**\n
    \t@error "invalid population size, must contain at least 2 elements"
    \t
  """
  function set_population_size!(algo::MoeliaAlgoTypes.MAT, new_size::Int)
    if new_size < 2 throw("invalid population size, must contain at least 2 elements") end
    algo.pop_size = new_size
  end

  """
    Setter for algorithm's pipeline of `MoeliaAlgoTypes.MAT` object\n
    **REQUIRES**\n
    \t@arg algo::MoeliaAlgoTypes.MAT # to be modified in-place
    \t@arg pipe::MoeliaTypes.MPipe
    **THROWS**\n
    \t@warning "the pipeline you set is empty" # when passed pipeline has no steps
    \t
  """
  function set_algorithm_pipeline!(algo::MoeliaAlgoTypes.MAT, pipe::MoeliaTypes.MPipe)
    if isempty(pipe.mpipe) println("\033[0;33mWarning: the pipeline you set is empty") end
    algo.algorithm_pipeline = pipe
  end

  """
    Generator function that instantiates a new `MoeliaAlgoTypes.MAT` object\n
    **REQUIRES**\n
    \t@arg problem::MoeliaProblemTypes.MPT
    \t@arg pop_size::Int
    **PRODUCES**\n
    \t@arg algorithm::MoeliaAlgoTypes.MAT
    \t
  """
  function create_algorithm(problem::MoeliaProblemTypes.MPT, pop_size::Int)::MoeliaAlgoTypes.MAT
    return MoeliaAlgoTypes.MAT(
      () -> undef,
      (),
      MoeliaTypes.MPipe(Tuple{String, Function, Any}[], MoeliaTypes.Iteration[]),
      problem,
      pop_size
    )
  end
   
end