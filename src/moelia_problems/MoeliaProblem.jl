export MoeliaProblem
"""
  Module exporting APIs for MPT (MoeliaProblemTypes) object
"""
module MoeliaProblem
  using MoeLia

  """
  Initialize a MoeliaProblem completly customizable with all predefined params\n
    REQUIRERS:\n
    \t@arg NOBJ::Int64 #number of objective_functions
    \t@arg NVAR::Int64 #number of variables 
    PRODUCES:\n
    \t@arg problem::MoeliaProblemTypes.MPT
    \t
  """
  function init_problem(NOBJ::Int64, NVAR::Int64)::MoeliaProblemTypes.MPT
    return MoeliaProblemTypes.MPT(
      Vector{Function}(undef, NOBJ),
      Vector{Tuple{<: Real,<: Real}}(undef, NVAR),
      MoeliaProblemTypes.MPTParams(NVAR, ()->false, (), false, false)
    )
  end
  """
  Customize the problem's objective functions\n
    REQUIRERS:\n
    \t@arg problem::MoeliaProblemTypes.MPT #problem to which set new objective functions
    \t@arg objective_functions::Tuple{Function}
    \t
  """
  function set_objectives!(problem::MoeliaProblemTypes.MPT, objective_functions::Function...)
    for (i, f) in enumerate(objective_functions)
      problem.objective_functions[i] = f
    end
  end
  """
  Customize the problem's boundaries\n
    REQUIRERS:\n
    \t@arg problem::MoeliaProblemTypes.MPT #problem to which set new objective functions
    \t@arg boundaries::Tuple{<: Real,<: Real}
    \t
  """
  function set_boundaries!(problem::MoeliaProblemTypes.MPT, boundaries::Tuple{<: Real,<: Real}...)
    for (i, b) in enumerate(boundaries)
      problem.bounds[i] = b
    end
    problem.params.nvar = length(problem.bounds)
  end
  """
  Customize the problem's criteria to define the stop conditions\n
    REQUIRERS:\n
    \t@arg problem::MoeliaProblemTypes.MPT #problem to which set new objective functions
    \t@arg new_validator::Function #function to use for criteria validation, it must return a Boolean 
    \t@arg with_population::Bool=false #if criteria requires population as input   
    \t@arg with_iterations::Bool=true #if criteria requires iteration as input 
    \t@arg new_params::Tuple{Any} #optional parameters
    THROWS:\n
    \t@error "Criteria validator must be a function returning a boolean and a boolean only"
    \t
  """
  function set_criteria!(
    problem::MoeliaProblemTypes.MPT, 
    new_validator::Function, 
    with_population::Bool=false,
    with_iterations::Bool=true,
    new_params...
  )
    
    if Base.return_types(new_validator)[1] != Bool
      throw("Criteria validator must be a function returning a boolean and a boolean only")
    end

    problem.params.criteria = new_validator
    problem.params.crequires_pop = with_population
    problem.params.crequires_it = with_iterations
    problem.params.cparams = new_params
  end
end