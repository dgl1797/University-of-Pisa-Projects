export Implementations
"""
Module exporting APIs for algorithm Implementation usage
"""
module Implementations
  
  using MoeLia

  """
  Allow to get in a direct way preexisting algorithms exposed through algos Dictionary .\n
    REQUIRERS:\n
    \t@arg name::String #name of the algorithm to search
    \t@arg problem::MoeliaProblemTypes.MPT #problem to be passed to the algorithm
    \t@arg pop_size::Int64
    PRODUCES:\n
    \t@arg algorithm::MoeliaAlgoTypes.MAT
    \t
  """
  function get_algorithm(name::String, problem::MoeliaProblemTypes.MPT, pop_size::Int64)::MoeliaAlgoTypes.MAT
    #check if the algorithm is undefinded, eventually return that with the related problem and pop_size
    required_implementation = get(Algos.MoeliaImplementations, name, undef)
    return required_implementation == undef ? throw("Algorithm $name not found") : required_implementation(problem, pop_size)
  end

end