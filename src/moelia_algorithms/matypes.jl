export MoeliaAlgoTypes
"""
  Module containing the struct for the Core Moelia Algorithm Object (MAT) composed of\n
    \t@arg population_initializer::Function
    \t@arg initializer_parameters::Any # Tuple containing all the required parameters for population_initializer to work
    \t@arg algorithm_pipeline::MoeliaTypes.MPipe # the pipeline representing the algorithm's implementation
    \t@arg problem::MoeliaProblemTypes.MPT # the problem to be solved by the algorithm
    \t@arg pop_size::Int # the target population size for the algorithm
"""
module MoeliaAlgoTypes

    using MoeLia

    mutable struct MAT
      population_initializer::Function
      initializer_parameters::Any
      algorithm_pipeline::MoeliaTypes.MPipe
      problem::MoeliaProblemTypes.MPT
      pop_size::Int
    end
end