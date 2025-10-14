export Populators
"""
 Module containing functions used for generating a population
"""
module Populators

  using MoeLia
  
  """
    Naive random initializer that takes bounds of a problem and generates a random population within those bounds\n
    **REQUIRES**\n
    \t@arg problem::MoeLia.MoeliaProblemTypes.MPT
    \t@arg pop_size::Int
    **PRODUCES**\n
    \t@arg population::Matrix{Float64}
    \t
  """
  function random_initializer(problem::MoeLia.MoeliaProblemTypes.MPT, pop_size::Int64)::AbstractArray
    lower_bounds = [b[1] for b in problem.bounds]
    upper_bounds = [b[2] for b in problem.bounds]
    pop = [lower_bounds .+ rand(Float64, problem.params.nvar) .* (upper_bounds .- lower_bounds) for _ in 1:pop_size]
    return vcat(pop'...)
end

  
end