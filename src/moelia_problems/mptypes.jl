export MoeliaProblemTypes
"""
  Module containing the structs for the Core Moelia Problem Types (MPT) composed of\n
    MPT struct:
    \t@arg objective_functions::Vector{Function} 
    \t@arg bounds::Vector{Tuple{<: Real, <: Real}}
    \t@arg params::MPTParams\n
    MPTParams mutable struct:
    \t@arg nvar::Int64 #number of variables 
    \t@arg criteria::Function #the function must return a Boolean and must take as input population before iteration  
    \t@arg cparams::Any #criteria_params, optional parameters useful for the function
    \t@arg crequires_pop::Bool #criteria_requires_population, Boolean representing if criteria requires population as input 
    \t@arg crequires_it::Bool #criteria_requires_iteration, Boolean representing if criteria requires iteration as input 
    \t
"""
module MoeliaProblemTypes
  
  mutable struct MPTParams
    nvar::Int64
    criteria::Function
    cparams::Any
    crequires_pop::Bool
    crequires_it::Bool
  end

  struct MPT
    objective_functions::Vector{Function} 
    bounds::Vector{Tuple{<: Real, <: Real}}
    params::MPTParams
  end

end