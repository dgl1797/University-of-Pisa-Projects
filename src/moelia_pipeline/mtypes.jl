export MoeliaTypes
"""
  Module containing the struct for the Core Moelia Pipeline Object (MPipe) composed of\n
  **MPipe Struct**\n
  \t@arg mpipe::Vector{Tuple{String, Function, Any}} # vector of the steps composed by name::String; action::Function; action_params::Any
  \t@arg iter::Vector{Iteration} # vector keeping track of the ran iterations (each run_pipeline call will push a new iteration)
  **Iteration Struct**\n
  \t@arg inputs::Vector{MData} # vector of MData inputs for this iteration
  \t@arg outputs::Vector{MData} # vector of MData outputs for this iteration
  **MData{T} Struct**\n
  \t@arg data::T # data of the i-th iteration and j-th step : iter[i].inputs[j] or iter[i].outputs[j] of type T
"""
module MoeliaTypes

    struct MData{T}
      data::T
    end

    struct Iteration
      inputs::Vector{MData}
      outputs::Vector{MData}
    end

    struct MPipe
      mpipe::Vector{Tuple{String, Function, Any}}
      iter::Vector{Iteration}
    end
    
end