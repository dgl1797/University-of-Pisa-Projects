export MoeliaPipeline
"""
  Module exporting APIs for MPipe object
"""
module MoeliaPipeline

  using MoeLia
  
  """
    Generator function that instantiates a new `MoeliaTypes.MPipe` object\n
    **PRODUCES**\n
    \t@arg pipeline::MoeliaTypes.MPipe # empty pipeline
    \t
  """
  function init_pipeline()::MoeliaTypes.MPipe
    return MoeliaTypes.MPipe(
      Tuple{String, Function, Any}[],
      MoeliaTypes.Iteration[]    
    )
  end

  """
    **LOL Non so che faccia sta roba**
  """
  function is_empty(pipe::MoeliaTypes.MPipe)::Bool
    return isempty(pipe.mpipe)
  end

  """
    Pushes a new action to be performed in the pipeline. Actions are executed in the same order they are inserted
    and their output type needs to be coherent with the next step's input type\n
    **REQUIRES**\n
    \t@arg pipe::MoeliaTypes.MPipe # the pipeline where to push the new step
    \t@optionalArg index::Int # position where to add the new step
    \t@arg name::String # the name that identifies the step
    \t@arg action::Function # the action to be executed by the step
    \t@arg action_args::Tuple{Any...} # configuration parameters for the action
    **EXAMPLE OF USAGE**\n
    \tmypipe = MoeliaPipeline.init_pipeline()
    \tMoeliaPipeline.add_step!(mypipe, "step1", first_action, config_param1, config_param2, ...)
    Notice that first_action is any functionality that takes a single input + some configuration parameters and returns a
    single output which is the elaborated input. Alternatively anonymous functions can be used to access inputs/outputs
    of other steps' in the pipeline\n
    \tmypipe = MoeliaPipeline.init_pipeline()
    \tMoeliaPipeline.add_step!(mypipe, "step1", first_action, config_param1, config_param2, ...)
    \tMoeliaPipeline.add_step!(mypipe, "step2", (population) -> population .* mypipe.iter[end].inputs[1].data)
    In this case `iter[end]` represents the data collected during the last iteration of the run while `inputs[1].data` is the
    input of the first step, same can be done with `mypipe.iter[end].outputs[1].data`\n
  """
  function add_step!(pipe::MoeliaTypes.MPipe, name::String, action::Function, action_args...)
    push!(pipe.mpipe, (name, action, action_args))
  end

  function add_step!(pipe::MoeliaTypes.MPipe, index::Int, name::String, action::Function, action_args...)
    if index < 0 || index > size(pipe.mpipe)[1] throw("index not valid") end 
    insert!(pipe.mpipe, index, (name, action, action_args))
  end

  """
    Deletes a step from a pipeline\n
    **REQUIRES**\n
    \t@arg pipe::MoeliaTypes.MPipe # modified in-place
    \t@arg index::Union{Int, String} # identifier of the step (either its name or its index)
    **THROWS**\n
    \t@error "index not valid" # when index is too high or too small
    \t
  """
  function delete_step!(pipe::MoeliaTypes.MPipe, index::Union{Int,String})
    if typeof(index) == Int && (index < 1 || index > size(pipe.mpipe)[1]) throw("index not valid") end
    if typeof(index) == String 
      index = (
        index == "end" 
          ? size(pipe.mpipe)[1] 
          : (index == "start" ? 1 : findfirst(x -> x[1] == index, pipe.mpipe))
      ) 
    end
    deleteat!(pipe.mpipe,index) 
  end

  """
  Views the entire pipeline of actions, giving the ordering and the naming, so that new elements can be accessed/substituted
  signularly using either the index or the name of the step. It also inspects the types to each action, if it is not specified
  in the action's declaration it will assume the action will not modify the type of the input\n
  **REQUIRES**\n
  \t@arg pipe::MoeliaTypes.MPipe # the pipeline to inspect
  \t@arg InputType::DataType # expected input type of the pipeline
  **PRODUCES**\n
  \t@arg inspection_string::String # string representation of the pipeline in the format: [index -> name]: action(arguments type)::ActionReturnType\n
  **EXAMPLE OF USAGE**\n
  \tmypipe = MoeliaPipeline.init_pipeline()
  \tMoeliaPipeline.add_step!(mypipeline, "mutate", Mutators.classic_mutator)
  \tMoeliaPipeline.add_step!(mypipeline, "unite", x -> vcat(mypipeline.iter[end].inputs[1].data, x))
  \tMoeliaPipeline.add_step!(mypipeline, "core", ResearcherLibrary.my_genetic_algorithm, ResearcherLibrary.myobjective)
  \tMoeliaPipeline.add_step!(mypipeline, "adjustments", (x,param) -> x .+ (1*param), 12)
  \tMoeliaPipeline.inspect(mypipeline)\n
  **RESULT WITH PRINTLN**\n
  \t[1 -> mutate]: classic_mutator(::Vector{Float64} + [])::Vector{Float64}
  \t[2 -> unite]: Anonymous Function(::Vector{Float64} + [])::Vector{Float64}
  \t[3 -> core]: my_genetic_algorithm(::Vector{Float64} + [Function])::Array{Float64, 5}
  \t[4 -> adjustments]: Anonymous Function(::Array{Float64, 5} + [Int64])::Array{Float64, 5}\n
  The first argument type in each action represents the streamed input type of the pipeline assuming the inital type is 
  InputType\n
  """
  function inspect(pipe::MoeliaTypes.MPipe, InputType::DataType)::String
    finalString::String = ""
    PreviousType = Type[InputType]
    for (index, (name, action, args)) in enumerate(pipe.mpipe)
      action_name = occursin("#", string(action)) ? "Anonymous Function" : string(action)
      arg_types = "$(join(map(x -> isa(x, Function) ? "Function" : typeof(x), args), ", "))"
      ftype = Base.return_types(action)
      fstring = ftype[1] == Any ? PreviousType[1] : ftype[1]
      finalString *= "[$index -> $name]: $action_name(\u001b[32m$(PreviousType[1])\u001b[0m + [$arg_types])::$fstring\n"
      push!(PreviousType, fstring)
      popfirst!(PreviousType)
    end
    return finalString
  end

  """
    Allows to substitute a specific step of a pipeline, suggested for small changes in existing one.\n
    **REQUIRES**\n
    \t@arg pipe::MoeliaTypes.MPipe # The pipeline to update in-place
    \t@arg step_to_susbsitute::Union{Int, String} # Allow to search the action through the name or the related index
    \t@arg new_name::String # Expected name for the new action
    \t@arg new_action::Function # Function related to the new step, it can be also anonymous 
    \t@arg params::Any # Optional parameters of the new action\n
    **EXAMPLE OF USAGE**\n
    \tMoeliaPipeline.add_step!(mypipeline, "core", ResearcherLibrary.my_genetic_algorithm, ResearcherLibrary.myobjective)
    \tMoeliaPipeline.add_step!(mypipeline, "adjustments", (x,param) -> x .+ (1*param), 12)
    \tMoeliaPipeline.substitute!(mypipeline,"adjustments","new_adjustments",(x,param,param2) -> x .+ (1*param-param2),12,0.5)\n
    Note that consistency between inputs and outputs between steps must be maintained, to see the specific pipeline's 
    action see inspect function. Often used with clone_pipeline to allow creation of different versions of a pipeline
    \t
  """
  function substitute!(
    pipe::MoeliaTypes.MPipe,
    step_to_susbsitute::Union{Int, String},
    new_name::String,
    new_action::Function,
    params...
  )
    if isa(step_to_susbsitute, Int8) || isa(step_to_susbsitute, Int32) || isa(step_to_susbsitute, Int64)
      pipe.mpipe[step_to_susbsitute]  = (new_name,new_action,params)
    else
      index = findfirst(x -> x[1] == step_to_susbsitute, pipe.mpipe)
      pipe.mpipe[index] = index !== nothing ? (new_name,new_action,params) : throw("no action matched")
    end
  end

  """
    Allows to generate a deep clone of a pipeline for which is possible to substitute some steps to have two different pipes
    of execution.\n
    **REQUIRES**\n
    \t@arg pipe::MoeliaTypes.Mpipe
    **PRODUCES**\n
    \t@arg new_pipeline::MoeliaTypes.Mpipe
    **EXAMPLE OF USAGE**\n
    \tMoeliaPipeline.add_step!(mypipeline, "core", ResearcherLibrary.my_genetic_algorithm, ResearcherLibrary.myobjective)
    \tMoeliaPipeline.add_step!(mypipeline, "adjustments", (x,param) -> x .+ (1*param), 12)
    \tcloned_pipeline = MoeliaPipeline.clone_pipeline(mypipeline)
    \tMoeliaPipeline.substitute!(cloned_pipeline,"adjustments","new_adjustments",(x,param,param2) -> x .+ (1*param-param2),12,0.5)
    **INSPECT RESULT**\n
    \t\t[1 -> core]: my_genetic_algorithm(::Vector{Float64} + [Function])::Array{Float64, 5}
    \t\t[2 -> adjustments]: Anonymous Function(::Array{Float64, 5} + [Int64])::Array{Float64, 5}
    \t cloned_pipeline:
    \t\t[1 -> core]: my_genetic_algorithm(::Vector{Float64} + [Function])::Array{Float64, 5}
    \t\t[2 -> new_adjustments]: Anonymous Function(::Array{Float64, 5} + [Int64, Float64])::Array{Float64, 5}
    \t
  """
  function clone_pipeline(pipe::MoeliaTypes.MPipe)::MoeliaTypes.MPipe
    newpipe = MoeliaTypes.MPipe(
      Vector{Tuple{String, Function, Any}}[], 
      MoeliaTypes.Iteration[]
    )
    if is_empty(pipe) return newpipe end
    for (Name, Action, Params) in pipe.mpipe
      push!(newpipe.mpipe, (Name, Action, Params))
    end
    return newpipe
  end

  """
    Runs all steps of a configured pipeline once\n
    **REQUIRES**\n
    \t@arg pipeline::MoeliaTypes.MPipe
    \t@arg startingInputs::MoeliaTypes.MInputs # inputs that will be forwarded through the pipeline
    **PRODUCES**\n
    \t@arg population::Matrix{Float64} # the most fitting population
    \t
  """
  function run_pipeline(pipe::MoeliaTypes.MPipe, x::Any)::AbstractArray
    push!(pipe.iter, MoeliaTypes.Iteration(MoeliaTypes.MData[],MoeliaTypes.MData[]))
    for (_, action, args) in pipe.mpipe
      push!(pipe.iter[end].inputs, MoeliaTypes.MData{typeof(x)}(x))
      x = action(x, args...)
      push!(pipe.iter[end].outputs, MoeliaTypes.MData{typeof(x)}(x))
    end
    return x
  end
end