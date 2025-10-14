export Algos
"""
Module exporting CORE algorithms using a String to Function mapping 
"""
module Algos

  using MoeLia

  """
  NSGA-II algorithm .\n
    REQUIRERS:\n
    \t@arg problem::MoeliaProblemTypes.MPT
    \t@arg pop_size::Int64
    PRODUCES:\n
    \t@arg algorithm::MoeliaAlgoTypes.MAT
    \t
  """
  function nr_nsga2(problem::MoeliaProblemTypes.MPT, pop_size::Int64)::MoeliaAlgoTypes.MAT
    #initialize the algorithm with an empty pipeline, the input's parameters and a random Populator initializer
    algorithm = MoeliaAlgoTypes.MAT(
      Functions.Populators.random_initializer,
      (problem, pop_size),
      MoeliaTypes.MPipe(Tuple{String, Function, Any}[], MoeliaTypes.Iteration[]),
      problem,
      pop_size
    )
    #initialize the algorithm's pipeline with predefined NSGA-II steps
    algorithm.algorithm_pipeline = MoeliaTypes.MPipe(Tuple{String, Function, Any}[
      ("crossover", Functions.Crossovers.single_point, (0.6,)),
      ("mutation", (p) -> Functions.Mutators.one_position(p[1], p[2], 0.5, problem.bounds), ()),
      #the anonymous function allows to access to the previously steps' inputs/outputs  
      ("concatenatePQ", 
        (p) -> vcat(filter(
          x -> !isempty(x), 
          [p, algorithm.algorithm_pipeline.iter[end].inputs[1].data, algorithm.algorithm_pipeline.iter[end].outputs[1].data[1]]
          )...
        ), 
        ()
      ),
      ("fast_non_dominated_sort", Functions.Auxiliaries.fnds.fast_non_dominated_sort, ()),
      ("filter_frontiers", (f) -> Functions.Auxiliaries.fnds.frontier_filter(f, algorithm.algorithm_pipeline.iter[end].outputs[3].data, pop_size), ()),
      ("crowding_distance", 
        (p) -> Functions.Auxiliaries.fnds.crowding_distance(
          p[2], 
          problem.objective_functions, 
          pop_size-(isempty(algorithm.algorithm_pipeline.iter[end].outputs[5].data[1]) ? 0 : size(algorithm.algorithm_pipeline.iter[end].outputs[5].data[1])[1])
        ),
        ()
      ),
      ("build_final_population", 
        (p) -> vcat(filter(x -> !isempty(x), [ p, algorithm.algorithm_pipeline.iter[end].outputs[5].data[1] ])...),
        ()
      ),
    ], MoeliaTypes.Iteration[])

    return algorithm

  end
  #Dictionary exposed to access in a direct way to the existing algorithm 
  MoeliaImplementations = Dict{String, Function}([
    ("naive_random_nsga2", nr_nsga2)
  ])

end