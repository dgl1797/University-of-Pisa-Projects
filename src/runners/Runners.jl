export Runners
"""
  Module implementing the main loops executing and handling iterations of Moelia Pipelines
"""
module Runners

  using MoeLia

  """
    Initializes the population exploiting algorithm.population_initializer(algorithm.initializer_parameters...) and
    runs the loop while MoeliaAlgorithm.validate_criteria(population, current_iteration, problem) is true\n
    **REQUIRES**\n
    \t@arg algorithm::MoeliaAlgoTypes.MAT
    **PRODUCES**\n
    \t@arg final_population::Matrix{Float64}
    \t
  """
  function basic_runner(algorithm::MoeliaAlgoTypes.MAT)::AbstractArray
    population = algorithm.population_initializer(algorithm.initializer_parameters...)
    current_iteration = 1
    while MoeliaAlgorithm.validate_criteria(population, current_iteration, algorithm.problem)
      population = MoeliaPipeline.run_pipeline(algorithm.algorithm_pipeline, population)
      current_iteration+=1
    end
    return population
  end

  """
    Initializes the population exploiting algorithm.population_initializer(algorithm.initializer_parameters...) and
    runs the loop while MoeliaAlgorithm.validate_criteria(population, current_iteration, problem) is true, printing 
    the final population once every `every` executions\n
    **REQUIRES**\n
    \t@arg algorithm::MoeliaAlgoTypes.MAT
    \t@arg every::Int @default(1) # printing frequency
    **PRODUCES**\n
    \t@arg final_population::Matrix{Float64}
    \t
  """
  function verbose_runner(algorithm::MoeliaAlgoTypes.MAT, every::Int=1)::AbstractArray
    population = algorithm.population_initializer(algorithm.initializer_parameters...)
    current_iteration = 1
    while MoeliaAlgorithm.validate_criteria(population, current_iteration, algorithm.problem)
      if (current_iteration % every == 0 || current_iteration == 1)
        println("[STEP $current_iteration]:\n-----INPUT-----\n$population\n-----OUTPUT-----\n")
      end
      population = MoeliaPipeline.run_pipeline(algorithm.algorithm_pipeline, population)
      if (current_iteration % every == 0 || current_iteration == 1)
        println(population)
      end
      current_iteration+=1
    end
    return population
  end


end