"""
Module exposing functions pretended to be written by the researcher
"""
module ResearcherLibrary

  #example of objective function definition 
  function researcher_objective1(chromosome::Vector{Float64})::Float64
    nvar = length(chromosome)
    return 1-exp(-sum([(x-1/sqrt(nvar))^2 for x in chromosome]))
  end

  function researcher_objective2(chromosome::Vector{Float64})::Float64
    nvar = length(chromosome)
    return 1-exp(-sum([(x+1/sqrt(nvar))^2 for x in chromosome]))
  end
  """
  Return a Boolean to check if the algorithm can continue \n
  _IMPORTANT_
  \t the function is valid for the library because it returns a Boolean and it doesn't require population after iteration 
  """
  function stop_criteria(current_iteration, max_iteration)::Bool
    return current_iteration <= max_iteration
  end
  """
  Example of naive mutator operator written by Researcher with predefined bounds.\n
    REQUIRERS:\n
    \t@arg (population, uniques_set)::Tuple{AbstractArray,Set{Vector{Float64}}}
    \t@arg mutation_rate::Float64
    PRODUCES:\n
    \t@arg offspring::Matrix{Float64} 
    \t
  """
  function unpack_mutator(
    (population, uniques_set)::Tuple{AbstractArray,Set{Vector{Float64}}},
    mutation_rate::Float64
  )::AbstractArray
  #For a better understanding read the one_position function in Functions.Mutators
    bounds = [(-4,4), (-4,4), (-4,4)]
    if isempty(population) return population end
    if mutation_rate < 0 || mutation_rate > 1 throw("invalid mutation rate, it has to be a probability in [0,1]") end
    (pop_size, nvar) = size(population)
    if pop_size <= 0 || nvar <= 0 throw("invalid population") end
    offspring = Vector{Float64}[]
    for i in 1:pop_size
      if rand()>mutation_rate continue end
      mutated_index = rand(1:nvar)
      lb, hb = bounds[mutated_index] 
      mutated_value = lb + rand(Float64) * (hb - lb)
      mutated_chromosome = population[i, :]
      mutated_chromosome[mutated_index] = mutated_value
      if !(mutated_chromosome in uniques_set)
        push!(uniques_set, mutated_chromosome)
        push!(offspring, mutated_chromosome)
      end
    end

    return convert(Matrix{Float64},hcat(offspring...)')

  end
  """
  Example of naive sorting and filtering individuals with priority order written by Researcher with predefined bounds.\n
    REQUIRERS:\n
    \t@arg population::Matrix{Float64}
    \t@arg pop_size::Int
    \t@arg objectives::Vector{Function}
    PRODUCES:\n
    \t@arg population::Matrix{Float64} 
    \t
  """
    function sort_and_filter(population::Matrix{Float64}, pop_size::Int, objectives::Vector{Function})
      Nobj = size(objectives)[1]
      Nvar = size(population)[2]
      #dictionary to store the fitnesses of each individual in the population.
      fitnesses = Dict{Int, Vector{Float64}}()
  
      # For each individual in the population initialize a vector to store its fitness for each objective.
      for i in 1:pop_size
        fitnesses[i] = Vector{Float64}(undef, Nvar)
      end
  
      # For each individual in the population and for each objective compute the fitness of the individual for that objective.
      for i in 1:pop_size
        for j in 1:Nobj
          fitnesses[i][j] = objectives[j](population[i,:])
        end
      end
  
      # Sort the individuals by their fitnesses in descending order.
      sorted_pairs = sort(collect(fitnesses), by = x -> -x[2])
      # Filter the sorted individuals to keep only the top 'pop_size' individuals.
      filtered_sorted_pairs = sorted_pairs[1:pop_size]
  
      # Return the top pop_size individuals.
      return population[[k[1] for k in filtered_sorted_pairs], :]
    end
  
  
end