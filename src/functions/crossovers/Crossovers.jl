export Crossovers
module Crossovers
  using MoeLia
  using Random, Dates
  
  """
  Naive single point crossover operator for NSGA-II algorithm.\n
    REQUIRERS:\n
    \t@arg population::AbstractArray 
    \t@arg crossover_rate::Float64
    PRODUCES:\n
    \t@arg offspring::Matrix{Float64} 
    \t@arg uniques::Set{Vector{Float64}} # A set of unique individuals in order to not generate duplicates also in other steps\n
    """
  function single_point(population::AbstractArray, crossover_rate::Float64)::Tuple{Matrix{Float64}, Set{Vector{Float64}}}
    if isempty(population) return [] end
    #initialize a seed based on time in order to generate real random population
    Random.seed!(convert(Int64,Dates.datetime2unix(Dates.now())*1000))
    dims = size(population)
    #prehemptive controls
    if crossover_rate < 0. && crossover_rate > 1. throw("invalid crossover rate, it must be a probability in [0,1]") end
    if length(dims) != 2 throw("population must be a 2D array, not well generated") end
    (pop_size, nvar) = dims
    if pop_size <= 0 || nvar <= 0 throw("invalid population") end
    #initialize a uniques Set in order to avoid individual duplication in a long run algorithm 
    uniques = Set{Vector{Float64}}([population[i, :] for i in 1:size(population)[1]])
    offspring = Vector{Float64}[]
    #based on crossover_rate produces offspring by taking a single crossover_point. it will generate 2 offspring individuals 
    for i in 1:2:pop_size
      if rand() > crossover_rate continue end
      #= take two parents to which apply the crossover. the first one will be chosen by iterating over a population through the for loop 
      the second one will be the following individual if the pop_size is not reached, otherwise choose it randomly =# 
      parent1, parent2 = (population[i,:], (i+1 <= pop_size ? population[i+1,:] : population[rand(1:pop_size),:]))
      crossover_point = rand(1:nvar)
      child1, child2 = (
        vcat(parent1[1:crossover_point-1], parent2[crossover_point:nvar]),
        vcat(parent2[1:crossover_point-1], parent1[crossover_point:nvar])
      )
      #check on duplicates and add the new offsprings in the args to return 
      if !(child1 in uniques)
        push!(uniques, child1)
        push!(offspring, child1)
      end
      if !(child2 in uniques)
        push!(uniques, child2)
        push!(offspring, child2)
      end
    end
    #convert the offspring type into Matrix{Float64} if it is not NULL  
    offspring = isempty(offspring) ? Matrix{Float64}(undef, 1, 0) : convert(Matrix{Float64}, vcat(offspring'...))
    return offspring, uniques
  end
  
end