export Mutators
module Mutators
  
  using MoeLia

  """
  Naive mutator operator for NSGA-II algorithm.\n
    REQUIRERS:\n
    \t@arg population::AbstractArray
    \t@arg uniques_set::Set{Vector{Float64}} #Set used to avoid individual duplication, often passed by crossover operator
    \t@arg mutation_rate::Float64
    \t@arg bounds::Vector{Tuple{<: Real, <: Real}} #bounds used to generate mutated chromosomes between a predifined range
    PRODUCES:\n
    \t@arg offspring::Matrix{Float64} 
    \t
  """
  function one_position(
    population::AbstractArray, 
    uniques_set::Set{Vector{Float64}},
    mutation_rate::Float64, 
    bounds::Vector{Tuple{<: Real, <: Real}}
  )::AbstractArray
    # prehemptive controls 
    if isempty(population) return population end
    if mutation_rate < 0 || mutation_rate > 1 throw("invalid mutation rate, it has to be a probability in [0,1]") end
    (pop_size, nvar) = size(population)
    if pop_size <= 0 || nvar <= 0 throw("invalid population") end
    #initialize offsprings
    offspring = Vector{Float64}[]
    for i in 1:pop_size
      if rand()>mutation_rate continue end
      mutated_index = rand(1:nvar)
      lb, hb = bounds[mutated_index]
      #generate a new mutated chromosome to substitute, within predefined bounds  
      mutated_value = lb + rand(Float64) * (hb - lb)
      mutated_chromosome = population[i, :]
      mutated_chromosome[mutated_index] = mutated_value
      #check if the new offspring infividual is not alredy present and eventually add it 
      if !(mutated_chromosome in uniques_set)
        push!(uniques_set, mutated_chromosome)
        push!(offspring, mutated_chromosome)
      end
    end
    #convert the offspring into a Matrix{Float64}
    return convert(Matrix{Float64},hcat(offspring...)')

  end

end