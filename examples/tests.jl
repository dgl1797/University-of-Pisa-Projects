function researcher_objective1(chromosome::Vector{Float64})::Float64
  nvar = length(chromosome)
  return 1-exp(-sum([(x-1/sqrt(nvar))^2 for x in chromosome]))
end

function researcher_objective2(chromosome::Vector{Float64})::Float64
  nvar = length(chromosome)
  return 1-exp(-sum([(x+1/sqrt(nvar))^2 for x in chromosome]))
end

function sort_and_filter(population::Matrix{Float64}, pop_size::Int, objectives::Vector{Function})
  Nobj = size(objectives)[1]
  Nvar = size(population)[2]
  fitnesses = Dict{Int, Vector{Float64}}()

  for i in 1:pop_size
    fitnesses[i] = Vector{Float64}(undef, Nvar)
  end

  for i in 1:pop_size
    for j in 1:Nobj
      fitnesses[i][j] = objectives[j](population[i, :])
    end
  end

  sorted_pairs = sort(collect(fitnesses), by = x -> -x[2])
  filtered_sorted_pairs = sorted_pairs[1:pop_size]

  return population[[k[1] for k in filtered_sorted_pairs], :]
end

sort_and_filter(rand(8,3), 3, [researcher_objective1, researcher_objective2])