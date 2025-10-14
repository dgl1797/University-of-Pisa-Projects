export fnds
"""
  Module containing all functionalities to implement fast-non-dominated-sort of NSGA-II
"""
module fnds

  """
    Fast non dominated sort for NSGA-II algorithm.\n
    **REQUIRES**\n
    \t@arg population::Matrix{Float}
    **PRODUCES**\n
    \t@arg frontiers::Vector{Vector{Int}}
    \t
  """
  function fast_non_dominated_sort(population)
    # Initialize domination count and dominated solutions for each solution
    
    S = [Set() for _ in 1:size(population)[1]] #ordered set containing for each i-th cell the individuals dominated by the i-th one 
    n = zeros(Int, size(population)[1]) #domination count, number of individuals that dominate the i-th one 
    rank = zeros(Int, size(population)[1]) #array in which each i-th cell corresponds to the rank/frontier of i-th ind.

    # Initialize the first front, a front contains all the individuals with the same number of dominant individuals
    F = Vector{Int}[]
    push!(F, Int[])

    # Calculate domination for pairs of solutions
    for p in 1:size(population)[1] #p is the index examinated element 
        for q in 1:size(population)[1] #q is the index of the other compared element 
            if p != q
                # Check if solution p dominates solution q (case true)
                if all(population[p,:] .<= population[q,:]) && any(population[p,:] .< population[q,:])
                    push!(S[p], q)
                elseif all(population[q,:] .<= population[p,:]) && any(population[q,:] .< population[p,:])
                    n[p] += 1
                end
            end
        end
        # If solution p is not dominated, counter == 0, it belongs to the first front
        if n[p] == 0
            rank[p] = 1
            push!(F[1], p)
        end
    end

    # Generate subsequent fronts
    i = 1
    while size(F[i])[1] != 0
        Q = Int[] # current front constructor
        for p in F[i]
            for q in S[p]
              # p is now extracted hence each q in S[p] decrease their dominated count
                n[q] -= 1
                if n[q] == 0
                    rank[q] = i + 1
                    push!(Q, q)
                end
            end
        end
        i += 1
        if isempty(Q) break end # last frontier will be empty and works as stop condition
        push!(F, Q)
    end
    return F
  end
  
  """
    Filters the population based on the received frontiers from `fast_non_dominated_sort` in NSGA-II\n
    **REQUIRES**\n
    \t@arg frontiers::Vector{Vector{Int}} # containing the indexes of population for each frontier
    \t@arg population::Matrix{Float64}
    \t@arg pop_size::Int
    **PRODUCES**\n
    \t@arg filtered_population::Matrix{Float64}
    \t@arg last_frontier::Matrix{Float64} # containing the elements of population for the last frontier to be crowd_sorted
    _produced arguments are returned as a tuple_
  """
  function frontier_filter(frontiers::Vector{Vector{Int}},population::AbstractArray,pop_size::Int)
    next_batch = Vector{Float64}[]
    k = 1
    # LOOP OVER FRONTIERS 
    while k <= size(frontiers)[1]
      #stop condition if the pop_size is reached
      if size(next_batch)[1] + size(frontiers[k])[1] > pop_size break end
      [push!(next_batch, population[cs, :]) for cs in frontiers[k]]
      k += 1
    end
    #convert the next_batch into Matrix{Float64}
    return (
      convert(Matrix{Float64}, hcat(next_batch...)'),
      k <= size(frontiers)[1] ? population[frontiers[k], :] : []
    )
  end
  
  """
    Sorts the remaining frontier's elements to distribute extracted solutions as much as possible in NSGA-II\n
    **REQUIRES**\n
    \t@arg population::Matrix{Float64} # of elements composing last frontier
    \t@arg objectives::Vector{Function} # objective function to evaluate the crowding distance based on fitness
    \t@arg remaining_size::Int = (pop_size - size(filtered_population))
    **PRODUCES**\n
    \t@arg selected_individuals::Matrix{Float64}
    \t
  """
  function crowding_distance(population::AbstractArray, objectives::Vector{Function}, remaining_size::Int)
    if remaining_size == 0 return [] end
    if isempty(population) return population end
    
    # Number of objectives
    M = size(objectives)[1]
    
    # Number of individuals
    N = size(population)[1]
    
    # Initialize crowding distance
    individual = Dict[Dict([("crowd", population[i, :]), ("distance", 0.0)]) for i in 1:N]

    for m in 1:M
        # Sort the population by the m-th objective function
        sort!(individual, by = x -> objectives[m](x["crowd"]))
      
        # Set the boundary points' crowding distance to infinity
        individual[1]["distance"] = Inf
        individual[N]["distance"] = Inf
      
        # For each remaining individual
        for i in 2:(N-1)
            # Update the crowding distance
            individual[i]["distance"] += (objectives[m](individual[i+1]["crowd"]) - objectives[m](individual[i-1]["crowd"]))
        end
    end
    sort!(individual, by = x -> -x["distance"])
    return convert(Matrix{Float64}, hcat([individual[i]["crowd"] for i in 1:remaining_size]...)')
  end

end