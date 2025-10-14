include("../src/MoeLia.jl")
using .MoeLia

include("./researcher_lib/researcher_library.jl")

# 0. Definition of constant parameters
const POP_SIZE = 5
const MAX_IT = 150

# 1. Initialize Problem
myproblem = MoeliaProblem.init_problem(2, 3)
MoeliaProblem.set_objectives!(myproblem, ResearcherLibrary.researcher_objective1, ResearcherLibrary.researcher_objective2)
MoeliaProblem.set_boundaries!(myproblem, (-4,4), (-4,4), (-4,4))
MoeliaProblem.set_criteria!(myproblem, ResearcherLibrary.stop_criteria, false, true, MAX_IT)

# 2. Request algorithm
nsga_algorithm = Implementations.get_algorithm("naive_random_nsga2", myproblem, POP_SIZE)
println(MoeliaPipeline.inspect(nsga_algorithm.algorithm_pipeline, Matrix{Float64}))

# 3. Run Algorithm through basic runner
best_population = Runners.basic_runner(nsga_algorithm)
println("Best population from original algorithm:\n$best_population\n------------------------------------\n")

# 4. Modification of the Pipeline
new_pipeline = MoeliaPipeline.clone_pipeline(nsga_algorithm.algorithm_pipeline)
MoeliaPipeline.substitute!(new_pipeline, "crossover", "mod_cross", Functions.Crossovers.single_point, 0.9 )
MoeliaPipeline.substitute!(new_pipeline, 2 , "mod_mut", ResearcherLibrary.unpack_mutator, 0.8)
MoeliaAlgorithm.set_algorithm_pipeline!(nsga_algorithm, new_pipeline)
println(MoeliaPipeline.inspect(nsga_algorithm.algorithm_pipeline, Matrix{Float64}))

# 5. Run Algorithm again through basic runner
best_population = Runners.basic_runner(nsga_algorithm)
println("Best population from modified algorithm:\n$best_population\n------------------------------------\n")