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

# 2. Create algorithm
myalgo = MoeliaAlgorithm.create_algorithm(myproblem, POP_SIZE)
MoeliaAlgorithm.set_populator!(myalgo, Functions.Populators.random_initializer, myproblem, POP_SIZE)

# 3. Set Algorithm Pipeline
MoeliaPipeline.add_step!(myalgo.algorithm_pipeline, "crossover", Functions.Crossovers.single_point, 0.8)
MoeliaPipeline.add_step!(myalgo.algorithm_pipeline, "mutation", (p) -> Functions.Mutators.one_position(p[1], p[2], 0.7, myproblem.bounds))
MoeliaPipeline.add_step!(
  myalgo.algorithm_pipeline, 
  "concatenate", 
  (p) -> vcat(filter(x -> !isempty(x), [
    p, 
    # Always reference to algorithm's pipeline
    myalgo.algorithm_pipeline.iter[end].inputs[1].data,
    myalgo.algorithm_pipeline.iter[end].outputs[1].data[1]
  ])...)
)
MoeliaPipeline.add_step!(myalgo.algorithm_pipeline, "sort and filter", ResearcherLibrary.sort_and_filter, POP_SIZE, myproblem.objective_functions)
println(MoeliaPipeline.inspect(myalgo.algorithm_pipeline, Matrix{Float64}))

# 4. Run Algorithm through basic runner
final_population = Runners.basic_runner(myalgo)
println("Best population from original algorithm:\n$final_population\n------------------------------------\n")

# 5. Modification of the Pipeline
new_pipeline = MoeliaPipeline.clone_pipeline(myalgo.algorithm_pipeline)
MoeliaPipeline.substitute!(new_pipeline, "crossover", "mod_cross", Functions.Crossovers.single_point, 0.9 )
MoeliaPipeline.substitute!(new_pipeline, 2 , "mod_mut", ResearcherLibrary.unpack_mutator, 0.8)
MoeliaPipeline.add_step!(new_pipeline, 3 , "new_step", 
(p) ->
  begin
    println(p)
    p
  end
)
MoeliaPipeline.delete_step!(new_pipeline, "new_step")
MoeliaAlgorithm.set_algorithm_pipeline!(myalgo, new_pipeline)
println(MoeliaPipeline.inspect(myalgo.algorithm_pipeline, Matrix{Float64}))

# 6. Run Algorithm again through basic runner
final_population = Runners.basic_runner(myalgo)
println("Best population from original algorithm:\n$final_population\n------------------------------------\n")