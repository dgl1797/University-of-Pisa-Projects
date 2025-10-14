include("../src/MoeLia.jl")
using .MoeLia

include("./researcher_lib/researcher_library.jl")

# Constants Definition
MAX_IT = 150
POP_SIZE = 3

# Problem initialization
myproblem = MoeliaProblem.init_problem(2, POP_SIZE)
MoeliaProblem.set_objectives!(myproblem, ResearcherLibrary.researcher_objective1, ResearcherLibrary.researcher_objective2)
MoeliaProblem.set_boundaries!(myproblem, (-4,4), (-4,4), (-4,4))
MoeliaProblem.set_criteria!(myproblem, ResearcherLibrary.stop_criteria, false, true, MAX_IT)

# Pipeline construction
population = Functions.Populators.random_initializer(myproblem, 3)
mypipe = MoeliaPipeline.init_pipeline()
MoeliaPipeline.add_step!(mypipe, "crossover", Functions.Crossovers.single_point, 0.9)
MoeliaPipeline.add_step!(mypipe, "mutation", (p) -> Functions.Mutators.one_position(p[1], p[2], 0.7, myproblem.bounds))
MoeliaPipeline.add_step!(mypipe, "concatenatePQ", (p) -> vcat(filter(x -> !isempty(x), [p,mypipe.iter[end].inputs[1].data,mypipe.iter[end].outputs[1].data[1]])...))
MoeliaPipeline.add_step!(mypipe, "FNDS", Functions.Auxiliaries.fnds.fast_non_dominated_sort)
MoeliaPipeline.add_step!(mypipe, "frontier_filter", (p) -> Functions.Auxiliaries.fnds.frontier_filter(p,mypipe.iter[end].outputs[3].data, size(population)[1]))
MoeliaPipeline.add_step!(
  mypipe, 
  "crowding_distance", 
  (p) -> Functions.Auxiliaries.fnds.crowding_distance(
    p[2], 
    myproblem.objective_functions, 
    size(population)[1]-(isempty(mypipe.iter[end].outputs[5].data[1]) ? 0 : size(mypipe.iter[end].outputs[5].data[1])[1])
  )
) 
MoeliaPipeline.add_step!(mypipe, "build_final_population", (p) -> 
  vcat(filter(x -> !isempty(x), [ p, mypipe.iter[end].outputs[5].data[1] ])...)
)

# Pipeline execution and printing
MoeliaPipeline.run_pipeline(mypipe,population)
println("-- pop")
println(population)
println("-- cross")
println(mypipe.iter[end].outputs[1].data)
println("-- mut")
println(mypipe.iter[end].outputs[2].data)
println("-- conc")
println(mypipe.iter[end].outputs[3].data)
println("-- fnds")
println(mypipe.iter[end].outputs[4].data)
println("-- filt")
println(mypipe.iter[end].outputs[5].data)
println("-- crowd")
println(mypipe.iter[end].outputs[6].data)
println("--final_pop")
println(mypipe.iter[end].outputs[7].data)
