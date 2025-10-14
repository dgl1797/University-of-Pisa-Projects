export Functions 
"""
  Exporter Module for exporting auxiliaries; mutators; crossovers and populators.
"""
module Functions

  export Crossovers
  export Mutators
  export Populators
  export Auxiliaries

  include("crossovers/Crossovers.jl")
  include("mutators/Mutators.jl")
  include("populators/Populators.jl")
  include("auxiliaries/Auxiliaries.jl")

end