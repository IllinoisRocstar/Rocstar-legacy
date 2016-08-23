This folder contains convergence study results for SSS problem.
Elmer is being used as structural sovler and Rocflo is used as the
fluids solver. The problem is solved with three different mesh den-
sities using three different partitioning. The Rocflo probe output
is compared against "GOLD" standard for all cases. The gold standard
is the one provided by Rocfrac/Rocflo solver.
mesh_conv.png : shows different levels of mesh with 4 partitions(cpus)
mesh_i_partition.png (i=1..3) : shows different partitioning of the
                                same mesh.
