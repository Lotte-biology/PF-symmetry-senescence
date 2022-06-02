# PF_symmetry_senescence
Paper responding to Pen and Flatt (2021)

Code in Code_PF is taken directly from Pen, I., & Flatt, T. (2021). Asymmetry, division of labour and the evolution of ageing in multicellular organisms. Philosophical Transactions of the Royal Society B, 376(1823), 20190729.

Code in PF_symmetrized is a slightly adjust version of the same code such that reproduction is now symmetrical. Of course there are multiple ways to ensure symmetry and to decide when the adult individual reproduces in the symmetrical model. Here we chose a version where reproduction happens if both single cells inside the adult "decide" to reproduce (if (mom[0].reproduces() && mom[1].reproduces()) ). Of course other versions are possible, e.g. using OR instead of AND increases the reproductive rate. Alternatively, it is possible to write the code such that one adult individual can create two offspring individuals within one time step, making the reproductive rate more similar to the original PF (for the same parameters). 

Simulation data can be found at https://figshare.com/articles/dataset/Simulation_data_and_code/19232535

