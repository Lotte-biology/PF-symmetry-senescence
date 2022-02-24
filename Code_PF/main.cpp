#include <iostream>
#include "Population.h"
#include "chrono"

// This version with damage-specific vibility

using namespace std;

int main()
{
    auto start = std::chrono::high_resolution_clock::now();

    unsigned long seed = randomize();
    cout << seed << "\n";

    Population pop{};
    pop.calc_stats();
    ofstream ef("evol_DEF.txt");
    pop.write_stats(ef,0);

    ofstream df("dead_DEF.txt");
    //pop.write_dead(df,0);

    for (unsigned long t=1; t<=n_timesteps; ++t){
        //cout << i << " ";
        pop.next_timestep();
        if (t % skip == 0){
            pop.calc_stats();
            pop.write_stats(ef,t);
        }
        if (t>=n_timesteps-75){
            pop.write_dead(df,t);
        }
    }

    ofstream pf("pop_DEF.txt");
      pop.write_pop(pf);
    
    // ofstream df("deads.text");
    // pop.write_deads(df);

    auto end = std::chrono::high_resolution_clock::now();
    auto diff = end - start;
    cout << endl;
    cout << " That took " << std::chrono::duration<double>(diff).count() << " seconds" << endl;

    return 0;
}
