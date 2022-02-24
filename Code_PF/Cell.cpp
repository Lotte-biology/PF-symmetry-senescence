#include "Cell.h"

void Cell::mutate(){
    for (size_t i=0; i<2; ++i) // repro & repair loci
        for (size_t j=0; j<2; ++j) if (ru()<p.mu){
            g[i][j] += rnorm(p.sd_mu);
        //  g[i][1]=g[i][0];
        }

    // harvest
    for (size_t j=0; j<2; ++j) if (ru()<p.mu_h){
        g[2][j] += rnorm(p.sd_mu);
        clip0_epsilon(g[2][j]);
      //g[2][1]=g[2][0];
    }

    // damage transmission locus
    for (size_t j=0; j<2; ++j) if (ru()<p.mu_t){
        g[3][j] += rnorm(p.sd_mu);
       //g[3][1]=g[3][0];
    }

    // mutate viability loci
    // biased: mostly 0->1 (as long as pr_back<1)
    for (size_t i=0; i<p.n_v_loci; ++i)
        if (ru()<p.mu_v) {
            if (vg[i]==0) vg[i].flip();
            else if (ru()<p.pr_back) vg[i].flip();
        }

}

