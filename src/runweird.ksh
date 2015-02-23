#!/bin/ksh
export FORT_CONVERT100000=big-endian
synow << EOF 

 &parms
    vphot     =  4000.0
    vmax      = 12000.0
    tbb       = 3700
    ea        =  2900.0
    eb        = 10000.0
    nlam      =  1000
    flambda   =  .true. 
    taumin    =     0.01
    grid      =    32
    zeta      =    1.0
    stspec    = 3300.0
    numref    = 2
    delta_v   = 300.0
    spectrum_file="fort.11"
    debug_out = .true.
    do_locnorm = .false.


    an    =    16,  14,  2,  8, 10, 12, 20, 22, 26, 26, 27, 28,
    ai    =    1,  1,  0,  0,  0,  1,  1,  1,  0,  1,  1,  1,
    tau1  =    2, 1,  1, .1, .8, .5, 15, .2,1e3,1.6,  0,1e3,
    pwrlawin = 6,  6,  0,  0,  0,  0,  0,  8,  0,  0,  8,  8,
    vmine =   1,  0,  7,  0,  0,  0,  0,  3,  0,  0,  0,  3,
    vmaxe =   40, 40, 40, 40,  0,  0, 40, 40,  1, 40, 40, 40,
    ve    =    2,  2,  2,  2,  0,  0,  2,  1,  2,  1,  1,  3
    vmaxg =   12, 12,  7,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    sigma_v =2.0,2.3, 2.3, 0,  0,  0,  0,4.5,  0,  0,  7,4.5,
    temp  =   10, 10, 10, 10,  0,  0, 10,4.5,  5, 10,  7,4.5,
    dprof =  'p','p','g','e','e','e','e','e','e','e','e','e',

/

EOF
