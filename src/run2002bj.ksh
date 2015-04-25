#n!/bin/ksh

synow << EOF 

 &parms
    vphot     = 05000.0
    vmax      = 40000.0
    tbb       = 10000
    ea        = 3500.0
    eb        = 9500.0
    nlam      =  1000
    flambda   =  .true. 
    taumin    =     0.01
    grid      =    30
    zeta      =    1.0
    stspec    = 3500.0
    numref    = 10
    delta_v   = 300.0
    spectrum_file="fit02bjx.dat"
    debug_out = .false.


    an    =    1,  2, 22,  8, 12, 14, 16, 20, 20, 26,
    ai    =    0,  0,  0,  1,  1,  1,  1,  1,  0,  1,
    tau1  =   .8, .3, .1, .2, .2, .4, .2, .6,  0, .2,
    pwrlawin = 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    vmine =   05, 05, 05, 07, 05, 05, 05, 05, 07, 05,
    vmaxe =   40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
    ve    =    2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
    vmaxg =    7,  7,  7,  7,  7,  7,  7,  7,  7,  7,
    sigma_v =  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
    temp  =   10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
    dprof =  'e','e','e','e','e','e','e','e','e','e',

/

:OF
