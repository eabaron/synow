#!/bin/ksh

synow << EOF 

 &parms
    ! synow_lines_path = '/net/linmer/myhome2/synow_lines/',
    ! kurucz_linelist_path = '/net/linmer/myhome2/lines/',
    synow_lines_path = '/Users/baron/synow/lines/',
    kurucz_linelist_path = '/Volumes/small_backup/baron/lines/',
    vphot     =  8000.0,
    vmax      = 25000.0,
    tbb       = 8000,
    ea        =  2900.0,
    eb        = 22000.0,
    nlam      =  1000,
    flambda   =  .true. ,
    taumin    =     0.01,
    grid      =    32,
    zeta      =    1.0,
    stspec    = 4100.0,
    numref    = 1,
    delta_v   = 300.0,
    spectrum_file="synspec_n12.dat",
    debug_out = .true.,
    do_locnorm = .true.,


    an    =    14,  22,  1,   2,  8, 10, 12, 20,   26, 26, 27, 28,
    ai    =    1,    1,  0,   0,  0,  0,  1,  1,    0,  1,  1,  1,
    tau1  =    2,   .2, .3,   1, .1, .8, .5, 15,   1e3,1.6,  0,1e3,
    pwrlawin = 12,    8,  0,   0,  0,  0,  0,  0,    0,  0,  8,  8,
    vmine =   6,     3,  0,   7,  0,  0,  0,  0,    0,  0,  0,  3,
    vmaxe =   25,    40, 40,  40, 40,  0,  0, 40,    1, 40, 40, 40,
    ve    =    1,    1,  2,   2,  2,  0,  0,  2,    2,  1,  1,  3
    vmaxg =   12,    0, 15,   7,  0,  0,  0,  0,    0,  0,  0,  0,
    sigma_v = 2.0,  4.5, 2.3,  2.3, 0,  0,  0,  0,    0,  0,  7,4.5,
    temp  =   10,   4.5, 10,   10, 10,  0,  0, 10,   5, 10,  7,4.5,
    dprof =  'p',  'e', 'g', 'g','e','e','e','e', 'e','e','e','e',

/

EOF
