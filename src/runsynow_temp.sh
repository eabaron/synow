#!/bin/bash

SYNTH_FILE='temp.dat'

$HOME/synow/src/synow << EOF

 &parms
    synow_lines_path     = '/Volumes/small_backup/baron/synow_lines/',
    kurucz_linelist_path = '/Volumes/small_backup/baron/lines/',
    refdata_path         = '$HOME/synow/src/',
    vphot                = 5000.0,
    vmax                 = 40000.0,
    tbb                  = 6000.0,
    ea                   = 3200.0,
    eb                   = 9000.0,
    nlam                 = 1000,
    flambda              = .true.,
    taumin               = 0.01,
    grid                 = 32,
    zeta                 = 1.0,
    stspec               = 3000.0,
    numref               = 2,
    delta_v              = 300.0,
    spectrum_file        = '$SYNTH_FILE',
    debug_out            = .true.,
    do_locnorm           = .true.,

    an       =                1,                6
    ai       =                0,                1
    tau1     =             20.0,                3
    pwrlawin =              2.0,              2.0
    vmine    =                5,                5
    vmaxe    =               40,               40
    ve       =              3.0,              1.0
    vmaxg    =             12.0,             12.0
    sigma_v  =              2.0,              2.0
    temp     =             10.0,             10.0
    dprof    =              'e',              'e'

/

EOF
