#!/bin/ksh



VPHOT=8000.0
((VP = $VPHOT/1000))
VMAX=40000.0
((VM = $VMAX/1000))

export VPHOT VP VMAX VM


$HOME/synow/src/synow << EOF 

 &parms
    synow_lines_path = '/net/linmer/myhome2/synow_lines/',
    kurucz_linelist_path = '/net/linmer/myhome2/lines/',
    refdata_path = '$HOME/synow/src/',
    vphot     = ${VPHOT},
    vmax      = ${VMAX},
    tbb       = 12000,
    ea        =  2900.0,
    eb        = 10000.0,
    nlam      =  1000,
    flambda   =  .true.,
    taumin    =     0.01,
    grid      =    32,
    zeta      =    1.0,
    stspec    = 3100.0,
    numref    =  19,
    delta_v   = 300.0,
    spectrum_file="example.dat",
    debug_out = .true.,
    do_locnorm = .true.,


    an    =     14,   14,   14,   16,   20, 20,    26,    26,   26,   12,   12,   12,    8,    28,    28,   27,    6,    22,    11,
    ai    =      1,    1,    2,    1,    1,  1,     1,     1,    2,    0,    1,    1,    0,     1,     1,   1,     1,    1,     0,
    tau1  =      4,    4,    0,    0,    1,  5,     1,     0,    0,    0,    1,    0,    1,     1,     1,   1,     1,    1,     1,
    pwrlawin =   2,    2,    2,    2,    2,  2,     2,     2,    2,    2,    2,    2,    2,     2,     2,   2,     2,    2,     2,
    vmine =     ${VP} 16,  ${VP}, ${VP},${VP},      19,  ${VP}, 20,  ${VP},${VP},${VP}, ${VP}, ${VP},${VP}, 20,  ${VP}, ${VP}, ${VP}, 
    vmaxe =    ${VM},${VM},${VM},  ${VM}, 18,     ${VM},  18,   ${VM},${VM},${VM}, 18, ${VM},  ${VM}, 18, ${VM}, ${VM}, ${VM}, ${VM}, 
    ve    =      1,  1,    1,    1,    1,  1,       1,     1,   1,     1,    1,     1,    1,     1,   1,    1,     1,     1,     1,
    vmaxg =     12, 12,   12,   12,   12, 12,      12,    12,   12,    12,   12,   12,   12,    12,   12,  12,    12,    12,    12,
    sigma_v =  2.0,2.0,  2.0,  2.0,  2.0,2.0,      2.0,  2.0,  2.0,   2.0,  2.0,  2.0,  2.0,   2.0,  2.0,  2.0,   2.0,   2.0,   2.0,
    temp  =     10, 10,   10,   10,   10, 10,      10,    10,   10,   10,   10,    10,   10,   10,    10,  10,    10,    10,    10,
    dprof =    'e','e',  'e',  'e',  'e','e',      'e',   'e',  'e',  'e',  'e',   'e',  'e',  'e',   'e', 'e',  'e',    'e',  'e',

 /

EOF
