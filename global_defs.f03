MODULE global_defs
    IMPLICIT NONE

    ! Default character length,  real / integer precision 
    integer(8), PARAMETER, PUBLIC:: charlen=1024, dp=8, ip=8
    
    ! I sometimes need work arrays, which I by default declare to this length,
    ! rather than having magic numbers everywhere
    integer(ip), PARAMETER, PUBLIC:: veclen=100

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! PHYSICAL CONSTANTS
    real(dp), PARAMETER, PUBLIC:: gravity=9.8_dp ! m/s**2

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! NUMERICAL CONSTANTS
    ! A missing_value for a real
    real(dp), PARAMETER, PUBLIC:: missing_value=-9.0e+30_dp
   
    ! A small 'almost zero' number 
    real(dp), PARAMETER, PUBLIC:: small_positive_real=1.0e-10_dp

    real(dp), PARAMETER, PUBLIC:: maximum_allowed_timestep=30._dp
    real(dp), PARAMETER, PUBLIC:: minimum_allowed_timestep=1.e-04_dp

    ! Consecutive timesteps are limited in size to max_timestep_increase*(dT_last)
    real(dp), PARAMETER, PUBLIC:: max_timestep_increase=1.30_dp ! dt_next <= dt_last * max_timestep_increase

    real(dp), PARAMETER, PUBLIC:: cfl_1d_solver=1.00_dp
    
    real(dp), PARAMETER, PUBLIC:: wet_dry_depth=1.0e-03_dp ! Used to define 'dry' cells at which we zero discharge / flux
    

    LOGICAL, PARAMETER, PUBLIC:: wet_dry_hacks=.TRUE. ! Flag to turn on/off wet-dry hacks. Useful for testing their effect
    
    real(dp), PARAMETER, PUBLIC:: min_junction_length=50._dp ! FIXME: Used to compute junction stage-volume curve in a hacky way

    ! Max stage on one-d-relations = max xsect_elev + one_d_relation_stage_protection
    real(dp), PARAMETER, PUBLIC:: one_d_relations_stage_protection=1000._dp !

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Simulation duration parameters

    integer, parameter, public:: max_its=10000 ! Number of time-steps
   
    ! Start time for simulation 
    real(dp), PARAMETER, PUBLIC:: start_time=0._dp
    ! Date/Time at which t=0 in the model
    character(len=charlen), PARAMETER, PUBLIC:: model_zero_datetime='Date/Time=25SEP2009,00:00'
   
    ! IO 
    integer, parameter, public:: writfreq=1 ! Write every writfreq'th timestep
    character(len=charlen), PARAMETER, PUBLIC:: output_folder='outputs'
END MODULE global_defs
