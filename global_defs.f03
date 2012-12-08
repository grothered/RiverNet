MODULE global_defs
    IMPLICIT NONE

    ! Default character length,  real / integer precision 
    integer(8), PARAMETER, PUBLIC:: charlen=1024, dp=8, ip=8
    
    ! I sometimes need work arrays, which I by default declare to this length,
    ! rather than having magic numbers everywhere
    integer(ip), PARAMETER, PUBLIC:: veclen=100

    ! A missing_value for a real
    real(dp), PARAMETER, PUBLIC:: missing_value=-9.0e+30_dp
   
    ! A small 'almost zero' number 
    real(dp), PARAMETER, PUBLIC:: small_positive_real=1.0e-10_dp

    ! PHYSICAL CONSTANT
    real(dp), PARAMETER, PUBLIC:: gravity=9.8_dp ! m/s**2

    ! NUMERICAL CONSTANTS
    real(dp), PARAMETER, PUBLIC:: maximum_allowed_timestep=1000._dp

    real(dp), PARAMETER, PUBLIC:: cfl_1d_solver=0.9999_dp
    
    real(dp), PARAMETER, PUBLIC:: wet_dry_depth=1.0e-03

    ! Consecutive timesteps are limited in size to max_timestep_increase*(dT_last)
    !real(dp), PARAMETER, PUBLIC:: max_timestep_increase=2.00_dp

END MODULE global_defs
