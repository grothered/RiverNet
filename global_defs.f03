MODULE global_defs
    IMPLICIT NONE

    ! Default character length,  real / integer precision 
    integer(8), PARAMETER, PUBLIC:: charlen=1024, dp=8, ip=8

    ! A missing_value for a real
    real(dp), PARAMETER, PUBLIC:: missing_value=-9.0e+30_dp

    ! I sometimes need work arrays, which I by default declare to this length,
    ! rather than having magic numbers everywhere
    integer(ip), PARAMETER, PUBLIC:: veclen=100

END MODULE global_defs
