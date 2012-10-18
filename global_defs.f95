MODULE global_defs
    IMPLICIT NONE

    ! Parameters relating to real/character/vector sizes, etc 
    integer(8), PARAMETER, PUBLIC:: veclen=100, charlen=100, dp=8, large_array_len=999999
    real(dp), PARAMETER, PUBLIC:: missing_value=-9.0e+30_dp

END MODULE global_defs
