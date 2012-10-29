MODULE network_solver
    ! Routines to solve the St-Venant Equations with Junctions in a river network
    USE global_defs
    USE river_classes

    contains

    SUBROUTINE evolve_hydraulics(network)
        ! Evolve the hydraulics in the network
        TYPE(network_data_type), INTENT(IN OUT):: network

        ! Update the timestep dT
        CALL update_timestep(network)

        ! Update the flow Stage, Area + Discharge in reaches + junctions

        DO i=1,network%num_reaches
            call one_mccormack_step(network%reach_data(i), network%time, dT, ...)
        END DO

        DO i=1,num_junctions
            call update_junction_values(network%reach_junctions(i))
        END DO

        ! Advance time
        network%time=network%time+dT
        
        ! New boundary values at new time 
        CALL update_boundaries(network)
        

    END SUBROUTINE evolve_hydraulics

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE update_timestep(network)
        ! Loop over every xsection, and compute the new timestep from the CFL condition
        TYPE(network_data_type), INTENT(IN OUT):: network

        INTEGER(ip):: i, j
        REAL(dp):: dT, local_wavespeed, local_dt

        dT = 1000._dp !network%dT
        DO i=1,network%num_reaches
            DO j=1, network%reach_data(i)%xsect_count
                local_wavespeed = abs(network%reach_data(i)%discharge(j)/network%reach_data(i)%area(j)) &
                                    + (gravity*network%reach_data(i)%mean_depth(j))**0.5_dp
                local_dt = downstream_distance/max(local_wavespeed, 1.0e-06)
                dT = min(dT, local_dt)
            END DO
        END DO

    END SUBROUTINE update_timestep

END MODULE network_solver
