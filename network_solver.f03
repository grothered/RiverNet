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

        ! Local vars
        INTEGER(ip):: i, j
        REAL(dp):: dT, local_wavespeed, local_dt, vel, grav_wavespeed

        ! Predefine timestep
        dT = maximum_allowed_timestep 

        ! Loop over all xsections on all reaches and get the new timestep
        DO i=1,network%num_reaches
            DO j=1, network%reach_data(i)%xsect_count
                ! 1D Velocity
                vel=network%reach_data(i)%Discharge(j)/max(network%reach_data(i)%Area(j), small_positive_real)
                ! Gravity wavespeed = sqrt( g * mean_depth)
                grav_wavespeed=(gravity*network%reach_data(i)%Area(j)/network%reach_data%Width(j))**0.5_dp
                local_wavespeed = abs(vel) + grav_wavespeed

                ! FIXME: Check interpretation of downstream distances -- perhaps incorrect
                local_dt = network%reach(i)%downstream_distance(j,2)/max(local_wavespeed, small_positive_real)
                local_dt = local_dt*network%CFL
                dT = min(dT, local_dt)
            END DO
        END DO

        network%dT=dT

    END SUBROUTINE update_timestep

END MODULE network_solver
