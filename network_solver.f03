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

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE one_mccormack_step(reach_data, time, dT)
        ! Mc-Cormack type flow solver with tweaks
        TYPE(reach_data_type), INTENT(INOUT):: reach_data
        REAL(dp), INTENT(IN):: time, dT

        ! Local vars
        INTEGER(ip):: n=reach_data%xsect_count, i
        REAL(dp):: delX(n), dry_flag(n)
        REAL(dp):: Area_pred(n), Stage_pred(n), Q_pred(n)
        REAL(dp):: Area_cor(n), Stage_cor(n), Q_cor(n)
        REAL(dp):: mean_depth, convective_flux(n), slope(n)

        ! Use channel delX as temporary delX here
        delX = reach_data%downstream_distances(:,2)
        ! Compute Area predictor
        DO i=2,n-1
            Area_pred(i) = reach_data%Area(i) - & 
                           dT/delX(i+1)*(reach_data%Discharge(i+1)-reach_data%Discharge(i))
        END DO
        ! FIXME
        ! Boundary conditions
        !

        ! Wet-dry flag
        DO i=1,n
            mean_depth=reach_data%Area(i)/reach_data%Width(i)
            IF( mean_depth < reach_data%wet_dry_depth) THEN
                dry_flag(i)=0.
            ELSE
                dry_flag(i)=1.
            END IF
        END DO

        DO i=2,n-1
            ! FIXME: Add in inuc type term here
            convective_flux(i) = reach_data%Discharge(i)**2 / reach_data%Area(i) *dry_flag(i)
            slope(i) = (reach_data%Stage(i+1) - reach_data%Stage(i))/delX(i+1)*dry_flag(i)
        END DO
        ! FIXME
        ! Boundary Conditions
        !
    
        ! Compute Q predictor
        DO i=2,n-1
            Qpred(i) = reach_data%Discharge(i) - dT/delX(i+1)* &
                       ( convective_flux(i+1) - convective_flux(i) + &
                         gravity*reach_data%Area(i)*slope(i) )
        END DO
        ! IMPLICIT FRICTION
        ! BOUNDARY CONDITIONS

        ! Back-calculate stage
        DO i=1,n
            Stage_pred(i) = reach_data%xsect(i)%stage_etc_curve%eval(Area_pred(i), 'area', 'stage')
        END DO



        ! Compute Area corrector

        ! Compute Q corrector

        ! Back-calculate Stage


        ! Compute 'final' update

        ! Compute 'conservative' discharge?


    END SUBROUTINE one_mccormack_step
END MODULE network_solver
