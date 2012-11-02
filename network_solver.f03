MODULE network_solver
    ! Routines to solve the St-Venant Equations with Junctions in a river network
    USE global_defs
    USE river_classes
   
    IMPLICIT NONE 
    contains

    SUBROUTINE evolve_hydraulics(network)
        ! Evolve the hydraulics in the network
        TYPE(network_data_type), INTENT(IN OUT):: network
        INTEGER:: i

        ! Update the timestep dT
        CALL update_timestep(network)

        ! Update the flow Stage, Area + Discharge in reaches + junctions

        DO i=1,network%num_reaches
            call one_mccormack_step(network%reach_data(i), network%time, network%dT)
        END DO

        DO i=1,network%num_junctions
            call update_junction_values(network%reach_junctions(i))
        END DO

        ! Advance time
        network%time=network%time+network%dT
        
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
                grav_wavespeed=(gravity*network%reach_data(i)%Area(j)/network%reach_data(i)%Width(j))**0.5_dp
                local_wavespeed = abs(vel) + grav_wavespeed

                ! FIXME: Check interpretation of downstream distances -- perhaps incorrect
                local_dt = network%reach_data(i)%downstream_dists(j,2)/max(local_wavespeed, small_positive_real)
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
        INTEGER(ip)::i, n
        REAL(dp):: delX(reach_data%xsect_count), dry_flag(reach_data%xsect_count)
        REAL(dp):: Area_pred(reach_data%xsect_count), Stage_pred(reach_data%xsect_count), Q_pred(reach_data%xsect_count)
        REAL(dp):: Area_cor(reach_data%xsect_count), Stage_cor(reach_data%xsect_count), Q_cor(reach_data%xsect_count)
        REAL(dp):: mean_depth, convective_flux(reach_data%xsect_count), slope(reach_data%xsect_count)

        n=reach_data%xsect_count

        ! Use channel delX as temporary delX here
        delX = reach_data%downstream_dists(:,2)
        ! Compute Area predictor
        Area_pred(1:n-1) = reach_data%Area(1:n-1) -dT/delX(2:n)*&
                          (reach_data%Discharge(2:n) - reach_data%Discharge(1:n-1))

        ! Wet-dry flag
        dry_flag=merge(1.0_dp, 0.0_dp, reach_data%Area/reach_data%Width >= reach_data%wet_dry_depth)

        convective_flux(1:n-1) = reach_data%Discharge(1:n-1)**2 / reach_data%Area(1:n-1) * dry_flag(1:n-1)  
        slope(1:n-1) = (reach_data%Stage(2:n) - reach_data%Stage(1:n-1))/delX(2:n)*dry_flag(1:n-1)

        ! FIXME
        ! Boundary Conditions
        !
    
        ! Compute Q predictor
        Q_pred(1:n-1) = reach_data%Discharge(1:n-1) -  &
                       dT/delX(2:n)*(convective_flux(2:n) - convective_flux(1:n-1)) &
                       -dT*gravity*0.5_dp*(reach_data%Area(1:n-1)+reach_data%Area(2:n))*slope(1:n-1) 

        ! FIXME:
        ! BOUNDARY CONDITIONS

        ! IMPLICIT FRICTION

        ! Back-calculate stage
        DO i=1,n
            Stage_pred(i) = reach_data%xsects(i)%stage_etc_curve%eval(Area_pred(i), 'area', 'stage')
        END DO



        ! Compute Area corrector

        ! Compute Q corrector

        ! Back-calculate Stage


        ! Compute 'final' update

        ! Compute 'conservative' discharge?


    END SUBROUTINE one_mccormack_step
END MODULE network_solver
