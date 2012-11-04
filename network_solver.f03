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
        !
        ! Mc-Cormack type flow solver with tweaks
        !

        TYPE(reach_data_type), INTENT(INOUT):: reach_data
        REAL(dp), INTENT(IN):: time, dT

        ! Local vars
        INTEGER(ip)::i, n
        REAL(dp):: delX(reach_data%xsect_count), dry_flag(reach_data%xsect_count)
        REAL(dp):: Area_pred(reach_data%xsect_count), Stage_pred(reach_data%xsect_count), Q_pred(reach_data%xsect_count)
        REAL(dp):: Area_cor(reach_data%xsect_count), Stage_cor(reach_data%xsect_count), Q_cor(reach_data%xsect_count)
        REAL(dp):: mean_depth, convective_flux(reach_data%xsect_count), slope(reach_data%xsect_count)
        REAL(dp):: drag_factor(reach_data%xsect_count), Af(reach_data%xsect_count), Ab(reach_data%xsect_count)
        REAL(dp):: Width_pred(reach_data%xsect_count), Width_cor(reach_data%xsect_count)

        n=reach_data%xsect_count

        ! Use channel delX as temporary delX here
        delX = reach_data%downstream_dists(:,2)
        ! Compute Area predictor
        Area_pred(1:n-1) = reach_data%Area(1:n-1) -dT/delX(2:n)*&
                          (reach_data%Discharge(2:n) - reach_data%Discharge(1:n-1))

        ! Wet-dry flag
        dry_flag=merge(1.0_dp, 0.0_dp, reach_data%Area/reach_data%Width > reach_data%wet_dry_depth)

        convective_flux= reach_data%Discharge**2 / max(reach_data%Area, small_positive_real) * dry_flag  
        slope(1:n-1) = (reach_data%Stage(2:n) - reach_data%Stage(1:n-1))/delX(2:n)*dry_flag(1:n-1)

        ! Compute Q predictor with implicit friction
        Af(1:n-1)=0.5_dp*(reach_data%Area(1:n-1)+reach_data%Area(2:n)) ! 'Forward' area estimate
        ! Convective + gravity terms
        Q_pred(1:n-1) = reach_data%Discharge(1:n-1) -  &
                       dT/delX(2:n)*(convective_flux(2:n) - convective_flux(1:n-1)) &
                       -dT*gravity*Af(1:n-1)*slope(1:n-1) 

        ! IMPLICIT FRICTION: g*Af*Sf = drag_factor*Q*abs(Q)
        drag_factor(1:n-1)=(gravity*Af(1:n-1)*(-sign(1._dp, Q_pred(1:n-1))/(Area_pred(1:n-1)**2._dp))*reach_data%Drag_1D(1:n-1) )
        DO i=1,n-1
             IF(drag_factor(i).ne.0._dp) THEN
                Q_pred(i)= (1._dp - sqrt(1._dp- 4._dp*dT*drag_factor(i)*Q_pred(i) ))/(2._dp*dT*drag_factor(i))
             ELSE
                ! Friction is negligible
             END IF
        END DO


        ! Back-calculate stage
        DO i=1,n
            Stage_pred(i) = reach_data%xsects(i)%stage_etc_curve%eval(Area_pred(i), 'area', 'stage')
            Width_pred(i) = reach_data%xsects(i)%stage_etc_curve%eval(Area_pred(i), 'area', 'width')
        END DO


        ! Compute Area corrector
        Area_cor(2:n) = reach_data%Area(2:n) -dT/delX(2:n)*&
                          (Q_pred(2:n) - Q_pred(1:n-1))

        ! Wet-dry flag
        dry_flag=merge(1.0_dp, 0.0_dp, Area_pred/Width_pred > reach_data%wet_dry_depth)

        convective_flux = Q_pred**2 / max(Area_pred, small_positive_real) * dry_flag  
        slope(2:n) = (Stage_pred(2:n) - Stage_pred(1:n-1))/delX(2:n)*dry_flag(2:n)
        
        ! Compute Q corrector with implicit friction
        Ab(2:n)=0.5_dp*(Area_pred(1:n-1)+Area_pred(2:n)) ! 'backward' area estimate
        ! Convective + gravity terms
        Q_cor(2:n) = reach_data%Discharge(2:n) -  &
                       dT/delX(2:n)*(convective_flux(2:n) - convective_flux(1:n-1)) &
                       -dT*gravity*Ab(2:n)*slope(2:n) 
        
        ! IMPLICIT FRICTION: g*Af*Sf = drag_factor*Q*abs(Q)
        drag_factor(2:n)=(gravity*Af(2:n)*(-sign(1._dp, Q_cor(2:n))/(Area_cor(2:n)**2._dp))*reach_data%Drag_1D(2:n) )
        DO i=2,n
             IF(drag_factor(i).ne.0._dp) THEN
                Q_cor(i)= (1._dp - sqrt(1._dp- 4._dp*dT*drag_factor(i)*Q_cor(i) ))/(2._dp*dT*drag_factor(i))
             ELSE
                ! Friction is negligible
             END IF
        END DO



        ! Compute 'final' update
        reach_data%Area(2:n-1)= 0.5_dp*(Area_pred(2:n-1) + Area_cor(2:n-1))
        reach_data%Discharge(2:n-1)= 0.5_dp*(Q_pred(2:n-1) + Q_cor(2:n-1))

        ! Back-calculate Stage, width
        DO i=2,n-1
            reach_data%Stage(i) = reach_data%xsects(i)%stage_etc_curve%eval(reach_data%Area(i), 'area', 'stage')
            reach_data%Width(i) = reach_data%xsects(i)%stage_etc_curve%eval(reach_data%Area(i), 'area', 'width')
        END DO
        
        ! Compute 'conservative' discharge?


    END SUBROUTINE one_mccormack_step
END MODULE network_solver
