subroutine calculate_forces(body_i, root)

    use body_type
    use params 
    use tree_type

    implicit none

    type(Body), intent(inout) :: body_i 
    type(Node), intent(in) :: root 
    real*8 :: Fx, Fy, Fz

    Fx = 0.0
    Fy = 0.0 
    Fz = 0.0

    call calculate_forces_recursive(body_i, root, Fx, Fy, Fz)

    body_i%fx = Fx 
    body_i%fy = Fy 
    body_i%fz = Fz 

end subroutine calculate_forces

subroutine calculate_forces_recursive(body_i, node_i, Fx, Fy, Fz)

    use body_type 
    use params 
    use tree_type 

    implicit none 

    type(Body), intent(in) :: body_i 
    type(Node), intent(in) :: node_i 
    real*8, intent(inout) :: Fx, Fy, Fz 
    real*8 :: dx, dy, dz, distance, distance3, gm
    integer :: i

    dx = node_i%xCenter - body_i%x 
    dy = node_i%yCenter - body_i%y 
    dz = node_i%zCenter - body_i%z
    distance = sqrt(dx*dx + dy*dy + dz*dz)
    if (distance < soft_l) then 
        distance = soft_l
    end if

    if (distance /= 0.0 .and. node_i%totalMass > 0.0) then 
        if ((node_i%xMax - node_i%xMin) / distance < theta) then 
            distance3 = distance * distance * distance 
            gm = G * body_i%m * node_i%totalMass / distance3
            Fx = Fx + gm * dx 
            Fy = Fy + gm * dy
            Fz = Fz + gm * dz
        else 
            if (associated(node_i%child(1)%node)) then 
                do i = 1, 8
                    call calculate_forces_recursive(body_i, node_i%child(i)%node, Fx, Fy, Fz)
                end do 
            else 
                distance3 = distance * distance * distance 
                gm = G * body_i%m * node_i%totalMass / distance3
                Fx = Fx + gm * dx 
                Fy = Fy + gm * dy
                Fz = Fz + gm * dz
            end if
        end if 
    end if 

end subroutine calculate_forces_recursive

subroutine calculate_acceleration(body_i)

    use body_type
    
    implicit none

    type(Body), intent(inout) :: body_i

    body_i%ax = body_i%fx / body_i%m
    body_i%ay = body_i%fy / body_i%m
    body_i%az = body_i%fz / body_i%m

end subroutine calculate_acceleration

subroutine update_vel(body_i, dt)

    use body_type 

    implicit none 

    type(Body), intent(inout) :: body_i
    real*8, intent(in) :: dt


    body_i%vx = body_i%vx + body_i%ax * dt
    body_i%vy = body_i%vy + body_i%ay * dt
    body_i%vz = body_i%vz + body_i%az * dt

end subroutine update_vel

subroutine update_pos(body_i, dt)

    use body_type 

    implicit none 

    type(Body), intent(inout) :: body_i 
    real*8, intent(in) :: dt

    body_i%x = body_i%x + body_i%vx * dt
    body_i%y = body_i%y + body_i%vy * dt
    body_i%z = body_i%z + body_i%vz * dt

end subroutine update_pos

subroutine leapfrog_kdk(bodies, step)
    use body_type 
    use tree_type 
    use params
    use omp_lib

    implicit none 

    type(Body), dimension(1:N), intent(inout) :: bodies 
    integer, intent(in) :: step
    type(Node) :: root
    integer :: i

    !$omp parallel private(i)

    ! first we kick
    !$omp do
    do i = 1, N 
        call update_vel(bodies(i), t_step/2.0)
    end do 
    !$omp end do

    ! then drift
    !$omp do
    do i = 1, N 
        call update_pos(bodies(i), t_step)
    end do 
    !$omp end do

    !$omp barrier
    !$omp single

    ! then caluclate forces and kick
    call initialize_root_node(root, root_xMin, root_xMax, root_yMin, root_yMax, root_zMin, root_zMax)

    do i = 1, N 
        call insert_body_into_tree(root, bodies(i), root)
    end do 

    call update_node_mass_com(root)
    !$omp end single
    !$omp barrier

    !$omp do
    do i = 1, N
        call calculate_forces(bodies(i), root)
    end do 
    !$omp end do

    !$omp do
    do i = 1, N 
        call calculate_acceleration(bodies(i))
    end do 
    !$omp end do

    !$omp do
    do i = 1, N 
        call update_vel(bodies(i), t_step/2.0)
    end do
    !$omp end do

    !$omp barrier
    !$omp end parallel

    if (mod(step, nbetween) == 0) then
        call save_body_positions(step)
    end if

    call deallocate_children(root)

end subroutine leapfrog_kdk