subroutine test_tree(root, bodies)
    use body_type
    use tree_type
    use params

    implicit none 

    type(Node), intent(in) :: root 
    type(Body), dimension(1:N), intent(in) :: bodies
    real*8 :: expectedCOMx, expectedCOMy, expectedCOMz
    real*8 :: totalMass
    integer :: i
    real*8 :: tolerance
    tolerance = 0.001

    ! Initialize expected COM and total mass
    expectedCOMx = 0.0
    expectedCOMy = 0.0
    expectedCOMz = 0.0
    totalMass = 0.0

    do i = 1, N
        if (bodies(i)%x >= root%xMin .and. bodies(i)%x < root%xMax .and. &
            bodies(i)%y >= root%yMin .and. bodies(i)%y < root%yMax .and. &
            bodies(i)%z >= root%zMin .and. bodies(i)%z < root%zMax) then
            totalMass = totalMass + bodies(i)%m
            expectedCOMx = expectedCOMx + bodies(i)%m * bodies(i)%x
            expectedCOMy = expectedCOMy + bodies(i)%m * bodies(i)%y
            expectedCOMz = expectedCOMz + bodies(i)%m * bodies(i)%z
        end if
    end do

    if (totalMass > 0.0) then
        expectedCOMx = expectedCOMx / totalMass
        expectedCOMy = expectedCOMy / totalMass
        expectedCOMz = expectedCOMz / totalMass
    end if

    ! Compare the expected COM with the actual COM in the node
    if (abs(root%xCenter - expectedCOMx) < tolerance .or. &
        abs(root%yCenter - expectedCOMy) < tolerance .or. &
        abs(root%zCenter - expectedCOMz) < tolerance) then
        if (root%totalMass == 1.0) then
            ! Report discrepancy
            print *, "Disc COM"
            ! print *, "Discrepancy in COM for Node at: ", root%xMin, root%yMin, root%zMin
            ! print *, "Discrepancy in COM for Node at: ", root%xMax, root%yMax, root%zMax
            ! print *, "Mass of Node: ", root%totalMass
            ! print *, "Expected COM: ", expectedCOMx, expectedCOMy, expectedCOMz
            ! print *, "Actual COM: ", root%xCenter, root%yCenter, root%zCenter
            print *, expectedCOMx-root%xCenter, expectedCOMy-root%yCenter, expectedCOMz-root%zCenter
        end if
    end if

    ! Recurse for each child node
    do i = 1, 8  ! Assuming each node has up to 8 children
        if (associated(root%child(i)%node)) then
            call test_tree(root%child(i)%node, bodies)
        end if
    end do

end subroutine test_tree


subroutine count_nodes_with_mass_one(node_i, count)
    use tree_type  ! Assuming tree_type is the module where Node is defined
    implicit none

    type(Node), intent(in) :: node_i  ! The current node
    integer, intent(inout) :: count  ! Counter for nodes with mass 1.0
    logical :: isLeaf

    integer :: i

    isLeaf = .true.
    ! Check if the current node has a mass of 1.0
    if (node_i%totalMass == 1.0) then
        do i = 1, 8 
            if (associated(node_i%child(i)%node)) then 
                isLeaf = .false.
                exit 
            end if 
        end do
        if (isLeaf) then
            count = count + 1
            print *, node_i%xMax - node_i%xMin
        end if 
    end if

    ! Recursively count in child nodes
    do i = 1, 8  ! Assuming each node can have up to 8 children
        if (associated(node_i%child(i)%node)) then
            call count_nodes_with_mass_one(node_i%child(i)%node, count)
        end if
    end do
end subroutine count_nodes_with_mass_one


subroutine test_tree_mass(root, bodies)
    use body_type
    use tree_type
    use params

    implicit none 

    type(Node), intent(in) :: root 
    type(Body), dimension(1:N), intent(in) :: bodies
    real*8 :: totalMass, weightedX, weightedY, weightedZ
    real*8 :: computedXCenter, computedYCenter, computedZCenter
    real*8, parameter :: tolerance = 0.00000000000001
    integer :: i

    totalMass = 0.0
    weightedX = 0.0
    weightedY = 0.0
    weightedZ = 0.0

    do i = 1, N
        if (bodies(i)%x >= root%xMin .and. bodies(i)%x < root%xMax .and. &
            bodies(i)%y >= root%yMin .and. bodies(i)%y < root%yMax .and. &
            bodies(i)%z >= root%zMin .and. bodies(i)%z < root%zMax) then
            totalMass = totalMass + bodies(i)%m
            weightedX = weightedX + bodies(i)%x * bodies(i)%m
            weightedY = weightedY + bodies(i)%y * bodies(i)%m
            weightedZ = weightedZ + bodies(i)%z * bodies(i)%m
        end if
    end do

    ! Calculate the computed center of mass
    if (totalMass > 0.0) then
        computedXCenter = weightedX / totalMass
        computedYCenter = weightedY / totalMass
        computedZCenter = weightedZ / totalMass
        if (abs(totalMass - root%totalMass) > tolerance .or. &
            abs(computedXCenter - root%xCenter) > tolerance .or. &
            abs(computedYCenter - root%yCenter) > tolerance .or. &
            abs(computedZCenter - root%zCenter) > tolerance) then
            print *, "------"
            print *, "Node Mass and Center of Mass Discrepancy Detected"
            print *, "Computed: ", totalMass, computedXCenter, computedYCenter, computedZCenter
            print *, "Stored:   ", root%totalMass, root%xCenter, root%yCenter, root%zCenter
            print *, "------"
        end if
    end if

    ! Recurse for each child node
    do i = 1, 8  ! Assuming each node has up to 8 children
        if (associated(root%child(i)%node)) then
            call test_tree_mass(root%child(i)%node, bodies)
        end if
    end do

end subroutine test_tree_mass


subroutine calculate_forces_brute_force(bodies)
    use body_type
    use params
    implicit none

    type(Body), dimension(N), intent(inout) :: bodies
    real*8 :: dx, dy, dz, distance, distance3
    real*8 :: Fx, Fy, Fz
    real*8 :: gm
    integer :: i, j

    ! Initialize forces to zero for all bodies
    do i = 1, N
        bodies(i)%fx = 0.0
        bodies(i)%fy = 0.0
        bodies(i)%fz = 0.0
    end do

    ! Calculate forces using brute force
    do i = 1, N - 1
        do j = i + 1, N
            dx = bodies(j)%x - bodies(i)%x
            dy = bodies(j)%y - bodies(i)%y
            dz = bodies(j)%z - bodies(i)%z
            distance = sqrt(dx*dx + dy*dy + dz*dz)
            distance3 = distance * distance * distance

            gm = G * bodies(i)%m * bodies(j)%m / distance3

            Fx = gm * dx
            Fy = gm * dy
            Fz = gm * dz

            bodies(i)%fx = bodies(i)%fx + Fx
            bodies(i)%fy = bodies(i)%fy + Fy
            bodies(i)%fz = bodies(i)%fz + Fz

            bodies(j)%fx = bodies(j)%fx - Fx
            bodies(j)%fy = bodies(j)%fy - Fy
            bodies(j)%fz = bodies(j)%fz - Fz
        end do
    end do
end subroutine calculate_forces_brute_force


subroutine compare_force_calculations(bodies, root)
    use body_type
    use params
    use tree_type
    implicit none

    type(Body), dimension(N), intent(inout) :: bodies
    type(Node), intent(in) :: root
    real*8 :: error_sum, error_percent
    real*8 :: force_magnitude_barnes, force_magnitude_brute
    integer :: i
    real*8, dimension(N) :: fxb, fyb, fzb, fxh, fyh, fzh
    real*8, dimension(N) :: error, brute

    error_sum = 0.0

    ! Calculate forces using Barnes-Hut algorithm
    do i = 1, N
        bodies(i)%fx = 0.0
        bodies(i)%fy = 0.0
        bodies(i)%fz = 0.0
    end do
    do i = 1, N
        call calculate_forces(bodies(i), root)
    end do
    do i = 1, N 
        fxh(i) = bodies(i)%fx
        fyh(i) = bodies(i)%fy
        fzh(i) = bodies(i)%fz
    end do
    do i = 1, N
        bodies(i)%fx = 0.0
        bodies(i)%fy = 0.0
        bodies(i)%fz = 0.0
    end do
    call calculate_forces_brute_force(bodies)
    do i = 1, N 
        fxb(i) = bodies(i)%fx
        fyb(i) = bodies(i)%fy
        fzb(i) = bodies(i)%fz
    end do

    ! Compare with brute force calculation
    do i = 1, N
        force_magnitude_barnes = sqrt(fxh(i)**2 + fyh(i)**2 + fzh(i)**2)
        force_magnitude_brute = sqrt(fxb(i)**2 + fyb(i)**2 + fzb(i)**2)
        brute(i) = force_magnitude_brute
        error(i) = abs(force_magnitude_barnes - force_magnitude_brute) / force_magnitude_brute
        error_sum = error_sum + error(i)
        print *, force_magnitude_barnes, force_magnitude_brute
    end do

    call save_two_arrays(brute, error, "force_comps.dat")

    error_percent = error_sum / N * 100

    print *, 'Average percentage error: ', error_percent
end subroutine compare_force_calculations
