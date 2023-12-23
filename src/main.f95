program main
    use omp_lib
    use params
    use body_type
    use model_vars
    use tree_type

    implicit none

    type(Node) :: root
    integer :: i
    double precision :: start_time, end_time, elapsed_time


    ! call init to initialize bodies
    call init
    call save_body_positions(0)

    ! Initialize the root node of the tree
    call initialize_root_node(root, root_xMin, root_xMax, root_yMin, root_yMax, root_zMin, root_zMax)

    do i = 1, N 
        call insert_body_into_tree(root, bodies(i), root)
    end do

    call update_node_mass_com(root)

    do i = 1, N
        call calculate_forces(bodies(i), root)
    end do

    ! Start timing
    start_time = omp_get_wtime()

    do i = 1, num_steps
        call leapfrog_kdk(bodies, i)
    end do

    ! End timing
    end_time = omp_get_wtime()

    ! Calculate elapsed time
    elapsed_time = end_time - start_time

    ! Print elapsed time
    print *, 'Time taken for leapfrog loop: ', elapsed_time, ' seconds'

end program main