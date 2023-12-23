subroutine save_body_positions(step)

    use params 
    use model_vars 
    use body_type
    implicit none

    integer, intent(in) :: step
    integer :: i
    character(len=100) :: fmt, filename
    write(fmt, "('(',I0,'F15.8)')") N

    ! Construct the filename with step number and data folder
    write(filename, "('data/', A, '_', I0, '.dat')") trim(fname_bodies), step

    open(unit=10, file=filename, status='replace', action='write')

    do i = 1, N
        write(10, fmt) bodies(i)%x, bodies(i)%y, bodies(i)%z
    end do

    close(10)

end subroutine save_body_positions

subroutine save_body_positions_datatest(step)

    use params 
    use model_vars 
    use body_type
    implicit none

    integer, intent(in) :: step
    integer :: i
    character(len=100) :: fmt, filename
    write(fmt, "('(',I0,'F15.8)')") N

    ! Construct the filename with step number and data folder
    write(filename, "('datatest/', A, '_', I0, '.dat')") trim(fname_bodies), step

    open(unit=10, file=filename, status='replace', action='write')

    do i = 1, N
        write(10, fmt) bodies(i)%x, bodies(i)%y, bodies(i)%z
    end do

    close(10)

end subroutine save_body_positions_datatest


subroutine save_box_bounds(tree)
    use params 
    use model_vars 
    use tree_type 
    implicit none 

    type(Node), intent(in) :: tree
    integer :: unit_number

    open(unit=unit_number, file=fname_boxes, status='replace', action='write')
    call save_box_bounds_recursive(tree, unit_number)
    close(unit_number)

end subroutine save_box_bounds


subroutine save_box_bounds_recursive(tree, unit_number)
    use tree_type
    implicit none

    type(Node), intent(in) :: tree
    integer, intent(in) :: unit_number
    integer :: i

    ! Write the current node's boundaries
    write(unit_number, '(6F10.5)') tree%xMin, tree%xMax, tree%yMin, tree%yMax, tree%zMin, tree%zMax

    ! Recurse for each child node
    do i = 1, 8  ! Assuming each node has up to 8 children
        if (associated(tree%child(i)%node)) then
            call save_box_bounds_recursive(tree%child(i)%node, unit_number)
        end if
    end do

end subroutine save_box_bounds_recursive


subroutine save_two_arrays(array1, array2, filename)
    
    use params
    implicit none

    real*8, dimension(N), intent(in) :: array1, array2
    character(len=100), intent(in) :: filename
    integer :: i, unit_id

    ! Open file
    unit_id = 12  ! Assuming unit_id 10 is free to use
    open(unit=unit_id, file=filename, status='replace', action='write')

    ! Write arrays to file
    do i = 1, N
        write(unit_id, '(2F10.5)') array1(i), array2(i)
    end do
    
end subroutine save_two_arrays
