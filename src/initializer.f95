subroutine init 

    use params 
    use body_type
    use model_vars

    implicit none 

    integer :: i 
    real*8 :: magnitude, vx, vy
    integer, dimension(:), allocatable :: seed
    !call random_seed(size = i) ! seed the random number generator
    !allocate(seed(i))
    !seed = 10
    !call random_seed(put = seed)
    call random_seed()

    do i = 1, N 

        call random_number(bodies(i)%x) ! generate random position
        call random_number(bodies(i)%y)
        call random_number(bodies(i)%z)
        bodies(i)%x = (bodies(i)%x -0.5)*2
        bodies(i)%y = (bodies(i)%y -0.5)*2
        bodies(i)%z = (bodies(i)%z -0.5)*2
    
        ! Calculate normalized clockwise velocity
        magnitude = sqrt(bodies(i)%x**2 + bodies(i)%y**2)
        if (magnitude /= 0.0) then
            vx = bodies(i)%y / magnitude
            vy = -bodies(i)%x / magnitude
        else
            vx = 0.0
            vy = 0.0
        end if

        ! Assign small clockwise velocity
        bodies(i)%vx = vx * 0.5
        bodies(i)%vy = vy * 0.5
        bodies(i)%vz = 0.0 ! No motion in z-direction

        bodies(i)%m = 1.0 ! assign mass

    end do 

end subroutine init 