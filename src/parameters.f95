module params 

    implicit none 

    ! natural constants
    real*8, parameter :: G = 1.0/10.0

    ! settings for simulation
    real*8, parameter :: theta = 0.7
    integer, parameter :: N = 1000
    real*8, parameter :: t_step = 0.001
    real*8, parameter :: T = 2.0
    integer, parameter :: num_steps = nint(T / t_step)
    real*8, parameter :: soft_l = 0.01

    ! root-node settings
    real*8, parameter :: root_xMin = -10
    real*8, parameter :: root_xMax = 10
    real*8, parameter :: root_yMin = -10
    real*8, parameter :: root_yMax = 10
    real*8, parameter :: root_zMin = -10
    real*8, parameter :: root_zMax = 10


    ! file names
    character*25, parameter :: fname = "data.dat"
    character*25, parameter :: fname_boxes = "box_data.dat"
    character*25, parameter :: fname_bodies = "body_data"

    ! steps between saving data
    integer, parameter :: nbetween = 10

end module params 


module model_vars 

    use params 
    use body_type

    implicit none 

    ! body arrays
    type(Body), dimension(1:N) :: bodies

end module model_vars 