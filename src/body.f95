module body_type

    implicit none 

    type Body 
        real*8 :: x, y, z ! position
        real*8 :: vx, vy, vz ! velocity
        real*8 :: m ! mass
        real*8 :: fx, fy, fz ! forces
        real*8 :: ax, ay, az ! accelerations
    end type Body 

end module body_type

