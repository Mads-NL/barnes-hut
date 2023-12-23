module tree_type
    implicit none

    type Node_p
        type(Node), pointer :: node
    end type Node_p 

    type Node
        real*8 :: xCenter, yCenter, zCenter ! center of mass for the node
        real*8 :: totalMass ! total mass of bodies in this node
        real*8 :: xMin, xMax ! x boundaries of the region
        real*8 :: yMin, yMax ! y boundaries of the region
        real*8 :: zMin, zMax ! z boundaries of the region
        type(Node_p), dimension(8) :: child ! pointers to child nodes
    end type Node

end module tree_type

subroutine initialize_root_node(root, xMin, xMax, yMin, yMax, zMin, zMax)
    use tree_type
    implicit none

    type(Node), intent(inout) :: root
    real*8, intent(in) :: xMin, xMax, yMin, yMax, zMin, zMax
    integer :: i

    root%xMin = xMin
    root%xMax = xMax
    root%yMin = yMin
    root%yMax = yMax
    root%zMin = zMin
    root%zMax = zMax
    root%totalMass = 0.0
    root%xCenter = 0.0
    root%yCenter = 0.0
    root%zCenter = 0.0

    do i = 1, 8
        root%child(i)%node => null()  ! Initialize child nodes to null
    end do 

end subroutine initialize_root_node


subroutine deallocate_children(node_root)
    use tree_type
    implicit none

    type(Node), intent(inout) :: node_root
    integer :: i

    ! Recursively deallocate all child nodes
    do i = 1, 8
        if (associated(node_root%child(i)%node)) then
            call deallocate_children(node_root%child(i)%node)
            deallocate(node_root%child(i)%node)
        end if
    end do

end subroutine deallocate_children


subroutine subdivide_node(parent)
    use tree_type 

    implicit none 

    type(Node), intent(inout) :: parent
    integer :: i 
    real*8 :: xMid, yMid, zMid 

    ! calculate midpoints for the nodes region
    xMid = (parent%xMin + parent%xMax) / 2.0
    yMid = (parent%yMin + parent%yMax) / 2.0
    zMid = (parent%zMin + parent%zMax) / 2.0

    ! create and initialize the 8 child nodes
    do i = 1, 8
        allocate(parent%child(i)%node) ! Allocate memory for each child node
        call set_child_boundaries(parent, i, xMid, yMid, zMid)
    end do 

end subroutine subdivide_node


subroutine set_child_boundaries(parent, childIndex, xMid, yMid, zMid)
    use tree_type

    implicit none 

    type(Node), intent(in) :: parent 
    integer, intent(in) :: childIndex
    real*8, intent(in) :: xMid, yMid, zMid 
    integer :: i

    parent%child(childIndex)%node%totalMass = 0.0

    do i = 1, 8
        parent%child(childIndex)%node%child(i)%node => null()  ! Initialize child nodes to null
    end do 

    select case(childIndex)
        case(1)  ! Bottom-front-left
            parent%child(childIndex)%node%xMin = parent%xMin
            parent%child(childIndex)%node%xMax = xMid
            parent%child(childIndex)%node%yMin = parent%yMin
            parent%child(childIndex)%node%yMax = yMid
            parent%child(childIndex)%node%zMin = parent%zMin
            parent%child(childIndex)%node%zMax = zMid
        case(2)  ! Bottom-front-right
            parent%child(childIndex)%node%xMin = xMid
            parent%child(childIndex)%node%xMax = parent%xMax
            parent%child(childIndex)%node%yMin = parent%yMin
            parent%child(childIndex)%node%yMax = yMid
            parent%child(childIndex)%node%zMin = parent%zMin
            parent%child(childIndex)%node%zMax = zMid
        case(3)  ! Bottom-back-left
            parent%child(childIndex)%node%xMin = parent%xMin
            parent%child(childIndex)%node%xMax = xMid
            parent%child(childIndex)%node%yMin = yMid
            parent%child(childIndex)%node%yMax = parent%yMax
            parent%child(childIndex)%node%zMin = parent%zMin
            parent%child(childIndex)%node%zMax = zMid
        case(4)  ! Bottom-back-right
            parent%child(childIndex)%node%xMin = xMid
            parent%child(childIndex)%node%xMax = parent%xMax
            parent%child(childIndex)%node%yMin = yMid
            parent%child(childIndex)%node%yMax = parent%yMax
            parent%child(childIndex)%node%zMin = parent%zMin
            parent%child(childIndex)%node%zMax = zMid
        case(5)  ! Top-front-left
            parent%child(childIndex)%node%xMin = parent%xMin
            parent%child(childIndex)%node%xMax = xMid
            parent%child(childIndex)%node%yMin = parent%yMin
            parent%child(childIndex)%node%yMax = yMid
            parent%child(childIndex)%node%zMin = zMid
            parent%child(childIndex)%node%zMax = parent%zMax
        case(6)  ! Top-front-right
            parent%child(childIndex)%node%xMin = xMid
            parent%child(childIndex)%node%xMax = parent%xMax
            parent%child(childIndex)%node%yMin = parent%yMin
            parent%child(childIndex)%node%yMax = yMid
            parent%child(childIndex)%node%zMin = zMid
            parent%child(childIndex)%node%zMax = parent%zMax
        case(7)  ! Top-back-left
            parent%child(childIndex)%node%xMin = parent%xMin
            parent%child(childIndex)%node%xMax = xMid
            parent%child(childIndex)%node%yMin = yMid
            parent%child(childIndex)%node%yMax = parent%yMax
            parent%child(childIndex)%node%zMin = zMid
            parent%child(childIndex)%node%zMax = parent%zMax
        case(8)  ! Top-back-right
            parent%child(childIndex)%node%xMin = xMid
            parent%child(childIndex)%node%xMax = parent%xMax
            parent%child(childIndex)%node%yMin = yMid
            parent%child(childIndex)%node%yMax = parent%yMax
            parent%child(childIndex)%node%zMin = zMid
            parent%child(childIndex)%node%zMax = parent%zMax
    end select

end subroutine set_child_boundaries


subroutine insert_body_into_tree(tree, body_i)
    use tree_type 
    use body_type

    implicit none

    type(Body), intent(in) :: body_i 
    type(Node), intent(inout) :: tree 
    type(Body) :: existing_body
    integer :: i 
    logical :: isLeaf

    ! check if node is a leaf
    isLeaf = .true. 
    do i = 1, 8 
        if (associated(tree%child(i)%node)) then 
            isLeaf = .false.
            exit 
        end if 
    end do

    if (isLeaf) then 
        if (tree%totalMass == 0.0) then 
            ! if leaf node is empty, place body there 
            tree%totalMass = body_i%m 
            tree%xCenter = body_i%x 
            tree%yCenter = body_i%y 
            tree%zCenter = body_i%z
        else ! if leaf node is not empty, subdivide and insert into correct child
            ! store the existing body
            !print *, tree%totalMass
            existing_body%m = tree%totalMass
            existing_body%x = tree%xCenter
            existing_body%y = tree%yCenter
            existing_body%z = tree%zCenter
            call subdivide_node(tree)
            call insert_body_into_correct_child(tree, body_i)
            call insert_body_into_correct_child(tree, existing_body)
        end if 
    else 
        ! for non-leaf node, find correct child and insert recursively
        call insert_body_into_correct_child(tree, body_i)
    end if

end subroutine insert_body_into_tree


subroutine insert_body_into_correct_child(parent, body_i)

    use tree_type
    use body_type

    implicit none

    type(Node), intent(inout) :: parent 
    type(Body), intent(in) :: body_i
    integer :: childIndex 
    real*8 :: xMid, yMid, zMid 

    ! calculate midpoints for the nodes region
    xMid = (parent%xMin + parent%xMax) / 2.0
    yMid = (parent%yMin + parent%yMax) / 2.0
    zMid = (parent%zMin + parent%zMax) / 2.0

    ! determine correct child index based on bodies position
    do childIndex = 1, 8
        if ((body_i%x > parent%child(childIndex)%node%xMin) .and. &
            (body_i%x < parent%child(childIndex)%node%xMax)  .and. &
            (body_i%y > parent%child(childIndex)%node%yMin) .and. &
            (body_i%y < parent%child(childIndex)%node%yMax)  .and. &
            (body_i%z > parent%child(childIndex)%node%zMin) .and. &
            (body_i%z < parent%child(childIndex)%node%zMax)) then
                call insert_body_into_tree(parent%child(childIndex)%node, body_i)
        end if
    end do

end subroutine insert_body_into_correct_child


subroutine update_node_mass_com(tree)

    use tree_type
    implicit none

    type(Node), intent(inout) :: tree  ! The current node
    logical :: isLeaf
    integer :: i
    real*8 :: weightedX, weightedY, weightedZ

    isLeaf = .true.
    weightedX = 0.0
    weightedY = 0.0
    weightedZ = 0.0

    ! check if it is a leaf
    do i = 1, 8 
        if (associated(tree%child(i)%node)) then 
            isLeaf = .false.
            exit 
        end if 
    end do 

    ! recursivly update the mass and COM using the child nodes
    if (.not. isLeaf) then 
        ! reset the mass and centers to 0
        tree%totalMass = 0.0
        tree%xCenter = 0.0
        tree%yCenter = 0.0
        tree%zCenter = 0.0
        ! accumulate mass from each chold node recursivly
        do i = 1, 8
            call update_node_mass_com(tree%child(i)%node)
            tree%totalMass = tree%totalMass + tree%child(i)%node%totalMass
            weightedX = weightedX + tree%child(i)%node%totalMass * tree%child(i)%node%xCenter
            weightedY = weightedY + tree%child(i)%node%totalMass * tree%child(i)%node%yCenter
            weightedZ = weightedZ + tree%child(i)%node%totalMass * tree%child(i)%node%zCenter
        end do 

        ! Calculate the center of mass if total mass is not zero
        if (tree%totalMass > 0.0) then
            tree%xCenter = weightedX / tree%totalMass
            tree%yCenter = weightedY / tree%totalMass
            tree%zCenter = weightedZ / tree%totalMass
        end if
    end if 

end subroutine update_node_mass_com