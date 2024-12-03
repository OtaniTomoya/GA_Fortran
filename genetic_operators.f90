module genetic_operators
    use parameters
    use tree_structure
    implicit none
contains
    subroutine subtree_crossover(ind1, ind2)
        type(TreeNode), pointer :: ind1
        type(TreeNode), pointer :: ind2
        type(TreeNode), pointer, allocatable :: nodes1(:), nodes2(:)
        integer :: num_nodes1, num_nodes2
        integer :: idx1, idx2
        real :: r

        allocate(nodes1(MAX_NODES))
        allocate(nodes2(MAX_NODES))
        num_nodes1 = 0
        num_nodes2 = 0

        call get_all_nodes(ind1, nodes1, num_nodes1)
        call get_all_nodes(ind2, nodes2, num_nodes2)

        call random_number(r)
        idx1 = int(r * num_nodes1) + 1
        call random_number(r)
        idx2 = int(r * num_nodes2) + 1

        call swap_subtrees(nodes1(idx1), nodes2(idx2))

        deallocate(nodes1)
        deallocate(nodes2)
    end subroutine subtree_crossover

    subroutine mutate(individual)
        type(TreeNode), pointer :: individual
        type(TreeNode), pointer, allocatable :: nodes(:)
        integer :: num_nodes, i
        real :: r

        allocate(nodes(MAX_NODES))
        num_nodes = 0

        call get_all_nodes(individual, nodes, num_nodes)

        do i = 1, num_nodes
            call random_number(r)
            if (r < MUTATION_RATE) then
                if (.not. nodes(i)%is_leaf) then
                    ! 内部ノードの変異
                    call random_number(r)
                    nodes(i)%variable = int(r * NUM_FEATURES)
                    call random_number(r)
                    nodes(i)%threshold = r * 6.0 - 3.0
                else
                    ! リーフノードの変異
                    call random_number(r)
                    nodes(i)%prediction = int(r * NUM_CLASSES)
                end if
            end if
        end do

        deallocate(nodes)
    end subroutine mutate

    ! 補助関数
    subroutine get_all_nodes(node, nodes, num_nodes)
        type(TreeNode), pointer :: node
        type(TreeNode), pointer :: nodes(:)
        integer, intent(inout) :: num_nodes
        if (.not. associated(node)) return
        if (num_nodes >= size(nodes)) then
            print *, "Exceeded MAX_NODES"
            return
        end if
        num_nodes = num_nodes + 1
        nodes(num_nodes) => node
        if (.not. node%is_leaf) then
            call get_all_nodes(node%left, nodes, num_nodes)
            call get_all_nodes(node%right, nodes, num_nodes)
        end if
    end subroutine get_all_nodes

    subroutine swap_subtrees(node1, node2)
        type(TreeNode), pointer :: node1
        type(TreeNode), pointer :: node2
        type(TreeNode), pointer :: temp

        temp => node1
        node1 => node2
        node2 => temp
    end subroutine swap_subtrees
end module genetic_operators
