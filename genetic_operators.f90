module genetic_operators
    use parameters
    use tree_structure
    implicit none

contains

    subroutine subtree_crossover(ind1, ind2)
        use tree_structure
        type(TreeNode), pointer :: ind1
        type(TreeNode), pointer :: ind2
        type(TreeNodePointer), allocatable :: nodes1(:)
        type(TreeNodePointer), allocatable :: nodes2(:)
        integer :: num_nodes1, num_nodes2
        integer :: idx1, idx2
        real :: r

        ! 配列の割り当て
        allocate(nodes1(MAX_NODES))
        allocate(nodes2(MAX_NODES))
        num_nodes1 = 0
        num_nodes2 = 0

        call get_all_nodes(ind1, nodes1, num_nodes1)
        call get_all_nodes(ind2, nodes2, num_nodes2)

        ! ノード数がゼロの場合のチェック
        if (num_nodes1 == 0 .or. num_nodes2 == 0) then
            print *, "No nodes available for crossover."
            return
        end if

        ! ランダムなノードを選択
        call random_number(r)
        idx1 = int(r * num_nodes1) + 1
        call random_number(r)
        idx2 = int(r * num_nodes2) + 1

        ! サブツリーを交換
        call swap_subtrees(nodes1(idx1), nodes2(idx2))

        ! 配列の解放
        deallocate(nodes1)
        deallocate(nodes2)
    end subroutine subtree_crossover

    subroutine mutate(individual)
        use parameters
        type(TreeNode), pointer :: individual
        type(TreeNodePointer), allocatable :: nodes(:)
        integer :: num_nodes, i
        real :: r

        ! 配列の割り当て
        allocate(nodes(MAX_NODES))
        num_nodes = 0

        call get_all_nodes(individual, nodes, num_nodes)

        do i = 1, num_nodes
            call random_number(r)
            if (r < MUTATION_RATE) then
                if (.not. nodes(i)%ptr%is_leaf) then
                    ! 内部ノードの変異
                    call random_number(r)
                    nodes(i)%ptr%variable = int(r * NUM_FEATURES)
                    call random_number(r)
                    nodes(i)%ptr%threshold = r * 6.0 - 3.0
                else
                    ! リーフノードの変異
                    call random_number(r)
                    nodes(i)%ptr%prediction = int(r * NUM_CLASSES)
                end if
            end if
        end do

        ! 配列の解放
        deallocate(nodes)
    end subroutine mutate

    recursive subroutine get_all_nodes(node, nodes, num_nodes)
        use tree_structure
        type(TreeNode), pointer :: node
        type(TreeNodePointer), intent(inout) :: nodes(:)
        integer, intent(inout) :: num_nodes
        if (.not. associated(node)) return
        if (num_nodes >= size(nodes)) then
            print *, "Exceeded MAX_NODES"
            return
        end if
        num_nodes = num_nodes + 1
        nodes(num_nodes)%ptr => node
        if (.not. node%is_leaf) then
            call get_all_nodes(node%left, nodes, num_nodes)
            call get_all_nodes(node%right, nodes, num_nodes)
        end if
    end subroutine get_all_nodes

    subroutine swap_subtrees(node1, node2)
        type(TreeNodePointer), intent(inout) :: node1
        type(TreeNodePointer), intent(inout) :: node2
        type(TreeNode), pointer :: temp_ptr

        temp_ptr => node1%ptr
        node1%ptr => node2%ptr
        node2%ptr => temp_ptr
    end subroutine swap_subtrees

end module genetic_operators
