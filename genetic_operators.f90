module genetic_operators
    use parameters
    use tree_structure
    implicit none

contains

    subroutine subtree_crossover(ind1, ind2)
        type(TreeNode), pointer :: ind1
        type(TreeNode), pointer :: ind2
        logical :: crossover_done

        crossover_done = .false.
        call crossover_recursive(ind1, ind2, crossover_done)

    end subroutine subtree_crossover

    recursive subroutine crossover_recursive(node1, node2, crossover_done)
        type(TreeNode), pointer :: node1
        type(TreeNode), pointer :: node2
        logical, intent(inout) :: crossover_done  ! 追加：crossover が完了したかどうか

        real :: r

        if (.not. associated(node1) .or. .not. associated(node2) .or. crossover_done) then
            return
        end if

        call random_number(r)
        if (r < 0.01) then
            call swap_subtrees(node1, node2)
            crossover_done = .true.  ! 交叉フラグを立てる
        else
            if (r < 0.5) then
                call crossover_recursive(node1%left, node2%left, crossover_done)
                call crossover_recursive(node1%right, node2%right, crossover_done)
            else
                call crossover_recursive(node1%right, node2%right, crossover_done)
                call crossover_recursive(node1%left, node2%left, crossover_done)
            end if
        end if
    end subroutine crossover_recursive


    subroutine mutate(individual)
        type(TreeNode), pointer :: individual

        call mutate_recursive(individual)
    end subroutine mutate

    recursive subroutine mutate_recursive(node)
        use parameters
        type(TreeNode), pointer :: node
        real :: r

        if (.not. associated(node)) return

        call random_number(r)
        if (r < MUTATION_RATE) then
            if (.not. node%is_leaf) then
                ! 内部ノードの変異
                call random_number(r)
                node%variable = int(r * NUM_FEATURES)
                call random_number(r)
                node%threshold = int(r * NUM_THRESHOLD)
            else
                ! リーフノードの変異
                call random_number(r)
                node%prediction = int(r * NUM_CLASSES)
            end if
        end if

        if (.not. node%is_leaf) then
            call mutate_recursive(node%left)
            call mutate_recursive(node%right)
        end if
    end subroutine mutate_recursive

    subroutine swap_subtrees(node1, node2)
        type(TreeNode), pointer :: node1
        type(TreeNode), pointer :: node2
        type(TreeNode), pointer :: temp_ptr

        temp_ptr => node1
        node1 => node2
        node2 => temp_ptr
    end subroutine swap_subtrees

end module genetic_operators
