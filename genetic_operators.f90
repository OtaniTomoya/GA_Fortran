module genetic_operators
    use parameters
    use tree_structure
    implicit none

contains

    subroutine subtree_crossover(ind1, ind2)
        type(TreeNode), pointer :: ind1
        type(TreeNode), pointer :: ind2
        real :: r

        call crossover_recursive(ind1, ind2)
    end subroutine subtree_crossover

    recursive subroutine crossover_recursive(node1, node2)
        type(TreeNode), pointer :: node1
        type(TreeNode), pointer :: node2
        real :: r

        if (.not. associated(node1) .or. .not. associated(node2)) return

        call random_number(r)
        if (r < CROSSOVER_RATE) then
            call swap_subtrees(node1, node2)
        else
            call crossover_recursive(node1%left, node2%left)
            call crossover_recursive(node1%right, node2%right)
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
                node%threshold = r * 6.0 - 3.0
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
