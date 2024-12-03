module tree_generation
    use parameters
    use tree_structure
    implicit none
contains
    subroutine create_random_tree(max_depth, current_depth, node)
        integer, intent(in) :: max_depth, current_depth
        type(TreeNode), pointer, intent(out) :: node
        real :: r

        allocate(node)
        call random_number(r)

        if (current_depth >= max_depth .or. (current_depth >= MIN_DEPTH .and. r < 0.1)) then
            ! リーフノード
            node%is_leaf = .true.
            call random_number(r)
            node%prediction = int(r * NUM_CLASSES)
        else
            ! 内部ノード
            node%is_leaf = .false.
            call random_number(r)
            node%variable = int(r * NUM_FEATURES)
            call random_number(r)
            node%threshold = r * 6.0 - 3.0  ! -3から3の範囲
            call create_random_tree(max_depth, current_depth + 1, node%left)
            call create_random_tree(max_depth, current_depth + 1, node%right)
        end if
    end subroutine create_random_tree
end module tree_generation
